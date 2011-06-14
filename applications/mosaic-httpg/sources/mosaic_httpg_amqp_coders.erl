
-module (mosaic_httpg_amqp_coders).

-export ([encode_request_routing_key/2, encode_request_message_body/4]).
-export ([decode_response_message_body/3]).


-include ("mosaic_httpg.hrl").

-define (request, mosaic_http_request).
-define (response, mosaic_http_response).


encode_request_routing_key (Request, CallbackIdentifier) ->
	
	try encode_request_routing_key_internal (Request, CallbackIdentifier) of
		Outcome = {ok, _RoutingKey} ->
			Outcome
	catch
		throw : Error = {enforcement_failed, _Context, _Reason, _Value} ->
			{error, {encoding_failed, Error}};
		throw : Error ->
			{error, {encoding_failed, {unknown_error, Error}}}
	end.


encode_request_message_body (Request, CallbackIdentifier, CallbackExchange, CallbackRoutingKey) ->
	
	try encode_request_message_body_internal (Request, CallbackIdentifier, CallbackExchange, CallbackRoutingKey) of
		Outcome = {ok, _Data, _ContentType, _ContentEncoding} ->
			Outcome
	catch
		throw : Error = {enforcement_failed, _Context, _Reason, _Value} ->
			{error, {encoding_failed, Error}};
		throw : Error ->
			{error, {encoding_failed, {unknown_error, Error}}}
	end.


decode_response_message_body (Data, ContentType, ContentEncoding) ->
	
	try decode_response_message_body_internal (Data, ContentType, ContentEncoding) of
		Outcome = {ok, _Response, _CallbackIdentifier} ->
			Outcome
	catch
		throw : Error = {enforcement_failed, _Context, _Reason, _Value} ->
			{error, {decoding_failed, Error}};
		throw : Error ->
			{error, {decoding_failed, {unknown_error, Error}}}
	end.


encode_request_routing_key_internal (_Request, CallbackIdentifier) ->
	
	RoutingKey = erlang:iolist_to_binary (enforce_string (CallbackIdentifier, true, request_callback_identifier)),
	
	{ok, RoutingKey}.


encode_request_message_body_internal (Request, CallbackIdentifier, CallbackExchange, CallbackRoutingKey) ->
	
	EncodedBody = enforce_iolist (Request#?request.http_body, true, request_http_body),
	EncodedBodySize = erlang:byte_size (EncodedBody),
	EmptyBody = (EncodedBodySize == 0),
	EmbeddedBody = ((EncodedBodySize > 0) and (EncodedBodySize =< 1024)),
	FollowingBody = (EncodedBodySize > 1024),
	
	JsonHeaders
		= {obj,
			lists:filter (
					fun ({_Name, Value}) -> Value /= undefined end,
					[
						{<<"version">>, 1},
						{<<"callback-identifier">>, enforce_string (CallbackIdentifier, true, request_callback_identifier)},
						{<<"callback-exchange">>, enforce_string (CallbackExchange, true, request_callback_exchange)},
						{<<"callback-routing-key">>, enforce_string (CallbackRoutingKey, true, request_callback_routing_key)},
						{<<"socket-remote-ip">>, enforce_ip (Request#?request.socket_remote_ip, false, request_socket_remote_ip)},
						{<<"socket-remote-port">>, enforce_port (Request#?request.socket_remote_port, false, request_socket_remote_port)},
						{<<"socket-remote-fqdn">>, enforce_string (Request#?request.socket_remote_fqdn, false, request_socket_remote_fqdn)},
						{<<"socket-local-ip">>, enforce_ip (Request#?request.socket_local_ip, false, request_socket_local_ip)},
						{<<"socket-local-port">>, enforce_port (Request#?request.socket_local_port, false, request_socket_local_port)},
						{<<"socket-local-fqdn">>, enforce_string (Request#?request.socket_local_fqdn, false, request_socket_local_fqdn)},
						{<<"http-version">>, enforce_string (Request#?request.http_version, true, request_http_version)},
						{<<"http-method">>, enforce_string (Request#?request.http_method, true, request_http_method)},
						{<<"http-uri">>, enforce_string (Request#?request.http_uri, true, request_http_uri)},
						{<<"http-headers">>,
							{obj,
								lists:map (
										fun
											({Name, Value}) ->
												{
													enforce_string (Name, true, request_http_header_name),
													enforce_string (Value, true, request_http_header_value)};
											(Value) ->
												enforcement_failed (request_http_header, invalid_value, Value)
										end,
										enforce_list (Request#?request.http_headers, false, request_http_headers))}},
						{<<"http-body">>,
							if
								EmptyBody -> <<"empty">>;
								EmbeddedBody -> <<"embedded">>;
								FollowingBody -> <<"following">>
							end},
						{<<"http-body-content">>,
							if
								EmptyBody; FollowingBody -> undefined;
								EmbeddedBody -> EncodedBody
							end}])},
	
	{ok, EncodedHeaders} = try rfc4627:encode (JsonHeaders) of
		EncodedHeaders_ ->
			try erlang:iolist_to_binary (EncodedHeaders_) of
				EncodedHeaders__ ->
					{ok, EncodedHeaders__}
			catch
				throw : Error ->
					enforcement_failed (request_headers, unknown_json_error, Error)
			end
	catch
		throw : Error ->
			enforcement_failed (request_headers, unknown_json_error, Error)
	end,
	
	EncodedHeadersSize = erlang:byte_size (EncodedHeaders),
	
	{ok, Data} = if
		EmptyBody; EmbeddedBody ->
			{ok, erlang:iolist_to_binary ([
						<<EncodedHeadersSize : 32/big-unsigned-integer-unit:1>>, EncodedHeaders])};
		FollowingBody ->
			{ok, erlang:iolist_to_binary ([
						<<EncodedHeadersSize : 32/big-unsigned-integer-unit:1>>, EncodedHeaders,
						<<EncodedBodySize : 32/big-unsigned-integer-unit:1>>, EncodedBody])}
	end,
	
	ContentType = <<"application/octet-stream">>,
	ContentEncoding = <<"binary">>,
	
	{ok, Data, ContentType, ContentEncoding}.


decode_response_message_body_internal (Data_, ContentType_, ContentEncoding_) ->
	
	Data = enforce_iolist (Data_, true, response_data),
	ContentType = enforce_string (ContentType_, false, response_content_type),
	ContentEncoding = enforce_string (ContentEncoding_, false, response_content_encoding),
	
	ok = case ContentType of
		<<"application/octet-stream">> ->
			ok;
		<<>> ->
			ok;
		undefined ->
			ok;
		_ ->
			enforcement_failed (response_content_type, unknown_value, ContentType)
	end,
	
	ok = case ContentEncoding of
		<<"binary">> ->
			ok;
		<<>> ->
			ok;
		undefined ->
			ok;
		_ ->
			enforcement_failed (response_content_encoding, unknown_value, ContentEncoding)
	end,
	
	{ok, EncodedHeaders, EncodedBody} = case Data of
		<<EncodedHeadersSize_ : 32/big-unsigned-integer-unit:1, EncodedHeaders_ : EncodedHeadersSize_ / binary>> ->
			{ok, EncodedHeaders_, none};
		<<EncodedHeadersSize_ : 32/big-unsigned-integer-unit:1, EncodedHeaders_ : EncodedHeadersSize_ / binary,
				EncodedBodySize_ : 32/big-unsigned-integer-unit:1, EncodedBody_ : EncodedBodySize_ / binary>> ->
			{ok, EncodedHeaders_, EncodedBody_};
		_ ->
			enforcement_failed (response_data, invalid_framing, Data)
	end,
	
	ok = case EncodedHeaders of
		<<>> ->
			enforcement_failed (response_headers, invalid_value, EncodedHeaders);
		_ ->
			ok
	end,
	
	{ok, JsonHeaders_} = try rfc4627:decode (EncodedHeaders) of
		{ok, {obj, JsonHeaders__}, []} ->
			{ok, JsonHeaders__};
		{ok, JsonHeadersValue_, []} ->
			enforcement_failed (response_headers, invalid_value_without_garbage, JsonHeadersValue_);
		{ok, JsonHeadersValue_, JsonHeadersGarbage_} ->
			enforcement_failed (response_headers, invalid_value_with_garbage, {JsonHeadersValue_, JsonHeadersGarbage_});
		{error, _} ->
			enforcement_failed (response_headers, invalid_json_syntax, EncodedHeaders)
	catch
		throw : Error ->
			enforcement_failed (response_headers, unknown_json_error, Error)
	end,
	
	JsonHeaders = lists:map (
				fun ({Name, Value}) -> {erlang:list_to_binary (Name), Value} end,
				JsonHeaders_),
	
	{ok, _Version} = case enforce_positive_integer (proplists:get_value (<<"version">>, JsonHeaders, undefined), true, response_version) of
		1 ->
			{ok, 1};
		Version_ ->
			enforcement_failed (response_version, unknown_value, Version_)
	end,
	
	CallbackIdentifier = enforce_string (proplists:get_value (<<"callback-identifier">>, JsonHeaders, undefined), true, response_callback_identifier),
	
	{ok, DecodedBody} = case enforce_string (proplists:get_value (<<"http-body">>, JsonHeaders, undefined), true, response_http_body_type) of
		<<"empty">> ->
			case EncodedBody of
				none ->
					{ok, <<>>};
				_ ->
					enforcement_failed (response_http_body, empty_with_following_data, none)
			end;
		<<"embedded">> ->
			case EncodedBody of
				none ->
					case enforce_string (proplists:get_value (<<"http-body-content">>, JsonHeaders, undefined), false, response_http_body) of
						undefined ->
							enforcement_failed (response_http_body, embedded_without_content, none);
						DecodedBody_ ->
							{ok, DecodedBody_}
					end;
				_ ->
					enforcement_failed (response_http_body, embedded_with_following_data, none)
			end;
		<<"following">> ->
			case EncodedBody of
				none ->
					enforcement_failed (response_http_body, following_without_following_data, none);
				DecodedBody_ ->
					{ok, DecodedBody_}
			end;
		BodyType ->
			enforcement_failed (response_http_body_type, unknown_value, BodyType)
	end,
	
	{ok, JsonEncodedHttpHeaders_} = case proplists:get_value (<<"http-headers">>, JsonHeaders, undefined) of
		{obj, JsonEncodedHttpHeaders__} ->
			{ok, JsonEncodedHttpHeaders__};
		JsonEncodedHttpHeadersValue_ ->
			enforcement_failed (response_http_headers, invalid_value, JsonEncodedHttpHeadersValue_)
	end,
	
	JsonEncodedHttpHeaders = lists:map (
				fun
					({Name, Value}) ->
						{
							enforce_string (Name, true, response_http_header_name),
							enforce_string (Value, true, response_http_header_value)};
					(Value) ->
						enforcement_failed (response_http_header, invalid_value, Value)
				end,
				JsonEncodedHttpHeaders_),
	
	Response
		= #?response{
			http_version = enforce_string (proplists:get_value (<<"http-version">>, JsonHeaders, undefined), true, response_http_version),
			http_code = enforce_positive_integer (proplists:get_value (<<"http-code">>, JsonHeaders, undefined), true, response_http_code),
			http_status = enforce_string (proplists:get_value (<<"http-status">>, JsonHeaders, undefined), true, response_http_status),
			http_headers = JsonEncodedHttpHeaders,
			http_body = DecodedBody},
	
	{ok, Response, CallbackIdentifier}.


enforce_string (List, _, Context) when is_list (List) ->
	ok = lists:foreach (
				fun (Value) ->
					if
						is_number (Value), Value >= 0, Value =< 255 ->
							ok;
						true ->
							enforcement_failed (Context, invalid_value, List)
					end
				end, List),
	erlang:list_to_binary (List);
enforce_string (Binary, _, _) when is_binary (Binary) ->
	Binary;
enforce_string (undefined, false, _) ->
	undefined;
enforce_string (Value, _, Context) ->
	enforcement_failed (Context, invalid_value, Value).


enforce_positive_integer (Number, _, _) when is_integer (Number), Number > 0 ->
	Number;
enforce_positive_integer (undefined, false, _) ->
	undefined;
enforce_positive_integer (Value, _, Context) ->
	enforcement_failed (Context, invalid_value, Value).


enforce_ip ({Byte1, Byte2, Byte3, Byte4}, _, _)
		when
			is_number (Byte1), Byte1 >= 0, Byte1 =< 255,
			is_number (Byte2), Byte2 >= 0, Byte2 =< 255,
			is_number (Byte3), Byte3 >= 0, Byte3 =< 255,
			is_number (Byte4), Byte4 >= 0, Byte4 =< 255 ->
	[<<"ip4">>, Byte1, Byte2, Byte3, Byte4];
enforce_ip (undefined, false, _) ->
	undefined;
enforce_ip (Value, _, Context) ->
	enforcement_failed (Context, invalid_value, Value).


enforce_port (Port, _, _) when is_number (Port), Port >= 0, Port =< 65535 ->
	Port;
enforce_port (undefined, false, _) ->
	undefined;
enforce_port (Value, _, Context) ->
	enforcement_failed (Context, invalid_value, Value).


enforce_list (List, _, _) when is_list (List) ->
	List;
enforce_list (undefined, false, _) ->
	undefined;
enforce_list (Value, _, Context) ->
	enforcement_failed (Context, invalid_value, Value).


enforce_iolist (List, _, Context) when is_list (List) ->
	ok = lists:foreach (
				fun (Value) ->
					if
						is_number (Value), Value >= 0, Value =< 255 -> ok;
						is_list (Value) -> enforce_iolist (Value, true, Context), ok;
						is_binary (Value) -> ok;
						true -> enforce_iolist (Value, true, Context)
					end
				end, List),
	erlang:iolist_to_binary (List);
enforce_iolist (Value, _, _) when is_binary (Value) ->
	Value;
enforce_iolist (undefined, false, _) ->
	undefined;
enforce_iolist (Value, _, Context) ->
	enforcement_failed (Context, invalid_value, Value).


enforcement_failed (Context, Reason, Value) ->
	erlang:throw ({enforcement_failed, Context, Reason, Value}).
