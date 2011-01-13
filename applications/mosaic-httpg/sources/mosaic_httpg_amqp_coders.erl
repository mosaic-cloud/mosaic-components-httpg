
-module (mosaic_httpg_amqp_coders).

-export ([encode_request_routing_key/2, encode_request_message_body/4]).
-export ([decode_response_message_body/3]).


-include ("mosaic_httpg.hrl").

-define (request, mosaic_http_request).
-define (response, mosaic_http_response).


encode_request_routing_key (_Request, CallbackIdentifier) ->
	
	RoutingKey = erlang:iolist_to_binary (enforce_string (CallbackIdentifier, true)),
	
	{ok, RoutingKey}.


encode_request_message_body (Request, CallbackIdentifier, CallbackExchange, CallbackRoutingKey) ->
	
	EncodedBody = enforce_iolist (Request#?request.http_body, true),
	EncodedBodySize = erlang:iolist_size (EncodedBody),
	EmptyBody = (EncodedBodySize == 0),
	EmbeddedBody = ((EncodedBodySize > 0) and (EncodedBodySize =< 1024)),
	FollowingBody = (EncodedBodySize > 1024),
	
	JsonHeaders = {obj,
			lists:filter (
				fun ({_Name, Value}) -> Value /= undefined end,
				[
					{<<"version">>, 1},
					{<<"callback-identifier">>, enforce_string (CallbackIdentifier, true)},
					{<<"callback-exchange">>, enforce_string (CallbackExchange, true)},
					{<<"callback-routing-key">>, enforce_string (CallbackRoutingKey, true)},
					{<<"socket-remote-ip">>, enforce_ip (Request#?request.socket_remote_ip, false)},
					{<<"socket-remote-port">>, enforce_port (Request#?request.socket_remote_port, false)},
					{<<"socket-remote-fqdn">>, enforce_string (Request#?request.socket_remote_fqdn, false)},
					{<<"socket-local-ip">>, enforce_ip (Request#?request.socket_local_ip, false)},
					{<<"socket-local-port">>, enforce_port (Request#?request.socket_local_port, false)},
					{<<"socket-local-fqdn">>, enforce_string (Request#?request.socket_local_fqdn, false)},
					{<<"http-version">>, enforce_string (Request#?request.http_version, true)},
					{<<"http-method">>, enforce_string (Request#?request.http_method, true)},
					{<<"http-uri">>, enforce_string (Request#?request.http_uri, true)},
					{<<"http-headers">>, {obj,
						lists:map (
							fun ({Name, Value}) -> {enforce_string (Name, true), enforce_string (Value, true)} end,
							enforce_list (Request#?request.http_headers, false))}},
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
	
	EncodedHeaders = rfc4627:encode (JsonHeaders),
	EncodedHeadersSize = erlang:iolist_size (EncodedHeaders),
	
	if
		EmptyBody; EmbeddedBody ->
			Data = erlang:iolist_to_binary ([<<EncodedHeadersSize : 32/big-unsigned-integer-unit:1>>, EncodedHeaders]);
		FollowingBody ->
			Data = erlang:iolist_to_binary ([<<EncodedHeadersSize : 32/big-unsigned-integer-unit:1>>, EncodedHeaders,
					<<EncodedBodySize : 32/big-unsigned-integer-unit:1>>, EncodedBody])
	end,
	
	ContentType = <<"application/octet-stream">>,
	ContentEncoding = <<"binary">>,
	
	{ok, Data, ContentType, ContentEncoding}.


decode_response_message_body (Body, ContentType, ContentEncoding)
		when ContentType == <<"application/octet-stream">>, ContentType == <<"application/octet-stream">> ->
	
	{EncodedHeadersSize, EncodedHeaders, EncodedBodySize, EncodedBody}
	= case Body of
		<<EncodedHeadersSize_ : 32/big-unsigned-integer-unit:1, EncodedHeaders_ : EncodedHeadersSize_ / binary>> ->
			{EncodedHeadersSize_, EncodedHeaders_, none, none};
		<<EncodedHeadersSize_ : 32/big-unsigned-integer-unit:1, EncodedHeaders_ : EncodedHeadersSize_ / binary,
				EncodedBodySize_ : 32/big-unsigned-integer-unit:1, EncodedBody_ : EncodedBodySize_ / binary>> ->
			{EncodedHeadersSize_, EncodedHeaders_, EncodedBodySize_, EncodedBody_}
	end,
	
	{ok, {obj, JsonHeaders_}, []} = rfc4627:decode (EncodedHeaders),
	JsonHeaders =
			lists:map (
				fun
					({Name, Value}) when is_number (Value); is_binary (Value); Value == true; Value == false; Value == none
						-> {enforce_string (Name, true), Value};
					({Name, Value}) when is_list (Value) -> {enforce_string (Name, true), enforce_string (Value, true)};
					({Name, {obj, Value}}) -> {enforce_string (Name, true), {obj, Value}}
				end,
				JsonHeaders_),
	
	% 1 = proplists:get_value (<<"version">>, JsonHeaders, undefined),
	
	CallbackIdentifier = enforce_string (proplists:get_value (<<"callback-identifier">>, JsonHeaders, undefined), true),
	
	case enforce_string (proplists:get_value (<<"http-body">>, JsonHeaders, undefined), true) of
		<<"empty">> ->
			if
				EncodedBodySize == none ->
					DecodedBody = <<"">>
			end;
		<<"embedded">> ->
			if
				EncodedBodySize == none ->
					DecodedBody = enforce_string (proplists:get_value (<<"http-body-content">>, JsonHeaders, undefined), true)
			end;
		<<"following">> ->
			if
				is_number (EncodedBodySize), EncodedBodySize >= 0 ->
					DecodedBody = EncodedBody
			end
	end,
	
	{obj, JsonEncodedHttpHeaders_} = proplists:get_value (<<"http-headers">>, JsonHeaders, undefined),
	JsonEncodedHttpHeaders =
			lists:map (
				fun ({Name, Value}) -> {enforce_string (Name, true), enforce_string (Value, true)} end,
				JsonEncodedHttpHeaders_),
	
	Response = #?response{
			http_version = enforce_string (proplists:get_value (<<"http-version">>, JsonHeaders, undefined), true),
			http_code = enforce_positive_integer (proplists:get_value (<<"http-code">>, JsonHeaders, undefined), true),
			http_status = enforce_string (proplists:get_value (<<"http-status">>, JsonHeaders, undefined), true),
			http_headers =
					lists:map (
						fun ({Name, Value}) -> {enforce_string (Name, true), enforce_string (Value, true)} end,
						enforce_list (JsonEncodedHttpHeaders, true)),
			http_body = DecodedBody},
	
	{ok, Response, CallbackIdentifier}.


enforce_string (String, _)
		when is_list (String) ->
	lists:foreach (fun (Character) -> if is_number (Character), (Character > 0) -> ok end end, String),
	erlang:list_to_binary (String);
enforce_string (String, _)
		when is_binary (String) ->
	String;
enforce_string (undefined, false) ->
	undefined.

enforce_positive_integer (Number, _)
		when is_integer (Number), Number > 0 ->
	Number;
enforce_positive_integer (undefined, false) ->
	undefined.


enforce_ip ({Byte1, Byte2, Byte3, Byte4}, _)
		when Byte1 >= 0, Byte1 =< 255, Byte2 >= 0, Byte2 =< 255, Byte3 >= 0, Byte3 =< 255, Byte4 >= 0, Byte4 =< 255 ->
	[<<"ip4">>, Byte1, Byte2, Byte3, Byte4];
enforce_ip (undefined, false) ->
	undefined.

enforce_port (Port, _)
		when Port >= 0, Port =< 65535 ->
	Port;
enforce_port (undefined, false) ->
	undefined.

enforce_list (List, _)
		when is_list (List) ->
	List.

enforce_iolist (Value, _) ->
	Value.
