
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
						lists:filter (
							fun ({_Name, Value}) -> Value /= undefined end,
							lists:map (
								fun ({Name, Value}) -> {enforce_string (Name, true), enforce_string (Value, true)} end,
								enforce_list (Request#?request.http_headers, false)))}},
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


decode_response_message_body (_Body, ContentType, ContentEncoding)
		when ContentType == <<"application/octet-stream">>, ContentEncoding == <<"binary">> ->
	
	Response = undefined,
	CallbackIdentifier = undefined,
	
	{ok, Response, CallbackIdentifier}.


enforce_string (String, _)
		when is_list (String) ->
	lists:foreach (fun (Character) -> if is_number (Character), (Character > 0) -> ok end end, String),
	String;
enforce_string (String, _)
		when is_binary (String) ->
	String;
enforce_string (undefined, false) ->
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
