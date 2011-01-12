
-module (mosaic_httpg_amqp_coders).

-export ([encode_request_routing_key/2, encode_request_message_body/2]).
-export ([decode_response_routing_key/1, decode_response_message_body/1]).


-include ("mosaic_httpg.hrl").

-define (request, mosaic_http_request).
-define (response, mosaic_http_response).


encode_request_routing_key (_Request, _Correlation) ->
	RoutingKey = <<"">>,
	{ok, RoutingKey}.

encode_request_message_body (_Request, _Correlation) ->
	Body = <<"">>,
	{ok, Body}.

decode_response_routing_key (_RoutingKey) ->
	Correlation = erlang:make_ref (),
	{ok, Correlation}.

decode_response_message_body (_Body) ->
	Response = #?response{},
	{ok, Response}.
