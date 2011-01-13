
-module (mosaic_httpg_amqp_dispatcher).

-behavior (gen_server).

-export ([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export ([configure/0]).


-include_lib ("amqp_client/include/amqp_client.hrl").

-include ("mosaic_httpg.hrl").

-define (configuration, mosaic_httpg_amqp_dispatcher_configuration).
-define (state, mosaic_httpg_amqp_dispatcher_state).
-define (request, mosaic_http_request).
-define (response, mosaic_http_response).

-record(?state, {
		configuration,
		callbacks_table,
		connection,
		requests_channel,
		responses_channel, responses_queue}).


init (Configuration) ->
	
	State_0 = #?state{
			configuration = Configuration,
			callbacks_table = none,
			connection = none,
			requests_channel = none,
			responses_channel = none, responses_queue = none},
	
	{ok, State_1} = ets_init (State_0),
	{ok, State_2} = amqp_init (State_1),
	
	{ok, State_2}.


terminate (_Reason, State_0) ->
	
	{ok, State_1} = amqp_terminate (State_0),
	{ok, _State_2} = ets_terminate (State_1),
	
	ok.


code_change (_OldVsn, State, _Data) ->
	
	{ok, State}.


handle_call (Call, _Caller, State_0) ->
	
	case Call of
		
		{dispatch, Request = #?request{}, Callback} ->
			{ok, State_1, CallbackIdentifier} = handle_dispatch_request (State_0, Request, Callback),
			{reply, {ok, CallbackIdentifier}, State_1}
	end.


handle_cast (_Cast, State) ->
	
	{noreply, State}.


handle_info (Message, State_0) ->
	
	RequestsExchange = (State_0#?state.configuration)#?configuration.requests_exchange,
	ResponsesQueue = State_0#?state.responses_queue,
	
	case Message of
		
		#'basic.consume_ok'{consumer_tag = ResponsesQueue} ->
			{noreply, State_0};
		
		{Deliver = #'basic.deliver'{consumer_tag = ResponsesQueue}, DeliverMessage} ->
			{ok, State_1} = amqp_consume_response (State_0, Deliver, DeliverMessage),
			{noreply, State_1};
		
		#'basic.return'{exchange = RequestsExchange} ->
			{noreply, State_0}
	end.


handle_dispatch_request (State_0, Request, Callback) ->
	
	{ok, State_1, CallbackIdentifier} = ets_register_callback (State_0, Callback),
	{ok, State_2} = amqp_publish_request (State_1, Request, CallbackIdentifier),
	
	{ok, State_2, CallbackIdentifier}.


handle_dispatch_response (State_0, Response, CallbackIdentifier) ->
	
	{ok, State_1, Callback} = ets_resolve_callback (State_0, CallbackIdentifier),
	{ok, State_2} = ets_unregister_callback (State_1, CallbackIdentifier),
	Callback ! {dispatch, Response, CallbackIdentifier},
	
	{ok, State_2}.


amqp_init (State_0) ->
	
	{ok, State_1} = amqp_connect (State_0),
	{ok, State_2} = amqp_declare (State_1),
	{ok, State_3} = amqp_subscribe (State_2),
	{ok, State_3}.


amqp_terminate (State_0) ->
	
	{ok, State_1} = amqp_unsubscribe (State_0),
	{ok, State_2} = amqp_disconnect (State_1),
	{ok, State_2}.


amqp_connect (State_0 = #?state{configuration = Configuration})
		when State_0#?state.connection == none ->
	
	ConnectionParameters = #amqp_params{
			host = Configuration#?configuration.broker_host,
			port = Configuration#?configuration.broker_port,
			virtual_host = Configuration#?configuration.broker_virtual_host,
			username = Configuration#?configuration.broker_user,
			password = Configuration#?configuration.broker_password},
	
	{ok, Connection} = amqp_connection:start (network, ConnectionParameters),
	
	State_1 = State_0#?state{connection = Connection},
	
	{ok, State_1}.


amqp_disconnect (State_0 = #?state{connection = Connection})
		when Connection /= none, Connection /= closed ->
	
	ok = amqp_connection:close (Connection),
	
	State_1 = State_0#?state{connection = closed},
	
	{ok, State_1}.


amqp_declare (State_0 = #?state{configuration = Configuration, connection = Connection})
		when
			Connection /= none, Connection /= closed,
			State_0#?state.responses_queue == none ->
	
	RequestsExchangeDeclare = #'exchange.declare'{
			exchange = Configuration#?configuration.requests_exchange,
			type = <<"topic">>, durable = false, auto_delete = false},
	
	ResponsesExchangeDeclare = #'exchange.declare'{
			exchange = Configuration#?configuration.responses_exchange,
			type = <<"direct">>, durable = false, auto_delete = false},
	
	ResponsesQueueDeclare = #'queue.declare'{
			queue = <<"">>, durable = false, exclusive = true, auto_delete = false},
	
	{ok, Channel} = amqp_connection:open_channel (Connection, none),
	
	#'exchange.declare_ok'{} = amqp_channel:call (Channel, RequestsExchangeDeclare),
	#'exchange.declare_ok'{} = amqp_channel:call (Channel, ResponsesExchangeDeclare),
	#'queue.declare_ok'{queue = ResponsesQueue} = amqp_channel:call (Channel, ResponsesQueueDeclare),
	
	ResponsesQueueBind = #'queue.bind'{
			queue = ResponsesQueue, exchange = Configuration#?configuration.responses_exchange, routing_key = ResponsesQueue},
	
	#'queue.bind_ok'{} = amqp_channel:call (Channel, ResponsesQueueBind),
	
	ok = amqp_channel:close (Channel),
	
	State_1 = State_0#?state{responses_queue = ResponsesQueue},
	
	{ok, State_1}.


amqp_subscribe (State_0 = #?state{connection = Connection, responses_queue = ResponsesQueue})
		when
			Connection /= none, Connection /= closed,
			ResponsesQueue /= none, ResponsesQueue /= destroyed,
			State_0#?state.requests_channel == none,
			State_0#?state.responses_channel == none ->
	
	ResponsesSubscribe = #'basic.consume'{
			queue = ResponsesQueue, consumer_tag = ResponsesQueue, exclusive = true},
	
	{ok, RequestsChannel} = amqp_connection:open_channel (Connection, none),
	ok = amqp_channel:register_return_handler (RequestsChannel, erlang:self ()),
	
	{ok, ResponsesChannel} = amqp_connection:open_channel (Connection, none),
	ok = amqp_channel:register_return_handler (ResponsesChannel, erlang:self ()),
	
	#'basic.consume_ok'{consumer_tag = ResponsesQueue}
			= amqp_channel:subscribe (ResponsesChannel, ResponsesSubscribe, self ()),
	
	State_1 = State_0#?state{
			requests_channel = RequestsChannel,
			responses_channel = ResponsesChannel},
	
	{ok, State_1}.


amqp_unsubscribe (State_0 = #?state{requests_channel = RequestsChannel, responses_channel = ResponsesChannel, responses_queue = ResponsesQueue})
		when
			RequestsChannel /= none, RequestsChannel /= closed,
			ResponsesChannel /= none, ResponsesChannel /= closed,
			ResponsesQueue /= none, ResponsesQueue /= destroyed ->
	
	ok = amqp_channel:close (RequestsChannel),
	ok = amqp_channel:close (ResponsesChannel),
	
	State_1 = State_0#?state{
			requests_channel = closed,
			responses_channel = closed, responses_queue = destroyed},
	
	{ok, State_1}.


amqp_publish_request (State = #?state{configuration = Configuration, requests_channel = RequestsChannel}, Request, CallbackIdentifier)
		when RequestsChannel /= none, RequestsChannel /= closed ->
	
	{ok, RoutingKey} = (Configuration#?configuration.request_routing_key_encoder) (
			Request, CallbackIdentifier),
	{ok, MessageBody, MessageContentType, MessageContentEncoding} = (Configuration#?configuration.request_message_body_encoder) (
			Request, CallbackIdentifier, Configuration#?configuration.responses_exchange, State#?state.responses_queue),
	
	Publish = #'basic.publish'{
			exchange = Configuration#?configuration.requests_exchange,
			routing_key = RoutingKey, mandatory = true, immediate = false},
	
	Message = #amqp_msg{
			payload = MessageBody,
			props = #'P_basic'{
				content_type = MessageContentType,
				content_encoding = MessageContentEncoding}},
	
	ok = amqp_channel:call (RequestsChannel, Publish, Message),
	
	{ok, State}.


amqp_consume_response (State_0 = #?state{configuration = Configuration}, Deliver, Message) ->
	
	#'basic.deliver'{delivery_tag = DeliveryTag} = Deliver,
	
	#amqp_msg{
				payload = MessageBody,
				props = #'P_basic'{content_type = MessageContentType, content_encoding = MessageContentEncoding}}
			= Message,
	
	{ok, Response, CallbackIdentifier} = (Configuration#?configuration.response_message_body_decoder) (
			MessageBody, MessageContentType, MessageContentEncoding),
	
	{ok, State_1} = handle_dispatch_response (State_0, Response, CallbackIdentifier),
	
	Acknowledge = #'basic.ack'{delivery_tag = DeliveryTag},
	
	ok = amqp_channel:call (State_1#?state.responses_channel, Acknowledge),
	
	{ok, State_1}.


ets_init (State_0 = #?state{configuration = Configuration})
		when State_0#?state.callbacks_table == none ->
	
	CallbacksTable = ets:new (Configuration#?configuration.callbacks_table, [set, public]),
	
	State_1 = State_0#?state{callbacks_table = CallbacksTable},
	
	{ok, State_1}.


ets_terminate (State_0 = #?state{callbacks_table = CallbacksTable})
		when CallbacksTable /= none, CallbacksTable /= destroyed ->
	
	true = ets:delete (CallbacksTable),
	
	State_1 = State_0#?state{callbacks_table = destroyed},
	
	{ok, State_1}.


ets_register_callback (State = #?state{callbacks_table = CallbacksTable}, Callback)
		when CallbacksTable /= none, CallbacksTable /= destroyed ->
	
	CallbackIdentifier = erlang:list_to_binary (uuid:to_string (uuid:random ())),
	
	true = ets:insert_new (CallbacksTable, {CallbackIdentifier, Callback}),
	
	{ok, State, CallbackIdentifier}.


ets_unregister_callback (State = #?state{callbacks_table = CallbacksTable}, CallbackIdentifier)
		when CallbacksTable /= none, CallbacksTable /= destroyed ->
	
	true = ets:delete (CallbacksTable, CallbackIdentifier),
	
	{ok, State}.


ets_resolve_callback (State = #?state{callbacks_table = CallbacksTable}, CallbackIdentifier)
		when CallbacksTable /= none, CallbacksTable /= destroyed ->
	
	[{CallbackIdentifier, Callback}] = ets:lookup (CallbacksTable, CallbackIdentifier),
	
	{ok, State, Callback}.


configure () ->
	{ok, #?configuration{
		broker_host = "127.0.0.1", broker_port = 5672, broker_virtual_host = <<"/">>,
		broker_user = <<"guest">>, broker_password = <<"guest">>,
		requests_exchange = <<"mosaic-http-requests">>,
		responses_exchange = <<"mosaic-http-responses">>,
		callbacks_table = mosaic_httpg_dispatcher_callbacks,
		request_routing_key_encoder = {mosaic_httpg_amqp_coders, encode_request_routing_key},
		request_message_body_encoder = {mosaic_httpg_amqp_coders, encode_request_message_body},
		response_routing_key_decoder = {mosaic_httpg_amqp_coders, decode_response_routing_key},
		response_message_body_decoder = {mosaic_httpg_amqp_coders, decode_response_message_body}}}.
