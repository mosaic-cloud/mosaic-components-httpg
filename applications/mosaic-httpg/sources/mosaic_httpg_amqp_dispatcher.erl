
-module (mosaic_httpg_amqp_dispatcher).

-behavior (gen_server).

-export ([start/0, start/1, start/2, start/3, start_link/0, start_link/1, start_link/2, start_link/3]).
-export ([dispatch_request/3, cancel_callback/2, stop/1, stop/2]).
-export ([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export ([configure/0]).

-import (mosaic_enforcements, [enforce_ok_1/1]).


start () ->
	start (defaults).

start (Configuration) ->
	gen_server:start (?MODULE, Configuration, []).

start (Configuration, Options) ->
	gen_server:start (?MODULE, Configuration, Options).

start (Server, Configuration, Options) ->
	gen_server:start (Server, ?MODULE, Configuration, Options).


start_link () ->
	start_link (defaults).

start_link (Configuration) ->
	gen_server:start_link (?MODULE, Configuration, []).

start_link (Configuration, Options) ->
	gen_server:start_link (?MODULE, Configuration, Options).

start_link (Server, Configuration, Options) ->
	gen_server:start_link (Server, ?MODULE, Configuration, Options).


dispatch_request (Server, Request, Callback) ->
	gen_server:call (Server, {dispatch_request, Request, Callback}).

cancel_callback (Server, CallbackIdentifier) ->
	gen_server:call (Server, {cancel_callback, CallbackIdentifier}).

stop (Server) ->
	gen_server:call (Server, {stop, normal}).

stop (Server, Reason) ->
	gen_server:call (Server, {stop, Reason}).


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


init (defaults) ->
	
	case configure () of
		{ok, Configuration} ->
			init (Configuration);
		{error, Reason} ->
			{stop, Reason}
	end;
	
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


terminate (Reason, State_0) ->
	
	case Reason of
		normal ->
			ok;
		_ ->
			ok
	end,
	
	{ok, State_1} = amqp_terminate (State_0),
	{ok, _State_2} = ets_terminate (State_1),
	
	ok.


code_change (OldVsn, State, Extra) ->
	
	ErrorReport = [{error, unhandled_code_change}, {old_vsn, OldVsn}, {extra, Extra}],
	error_logger:error_report (ErrorReport),
	{ok, State}.


handle_call (Call, Caller, State_0) ->
	
	case Call of
		
		{dispatch_request, Request = #?request{}, Callback} ->
			{ok, State_1, CallbackIdentifier} = handle_dispatch_request (State_0, Request, Callback),
			{reply, {ok, CallbackIdentifier}, State_1};
		
		{dispatch_request, _, _} ->
			Error = {error, invalid_call}, ErrorReport = [Error, {call, Call}, {caller, Caller}],
			error_logger:error_report (ErrorReport),
			{reply, Error, State_0};
		
		{cancel_callback, _CallbackIdentifier} ->
			Error = {error, unhandled_call}, ErrorReport = [Error, {call, Call}, {caller, Caller}],
			error_logger:error_report (ErrorReport),
			{reply, Error, State_0};
		
		{stop, Reason} ->
			{stop, Reason, ok, State_0};
		
		_ ->
			Error = {error, unknown_call}, ErrorReport = [Error, {call, Call}, {caller, Caller}],
			error_logger:error_report (ErrorReport),
			{reply, Error, State_0}
	end.


handle_cast (Cast, State) ->
	
	case Cast of
		
		_ ->
			Error = {error, unknown_call}, ErrorReport = [Error, {cast, Cast}],
			error_logger:error_report (ErrorReport),
			{noreply, State}
	end.


handle_info (Message, State_0) ->
	
	RequestsExchange = (State_0#?state.configuration)#?configuration.requests_exchange,
	ResponsesQueue = State_0#?state.responses_queue,
	
	case Message of
		
		#'basic.consume_ok'{consumer_tag = ResponsesQueue} ->
			{noreply, State_0};
		
		Consume = #'basic.consume_ok'{} ->
			Error = {error, invalid_message}, ErrorReport = [Error, {reason, unexpected_consumer_tag}, {message, Consume}],
			error_logger:error_report (ErrorReport),
			{noreply, State_0};
		
		{Deliver = #'basic.deliver'{consumer_tag = ResponsesQueue}, DeliverMessage} ->
			{ok, State_1} = amqp_consume_response (State_0, Deliver, DeliverMessage),
			{noreply, State_1};
		
		{Deliver = #'basic.deliver'{}, DeliverMessage} ->
			Error = {error, invalid_message}, ErrorReport = [Error, {reason, unexpected_consumer_tag}, {message, Deliver, DeliverMessage}],
			error_logger:error_report (ErrorReport),
			{noreply, State_0};
		
		{Return = #'basic.return'{exchange = RequestsExchange}, ReturnMessage} ->
			{ok, State_1} = amqp_return_request (State_0, Return, ReturnMessage),
			{noreply, State_1};
		
		{Return = #'basic.return'{}, ReturnMessage} ->
			Error = {error, invalid_message}, ErrorReport = [Error, {reason, unexpected_exchange}, {message, Return, ReturnMessage}],
			error_logger:error_report (ErrorReport),
			{noreply, State_0};
		
		_ ->
			Error = {unknown_message}, ErrorReport = [Error, {message, Message}],
			error_logger:error_report (ErrorReport),
			{noreply, State_0}
	end.


handle_dispatch_request (State_0, Request, Callback) ->
	
	{ok, State_1, CallbackIdentifier} = ets_register_callback (State_0, Callback),
	{ok, State_2} = amqp_publish_request (State_1, CallbackIdentifier, Request),
	
	{ok, State_2, CallbackIdentifier}.


handle_dispatch_callback (State_0, CallbackIdentifier, CallbackOutcome) ->
	
	case ets_resolve_callback (State_0, CallbackIdentifier) of
		{ok, State_1, Callback} ->
			{ok, State_2} = ets_unregister_callback (State_1, CallbackIdentifier),
			Callback ! {dispatch_callback, CallbackIdentifier, CallbackOutcome};
		{error, State_1, undefined} ->
			State_2 = State_1,
			Error = {error, unresolved_callback}, ErrorReport = [Error, {callback_identifier, CallbackIdentifier}],
			error_logger:error_report (ErrorReport)
	end,
	
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
			username = Configuration#?configuration.broker_username,
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


amqp_publish_request (State = #?state{configuration = Configuration, requests_channel = RequestsChannel}, CallbackIdentifier, Request)
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
				content_encoding = MessageContentEncoding,
				correlation_id = CallbackIdentifier}},
	
	ok = amqp_channel:call (RequestsChannel, Publish, Message),
	
	{ok, State}.


amqp_return_request (State_0, _Return, Message) ->
	
	#amqp_msg{
			props = #'P_basic'{correlation_id = CallbackIdentifier}}
	= Message,
	
	{ok, State_1} = handle_dispatch_callback (State_0, CallbackIdentifier, {error, unhandled}),
	
	{ok, State_1}.


amqp_consume_response (State_0 = #?state{configuration = Configuration}, Deliver, Message) ->
	
	#'basic.deliver'{delivery_tag = DeliveryTag} = Deliver,
	
	#amqp_msg{
				payload = MessageBody,
				props = #'P_basic'{content_type = MessageContentType, content_encoding = MessageContentEncoding}}
	= Message,
	
	case (Configuration#?configuration.response_message_body_decoder) (
				MessageBody, MessageContentType, MessageContentEncoding) of
		
		{ok, Response, CallbackIdentifier} ->
			{ok, State_1} = handle_dispatch_callback (State_0, CallbackIdentifier, {ok, Response});
		
		{error, {decoding_failed, {enforcement_failed, Context, Reason, Value}}} ->
			Error = {error, decoding_failed}, ErrorReport = [Error, {reason, Reason}, {context, Context}, {value, Value}],
			error_logger:error_report (ErrorReport),
			State_1 = State_0
	end,
	
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
	
	case ets:lookup (CallbacksTable, CallbackIdentifier) of
		[{CallbackIdentifier, Callback}] ->
			{ok, State, Callback};
		[] ->
			{error, State, undefined}
	end.


configure () ->
	try
		Configuration = #?configuration{
				broker_host = enforce_ok_1 (mosaic_generic_coders:application_env_get (broker_ip, mosaic_httpg,
						{validate, {is_list, invalid_broker_ip}}, {error, missing_broker_ip})),
				broker_port = enforce_ok_1 (mosaic_generic_coders:application_env_get (broker_port, mosaic_httpg,
						{validate, {is_integer, invalid_broker_port}}, {error, missing_broker_port})),
				broker_virtual_host = enforce_ok_1 (mosaic_generic_coders:application_env_get (broker_virtual_host, mosaic_httpg,
						{validate, {is_binary, invalid_broker_virtual_host}}, {error, missing_broker_virtual_host})),
				broker_username = enforce_ok_1 (mosaic_generic_coders:application_env_get (broker_username, mosaic_httpg,
						{validate, {is_binary, invalid_broker_username}}, {error, missing_broker_username})),
				broker_password = enforce_ok_1 (mosaic_generic_coders:application_env_get (broker_password, mosaic_httpg,
						{validate, {is_binary, invalid_broker_password}}, {error, missing_broker_password})),
				requests_exchange = enforce_ok_1 (mosaic_generic_coders:application_env_get (gateway_requests_exchange, mosaic_httpg,
						{validate, {is_binary, invalid_gateway_requests_exchange}}, {error, missing_gateway_requests_exchange})),
				responses_exchange = enforce_ok_1 (mosaic_generic_coders:application_env_get (gateway_responses_exchange, mosaic_httpg,
						{validate, {is_binary, invalid_gateway_responses_exchange}}, {error, missing_gateway_responses_exchange})),
				callbacks_table = mosaic_httpg_dispatcher_callbacks,
				request_routing_key_encoder = {mosaic_httpg_amqp_coders, encode_request_routing_key},
				request_message_body_encoder = {mosaic_httpg_amqp_coders, encode_request_message_body},
				response_message_body_decoder = {mosaic_httpg_amqp_coders, decode_response_message_body}},
		{ok, Configuration}
	catch throw : {error, Reason} -> {error, {failed_amqp_dispatcher_configuration, Reason}} end.
