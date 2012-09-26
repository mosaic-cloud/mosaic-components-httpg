
-module (mosaic_httpg_callbacks).

-behaviour (mosaic_component_callbacks).


-export ([configure/0, standalone/0]).
-export ([init/0, terminate/2, handle_call/5, handle_cast/4, handle_info/2]).


-import (mosaic_enforcements, [enforce_ok/1, enforce_ok_1/1, enforce_ok_2/1]).


-record (state, {status, identifier, group, broker_group, gateway_socket, broker_socket, amqp_dispatcher, misultin_adapter}).


init () ->
	try
		State = #state{
					status = waiting_initialize,
					identifier = none, group = none, broker_group = none,
					gateway_socket = none, broker_socket = none,
					amqp_dispatcher = none, misultin_adapter = none},
		erlang:self () ! {mosaic_httpg_callbacks_internals, trigger_initialize},
		{ok, State}
	catch throw : {error, Reason} -> {stop, Reason} end.


terminate (_Reason, _State = #state{}) ->
	ok.


handle_call (<<"mosaic-httpg:get-gateway-endpoint">>, null, <<>>, _Sender, State = #state{status = executing, gateway_socket = Socket}) ->
	{SocketIp, SocketPort, SocketFqdn} = Socket,
	Outcome = {ok, {struct, [
					{<<"ip">>, SocketIp}, {<<"port">>, SocketPort}, {<<"fqdn">>, SocketFqdn},
					{<<"url">>, erlang:iolist_to_binary (["http://", SocketFqdn, ":", erlang:integer_to_list (SocketPort), "/"])}
				]}, <<>>},
	{reply, Outcome, State};
	
handle_call (<<"mosaic-httpg:get-node-identifier">>, null, <<>>, _Sender, State) ->
	Outcome = {ok, erlang:atom_to_binary (erlang:node (), utf8), <<>>},
	{reply, Outcome, State};
	
handle_call (Operation, Inputs, _Data, _Sender, State = #state{status = executing}) ->
	ok = mosaic_transcript:trace_error ("received invalid call request; ignoring!", [{operation, Operation}, {inputs, Inputs}]),
	{reply, {error, {invalid_operation, Operation}}, State};
	
handle_call (Operation, Inputs, _Data, _Sender, State = #state{status = Status})
		when (Status =/= executing) ->
	ok = mosaic_transcript:trace_error ("received invalid call request; ignoring!", [{operation, Operation}, {inputs, Inputs}, {status, Status}]),
	{reply, {error, {invalid_status, Status}}, State}.


handle_cast (Operation, Inputs, _Data, State = #state{status = executing}) ->
	ok = mosaic_transcript:trace_error ("received invalid cast request; ignoring!", [{operation, Operation}, {inputs, Inputs}]),
	{noreply, State};
	
handle_cast (Operation, Inputs, _Data, State = #state{status = Status})
		when (Status =/= executing) ->
	ok = mosaic_transcript:trace_error ("received invalid cast request; ignoring!", [{operation, Operation}, {inputs, Inputs}, {status, Status}]),
	{noreply, State}.


handle_info ({mosaic_httpg_callbacks_internals, trigger_initialize}, OldState = #state{status = waiting_initialize}) ->
	try
		Identifier = enforce_ok_1 (mosaic_generic_coders:application_env_get (identifier, mosaic_httpg,
					{decode, fun mosaic_component_coders:decode_component/1}, {error, missing_identifier})),
		Group = enforce_ok_1 (mosaic_generic_coders:application_env_get (group, mosaic_httpg,
					{decode, fun mosaic_component_coders:decode_group/1}, {error, missing_group})),
		BrokerGroup = enforce_ok_1 (mosaic_generic_coders:application_env_get (broker_group, mosaic_httpg,
					{decode, fun mosaic_component_coders:decode_group/1}, {error, missing_broker_group})),
		ok = enforce_ok (mosaic_component_callbacks:acquire_async (
					[{<<"gateway_socket">>, <<"socket:ipv4:tcp-service:http">>}],
					{mosaic_httpg_callbacks_internals, acquire_return})),
		NewState = OldState#state{status = waiting_acquire_return, identifier = Identifier, group = Group, broker_group = BrokerGroup},
		{noreply, NewState}
	catch throw : Error = {error, _Reason} -> {stop, Error, OldState} end;
	
handle_info ({{mosaic_httpg_callbacks_internals, acquire_return}, Outcome}, OldState = #state{status = waiting_acquire_return, broker_group = BrokerGroup}) ->
	try
		Descriptors = enforce_ok_1 (Outcome),
		[GatewaySocket] = enforce_ok_1 (mosaic_component_coders:decode_socket_ipv4_tcp_descriptors (
					[<<"gateway_socket">>], Descriptors)),
		ok = enforce_ok (mosaic_component_callbacks:call_async (
					BrokerGroup, <<"mosaic-rabbitmq:get-broker-endpoint">>, null, <<>>,
					{mosaic_httpg_callbacks_internals, resolve_return})),
		NewState = OldState#state{status = waiting_resolve_return, gateway_socket = GatewaySocket},
		{noreply, NewState}
	catch throw : Error = {error, _Reason} -> {stop, Error, OldState} end;
	
handle_info ({{mosaic_httpg_callbacks_internals, resolve_return}, Outcome}, OldState = #state{status = waiting_resolve_return, identifier = Identifier, group = Group, gateway_socket = GatewaySocket}) ->
	try
		BrokerAttributes = case enforce_ok_2 (Outcome) of
			{{struct, BrokerAttributes_}, <<>>} -> BrokerAttributes_;
			{Outputs, _Data} -> throw ({error, {invalid_broker_endpoint_outputs, Outputs}})
		end,
		ok = case enforce_ok_1 (mosaic_generic_coders:proplist_get (<<"transport">>, BrokerAttributes,
					{decode, fun mosaic_generic_coders:decode_string/1}, {error, {invalid_broker_endpoint, BrokerAttributes, missing_transport}})) of
			<<"tcp">> -> ok;
			_ -> throw ({error, {invalid_broker_endpoint, BrokerAttributes, invalid_transport}})
		end,
		BrokerSocketIp = enforce_ok_1 (mosaic_generic_coders:proplist_get (<<"ip">>, BrokerAttributes,
					{decode, fun mosaic_generic_coders:decode_string/1}, {error, {invalid_broker_endpoint, BrokerAttributes, missing_ip}})),
		BrokerSocketPort = enforce_ok_1 (mosaic_generic_coders:proplist_get (<<"port">>, BrokerAttributes,
					{validate, {is_integer, {invalid_broker_endpoint, BrokerAttributes, invalid_port}}},
					{error, {invalid_broker_endpoint, BrokerAttributes, missing_port}})),
		BrokerSocket = {BrokerSocketIp, BrokerSocketPort},
		ok = enforce_ok (setup_applications (Identifier, GatewaySocket, BrokerSocket)),
		{AmqpDispatcher, MisultinAdapter} = enforce_ok_2 (start_applications ()),
		ok = enforce_ok (mosaic_component_callbacks:register_async (Group, {mosaic_httpg_callbacks_internals, register_return})),
		NewState = OldState#state{status = waiting_register_return, broker_socket = BrokerSocket, amqp_dispatcher = AmqpDispatcher, misultin_adapter = MisultinAdapter},
		{noreply, NewState}
	catch throw : Error = {error, _Reason} -> {stop, Error, OldState} end;
	
handle_info ({{mosaic_httpg_callbacks_internals, register_return}, Outcome}, OldState = #state{status = waiting_register_return}) ->
	try
		ok = enforce_ok (Outcome),
		NewState = OldState#state{status = executing},
		{noreply, NewState}
	catch throw : Error = {error, _Reason} -> {stop, Error, OldState} end;
	
handle_info (Message, State = #state{status = Status}) ->
	ok = mosaic_transcript:trace_error ("received invalid message; terminating!", [{message, Message}, {status, Status}]),
	{stop, {error, {invalid_message, Message}}, State}.


standalone () ->
	mosaic_application_tools:boot (fun standalone_1/0).

standalone_1 () ->
	try
		ok = enforce_ok (load_applications ()),
		ok = enforce_ok (mosaic_component_callbacks:configure ([{identifier, mosaic_httpg}])),
		Identifier = enforce_ok_1 (mosaic_generic_coders:application_env_get (identifier, mosaic_httpg,
					{decode, fun mosaic_component_coders:decode_component/1}, {error, missing_identifier})),
		GatewaySocket = {<<"0.0.0.0">>, 20760, <<"127.0.0.1">>},
		BrokerSocket = {<<"127.0.0.1">>, 21688},
		ok = enforce_ok (setup_applications (Identifier, GatewaySocket, BrokerSocket)),
		{AmqpDispatcher, MisultinAdapter} = enforce_ok_2 (start_applications ()),
		Self = erlang:self (),
		_ = erlang:spawn_link (
					fun () ->
						true = erlang:unlink (Self),
						_ = mosaic_process_tools:wait (AmqpDispatcher),
						_ = mosaic_application_tools:shutdown_async (0)
					end),
		_ = erlang:spawn_link (
					fun () ->
						true = erlang:unlink (Self),
						_ = mosaic_process_tools:wait (MisultinAdapter),
						_ = mosaic_application_tools:shutdown_async (0)
					end),
		ok
	catch throw : Error = {error, _Reason} -> Error end.


configure () ->
	try
		ok = enforce_ok (load_applications ()),
		ok = enforce_ok (mosaic_component_callbacks:configure ([
					{identifier, mosaic_httpg},
					{group, mosaic_httpg},
					harness])),
		ok
	catch throw : Error = {error, _Reason} -> Error end.


load_applications () ->
	try
		ok = enforce_ok (mosaic_application_tools:load (mosaic_httpg, without_dependencies)),
		ok = enforce_ok (mosaic_application_tools:load ([rabbit_common, amqp_client, misultin], without_dependencies)),
		ok
	catch throw : Error = {error, _Reason} -> Error end.


setup_applications (Identifier, GatewaySocket, BrokerSocket) ->
	try
		IdentifierString = enforce_ok_1 (mosaic_component_coders:encode_component (Identifier)),
		{GatewaySocketIp, GatewaySocketPort, GatewaySocketFqdn} = GatewaySocket,
		{BrokerSocketIp, BrokerSocketPort} = BrokerSocket,
		GatewaySocketIpString = erlang:binary_to_list (GatewaySocketIp),
		GatewaySocketFqdnString = erlang:binary_to_list (GatewaySocketFqdn),
		BrokerSocketIpString = erlang:binary_to_list (BrokerSocketIp),
		ok = enforce_ok (mosaic_component_callbacks:configure ([
					{env, mosaic_httpg, gateway_ip, GatewaySocketIpString},
					{env, mosaic_httpg, gateway_port, GatewaySocketPort},
					{env, mosaic_httpg, broker_ip, BrokerSocketIpString},
					{env, mosaic_httpg, broker_port, BrokerSocketPort}])),
		ok = error_logger:info_report (["Configuring mOSAIC HTTP-gateway component...",
					{identifier, IdentifierString},
					{url, erlang:list_to_binary ("http://" ++ GatewaySocketFqdnString ++ ":" ++ erlang:integer_to_list (GatewaySocketPort) ++ "/")},
					{gateway_endpoint, GatewaySocket}, {broker_endpoint, BrokerSocket}]),
		ok
	catch throw : Error = {error, _Reason} -> Error end.


start_applications () ->
	try
		ok = enforce_ok (mosaic_application_tools:start ([rabbit_common, amqp_client, misultin], without_dependencies)),
		AmqpDispatcher = enforce_ok_1 (mosaic_httpg_amqp_dispatcher:start_link ({local, mosaic_httpg_dispatcher}, defaults, [])),
		MisultinAdapter = enforce_ok_1 (mosaic_httpg_misultin_adapter:start_link ({local, mosaic_httpg_gateway}, defaults, [])),
		{ok, AmqpDispatcher, MisultinAdapter}
	catch throw : Error = {error, _Reason} -> Error end.
