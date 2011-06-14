
-module (mosaic_httpg_callbacks).

-behaviour (mosaic_component_callbacks).


-export ([configure/0]).
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
	{SocketIp, SocketPort} = Socket,
	Outcome = {ok, {struct, [
					{<<"ip">>, SocketIp}, {<<"port">>, SocketPort},
					{<<"url">>, erlang:iolist_to_binary (["http://", SocketIp, ":", erlang:integer_to_list (SocketPort), "/"])}
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
					[{<<"gateway_socket">>, <<"socket:ipv4:tcp">>}],
					{mosaic_httpg_callbacks_internals, acquire_return})),
		NewState = OldState#state{status = waiting_acquire_return, identifier = Identifier, group = Group, broker_group = BrokerGroup},
		{noreply, NewState}
	catch throw : Error = {error, _Reason} -> {stop, Error, OldState} end;
	
handle_info ({{mosaic_httpg_callbacks_internals, acquire_return}, Outcome}, OldState = #state{status = waiting_acquire_return, broker_group = BrokerGroup}) ->
	try
		Descriptors = enforce_ok_1 (Outcome),
		[GatewaySocket] = enforce_ok_1 (mosaic_component_coders:decode_socket_ipv4_tcp_descriptors (
					[<<"gateway_socket">>], Descriptors)),
		{GatewaySocketIp, GatewaySocketPort} = GatewaySocket,
		GatewaySocketIpString = erlang:binary_to_list (GatewaySocketIp),
		ok = enforce_ok (mosaic_component_callbacks:configure ([
					{env, mosaic_httpg, gateway_ip, GatewaySocketIpString},
					{env, mosaic_httpg, gateway_port, GatewaySocketPort}])),
		ok = enforce_ok (mosaic_component_callbacks:call_async (
					BrokerGroup, <<"mosaic-rabbitmq:get-broker-endpoint">>, null, <<>>,
					{mosaic_httpg_callbacks_internals, resolve_return})),
		NewState = OldState#state{status = waiting_resolve_return, gateway_socket = GatewaySocket},
		{noreply, NewState}
	catch throw : Error = {error, _Reason} -> {stop, Error, OldState} end;
	
handle_info ({{mosaic_httpg_callbacks_internals, resolve_return}, Outcome}, OldState = #state{status = waiting_resolve_return, group = Group}) ->
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
		BrokerSocketIpString = erlang:binary_to_list (BrokerSocketIp),
		ok = enforce_ok (mosaic_component_callbacks:configure ([
					{env, mosaic_httpg, broker_ip, BrokerSocketIpString},
					{env, mosaic_httpg, broker_port, BrokerSocketPort}])),
		ok = enforce_ok (mosaic_component_callbacks:configure ([
				{start, [rabbit_common, amqp_client, misultin, mosaic_httpg], without_dependencies}])),
		AmqpDispatcher = enforce_ok_1 (mosaic_httpg_amqp_dispatcher:start_link ({local, mosaic_httpg_dispatcher}, defaults, [])),
		MisultinAdapter = enforce_ok_1 (mosaic_httpg_misultin_adapter:start_link ({local, mosaic_httpg_gateway}, defaults, [])),
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


configure () ->
	mosaic_component_callbacks:configure ([
				{load, [rabbit_common, amqp_client, misultin, mosaic_httpg], without_dependencies},
				{identifier, mosaic_httpg},
				{group, mosaic_httpg},
				harness]).
