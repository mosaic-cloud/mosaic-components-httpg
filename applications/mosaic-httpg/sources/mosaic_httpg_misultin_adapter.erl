
-module (mosaic_httpg_misultin_adapter).


-include ("mosaic_httpg.hrl").

-define (configuration, mosaic_httpg_misultin_adapter_configuration).
-define (state, mosaic_httpg_misultin_adapter_state).
-define (request, mosaic_http_request).
-define (response, mosaic_http_response).

-record(?state, {
		configuration :: #?configuration{},
		misultin :: process ()}).


-define (server_default_name, ?module).
-define (server_default_start_argument, defaults).
-define (server_default_start_options, []).
-define (server_default_stop_reason, normal).

-define (server_start_argument_spec, defaults | #?configuration{}).
-define (server_state_spec, #?state{}).

-include_lib ("vme/include/vme_gen_server_module.hrl").


?behaviour.

?export_behaviour.

?export_start_and_start_link_0.
?export_start_and_start_link_1.
?export_start_and_start_link_2.
?export_start_and_start_link_3.

?export_stop_0.
?export_stop_1.

-export ([configure/0, configure/1, handle_request/3]).


?start_spec.
?start_dsmao_default.
?start_link_default_spec.
?start_link_dsmao_default.

?start_a_default_spec.
?start_dsmo_a_default.
?start_link_a_default_spec.
?start_link_dsmo_a_default.

?start_ao_default_spec.
?start_dsm_ao_default.
?start_link_ao_default_spec.
?start_link_dsm_ao_default.

?start_sao_default_spec.
?start_dm_sao_default.

?start_link_sao_default_spec.
?start_link_dm_sao_default.

?stop_default_spec.
?stop_dsr_default.

?stop_s_default_spec.
?stop_dr_s_default.


?init_ok_or_stop_with_error_spec (?server_start_argument_spec, {misultin, any ()}, #?state{}).
?init_wrapper (Configuration_, begin
	
	{ok, Configuration} = case Configuration_ of
		#?configuration{} ->
			{ok, Configuration_};
		defaults ->
			{ok, configure ()}
	end,
	
	Self = erlang:self (),
	
	MisultinStart
			= misultin:start_link (
				lists:filter (
					fun ({_Name, Value}) -> Value /= undefined end,
					[
						{loop, fun (Session) -> handle_request (Configuration, Self, Session) end},
						{ip, Configuration#?configuration.ip},
						{port, Configuration#?configuration.port},
						{backlog, Configuration#?configuration.backlog},
						{recv_timeout, Configuration#?configuration.recv_timeout},
						{compress, Configuration#?configuration.compress}])),
	
	case MisultinStart of
		
		{ok, Misultin} ->
			?init_ok (#?state{configuration = Configuration, misultin = Misultin});
		
		{error, MisultinError} ->
			?init_stop_with_error ({misultin, MisultinError})
	end
end).


?handle_call_default_spec.
?handle_call_default.

?handle_cast_default_spec.
?handle_cast_default.

?handle_info_default_spec.
?handle_info_default.


?terminate_default_spec.
?terminate_default.

?code_change_default_spec.
?code_change_default.


-spec (handle_request (#?configuration{}, gen__send_name (), any ()) -> ok).

handle_request (Configuration, _Adapter, Session) ->
	
	Dispatcher = Configuration#?configuration.dispatcher,
	
	{ok, HttpVersion} = case Session:get (vsn) of
		{1, 1} -> {ok, <<"1.1">>};
		{1, 0} -> {ok, <<"1.0">>}
	end,
	
	{ok, HttpMethod} = case Session:get (method) of
		'GET' -> {ok, <<"GET">>};
		'POST' -> {ok, <<"POST">>}
	end,
	
	{ok, HttpUri} = case {Session:get (uri), Session:get (args)} of
		{{_, Uri}, []} -> {ok, Uri};
		{{_, Uri}, Args} -> {ok, Uri ++ "?" ++ Args}
	end,
	
	HttpHeaders =
			lists:map (
				fun
					({Name, Value}) when is_atom (Name) -> {erlang:atom_to_binary (Name, utf8), Value};
					({Name, Value}) when is_list (Name) -> {erlang:list_to_binary (Name), Value};
					({Name, Value}) when is_binary (Name) -> {Name, Value}
				end,
				Session:get (headers)),
	
	HttpBody = Session:get (body),
	
	Request = #?request{
			http_version = HttpVersion,
			http_method = HttpMethod,
			http_uri = HttpUri,
			http_headers = HttpHeaders,
			http_body = HttpBody},
	
	{ok, CallbackIdentifier} = gen_server:call (Dispatcher, {dispatch_request, Request, erlang:self ()}),
	
	ok = receive
		
		{dispatch_callback, CallbackIdentifier, {ok, Response = #?response{}}} ->
			Session:respond (
					Response#?response.http_code,
					lists:map (
						fun ({Name, Value}) when is_binary (Name) -> {erlang:binary_to_list (Name), Value} end,
						Response#?response.http_headers),
					Response#?response.http_body),
			ok;
		
		{dispatch_callback, CallbackIdentifier, {error, unhandled}} ->
			Session:respond (502, [], "unhandled request"),
			ok;
		
		_ ->
			Session:respond (502, [], "unknown callback"),
			ok
		
	after
		6 * 1000 ->
			Session:respond (502, [], "timedout response"),
			ok
	end,
	
	ok.


-spec (configure () -> {ok, #?configuration{}}).

configure () ->
	configure (mosaic_httpg_dispatcher).


-spec (configure (gen__send_name ()) -> {ok, #?configuration{}}).

configure (Dispatcher) ->
	{ok, #?configuration{
		dispatcher = Dispatcher,
		ip = "0.0.0.0",
		port = 9090,
		backlog = undefined,
		recv_timeout = undefined,
		compress = undefined}}.
