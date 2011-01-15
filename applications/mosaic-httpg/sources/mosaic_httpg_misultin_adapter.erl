
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
-define (server_default_start_argument, configure).
-define (server_default_start_options, []).
-define (server_default_stop_reason, normal).

-define (server_start_argument_spec, configure | #?configuration{}).
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
?init_wrapper (Configuration_1, begin
	
	case Configuration_1 of
		#?configuration{} -> Configuration = Configuration_1;
		configure -> {ok, Configuration} = configure ()
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


?terminate_default_spec.
?terminate_default.

?code_change_default_spec.
?code_change_default.

?handle_call_default_spec.
?handle_call_default.

?handle_cast_default_spec.
?handle_cast_default.

?handle_info_default_spec.
?handle_info_default.


-spec (handle_request (#?configuration{}, gen__send_name (), any ()) -> ok).

handle_request (Configuration, _Adapter, Session) ->
	
	Dispatcher = Configuration#?configuration.dispatcher,
	
	case Session:get (vsn) of
		{1, 1} -> HttpVersion = <<"1.1">>;
		{1, 0} -> HttpVersion = <<"1.0">>
	end,
	
	case Session:get (method) of
		'GET' -> HttpMethod = <<"GET">>;
		'POST' -> HttpMethod = <<"POST">>
	end,
	
	case {Session:get (uri), Session:get (args)} of
		{{_, Uri}, []} -> HttpUri = Uri;
		{{_, Uri}, Args} -> HttpUri = Uri ++ "?" ++ Args
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
	
	receive
		
		{dispatch_callback, Response = #?response{}, CallbackIdentifier} ->
			Session:respond (
					Response#?response.http_code,
					Response#?response.http_headers,
					Response#?response.http_body);
		
		{dispatch_callback, unhandled, CallbackIdentifier} ->
			Session:respond (502, [], "unhandled request");
		
		_ ->
			Session:respond (502, [], "unknown callback")
	end,
	
	ok.


-spec (configure () -> {ok, #?configuration{}}).

configure () ->
	configure (mosaic_httpg_dispatcher).


-spec (configure (gen__send_name ()) -> {ok, #?configuration{}}).

configure (Dispatcher) ->
	{ok, #?configuration{
		dispatcher = Dispatcher,
		ip = "127.0.0.1",
		port = 8080,
		backlog = undefined,
		recv_timeout = undefined,
		compress = undefined}}.
