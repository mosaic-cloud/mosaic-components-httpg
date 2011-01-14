
-module (mosaic_httpg_misultin_adapter).


-define (server_name, ?module).
-define (server_start_argument, configure).
-define (server_start_options, []).
-include_lib ("vme/include/vme_gen_server_module.hrl").


?behaviour.

?export_behaviour.

?export_start_and_start_link_0.
?export_start_and_start_link_1.
?export_start_and_start_link_2.

-export ([configure/0, configure/1, handle_request/3]).


?start_dsmao_default. ?start_link_dsmao_default.
?start_dsmo_a_default. ?start_link_dsmo_a_default.
?start_dsm_ao_default. ?start_link_dsm_ao_default.


-include ("mosaic_httpg.hrl").

-define (configuration, mosaic_httpg_misultin_adapter_configuration).
-define (state, mosaic_httpg_misultin_adapter_state).
-define (request, mosaic_http_request).
-define (response, mosaic_http_response).

-record(?state, {
		configuration,
		misultin}).


?init_wrapper (Configuration_1, begin
	
	case Configuration_1 of
		#?configuration{} -> Configuration = Configuration_1;
		configure -> {ok, Configuration} = configure ()
	end,
	
	Self = erlang:self (),
	
	{ok, Misultin} =
			misultin:start_link (
				lists:filter (
					fun ({_Name, Value}) -> Value /= undefined end,
					[
						{loop, fun (Session) -> handle_request (Configuration, Self, Session) end},
						{ip, Configuration#?configuration.ip},
						{port, Configuration#?configuration.port},
						{backlog, Configuration#?configuration.backlog},
						{recv_timeout, Configuration#?configuration.recv_timeout},
						{compress, Configuration#?configuration.compress}])),
	
	?init_ok (#?state{configuration = Configuration, misultin = Misultin})
end).


?terminate_default.
?code_change_default.
?handle_call_default.
?handle_cast_default.
?handle_info_default.


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


configure () ->
	configure (mosaic_httpg_dispatcher).


configure (Dispatcher) ->
	{ok, #?configuration{
		dispatcher = Dispatcher,
		ip = "127.0.0.1",
		port = 8080,
		backlog = undefined,
		recv_timeout = undefined,
		compress = undefined}}.
