
-module (mosaic_httpg_app).

-export ([run/0]).


-include ("mosaic_httpg.hrl").

-define (request, mosaic_http_request).
-define (response, mosaic_http_response).


run () ->
	
	RabbitManagementEnabled = case application:get_env (mosaic_httpg, rabbit_management_enabled) of
		{ok, true} -> true;
		{ok, false} -> false;
		undefined -> true
	end,
	
	MandatoryApplicationsStep1 = [sasl, os_mon, mnesia],
	MandatoryApplicationsStep2 = [rabbit, amqp_client],
	if
		RabbitManagementEnabled ->
			OptionalApplicationsStep1 = [inets, crypto, mochiweb, webmachine, rabbit_mochiweb],
			OptionalApplicationsStep2 = [rabbit_management_agent, rabbit_management];
		true ->
			OptionalApplicationsStep1 = [],
			OptionalApplicationsStep2 = []
	end,
	
	lists:foreach (fun (Application) -> ok = application:load (Application) end,
			MandatoryApplicationsStep1 ++ OptionalApplicationsStep1 ++
			MandatoryApplicationsStep2 ++ OptionalApplicationsStep2),
	
	lists:foreach (fun (Application) -> ok = application:start (Application) end,
			MandatoryApplicationsStep1 ++ OptionalApplicationsStep1),
	
	ok = rabbit:prepare (),
	
	lists:foreach (fun (Application) -> ok = application:start (Application) end,
			MandatoryApplicationsStep2 ++ OptionalApplicationsStep2),
	
	{ok, DispatcherConfiguration} = mosaic_httpg_amqp_dispatcher:configure (),
	
	{ok, Dispatcher} = mosaic_httpg_amqp_dispatcher:start_link (DispatcherConfiguration, [{debug, []}]),
	
	ok = timer:sleep (1 * 1000),
	
	if
		true ->
			
			Loop = fun (Req) ->
				
				case Req:get (vsn) of
					{1, 1} -> HttpVersion = <<"1.1">>;
					{1, 0} -> HttpVersion = <<"1.0">>
				end,
				
				case Req:get (method) of
					'GET' -> HttpMethod = <<"GET">>;
					'POST' -> HttpMethod = <<"POST">>
				end,
				
				case {Req:get (uri), Req:get (args)} of
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
							Req:get (headers)),
				
				HttpBody = Req:get (body),
				
				Request = #?request{
						http_version = HttpVersion,
						http_method = HttpMethod,
						http_uri = HttpUri,
						http_headers = HttpHeaders,
						http_body = HttpBody},
				
				{ok, CallbackIdentifier} = mosaic_httpg_amqp_dispatcher:dispatch_request (Dispatcher, Request, erlang:self ()),
				
				receive
					
					{dispatch_callback, Response = #?response{}, CallbackIdentifier} ->
						
						% io:format ("---- request: ~s~n", [CallbackIdentifier]),
						
						Req:respond (
								Response#?response.http_code,
								Response#?response.http_headers,
								Response#?response.http_body);
					
					{dispatch_callback, unhandled, CallbackIdentifier} ->
						
						io:format ("---- unhandled: ~s~n", [CallbackIdentifier]),
						
						Req:respond (502, [], "unhandled request");
					
					_ ->
						
						io:format ("---- unknown~n"),
						
						Req:respond (502, [], "unknown callback")
				end
			end,
			
			{ok, Misultin} = misultin:start_link ([{port, 8080}, {loop, Loop}]);
		
		true ->
			
			Loop = fun (Loop, Timer, Iteration) ->
				receive
					
					dispatch_request ->
						
						RequestUri = erlang:list_to_binary ("/resource/" ++ uuid:to_string (uuid:random ())),
						
						Request = #?request{
								socket_remote_ip = {127, 1, 2, 3},
								socket_remote_port = 1234,
								socket_remote_fqdn = <<"client.domain.tld.">>,
								socket_local_ip = {127, 0, 0, 1},
								socket_local_port = 8080,
								socket_local_fqdn = <<"server.domain.tld.">>,
								http_version = <<"1.1">>,
								http_method = <<"GET">>,
								http_uri = RequestUri,
								http_headers = [
									{<<"Host">>, <<"domain1.tld">>}],
								http_body = <<"">>},
						
						{ok, _CallbackIdentifier} = mosaic_httpg_amqp_dispatcher:dispatch_request (Dispatcher, Request, erlang:self ()),
						
						% io:format ("---- request: ~s~n", [CallbackIdentifier]),
						
						Loop (Loop, Timer, Iteration + 1);
					
					{dispatch_callback, _Response = #?response{}, _CallbackIdentifier} ->
						
						% io:format ("---- response: ~s~n", [CallbackIdentifier]),
						
						Loop (Loop, Timer, Iteration + 1);
					
					{dispatch_callback, unhandled, CallbackIdentifier} ->
						
						io:format ("---- unhandled: ~s~n", [CallbackIdentifier]),
						
						Loop (Loop, Timer, Iteration + 1);
					
					stop ->
						
						io:format ("---- stop~n"),
						
						{ok, cancel} = timer:cancel (Timer),
						
						ok;
					
					_ ->
						
						io:format ("---- unknown~n"),
						
						Loop (Loop, Timer, Iteration + 1)
				end
			end,
			
			erlang:spawn_link (
					fun () ->
						{ok, Timer} = timer:send_interval (1 * 100, erlang:self (), dispatch_request),
						Loop (Loop, Timer, 0)
					end)
	end,
	
	timer:sleep (120 * 1000),
	
	ok = init:stop(),
	ok.
