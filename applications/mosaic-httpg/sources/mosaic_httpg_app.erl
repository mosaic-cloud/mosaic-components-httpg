
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
	
	{ok, Dispatcher} = gen_server:start_link (mosaic_httpg_amqp_dispatcher, DispatcherConfiguration, []),
	
	{ok, Timer} = timer:send_interval (1 * 1000, erlang:self (), dispatch),
	
	Loop = fun (Loop, Iteration) ->
		receive
			
			dispatch ->
				
				Request = #?request{
						socket_remote_ip = {127, 1, 2, 3},
						socket_remote_port = 1234,
						socket_remote_fqdn = <<"client.domain.tld.">>,
						socket_local_ip = {127, 0, 0, 1},
						socket_local_port = 8080,
						socket_local_fqdn = <<"server.domain.tld.">>,
						http_version = <<"1.1">>,
						http_method = <<"GET">>,
						http_uri = <<"/resource/id">>,
						http_headers = [
							{<<"Host">>, <<"domain1.tld">>}],
						http_body = <<"">>},
				
				{ok, Correlation} = gen_server:call (Dispatcher, {dispatch, Request}),
				
				Loop (Loop, Iteration + 1);
			
			stop ->
				ok
		end
	end,
	
	Loop (Loop, 0),
	
	ok = init:stop(),
	ok.
