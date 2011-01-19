
-module (mosaic_httpg_app).

-export ([run/0]).


-include ("mosaic_httpg.hrl").

-define (request, mosaic_http_request).
-define (response, mosaic_http_response).


run () ->
	
	case application:get_env (mosaic_httpg, rabbit_management_enabled) of
		{ok, true} -> RabbitManagementEnabled = true;
		{ok, false} -> RabbitManagementEnabled = false;
		undefined -> RabbitManagementEnabled = true
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
	
	ok = lists:foreach (
			fun (Application) -> ok = application:load (Application) end,
			MandatoryApplicationsStep1 ++ OptionalApplicationsStep1 ++
			MandatoryApplicationsStep2 ++ OptionalApplicationsStep2),
	
	ok = lists:foreach (
			fun (Application) -> ok = application:start (Application) end,
			MandatoryApplicationsStep1 ++ OptionalApplicationsStep1),
	
	ok = rabbit:prepare (),
	
	ok = lists:foreach (fun (Application) -> ok = application:start (Application) end,
			MandatoryApplicationsStep2 ++ OptionalApplicationsStep2),
	
	{ok, DispatcherConfiguration} = mosaic_httpg_amqp_dispatcher:configure (),
	
	{ok, Dispatcher} = mosaic_httpg_amqp_dispatcher:start_link (DispatcherConfiguration),
	
	{ok, MisultinAdapterConfiguration} = mosaic_httpg_misultin_adapter:configure (Dispatcher),
	
	{ok, MisultinAdapter} = mosaic_httpg_misultin_adapter:start_link (MisultinAdapterConfiguration),
	
	ok = timer:sleep (28 * 24 * 60 * 1000),
	
	ok = mosaic_httpg_misultin_adapter:stop (MisultinAdapter),
	
	ok = mosaic_httpg_amqp_dispatcher:stop (Dispatcher),
	
	ok = timer:sleep (1 * 1000),
	
	ok = init:stop(),
	
	ok.
