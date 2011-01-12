
-module (mosaic_httpg_app).

-export ([run/0]).


-include ("mosaic_httpg.hrl").

-define (request, mosaic_http_request).
-define (response, mosaic_http_response).


run () ->
	
	RabbitManagementEnabled = application:get_env (mosaic_httpg, rabbit_management_enabled),
	
	ok = application:start (sasl, permanent),
	ok = application:start (os_mon, permanent),
	ok = application:start (mnesia, permanent),
	
	case RabbitManagementEnabled of
		true ->
			ok = application:start (inets, permanent),
			ok = application:start (crypto, permanent),
			ok = application:start (mochiweb, permanent),
			ok = application:start (rabbit_mochiweb, permanent);
		false -> ok;
		undefined -> ok
	end,
	
	ok = rabbit:prepare (),
	ok = application:start (rabbit, permanent),
	ok = application:start (amqp_client, permanent),
	
	case RabbitManagementEnabled of
		true ->
			ok = application:start (rabbit_management_agent, permanent),
			ok = application:start (webmachine, permanent),
			ok = application:start (rabbit_management, permanent);
		false -> ok;
		undefined -> ok
	end,
	
	{ok, Configuration} = mosaic_httpg_amqp_dispatcher:configure (),
	
	{ok, Self} = gen_server:start_link (mosaic_httpg_amqp_dispatcher, Configuration, []),
	
	ok = timer:sleep (1 * 1000),
	
	{ok, Correlation} = gen_server:call (Self, {dispatch, #?request{}}),
	
	ok = timer:sleep (120 * 1000),
	
	ok = init:stop(),
	ok.
