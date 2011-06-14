
-record(mosaic_httpg_amqp_dispatcher_configuration, {
		broker_host, broker_port, broker_virtual_host, broker_username, broker_password,
		requests_exchange, request_routing_key_encoder, request_message_body_encoder,
		responses_exchange, response_message_body_decoder,
		callbacks_table}).

-record(mosaic_httpg_misultin_adapter_configuration, {
		dispatcher,
		ip, port, backlog, recv_timeout, compress}).

-record(mosaic_http_request, {
		socket_remote_ip, socket_remote_port, socket_remote_fqdn,
		socket_local_ip, socket_local_port, socket_local_fqdn,
		http_version, http_method, http_uri,
		http_headers, http_body}).

-record(mosaic_http_response, {
		version,
		http_version, http_code, http_status,
		http_headers, http_body}).
