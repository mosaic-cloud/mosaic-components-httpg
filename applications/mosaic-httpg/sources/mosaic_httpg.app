
{application, mosaic_httpg, [
	{description, "mOSAIC HTTP-gateway component"},
	{vsn, "1"},
	{applications, [kernel, stdlib, mosaic_component, misultin, amqp_client]},
	{modules, []},
	{registered, []},
	{mod, {mosaic_dummy_app, defaults}},
	{env, []}
]}.
