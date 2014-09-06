
{application, mosaic_httpg, [
	{description, "mOSAIC HTTP-gateway component"},
	{vsn, "1"},
	{applications, [kernel, stdlib, mosaic_component]},
	{modules, []},
	{registered, []},
	{mod, {mosaic_dummy_app, defaults}},
	{env, []}
]}.
