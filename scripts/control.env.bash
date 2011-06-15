#!/dev/null

_erl_path=''

_erl_run_argv=(
	+Bd +Ww
	-env ERL_CRASH_DUMP /dev/null
	-env ERL_LIBS "${_deployment_erlang_path:-./erlang}/lib"
	-env LANG C
	-noshell -noinput
	-config "${_deployment_erlang_path:-./erlang}/lib/mosaic_httpg/priv/mosaic_httpg.config"
	-run mosaic_httpg_callbacks standalone
)

_ez_bundle_names=(
	amqp_client
	goodies
	misultin
	mosaic_component
	mosaic_harness
	mosaic_httpg
	mosaic_tools
	rabbit_common
	vme
)

_bundles_token="bffeb597b112f0c3d7147fc906b5cf56"
_bundles_base_url="http://data.volution.ro/ciprian/${_bundles_token}"
_bundles_base_path="/afs/olympus.volution.ro/people/ciprian/web/data/${_bundles_token}"
