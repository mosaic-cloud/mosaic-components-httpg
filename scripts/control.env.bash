#!/dev/null

_identifier="${mosaic_component_identifier:-00000000ba8d275d66fd454594ee28264ab4a710}"

_erl_args+=(
	-noshell -noinput
	-sname "mosaic-httpg-${_identifier}@localhost"
	-env mosaic_component_identifier "${_identifier}"
	-boot start_sasl
	-config "${_deployment_erlang_path}/lib/mosaic_httpg/priv/mosaic_httpg.config"
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

_bundles_token=bffeb597b112f0c3d7147fc906b5cf56
_bundles_base_url="http://data.volution.ro/ciprian/${_bundles_token}"
_bundles_base_path="/afs/olympus.volution.ro/people/ciprian/web/data/${_bundles_token}"
