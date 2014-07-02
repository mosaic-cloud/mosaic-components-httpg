#!/dev/null

if ! test "${#}" -le 1 ; then
	echo "[ee] invalid arguments; aborting!" >&2
	exit 1
fi

_identifier="${1:-00000000ba8d275d66fd454594ee28264ab4a710}"
_fqdn="${mosaic_node_fqdn:-mosaic-0.loopback.vnet}"

if test -n "${mosaic_component_temporary:-}" ; then
	_tmp="${mosaic_component_temporary:-}"
elif test -n "${mosaic_temporary:-}" ; then
	_tmp="${mosaic_temporary}/components/${_identifier}"
else
	_tmp="${TMPDIR:-/tmp}/mosaic/components/${_identifier}"
fi

_erl_args+=(
		-noinput -noshell
		-name "mosaic-httpg-${_identifier}@${_fqdn}"
		-setcookie "${_erl_cookie}"
		-boot start_sasl
		-config "${_erl_libs}/mosaic_httpg/priv/mosaic_httpg.config"
)
_erl_env+=(
		mosaic_component_identifier="${_identifier}"
		mosaic_component_temporary="${_tmp}"
		mosaic_node_fqdn="${_fqdn}"
)

if test "${_identifier}" != 00000000ba8d275d66fd454594ee28264ab4a710 ; then
	_erl_args+=(
			-run mosaic_component_app boot
	)
	_erl_env+=(
			mosaic_component_harness_input_descriptor=3
			mosaic_component_harness_output_descriptor=4
	)
	exec 3<&0- 4>&1- </dev/null >&2
else
	_erl_args+=(
			-run mosaic_httpg_callbacks standalone
	)
fi

mkdir -p -- "${_tmp}"
cd -- "${_tmp}"

exec env "${_erl_env[@]}" "${_erl_bin}" "${_erl_args[@]}"

exit 1
