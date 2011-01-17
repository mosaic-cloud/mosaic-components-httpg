#!/dev/null

_erl_argv=(
	"${_erl}"
		"${_erl_args[@]}"
		-noshell -noinput
		-config "${_outputs}/erlang/applications/mosaic_httpg/priv/mosaic_httpg.config"
		-run mosaic_httpg_app run
)

if test "${#}" -eq 0 ; then
	exec "${_erl_argv[@]}"
else
	exec "${_erl_argv[@]}" "${@}"
fi
