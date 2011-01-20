#!/dev/null

if ! test "${#}" -eq 0 ; then
	echo "[ee] invalid arguments; aborting!" >&2
	exit 1
fi

_erl_argv=(
	"${_erl}"
		"${_erl_args[@]}"
		-noshell -noinput
		-config "${_outputs}/erlang/applications/mosaic_httpg/priv/mosaic_httpg.config"
		-run mosaic_httpg_app run
)

exec "${_erl_argv[@]}"
