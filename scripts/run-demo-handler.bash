#!/dev/null

if ! test "${#}" -eq 0 ; then
	echo "[ee] invalid arguments; aborting!" >&2
	exit 1
fi

_python_argv=(
	"${_python}"
		"${_python_args[@]}"
		./applications/mosaic-httpg/sources/mosaic_httpg_demo_handler.py
)

exec "${_python_argv[@]}"
