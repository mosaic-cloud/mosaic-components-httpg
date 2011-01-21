#!/dev/null

if ! test "${#}" -eq 0 ; then
	echo "[ee] invalid arguments; aborting!" >&2
	exit 1
fi

_httperf_argv=(
	"${_httperf}"
		"${_httperf_args[@]}"
		--server 127.0.0.1
		--port 8080
		--uri /
		--num-conn 2147483647
		--num-calls 1
		--rate 10
		--timeout 0.5
)

exec "${_httperf_argv[@]}"
