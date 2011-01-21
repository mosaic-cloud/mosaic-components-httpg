#!/dev/null

_scripts="$( readlink -e -- ./scripts || true )"
_tools="$( readlink -f -- ./.tools || true )"
_outputs="$( readlink -f -- ./.outputs || true )"

_PATH="${_tools}/bin:${PATH}"


_erl="$( PATH="${_PATH}" type -P -- erl || true )"
if test -z "${_erl}" ; then
	echo "[ww] missing \`erl\` (Erlang interpreter) executable in path: \`${_PATH}\`; ignoring!" >&2
	_erl=erl
fi

_python="$( PATH="${_PATH}" type -P -- python || true )"
if test -z "${_python}" ; then
	echo "[ww] missing \`python\` (Python interpreter) executable in path: \`${_PATH}\`; ignoring!" >&2
	_python=python
fi

_httperf="$( PATH="${_PATH}" type -P -- httperf || true )"
if test -z "${_httperf}" ; then
	echo "[ww] missing \`httperf\` executable in path: \`${_PATH}\`; ignoring!" >&2
	_httperf=httperf
fi

_vbs="$( PATH="${_PATH}" type -P -- vbs || true )"
if test -z "${_vbs}" ; then
	echo "[ww] missing \`vbs\` (Volution Build System tool) executable in path: \`${_PATH}\`; ignoring!" >&2
	_vbs=vbs
fi

_mk="$( PATH="${_PATH}" which -- mk 2>/dev/null || true )"
if test -z "${_mk}" ; then
	echo "[ww] missing \`mk\` (Plan9 make tool) executable in path: \`${_PATH}\`; ignoring!" >&2
	_mk=mk
fi

_erl_libs="${_outputs}/erlang/applications"
_erl_args=(
	+Bd +Ww
	-env ERL_CRASH_DUMP /dev/null
	-env ERL_LIBS "${_erl_libs}"
)

_python_args=(
	-B
)

_httperf_args=(
	--recv-buffer=131072
	--send-buffer=131072
)

_mk_file="${_outputs}/.make.mk"
_mk_args=(
	-f "${_mk_file}"
)
_mk_vars=(
	NPROC=8
)
