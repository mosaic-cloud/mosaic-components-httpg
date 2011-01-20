#!/dev/null

if ! test "${#}" -eq 0 ; then
	echo "[ee] invalid arguments; aborting!" >&2
	exit 1
fi

_prefix="${_outputs}/tools/python"

if test -e "${_prefix}" ; then
	echo "[ee] Python already installed: \`${_prefix}\`; aborting!" >&2
	exit 1
fi

if type -P -- python2.7 >/dev/null ; then
	_python="$( type -P -- python2.7 )"
elif type -P -- python2.6 >/dev/null ; then
	_python="$( type -P -- python2.6 )"
else
	echo "[ee] Python interpreter not found; aborting!" >&2
fi

if type -P -- virtualenv ; then
	_virtualenv="$( type -P -- virtualenv )"
else
	echo "[ee] VirtualEnv command not found; aborting!" >&2
fi

echo "[ii] Python interpreter found: \`${_python}\`" >&2
echo "[ii] VirtualEnv command found: \`${_virtualenv}\`" >&2

echo "[ii] Installing Python..." >&2
"${_virtualenv}" --quiet --python "${_python}" "${_prefix}"

echo "[ii] Installing Pika..." >&2
"${_prefix}/bin/easy_install" --quiet --optimize 2 pika

if ! test -e "${_tools}/bin/python" ; then
	echo "[ii] Symlink-ing Python in: \`${_tools}/bin\`..." >&2
	mkdir -p "${_tools}/bin"
	ln -s -T -- "${_prefix}/bin/python" "${_tools}/bin/python"
else
	echo "[ww] Python symlink already exists in: \`${_tools}/bin\`; ignoring!" >&2
fi

exit 0
