#!/bin/bash

set -x -e -E -u || exit 1
cd -- "$( dirname -- "$( readlink -e -- "${0}" )" )/.."

. ./scripts/env.bash

mkdir -p -- "${_tmp}"
mkdir -p -- "${_outputs}"
mkdir -p -- "${_store}"

if \
		test ! -e "${_mk_file}" -o "${_mk_file}" -ot "${_vbs}" \
		|| test -n "$( find . -name '*.vbsd' -cnewer "${_mk_file}" -printf . )"
then
	"${_vbs}" -- generate-mk-script . "${_outputs}" "${_mk_file}"
fi

if test "${#}" -eq 0 ; then
	exec "${_mk}" "${_mk_args[@]}" "${_mk_vars[@]}"
else
	exec "${_mk}" "${_mk_args[@]}" -s "${@}" "${_mk_vars[@]}"
fi
