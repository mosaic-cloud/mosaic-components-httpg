#!/bin/bash

set -x -e -E -u || exit 1
cd -- "$( dirname -- "$( readlink -e -- "${0}" )" )/.."

. ./scripts/env.bash

if test "${#}" -eq 0 ; then
	exec "${_erl}" "${_erl_args[@]}"
else
	exec "${_erl}" "${_erl_args[@]}" "${@}"
fi
