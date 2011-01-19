#!/bin/bash

set -e -u -o pipefail || exit 1
test "${#}" -eq 0 -o "${#}" -eq 1

cd "$( dirname -- "$( readlink -e -- "${0}" )" )"

if test "${#}" -eq 1 ; then
	_output="${1}"
else
	_output=/dev/stdout
fi

_files=(
	./control.bundle.bootstrap.bash
	./control.main.bash
	./scriptlets.harness.bash
	./scriptlets.library.bash
)

if test -e ./bash.elf ; then
	_files+=( ./bash.elf )
fi

exec ./bundle.bash /tmp/httpg-control-scriptlet gzip "${_files[@]}" >"${_output}"
