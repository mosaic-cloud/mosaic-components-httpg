#!/bin/bash

set -e -u -o pipefail || exit 1
test "${#}" -eq 0

cd "$( dirname -- "$( readlink -e -- "${0}" )" )"

_files=(
	./control.bundle.bootstrap.bash
	./control.main.bash
	./scriptlets.harness.bash
	./scriptlets.library.bash
)

if test -e ./bash.elf ; then
	_files+=( ./bash.elf )
fi

exec ./bundle.bash /tmp/httpg-control-scriptlet gzip "${_files[@]}"
