#!/bin/bash

set -x -e -E -u || exit 1
test "${#}" -eq 0
cd -- "$( dirname -- "$( readlink -e -- "${0}" )" )/.."

. ./scripts/env.bash

find ./applications-ez -type f -name '*.ez' \
| while read _ez ; do
	_cwd="$( pwd )"
	_ez="$( readlink -e -- "${_ez}" )"
	cd -- "${_outputs}/erlang/applications"
	unzip -q -o -x "${_ez}"
	cd -- "${_cwd}"
done

exit 0
