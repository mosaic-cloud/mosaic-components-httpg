#!/bin/bash

set -x -e -E -u || exit 1
test "${#}" -eq 0
cd -- "$( dirname -- "$( readlink -e -- "${0}" )" )/.."

. ./scripts/env.bash

exec "${_erl}" "${_erl_args[@]}" \
		-noshell -noinput \
		-config "${_outputs}/erlang/applications/mosaic_httpg/priv/mosaic_httpg.config" \
		-run mosaic_httpg_app run
