#!/bin/bash

set -e -u -o pipefail || exit 1
test "${#}" -eq 0

cd "$( dirname -- "$( readlink -e -- "${0}" )" )"

exec ./control.bundle.create.bash /afs/olympus.volution.ro/people/ciprian/web/data/6f51b85b25e3dabdf1822800f279602c/control.bash
