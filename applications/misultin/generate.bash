#!/bin/bash

set -e -E -u -o pipefail || exit 1
test "${#}" -eq 0

cd -- "$( dirname -- "$( readlink -e -- "${0}" )" )"

set -x

rm -Rf ./generated
mkdir ./generated

cp -T ./misultin--0.6.2/src/misultin.app.src ./generated/misultin.app

exit 0
