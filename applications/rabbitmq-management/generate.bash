#!/bin/bash

set -e -E -u -o pipefail || exit 1
test "${#}" -eq 0

cd -- "$( dirname -- "$( readlink -e -- "${0}" )" )"

set -x

rm -Rf ./generated
mkdir ./generated

VERSION=2.3.1

cp -T ./mochiweb--latest/src/mochiweb.app.src ./generated/mochiweb.app
cp -T ./webmachine--latest/ebin/webmachine.app ./generated/webmachine.app

cp -T ./rabbitmq-management--latest/ebin/rabbit_management.app.in ./generated/rabbit_management.app
cp -T ./rabbitmq-management-agent--latest/ebin/rabbit_management_agent.app.in ./generated/rabbit_management_agent.app
cp -T ./rabbitmq-mochiweb--latest/ebin/rabbit_mochiweb.app.in ./generated/rabbit_mochiweb.app

sed -r -e 's!%%VSN%%!'"${VERSION}"'!g' -i ./generated/rabbit_management.app
sed -r -e 's!%%VSN%%!'"${VERSION}"'!g' -i ./generated/rabbit_management_agent.app
sed -r -e 's!%%VSN%%!'"${VERSION}"'!g' -i ./generated/rabbit_mochiweb.app

exit 0
