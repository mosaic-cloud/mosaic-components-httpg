#!/bin/bash

set -e -E -u -o pipefail || exit 1
test "${#}" -eq 0

cd -- "$( dirname -- "$( readlink -e -- "${0}" )" )"

set -x

rm -Rf ./generated
mkdir ./generated

PYTHONPATH=./rabbitmq-codegen--latest \
python2 ./rabbitmq-server--latest/codegen.py \
		header --ignore-conflicts \
		./rabbitmq-codegen--latest/amqp-rabbitmq-0.9.1.json \
		./rabbitmq-codegen--latest/amqp-rabbitmq-0.8.json \
		./generated/rabbit_framing.hrl

PYTHONPATH=./rabbitmq-codegen--latest \
python2 ./rabbitmq-server--latest/codegen.py \
		body \
		./rabbitmq-codegen--latest/amqp-rabbitmq-0.9.1.json \
		./generated/rabbit_framing_amqp_0_9_1.erl

PYTHONPATH=./rabbitmq-codegen--latest \
python2 ./rabbitmq-server--latest/codegen.py \
		body \
		./rabbitmq-codegen--latest/amqp-rabbitmq-0.8.json \
		./generated/rabbit_framing_amqp_0_8.erl

find ./rabbitmq-server--latest/src -name '*.erl' -printf '%f\n' \
| sed -r -e 's!^([a-z]([a-z0-9_]+[a-z0-9])?)(\.erl)$!\1!g' \
	>./generated/rabbit_modules.txt

erl +Bd -mode minimal -noinput -noshell \
		-eval '
	{ok,[{_,_,[_,_,{modules, Mods},_,_,_]}]} = file:consult("./rabbitmq-erlang-client--latest/rabbit_common.app.in"),
	[io:format("~p\n",[M]) || M <- Mods],
	init:stop ().
' >./generated/rabbit_common_modules.txt

cp -T ./rabbitmq-server--latest/ebin/rabbit_app.in ./generated/rabbit.app
cp -T ./rabbitmq-erlang-client--latest/rabbit_common.app.in ./generated/rabbit_common.app
cp -T ./rabbitmq-erlang-client--latest/ebin/amqp_client.app.in ./generated/amqp_client.app

sed -r -e 's!%%VSN%%!2.2.0!g' -i ./generated/rabbit.app
sed -r -e 's!%%VSN%%!2.2.0!g' -i ./generated/amqp_client.app
sed -r -e 's!%%VSN%%!2.2.0!g' -i ./generated/rabbit_common.app

sed -r \
		-e 's!(\{modules, \[)(\]\})!\1'"$( tr '\n' ',' <./generated/rabbit_modules.txt )"'\2!g' \
		-e 's!(\{modules, \[)(([a-z]([a-z0-9_]+[a-z0-9])?,)*)([a-z]([a-z0-9_]+[a-z0-9])?),(\]\})!\1\2\5\7!g' \
		-i ./generated/rabbit.app

exit 0
