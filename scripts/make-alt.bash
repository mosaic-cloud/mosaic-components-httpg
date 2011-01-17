#!/bin/sh

set -x -e -u -o pipefail || exit 1

test "${#}" -ne 0

sed -r \
		-e 's!/mnt/motirare/ciprian/workbench/mosaic-components-httpg!.!g' \
	<./.outputs/.make.mk \
	>./.outputs/.make.mk-1

sed -r \
		-e 's!==!=!g' \
		-e 's!:.*:!:!g' \
		-e 's!(^/(.*))'\''!\1!g' \
	<./.outputs/.make.mk \
	>./.outputs/.make.mk-2

exec make -f ./.outputs/.make.mk-2 "${@}"
