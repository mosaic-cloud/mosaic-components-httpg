#!/dev/null

if ! test "${#}" -eq 0 ; then
	echo "[ee] invalid arguments; aborting!" >&2
	exit 1
fi

exec find "${_outputs}/erlang/applications-ez" \
	-type f -name '*.ez' \
	-fprintf /dev/stderr '[ii] publishing `%f`...\n' \
	-exec cp -t /afs/olympus.volution.ro/people/ciprian/web/data/6f51b85b25e3dabdf1822800f279602c -- {} \;
