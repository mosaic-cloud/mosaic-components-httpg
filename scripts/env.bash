#!/dev/null

_tmp=/tmp/mosaic-components-httpg
_outputs="${_tmp}/outputs"
_store="${_tmp}/store"

_erl=erl
_erl_libs="${_tmp}/outputs/erlang/applications"
_erl_args=(
	+Bd +Ww
	-env ERL_CRASH_DUMP /dev/null
	-env ERL_LIBS "${_erl_libs}"
)

_mk=../vbs/.tools/mk
_mk_file="${_tmp}/make.mk"
_mk_args=(
	-f "${_mk_file}"
)
_mk_vars=(
	NPROC=8
)

_vbs=../vbs/.outputs/vbs.elf
