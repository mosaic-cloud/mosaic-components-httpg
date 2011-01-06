
-module(misultin_examples).
-export([start/1]).

start([ModuleToken, PortToken]) ->
	Module = list_to_atom("misultin_" ++ ModuleToken),
	Port = list_to_integer(PortToken),
	{ok, Pid} = Module:start(Port),
	true = unlink(Pid),
	ok.
