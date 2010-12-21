-module(erlv8_fun_invocation,[ICC,Holder,This,Ref,VM, Ctx]).
-export([is_construct_call/0, holder/0, this/0, global/0, vm/0]).

is_construct_call() ->
	ICC.

holder() ->
	Holder.

this() ->
	This.

global() ->
	erlv8_context:global({VM,Ctx}).

vm() ->
	VM.
