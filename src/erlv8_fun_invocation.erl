-module(erlv8_fun_invocation,[ICC,Holder,This,Ref,Server]).
-export([is_construct_call/0, holder/0, this/0, global/0]).

is_construct_call() ->
	ICC.

holder() ->
	Holder.

this() ->
	This.

global() ->
	erlv8_script:global(Server).
