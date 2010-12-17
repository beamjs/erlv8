-module(erlv8_fun_invocation,[ICC,Holder,This,Ref,Server]).
-export([is_construct_call/0, holder/0, this/0, this/1, global/0, global/1]).

is_construct_call() ->
	ICC.

holder() ->
	Holder.

this() ->
	This.

this(NewThis) ->	
	gen_server2:cast(Server, {this, Ref, NewThis}),
	NewThis.

global() ->
	erlv8_script:global(Server).

global(NewGlobal) ->
	gen_server2:cast(Server, {global, Ref, NewGlobal}),
	undefined.
