-module(erlv8_fun,[Resource,Script,Object]).
-export([call/0,call/1,object/0,new/2]).

call() ->
	call([]).

call(Args) ->
	erlv8_vm:next_tick(Script, {call, Resource, Args}).

object() ->
	Object.

new(Fun, {erlv8_object,_}=Obj) when is_function(Fun) ->
	instance(Fun,undefined,Obj).
