-module(erlv8_fun,[Resource,VM,Object]).
-export([call/0,call/1,object/0,new/2,equals/1,strict_equals/1]).

call() ->
	call([]).

call(Args) when is_list(Args) ->
	erlv8_vm:next_tick(VM, {call, Resource, Args}).

object() ->
	Object.

new(Fun, {erlv8_object,_}=Obj) when is_function(Fun) ->
	instance(Fun,undefined,Obj).

equals({erlv8_fun,AnotherFun,_,_}) ->
	erlv8_value:equals(Resource, AnotherFun).

strict_equals({erlv8_fun,AnotherFun,_,_}) ->
	erlv8_value:strict_equals(Resource, AnotherFun).
