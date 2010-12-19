-module(erlv8_fun,[Resource,VM]).
-extends(erlv8_object).
-export([call/0,call/1,call/2,object/0,equals/1,strict_equals/1]).

call() ->
	call([]).

call({erlv8_object, _,_}=T) ->
	call(T,[]);

call(Args) when is_list(Args) ->
	erlv8_vm:next_tick(VM, {call, Resource, Args}).

call({erlv8_object, _,_}=This, Args) when is_list(Args) ->
	erlv8_vm:next_tick(VM, {call, Resource, Args, This}).

object() ->
	{erlv8_object, Resource, VM}.

equals({erlv8_fun,AnotherFun,_}) ->
	erlv8_value:equals(Resource, AnotherFun).

strict_equals({erlv8_fun,AnotherFun,_}) ->
	erlv8_value:strict_equals(Resource, AnotherFun).

