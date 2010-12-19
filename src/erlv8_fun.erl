-module(erlv8_fun,[Resource,VM]).
-extends(erlv8_object).
-export([call/0,call/1,call/2,object/0]).

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
