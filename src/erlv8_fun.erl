-module(erlv8_fun,[Resource,VM]).
-extends(erlv8_object).
-export([call/0,call/1,call/2,instantiate/0, instantiate/1, object/0]).
-include_lib("erlv8/include/erlv8.hrl").

call() ->
	call([]).

call({erlv8_object, _,_}=T) ->
	call(T,[]);

call(Args) when is_list(Args) ->
	erlv8_vm:next_tick(VM, {?CallTick, Resource, Args}).

call({erlv8_object, _,_}=This, Args) when is_list(Args) ->
	erlv8_vm:next_tick(VM, {?CallTick, Resource, Args, This}).

instantiate() ->
	instantiate([]).

instantiate(Args) when is_list(Args) ->
	erlv8_vm:next_tick(VM, {?InstantiateTick, Resource, Args}).

object() ->
	{erlv8_object, Resource, VM}.
