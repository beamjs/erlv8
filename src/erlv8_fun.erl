-module(erlv8_fun).

-record(erlv8_fun, {resource, %% or fun()
                    vm}). %% or proplist()

-extends(erlv8_object).

-export([call/1,call/2,call/3,instantiate/1, instantiate/2, object/1,

         new/1, new/2]).

call(Self) ->
    call([], Self).

call({erlv8_object, _,_}=T, Self) ->
    call(T,[], Self);

call(Args, #erlv8_fun{resource = Resource, vm = VM}) when is_list(Args) ->
    erlv8_vm:enqueue_tick(VM, {call, Resource, Args}).

call({erlv8_object, _,_}=This, Args, #erlv8_fun{resource = Resource, vm = VM}) when is_list(Args) ->
    erlv8_vm:enqueue_tick(VM, {call, Resource, Args, This}).

instantiate(Self) ->
    instantiate([], Self).

instantiate(Args, #erlv8_fun{resource = Resource, vm = VM}) when is_list(Args) ->
    erlv8_vm:enqueue_tick(VM, {inst, Resource, Args}).

object(#erlv8_fun{resource = Resource, vm = VM}) ->
    {erlv8_object, Resource, VM}.

new(O) ->
    new(O,undefined).

new(O, V) ->
    #erlv8_fun{resource = O, vm = V}.
