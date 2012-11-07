-module(erlv8_array).

-record(erlv8_array, {resource, %% or array()
                      vm}). %% or proplist()

-extends(erlv8_object).

-compile({no_auto_import,[length/1]}).

-export([list/1, object/1, length/1, push/2, unshift/2, delete/2,

         new/1, new/2]).

list(#erlv8_array{resource = Resource, vm = VM}) ->
    erlv8_vm:enqueue_tick(VM,{list,Resource}).

object(#erlv8_array{resource = Resource, vm = VM}) ->
    erlv8_object:new(Resource,VM).

new(O) ->
    new(O, undefined).

new(O,V) ->
    #erlv8_array{resource = O, vm = V}.

length(Self) ->
    erlang:length(list(Self)). %% TODO: I guess it will be more efficient if we had a NIF for that?

push(Val, Self) ->
    M = Self:object(),
    M:set_value(length(Self),Val).

unshift(Val, Self) ->
    M = Self:object(),
    L = length(Self),
    lists:foreach(fun (I) ->
                          M:set_value(L-I,M:get_value(L-I-1))
                  end, lists:seq(0,L-1)),
    M:set_value(0,Val).

delete(Index, Self) ->
    M = Self:object(),
    L = length(Self),
    V = M:get_value(Index),
    lists:foreach(fun (I) ->
                          M:set_value(I,M:get_value(I+1))
                  end, lists:seq(Index,L-1)),
    M:set_value(length,L-1),
    V.
