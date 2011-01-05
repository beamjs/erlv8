-module(erlv8_array,[Resource,VM]).
-extends(erlv8_object).
-export([list/0,object/0,new/1, length/0, push/1, unshift/1, delete/1]).

list() ->
	erlv8_vm:enqueue_tick(VM,{list,Resource}).

object() ->
	erlv8_object:new(Resource,VM).

new(O) ->
	{erlv8_array, O, undefined}.

length() ->
	length(list()). %% TODO: I guess it will be more efficient if we had a NIF for that?

push(Val) ->
	M = {?BASE_MODULE, Resource, VM},
	M:set_value(length(),Val).

unshift(Val) ->
	M = {?BASE_MODULE, Resource, VM},
	L = length(),
	lists:foreach(fun (I) ->
						  M:set_value(L-I,M:get_value(L-I-1))
				  end, lists:seq(0,L-1)),
	M:set_value(0,Val).

delete(Index) ->
	M = {?BASE_MODULE, Resource, VM},
	L = length(),
	V = M:get_value(Index),
	lists:foreach(fun (I) ->
						  M:set_value(I,M:get_value(I+1))
				  end, lists:seq(Index,L-1)),
	M:set_value(length,L-1),
	V.
	
	
