-module(erlv8_object,[Res]).
-export([proplist/0, set_value/2, get_value/1, get_value/2, set_prototype/1, get_prototype/0]).

proplist() ->
	erlv8_nif:to_proplist(Res).

set_value(Key,Value) ->
	erlv8_nif:object_set(Res, Key, Value).

get_value(Key) ->
	get_value(Key, undefined).

get_value(Key, Default) ->
	case erlv8_nif:object_get(Res, Key) of
		undefined ->
			Default;
		Val ->
			Val
	end.

set_prototype(Proto) ->
	erlv8_nif:object_set_proto(Res, Proto).

get_prototype() ->
	erlv8_nif:object_get_proto(Res).
