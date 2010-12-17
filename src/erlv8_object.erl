-module(erlv8_object,[Res]).
-export([proplist/0, set_value/2, get_value/1, get_value/2]).

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
	
