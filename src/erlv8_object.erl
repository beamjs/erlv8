-module(erlv8_object,[Resource,VM]).
-export([proplist/0, set_value/2, set_value/3, set_hidden_value/2, get_value/1, get_value/2, get_hidden_value/1, get_hidden_value/2, 
		 set_prototype/1, get_prototype/0, delete/1, set_accessor/2, set_accessor/3, set_accessor/4, set_accessor/5,
		 equals/1, strict_equals/1, call/1, call/2,new/1]).
-include_lib("erlv8/include/erlv8.hrl").

proplist() ->
	erlv8_vm:next_tick(VM,{?ProplistTick, Resource}).

set_value(Key,Value) ->
	erlv8_vm:next_tick(VM, {?SetTick, Resource, Key, Value}).

set_value(Key,Value,PropertyAttribute) ->
	erlv8_vm:next_tick(VM, {?SetTick, Resource, Key, Value, PropertyAttribute}).

set_hidden_value(Key,Value) ->
	erlv8_nif:object_set_hidden(Resource, Key, Value).

get_value(Key) ->
	get_value(Key, undefined).

get_value(Key, Default) ->
	case erlv8_vm:next_tick(VM, {?GetTick, Resource, Key}) of
		undefined ->
			Default;
		Val ->
			Val
	end.

get_hidden_value(Key) ->
	get_hidden_value(Key, undefined).

get_hidden_value(Key, Default) ->
	case erlv8_nif:object_get_hidden(Resource, Key) of
		undefined ->
			Default;
		Val ->
			Val
	end.

set_prototype(Proto) ->
	erlv8_nif:object_set_proto(Resource, Proto).

get_prototype() ->
	erlv8_nif:object_get_proto(Resource).

delete(Key) ->
	erlv8_nif:object_delete(Resource,Key).

set_accessor(Property, Getter) ->
	erlv8_nif:object_set_accessor(Resource, Property, Getter).

set_accessor(Property, Getter, Setter) ->
	erlv8_nif:object_set_accessor(Resource, Property, Getter, Setter).

set_accessor(Property, Getter, Setter, AccessControl) ->
	erlv8_nif:object_set_accessor(Resource, Property, Getter, Setter, AccessControl).

set_accessor(Property, Getter, Setter, AccessControl, PropertyAttribute) ->
	erlv8_nif:object_set_accessor(Resource, Property, Getter, Setter, AccessControl, PropertyAttribute).


equals({_Tag,AnotherObject,_}) ->
	erlv8_value:equals(VM, Resource, AnotherObject).

strict_equals({_Tag,AnotherObject,_}) ->
	erlv8_value:strict_equals(VM, Resource, AnotherObject).

call(Fun) ->
	call(Fun,[]).

call(Fun,Args) ->
    Fun:call({erlv8_object, Resource,VM}, Args).
	
new(O) ->
	instance(O,undefined).
	
