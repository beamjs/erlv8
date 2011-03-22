-module(erlv8_object,[Resource,VM]).
-export([proplist/0, set_value/2, set_value/3, set_hidden_value/2, get_value/1, get_value/2, get_hidden_value/1, get_hidden_value/2, 
		 internal_field_count/0, get_internal_field/1, set_internal_field/2,
		 set_prototype/1, get_prototype/0, delete/1, set_accessor/2, set_accessor/3, set_accessor/4, set_accessor/5,
		 equals/1, strict_equals/1, call/1, call/2,new/1]).

proplist() ->
	erlv8_vm:enqueue_tick(VM,{proplist, Resource}).

set_value(Key,Value) ->
	erlv8_vm:enqueue_tick(VM, {set, Resource, Key, Value}).

set_value(Key,Value,PropertyAttribute) ->
	erlv8_vm:enqueue_tick(VM, {set, Resource, Key, Value, PropertyAttribute}).

set_hidden_value(Key,Value) ->
	erlv8_vm:enqueue_tick(VM, {set_hidden, Resource, Key, Value}).

get_value(Key) ->
	get_value(Key, undefined).

get_value(Key, Default) ->
	case erlv8_vm:enqueue_tick(VM, {get, Resource, Key}) of
		undefined ->
			Default;
		Val ->
			Val
	end.

get_hidden_value(Key) ->
	get_hidden_value(Key, undefined).

get_hidden_value(Key, Default) ->
	case erlv8_vm:enqueue_tick(VM, {get_hidden, Resource, Key}) of
		undefined ->
			Default;
		Val ->
			Val
	end.

internal_field_count() ->
	erlv8_vm:enqueue_tick(VM, {internal_count, Resource}).

get_internal_field(Index) ->
	erlv8_vm:enqueue_tick(VM, {get_internal, Resource, Index}).

set_internal_field(Index, {extern, Value}) ->
	erlv8_vm:enqueue_tick(VM, {set_internal_extern, Resource, Index, Value, erlv8_extern:type(Value)});

set_internal_field(Index, Value) ->
	erlv8_vm:enqueue_tick(VM, {set_internal, Resource, Index, Value}).

set_prototype(Proto) ->
    erlv8_vm:enqueue_tick(VM, {set_proto, Resource, Proto}).

get_prototype() ->
	erlv8_vm:enqueue_tick(VM, {get_proto, Resource}).

delete(Key) ->
	erlv8_vm:enqueue_tick(VM, {delete, Resource, Key}).

set_accessor(Property, Getter) ->
	case erlv8_vm:enqueue_tick(VM, {set_accessor, Resource, Property, Getter}) of
        badarg ->
            throw(badarg);
        Result ->
            Result
    end.

set_accessor(Property, Getter, Setter) ->
	case erlv8_vm:enqueue_tick(VM, {set_accessor, Resource, Property, Getter, Setter}) of
        badarg ->
            throw(badarg);
        Result ->
            Result
    end.

set_accessor(Property, Getter, Setter, AccessControl) ->
	case erlv8_vm:enqueue_tick(VM, {set_accessor, Resource, Property, Getter, Setter, AccessControl}) of
        badarg ->
            throw(badarg);
        Result ->
            Result
    end.

set_accessor(Property, Getter, Setter, AccessControl, PropertyAttribute) ->
	case erlv8_vm:enqueue_tick(VM, {set_accessor, Resource, Property, Getter, Setter, AccessControl, PropertyAttribute}) of
        badarg ->
            throw(badarg);
        Result ->
            Result
    end.

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
	
