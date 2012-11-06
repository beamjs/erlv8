-module(erlv8_object).

-record(erlv8_object, {resource,vm}).

-export([proplist/1, set_value/3, set_value/4, set_hidden_value/3, get_value/2, get_value/3, get_hidden_value/2, get_hidden_value/3,
         internal_field_count/1, get_internal_field/2, set_internal_field/3,
         set_prototype/2, get_prototype/1, delete/2, set_accessor/3, set_accessor/4, set_accessor/5, set_accessor/6,
         equals/2, strict_equals/2, call/2, call/3,

         new/1, new/2]).

proplist({_Erlv8Obj,  Resource, VM}) ->
    erlv8_vm:enqueue_tick(VM,{proplist, Resource}).

set_value(Key, Value, {_Erlv8Obj,  Resource, VM}) ->
    erlv8_vm:enqueue_tick(VM, {set, Resource, Key, Value}).

set_value(Key,Value,PropertyAttribute, {_Erlv8Obj,  Resource, VM}) ->
    erlv8_vm:enqueue_tick(VM, {set, Resource, Key, Value, PropertyAttribute}).

set_hidden_value(Key,Value, {_Erlv8Obj,  Resource, VM}) ->
    erlv8_vm:enqueue_tick(VM, {set_hidden, Resource, Key, Value}).

get_value(Key, {_Erlv8Obj,  _Resource, _VM} = Self) ->
    get_value(Key, undefined, Self).

get_value(Key, Default, {_Erlv8Obj,  Resource, VM}) ->
    case erlv8_vm:enqueue_tick(VM, {get, Resource, Key}) of
        undefined ->
            Default;
        Val ->
            Val
    end.

get_hidden_value(Key, {_Erlv8Obj,  _Resource, _VM} = Self) ->
    get_hidden_value(Key, undefined, Self).

get_hidden_value(Key, Default, {_Erlv8Obj,  Resource, VM}) ->
    case erlv8_vm:enqueue_tick(VM, {get_hidden, Resource, Key}) of
        undefined ->
            Default;
        Val ->
            Val
    end.

internal_field_count({_Erlv8Obj,  Resource, VM}) ->
    erlv8_vm:enqueue_tick(VM, {internal_count, Resource}).

get_internal_field(Index, {_Erlv8Obj,  Resource, VM}) ->
    erlv8_vm:enqueue_tick(VM, {get_internal, Resource, Index}).

set_internal_field(Index, {extern, Value}, {_Erlv8Obj,  Resource, VM}) ->
    erlv8_vm:enqueue_tick(VM, {set_internal_extern, Resource, Index, Value, erlv8_extern:type(Value)});

set_internal_field(Index, Value, {_Erlv8Obj,  Resource, VM}) ->
    erlv8_vm:enqueue_tick(VM, {set_internal, Resource, Index, Value}).

set_prototype(Proto, {_Erlv8Obj,  Resource, VM}) ->
    erlv8_vm:enqueue_tick(VM, {set_proto, Resource, Proto}).

get_prototype({_Erlv8Obj,  Resource, VM}) ->
    erlv8_vm:enqueue_tick(VM, {get_proto, Resource}).

delete(Key, {_Erlv8Obj,  Resource, VM}) ->
    erlv8_vm:enqueue_tick(VM, {delete, Resource, Key}).

set_accessor(Property, Getter, {_Erlv8Obj,  Resource, VM}) ->
    case erlv8_vm:enqueue_tick(VM, {set_accessor, Resource, Property, Getter}) of
        badarg ->
            throw(badarg);
        Result ->
            Result
    end.

set_accessor(Property, Getter, Setter, {_Erlv8Obj,  Resource, VM}) ->
    case erlv8_vm:enqueue_tick(VM, {set_accessor, Resource, Property, Getter, Setter}) of
        badarg ->
            throw(badarg);
        Result ->
            Result
    end.

set_accessor(Property, Getter, Setter, AccessControl, {_Erlv8Obj,  Resource, VM}) ->
    case erlv8_vm:enqueue_tick(VM, {set_accessor, Resource, Property, Getter, Setter, AccessControl}) of
        badarg ->
            throw(badarg);
        Result ->
            Result
    end.

set_accessor(Property, Getter, Setter, AccessControl, PropertyAttribute, {_Erlv8Obj,  Resource, VM}) ->
    case erlv8_vm:enqueue_tick(VM, {set_accessor, Resource, Property, Getter, Setter, AccessControl, PropertyAttribute}) of
        badarg ->
            throw(badarg);
        Result ->
            Result
    end.

equals({_Tag,AnotherObject,_}, {_Erlv8Obj,  Resource, VM}) ->
    erlv8_value:equals(VM, Resource, AnotherObject).

strict_equals({_Tag,AnotherObject,_}, {_Erlv8Obj,  Resource, VM}) ->
    erlv8_value:strict_equals(VM, Resource, AnotherObject).

call(Fun, Self) ->
    call(Fun,[], Self).

call(Fun,Args, {_Erlv8Obj,  Resource, VM}) ->
    Fun:call({erlv8_object, Resource,VM}, Args).

new(O) ->
    new(O,undefined).

new(O, V) ->
    #erlv8_object{resource = O, vm = V}.
