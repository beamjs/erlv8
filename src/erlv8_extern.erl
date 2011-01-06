-module(erlv8_extern).
-export([get_proto/2, extern/2, type/1]).

get_proto(VM, Proto) ->
	erlv8_vm:enqueue_tick(VM, {extern_proto, Proto}).

extern(VM, Value) ->
	erlv8_vm:enqueue_tick(VM, {externalize, type(Value), Value}).
	
type(Value) when is_number(Value) ->
	num;
type(Value) when is_atom(Value) ->
	atom;
type(Value) when is_binary(Value) ->
	bin;
type(Value) when is_reference(Value) ->
	ref;
type(Value) when is_function(Value) ->
	'fun';
type(Value) when is_port(Value) ->
	port;
type(Value) when is_pid(Value) ->
	pid;
type(Value) when is_tuple(Value) ->
	tuple;
type(Value) when is_list(Value) ->
	list.

