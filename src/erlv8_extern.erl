-module(erlv8_extern).
-export([get_proto/2, extern/2]).

get_proto(VM, Proto) ->
	erlv8_vm:enqueue_tick(VM, {extern_proto, Proto}).

extern(VM, Value) when is_number(Value) ->
	erlv8_vm:enqueue_tick(VM, {externalize, num, Value});
extern(VM, Value) when is_atom(Value) ->
	erlv8_vm:enqueue_tick(VM, {externalize, atom, Value});
extern(VM, Value) when is_binary(Value) ->
	erlv8_vm:enqueue_tick(VM, {externalize, bin, Value});
extern(VM, Value) when is_reference(Value) ->
	erlv8_vm:enqueue_tick(VM, {externalize, ref, Value});
extern(VM, Value) when is_function(Value) ->
	erlv8_vm:enqueue_tick(VM, {externalize, 'fun', Value});
extern(VM, Value) when is_port(Value) ->
	erlv8_vm:enqueue_tick(VM, {externalize, port, Value});
extern(VM, Value) when is_pid(Value) ->
	erlv8_vm:enqueue_tick(VM, {externalize, pid, Value});
extern(VM, Value) when is_tuple(Value) ->
	erlv8_vm:enqueue_tick(VM, {externalize, tuple, Value});
extern(VM, Value) when is_list(Value) ->
	erlv8_vm:enqueue_tick(VM, {externalize, list, Value}).

	
