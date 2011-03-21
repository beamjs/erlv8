-module(erlv8_nif).
-on_load(init/0).

-export([init/0,new_vm/0,set_server/2,global/1,context/1, new_context/1, tick/3,
		 object_set_accessor/3, object_set_accessor/4,
         object_set_accessor/5, object_set_accessor/6, object_set_accessor/7,
		 value_equals/3, value_strict_equals/3, value_taint/2]).

-define(DEFAULT_PREEMPTION, 100).

init() ->
	Preemption = 
		case application:get_env(erlv8, preemption_ms) of
			{ok, V} ->
				V;
			_ ->
				?DEFAULT_PREEMPTION
		end,
	case os:getenv("ERLV8_SO_PATH") of
		false ->
			case code:which(erlv8_nif) of
				Filename when is_list(Filename) ->
					erlang:load_nif(filename:join([filename:dirname(Filename),"../priv/erlv8_drv"]), Preemption);
				Err ->
					Err
			end;
		Path ->
			Filename = filename:join([Path,"erlv8_drv"]),
			erlang:load_nif(Filename,Preemption)
	end.


new_vm() ->
	error(not_loaded).

set_server(_VMObject,_Pid) ->
	error(not_loaded).

context(_VMObject) ->
	error(not_loaded).

new_context(_VMObject) ->
	error(not_loaded).

global(_ContextObject) ->
	error(not_loaded).

tick(_VMObject, _Ref, _Tick) ->
	error(not_loaded).

object_set_accessor(_ObjectRes, _Name, _Getter) ->
	error(not_loaded).

object_set_accessor(_ObjectRes, _Name, _Getter, _Setter) ->
	error(not_loaded).

object_set_accessor(_ObjectRes, _Name, _Getter, _Setter, _Data) ->
	error(not_loaded).

object_set_accessor(_ObjectRes, _Name, _Getter, _Setter, _Data, _Setting) ->
	error(not_loaded).

object_set_accessor(_ObjectRes, _Name, _Getter, _Setter, _Data, _Setting, _Attribute) ->
	error(not_loaded).


value_equals(_VMRes, _Val1,_Val2) ->
	error(not_loaded).

value_strict_equals(_VMRes, _Val1,_Val2) ->
	error(not_loaded).

value_taint(_VM, _Val) ->
	error(not_loaded).
