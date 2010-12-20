-module(erlv8_nif).
-on_load(init/0).

-export([new_vm/0,set_server/2,global/1,to_string/2,to_detail_string/2,tick/3,to_proplist/1,to_list/1,object_set/3,object_get/2,
		 object_set_hidden/3, object_get_hidden/2,object_set_proto/2, object_get_proto/1, object_delete/2, 
		 object_set_accessor/3, object_set_accessor/4, object_set_accessor/5, object_set_accessor/6, object_set_accessor/7,
		 value_equals/2, value_strict_equals/2, value_taint/2]).

init() ->
	case os:getenv("ERLV8_SO_PATH") of
		false ->
			case code:which(erlv8_nif) of
				Filename when is_list(Filename) ->
					erlang:load_nif(filename:join([filename:dirname(Filename),"../priv/erlv8_drv"]), 0);
				Err ->
					Err
			end;
		Path ->
			Filename = filename:join([Path,"erlv8_drv"]),
			erlang:load_nif(Filename,0)
	end.


new_vm() ->
	error(not_loaded).

set_server(_VMObject,_Pid) ->
	error(not_loaded).

global(_VMObject) ->
	error(not_loaded).

to_string(_VMObject,_Obj) ->
	error(not_loaded).

to_detail_string(_VMObject,_Obj) ->
	error(not_loaded).

tick(_VMObject, _Ref, _Tick) ->
	error(not_loaded).

to_proplist(_ObjectRes) ->
	error(not_loaded).

to_list(_ObjectRes) ->
	error(not_loaded).

object_set(_ObjectRes, _Key, _Value) ->
	error(not_loaded).

object_get(_ObjectRes, _Key) ->
	error(not_loaded).

object_set_hidden(_ObjectRes, _Key, _Value) ->
	error(not_loaded).

object_get_hidden(_ObjectRes, _Key) ->
	error(not_loaded).

object_set_proto(_ObjectRes, _ProtoObjectRes) ->
	error(not_loaded).

object_get_proto(_ObjectRes) ->
	error(not_loaded).

object_delete(_ObjectRes, _Key) ->
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


value_equals(_Val1,_Val2) ->
	error(not_loaded).

value_strict_equals(_Val1,_Val2) ->
	error(not_loaded).

value_taint(_VM, _Val) ->
	error(not_loaded).
