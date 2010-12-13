-module(erlv8_nif).
-on_load(init/0).

-export([new_script/1,get_script/1,run/2,register/3,script_send/2,result/2,get_global/1,set_global/2]).

init() ->
	case code:which(erlv8_nif) of
		Filename when is_list(Filename) ->
			erlang:load_nif(filename:join([filename:dirname(Filename),"../priv/erlv8_drv"]), 0);
		Err ->
			Err
	end.

new_script(_Buf) ->
	error(not_loaded).

get_script(_ScriptObject) ->
	error(not_loaded).

run(_ScriptObject,_Server) ->
	error(not_loaded).

register(_ScriptObject,_Name,_Exports) ->
	error(not_loaded).

script_send(_ScriptObject, _Data) ->
	error(not_loaded).

result(_ScriptObject, _Data) ->
	error(not_loaded).

get_global(_ScriptObject) ->
	error(not_loaded).

set_global(_ScriptObject,_Global) ->
	error(not_loaded).
