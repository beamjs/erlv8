-module(erlv8_nif).
-on_load(init/0).

-export([new_script/0,set_server/2,to_string/2,to_detail_string/2,tick/3]).

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


new_script() ->
	error(not_loaded).

set_server(_ScriptObject,_Pid) ->
	error(not_loaded).

to_string(_ScriptObject,_Obj) ->
	error(not_loaded).

to_detail_string(_ScriptObject,_Obj) ->
	error(not_loaded).

tick(_ScriptObject, _Ref, _Tick) ->
	error(not_loaded).
