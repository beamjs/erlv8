-module(erlv8_nif).
-on_load(init/0).

-export([new_script/1,get_script/1,run/2]).

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
