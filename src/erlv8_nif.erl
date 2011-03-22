-module(erlv8_nif).
-on_load(init/0).

-export([init/0, new_vm/0, set_server/2, global/1, context/1, new_context/1, tick/3]).

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

