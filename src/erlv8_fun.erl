-module(erlv8_fun,[Resource,Script,Object]).
-export([call/0,call/1,object/0]).

call() ->
	call([]).

call(Args) ->
	erlv8_script:next_tick(Script, {call, Resource, Args}).

object() ->
	Object.
