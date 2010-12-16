-module(erlv8_fun,[Resource,Script]).
-export([call/0,call/1]).

call() ->
	call([]).

call(Args) ->
	erlv8_script:next_tick(Script, {call, Resource, Args}).
