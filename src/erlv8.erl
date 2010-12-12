-module(erlv8).
-export([start/0,stop/0,new_script/1]).

start() ->
	application:start(erlv8).

stop() ->
	application:stop(erlv8).

new_script(Buf) ->
	Script = erlv8_nif:new_script(Buf),
	supervisor:start_child(erlv8_sup,[Script]).


%% TESTS
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
valid_script_creation_test() ->
	start(),
	{ok, Pid} = new_script("1+1"),
	?assert(is_pid(Pid)),
	stop().

apply_test() ->
	start(),
	{ok, Pid} = new_script("Erlang.__call__('io','format',['Hello world~n']);"),
	erlv8_script:run(Pid),
	timer:sleep(1000),
	stop().

registration_for_aliasing_test() ->
	start(),
	{ok, Pid} = new_script("Erlang.__call__('IO','format',['Hello world~n']);"),
	erlv8_script:register(Pid,'IO',io),
	erlv8_script:run(Pid),
	timer:sleep(1000),
	stop().



-endif.
