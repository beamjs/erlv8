-module(erlv8).
-export([start/0,stop/0,new_script/1,load_file/1]).

start() ->
	application:start(erlv8).

stop() ->
	application:stop(erlv8).

new_script(Buf) ->
	Script = erlv8_nif:new_script(Buf),
	supervisor:start_child(erlv8_sup,[Script]).

load_file(Filename) ->
	{ok, B} = file:read_file(Filename),
	L = binary_to_list(B),
	erlv8:new_script(L).


%% TESTS
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
valid_script_creation_test() ->
	start(),
	{ok, Pid} = new_script("1+1;"),
	?assert(is_pid(Pid)),
	Self = self(),
	erlv8_script:add_handler(Pid,erlv8_capturer,[fun (X) -> Self ! X end]),
	erlv8_script:run(Pid),
	receive 
		{finished, 2} ->
			ok;
		Other ->
			error({bad_result,Other})
	end,
	stop().

script_global_test() ->
	start(),
	{ok, Pid} = new_script("var a = 1+1;"),
	Self = self(),
	erlv8_script:add_handler(Pid,erlv8_capturer,[fun (X) -> Self ! X end]),
	erlv8_script:run(Pid),
	receive 
		{finished, _} ->
			?assertEqual([{"a",2}],erlv8_script:global(Pid));
		Other ->
			error({bad_result,Other})
	end,
	stop().

script_set_global_test() ->
	start(),
	{ok, Pid} = new_script("var b = a+1;"),
	Self = self(),
	erlv8_script:add_handler(Pid,erlv8_capturer,[fun (X) -> Self ! X end]),
	erlv8_script:global(Pid,[{"a",1}]),
	erlv8_script:run(Pid),
	receive 
		{finished, _} ->
			?assertEqual([{"a",1},{"b",2}],erlv8_script:global(Pid));
		Other ->
			error({bad_result,Other})
	end,
	stop().



-endif.
