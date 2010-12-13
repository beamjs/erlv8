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

apply_test() ->
	start(),
	{ok, Pid} = new_script("__call__('io','format',['Hello world~n']);"),
	erlv8_script:register(Pid,'__call__',erlv8_mod_call),
	Self = self(),
	erlv8_script:add_handler(Pid,erlv8_capturer,[fun (X) -> Self ! X end]),
	erlv8_script:run(Pid),
	receive
		{finished, _} ->
			ok
	end,
	stop().

aliasing_test() ->
	start(),
	{ok, Pid} = new_script("__call__('IO','format',['Hello world~n']);"),
	erlv8_script:register(Pid,'__call__',erlv8_mod_call),
	erlv8_script:alias(Pid,'IO',io),
	Self = self(),
	erlv8_script:add_handler(Pid,erlv8_capturer,[fun (X) -> Self ! X end]),
	erlv8_script:run(Pid),
	receive
		{finished, _} ->
			ok
	end,
	stop().

exports_test() ->
	start(),
	{ok, Pid} = new_script("exports.test = 1; __call__('io','format',['Exports: ~p~n',[exports]]);"),
	erlv8_script:register(Pid,'__call__',erlv8_mod_call),
	erlv8_script:register(Pid,exports,erlv8_mod_exports),
	erlv8_script:run(Pid),
	Self = self(),
	erlv8_script:add_handler(Pid,erlv8_capturer,[fun (X) -> Self ! X end]),
	receive
		{finished, _} ->
			ok
	end,
	stop().


-endif.
