-module(erlv8).
-export([start/0,stop/0,new_script/1]).
-include_lib("erlv8/include/erlv8.hrl").

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

script_change_before_start_test() ->
	start(),
	{ok, Pid} = new_script("1+1;"),
	Self = self(),
	erlv8_script:add_handler(Pid,erlv8_capturer,[fun (X) -> Self ! X end]),
	erlv8_script:source(Pid,"2+2;"),
	erlv8_script:run(Pid),
	receive 
		{finished, 4} ->
			ok;
		Other ->
			error({bad_result,Other})
	end,
	stop().

script_change_rerun_test() ->
	start(),
	{ok, Pid} = new_script("1+1;"),
	Self = self(),
	erlv8_script:add_handler(Pid,erlv8_capturer,[fun (X) -> Self ! X end]),
	erlv8_script:run(Pid),
	receive 
		{finished, 2} ->
			ok;
		Other ->
			error({bad_result,Other})
	end,
	erlv8_script:source(Pid,"2+2;"),
	erlv8_script:run(Pid),
	receive 
		{finished, 4} ->
			ok;
		Other1 ->
			error({bad_result,Other1})
	end,
	stop().

compilation_error_test() ->
	start(),
	{ok, Pid} = new_script("1+;"),
	Self = self(),
	erlv8_script:add_handler(Pid,erlv8_capturer,[fun (X) -> Self ! X end]),
	erlv8_script:run(Pid),
	receive 
		{compilation_failed, _} ->
			ok;
		Other ->
			error({bad_result,Other})
	end,
	stop().

script_stopping_test() ->
	start(),
	{ok, Pid} = new_script(""),
	?assert(is_pid(Pid)),
	Self = self(),
	erlv8_script:add_handler(Pid,erlv8_capturer,[fun (X) -> Self ! X end]),
	erlv8_script:run(Pid),
	receive 
		{finished, _} ->
			erlv8_script:stop(Pid),
			?assertEqual(false,erlang:is_process_alive(Pid));
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

term_to_js_object_test() ->
	start(),
	{ok, Pid} = new_script(""),
	Self = self(),
	erlv8_script:add_handler(Pid,erlv8_capturer,[fun (X) -> Self ! X end]),
	erlv8_script:global(Pid,[{"a",1},{"b","c"},{"c",[]}]),
	erlv8_script:run(Pid),
	receive 
		{finished, _} ->
			?assertEqual([{"a",1},{"b","c"},{"c",[]}],erlv8_script:global(Pid));
		Other ->
			error({bad_result,Other})
	end,
	stop().

term_to_js_boolean_test() ->
	start(),
	{ok, Pid} = new_script(""),
	Self = self(),
	erlv8_script:add_handler(Pid,erlv8_capturer,[fun (X) -> Self ! X end]),
	erlv8_script:global(Pid,[{"a",true},{"b",false}]),
	erlv8_script:run(Pid),
	receive 
		{finished, _} ->
			?assertEqual([{"a",true},{"b",false}],erlv8_script:global(Pid));
		Other ->
			error({bad_result,Other})
	end,
	stop().

term_to_js_undefined_test() ->
	start(),
	{ok, Pid} = new_script(""),
	Self = self(),
	erlv8_script:add_handler(Pid,erlv8_capturer,[fun (X) -> Self ! X end]),
	erlv8_script:global(Pid,[{"a",undefined}]),
	erlv8_script:run(Pid),
	receive 
		{finished, _} ->
			?assertEqual([{"a",undefined}],erlv8_script:global(Pid));
		Other ->
			error({bad_result,Other})
	end,
	stop().

term_to_js_number_test() ->
	start(),
	{ok, Pid} = new_script(""),
	Self = self(),
	erlv8_script:add_handler(Pid,erlv8_capturer,[fun (X) -> Self ! X end]),
	erlv8_script:global(Pid,[{"a",2147483648},{"b",-2147483649},{"c",1},{"d",4294967296},{"dd",4294967297},{"e",3.555}]),
	erlv8_script:run(Pid),
	receive 
		{finished, _} ->
			?assertEqual([{"a",2147483648},{"b",-2147483649},{"c",1},{"d",4294967296},{"dd",4294967297},{"e",3.555}],erlv8_script:global(Pid));
		Other ->
			error({bad_result,Other})
	end,
	stop().

term_to_js_fun_test() ->
	start(),
	{ok, Pid} = new_script("x = function () {}"),
	Self = self(),
	erlv8_script:add_handler(Pid,erlv8_capturer,[fun (X) -> Self ! X end]),
	erlv8_script:global(Pid,[]),
	erlv8_script:run(Pid),
	receive 
		{finished, _} ->
			?assertMatch([{"x",{erlv8_fun,_}}],erlv8_script:global(Pid));
		Other ->
			error({bad_result,Other})
	end,
	stop().

invocation_test() ->
	start(),
	{ok, Pid} = new_script("test()"),
	Self = self(),
	erlv8_script:add_handler(Pid,erlv8_capturer,[fun (X) -> Self ! X end]),
	erlv8_script:register(Pid, test, fun () -> F = fun (_Script, #erlv8_fun_invocation{} = _Invocation) -> 123 end, F end),
	erlv8_script:run(Pid),
	receive 
		{finished, 123} ->
			ok;
		Other ->
			error({bad_result,Other})
	end,
	stop().

fun_test() ->
	start(),
	{ok, Pid} = new_script("f = function() { return test(321) }; test0(f);"),
	Self = self(),
	erlv8_script:add_handler(Pid,erlv8_capturer,[fun (X) -> Self ! X end]),
	erlv8_script:register(Pid, test0, fun () -> F = fun (_Script, #erlv8_fun_invocation{} = _Invocation, F) -> F:call() end, F end),
	erlv8_script:register(Pid, test, fun () -> F = fun (_Script, #erlv8_fun_invocation{} = _Invocation,Val) -> Val end, F end),
	erlv8_script:run(Pid),
	receive 
		{finished, 321} ->
			ok;
		Other ->
			error({bad_result,Other})
	end,
	stop().

fun_script_is_pid_test() ->
	start(),
	{ok, Pid} = new_script("test();"),
	Self = self(),
	erlv8_script:add_handler(Pid,erlv8_capturer,[fun (X) -> Self ! X end]),
	erlv8_script:register(Pid, test, fun () -> F = fun (Script, #erlv8_fun_invocation{} = _Invocation) -> is_pid(Script) end, F end),
	erlv8_script:run(Pid),
	receive 
		{finished, true} ->
			ok;
		Other ->
			error({bad_result,Other})
	end,
	stop().

fun_returning_fun_test() ->
	start(),
	{ok, Pid} = new_script("f = function() {}; test(f);"),
	Self = self(),
	erlv8_script:add_handler(Pid,erlv8_capturer,[fun (X) -> Self ! X end]),
	erlv8_script:register(Pid, test, fun () -> F = fun (_Script, #erlv8_fun_invocation{} = _Invocation,Val) -> Val end, F end),
	erlv8_script:run(Pid),
	receive 
		{finished, {erlv8_fun, _}} ->
			ok;
		Other ->
			error({bad_result,Other})
	end,
	stop().

fun_new_script_inside_test() ->
	start(),
	{ok, Pid} = new_script("test()"),
	Self = self(),
	erlv8_script:add_handler(Pid,erlv8_capturer,[fun (X) -> Self ! X end]),
	erlv8_script:register(Pid, test, fun () -> F = fun (_Script, #erlv8_fun_invocation{} = _Invocation) -> new_script(""), 321 end, F end),
	erlv8_script:run(Pid),
	receive 
		{finished, 321} ->
			ok;
		Other ->
			error({bad_result,Other})
	end,
	stop().


to_string_test() ->
	start(),
	{ok, Pid} = new_script(""),
	?assertEqual("1",erlv8_script:to_string(Pid,1)),
	?assertEqual("1",erlv8_script:to_string(Pid,"1")),
	?assertEqual("true",erlv8_script:to_string(Pid,true)),
	?assertEqual("[object Object]",erlv8_script:to_string(Pid,[])),
	stop().

to_detail_string_test() ->
	start(),
	{ok, Pid} = new_script(""),
	?assertEqual("1",erlv8_script:to_string(Pid,1)),
	?assertEqual("1",erlv8_script:to_string(Pid,"1")),
	?assertEqual("true",erlv8_script:to_string(Pid,true)),
	?assertEqual("[object Object]",erlv8_script:to_string(Pid,[])),
	stop().

-endif.
