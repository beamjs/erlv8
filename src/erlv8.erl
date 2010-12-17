-module(erlv8).
-export([start/0,stop/0]).
-include_lib("erlv8/include/erlv8.hrl").

start() ->
	application:start(erlv8).

stop() ->
	application:stop(erlv8).

%% TESTS
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
valid_script_creation_test() ->
	start(),
	{ok, Script} = erlv8_script:new(),
	?assert(is_pid(Script)),
	?assertEqual({ok, 2}, erlv8_script:run(Script,"1+1;")),
	ok = stop().

few_scripts_test() ->
	start(),
	{ok, Script} = erlv8_script:new(),
	?assertEqual({ok,2}, erlv8_script:run(Script,"1+1;")),
	?assertEqual({ok,4}, erlv8_script:run(Script,"2*2;")),
	stop().


compilation_error_test() ->
	start(),
	{ok, Script} = erlv8_script:new(),
	?assertMatch({compilation_failed, _}, erlv8_script:run(Script,"1+;")),
	stop().

script_stopping_test() ->
	start(),
	{ok, Script} = erlv8_script:new(),
	erlv8_script:stop(Script),
	timer:sleep(100), %% allow time for process to stop
	?assertEqual(false,erlang:is_process_alive(Script)),
	stop().

script_global_test() ->
	start(),
	{ok, Script} = erlv8_script:new(),
	erlv8_script:run(Script,"var a = 1+1;"),
	?assertEqual([{"a",2}],erlv8_script:global(Script)),
	stop().

script_set_global_test() ->
	start(),
	{ok, Script} = erlv8_script:new(),
	erlv8_script:global(Script,[{"a",1}]),
	erlv8_script:run(Script,"var b = a+1;"),
	?assertEqual([{"a",1},{"b",2}],erlv8_script:global(Script)),
	stop().

term_to_js_object_test() ->
	start(),
	{ok, Script} = erlv8_script:new(),
	erlv8_script:global(Script,[{"a",1},{"b","c"},{"c",[]}]),
	?assertEqual([{"a",1},{"b","c"},{"c",[]}],erlv8_script:global(Script)),
	stop().

term_to_js_boolean_test() ->
	start(),
	{ok, Script} = erlv8_script:new(),
	erlv8_script:global(Script,[{"a",true},{"b",false}]),
	?assertEqual([{"a",true},{"b",false}],erlv8_script:global(Script)),
	stop().

term_to_js_atom_test() ->
	start(),
	{ok, Script} = erlv8_script:new(),
	erlv8_script:global(Script,[{a,b},{c,d}]),
	?assertEqual([{"a","b"},{"c","d"}],erlv8_script:global(Script)),
	stop().

term_to_js_undefined_test() ->
	start(),
	{ok, Script} = erlv8_script:new(),
	erlv8_script:global(Script,[{"a",undefined}]),
	?assertEqual([{"a",undefined}],erlv8_script:global(Script)),
	stop().

term_to_js_number_test() ->
	start(),
	{ok, Script} = erlv8_script:new(),
	erlv8_script:global(Script,[{"a",2147483648},{"b",-2147483649},{"c",1},{"d",4294967296},{"dd",4294967297},{"e",3.555}]),
	?assertEqual([{"a",2147483648},{"b",-2147483649},{"c",1},{"d",4294967296},{"dd",4294967297},{"e",3.555}],erlv8_script:global(Script)),
	stop().

js_to_term_fun_test() ->
	start(),
	{ok, Script} = erlv8_script:new(),
	erlv8_script:run(Script,"x = function () {}"),
	?assertMatch([{"x",{erlv8_fun,_,Script,[]}}],erlv8_script:global(Script)),
	stop().

js_object_to_term_fun_test() ->
	start(),
	{ok, Script} = erlv8_script:new(),
	erlv8_script:run(Script,"x = function () {}; x.a = 1"),
	?assertMatch([{"x",{erlv8_fun,_,Script,[{"a",1}]}}],erlv8_script:global(Script)),
	stop().

term_to_js_object_fun_erlv8_fun_test() ->
	start(),
	{ok, Script} = erlv8_script:new(),
	{ok, Fun} = erlv8_script:run(Script,"x = function () {}; x.a = 1; x"),
	?assertMatch({erlv8_fun,_,Script,[{"a",1}]},Fun),
	erlv8_script:global(Script,[{"y",Fun}]),
	?assertMatch({ok, 1}, erlv8_script:run(Script,"y.a")),
	stop().

term_to_js_object_fun_test() ->
	start(),
	{ok, Script} = erlv8_script:new(),
	erlv8_script:global(Script,[{"x",erlv8_funobj:new(fun (#erlv8_fun_invocation{},[]) -> 123 end, [{"y",1}])}]),
	?assertMatch({ok, 1}, erlv8_script:run(Script,"x.y")),
	?assertMatch({ok, 123}, erlv8_script:run(Script,"x()")),
	stop().

term_to_js_error_test() ->
	start(),
	{ok, Script} = erlv8_script:new(),
	erlv8_script:global(Script,[{"x",fun (#erlv8_fun_invocation{},[]) -> {throw, {error, "Hello"}} end}]),
	{exception, Exception} = erlv8_script:run(Script,"x()"),
	?assertEqual("Hello", proplists:get_value("message",Exception)),
	erlv8_script:global(Script,[{"x",fun (#erlv8_fun_invocation{},[]) -> {throw, "Goodbye"} end}]),
	{exception, "Goodbye"} = erlv8_script:run(Script,"x()"),
	stop().

object_fun_test() ->
	start(),
	{ok, Script} = erlv8_script:new(),
	{ok, Fun} = erlv8_script:run(Script,"f = function() {}; f.y = 1; f"),
	?assertEqual([{"y",1}],Fun:object()),
	stop().


funobj_test() ->
	FunObj = erlv8_funobj:new(fun (#erlv8_fun_invocation{},[]) -> 123 end, [{"y",1}]),
	?assertEqual([{"y",1}],FunObj:object()).
	

invocation_test() ->
	start(),
	{ok, Script} = erlv8_script:new(),
	erlv8_script:register(Script, "test", fun () -> F = fun (#erlv8_fun_invocation{} = _Invocation,[]) -> 123 end, F end),
	?assertEqual({ok, 123}, erlv8_script:run(Script,"test()")),
	stop().

fun_test() ->
	start(),
	{ok, Script} = erlv8_script:new(),
	erlv8_script:register(Script, "test0", fun () -> F = fun (#erlv8_fun_invocation{} = _Invocation, [F]) -> F:call([321]) end, F end),
	erlv8_script:register(Script, "test", fun () -> F = fun (#erlv8_fun_invocation{} = _Invocation,[Val]) -> Val end, F end),
	?assertEqual({ok, 321}, erlv8_script:run(Script,"f = function(x) { return test(x) }; test0(f);")),
	stop().

fun_server_is_pid_test() ->
	start(),
	{ok, Script} = erlv8_script:new(),
	erlv8_script:register(Script, "test", fun () -> F = fun (#erlv8_fun_invocation{ server = Server} = _Invocation,[]) -> is_pid(Server) end, F end),
	?assertEqual({ok, true}, erlv8_script:run(Script,"test();")),
	stop().

fun_returning_fun_test() ->
	start(),
	{ok, Script} = erlv8_script:new(),
	erlv8_script:register(Script, "test", fun () -> F = fun (#erlv8_fun_invocation{} = _Invocation,[Val]) -> Val end, F end),
	?assertMatch({ok, {erlv8_fun, _, Script, []}}, erlv8_script:run(Script,"f = function() {}; test(f);")),
	stop().

fun_new_script_inside_test() ->
	start(),
	{ok, Script} = erlv8_script:new(),
	erlv8_script:register(Script, "test", fun () -> F = fun (#erlv8_fun_invocation{} = _Invocation,[]) -> {ok, _Pid} = erlv8_script:new(), 321 end, F end),
	?assertEqual({ok, 321},erlv8_script:run(Script, "test()")),
	stop().

fun_this_test() ->
	start(),
	{ok, Script} = erlv8_script:new(),
	erlv8_script:global(Script,[{"x",fun (#erlv8_fun_invocation{}=I,[]) -> I:this() end}]),
	?assertEqual({ok, erlv8_script:global(Script)}, erlv8_script:run(Script,"x()")),
	stop().

fun_is_construct_call_test() ->
	start(),
	{ok, Script} = erlv8_script:new(),
	erlv8_script:global(Script,[{"x",fun (#erlv8_fun_invocation{}=I,[]) -> I:is_construct_call() end}]),
	?assertEqual({ok, false}, erlv8_script:run(Script,"x()")),
	erlv8_script:global(Script,[{"x",fun (#erlv8_fun_invocation{}=I,[]) -> I:this([{"icc",I:is_construct_call()}]) end}]),
	?assertEqual({ok, true}, erlv8_script:run(Script,"new x().icc")),
	stop().

fun_global_test() ->
	start(),
	{ok, Script} = erlv8_script:new(),
	erlv8_script:global(Script,[{"a", 1}, {"x",fun (#erlv8_fun_invocation{}=I,[]) -> 
													   G0 = I:global(),
													   G1 = [{"a", 2}, proplists:delete("a", G0)],
													   I:global(G1)
											   end}]),
	erlv8_script:run(Script,"x()"),
	?assertMatch([{"a",1},{"x", _}], erlv8_script:global(Script)),
	stop().

fun_callback_test() ->
	start(),
	{ok, Script} = erlv8_script:new(),
	Self = self(),
	erlv8_script:register(Script, "test", fun () -> F = fun (#erlv8_fun_invocation{} = _Invocation, [Cb]) -> 
														   spawn(fun () ->
																		 timer:sleep(1000), %% allow ample time
																		 Self ! {ok, Cb:call([1])}
																 end)
												   end, F end),
	
	erlv8_script:run(Script,"f = function(x) { return x}; test(f);"),
	receive
		{ok, 1} ->
			ok;
		Other1 ->
			error({bad_result,Other1})
	end,
	stop().



to_string_test() ->
	start(),
	{ok, Script} = erlv8_script:new(),
	?assertEqual("1",erlv8_script:to_string(Script,1)),
	?assertEqual("1",erlv8_script:to_string(Script,"1")),
	?assertEqual("true",erlv8_script:to_string(Script,true)),
	?assertEqual("[object Object]",erlv8_script:to_string(Script,[])),
	stop().

to_detail_string_test() ->
	start(),
	{ok, Script} = erlv8_script:new(),
	?assertEqual("1",erlv8_script:to_string(Script,1)),
	?assertEqual("1",erlv8_script:to_string(Script,"1")),
	?assertEqual("true",erlv8_script:to_string(Script,true)),
	?assertEqual("[object Object]",erlv8_script:to_string(Script,[])),
	stop().

-endif.
