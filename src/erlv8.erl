-module(erlv8).
-export([start/0,stop/0]).
-include_lib("erlv8/include/erlv8.hrl").

start() ->
	application:start(erlv8).

stop() ->
	application:stop(erlv8).

%% TESTS
-include_lib("eunit/include/eunit.hrl").%
-ifdef(TEST).
valid_vm_creation_test() ->
	start(),
	{ok, VM} = erlv8_vm:new(),
	?assert(is_pid(VM)),
	?assertEqual({ok, 2}, erlv8_vm:run(VM,"1+1;")),
	ok = stop().

few_vms_test() ->
	start(),
	{ok, VM} = erlv8_vm:new(),
	?assertEqual({ok,2}, erlv8_vm:run(VM,"1+1;")),
	?assertEqual({ok,4}, erlv8_vm:run(VM,"2*2;")),
	stop().


compilation_error_test() ->
	start(),
	{ok, VM} = erlv8_vm:new(),
	?assertMatch({compilation_failed, _}, erlv8_vm:run(VM,"1+;")),
	stop().

vm_stopping_test() ->
	start(),
	{ok, VM} = erlv8_vm:new(),
	erlv8_vm:stop(VM),
	timer:sleep(100), %% allow time for process to stop
	?assertEqual(false,erlang:is_process_alive(VM)),
	stop().

vm_global_test() ->
	start(),
	{ok, VM} = erlv8_vm:new(),
	erlv8_vm:run(VM,"var a = 1+1;"),
	Global = erlv8_vm:global(VM),
	?assertEqual([{"a",2}],Global:proplist()),
	stop().

vm_set_global_test() ->
	start(),
	{ok, VM} = erlv8_vm:new(),
	Global = erlv8_vm:global(VM),
	Global:set_value("a",1),
	erlv8_vm:run(VM,"var b = a+1;"),
	?assertEqual([{"a",1},{"b",2}],Global:proplist()),
	stop().

term_to_js_object_test() ->
	start(),
	{ok, VM} = erlv8_vm:new(),
	Obj = erlv8_vm:taint(VM,?V8Obj([{"a",1},{"b","c"}])),
	?assertMatch([{"a",1},{"b","c"}],Obj:proplist()),
	stop().

term_to_js_boolean_test() ->
	start(),
	{ok, VM} = erlv8_vm:new(),
	?assertEqual(true, erlv8_vm:taint(VM,true)),
	?assertEqual(false, erlv8_vm:taint(VM,false)),
	stop().

term_to_js_atom_test() ->
	start(),
	{ok, VM} = erlv8_vm:new(),
	?assertEqual("a", erlv8_vm:taint(VM,a)),
	?assertEqual("b", erlv8_vm:taint(VM,b)),
	stop().

term_to_js_undefined_test() ->
	start(),
	{ok, VM} = erlv8_vm:new(),
	?assertEqual(undefined, erlv8_vm:taint(VM,undefined)),
	stop().

term_to_js_null_test() ->
	start(),
	{ok, VM} = erlv8_vm:new(),
	?assertEqual(null, erlv8_vm:taint(VM,null)),
	stop().

term_to_js_number_test() ->
	start(),
	{ok, VM} = erlv8_vm:new(),
	Nums = [2147483648,-2147483649,1,4294967296,4294967297,3.555],
	[ ?assertEqual(N, erlv8_vm:taint(VM,N)) || N <- Nums ],
	stop().

term_to_js_array_test() ->
	start(),
	{ok, VM} = erlv8_vm:new(),
	?assertEqual([1,2,3],erlv8_vm:taint(VM,[1,2,3])),
	?assertEqual([],erlv8_vm:taint(VM,[])),
	stop().

term_to_js_pid_test() ->
	start(),
	{ok, VM} = erlv8_vm:new(),
	?assertEqual(self(), erlv8_vm:taint(VM,self())),
	?assertEqual(self(), erlv8_vm:taint(VM,self())), % the second call is to ensure memory is managed properly (regression)
	stop().

term_to_js_unsupported_test() ->
	start(),
	{ok, VM} = erlv8_vm:new(),
	?assertEqual(undefined,erlv8_vm:taint(VM,{this_tuple,is_not_supported})),
	stop().

term_to_js_object_invalid_proplist_test() ->
	start(),
	{ok, VM} = erlv8_vm:new(),
	?assertEqual(undefined, erlv8_vm:taint(VM,?V8Obj([{"a",1},{b,2},{3,4}]))),
	stop().


js_to_term_fun_test() ->
	start(),
	{ok, VM} = erlv8_vm:new(),
	erlv8_vm:run(VM,"x = function () {}"),
	Global = erlv8_vm:global(VM),
	{erlv8_fun,_,VM} = Global:get_value("x"),
	stop().

js_object_to_term_fun_test() ->
	start(),
	{ok, VM} = erlv8_vm:new(),
	erlv8_vm:run(VM,"x = function () {}; x.a = 1"),
	Global = erlv8_vm:global(VM),
	X = Global:get_value("x"),
	O = X:object(),
	?assertEqual([{"a",1}],O:proplist()),
	stop().

term_to_js_object_fun_erlv8_fun_test() ->
	start(),
	{ok, VM} = erlv8_vm:new(),
	Global = erlv8_vm:global(VM),
	{ok, {erlv8_fun,_, VM}=Fun} = erlv8_vm:run(VM,"x = function () {}; x.a = 1; x"),
	O = Fun:object(),
	?assertEqual([{"a",1}],O:proplist()),
	Global:set_value("y",Fun),
	Y = Global:get_value("y"),
	YObj = Y:object(),
	?assertEqual(1, YObj:get_value("a")),
	stop().

term_to_js_object_fun_test() ->
	start(),
	{ok, VM} = erlv8_vm:new(),
	Global = erlv8_vm:global(VM),
	Global:set_value("x",fun (#erlv8_fun_invocation{},[]) -> 123 end),
	X = Global:get_value("x"),
	XObj = X:object(),
	XObj:set_value("y",1),
	?assertMatch({ok, 1}, erlv8_vm:run(VM,"x.y")),
	X0 = Global:get_value("x"), X1 = X0:object(),
	?assertMatch(1, X1:get_value("y")),
	?assertMatch({ok, 123}, erlv8_vm:run(VM,"x()")),
	stop().

term_to_js_error_test() ->
	start(),
	{ok, VM} = erlv8_vm:new(),
	Global = erlv8_vm:global(VM),
	Global:set_value("x",fun (#erlv8_fun_invocation{},[]) -> {throw, {error, "Hello"}} end),
	{exception, Exception} = erlv8_vm:run(VM,"x()"),
	?assertEqual("Hello", Exception:get_value("message")),
	Global:set_value("x",fun (#erlv8_fun_invocation{},[]) -> {throw, "Goodbye"} end),
	{exception, "Goodbye"} = erlv8_vm:run(VM,"x()"),
	stop().

object_fun_test() ->
	start(),
	{ok, VM} = erlv8_vm:new(),
	{ok, Fun} = erlv8_vm:run(VM,"f = function() {}; f.y = 1; f"),
	FunObj = Fun:object(),
	?assertEqual([{"y",1}],FunObj:proplist()),
	stop().

fun_obj_test() ->
	start(),
	{ok, VM} = erlv8_vm:new(),
	F = erlv8_vm:taint(VM, erlv8_fun:new(fun (#erlv8_fun_invocation{},[]) -> 1 end, erlv8_object:new([{"a",1}]))),
	FObj = F:object(),
	?assertEqual(1,FObj:get_value("a")),
	stop().

invocation_test() ->
	start(),
	{ok, VM} = erlv8_vm:new(),
	erlv8_vm:register(VM, "test", fun () -> F = fun (#erlv8_fun_invocation{} = _Invocation,[]) -> 123 end, F end),
	?assertEqual({ok, 123}, erlv8_vm:run(VM,"test()")),
	stop().

fun_test() ->
	start(),
	{ok, VM} = erlv8_vm:new(),
	erlv8_vm:register(VM, "test0", fun () -> F = fun (#erlv8_fun_invocation{} = _Invocation, [F]) -> F:call([321]) end, F end),
	erlv8_vm:register(VM, "test", fun () -> F = fun (#erlv8_fun_invocation{} = _Invocation,[Val]) -> Val end, F end),
	?assertEqual({ok, 321}, erlv8_vm:run(VM,"f = function(x) { return test(x) }; test0(f);")),
	stop().

fun_vm_is_pid_test() ->
	start(),
	{ok, VM} = erlv8_vm:new(),
	erlv8_vm:register(VM, "test", fun () -> F = fun (#erlv8_fun_invocation{ vm = VM1 } = _Invocation,[]) -> is_pid(VM1) end, F end),
	?assertEqual({ok, true}, erlv8_vm:run(VM,"test();")),
	stop().

fun_returning_fun_test() ->
	start(),
	{ok, VM} = erlv8_vm:new(),
	erlv8_vm:register(VM, "test", fun () -> F = fun (#erlv8_fun_invocation{} = _Invocation,[Val]) -> Val end, F end),
	{ok, {erlv8_fun, _, VM}=F} = erlv8_vm:run(VM,"f = function() {}; test(f);"),
	O = F:object(),
	?assertEqual([],O:proplist()),
	stop().

fun_new_vm_inside_test() ->
	start(),
	{ok, VM} = erlv8_vm:new(),
	erlv8_vm:register(VM, "test", fun () -> F = fun (#erlv8_fun_invocation{} = _Invocation,[]) -> {ok, _Pid} = erlv8_vm:new(), 321 end, F end),
	?assertEqual({ok, 321},erlv8_vm:run(VM, "test()")),
	stop().

fun_this_test() ->
	start(),
	{ok, VM} = erlv8_vm:new(),
	Global = erlv8_vm:global(VM),
	Global:set_value("x",fun (#erlv8_fun_invocation{}=I,[]) -> I:this() end),
	{ok, Result} = erlv8_vm:run(VM,"x()"),
	?assertEqual(Global:proplist(), Result:proplist()),
	stop().

fun_is_construct_call_test() ->
	start(),
	{ok, VM} = erlv8_vm:new(),
	Global = erlv8_vm:global(VM),
	Global:set_value("x",fun (#erlv8_fun_invocation{}=I,[]) -> I:is_construct_call() end),
	?assertEqual({ok, false}, erlv8_vm:run(VM,"x()")),
	Global:set_value("x",fun (#erlv8_fun_invocation{ this = This }=I,[]) -> This:set_value("icc",I:is_construct_call()) end),
	?assertEqual({ok, true}, erlv8_vm:run(VM,"new x().icc")),
	stop().

fun_global_test() ->
	start(),
	{ok, VM} = erlv8_vm:new(),
	Global = erlv8_vm:global(VM),
	Global:set_value("x",fun (#erlv8_fun_invocation{}=I,[]) -> 
								 Global = I:global(),
								 Global:set_value("a",2)
						 end),
	?assertMatch([{"x", _}], Global:proplist()),
	erlv8_vm:run(VM,"x()"),
	?assertMatch([{"x", _},{"a", 2}], Global:proplist()),
	stop().

fun_callback_test() ->
	start(),
	{ok, VM} = erlv8_vm:new(),
	Self = self(),
	erlv8_vm:register(VM, "test", fun () -> F = fun (#erlv8_fun_invocation{} = _Invocation, [Cb]) -> 
														spawn(fun () ->
																	  timer:sleep(1000), %% allow ample time
																	  Self ! {ok, Cb:call([1])}
															  end),
														undefined
												   end, F end),
	
	erlv8_vm:run(VM,"f = function(x) { return x}; test(f);"),
	receive
		{ok, 1} ->
			ok;
		Other1 ->
			error({bad_result,Other1})
	end,
	stop().

js_fun_test() ->
	start(),
	{ok, VM} = erlv8_vm:new(),
	Global = erlv8_vm:global(VM),
	erlv8_vm:run(VM,"f = function () { return 100; }"),
	F = Global:get_value("f"),
	?assertEqual(100,F:call()),
	erlv8_vm:run(VM,"f1 = function (x) { return x*100; }"),
	F1 = Global:get_value("f1"),
	?assertEqual(200,F1:call([2])),
	stop().

js_fun_this_test() ->
	start(),
	{ok, VM} = erlv8_vm:new(),
	Global = erlv8_vm:global(VM),
	erlv8_vm:run(VM,"f = function (a) { this.x = a*100; }; y = {}"),
	F = Global:get_value("f"),
	Y = Global:get_value("y"),
	F:call(Y,[1]),
	?assertEqual(100, Y:get_value("x")),
	Y:call(F,[2]), % test another API
	?assertEqual(200, Y:get_value("x")),
	stop().


to_string_test() ->
	start(),
	{ok, VM} = erlv8_vm:new(),
	?assertEqual("1",erlv8_vm:to_string(VM,1)),
	?assertEqual("1",erlv8_vm:to_string(VM,"1")),
	?assertEqual("true",erlv8_vm:to_string(VM,true)),
	?assertEqual("[object Object]",erlv8_vm:to_string(VM,?V8Obj([{a,1}]))),
	stop().

to_detail_string_test() ->
	start(),
	{ok, VM} = erlv8_vm:new(),
	?assertEqual("1",erlv8_vm:to_detail_string(VM,1)),
	?assertEqual("1",erlv8_vm:to_detail_string(VM,"1")),
	?assertEqual("true",erlv8_vm:to_detail_string(VM,true)),
	?assertEqual("#<an Object>",erlv8_vm:to_detail_string(VM,?V8Obj([{a,1}]))),
	stop().

proto_test() ->
	start(),
	{ok, VM} = erlv8_vm:new(),
	Global = erlv8_vm:global(VM),
	Global:set_value("proto",erlv8_object:new([{"x",1}])),
	Global:set_value("obj",erlv8_object:new([{"y",1}])),
	Proto = Global:get_value("proto"),
	Obj = Global:get_value("obj"),
	?assertEqual(true, Obj:set_prototype(Proto)),
	ObjProto = Obj:get_prototype(),
	?assertEqual(Proto:proplist(),ObjProto:proplist()),
	?assertEqual({ok, 1},erlv8_vm:run(VM,"obj.x")),
	stop().

hidden_value_test() ->
	start(),
	{ok, VM} = erlv8_vm:new(),
	Global = erlv8_vm:global(VM),
	Global:set_hidden_value("a",1),
	?assertEqual(1,Global:get_hidden_value("a")),
	?assertEqual({ok, undefined}, erlv8_vm:run(VM,"this.a")),
	?assertEqual(undefined, Global:get_hidden_value("shouldntbethere")),
	stop().

equality_test() ->
	start(),
	{ok, VM} = erlv8_vm:new(),
	Global = erlv8_vm:global(VM),
	Global:set_value("v1",?V8Obj([{"a",1}])),
	Global:set_value("v2",?V8Obj([{"a",1}])),
	V1 = Global:get_value("v1"),
	V2 = Global:get_value("v2"),
	?assert(V1:equals(V1)),
	?assert(not V1:strict_equals(V2)),
	erlv8_vm:run(VM,"f1 = function() { return 1; }; f2 = function() { return 2; };"),
	F1 = Global:get_value("f1"),
	F2 = Global:get_value("f2"),
	?assert(F1:equals(F1)),
	?assert(not F1:strict_equals(F2)),
	stop().


taint_test() ->
	start(),
	{ok, VM} = erlv8_vm:new(),
	?assertMatch({erlv8_object, _, _},erlv8_vm:taint(VM, ?V8Obj([{"a",1}]))),
	stop().

fun_extends_object_test() ->	
	start(),
	{ok, VM} = erlv8_vm:new(),
	{ok, F} = erlv8_vm:run(VM,"f = function() { return 1; }; f.x = 1; f"),
	?assertEqual(1, F:get_value("x")),
	stop().
	

-endif.
