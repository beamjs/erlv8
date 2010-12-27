-record(erlv8_fun_invocation, {
		  is_construct_call = false,
		  holder,
		  this,
		  ref,
		  vm,
		  ctx
		 }).
		  
-define(V8Obj(X),erlv8_object:new(X)).
-define(V8Arr(X),erlv8_array:new(X)).

-record(erlv8_object, { resource, vm }).
-record(erlv8_fun, { resource, vm }).
-record(erlv8_array, { resource, vm }).

%% tick constants

-define(StopTick, 0).
-define(ResultTick, 1).
-define(CallTick, 2).
-define(InstantiateTick, 3).
-define(GetTick, 4).
-define(SetTick, 5).
-define(ProplistTick, 6).
-define(ListTick, 7).
-define(ScriptTick, 8).
