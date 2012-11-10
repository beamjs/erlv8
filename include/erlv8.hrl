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
-record(erlv8_array, {resource, %% or array()
                      vm}). %% or proplist()

		  
-define(is_v8(X), (is_record(X, erlv8_object) orelse is_record(X, erlv8_fun) orelse is_record(X, erlv8_array))).
