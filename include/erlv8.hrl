-record(erlv8_fun_invocation, {
		  is_construct_call = false,
		  holder,
		  this,
		  ref,
		  vm
		 }).
		  
-define(V8Obj(X),erlv8_object:new(X)).
-define(V8Arr(X),erlv8_array:new(X)).
