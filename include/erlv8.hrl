-record(erlv8_fun_invocation, {
		  is_construct_call = false,
		  holder,
		  this,
		  ref,
		  server
		 }).
		  
-define(V8Obj(X),erlv8_object:new(X)).
