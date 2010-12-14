-module(erlv8_fun,[Resource]).
-export([call/0,call/1]).

call() ->
	call([]).

call(Args) ->
	erlv8_nif:call(Resource,self(),Args),
	receive
		X ->
			X
	end.
	   
	
