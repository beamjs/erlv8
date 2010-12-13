-module(erlv8_mod_require).

-export([exports/0]).

exports() ->
	fun require/1.


require(What) ->
	{ok, B} = file:read_file(What),
	L = binary_to_list(B),
	{ok, Script} = erlv8:new_script(L),
	erlv8_script:run(Script),
	[].



