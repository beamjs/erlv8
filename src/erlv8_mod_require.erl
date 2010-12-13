-module(erlv8_mod_require).

-export([exports/0]).

exports() ->
	fun require/1.


require(What) ->
	{ok, Script} = erlv8:load_file(What),
	erlv8_script:run(Script),
	[].



