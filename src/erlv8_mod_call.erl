-module(erlv8_mod_call).

-export([exports/0]).

exports() ->
	fun send_call/4.


send_call(Script,M,F,A) when is_list(M), is_list(F) ->
	send_call(Script,list_to_atom(M),list_to_atom(F),A);
send_call(Script,M,F,A) ->
	erlv8_nif:script_send(Script, {M,F,A}),
	[].



