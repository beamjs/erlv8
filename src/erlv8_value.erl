-module(erlv8_value).
-export([equals/2,strict_equals/2]).

equals(V1,V2) ->
	erlv8_nif:value_equals(V1,V2).

strict_equals(V1,V2) ->
	erlv8_nif:value_strict_equals(V1,V2).
