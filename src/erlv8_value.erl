-module(erlv8_value).
-export([equals/3,strict_equals/3]).

equals(VM,V1,V2) ->
	erlv8_nif:value_equals(VM,V1,V2).

strict_equals(VM, V1,V2) ->
	erlv8_nif:value_strict_equals(VM, V1,V2).
