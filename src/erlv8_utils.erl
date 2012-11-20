-module(erlv8_utils).

-export([clone/1,
         copy_properties/2]).

-include("erlv8.hrl").

clone(Source) ->
    erlv8_vm:taint(Source:vm(), ?V8Obj(Source:proplist())).

copy_properties(Destination, Source) ->
    lists:foreach(fun({K, V}) ->
                          Destination:set_value(K, V)
                  end, Source:proplist()).
