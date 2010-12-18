-module(erlv8_module).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{init,1},
     {exports, 0}];

behaviour_info(_Other) ->
    undefined.
