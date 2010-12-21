-module(erlv8_context).
-export([get/1,global/1,new/1]).

get(Server) ->
	gen_server2:call(Server,context).

new(Server) ->
	gen_server2:call(Server,new_context).

global({Server, Resource}) ->
	gen_server2:call(Server,{global, Resource}).


