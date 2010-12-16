-module(erlv8_capturer).
-behaviour(gen_event).

-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).

-record(state, {f}).

init([Fun]) ->
    {ok, #state{f = Fun}}.

handle_event({compilation_failed, _Ref, _Error}=Evt, #state{ f = Fun } = State) ->
	erlang:apply(Fun,[Evt]),
    {ok, State};

handle_event({exception, _Ref, _Exception}=Evt, #state{ f = Fun } = State) ->
	erlang:apply(Fun,[Evt]),
    {ok, State};

handle_event({finished, _Ref, _Result}=Evt, #state{ f = Fun } = State) ->
	erlang:apply(Fun,[Evt]),
    {ok, State}.

handle_call(_Req,State) ->
	{ok, ok, State}.

handle_info(_Info,State) ->
	{ok, State}.

terminate(_Args, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
