-module(erlv8_script).

-behaviour(gen_server2).
-include_lib("erlv8/include/erlv8.hrl").

%% API
-export([start_link/1,new/0,run/2,register/2,register/3,global/1,global/2,add_handler/3,stop/1,
		 to_string/2,to_detail_string/2,next_tick/2, next_tick/3, next_tick/4]).

%% gen_server2 callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3,prioritise_info/2]).

-define(SERVER, ?MODULE). 

-record(state, {
		  script,
		  requests = [],
		  mods = [],
		  this = [], globals = [],
		  ticks,
		  ticked = [],
		  flush_tick = false,
		  event_mgr
		 }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(Script) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Script) ->
	gen_server2:start_link(?MODULE, [Script], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Script]) ->
	process_flag(trap_exit, true),
	{ok, EventPid} = gen_event:start_link(),
	erlv8_nif:set_server(Script, self()),
	{ok, #state{script = Script, ticks = queue:new(), event_mgr = EventPid}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({add_handler, Handler, Args}, _From, #state{ event_mgr = EventMgr } = State) ->
	Result = gen_event:add_handler(EventMgr,Handler,Args),
	{reply, Result, State};

handle_call(global, From, #state{requests = Requests } = State) ->
	Ref = make_ref(),
	Self = self(),
	spawn(fun () -> next_tick(Self,{global}, Ref) end),
	{noreply, State#state{ requests = [{Ref, From}|Requests] }};

handle_call({set_global, _Global}=Tick, From, #state{ requests = Requests } = State) ->
	Ref = make_ref(),
	Self = self(),
	spawn(fun () -> next_tick(Self,Tick, Ref) end),
	{noreply, State#state{ requests = [{Ref, From}|Requests] }};

handle_call({to_string, Val}, _From, #state { script = Script } = State) ->
	Reply = erlv8_nif:to_string(Script, Val),
	{reply, Reply, State};

handle_call({to_detail_string, Val}, _From, #state { script = Script } = State) ->
	Reply = erlv8_nif:to_detail_string(Script, Val),
	{reply, Reply, State};

handle_call(stop, _From, State) ->
	{stop, normal, ok, State};

handle_call({next_tick, Tick}, From, State) ->
	Ref = make_ref(),
	handle_call({next_tick, Tick, Ref}, From, State);

handle_call({next_tick, Tick, Ref}, From, #state{ script = Script, ticks = Ticks, ticked = Ticked, flush_tick = true } = State) ->
	tack = erlv8_nif:tick(Script, Ref, Tick),
	{noreply, State#state{ ticks = Ticks, ticked = [{Ref,{From,Tick}}|Ticked], flush_tick = false }};

handle_call({next_tick, Tick, Ref}, From, #state{ ticks = Ticks } = State) ->
	{noreply, State#state{ ticks = queue:in({Ref,{From,Tick}}, Ticks) }};

handle_call({script, _Source}=Tick, From, #state{ requests = Requests } = State) ->
	Ref = make_ref(),
	Self = self(),
	spawn(fun () -> next_tick(Self,Tick, Ref) end),
	{noreply, State#state{ requests = [{Ref, From}|Requests] }};

handle_call(_Request, _From, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(run, #state{ script = Script } = State) ->
	erlv8_nif:run(Script, self()),
	{noreply, State};

handle_cast({globals, Ref, delete}, #state{ globals = Globals } = State) ->
	{noreply, State#state{ globals = proplists:delete(Ref, Globals) }};

handle_cast({global, Ref, NewGlobal}, #state{ globals = Globals } = State) ->
	{noreply, State#state{ globals = [{Ref, NewGlobal}|Globals] }};

handle_cast({this, Ref, delete}, #state{ this = This } = State) ->
	{noreply, State#state{ this = proplists:delete(Ref, This) }};

handle_cast({this, Ref, NewThis}, #state{ this = This } = State) ->
	{noreply, State#state{ this = [{Ref, NewThis}|This] }};

handle_cast(_Msg, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({retick, Ref}, #state{ script = Script, ticked = Ticked, ticks = Ticks } = State) ->
	case proplists:get_value(Ref, Ticked) of
		undefined ->
			{noreply, State};
		{From, Tick} ->
			tack = erlv8_nif:tick(Script, Ref, Tick),
			{noreply, State#state { ticks = queue:in({Ref, {From, Tick}},Ticks) } }
	end;

handle_info(tick_me, #state{ script = Script, ticks = Ticks, ticked = Ticked } = State) ->
	case queue:out(Ticks) of
		{empty, Ticks1} ->
			{noreply, State#state{ ticks = Ticks1, flush_tick = true }};
		{{value, {Ref, {From,Tick}}}, Ticks1} ->
			tack = erlv8_nif:tick(Script, Ref, Tick),
			{noreply, State#state{ ticks = Ticks1, ticked = [{Ref,{From, Tick}}|Ticked], flush_tick = false }}
	end;

%% Invocation
handle_info({F,#erlv8_fun_invocation{ ref = Ref, this = IThis } = Invocation,Args}, #state{ this = This, globals = Globals } = State) when is_function(F), is_list(Args) ->
	Self = self(),
	spawn(fun () ->
				  Result = erlang:apply(F,[Invocation,Args]),
				  next_tick(Self, {result, Ref, Result, proplists:get_value(Ref, This, IThis)}),
				  global(Self, proplists:get_value(Ref, Globals, global(Self))),
				  gen_server2:cast({this, Ref, delete}),
				  gen_server2:cast({global, Ref, delete})
		  end),
	{noreply, State};
handle_info({result, Ref, Result}, #state{ ticked = Ticked } = State) ->
	case proplists:get_value(Ref, Ticked) of
		undefined ->
			{noreply, State};
		{From, _Tick} ->
			gen_server2:reply(From, Result),
			{noreply, State#state{ ticked = proplists:delete(Ref, Ticked) } }
	end;
handle_info({request_response, Ref, Response}, #state{ requests = Requests } = State) ->
	State1 =
	case proplists:get_value(Ref,Requests) of
		undefined ->
			error_logger:warning_msg("Ref ~p not found in Requests. This shouldn't normally happen~n",[Ref]),
			State;
		From ->
			gen_server2:reply(From, Response),
			State#state{ requests = proplists:delete(Ref, Requests) }
	end,
	{noreply, State1};
handle_info({compilation_failed, Ref, Error}=Evt, #state{ event_mgr = EventMgr, requests = Requests  } = State) ->
	State1 =
	case proplists:get_value(Ref,Requests) of
		undefined ->
			error_logger:warning_msg("Ref ~p not found in Requests. This shouldn't normally happen~n",[Ref]),
			State;
		From ->
			gen_server2:reply(From, {compilation_failed, Error}),
			State#state{ requests = proplists:delete(Ref, Requests) }
	end,
	gen_event:notify(EventMgr, Evt),
	{noreply, State1};
handle_info({starting, _Ref}, State) ->
	{noreply, State};
handle_info({exception, Ref, Exception}=Evt, #state{ event_mgr = EventMgr, requests = Requests } = State) ->
	State1 =
	case proplists:get_value(Ref,Requests) of
		undefined ->
			error_logger:warning_msg("Ref ~p not found in Requests. This shouldn't normally happen~n",[Ref]),
			State;
		From ->
			gen_server2:reply(From, {exception, Exception}),
			State#state{ requests = proplists:delete(Ref, Requests) }
	end,
	gen_event:notify(EventMgr,Evt),
	{noreply, State1};
handle_info({finished, Ref, Result}=Evt, #state{ event_mgr = EventMgr, requests = Requests } = State) ->
	State1 =
	case proplists:get_value(Ref,Requests) of
		undefined ->
			error_logger:warning_msg("Ref ~p not found in Requests. This shouldn't normally happen~n",[Ref]),
			State;
		From ->
			gen_server2:reply(From, {ok, Result}),
			State#state{ requests = proplists:delete(Ref, Requests) }
	end,
	gen_event:notify(EventMgr,Evt),
	{noreply, State1};

handle_info(_Info, State) ->
	{noreply, State}.

prioritise_info({retick, _}, _State) ->
	1;
prioritise_info(tick_me,_State) ->
	0;
prioritise_info(_,_State) ->
	0.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{ script = Script, event_mgr = EventMgr } = _State) ->
	tack = erlv8_nif:tick(Script,make_ref(),{stop}),
	gen_event:stop(EventMgr),
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%===================================================================
%%% Public functions
%%%===================================================================
new() ->
	Script = erlv8_nif:new_script(),
	supervisor2:start_child(erlv8_sup,[Script]).

add_handler(Server, Handler, Args) ->
	gen_server2:call(Server, {add_handler, Handler, Args}).

register(Server, Mod) when is_atom(Mod) ->
	register(Server, Mod, Mod).

register(Server, Name, Mod) when is_atom(Mod) ->
	register(Server, Name, fun () -> Mod:exports() end);

register(Server, Name, Mod) when is_function(Mod) ->
	G0 = global(Server),
	global(Server, [{Name, Mod()}|proplists:delete(Name, G0)]).

run(Server, Source) ->
	gen_server2:call(Server, {script, Source}, infinity).

global(Server) ->
	gen_server2:call(Server, global, infinity).

global(Server, Global) when is_list(Global) ->
	gen_server2:call(Server, {set_global, Global},infinity).

stop(Server) ->
	gen_server2:call(Server,stop).

to_string(Server, Val) ->
	gen_server2:call(Server,{to_string, Val}).

to_detail_string(Server, Val) ->
	gen_server2:call(Server,{to_detail_string, Val}).

next_tick(Server, Tick) ->
	gen_server2:call(Server,{next_tick, Tick}, infinity).

next_tick(Server, Tick, Ref) when is_reference(Ref) ->
	gen_server2:call(Server,{next_tick, Tick, Ref}, infinity);

next_tick(Server, Tick, Timeout) ->
	gen_server2:call(Server,{next_tick, Tick}, Timeout).

next_tick(Server, Tick, Timeout, Ref) when is_reference(Ref) ->
	gen_server2:call(Server,{next_tick, Tick, Ref}, Timeout).


