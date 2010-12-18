-module(erlv8_vm).

-behaviour(gen_server2).
-include_lib("erlv8/include/erlv8.hrl").

%% API
-export([start_link/1,new/0,run/2,register/2,register/3,global/1,add_handler/3,stop/1,
		 to_string/2,to_detail_string/2,taint/2,next_tick/2, next_tick/3, next_tick/4]).

%% gen_server2 callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3,prioritise_info/2]).

-define(SERVER, ?MODULE). 

-record(state, {
		  vm,
		  requests = [],
		  mods = [],
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
%% @spec start_link(VM) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(VM) ->
	gen_server2:start_link(?MODULE, [VM], []).

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
init([VM]) ->
	process_flag(trap_exit, true),
	{ok, EventPid} = gen_event:start_link(),
	erlv8_nif:set_server(VM, self()),
	{ok, #state{vm = VM, ticks = queue:new(), event_mgr = EventPid}}.

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


handle_call({taint, Value}, _From, #state{ vm = VM } = State) ->
	{reply, erlv8_nif:value_taint(VM,Value), State};

handle_call(global, _From, #state{ vm = VM } = State) ->
	{reply, erlv8_nif:global(VM), State};

handle_call({to_string, Val}, _From, #state { vm = VM } = State) ->
	Reply = erlv8_nif:to_string(VM, Val),
	{reply, Reply, State};

handle_call({to_detail_string, Val}, _From, #state { vm = VM } = State) ->
	Reply = erlv8_nif:to_detail_string(VM, Val),
	{reply, Reply, State};

handle_call(stop, _From, State) ->
	{stop, normal, ok, State};

handle_call({next_tick, Tick}, From, State) ->
	Ref = make_ref(),
	handle_call({next_tick, Tick, Ref}, From, State);

handle_call({next_tick, Tick, Ref}, From, #state{ vm = VM, ticks = Ticks, ticked = Ticked, flush_tick = true } = State) ->
	tack = erlv8_nif:tick(VM, Ref, Tick),
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
handle_cast(run, #state{ vm = VM } = State) ->
	erlv8_nif:run(VM, self()),
	{noreply, State};

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
handle_info({retick, Ref}, #state{ ticked = Ticked, ticks = Ticks } = State) ->
	case proplists:get_value(Ref, Ticked) of
		undefined ->
			{noreply, State};
		{From, Tick} ->
			{noreply, State#state { ticks = queue:in({Ref, {From, Tick}},Ticks) } }
	end;

handle_info(tick_me, #state{ vm = VM, ticks = Ticks, ticked = Ticked } = State) ->
	case queue:out(Ticks) of
		{empty, Ticks1} ->
			{noreply, State#state{ ticks = Ticks1, flush_tick = true }};
		{{value, {Ref, {From,Tick}}}, Ticks1} ->
			tack = erlv8_nif:tick(VM, Ref, Tick),
			{noreply, State#state{ ticks = Ticks1, ticked = [{Ref,{From, Tick}}|Ticked], flush_tick = false }}
	end;

%% Invocation
handle_info({F,#erlv8_fun_invocation{ ref = Ref } = Invocation,Args}, #state{} = State) when is_function(F), is_list(Args) ->
	Self = self(),
	spawn(fun () ->
				  Result = erlang:apply(F,[Invocation,Args]),
				  next_tick(Self, {result, Ref, Result})
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
terminate(_Reason, #state{ vm = VM, event_mgr = EventMgr } = _State) ->
	tack = erlv8_nif:tick(VM,make_ref(),{stop}),
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
	VM = erlv8_nif:new_vm(),
	supervisor2:start_child(erlv8_sup,[VM]).

add_handler(Server, Handler, Args) ->
	gen_server2:call(Server, {add_handler, Handler, Args}).

register(Server, Mod) when is_atom(Mod) ->
	register(Server, Mod, Mod).

register(Server, Name, Mod) when is_atom(Mod) ->
	register(Server, Name, fun () -> Mod:exports(Server) end),
	Mod:init(Server);

register(Server, Name, Mod) when is_function(Mod) ->
	Global = global(Server),
	Global:set_value(Name, Mod()).

run(Server, Source) ->
	gen_server2:call(Server, {script, Source}, infinity).

global(Server) ->
	gen_server2:call(Server, global, infinity).

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

taint(Server, Value) ->
	gen_server2:call(Server, {taint, Value}).
