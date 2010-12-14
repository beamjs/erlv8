-module(erlv8_script).

-behaviour(gen_server).

%% API
-export([start_link/1,source/1,run/1,register/2,register/3,global/1,global/2,add_handler/3,stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {
		  script,
		  mods = [],
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
	gen_server:start_link(?MODULE, [Script], []).

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
	{ok, EventPid} = gen_event:start_link(),
	{ok, #state{script = Script, event_mgr = EventPid}}.

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

handle_call({register, Mod}, _From, #state{ script = Script, mods = Mods} = State) ->
	erlv8_nif:register(Script, Mod,Mod:exports()),
	{reply, ok, State#state{ mods = [{Mod,Mod}|Mods] }};

handle_call({register, Name, Mod}, _From, #state{ script = Script, mods = Mods} = State) ->
	erlv8_nif:register(Script, Name, Mod:exports()),
	{reply, ok, State#state{ mods = [{Name,Mod}|Mods] }};

handle_call(source, _From, #state{ script = Script } = State) ->
	Reply = erlv8_nif:get_script(Script),
	{reply, Reply, State};

handle_call(global, _From, #state{ script = Script } = State) ->
	Reply = erlv8_nif:get_global(Script),
	{reply, Reply, State};

handle_call({global, Global}, _From, #state{ script = Script } = State) ->
	Reply = erlv8_nif:set_global(Script, Global),
	{reply, Reply, State};

handle_call(stop, _From, State) ->
	{stop, normal, ok, State};

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
handle_info({F,A}, #state{ script = Script } = State) when is_function(F), is_list(A) ->
	Result = erlang:apply(F,A),
	erlv8_nif:result(Script, Result),
	{noreply, State};
handle_info({M, F, A}, State) ->
	spawn(fun () ->
				  erlang:apply(get_mod(M, State),F,A)
		  end),
	{noreply, State};
%% TODO: make these events
handle_info({compilation_failed, _Error}=Evt, #state{ event_mgr = EventMgr } = State) ->
	gen_event:notify(EventMgr, Evt),
	{noreply, State};
handle_info(starting, State) ->
	{noreply, State};
handle_info({exception, _Exception}=Evt, #state{ event_mgr = EventMgr } = State) ->
	gen_event:notify(EventMgr,Evt),
	{noreply, State};
handle_info({finished, _Result}=Evt, #state{ event_mgr = EventMgr } = State) ->
	gen_event:notify(EventMgr,Evt),
	{noreply, State};
handle_info(_Info, State) ->
	{noreply, State}.

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
terminate(_Reason, #state{ event_mgr = EventMgr } = _State) ->
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
get_mod(M,#state{ mods = Mods }) ->
	proplists:get_value(M,Mods,M).

%%%===================================================================
%%% Public functions
%%%===================================================================
add_handler(Server, Handler, Args) ->
	gen_server:call(Server, {add_handler, Handler, Args}).

register(Server, Mod) ->
	gen_server:call(Server, {register, Mod}).

register(Server, Name, Mod) ->
	gen_server:call(Server, {register, Name, Mod}).

source(Server) ->
	gen_server:call(Server, source).

run(Server) ->
	gen_server:cast(Server, run).

global(Server) ->
	gen_server:call(Server, global).

global(Server, Global) when is_list(Global) ->
	gen_server:call(Server, {global, Global}).

stop(Server) ->
	gen_server:call(Server,stop).
