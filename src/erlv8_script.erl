-module(erlv8_script).

-behaviour(gen_server).

%% API
-export([start_link/1,source/1,run/1,register/2,register/3,alias/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {
		  script,
		  mods = []
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
	{ok, #state{script = Script}}.

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
handle_call({register, Mod}, _From, #state{ script = Script, mods = Mods} = State) ->
	erlv8_nif:register(Script, Mod,Mod:exports()),
	{reply, ok, State#state{ mods = [{Mod,Mod}|Mods] }};

handle_call({register, Name, Mod}, _From, #state{ script = Script, mods = Mods} = State) ->
	erlv8_nif:register(Script, Name, Mod:exports()),
	{reply, ok, State#state{ mods = [{Name,Mod}|Mods] }};

handle_call({alias, Name, Mod}, _From, #state{  mods = Mods} = State) ->
	{reply, ok, State#state{ mods = [{Name,Mod}|Mods] }};

handle_call(source, _From, #state{ script = Script } = State) ->
	Reply = erlv8_nif:get_script(Script),
	{reply, Reply, State};

handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

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
handle_info({M, F, A}, State) ->
	spawn(fun () ->
				  erlang:apply(get_mod(M, State),F,A)
		  end),
	{noreply, State};
%% TODO: make these events
handle_info(compilation_failed, State) ->
	Self = self(),
	spawn(fun () ->
				  error_logger:error_msg("Compilation failed:~s~n",[source(Self)])
		  end),
	{noreply, State};
handle_info(starting, State) ->
	{noreply, State};
handle_info(finished, State) ->
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
terminate(_Reason, _State) ->
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
register(Server, Mod) ->
	gen_server:call(Server, {register, Mod}).

register(Server, Name, Mod) ->
	gen_server:call(Server, {register, Name, Mod}).

alias(Server, Name, Mod) ->
	gen_server:call(Server, {alias, Name, Mod}).

source(Server) ->
	gen_server:call(Server, source).

run(Server) ->
	gen_server:cast(Server, run).
