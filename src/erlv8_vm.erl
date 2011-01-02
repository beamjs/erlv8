-module(erlv8_vm).

-behaviour(gen_server2).
-include_lib("erlv8/include/erlv8.hrl").

%% API
-export([start_link/1,start/0,run/2,run/3,run/4,global/1,stop/1,
		 to_string/2,to_detail_string/2,taint/2,untaint/1,equals/3, strict_equals/3, next_tick/2, next_tick/3, next_tick/4,
		 stor/3, retr/2]).

%% gen_server2 callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3,prioritise_info/2]).

-define(SERVER, ?MODULE). 

-record(state, {
		  vm,
		  mods = [],
		  ticks,
		  ticked = [],
		  flush_tick = false,
		  storage = [],
		  context
		 }).

-define(Error(Msg), lists:flatten(io_lib:format("~s: ~p",[Msg,Trace]))).
-define(ErrorVal(Msg), lists:flatten(io_lib:format("~s: ~p ~p",[Msg,Val,Trace]))).


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
	erlv8_nif:set_server(VM, self()),
	Ctx = erlv8_nif:context(VM),
	{ok, #state{vm = VM, ticks = queue:new(), context = Ctx}}.

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
handle_call({stor, Key, Value}, _From, #state{ storage = Storage } = State) ->
	{reply, ok, State#state{ storage = [{Key, Value}|Storage] }};

handle_call({retr, Key}, _From, #state{ storage = Storage } = State) ->
	{reply, proplists:get_value(Key, Storage), State};

handle_call({taint, Value}, _From, #state{ vm = VM } = State) ->
	{reply, erlv8_nif:value_taint(VM,Value), State};

handle_call({equals, V1, V2}, _From, #state{ vm = VM } = State) ->
	{reply, erlv8_nif:value_equals(VM,V1,V2), State};

handle_call({strict_equals, V1, V2}, _From, #state{ vm = VM } = State) ->
	{reply, erlv8_nif:value_strict_equals(VM,V1,V2), State};

handle_call(context, _From, #state{} = State) ->
	{reply, {self(), State#state.context}, State};

handle_call(new_context, _From, #state{ vm = VM } = State) ->
	{reply, {self(), erlv8_nif:new_context(VM)}, State};

handle_call({global, Resource}, _From, #state{} = State) ->
	{reply, erlv8_nif:global(Resource), State};

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
	{noreply, State#state{ ticks = Ticks, ticked = update_ticked(Ref, From, Tick, Ticked), flush_tick = false }};

handle_call({next_tick, Tick, Ref}, From, #state{ ticks = Ticks } = State) ->
	{noreply, State#state{ ticks = queue:in({Ref,{From,Tick}}, Ticks) }};

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
			{noreply, State#state{ ticks = Ticks1, ticked = update_ticked(Ref, From, Tick, Ticked), flush_tick = false }}
	end;

%% Invocation
handle_info({F,#erlv8_fun_invocation{ is_construct_call = ICC, this = This, ref = Ref } = Invocation,Args}, #state{} = State) when is_function(F), is_list(Args) ->
	Self = self(),
	spawn(fun () ->
				  Result = (catch erlang:apply(F,[Invocation,Args])),
				  case Result of 
					  {'EXIT',{badarg, Trace}} ->
						  next_tick(Self, {result, Ref, {throw, {error, ?Error("Bad argument(s)")}}});
					  {'EXIT',{function_clause, Trace}} ->
						  next_tick(Self, {result, Ref, {throw, {error, ?Error("No matching function implementation")}}});
					  {'EXIT',{{badmatch, Val}, Trace}} ->
						  next_tick(Self, {result, Ref, {throw, ?ErrorVal("Bad match")}});
					  {'EXIT',{badarith,Trace}} ->
						  next_tick(Self, {result, Ref, {throw, {error, ?Error("Bad arithmetic operation")}}});
					  {'EXIT',{{case_clause, Val},Trace}} ->
						  next_tick(Self, {result, Ref, {throw, {error, ?ErrorVal("No case clause matched")}}});
					  {'EXIT',{if_clause, Trace}} ->
						  next_tick(Self, {result, Ref, {throw, {error, ?Error("Bad formed if expression, no true branch found")}}});
					  {'EXIT',{{try_clause, Val}, Trace}} ->
						  next_tick(Self, {result, Ref, {throw, {error, ?ErrorVal("No matching branch is found when evaluating the of-section of a try expression")}}});
					  {'EXIT',{undef, Trace}} ->
						  next_tick(Self, {result, Ref, {throw, {error, ?Error("The function cannot be found")}}});
					  {'EXIT',{{badfun, Val}, Trace}} ->
						  next_tick(Self, {result, Ref, {throw, {error, ?ErrorVal("Bad function")}}});
					  {'EXIT',{{badarity, Val}, Trace}} ->
						  next_tick(Self, {result, Ref, {throw, {error, ?ErrorVal("A fun is applied to the wrong number of arguments")}}});
					  {'EXIT',{timeout_value, Trace}} ->
						  next_tick(Self, {result, Ref, {throw, {error, ?Error("The timeout value in a receive..after expression is evaluated to something else than an integer or infinity")}}});
					  {'EXIT',{noproc, Trace}} ->
						  next_tick(Self, {result, Ref, {throw, {error, ?Error("Trying to link to a non-existing process")}}});
					  {'EXIT',{{nocatch, Val}, Trace}} ->
						  next_tick(Self, {result, Ref, {throw, {error, ?ErrorVal("Trying to evaluate a throw outside a catch")}}});
					  {'EXIT',{system_limit, Trace}} ->
						  next_tick(Self, {result, Ref, {throw, {error, ?Error("A system limit has been reached")}}});
					  {'EXIT',{Val, Trace}} ->
						  next_tick(Self, {result, Ref, {throw, {error, ?ErrorVal("Unknown error")}}});
					  _ ->
						  case ICC of 
							  true ->
								  next_tick(Self, {result, Ref, This});
							  false ->
								  next_tick(Self, {result, Ref, Result})
						  end
				  end
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
terminate(_Reason, #state{ vm = VM } = _State) ->
	tack = erlv8_nif:tick(VM,make_ref(),{stop}),
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
update_ticked(_Ref, From, {result, _, _}, Ticked) -> %% do not insert results, nobody is going to reply on them
	gen_server2:reply(From, ok),
	Ticked;
update_ticked(Ref, From, Tick, Ticked) ->
	[{Ref,{From,Tick}}|Ticked].

%%%===================================================================
%%% Public functions
%%%===================================================================
start() ->
	VM = erlv8_nif:new_vm(),
	supervisor2:start_child(erlv8_sup,[VM]).

run(Server, Source) ->
	run(Server, erlv8_context:get(Server), Source).

run(Server, {_, _CtxRes} = Context, Source) ->
	run(Server, Context, Source, {"unknown",0,0}).

run(Server, {_, CtxRes}, Source, {Name, LineOffset, ColumnOffset}) ->
	next_tick(Server, {script, CtxRes, Source, Name, LineOffset, ColumnOffset}).

global(Server) ->
	Ctx = erlv8_context:get(Server),
	erlv8_context:global(Ctx).

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

equals(Server, V1, V2) ->
	gen_server2:call(Server, {equals, V1, V2}).

strict_equals(Server, V1, V2) ->
	gen_server2:call(Server, {strict_equals, V1, V2}).


stor(Server, Key, Value) ->
	gen_server2:call(Server, {stor, Key, Value}).

retr(Server, Key) ->
	gen_server2:call(Server, {retr, Key}).


untaint({erlv8_object, _,_}=O) ->
	{erlv8_object,lists:map(fun ({Key, Val}) ->
									{Key, untaint(Val)}
							end,O:proplist()), undefined};
untaint({erlv8_array, _,_}=O) ->
	{erlv8_array,lists:map(fun untaint/1,O:list()), undefined};
untaint({erlv8_fun, _,_}=F) -> %% broken
	{erlv8_object,untaint(F:object()),undefined};
untaint([H|T]) ->
	[untaint(H)|untaint(T)];
untaint([]) ->
	[];
untaint(Other) ->
	Other.
