-module(erlv8_vm).

-behaviour(gen_server2).
-include_lib("erlv8/include/erlv8.hrl").

%% API
-export([start_link/1,start/0,vm_resource/1,run/2,run/3,run/4,global/1,stop/1,
		 to_string/2,to_detail_string/2,taint/2,untaint/1,equals/3, strict_equals/3, 
		 enqueue_tick/2, enqueue_tick/3, enqueue_tick/4, next_tick/2, next_tick/3, next_tick/4,
		 stor/3, retr/2, gc/1]).

%% gen_server2 callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3,prioritise_info/2]).

-define(SERVER, ?MODULE). 

-record(state, {
		  vm,
		  ticked,
		  storage = [],
		  context,
		  debug
		 }).

-define(Error(Msg), lists:flatten(io_lib:format("~s: ~p",[Msg,Trace]))).
-define(ErrorVal(Msg), lists:flatten(io_lib:format("~s: ~p ~p",[Msg,Val,Trace]))).


%%%===================================================================
%%% API
%%%===================================================================
start() ->
	VM = erlv8_nif:new_vm(),
	supervisor2:start_child(erlv8_sup,[VM]).

vm_resource(Server) ->
	gen_server2:call(Server, vm_resource).

run(Server, Source) ->
	run(Server, erlv8_context:get(Server), Source).

run(Server, {_, _CtxRes} = Context, Source) ->
	run(Server, Context, Source, {"unknown",0,0}).

run(Server, {_, CtxRes}, Source, {Name, LineOffset, ColumnOffset}) ->
	enqueue_tick(Server, {script, CtxRes, Source, Name, LineOffset, ColumnOffset}).

global(Server) ->
	Ctx = erlv8_context:get(Server),
	erlv8_context:global(Ctx).

stop(Server) ->
	gen_server2:call(Server,stop).

to_string(Server, Val) ->
	enqueue_tick(Server, {to_string, Val}).

to_detail_string(Server, Val) ->
	enqueue_tick(Server, {to_detail_string, Val}).

enqueue_tick(Server, Tick) ->
	gen_server2:call(Server,{enqueue_tick, Tick}, infinity).

enqueue_tick(Server, Tick, Ref) when is_reference(Ref) ->
	gen_server2:call(Server,{enqueue_tick, Tick, Ref}, infinity);

enqueue_tick(Server, Tick, Timeout) ->
	gen_server2:call(Server,{enqueue_tick, Tick}, Timeout).

enqueue_tick(Server, Tick, Timeout, Ref) when is_reference(Ref) ->
	gen_server2:call(Server,{enqueue_tick, Tick, Ref}, Timeout).

next_tick(Server, Tick) ->
	gen_server2:call(Server,{next_tick, Tick}, infinity).

next_tick(Server, Tick, Ref) when is_reference(Ref) ->
	gen_server2:call(Server,{next_tick, Tick, Ref}, infinity);

next_tick(Server, Tick, Timeout) ->
	gen_server2:call(Server,{next_tick, Tick}, Timeout).

next_tick(Server, Tick, Timeout, Ref) when is_reference(Ref) ->
	gen_server2:call(Server,{next_tick, Tick, Ref}, Timeout).

taint(Server, Value) ->
    enqueue_tick(Server, {taint, Value}).

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

gc(Server) ->
	(catch enqueue_tick(Server, {gc}, 0)),
	ok.

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
	{ok, #state{vm = VM, context = Ctx, debug = ets:new(erlv8_vm_debug,[]), ticked = ets:new(erlv8_vm_ticked,[public]) }}.

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
handle_call(vm_resource, _From, #state{ vm = VM } = State) ->
	{reply, VM, State};

handle_call({stor, Key, Value}, _From, #state{ storage = Storage } = State) ->
	{reply, ok, State#state{ storage = [{Key, Value}|Storage] }};

handle_call({retr, Key}, _From, #state{ storage = Storage } = State) ->
	{reply, proplists:get_value(Key, Storage), State};

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

handle_call({enqueue_tick, Tick}, From, State) ->
	Ref = make_ref(),
	handle_call({enqueue_tick, Tick, Ref}, From, State);

handle_call({enqueue_tick, Tick, Ref}, From, #state{ vm = VM, ticked = Ticked } = State) ->
	tack = erlv8_nif:tick(VM, Ref, Tick),
	update_ticked(Ref, From, Tick, Ticked),
	{noreply, State};

handle_call({next_tick, Tick}, From, State) ->
	Ref = make_ref(),
	handle_call({next_tick, Tick, Ref}, From, State);

handle_call({next_tick, Tick, Ref}, From, #state{ vm = VM, ticked = Ticked } = State) ->
	tack = erlv8_nif:tick(VM, Ref, Tick),
	update_ticked(Ref, From, Tick, Ticked),
	{noreply, State};

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
%% Invocation
handle_info({F,#erlv8_fun_invocation{ is_construct_call = ICC, this = This, ref = Ref } = Invocation,Args}, #state{ ticked = Ticked } = State) when is_function(F), is_list(Args) ->
	Self = self(),
	spawn(fun () ->
				  Result = (catch erlang:apply(F,[Invocation,Args])),
				  Result1 = 
				  case Result of 
					  {'EXIT',{Val, Trace}} when is_atom(Val) ->
						  {throw, {error, ?Error(Val)}};
					  {'EXIT',{{Tag, Val}, Trace}} ->
						  {throw, {error, ?ErrorVal(Tag)}};
					  _ ->
						  case ICC of 
							  true ->
								  This;
							  false ->
								  Result
						  end
				  end,
				  case ets:lookup(Ticked, Ref) of
					  [{Ref, {From, {call, _, _, _}}}] ->
						  gen_server2:reply(From, Result1),
						  ets:delete(Ticked, Ref);
					  [{Ref, {From, {call, _, _}}}] ->
						  gen_server2:reply(From, Result1),
						  ets:delete(Ticked, Ref);
					  [{Ref, {From, {inst, _, _}}}] ->
						  gen_server2:reply(From, Result1),
						  ets:delete(Ticked, Ref);
					  _ ->
						  enqueue_tick(Self, {result, Ref, Result1})
				  end
		  end),
	{noreply, State};
handle_info({result, Ref, Result}, #state{ ticked = Ticked } = State) ->
	case ets:lookup(Ticked, Ref) of
		[] ->
			{noreply, State};
		[{Ref, {From, _Tick}}] ->
			gen_server2:reply(From, Result),
			ets:delete(Ticked, Ref),
			{noreply, State}
	end;

handle_info({'DEBUG',Name,Payload}, #state{ debug = Debug } = State) ->
	ets:insert(Debug, {Name, Payload}),
	{noreply, State};

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
	ets:insert(Ticked, {Ref, {From, Tick}}).


