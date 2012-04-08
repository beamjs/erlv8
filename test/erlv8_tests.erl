-module(erlv8_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-behaviour(proper_statem).

-export([initial_state/0, command/1, precondition/2, postcondition/3,
         next_state/3]).

-compile(export_all).

-record(state, {
          vms = [],
          passed = []
         }).

%% helpers
start_vm() ->
    {ok, VM} = erlv8_vm:start(),
    VM.

exit_vm(VM) ->
    erlv8_vm:stop(VM),
    VM.

run_tests(VM) ->
    ets:insert(?MODULE, {vm, VM}),
    proper:module(?MODULE,
                  [{'on_output',
                    fun(Format, Data) ->
                            io:format(standard_error, Format, Data)
                    end},
                   {numtests, 100}]).


%% statem

initial_state() ->
    #state{}.

command(#state{ vms = VMs, passed = Passed }) ->
    frequency([
               {1 - (length(VMs) + length(Passed)), {call, ?MODULE, start_vm, []}},
               {length(Passed), {call, ?MODULE, exit_vm, [oneof(Passed)]}},
               {length(VMs), {call, ?MODULE, run_tests, [oneof(VMs)]}}
               ]).

precondition(_State, _Call) ->
    true.

postcondition(_S, {call, ?MODULE, run_tests, [_VM]}, []) ->
    true;

postcondition(_S, {call, ?MODULE, run_tests, [_VM]}, _) ->
    false;

postcondition(_S, _C, _R) ->
    true.

next_state(#state{ vms = VMs } = State, V, {call, ?MODULE, start_vm, []}) ->
    State#state{ vms = [V|VMs] };

next_state(#state{ passed = VMs } = State, _V, {call, ?MODULE, exit_vm, [VM]}) ->
    State#state{ passed = VMs -- [VM] };

next_state(#state{ vms = VMs, passed = Passed } = State, _V, {call, ?MODULE, run_tests, [VM]}) ->
    State#state{ vms = VMs -- [VM], passed = [VM|Passed] };

next_state(State, _V, _C) ->
    State.


statem() ->
    ?FORALL(Cmds, commands(?MODULE),
            ?TRAPEXIT(
               begin
                   {History,State,Result} = run_commands(?MODULE, Cmds),
                   [ erlv8_vm:stop(VM) || VM <- State#state.vms ],
                   ?WHENFAIL(io:format("History: ~p\nState: ~p\nResult: ~p\n",
                                       [History, State, Result]),
                             aggregate(command_names(Cmds), Result =:= ok))
               end)).

%%% generators
js_string() ->
    oneof([list(proper_stdgen:utf8_char()),
           proper_stdgen:utf8_bin()]).

%%% properties

%% type conversion
prop_to_js_string() ->
    ?FORALL(String, js_string(),
                  begin
                      [{vm, VM}] = ets:lookup(?MODULE, vm),
		      Bin = unicode:characters_to_binary(String),
                      Obj = erlv8_vm:taint(VM, Bin),
		      case Obj =:= Bin of
			  true ->
			      ok;
			  false ->
			      io:format("->~p/~p~n", [Bin, Obj]),
			      ok
		      end,
                      Obj =:= Bin
                  end).
                

%% eunit
t_properties() ->
    ?assertEqual(true, proper:quickcheck(statem(),
                                   [{'on_output',
                                     fun(Format, Data) ->
                                             io:format(standard_error, Format, Data)
                                     end},
                                    {numtests, 10}])).


erlv8_test_() ->
    [{setup,
      fun() ->
              ets:new(?MODULE,[named_table, public]),
              ok = application:start(erlv8)
      end,
      fun(_) ->
              ets:delete(?MODULE),
              application:stop(erlv8)
      end,
      [
       {timeout, 30, {"PropEr tests", ?_test(t_properties())}}
      ]}].
