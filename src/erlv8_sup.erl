-module(erlv8_sup).

-behaviour(supervisor2).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Restart, Type), {I, {I, start_link, []}, Restart, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor2:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {simple_one_for_one_terminate, 5, 10}, [?CHILD(erlv8_vm,transient,worker)]} }.

