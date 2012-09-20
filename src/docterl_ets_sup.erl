
-module(docterl_ets_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 2000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    DocterlTable = ?CHILD(doe_ets, worker),
    DocterlEvent = ?CHILD(doe_event_mgr, worker),
    DocterlIDMgr = ?CHILD(doe_id_mgr, worker),
    RestartStrategy = {one_for_one, 5, 10},
    {ok, { RestartStrategy, [DocterlTable, DocterlEvent, DocterlIDMgr]} }.

