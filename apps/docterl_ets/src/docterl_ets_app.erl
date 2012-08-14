-module(docterl_ets_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Pid = docterl_ets_sup:start_link(),
    % add the local listener for change events
    gen_event:add_handler(doe_event_mgr, doe_update_handler, []),
    Pid.

stop(_State) ->
    ok.
