-module(docterl_ets_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case docterl_ets_sup:start_link() of
        {ok, Pid} -> do_add_handler(Pid);
        {error,{already_started, Pid}} ->  do_add_handler(Pid);
        {error, Reason} -> {error, Reason};
        Error -> {error, unkown_error, Error}
    end.

stop(_State) ->
    ok.

do_add_handler(Pid) ->
    case gen_event:add_handler(doe_event_mgr, doe_update_handler, []) of
        ok -> {ok, Pid};
        {'EXIT', Reason} -> {error, {'EXIT', Reason}};
        Error -> {error, unkown_error, Error}
    end.
