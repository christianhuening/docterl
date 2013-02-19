%%% -------------------------------------------------------------------
%%% Author  : sage
%%% Description : recieve events of updates on other nodes and write
%%%               updates into local data structure
%%%
%%% Created : 13.08.2012
%%% -------------------------------------------------------------------
-module(doe_update_handler).

-behaviour(gen_event).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%% ====================================================================
%% External functions
%% ====================================================================


%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          Other
%% --------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%% --------------------------------------------------------------------
%% Func: handle_event/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler
%% --------------------------------------------------------------------
handle_event({new_tree, TreeID, Options}, State) ->
     doe_ets:remote_new_tree(TreeID, Options),
     {ok, State};

handle_event({remote_add_obj, ObjId, AreaSpec, Extra}, State) ->
    doe_ets:remote_add_obj(ObjId, AreaSpec, Extra),
    ?debugFmt("handle_event(remote_add_obj, ~p, ~p, ~p) was called.~n", [ObjId, AreaSpec, Extra]),
    {ok, State};

handle_event({remove_obj, ObjId, AreaSpec}, State) ->
    doe_ets:remove_obj(ObjId, AreaSpec),
    {ok, State};

handle_event({enter_area, ObjId, AreaSpec}, State) ->
    doe_ets:enter_area(ObjId, AreaSpec),
    {ok, State};

handle_event({leave_area, ObjId, AreaSpec}, State) ->
    doe_ets:leave_area(ObjId, AreaSpec),
    {ok, State};

% as the information is tracked in the tables, this is a NoOp for this handler.
% the event will still be of importance of actual users of this information
handle_event({update_position, _ObjId, _AreaSpec, _NewPos, _NewBBSize}, State) ->
    {ok, State};

%
% these are NoOps as the operation has already been performed. The events
% exists mainly for use by applications using the docterl lib.
%
handle_event({local_remove_obj, _ObjId, _AreaSpec}, State) -> {ok, State};
handle_event({local_new_tree, _TreeId, _Options}, State) -> {ok, State};
handle_event({local_add_obj, _ObjId, _AreaSpec, _Extra}, State) ->{ok, State};
handle_event({local_leave_area, _ObjId, _AreaSpec}, State) -> {ok, State};
handle_event({local_enter_area, _ObjId, _AreaSpec}, State) -> {ok, State};
handle_event({local_update_position, _ObjId, _AreaSpec, _NewPos, _NewBBSize}, State) -> {ok, State};

handle_event(Event, State) ->
    ?debugFmt("unknown event ~p called~n", [Event]),
    {ok, State}.

%% --------------------------------------------------------------------
%% Func: handle_call/2
%% Returns: {ok, Reply, State}                                |
%%          {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%          {remove_handler, Reply}
%% --------------------------------------------------------------------
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%% --------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%% --------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

