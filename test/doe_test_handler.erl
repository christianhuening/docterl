%%% -------------------------------------------------------------------
%%% Author  : sage
%%% Description :
%%%
%%% Created : 13.08.2012
%%% -------------------------------------------------------------------
-module(doe_test_handler).

-behaviour(gen_event).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([get_last_event/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {last_event}).

%% ====================================================================
%% External functions
%% ====================================================================

get_last_event() -> gen_event:call(doe_event_mgr, doe_test_handler, {get_last_event}).

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
handle_event(Event, State) ->
%%      ?debugFmt("setting last_event to ~p~n", [Event]),
    {ok, State#state{last_event=Event}}.

%% --------------------------------------------------------------------
%% Func: handle_call/2
%% Returns: {ok, Reply, State}                                |
%%          {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%          {remove_handler, Reply}
%% --------------------------------------------------------------------
handle_call({get_last_event}, State) ->
    {ok, State#state.last_event, State}.

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

