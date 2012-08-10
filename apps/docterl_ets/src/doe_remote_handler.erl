%%% -------------------------------------------------------------------
%%% Author  : sage
%%% Description :
%%%
%%% Created : 10.08.2012
%%% -------------------------------------------------------------------
-module(doe_remote_handler).

-behaviour(gen_event).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([add_handler/1]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%% ====================================================================
%% External functions
%% ====================================================================
add_handler(NodeList) ->
    lists:map(fun(Node) -> 
                      doe_tree_event:add_handler({Node, ?MODULE}, []) 
              end,
              NodeList).


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
handle_event({new_obj, _ObjId, _AreaSpec}, State) ->
    {ok, State};

handle_event({update_area, _ObjId, _AreaSpec}, State) ->
    {ok, State}.

%% --------------------------------------------------------------------
%% Func: handle_call/2
%% Returns: {ok, Reply, State}                                |
%%          {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%          {remove_handler, Reply}
%% --------------------------------------------------------------------
handle_call(Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%% --------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler
%% --------------------------------------------------------------------
handle_info(Info, State) ->
    {ok, State}.

%% --------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any
%% --------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

