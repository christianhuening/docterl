%%% -------------------------------------------------------------------
%%% Author  : sage
%%% Description :
%%%
%%% Created : 10.08.2012
%%% -------------------------------------------------------------------
-module(doe_tree_event).

-behaviour(gen_event).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-define(SERVER, ?MODULE).

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0, add_handler/2, delete_handler/2, new_obj/2, update_area/3]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link() -> ok.

add_handler(Handler, Args) -> 
    gen_event:add_handler(?SERVER, Handler, Args).

delete_handler(Handler, Args) -> 
    gen_event:delete_handler(?SERVER, Handler, Args).


%
% this only sets the initial position. Changes in position
% will be handled by the area update.
%
new_obj(ObjId, AreaSpec) -> 
    gen_event:notify(?SERVER, {new_obj, ObjId, AreaSpec}).

%
% the old content is resent, so that the recipient need not query 
% an addition data source to get it.
%
update_area(AreaSpec, OldContent, NewContent) -> 
    gen_event:notify(?SERVER, {update_area, AreaSpec, OldContent, NewContent}).

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
handle_event({new_obj, ObjId, AreaSpec}, State) ->
    {ok, State};

handle_event({update_area, ObjId, AreaSpec}, State) ->
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

