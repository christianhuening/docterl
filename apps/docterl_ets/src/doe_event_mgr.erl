%%% -------------------------------------------------------------------
%%% Author  : sage
%%% Description : handle the subscription and notification of remote
%%%               nodes and local handlers. All handlers are registered
%%%               locally, the events are sent to all connected nodes.
%%%
%%%               currently the death of nodes is neither detected
%%%               nor handled adequately.
%%%
%%% Created : 10.08.2012
%%% -------------------------------------------------------------------
-module(doe_event_mgr).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

-define(SERVER, ?MODULE).

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0, stop/0, add_handler/2, delete_handler/2, 
         new_tree/2, subscribe/1, unsubscribe/1, 
         add_obj/2, update_area/3, update_position/4]).

%% ====================================================================
%% External functions
%% ====================================================================
start_link() -> 
    Ret = gen_event:start_link({local, ?SERVER}),
    ?debugFmt("gen_event:start_link returned: ~p~n", [Ret]),
		Ret.

stop() -> gen_event:stop(?SERVER).

%
% register a local handler for events.
%
add_handler(Handler, Args) -> 
    gen_event:add_handler(?SERVER, Handler, Args).

%
% remove a local handler for events.
%
delete_handler(Handler, Args) -> 
    gen_event:delete_handler(?SERVER, Handler, Args).


subscribe(AreaSpec) ->
    gen_server:abcast(erlang:nodes(), doe_ets, 
                      {subscribe, AreaSpec, erlang:node()}).

unsubscribe(AreaSpec) ->
    gen_server:abcast(erlang:nodes(), doe_ets, 
                      {unsubscribe, AreaSpec, erlang:node()}).


new_tree(TreeId, Options) -> 
    % notify the local event handler first
    gen_event:notify(?SERVER, {local_new_tree, TreeId, Options}),
    % then the others, as this may take some time.
    gen_server:abcast(erlang:nodes(), doe_ets, {new_tree, TreeId, Options}).

%
% this only sets the initial position. Changes in position
% will be handled by the area update.
%
add_obj(ObjId, AreaSpec) ->
    ?debugFmt("notifying of add_obj for ~p in ~p~n", [ObjId, AreaSpec]),
    % notify the local event handler first
    gen_event:notify(?SERVER, {local_new_obj, ObjId, AreaSpec}),
    Subscribers = gen_server:call(doe_ets, {get_subscribers, AreaSpec}),
    lists:map(fun(Sub) -> 
                      gen_event:notify({Sub, ?SERVER}, 
                                       {new_obj, ObjId, AreaSpec}) 
              end, 
              Subscribers).

update_area(ObjId, OldAreaSpec, NewAreaSpec) -> 
    % notify the local event handler first
    gen_event:notify(?SERVER, {local_leave_area, ObjId, OldAreaSpec}),
    gen_event:notify(?SERVER, {local_enter_area, ObjId, NewAreaSpec}),
    OldSubscribers = gen_server:call(doe_ets, {get_subscribers, OldAreaSpec}),
    NewSubscribers = gen_server:call(doe_ets, {get_subscribers, NewAreaSpec}),
    lists:map(fun(Sub) -> 
                      gen_event:notify({Sub, ?SERVER}, 
                                       {leave_area, ObjId, OldAreaSpec}) 
              end, 
              OldSubscribers),
        lists:map(fun(Sub) -> 
                      gen_event:notify({Sub, ?SERVER}, 
                                       {enter_area, ObjId, NewAreaSpec}) 
              end, 
              NewSubscribers).


update_position(ObjId, AreaSpec, NewPos, NewBBSize) -> 
    ?debugHere,
    % notify the local event handler first
    gen_event:notify(?SERVER, {local_update_position, ObjId, AreaSpec, NewPos, NewBBSize}),
    Subscribers = gen_server:call(doe_ets, {get_subscribers, AreaSpec}),
    lists:map(fun(Sub) -> 
                      gen_event:notify({Sub, ?SERVER}, 
                                       {update_position, ObjId, AreaSpec, NewPos, NewBBSize}) 
              end, 
              Subscribers).


%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

