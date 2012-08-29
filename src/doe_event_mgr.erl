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
         new_tree/2, subscribe/1, unsubscribe/1, subscribe/2, unsubscribe/2, 
         add_obj/2, add_obj/3, update_area/3, update_position/4, remove_obj/2]).

-ifdef(TEST).
%% export the private functions for testing only.
-export([notify_subs/2]).
-endif.

%% ====================================================================
%% External functions
%% ====================================================================
start_link() -> 
    Ret = gen_event:start_link({local, ?SERVER}),
    % ?debugFmt("gen_event:start_link returned: ~p~n", [Ret]),
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



subscribe(AreaSpec) -> subscribe(AreaSpec, node()).

subscribe(AreaSpec, Node) ->
    gen_server:abcast(erlang:nodes(), doe_ets, 
                      {subscribe, AreaSpec, Node}).

unsubscribe(AreaSpec) -> unsubscribe(AreaSpec, node()).

unsubscribe(AreaSpec, Node) ->
    gen_server:abcast(erlang:nodes(), doe_ets, 
                      {unsubscribe, AreaSpec, Node}).


new_tree(TreeId, Options) -> 
    % notify the local event handler first
    gen_event:notify(?SERVER, {local_new_tree, TreeId, Options}),
%%  ?debugMsg("local notification sent"),
    % then the others, as this may take some time.
    gen_server:abcast(erlang:nodes(), doe_ets, {remote_new_tree, TreeId, Options}).


add_obj(ObjId, AreaSpec) -> add_obj(ObjId, AreaSpec, []).

%
% this only sets the initial position. Changes in position
% will be handled by the area update.
%
add_obj(ObjId, AreaSpec, Extra) ->
    % ?debugFmt("notifying of add_obj for ~p in ~p~n", [ObjId, AreaSpec]),
    % notify the local event handler first
    gen_event:notify(?SERVER, {local_new_obj, ObjId, AreaSpec, Extra}),
    Subscribers = gen_server:call(doe_ets, {get_subscribers, AreaSpec}),
    notify_subs(Subscribers, {remote_add_obj, ObjId, AreaSpec, Extra}).

update_area(ObjId, OldAreaSpec, NewAreaSpec) -> 
    % notify the local event handler first
    gen_event:notify(?SERVER, {local_leave_area, ObjId, OldAreaSpec}),
    gen_event:notify(?SERVER, {local_enter_area, ObjId, NewAreaSpec}),
    OldSubscribers = gen_server:call(doe_ets, {get_subscribers, OldAreaSpec}),
    NewSubscribers = gen_server:call(doe_ets, {get_subscribers, NewAreaSpec}),
    notify_subs(OldSubscribers, {leave_area, ObjId, OldAreaSpec}),
    notify_subs(NewSubscribers, {enter_area, ObjId, NewAreaSpec}).


update_position(ObjId, AreaSpec, NewPos, NewBBSize) -> 
    % notify the local event handler first
    gen_event:notify(?SERVER, {local_update_position, ObjId, AreaSpec, NewPos, NewBBSize}),
    Subscribers = gen_server:call(doe_ets, {get_subscribers, AreaSpec}),
    notify_subs(Subscribers, {update_position, ObjId, AreaSpec, NewPos, NewBBSize}).


remove_obj(ObjId, AreaSpec) -> 
    % notify the local event handler first
    gen_event:notify(?SERVER, {local_remove_obj, ObjId, AreaSpec}),
    Subscribers = gen_server:call(doe_ets, {get_subscribers, AreaSpec}),
    notify_subs(Subscribers, {remove_obj, ObjId, AreaSpec}).
    

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

notify_subs(Subscribers, Event) ->
    lists:map(fun(Sub) -> 
%%                       ?debugFmt("sending event ~p for ~p~n", [Event, Sub]),
                      gen_event:notify({?SERVER, Sub}, Event) 
              end, 
              Subscribers).
