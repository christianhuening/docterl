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
-module(doe_tree_event).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-define(SERVER, ?MODULE).

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0, add_handler/2, delete_handler/2, 
				 subscribe/1, unsubscribe/1, 
				 new_obj/2, update_area/3]).

%% ====================================================================
%% External functions
%% ====================================================================
start_link() -> 
    gen_event:start_link({local, ?SERVER}).

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

%
% this only sets the initial position. Changes in position
% will be handled by the area update.
%
new_obj(ObjId, AreaSpec) ->
		Subscribers = gen_server:call(doe_ets, {get_subscribers, AreaSpec}),
		lists:map(fun(Sub) -> 
											gen_event:notify({Sub, ?SERVER}, 
																			 {new_obj, ObjId, AreaSpec}) 
							end, 
							Subscribers).

%
% the old content is resent, so that the recipient need not query 
% an addition data source to get it.
%
update_area(AreaSpec, OldContent, NewContent) -> 
		Subscribers = gen_server:call(doe_ets, {get_subscribers, AreaSpec}),
		lists:map(fun(Sub) -> 
											gen_event:notify({Sub, ?SERVER}, 
																			 {update_area, AreaSpec, OldContent, NewContent}) 
							end, 
							Subscribers).


%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

