%%% -------------------------------------------------------------------
%%% Author  : sage
%%% Description : External API of octree_ets application
%%%
%%%  Important note: The 'objs' table does not hold any information 
%%%  about the objects maneged by this octree, beyond the position.
%%%  If there is additional data to be maintained, this needs to be done
%%%  by the application using this code in a separate table/database
%%%
%%% Created : 04.08.2012
%%% -------------------------------------------------------------------
-module(docterl_ets).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

-include("../include/docterl.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([new_tree/0, new_tree/1, add_obj/3, remove_obj/1, 
         update_position/4, get_obj/1, add_handler/1, set_extra/2, get_extra/1, 
         start_app/1]).

%% Utility functions for tree navigation. These allow hiding the implementation
%% details of the tree.
-export([parent/1, children/1, root/1]).


%% ====================================================================
%% External functions
%% ====================================================================

start_app(_Init) -> application:start(docterl_ets).

%% --------------------------------------------------------------------
%% @doc initialise a new tree.
%% <p>
%% the tree will contain a 3-D box with a edge length of 1.0 
%% </p>
%% @end
%% --------------------------------------------------------------------
-spec new_tree() -> {ok, tree_id()} | {error | term()}.
new_tree() -> new_tree([]).

%% --------------------------------------------------------------------
%% @doc initialise a new tree.
%% <p>
%% the tree will contain a 3-D box with a edge length of 1.0 
%% </p>
%%  Options:
%%
%%  <ul>
%%   <li>{max_depth, DEPTH}: maximum depth of tree (Default: 10).</li>
%%  </ul>
%% @end
%% --------------------------------------------------------------------
-spec new_tree(Options::list()) -> {ok, tree_id()} | {error | term()}.
new_tree(Options) -> 
    case (catch doe_ets:new_tree(Options)) of
        {ok, TreeId} -> doe_event_mgr:new_tree(TreeId, Options), 
                        {ok, TreeId};
        {error, Reason} -> {error, Reason};
        Other -> {error, {unknown_result, Other}} 
    end.

%% --------------------------------------------------------------------
%% Function: add_obj/3
%% Description: add an object to a tree. the cost of computing the proper 
%%              position in the tree are borne by this thread.
%% Returns: {ok, ObjId, AreaSpec}
%%          {error, invalid_tree}
%%          {error, Reason}
%% --------------------------------------------------------------------
-spec add_obj(TreeId::pos_integer(), Position::vec_3d(), BBSize::vec_3d()) -> 
          {ok, ObjId::pos_integer(), AreaSpec::area_spec()} | {error, term()}.
add_obj(TreeId, Position, BBSize) -> 
    case  (catch doe_ets:new_obj(TreeId, Position, BBSize)) of
        {ok, ObjId, AreaSpec} -> doe_event_mgr:add_obj(ObjId, AreaSpec),
                                 {ok, ObjId, AreaSpec};
        {error, Reason} -> {error, Reason};
        Other -> {error, {unknown_result, Other}} 
    end.

-spec remove_obj(ObjId::pos_integer()) -> 
          ok | {error, unkown_tree} | {error, invalid_id} | {error, Reason::term()}.
remove_obj(ObjId) ->
    case (catch doe_ets:remove_obj(ObjId)) of
        {ok, AreaSpec} -> doe_event_mgr:remove_obj(ObjId, AreaSpec),
%%               ?debugFmt("object ~p removed", [ObjId]),
              ok;
        {error, Reason} -> {error, Reason};
        Other -> {error, {unknown_result, Other}} 
    end.
                 

%%
%% @doc update the position of an object.
%%
%% there are a number of optimisations here:
%%  <ul>
%%   <li> if the content lists did not change, to not send the area updates</li>
%%   <li> movement event and area change could be combined into one event, at
%%     least for transmitting over the network.</li>
%%   </ul>
%%
%% Both optimisations are not used right now.
%%
%% TODO: check for position interval ( <code>0 &lt; pos &lt; 1.0</code> ) and size
%%
-spec update_position(TreeId::tree_id(), ObjId::obj_id(), 
                      NewPos::vec_3d(), NewBBSize::vec_3d()) -> {ok, AreaSpec::area_spec()} | {error, term()}.
update_position(TreeId, ObjId, NewPos, NewBBSize) -> 
    case (catch doe_ets:update_position(TreeId, ObjId, NewPos, NewBBSize)) of
        {ok, AreaSpec} ->   % the area was not changed. 
            doe_event_mgr:update_position(ObjId, AreaSpec, NewPos, NewBBSize),
            {ok, AreaSpec}; 
        {ok, OldAreaSpec, NewAreaSpec} -> 
            doe_event_mgr:update_area(ObjId, OldAreaSpec, NewAreaSpec),
            doe_event_mgr:update_position(ObjId, NewAreaSpec, NewPos, NewBBSize),
            {ok, NewAreaSpec};
        {error, Reason} -> {error, Reason};
        Other -> {error, {unknown_result, Other}} 
    end.


-spec get_obj(ObjId::obj_id()) -> {ok, AreaSpec::area_spec()} | {error, unknown_id} | {error, term()}.
get_obj(ObjId) -> doe_ets:get_obj(ObjId).


%% --------------------------------------------------------------------
%% @doc set extra information on the object
%%
%%   if the object id is not valid, the operation will fail silently.
%%
%% --------------------------------------------------------------------
-spec set_extra(ObjId::obj_id(), Extra::term()) -> ok.
set_extra(ObjId, Extra) -> doe_ets:set_extra(ObjId, Extra).

%% --------------------------------------------------------------------
%% @doc retrieve extra information on the object
%%
%%   will throw <code>{invalid_id, ObjId}</code> if the object Id can not be found.
%%
%% @throws invalid_id
%% @end
%% --------------------------------------------------------------------
-spec get_extra(ObjId::obj_id()) -> {ok, term()} | {error, term()}.
get_extra(ObjId) -> doe_ets:get_extra(ObjId).


-spec add_handler(Handler::atom()) -> ok | term().
add_handler(Handler) -> doe_event_mgr:add_handler(Handler, []).

%% --------------------------------------------------------------------
%% @doc get the parent node of a given octree node
%%
%% <p>the parent of the root is the root.</p>
%%
%% @throws invalid_area
%% @end
%% --------------------------------------------------------------------
-spec parent(AreaSpec::area_spec()) -> {ok, area_spec()} | {error, invalid_area}.
parent([]) -> throw(invalid_area);

parent([X]) -> [X];

parent(AreaSpec) when is_list(AreaSpec) -> lists:reverse(tl(lists:reverse(AreaSpec)));

parent(_) -> throw(invalid_area).


%% --------------------------------------------------------------------
%% @doc generate a list of all valid children of a given octree node.
%% @end
%% --------------------------------------------------------------------
-spec children(AreaSpec::area_spec()) -> [area_spec()].
children(AreaSpec) -> [lists:append(AreaSpec, [X]) || X <- lists:seq(0, 7) ].


%% --------------------------------------------------------------------
%% @doc get the root node of the tree, that the node is part of.
%%
%% @throws invalid_area
%% @end
%% --------------------------------------------------------------------
-spec root(AreaSpec::area_spec()) -> area_spec().
root([]) -> throw(invalid_area);

root([X]) -> [X];

root([X| _RestSpec]) -> [X]; % only the tree-id

root(_) ->  throw(invalid_area).



