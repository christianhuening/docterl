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

-type vec_3d() :: {float(), float(), float()}.
-type areas_spec() :: list(integer()).

%% --------------------------------------------------------------------
%% External exports
-export([new_tree/1, add_obj/3, remove_obj/2, 
         update_position/4]).


%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: new_tree/1
%% Description: initialise a new tree
%%
%% the tree will contain a 3-D box with a edge length of 1.0 
%%
%%  Options:
%%   {max_depth, DEPTH}: maximum depth of tree (Default: 10).
%%
%% Returns: {ok, TreeId}          |
%%          {error, Reason}
%% --------------------------------------------------------------------
-spec new_tree(Options::list()) -> {ok, pos_integer()} | {error | term()}.
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
          {ok, ObjId::pos_integer(), AreaSpec::list()} | {error, term()}.
add_obj(TreeId, Position, BBSize) -> 
    case (catch doe_ets:add_obj(TreeId, Position, BBSize)) of
        {ok, ObjId, AreaSpec} -> doe_event_mgr:add_obj(ObjId, AreaSpec),
                                 {ok, ObjId, AreaSpec};
        {error, Reason} -> {error, Reason};
        Other -> {error, {unknown_result, Other}} 
    end.

-spec remove_obj(TreeId::pos_integer(), ObjId::pos_integer()) -> 
          ok | {error, unkown_tree} | {error, invalid_obj} | {error, Reason::term()}.
remove_obj(TreeId, ObjId) ->
    case (catch doe_ets:remove_obj(TreeId, ObjId)) of
        ok -> doe_event_mgr:remove_obj(TreeId, ObjId),
              ok;
        {error, Reason} -> {error, Reason};
        Other -> {error, {unknown_result, Other}} 
    end.
                 

%%
%% there are a number of optimisations here:
%%   - if the content lists did not change, to not send the area updates
%%   - movement event and area change could be combined into one event, at
%%     least for transmitting over the network.
%%
%% Both optimisations are not used right now.
%%
%% TODO: check for position interval ( 0 < pos < 1.0 ) and size
%%

-spec update_position(TreeId::pos_integer(), ObjId::pos_integer(), 
                      NewPos::vec_3d(), NewBBSize::vec_3d()) -> {ok, AreaSpec::list()} | {error, term()}.
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

