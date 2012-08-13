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
-export([make_tree/1, add_obj/3, remove_obj/2, 
         update_position/4]).


%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: make_tree/1
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
-spec make_tree(Options::list()) -> {ok, pos_integer()} | {error | term()}.
make_tree(Options) -> gen_server:call(doe_ets, {make_tree, Options}).

%% --------------------------------------------------------------------
%% Function: add_obj/3
%% Description: add an object to a tree. the cost of computing the proper 
%%              position in the tree are borne by this thread.
%% Returns: {ok, AreaSpec}
%%          {error, invalid_tree}
%%          {error, Reason}
%% --------------------------------------------------------------------
-spec add_obj(TreeId::pos_integer(), Position::vec_3d(), BBSize::vec_3d()) -> 
          {ok, ObjId::pos_integer(), AreaSpec::list()} | {error, term()}.
add_obj(TreeId, Position, BBSize) -> 
    gen_server:call(doe_ets, {add_obj, TreeId, Position, BBSize}).

-spec remove_obj(TreeId::pos_integer(), ObjId::pos_integer()) -> 
          ok | {error, unkown_tree} | {error, invalid_obj} | {error, Reason::term()}.
remove_obj(TreeId, ObjId) ->
    gen_server:call(doe_ets, {remove_obj, TreeId, ObjId}).
                 
-spec update_position(TreeId::pos_integer(), ObjId::pos_integer(), 
					  NewPos::vec_3d(), NewBBSize::vec_3d()) -> {ok, AreaSpec::list()} | {error, term()}.
update_position(TreeId, ObjId, NewPos, NewBBSize) -> 
    gen_server:call(doe_ets, {update_position, TreeId, ObjId, NewPos, NewBBSize}).

