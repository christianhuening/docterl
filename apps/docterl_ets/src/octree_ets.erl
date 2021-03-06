%%% -------------------------------------------------------------------
%%% Author  : sage
%%% Description :
%%%
%%%  Important note: The 'objs' table does not hold any information 
%%%  about the objects maneged by this octree, beyond the position.
%%%  If there is additional data to be maintained, this needs to be done
%%%  by the application using this code in a separate table/database
%%%
%%% Created : 04.08.2012
%%% -------------------------------------------------------------------
-module(octree_ets).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

-define(MAX_DEPTH_DEFAULT, 10).

-type vec_3d() :: {float(), float(), float()}.
-type areas_spec() :: list(integer()).

-record(state, {trees_tid, areas_tid, objs_tid}).


%% --------------------------------------------------------------------
%% External exports
-export([start_link/0, make_tree/1, add_obj/3, remove_obj/2, 
         update_position/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-ifdef(TEST).
-export([make_area_code/4, make_area_code_step/6, make_tree_id/1, make_obj_id/1, 
         do_make_tree/2, do_make_obj/2]).
-endif.

%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: start_link/0
%% Description: set up the ets tables 
%%  Options:
%% Returns: {ok, PID}
%% --------------------------------------------------------------------
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
	

%% --------------------------------------------------------------------
%% Function: make_tree/1
%% Description: initialise a new tree
%%
%% the tree will contain a 3-D box with a edge length of 1.0 
%%
%%  Options:
%%   max_depth: maximum depth of tree (Default: 10).
%%
%% Returns: {ok, TreeId}          |
%%          {error, Reason}
%% --------------------------------------------------------------------
-spec make_tree(Options::list()) -> {ok, pos_integer()} | {error | term()}.
make_tree(Options) -> gen_server:call(?MODULE, {make_tree, Options}).

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
    gen_server:call(?MODULE, {add_obj, TreeId, Position, BBSize}).

-spec remove_obj(TreeId::pos_integer(), ObjId::pos_integer()) -> 
          ok | {error, unkown_tree} | {error, invalid_obj} | {error, Reason::term()}.
remove_obj(TreeId, ObjId) ->
    gen_server:call(?MODULE, {remove_obj, TreeId, ObjId}).
                 
-spec update_position(TreeId::pos_integer(), ObjId::pos_integer(), 
					  NewPos::vec_3d(), NewBBSize::vec_3d()) -> {ok, AreaSpec::list()} | {error, term()}.
update_position(TreeId, ObjId, NewPos, NewBBSize) -> 
    gen_server:call(?MODULE, {update_position, TreeId, ObjId, NewPos, NewBBSize}).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
	TreesTId = ets:new(trees, [set, {read_concurrency, true}]),
    ObjsTId = ets:new(objs, [set]),
    AreasTId = ets:new(areas, [set]),
    {ok, #state{trees_tid = TreesTId, areas_tid = AreasTId, objs_tid = ObjsTId}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({make_tree, Options}, _From, State) ->
		NewId = do_make_tree(State#state.trees_tid, Options),
	  {reply, NewId, State};

handle_call({add_obj, TreeId, Position, BBSize}, _From, State) ->	
    [{TreeId, TreeOpts}] = ets:lookup(State#state.trees_tid, TreeId), % TODO: handle error
    AreaSpec = make_area_code(TreeId, Position, BBSize, max_depth_opt(TreeOpts)),
    ObjId = do_make_obj(State#state.objs_tid, AreaSpec),
    do_area_add_obj(State#state.areas_tid, AreaSpec, ObjId),
    {reply, {ok, ObjId, AreaSpec}, State};

% TODO: ensure that the object was actually placed in this tree. 
handle_call({remove_obj, _TreeId, ObjId}, _From, State) ->
    [{ObjId, AreaSpec}] = ets:lookup(State#state.objs_tid, ObjId),
    [{AreaSpec, ObjList}] = ets:lookup(State#state.areas_tid, AreaSpec),
		do_area_remove_obj(State#state.areas_tid, AreaSpec, ObjId),
    ets:update_element(State#state.areas_tid, AreaSpec, {2, lists:delete(ObjId, ObjList)}),
    {reply, ok, State};

handle_call({update_position, TreeId, ObjId, NewPos, NewBBSize}, _From, State) ->
		case (catch do_update_position(State, TreeId, ObjId, NewPos, NewBBSize)) of
				{ok, NewAreaSpec} -> {reply, {ok, NewAreaSpec}, State};
				{error, Reason} -> {reply, {error, Reason}, State};
				Unknown -> {stop, {error, unknown_cause, Unknown}}
		end.

do_update_position(State, TreeId, ObjId, NewPos, NewBBSize) ->
		case ets:lookup(State#state.trees_tid, TreeId) of
				[{TreeId, TreeOpts}] -> ok;
				[] -> throw(invalid_key_id), TreeOpts = []
				end,
    NewAreaSpec = make_area_code(TreeId, NewPos, NewBBSize, max_depth_opt(TreeOpts)),
    case ets:lookup(State#state.objs_tid, ObjId) of
        [{_, OldAreaSpec}] -> 
							% add to new area
							do_area_add_obj(State#state.areas_tid, NewAreaSpec, ObjId),
							% update obj entry
							ets:update_element(State#state.objs_tid, ObjId, {2, NewAreaSpec}),
							% remove from old area
							do_area_remove_obj(State#state.areas_tid, OldAreaSpec, ObjId),
              {ok, NewAreaSpec};
        [] -> {error, invalid_obj_id}
        end.


%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
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

max_depth_opt(Options) ->
    case opt(max_depth, Options) of
        {ok, MD_Val} ->
            MD_Val;
        _ -> ?MAX_DEPTH_DEFAULT
    end.

opt(Op, [{Op, Value}|_]) ->
    {ok, Value};
opt(Op, [_|Options]) ->
    opt(Op, Options);
opt(_, []) ->
    false.


% compute the path to the area
make_area_code(TreeId, Position, BBSize, MaxDepth) -> 
	%% TODO: check that position and/or bbsize are not greater than 1.0
	AreaSpec = make_area_code_step([], {0.0, 0.0, 0.0}, {1.0, 1.0, 1.0}, Position, BBSize, MaxDepth),
	%% TODO: test if list not empty
  %% cut of the final value, it is an artefact of the termination criterion
  [_Final|CorrectSpec] = AreaSpec,
  [TreeId|lists:reverse(CorrectSpec)].


make_area_code_step(AreaSpec, _MinPos, _MaxPos, _ObjPos, _BBSize, ResRest) 
  when ResRest < 0 -> 
    ?debugHere,
    AreaSpec;

%% if any dimension objpos < minpos -> fail
make_area_code_step(AreaSpec, {MinPos1, _, _}, {MaxPos1, _, _}, {ObjPos1, _, _}, _, _) 
	when ObjPos1 < MinPos1 -> 
%		?debugFmt("MinPos1: ~p, MaxPos1: ~p, ObjPos1: ~p~n", [MinPos1, MaxPos1, ObjPos1]),
		AreaSpec;
make_area_code_step(AreaSpec, {_, MinPos2, _}, _, {_, ObjPos2, _}, _, _) when ObjPos2 < MinPos2 -> AreaSpec;
make_area_code_step(AreaSpec, {_, _, MinPos3}, _, {_, _, ObjPos3}, _, _) when ObjPos3 < MinPos3 -> AreaSpec;

%% if any dimension objpos+bbsize > maxpos -> fail
make_area_code_step(AreaSpec, _, {MaxPos1, _, _}, {ObjPos1, _, _}, {BBSize1, _, _}, _) 
	when (ObjPos1 + BBSize1) > MaxPos1 -> AreaSpec;
make_area_code_step(AreaSpec, _, {_, MaxPos2, _}, {_, ObjPos2, _}, {_, BBSize2, _}, _) 
	when (ObjPos2 + BBSize2) > MaxPos2 -> AreaSpec;
make_area_code_step(AreaSpec, _, {_, _, MaxPos3}, {_, _, ObjPos3}, {_, _, BBSize3}, _) 
	when (ObjPos3 + BBSize3) > MaxPos3 -> AreaSpec;


make_area_code_step(AreaSpec, {MinPos1, MinPos2, MinPos3}, {MaxPos1, MaxPos2, MaxPos3},
                    {ObjPos1, ObjPos2, ObjPos3}, {BBSize1, BBSize2, BBSize3}, ResRest) ->
    {Bit1, NewMin1, NewMax1} = calc_border(MinPos1, MaxPos1, ObjPos1, BBSize1, 1),	
    {Bit2, NewMin2, NewMax2} = calc_border(MinPos2, MaxPos2, ObjPos2, BBSize2, 2),	
    {Bit3, NewMin3, NewMax3} = calc_border(MinPos3, MaxPos3, ObjPos3, BBSize3, 4),
    SubIdx = Bit1 + Bit2 + Bit3, 
    %		?debugFmt("SubId: ~p~n", [SubIdx]),
    make_area_code_step([SubIdx|AreaSpec], {NewMin1, NewMin2, NewMin3}, {NewMax1, NewMax2, NewMax3},
                        {ObjPos1, ObjPos2, ObjPos3}, {BBSize1, BBSize2, BBSize3}, ResRest-1).


-spec calc_border(MinPos::float(), MaxPos::float(), ObjPos::float(), BBLen::float(), BitMult::pos_integer()) 
        -> {pos_integer(), float(), float()} | error.
calc_border(MinPos, MaxPos, ObjPos, BBLen, BitMult) ->
    Center = ((MaxPos - MinPos) / 2) + MinPos,
    %	?debugFmt("MinPos: ~p, MaxPos: ~p, Center: ~p, ObjPos: ~p, ObjPos+BBLen: ~p, BitMult: ~p~n", 
    %						[MinPos, MaxPos, Center, ObjPos, (ObjPos+BBLen), BitMult]),
    if
        (Center > ObjPos), (Center > (ObjPos + BBLen)) -> 
            {0, MinPos, Center};
        true -> {BitMult, Center, MaxPos}
    end.

make_tree_id(TreesTId) -> ets:foldl(fun id_max/2, 0, TreesTId) + 1.

%% this is neither fast nor can it be distributed, but it will work for now.
make_obj_id(ObjsTId) -> ets:foldl(fun id_max/2, 0, ObjsTId) + 1.

id_max({Id, _}, Curr) ->
    max(Id, Curr).

do_make_tree(TreesTId, Options) ->
    NewId = make_tree_id(TreesTId),
    ets:insert(TreesTId, {NewId, Options}),
    NewId.

do_make_obj(ObjsTId, AreaSpec) ->
    NewId = make_obj_id(ObjsTId),
    ets:insert(ObjsTId, {NewId, AreaSpec}),
    NewId.

%%
%% remove single object entry from the list of objects in area
%% 
-spec do_area_remove_obj(AreasTId::integer(), AreasSpec::areas_spec(), ObjId::pos_integer()) ->
					true | false.
do_area_remove_obj(AreasTId, AreaSpec, ObjId) ->
    [{AreaSpec, ObjList}] = ets:lookup(AreasTId, AreaSpec),
    ets:update_element(AreasTId, AreaSpec, {2, lists:delete(ObjId, ObjList)}).
    
do_area_add_obj(AreasTId, AreaSpec, ObjId) ->
		case ets:lookup(AreasTId, AreaSpec) of
				[] -> ToWrite = [ObjId];
				[{AreaSpec, ObjList}] -> ToWrite = [ObjId|ObjList];
				Ret -> ?debugFmt("multiple Entries: ~p~n", [Ret]), 
							 ToWrite = [],
							 throw(multiple_area_entries)
		end,
    ets:insert(AreasTId, {AreaSpec, ToWrite}).

