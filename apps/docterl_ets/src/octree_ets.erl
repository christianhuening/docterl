%%% -------------------------------------------------------------------
%%% Author  : sage
%%% Description :
%%%
%%% Created : 04.08.2012
%%% -------------------------------------------------------------------
-module(octree_ets).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

-type vec_3d() :: {float(), float(), float()}.

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0, make_tree/1, add_obj/4, add_obj_lazy/4, update_position/6, update_position_lazy/6]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

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
make_tree(Options) -> gen_server:call({local, ?MODULE}, {make_tree, Options}).

%% --------------------------------------------------------------------
%% Function: add_obj/4
%% Description: add an object to a tree. the cost of computing the proper 
%%              position in the tree are borne by this thread. See 
%%              add_obj_lazy/4 if the server should carry the load.
%% Returns: {ok, AreaSpec}
%%          {error, invalid_tree}
%%          {error, Reason}
%% --------------------------------------------------------------------
-spec add_obj(TreeId::pos_integer(), 
              Obj::term(), Position::vec_3d(), BBSize::vec_3d()) -> {ok, AreaSpec::list()} | {error, term()}.
add_obj(TreeId, Obj, Position, BBSize) -> 
	AreaSpec = make_area_code(TreeId, Position, BBSize),
	Ret = gen_server:call({local, ?MODULE}, {add_obj, AreaSpec, Obj}),
    case Ret of
      {ok, AreaSpec} -> {ok, AreaSpec};
      {error, Reason} -> {error, Reason};
      _ -> {error, { unknown_failure, Ret}}
    end.

%% have others do the work of computing the 
-spec add_obj_lazy(TreeId::pos_integer(), 
              Obj::term(), Position::vec_3d(), BBSize::vec_3d()) -> {ok, AreaSpec::list()} | {error, term()}.
add_obj_lazy(_TreeId, _Obj, _Position, _BBSize) -> {error, not_implemented}.

-spec update_position(TreeId::pos_integer(), Obj::term(), 
					  OldPos::vec_3d(), OldBBSize::vec_3d(),
					  NewPos::vec_3d(), NewBBSize::vec_3d()) -> {ok, AreaSpec::list()} | {error, term()}.
update_position(_TreeId, _Obj, _OldPos, _OldBBSize, _NewPos, _NewBBSize) -> {error, not_implemented}.

-spec update_position_lazy(TreeId::pos_integer(), Obj::term(), 
					  OldPos::vec_3d(), OldBBSize::vec_3d(),
					  NewPos::vec_3d(), NewBBSize::vec_3d()) -> {ok, AreaSpec::list()} | {error, term()}.
update_position_lazy(_TreeId, _Obj, _OldPos, _OldBBSize, _NewPos, _NewBBSize) -> {error, not_implemented}.
	
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
	ets:new(trees, [set, named_table, {read_concurrency, true}]),
	ets:new(areas, [set, named_table]),
    {ok, ?MODULE}.

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
    NewId = make_tree_id(),
	ets:insert(trees, {NewId, Options}),
    {reply, NewId, State};

handle_call({add_obj, AreaSpec, Obj}, _From, State) ->	
	{ObjList} = ets:lookup(areas, AreaSpec),
	ets:insert(areas, {[Obj|ObjList]}),
    {reply, {ok, AreaSpec}, State};

handle_call({add_obj_lazy, TreeId, Obj, Position, BBSize}, From, State) ->	
	AreaSpec = make_area_code(TreeId, Position, BBSize),
	handle_call({add_obj, AreaSpec, Obj}, From, State).


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
					_ -> 10
			end.

opt(Op, [{Op, Value}|_]) ->
    {ok, Value};
opt(Op, [_|Options]) ->
    opt(Op, Options);
opt(_, []) ->
    false.


% compute the path to the area
make_area_code(TreeId, Position, BBSize) -> 
	%% TODO: check that position and/or bbsize are not greater than 1.0
	AreaSpec = make_area_code_step([], {0.0, 0.0, 0.0}, {1.0, 1.0, 1.0}, Position, BBSize, 10),
	%% TODO: test if list not empty
    [TreeId|lists:inverse(AreaSpec)].


make_area_code_step(_AreaSpec, _MinPos, _MaxPos, _ObjPos, _BBSize, ResRest) when ResRest < 0 -> [];

%% if any dimension objpos < minpos -> fail
make_area_code_step(_, {MinPos1, _, _}, _, {ObjPos1, _, _}, _, _) when ObjPos1 < MinPos1 -> [];
make_area_code_step(_, {_, MinPos2, _}, _, {_, ObjPos2, _}, _, _) when ObjPos2 < MinPos2 -> [];
make_area_code_step(_, {_, _, MinPos3}, _, {_, _, ObjPos3}, _, _) when ObjPos3 < MinPos3 -> [];

%% if any dimension objpos+bbsize > maxpos -> fail
make_area_code_step(_, _, {MaxPos1, _, _}, {ObjPos1, _, _}, {BBSize1, _, _}, _) when (ObjPos1 + BBSize1) > MaxPos1 -> [];
make_area_code_step(_, _, {_, MaxPos2, _}, {_, ObjPos2, _}, {_, BBSize2, _}, _) when (ObjPos2 + BBSize2) > MaxPos2 -> [];
make_area_code_step(_, _, {_, _, MaxPos3}, {_, _, ObjPos3}, {_, _, BBSize3}, _) when (ObjPos3 + BBSize3) > MaxPos3 -> [];


make_area_code_step(AreaSpec, {MinPos1, MinPos2, MinPos3}, {MaxPos1, MaxPos2, MaxPos3},
					{ObjPos1, ObjPos2, ObjPos3}, {BBSize1, BBSize2, BBSize3}, ResRest) ->
	{Bit1, NewMin1, NewMax1} = calc_border(MinPos1, MaxPos1, ObjPos1, BBSize1, 1),	
	{Bit2, NewMin2, NewMax2} = calc_border(MinPos2, MaxPos2, ObjPos2, BBSize2, 2),	
	{Bit3, NewMin3, NewMax3} = calc_border(MinPos3, MaxPos3, ObjPos3, BBSize3, 4),
	SubIdx = Bit1 + Bit2 + Bit3,
	make_area_code_step([SubIdx|AreaSpec], {NewMin1, NewMin2, NewMin3}, {NewMax1, NewMax2, NewMax3},
						{ObjPos1, ObjPos2, ObjPos3}, {BBSize1, BBSize2, BBSize3}, ResRest-1).


-spec calc_border(MinPos::float(), MaxPos::float(), ObjPos::float(), BBLen::float(), BitMult::pos_integer()) 
		-> {pos_integer(), float(), float()} | error.
calc_border(MinPos, MaxPos, ObjPos, BBLen, BitMult) ->
	Center = ((MaxPos - MinPos) / 2) + MinPos,
	if
		(Center > ObjPos), (Center > (ObjPos + BBLen)) -> {BitMult, Center, MaxPos};
		true -> {0, MinPos, Center}
	end.

%% TODO: get numbers from tree list
make_tree_id() -> 1.