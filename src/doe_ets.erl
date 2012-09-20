
%%% @doc maintain actual octree data structure.
%%%
%%%   (or ets tables in this case.) This module will not publish 
%%%    the changes to other nodes!
%%%  <p>
%%%  Important note: The 'objs' table does not hold any information 
%%%  about the objects managed by this octree, beyond the position.
%%%  If there is additional data to be maintained, this needs to be done
%%%  by the application using this code in a separate table/database
%%%  </p>
%%%
%%% @author Lutz Behnke <lutz.behnke@informatik.haw-hamburg.de>
%%% @copyright Lutz Behnke
%%%
-module(doe_ets).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

-include("../include/docterl.hrl").

-define(DEFAULT_MAX_DEPTH, 10).

-define(DEFAULT_TIMEOUT, infinity).


-record(state, {trees_tid, areas_tid, objs_tid}).


%% --------------------------------------------------------------------
%% External exports
-export([start_link/0, stop/0, new_tree/1, remote_new_tree/2, remote_add_obj/3, new_obj/3, get_obj/1,
         remove_obj/1, remove_obj/2, get_subscribers/1, update_position/4, leave_area/2, enter_area/2, 
         get_members/1, set_extra/2, get_extra/1, subscribe/2, unsubscribe/2, get_tree_ids/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-ifdef(TEST).
%% export the private functions for testing only.
-export([make_area_code/4, make_area_code_step/6, make_new_id/1, 
         do_make_tree/2, do_make_obj/2]).
-endif.

%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc start server.
%% @end
%% --------------------------------------------------------------------
-spec start_link() -> {ok, Pid::pid()} | ignore | {error, Reason::term()}.
start_link() -> 
    % ?debugMsg("starting doe_ets server"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
	
%% --------------------------------------------------------------------
%% @doc halt server.
%% @end
%% --------------------------------------------------------------------
-spec stop() -> ok.
stop() -> Ret = gen_server:cast(?MODULE, {stop}),
          % ?debugFmt("the call to stop the server returned: ~p~n", [Ret]),
          Ret.


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
-spec new_tree(Options::list()) -> {ok, pos_integer()} | {error | term()}.
new_tree(Options) -> gen_server:call(?MODULE, {new_tree, Options}, ?DEFAULT_TIMEOUT).

%% --------------------------------------------------------------------
%% Function: new_obj/3
%% Description: add an object to a tree. 
%% Returns: {ok, ObjId, AreaSpec}
%%          {error, invalid_tree}
%%          {error, Reason}
%% --------------------------------------------------------------------
-spec new_obj(TreeId :: pos_integer(), Position :: vec_3d(),
              BBSize :: vec_3d() | float()) -> 
                 {ok, ObjId :: pos_integer(), AreaSpec :: list()}
                  | {error, term()}.
new_obj(TreeId, Position, BBSize) when is_float(BBSize) ->
    gen_server:call(?MODULE, 
                    {new_obj, TreeId, Position, {BBSize,BBSize,BBSize}}, 
                    ?DEFAULT_TIMEOUT);
new_obj(TreeId, Position, {Sx,Sy,Sz}) ->
    gen_server:call(?MODULE, 
                    {new_obj, TreeId, Position, {Sx,Sy,Sz}}, 
                    ?DEFAULT_TIMEOUT);
new_obj(_TreeId, _Position, _Size) -> 
    {error, invalid_pos_or_size}.

-spec get_obj(ObjId::pos_integer()) -> 
          {ok, AreaSpec::area_spec()} | {error, unknown_id} | {error, Reason::term()}.
get_obj(ObjId) -> gen_server:call(?MODULE, {get_obj, ObjId}, ?DEFAULT_TIMEOUT).

%% the area spec is returned for use by the event mechanism
-spec remove_obj(ObjId::pos_integer()) -> 
          {ok, AreaSpec::area_spec()} | {error, invalid_id} | {error, Reason::term()}.
remove_obj(ObjId) ->
    gen_server:call(?MODULE, {remove_obj, ObjId},?DEFAULT_TIMEOUT).

-spec update_position(TreeId::pos_integer(), ObjId::pos_integer(), 
                      NewPos::vec_3d(), NewBBSize::vec_3d() | float()) -> 
          {ok, AreaSpec::list()} | {ok, OldAreaSpec::list(), NewAreaSpec::list()} | {error, term()}.
update_position(TreeId, ObjId, NewPos, NewBBSize) when is_float(NewBBSize) -> 
    gen_server:call(?MODULE, 
                    {update_position, TreeId, ObjId, NewPos, {NewBBSize, NewBBSize, NewBBSize}},
                    ?DEFAULT_TIMEOUT);
update_position(TreeId, ObjId, NewPos, {Sx, Sy, Sz}) -> 
    gen_server:call(?MODULE, 
                    {update_position, TreeId, ObjId, NewPos, {Sx, Sy, Sz}}, 
                    ?DEFAULT_TIMEOUT);
update_position(_TreeId, _ObjId, _NewPos, _NewSize) ->
    {error, invalid_pos_or_size}.

-spec get_members(AreaSpec::list()) -> {ok, Members::list()} | {error, term()}.
get_members(AreaSpec) -> gen_server:call(?MODULE, 
                                         {get_members, AreaSpec},
                                         ?DEFAULT_TIMEOUT).


-spec get_subscribers(AreaSpec::area_spec()) -> {ok, Subsribers::list()} | {error, term()}.
get_subscribers(AreaSpec) -> gen_server:call(doe_ets, {get_subscribers, AreaSpec}, ?DEFAULT_TIMEOUT).


%% should only be called internally by doe_event_mgr
-spec remote_new_tree(TreeId :: pos_integer(), Options :: list()) -> 
                         {ok, pos_integer()}  | {error, term()}.
remote_new_tree(TreeId, Options) -> gen_server:cast(?MODULE, {remote_new_tree, TreeId, Options}).

%% should only be called internally by doe_event_mgr
-spec leave_area(ObjId::pos_integer(), AreaSpec::list()) ->
          ok.
leave_area(ObjId, AreaSpec) -> gen_server:cast(?MODULE, {remote_leave_area, ObjId, AreaSpec}).

%% should only be called internally by doe_event_mgr
-spec enter_area(ObjId::pos_integer(), AreaSpec::list()) ->
          ok.
enter_area(ObjId, AreaSpec) -> gen_server:cast(?MODULE, {remote_enter_area, ObjId, AreaSpec}).

%% should only be called internally by doe_event_mgr
-spec remote_add_obj(ObjId::pos_integer(), AreaSpec::list(), Extra::term()) -> 
          ok | {error, term()}.
remote_add_obj(ObjId, AreaSpec, Extra) -> 
    gen_server:call(?MODULE, {remote_add_obj, ObjId, AreaSpec, Extra}, ?DEFAULT_TIMEOUT).

%% should only be called internally by doe_event_mgr
-spec remove_obj(ObjId::obj_id(), AreaSpec::area_spec()) -> 
          ok | {error, invalid_id} | {error, Reason::term()}.
remove_obj(ObjId, AreaSpec) ->
    gen_server:call(?MODULE, {remove_obj, ObjId, AreaSpec}, ?DEFAULT_TIMEOUT).


-spec set_extra(ObjId::obj_id, Extra::term()) -> ok.
set_extra(ObjId, Extra) -> gen_server:cast(?MODULE, {set_extra, ObjId, Extra}).

-spec get_extra(ObjId::obj_id()) -> {ok, term()} | {error, term()}.
get_extra(ObjId) -> gen_server:call(?MODULE, {get_extra, ObjId}, ?DEFAULT_TIMEOUT).


-spec subscribe(AreaSpec::area_spec(), Node::atom()) -> ok.
subscribe(AreaSpec, Node) -> 
    gen_server:cast(?MODULE, {subscribe, AreaSpec, Node}).

-spec unsubscribe(AreaSpec::area_spec(), Node::atom()) -> ok.
unsubscribe(AreaSpec, Node) -> 
    gen_server:cast(?MODULE, {unsubscribe, AreaSpec, Node}).

-spec get_tree_ids() -> [ pos_integer()].
get_tree_ids() ->
    gen_server:call(?MODULE, {get_tree_ids}).

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
%%    process_flag(trap_exit, true),
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
handle_call({new_tree, Options}, _From, State) ->
      NewId = do_make_tree(State#state.trees_tid, Options),
	  {reply, {ok, NewId}, State};

handle_call({new_obj, TreeId, Position, BBSize}, _From, State) ->
    case (catch ets:lookup(State#state.trees_tid, TreeId)) of
        [{TreeId, TreeOpts}] -> 
            AreaSpec = make_area_code(TreeId, Position, BBSize, max_depth_opt(TreeOpts)),
            ObjId = do_make_obj(State#state.objs_tid, AreaSpec),
            do_area_add_obj(State#state.areas_tid, AreaSpec, ObjId),
            {reply, {ok, ObjId, AreaSpec}, State};        
        [] -> 
            {reply, {error, unknown_tree_id}, State};        
        Unknown -> 
            {reply, {error, Unknown}, State} 
    end;

% TODO: assert that obj id has not been used before
handle_call({remote_add_obj, ObjId, AreaSpec, Extra}, _From, State) ->
      ets:insert(State#state.objs_tid, {ObjId, AreaSpec, Extra}),
      do_area_add_obj(State#state.areas_tid, AreaSpec, ObjId),
      {reply, ok, State};

handle_call({remove_obj, ObjId}, From, State) ->
    [{ObjId, AreaSpec, _Extra}] = ets:lookup(State#state.objs_tid, ObjId),
    handle_call({remove_obj, ObjId, AreaSpec}, From, State);

handle_call({remove_obj, ObjId, AreaSpec}, _From, State) ->
    case (catch do_area_remove_obj(State#state.areas_tid, AreaSpec, ObjId)) of
        true -> ets:delete(State#state.objs_tid, ObjId), 
                {reply, {ok, AreaSpec}, State};        
        {error, Reason} -> {reply, {error, Reason}, State}
        end;

handle_call({update_position, TreeId, ObjId, NewPos, NewBBSize}, _From, State) ->
		case (catch do_update_position(State, TreeId, ObjId, NewPos, NewBBSize)) of
                {ok, AreaSpec} -> {reply, {ok, AreaSpec}, State};
                {ok, OldAreaSpec, NewAreaSpec} -> {reply, {ok, OldAreaSpec, NewAreaSpec}, State};
				{error, Reason} -> {reply, {error, Reason}, State};
				Unknown -> {stop, {error, unknown_cause, Unknown}}
		end;

handle_call({update_precalc_position, ObjId, OldAreaSpec, NewAreaSpec}, _From, State) ->
        case (catch do_precalc_update_position(State, ObjId, OldAreaSpec, NewAreaSpec)) of
                {ok, AreaSpec} -> {reply, {ok, AreaSpec}, State};
                {ok, OldAreaSpec, NewAreaSpec} -> {reply, {ok, OldAreaSpec, NewAreaSpec}, State};
                {error, Reason} -> {reply, {error, Reason}, State};
                Unknown -> {stop, {error, unknown_cause, Unknown}}
        end;

handle_call({get_subscribers, AreaSpec}, _From, State) ->
		case (catch ets:lookup(State#state.areas_tid, AreaSpec)) of
				[{AreaSpec, _ObjList, Subscribers}] -> {reply, Subscribers, State};
				[] -> {reply, [], State};
				Unknown ->  {stop, {error, unknown_cause, Unknown}}
		end;

handle_call({get_members, AreaSpec}, _From, State) ->
        case (catch ets:lookup(State#state.areas_tid, AreaSpec)) of
            [] -> {reply, {ok, []}, State};
            [{AreaSpec, Members, _Subscribers}] -> {reply, {ok, Members}, State};
            Unknown -> {stop, {error, unknown_cause, Unknown}, State} 
            end;

handle_call({get_obj, ObjId}, _From, State) ->
        case (catch ets:lookup(State#state.objs_tid, ObjId)) of
            [{ObjId, AreaSpec, _Extra}] -> {reply, {ok, AreaSpec}, State};
            [] -> {reply, {error, unknown_id}, State};
            Other -> {reply, {error, Other}}
        end;

handle_call({get_extra, ObjId}, _From, State) ->
        case ets:lookup(State#state.objs_tid, ObjId) of
            [{ObjId, _AreaSpec, Extra}] -> {reply, {ok, Extra}, State};
            [] -> {reply, {error, unknown_id}, State};
            Other -> {reply, {error, Other}}
        end;

handle_call({get_tree_ids}, _From, State) ->
    ets:foldl(fun({TreeId, Options}, Acc) ->
                      [TreeId, Acc]
                      end, 
              [], State#state.trees_tid);

handle_call(Request, _From, State) ->
    ?debugFmt("unknown request: ~p~n", [Request]),
    {reply, {error, unknown_request}, State}.



%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
%% tree was created on other node, the tree id is already determined.
%% TODO: this would require massive consistency checking and handling!!!
%% TODO: assert that tree id has not been used before
handle_cast({remote_new_tree, TreeId, Options}, State) ->
    ets:insert(State#state.trees_tid, {TreeId, Options}),
    {noreply, State};

handle_cast({remote_enter_area, ObjId, AreaSpec}, State) ->
    do_area_add_obj(State#state.areas_tid, AreaSpec, ObjId),
    {noreply, State};

handle_cast({remote_leave_area, ObjId, AreaSpec}, State) ->
    do_area_remove_obj(State#state.areas_tid, AreaSpec, ObjId),
    {noreply, State};

handle_cast({set_extra, ObjId, Extra}, State) ->
    ets:update_element(State#state.objs_tid, ObjId, [{3, Extra}]),
    {noreply, State};

handle_cast({subscribe, AreaSpec, Node}, State) ->
    Tid = State#state.areas_tid,
    case (catch ets:lookup(Tid, AreaSpec)) of
        [] -> 
            ets:insert(Tid, {AreaSpec, [], [Node]}),
            Result = {noreply, State};
        [{AreaSpec, _ObjList, OldSubs}] -> 
            NewList = lists:usort([Node|OldSubs]),
            ets:update_element(Tid, AreaSpec, [{3, NewList}]),
            Result = {noreply, State};
        Unknown ->  
            Result = {stop, {error, unknown_cause, Unknown}, State}  
    end,
    ?debugFmt("subscribing ~p to ~p resulted in ~p", [Node, AreaSpec, Result]),
    Result;

handle_cast({unsubscribe, AreaSpec, Node}, State) ->
    Tid = State#state.areas_tid,
    case (catch ets:lookup(Tid, AreaSpec)) of
        [] -> % was not in the list before, will not be now 
            {noreply, State};
        [{AreaSpec, _ObjList, OldSubs}] -> 
            NewSubs = lists:delete(Node, OldSubs), 
            ets:update_element(Tid, AreaSpec, [{3, NewSubs}]),
            {noreply, State};
        Unknown ->  
            {stop, {error, unknown_cause, Unknown}, State}  
    end;

handle_cast({stop}, State) ->
    {stop, normal, State}.

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
terminate(_Reason, State) ->
    ets:delete(State#state.trees_tid),
    ets:delete(State#state.areas_tid),
    ets:delete(State#state.objs_tid),
%%     ?debugFmt("terminating doe_ets server. Reason: ~p~n", [Reason]),
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% --------------------------------------------------------------------
%%% Internal functions
%%% --------------------------------------------------------------------

do_precalc_update_position(State, ObjId, OldAreaSpec, NewAreaSpec) ->
    % add to new area
    do_area_add_obj(State#state.areas_tid, NewAreaSpec, ObjId),
    % update obj entry
    ets:update_element(State#state.objs_tid, ObjId, {2, NewAreaSpec}),
    % remove from old area
    do_area_remove_obj(State#state.areas_tid, OldAreaSpec, ObjId),
    {ok, OldAreaSpec, NewAreaSpec}.

do_update_position(State, TreeId, ObjId, NewPos, NewBBSize) ->
    case ets:lookup(State#state.trees_tid, TreeId) of
        [{TreeId, TreeOpts}] -> ok;
        [] -> throw(invalid_key_id), TreeOpts = []
    end,
    NewAreaSpec = make_area_code(TreeId, NewPos, NewBBSize, max_depth_opt(TreeOpts)),        
    case ets:lookup(State#state.objs_tid, ObjId) of
        [{_, OldAreaSpec, _Extra}] when OldAreaSpec == NewAreaSpec ->   
            % ?debugFmt("area has not changed, just report success (~p, ~p)~n", [OldAreaSpec, NewAreaSpec]), 
            {ok, NewAreaSpec};
        
        [{_, OldAreaSpec, _Extra}] -> 
            % ?debugFmt("area has changed, send updates (~p, ~p)~n", [OldAreaSpec, NewAreaSpec]), 
            do_precalc_update_position(State, ObjId, OldAreaSpec, NewAreaSpec);
        
        [] -> throw(invalid_obj_id)
    end.


max_depth_opt(Options) ->
    case opt(max_depth, Options) of
        {ok, MD_Val} ->
            MD_Val;
        _ -> ?DEFAULT_MAX_DEPTH
    end.

opt(Op, [{Op, Value}|_]) ->
    {ok, Value};
opt(Op, [_|Options]) ->
    opt(Op, Options);
opt(_, []) ->
    false.


% compute the path to the area
make_area_code(TreeId, {Px,_,_},{Sx,_,_},_) when (Px + Sx) > 1.0 -> [TreeId];
make_area_code(TreeId, {_,Py,_},{_,Sy,_},_) when (Py + Sy) > 1.0 -> [TreeId];
make_area_code(TreeId, {_,_,Pz},{_,_,Sz},_) when (Pz + Sz) > 1.0 -> [TreeId];
make_area_code(TreeId, Position, BBSize, MaxDepth) -> 
	AreaSpec = make_area_code_step([], {0.0, 0.0, 0.0}, {1.0, 1.0, 1.0}, Position, BBSize, MaxDepth),
    %% cut of the final value, it is an artifact of the termination criterion
    [_Final|CorrectSpec] = AreaSpec,
    [TreeId|lists:reverse(CorrectSpec)].


%% Tree deeper than allowed? -> break
make_area_code_step(AreaSpec, _MinPos, _MaxPos, _ObjPos, _BBSize, ResRest) 
  when ResRest < 0 ->  AreaSpec;

%% if any dimension objpos < minpos -> break
make_area_code_step(AreaSpec, {MinPos1, _, _}, {_MaxPos1, _, _}, {ObjPos1, _, _}, _, _) 
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
        -> {0 | 1 | 2 | 4, float(), float()}.
calc_border(MinPos, MaxPos, ObjPos, BBLen, BitMult) ->
    Center = ((MaxPos - MinPos) / 2) + MinPos,
%	?debugFmt("MinPos: ~p, MaxPos: ~p, Center: ~p, ObjPos: ~p, ObjPos+BBLen: ~p, BitMult: ~p~n", 
%						[MinPos, MaxPos, Center, ObjPos, (ObjPos+BBLen), BitMult]),
    if
        (Center > ObjPos), (Center > (ObjPos + BBLen)) -> 
            {0, MinPos, Center};
        true -> {BitMult, Center, MaxPos}
    end.

%% this is neither fast nor can it be distributed, but it will work for now.
make_new_id(TabId) -> ets:foldl(fun id_max/2, 0, TabId) + 1.

id_max({Id, _}, Curr) -> max(Id, Curr);
id_max({Id, _, _}, Curr) -> max(Id, Curr).

do_make_tree(TreesTId, Options) ->
    NewId = make_new_id(TreesTId),
    ets:insert(TreesTId, {NewId, Options}),
    NewId.

do_make_obj(ObjsTId, AreaSpec) ->
    NewId = make_new_id(ObjsTId),
    ets:insert(ObjsTId, {NewId, AreaSpec, []}),
    NewId.

%%
%% @doc remove single object entry from the list of objects in area.
%%    will throw invalid_area if the AreaSpec cannot be found.
%%
%% @throws invalid_area
%% @end
-spec do_area_remove_obj(AreasTId::integer(), AreasSpec::area_spec(), ObjId::pos_integer()) ->
					true.
do_area_remove_obj(AreasTId, AreaSpec, ObjId) ->
    case ets:lookup(AreasTId, AreaSpec) of
        [{AreaSpec, ObjList, _Subscribers}] ->
            ets:update_element(AreasTId, AreaSpec, {2, lists:delete(ObjId, ObjList)}),
            true;
        [] -> throw({error, invalid_spec})
    end.
    
do_area_add_obj(AreasTId, AreaSpec, ObjId) ->
		case ets:lookup(AreasTId, AreaSpec) of
				[] ->                                 ObjsToWrite = [ObjId], SubsToWrite = [];
            
				[{AreaSpec, ObjList, Subscribers}] -> ObjsToWrite = [ObjId|ObjList], SubsToWrite = Subscribers;
            
				Ret ->                                ?debugFmt("multiple Entries: ~p~n", [Ret]), 
							                          ObjsToWrite = SubsToWrite = [],
							                          throw(multiple_area_entries)
		end,
    ets:insert(AreasTId, {AreaSpec, ObjsToWrite, SubsToWrite}).

