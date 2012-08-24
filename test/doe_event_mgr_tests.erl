-module(doe_event_mgr_tests).

-include_lib("eunit/include/eunit.hrl").

info_test_() -> 
    { setup, fun() -> ok end, 
      fun() -> ?debugFmt("~n############################################~n      starting ~p~n############################################~n  ", [?MODULE]) end }.


event_mgt_test_() -> 
    { "test the transmission of the proper events",
      setup,
      
      fun() ->
              %%     application:start(sasl),
              doe_ets:start_link(),
              doe_event_mgr:start_link(),    
              ok
      end,
      
      fun(_Args) ->
              %%     application:stop(sasl),
              doe_ets:stop(),
              doe_event_mgr:stop(),
              ok
      end,
      fun(_Foo) -> [
                    ?_test(test_event_new_tree()),
                    ?_test(test_event_new_obj()),
                    ?_test(test_event_update_area()),
                    ?_test(test_event_update_multi_area())
                   ] 
      end }.

%% multi_node_event_test_() ->
%%     { "test functions on multiple nodes.",
%%       inparralel 
%%     [{setup,
%%       {spawn 'server1@flar.informatik.haw-hamburg.de'},
%%      {}]
%%       }.

% test the transmission of the proper events: 1. new tree",
test_event_new_tree() -> 
    doe_event_mgr:add_handler(doe_test_handler, []),
    % execute some actions, the event will capture them
    {ok, TreeId} = docterl_ets:new_tree([]),
    sleep(100),
    ?assertEqual({local_new_tree, TreeId, []}, doe_test_handler:get_last_event()).

% test the transmission of the proper events: 2. new obj
test_event_new_obj() -> 
    doe_event_mgr:add_handler(doe_test_handler, []),
    % execute some actions, the event will capture them
    {ok, TreeId} = docterl_ets:new_tree([]),
    doe_event_mgr:subscribe([TreeId, 0, 0]),
    {ok, ObjId, [TreeId, 0, 0]} = docterl_ets:add_obj(TreeId, {0.1, 0.1, 0.1}, {0.1, 0.1, 0.1}),
    sleep(100),
    ?assertEqual({local_new_obj, ObjId, [TreeId, 0, 0], []}, doe_test_handler:get_last_event()).

% test the transmission of the proper events: 3. update obj position (same area)
test_event_update_area() -> 
    OldPos = {0.1, 0.1, 0.1},
    NewPos = {0.101, 0.101, 0.101},
    Size = {0.1, 0.1, 0.1},
    doe_event_mgr:add_handler(doe_test_handler, []),
    % execute some actions, the event will capture them
    {ok, TreeId} = docterl_ets:new_tree([]),
    {ok, ObjId, [TreeId, 0, 0]} = docterl_ets:add_obj(TreeId, OldPos, Size),
    _Ret = docterl_ets:update_position(TreeId, ObjId, NewPos, Size),
%%    ?debugFmt("update_position returned: ~p~n", [Ret]),
    sleep(100),
    ?assertEqual({local_update_position, ObjId, [TreeId, 0, 0], NewPos, Size}, 
                 doe_test_handler:get_last_event()).

% test the transmission of the proper events: 4. update obj position, with area change
test_event_update_multi_area() -> 
    OldPos = {0.1, 0.1, 0.1},
    NewPos = {0.1, 0.1, 0.26},
    Size = {0.1, 0.1, 0.1},
    doe_event_mgr:add_handler(doe_test_handler, []),
    % execute some actions, the event will capture them
    {ok, TreeId} = docterl_ets:new_tree([]),
    {ok, ObjId, [TreeId, 0, 0]} = docterl_ets:add_obj(TreeId, OldPos, Size),
    _Ret = docterl_ets:update_position(TreeId, ObjId, NewPos, Size),
%%     ?debugFmt("update_position returned: ~p~n", [Ret]),
    sleep(100),
    ?assertEqual({local_update_position, ObjId, [TreeId, 0, 4], NewPos, Size}, 
                 doe_test_handler:get_last_event()).

test_local_subscribe() ->
    Node = node(),
    ?debugFmt("node: ~p~n", [Node]),
    TreeId = docterl_ets:new_tree(),
    AreaSpec = [TreeId],
    doe_event_mgr:subscribe(AreaSpec),
    ?assertEqual([Node], gen_server:call(doe_ets, {get_subscribers, AreaSpec})).

%% sleep for number of miliseconds
sleep(T) ->
	receive 
		after T -> ok 
	end.


%
% Mulit-Node Test
%

%% start_node_fixture_test_() ->
%%     { Node, Host } = split_node(node()),
%%     S  = Node ++ "_slave",
%%     RN = erlang:list_to_atom( S ++ "@" ++ Host),
%%     ?debugFmt("trying to run on ~p~n", [RN]),
%%      {node, RN, fun(_) -> [
%%                            ?_test(start_node_test_1())
%%                            ]
%%       end }.

%%     {node, RN,
%%     { setup, 
%% %%       { spawn, RN },    
%%       fun start_node_test_setup/0,
%%       fun start_node_test_cleanup/1,
%%       [
%%        % w/out the list eunit doesn't even try to run test_1 on remote node? weird.
%%        fun start_node_test_1/0
%%       ]
%%     }}.

start_node_test_setup() ->
     io:format(user, "setup is on: ~p~n", [ node() ]).

start_node_test_cleanup(_) ->
     io:format(user, "cleanup is on: ~p~n", [ node() ]).

start_node_test_1() ->
     io:format(user, "Where does this go: ~p~n", [ node() ]).

split_node(Node) when is_atom(Node) ->
   split_node(atom_to_list(Node), []).
split_node([], UseAsHost )    -> { [], UseAsHost };
split_node([ $@ | T ], Node ) -> { Node, T };
split_node([ H | T ], Node )  -> split_node(T,  Node ++ [H] ).

