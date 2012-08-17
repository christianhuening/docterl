-module(docterl_ets_app_tests).

-include_lib("eunit/include/eunit.hrl").

info_test_() -> 
    { setup, fun() -> ok end, 
      fun() -> ?debugFmt("~n############################################~n      starting ~p~n############################################~n  ", [?MODULE]) end }.

start_app_test_() -> 
    { "start the application",
      { setup,
        fun() ->
                %% application:start(sasl),
                ok
        end, 
        fun(_Pid) ->
                application:stop(docterl_ets),
                %% application:stop(sasl),
                sleep(100),
                ok
        end,
        fun(_Foo) -> [
                      ?_test(
                      begin
                          ?assertEqual(ok, application:start(docterl_ets)),
                          {ok, TreeId} = docterl_ets:new_tree([]),
                          {ok, _ObjId, _AreaSpec} = docterl_ets:add_obj(TreeId, {0.1, 0.1, 0.1}, {0.1, 0.1, 0.1})
                      end)
                     ] end }}.

start_node_fixture_test_() ->
    {Node, Host} = split_node(node()),    
    S  = Node ++ "_slave",
    RN = erlang:list_to_atom( S ++ "@" ++ Host),

    { node, RN, { setup,    
                  { spawn, RN }, 
                  fun  start_node_test_setup/0,
                  fun start_node_test_cleanup/1,
                  [
                   % w/out the list eunit doesn't run test_1 on remote node?
                   fun start_node_test_1/0
                  ]
                }}.

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

% sleep for number of miliseconds
sleep(T) ->
 receive 
   after T -> ok 
 end.
