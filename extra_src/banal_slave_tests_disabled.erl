%% Author: sage
%% Created: 26.08.2012
%% Description: TODO: Add description to banal_slave_tests
-module(banal_slave_tests_disabled).

-include_lib("eunit/include/eunit.hrl").

info_test_() -> 
    { setup, fun() -> ok end, 
      fun() -> ?debugFmt("~n############################################~n      starting ~p~n############################################~n  ", [?MODULE]) end }.


slave_test_() ->
    {"simple slave test",
     node, 'foo@127.0.0.1',
%%      node, foo,
          fun (Node) ->
              [
               ?_assertMatch(pong, net_adm:ping(Node)),
               ?_assertMatch("olleh",
                             rpc:call(Node, lists, reverse, ["hello"]))
               ]
     end
    }.

start_node_fixture_test_() ->
    { Node, Host } = split_node(node()),
    S  = Node ++ "_slave",
    RN = erlang:list_to_atom( S ++ "@" ++ Host),
    ?debugFmt("trying to run on ~p~n", [RN]),
    
    {"slave start by hand",
     foreach,
     fun distr_setup/0,
     fun distr_cleanup/1,
     [?_test(start_node_test_1)
      ]}.

start_node_test_setup() ->
     io:format(user, "setup is on: ~p~n", [ node() ]),
     erlang:module_info(doe_event_mgr_tests),
     application:load(docterl_ets).

start_node_test_cleanup(_) ->
     io:format(user, "cleanup is on: ~p~n", [ node() ]).

start_node_test_1(Node) ->
     io:format(user, "Where does this go: ~p~n", [ node() ]).

split_node(Node) when is_atom(Node) ->
   split_node(atom_to_list(Node), []).
split_node([], UseAsHost )    -> { [], UseAsHost };
split_node([ $@ | T ], Node ) -> { Node, T };
split_node([ H | T ], Node )  -> split_node(T,  Node ++ [H] ).

distr_setup() ->
%%     erlang:set_cookie(node(),eunit),
    Host = list_to_atom(inet_db:gethostname()),
    Args = " -pa "++hd(code:get_path())++" -setcookie eunit",
    {ok,N1} = slave:start(Host,n1,Args),
    {ok,N2} = slave:start(Host,n2,Args),
    rpc:call(N1,net_kernel,connect_node,[N2]),
    [N1,N2].

distr_cleanup([N1,N2]) ->
    slave:stop(N1),
    slave:stop(N2).

