%% Author: sage
%% Created: 27.08.2012
%% Description: TODO: Add description to doe_event_mgr_distr_tests
%% 
%% For this module to run, an additional node with the name 'server1' has to be started beforehand
%%
 -module(doe_event_mgr_distr_tests).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").

info_test_() -> 
    { setup, fun() -> ok end, 
      fun() -> ?debugFmt("~n############################################~n      starting ~p~n############################################~n  ", [?MODULE]) end }.


multi_node_event_test_() ->
    { "test functions on multiple nodes.",
      setup,
      fun() ->
              Nodes = ["server1"],
              {_, Host} = split_node(node()),
              Contacts = lists:map(fun(A) -> list_to_atom(A ++ "@" ++ Host) end, Nodes),
              lists:map(fun net_adm:ping/1, Contacts),
              application:start(docterl_ets)
            end,
      fun(_Nodes) ->
%%               application:stop(docterl_ets)
ok, application:info()
      end,
      fun(Nodes) -> [
                 ?_test(test_basic_setup()),
                 ?_test(test_local_subscribe(Nodes))]
        end
      }.

test_basic_setup() ->
    ?debugFmt("I am running on node ~p, connected to ~p~n", [node(), nodes()]).


test_local_subscribe(Remotes) ->
    Node = node(),
    {ok, TreeId} = docterl_ets:new_tree(),
    ?debugFmt("done new_tree: ~p~n", [TreeId]),

    AreaSpec = [TreeId],
    doe_event_mgr:subscribe(AreaSpec),
%%     ?assertEqual([Node], doe_ets:get_subscribers(AreaSpec)),
    lists:map(fun(Remote) -> checkRemotes(Remote, AreaSpec, [Node]) end, Remotes).

checkRemotes(Remote, AreaSpec, Subscribers) -> 
    {ok, RemoteSubs} = rpc:call(Remote, doe_ets, get_subscribers, [AreaSpec]),
    ?assertEqual(Subscribers, RemoteSubs).



split_node(Node) when is_atom(Node) ->
   split_node(atom_to_list(Node), []).
split_node([], UseAsHost )    -> { [], UseAsHost };
split_node([ $@ | T ], Node ) -> { Node, T };
split_node([ H | T ], Node )  -> split_node(T,  Node ++ [H] ).

