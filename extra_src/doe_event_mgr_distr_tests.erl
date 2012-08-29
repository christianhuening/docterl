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
              application:start(docterl_ets),
              Contacts
            end,
      fun(_Nodes) ->
              application:stop(docterl_ets)
      end,
      fun(Nodes) -> [
                 ?_test(test_basic_setup(Nodes)),
                 ?_test(test_local_subscribe(Nodes)),
                 ?_test(test_local_add_obj(Nodes))]
        end
      }.

test_basic_setup(Nodes) ->
    ?debugFmt("I am running on node ~p, connected to ~p. Setup for use in doe are: ~p~n", 
              [node(), nodes(), Nodes]).


test_local_subscribe(Remotes) ->
    Node = node(),
    {ok, TreeId} = docterl_ets:new_tree(),
%%     ?debugFmt("done new_tree: ~p~n", [TreeId]),

    AreaSpec = [TreeId],
    doe_event_mgr:subscribe(AreaSpec),
%%     ?assertEqual([Node], doe_ets:get_subscribers(AreaSpec)),
    lists:map(fun(Remote) -> checkRemotes(Remote, AreaSpec, [Node]) end, Remotes).

checkRemotes(Remote, AreaSpec, Subscribers) -> 
     case (catch rpc:call(Remote, doe_ets, get_subscribers, [AreaSpec])) of
         RemoteSubs when is_list(RemoteSubs) -> 
             ?assertEqual(Subscribers, RemoteSubs);         
         {badrpc, nodedown} -> 
             ?debugFmt("node down: ~p~n", [Remote]),
             ?assert(false);             
         Unknown -> 
             ?debugFmt("recieved unknown error: ~p~n",[Unknown]),
             ?assert(false)
     end.

%
% have a remote node subscribe to this doe_ets, than create a new tree and a new object localy
% 
test_local_add_obj(Remotes) ->
    Position = {0.1, 0.1, 0.1},
    Position2 = {0.1, 0.1, 0.1000001},
    BBSize = {0.16, 0.16, 0.16},
    AreaSpecRem = [0],
    [Remote|_Rest] = Remotes,
    
    {ok, TreeId} = docterl_ets:new_tree(),
    case (catch rpc:call(Remote, doe_ets, subscribe, [[TreeId|AreaSpecRem], node()])) of
        ok -> ok;               
        {badrpc, nodedown} -> 
            ?debugFmt("node down: ~p~n", [Remote]),
            ?assert(false);             
        Unknown -> 
            ?debugFmt("recieved unknown error: ~p~n",[Unknown]),
            ?assert(false)
    end,
    case (catch gen_event:add_handler(doe_event_mgr, doe_test_handler, [])) of
        ok -> ok;
        Unknown2 -> 
            ?debugFmt("recieved unknown error: ~p for node~p~n",[Unknown2, Remote]),
            ?assert(false)                  
    end,
    docterl_ets:add_obj(TreeId, Position, BBSize),
    sleep(100),
    ?assertMatch({local_new_obj, 1, [TreeId, 0], []}, doe_test_handler:get_last_event()),
    case (catch rpc:call(Remote, docterl_ets, add_obj, [TreeId, Position2, BBSize])) of
        {ok, NewObjId, AreaSpec2} -> 
            ?debugFmt("return: ~p~n", [{ok,NewObjId,AreaSpec2}]),
            sleep(100),
            ?debugFmt("local handlers: ~p~n", [gen_event:which_handlers(doe_event_mgr)]),
            ?debugFmt("remote handlers: ~p~n", [gen_event:which_handlers({doe_event_mgr, Remote})]),
            ?assertMatch({remote_add_obj, NewObjId, [TreeId, 0], []}, doe_test_handler:get_last_event());               
        {badrpc, nodedown} -> 
            ?debugFmt("node down: ~p~n", [Remote]),
            ?assert(false);             
        Unknown3 -> 
            ?debugFmt("recieved unknown error: ~p~n",[Unknown3]),
            ?assert(false)
    end,    
    ok.

%% sleep for number of miliseconds
sleep(T) ->
    receive 
        after T -> ok 
    end.


split_node(Node) when is_atom(Node) ->
   split_node(atom_to_list(Node), []).
split_node([], UseAsHost )    -> { [], UseAsHost };
split_node([ $@ | T ], Node ) -> { Node, T };
split_node([ H | T ], Node )  -> split_node(T,  Node ++ [H] ).

