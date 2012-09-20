%% Author: sage
%% Created: 27.08.2012
%% Description: TODO: Add description to doe_event_mgr_distr_tests
%% 
%% For this module to run, an additional node with the name 'server1' has to be started beforehand
%%
 -module(doe_event_mgr_distr_tests).

%%
%% The following tests need to be done:
%%
%% - add a node to a cluster, that allready contains a number of trees 
%%   (currently adding nodes is not supported)
%%
%% 
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
              Nodes = test_slave_starter:start(),
              test_slave_starter:start_app_on_all(Nodes, docterl_ets),             
              application:start(docterl_ets),
              Nodes
            end,
      fun(Nodes) ->
              application:stop(docterl_ets),
              test_slave_starter:stop(Nodes)
      end,
      fun(Nodes) -> [
                 ?_test(test_basic_setup(Nodes)),
                 ?_test(test_create_tree(Nodes)),
                 ?_test(test_local_add_obj(Nodes)),
                 ?_test(test_event_handling(Nodes)),
                 ?_test(test_local_subscribe(Nodes))]
        end
      }.

test_basic_setup(Nodes) ->
    ?debugFmt("I am running on node ~p, connected to ~p. Setup for use in doe are: ~p", 
              [node(), nodes(), Nodes]).

test_create_tree(Nodes) ->
    {ok, TreeId} = docterl_ets:new_tree(),
    lists:map(fun(Remote) -> checkRemoteTree(Remote, TreeId) end, Nodes),
%%     ?debugFmt("tree Id transported successfully to all nodes: ~p~n", [TreeId]),
    ok.


%%
%% TODO: currently this tests nothing of value.
%%
%% the subscribtions are suppressed, as the areas are not 
%% populated and thus subscriptions are ignored. 
%% 
test_local_subscribe(_Remotes) ->
    _Node = node(),
    {ok, TreeId} = docterl_ets:new_tree(),
%%     ?debugFmt("done new_tree: ~p~n", [TreeId]),

    AreaSpec = [TreeId],
    
    doe_event_mgr:subscribe(AreaSpec),
%%     ?assertEqual([Node], doe_ets:get_subscribers(AreaSpec)),
%%     lists:map(fun(Remote) -> checkRemoteSubscribers(Remote, AreaSpec, [Node]) end, Remotes),
    ok.

checkRemoteSubscribers(Remote, AreaSpec, Subscribers) ->
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

checkRemoteTree(Remote, TreeId) -> 
     case (catch rpc:call(Remote, doe_ets, get_tree_ids, [])) of
         RemoteTreeIds when is_list(RemoteTreeIds) -> 
             ?assert(lists:member(TreeId, RemoteTreeIds));         
         {badrpc, nodedown} -> 
             ?debugFmt("node down: ~p~n", [Remote]),
             ?assert(false);             
         Unknown -> 
             ?debugFmt("recieved unknown error: ~p~n",[Unknown]),
             ?assert(false)
     end.

%
% add remote objects to a given area, than add locally, to see if objects are correctly transfered
% 
test_local_add_obj(Remotes) ->
    Position = {0.1, 0.1, 0.1},
    SlavePositions = [{0.1, 0.1, 0.1000001}, {0.1, 0.1, 0.1000002}],
    BBSize = {0.16, 0.16, 0.16},
    AreaSpecRem = [0],
    
    {ok, TreeId} = docterl_ets:new_tree(),
    TestArea = [TreeId|AreaSpecRem],
    
    %% add the observer objects to the other nodes.
    RemoteObjIds = 
        lists:map(fun({Node, Position}) -> 
                          {ok, ObjId, AreaSpec} = 
                              rpc:call(Node, docterl_ets, add_obj, [TreeId, Position, BBSize]),
                          ?assertEqual(TestArea, AreaSpec),
                          ObjId
                  end, 
                  lists:zip(Remotes, SlavePositions)),
    
    sleep(100),
        
    {ok, LocalObjId, _AreaSpec} = docterl_ets:add_obj(TreeId, Position, BBSize),
    
    ?debugFmt("the object ids are: ~p (TODO: they should be unique)",[[LocalObjId|RemoteObjIds]]),
    ?assertMatch([_A, _B, _C], [LocalObjId|RemoteObjIds]),
    
    sleep(1000),

   ?assertMatch({ok, [_A, _B, _C]},docterl_ets:get_members(TestArea)),

    {ok, RetrievedObjIds} = docterl_ets:get_members(TestArea),
    
    ?assertEqual([], lists:subtract([LocalObjId|RemoteObjIds], 
                                    RetrievedObjIds)),
    ?debugMsg("objects successully transferred"),
    ok.

test_event_handling(Remotes) ->
    Position = {0.1, 0.1, 0.1},
    SlavePositions = [{0.1, 0.1, 0.1000001}, {0.1, 0.1, 0.1000002}],
    BBSize = {0.16, 0.16, 0.16},
    AreaSpecRem = [0],
    
    {ok, TreeId} = docterl_ets:new_tree(),
    TestArea = [TreeId|AreaSpecRem],

    %% add local handler
    case (catch gen_event:add_handler(doe_event_mgr, doe_test_handler, [])) of
        ok -> ok;
        Unknown2 -> 
            ?debugFmt("recieved unknown error: ~p for add ing test handler",[Unknown2]),
            ?assert(false)                  
    end,
    
    %% add remote handlers
    lists:map(fun(Node) -> 
                      case (catch gen_event:add_handler({doe_event_mgr, Node}, doe_test_handler, [])) of
                          ok -> ok;
                          Unknown3 -> 
                              ?debugFmt("recieved unknown error: ~p for add ing test handler on node ~p",
                                        [Unknown3, Node]),
                              ?assert(false)                  
                      end
              end, Remotes),
    
    ?assertMatch({ok, _ObjID, _AreaSpec}, docterl_ets:add_obj(TreeId, Position, BBSize)),
    sleep(100),

    %% add remote objects
    lists:map(fun({Node, Pos}) ->
                        rpc:call(Node, docterl_ets, add_obj, [TreeId, Pos, BBSize])                      
                      end, 
              lists:zip(Remotes, SlavePositions)),

    %% check local
    ?assertMatch({local_add_obj, 1, TestArea, []}, doe_test_handler:get_last_event()),
    
    %% check remote
    lists:map(fun(Node) ->
                      ?assertMatch({remote_add_obj, 1, TestArea, []}, 
                                   doe_test_handler:get_last_event(Node))
                      end, Remotes),
    
    %% check that the events arrived on the remotes
%%     case (catch rpc:call(Remote, docterl_ets, add_obj, [TreeId, Position2, BBSize])) of
%%         {ok, NewObjId, AreaSpec2} -> 
%%             ?debugFmt("return: ~p~n", [{ok,NewObjId,AreaSpec2}]),
%%             sleep(100),
%%             ?debugFmt("local handlers: ~p~n", [gen_event:which_handlers(doe_event_mgr)]),
%%             ?debugFmt("remote handlers: ~p~n", [gen_event:which_handlers({doe_event_mgr, Remote})]),
%%             ?assertMatch({remote_add_obj, NewObjId, [TreeId, 0], []}, doe_test_handler:get_last_event());               
%%         {badrpc, nodedown} -> 
%%             ?debugFmt("node down: ~p~n", [Remote]),
%%             ?assert(false);             
%%         Unknown3 -> 
%%             ?debugFmt("recieved unknown error: ~p~n",[Unknown3]),
%%             ?assert(false)
%%     end,    
    ok.

%% sleep for number of miliseconds
sleep(T) ->
    receive 
        after T -> ok 
    end.

