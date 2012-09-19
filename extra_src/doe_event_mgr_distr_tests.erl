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
                 ?_test(test_local_add_obj(Nodes)),
                 ?_test(test_local_subscribe(Nodes))]
        end
      }.

test_basic_setup(Nodes) ->
    ?debugFmt("I am running on node ~p, connected to ~p. Setup for use in doe are: ~p", 
              [node(), nodes(), Nodes]).


test_local_subscribe(Remotes) ->
    Node = node(),
    {ok, TreeId} = docterl_ets:new_tree(),
%%     ?debugFmt("done new_tree: ~p~n", [TreeId]),

    AreaSpec = [TreeId],
    doe_event_mgr:subscribe(AreaSpec),
%%     ?assertEqual([Node], doe_ets:get_subscribers(AreaSpec)),
    lists:map(fun(Remote) -> checkRemoteSubscribers(Remote, AreaSpec, [Node]) end, Remotes).

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
% have a remote node subscribe to this doe_ets, than create a new object localy
% 
test_local_add_obj(Remotes) ->
    Position = {0.1, 0.1, 0.1},
    SlavePositions = [{0.1, 0.1, 0.1000001}, {0.1, 0.1, 0.1000002}],
    BBSize = {0.16, 0.16, 0.16},
    AreaSpecRem = [0],
    
    {ok, TreeId} = docterl_ets:new_tree(),
    
    %% TODO: this code should be run on a differing area spec, as to not interfere with the object
    %%       and tree creation.
%%     lists:map(fun(Remote) ->
%%                       case (catch rpc:call(Remote, doe_ets, subscribe, [[TreeId|AreaSpecRem], node()])) of
%%                           ok -> ok;               
%%                           {badrpc, nodedown} -> 
%%                               ?debugFmt("node down: ~p~n", [Remote]),
%%                               ?assert(false);             
%%                           Unknown -> 
%%                               ?debugFmt("recieved unknown error: ~p~n",[Unknown]),
%%                               ?assert(false)
%%                       end
%%               end, Remotes),
%%     
%%     %% verfify subscription
%%     lists:map(fun(Remote) -> checkRemotes(Remote, [TreeId|AreaSpecRem], [node()]) end, Remotes),

    %%     %% verfify tree id
    lists:map(fun(Remote) -> checkRemoteTree(Remote, TreeId) end, Remotes),

    
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
    ?assertMatch({local_add_obj, 1, [TreeId|AreaSpecRem], []}, doe_test_handler:get_last_event()),
    
    %% check remote
    lists:map(fun(Node) ->
                      ?assertMatch({remote_add_obj, 1, [TreeId|AreaSpecRem], []}, 
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

