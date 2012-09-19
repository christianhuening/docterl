%% Author: sage
%% Created: 18.09.2012
%% Description: TODO: Add description to test_slave_starter
-module(test_slave_starter).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start/0, start/1, stop/1, start_on_all/2]).

%%
%% API Functions
%%

start() ->
    NodeDefs = [{ubuntu, slave1}, {ubuntu, slave2}],
    start(NodeDefs).

start(NodeDefs) ->
    Args = "-rsh ssh",
    StartResults = lists:map(fun({Host, Name}) -> 
                                     StartResult = slave:start_link(Host, Name, Args),
                                     case StartResult of
                                         {ok, Node} ->
                                             {{Host, Name}, {ok, Node}};
                                         {error, {already_running, Node}} ->
                                             {{Host, Name}, {ok, Node}};
                                         {error, Reason} ->
                                             {{Host, Name}, {error, Reason}}     
                                     end                                             
                             end, 
                             NodeDefs),
    RunningNodes = 
        lists:filter(fun({{Host, Name}, {Result, NodeOrReason}}) ->
                             case Result of
                                 ok -> true;
                                 error -> 
                                     io:format("starting node ~s@~s failed: ~p", 
                                               [Name, Host, NodeOrReason]),
                                     false
                             end
                     end , StartResults),
    lists:map(fun({_HostInfo, {ok, Node}}) -> Node end, RunningNodes).
    

stop(Nodes) -> 
    lists:map(fun(Node) -> slave:stop(Node) end, Nodes).

start_on_all(Nodes, App) ->
    
    rpc:call(Node, Module, Function, Args, Timeout)


%%
%% Local Functions
%%

