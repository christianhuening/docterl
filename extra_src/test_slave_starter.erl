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
-export([start/0, start/2, stop/1, start_app_on_all/2, run_on/4]).

%%
%% API Functions
%%

start() ->
    NodeNames = [slave1, slave2],
    {_, Host} = split_node(node()),
    NodeDefs = lists:map(fun(Name) -> {Host, Name} end, NodeNames),
    Args = "-rsh ssh -pa " ++ hd(code:get_path()),  % set search path to same as this one
    start(NodeDefs, Args).

start(NodeDefs, Args) ->
    StartResults = 
        lists:map(fun({Host, Name}) -> 
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

-spec start_app_on_all(Nodes::[node()], App::atom()) -> ok.
start_app_on_all(Nodes, App) ->
    lists:map(fun(Node) ->
                      Result = rpc:call(Node, application, start, [App]),
                      {Node, Result}
                      end, Nodes).

    
run_on(Node, M, F, A) -> 
    rpc:call(Node, M, F, A).



%%
%% Local Functions
%%
split_node(Node) when is_atom(Node) ->
   split_node(atom_to_list(Node), []).
split_node([], UseAsHost )    -> { [], UseAsHost };
split_node([ $@ | T ], Node ) -> { Node, T };
split_node([ H | T ], Node )  -> split_node(T,  Node ++ [H] ).


