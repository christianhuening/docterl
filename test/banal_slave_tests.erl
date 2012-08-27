%% Author: sage
%% Created: 26.08.2012
%% Description: TODO: Add description to banal_slave_tests
-module(banal_slave_tests).

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
