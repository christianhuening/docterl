%% Author: sage
%% Created: 14.08.2012
%% Description: TODO: Add description to docterl_ets_tests
-module(docterl_ets_tests).

-include_lib("eunit/include/eunit.hrl").

info_test_() -> 
    { setup, fun() -> ok end, 
      fun() -> ?debugFmt("~n############################################~n      starting ~p~n############################################~n  ", [?MODULE]) end }.

basic_api_test_() ->
    { "test the api functions of the docter_ets module",
      setup,
      fun() -> 
              %%  application:start(sasl),
              doe_ets:start_link(),
              doe_event_mgr:start_link(),    
              ok
      end,
      fun(_Args) ->
              %%  application:stop(sasl),
              doe_ets:stop(),
              doe_event_mgr:stop(),
              sleep(100),
              ok
      end,
      fun(_Args) -> [?_test(test_new_tree()),
                     ?_test(test_new_obj()),
                     ?_test(test_remove_obj()),
                     ?_test(test_set_get_extra())] 
              end
      }.

test_new_tree() -> 
    ?assertMatch({ok, _TreeId}, docterl_ets:new_tree([])).

test_new_obj() -> 
    Pos = {0.1, 0.1, 0.1},
    Size = {0.1, 0.1, 0.1},
    {ok, TreeId} = docterl_ets:new_tree([]),
    {ok, ObjId, AreaSpec} = docterl_ets:add_obj(TreeId, Pos, Size),
    ?assertMatch({ok, AreaSpec}, docterl_ets:get_obj(ObjId)).


test_remove_obj() -> 
    {ok, TreeId} = docterl_ets:new_tree([]),
    {ok, ObjId, AreaSpec} = docterl_ets:add_obj(TreeId, {0.1, 0.1, 0.1}, {0.1, 0.1, 0.1}),
    ?assertMatch({ok, AreaSpec}, docterl_ets:get_obj(ObjId)),
    docterl_ets:remove_obj(ObjId),
    ?assertMatch({error, unknown_id}, docterl_ets:get_obj(ObjId)),
    {ok, Members} = doe_ets:get_members(AreaSpec),
    ?assertNot(lists:member(ObjId, Members)),
    ?debugHere.

test_set_get_extra() ->
    Pos = {0.1, 0.1, 0.1},
    Size = {0.1, 0.1, 0.1},
    {ok, TreeId} = docterl_ets:new_tree(),
    {ok, ObjId, _AreaSpec} = docterl_ets:add_obj(TreeId, Pos, Size),
    docterl_ets:set_extra(ObjId, {some_extras}),
    ?assertEqual({ok, {some_extras}}, docterl_ets:get_extra(ObjId)).


%% sleep for number of miliseconds
sleep(T) ->
 receive 
   after T -> ok 
 end.
