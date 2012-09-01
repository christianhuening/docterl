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
                     ?_test(test_set_get_extra()),
                     ?_test(test_add_handler())] 
              end
      }.

utility_funcs_test_() ->
    { "test the utility functions for navigating the octree",
      setup,
      fun() ->
              ok
              end,
      fun(_Args) ->
              ok
              end,
      fun(_Args) -> [?_test(test_root()),
                     ?_test(test_is_root()),
                     ?_test(test_children()),
                     ?_test(test_parent())]
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
    ?assertNot(lists:member(ObjId, Members)).

test_set_get_extra() ->
    Pos = {0.1, 0.1, 0.1},
    Size = {0.1, 0.1, 0.1},
    {ok, TreeId} = docterl_ets:new_tree(),
    {ok, ObjId, _AreaSpec} = docterl_ets:add_obj(TreeId, Pos, Size),
    docterl_ets:set_extra(ObjId, {some_extras}),
    ?assertEqual({ok, {some_extras}}, docterl_ets:get_extra(ObjId)).


test_add_handler() -> 
    ?assertMatch(ok, docterl_ets:add_handler(doe_dummy_handler)),
    ?assert(lists:member(doe_dummy_handler, gen_event:which_handlers(doe_event_mgr))).


test_root() ->
    ?assertThrow(invalid_area, docterl_ets:root([])),
    ?assertThrow(invalid_area, docterl_ets:root(atom)),
    ?assertEqual([1], catch docterl_ets:root([1])),
    ?assertEqual([1], catch docterl_ets:root([1,2,3,4,5,6])).

test_is_root() ->
    ?assertThrow(invalid_area, docterl_ets:is_root([])),
    ?assertThrow(invalid_area, docterl_ets:is_root(atom)),
    ?assertEqual(true, docterl_ets:is_root([1])),
    ?assertEqual(false, docterl_ets:is_root([1,2])),
    ?assertEqual(false, docterl_ets:is_root([1,2,3,4,5,6])).

test_children() ->
    ?assertMatch([[1,0,0],
                  [1,0,1],
                  [1,0,2],
                  [1,0,3],
                  [1,0,4],
                  [1,0,5],
                  [1,0,6],
                  [1,0,7]], catch docterl_ets:children([1,0])).

test_parent() ->
    ?assertEqual(invalid_area, catch docterl_ets:parent([])),
    ?assertEqual(invalid_area, catch docterl_ets:parent(atom)),
    ?assertEqual([1], catch docterl_ets:parent([1])),
    ?assertEqual([1,1], catch docterl_ets:parent([1,1,1])),
    ?assertEqual([1,2,4], catch docterl_ets:parent([1,2,4,6])).

%% sleep for number of miliseconds
sleep(T) ->
 receive 
   after T -> ok 
 end.
