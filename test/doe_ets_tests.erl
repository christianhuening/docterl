-module(doe_ets_tests).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").

-export([do_update/5]).

info_test_() -> 
    { setup, fun() -> ok end, 
      fun() -> ?debugFmt("~n############################################~n      starting ~p~n############################################~n  ", [?MODULE]) end }.


internal_funcs_test_() -> 
    {setup,
     fun() ->
%%              application:start(sasl),
             ok
     end,
     fun(_PId) ->
%%              application:stop(sasl),
             ok
     end,
     fun(PId) -> 
             [?_test(test_start_stop(PId)),
              ?_test(test_make_area(PId)),
              ?_test(test_make_new_treeid(PId)),
              ?_test(test_make_new_obj_id(PId))
             ]
     end 
    }.

internal_API_test_() -> 
    {setup,
     fun() ->
             %% application:start(sasl),
             _StartRet = doe_ets:start_link(),
             %% ?debugFmt("StartRet: ~p~n", [StartRet]),
             ok
     end,
     fun(_PId) ->
             %% application:stop(sasl),
             doe_ets:stop(),
             sleep(100),
             ok
     end,
     fun(_Args) -> [?_test(test_create_and_remove_obj()),
                   ?_test(test_create_and_get_obj())]              
     end 
    }.

simple_benchmark_test_() -> 
    {setup,
     fun() ->
             %% application:start(sasl),
             _StartRet = doe_ets:start_link(),
             %% ?debugFmt("StartRet: ~p~n", [StartRet]),
             ok
     end,
     fun(_PId) ->
             %% application:stop(sasl),
             doe_ets:stop(),
             sleep(100),
             ok
     end,
     fun(Args) -> [?_test(test_run_a_thousand_updates(Args)),
                   ?_test(test_run_a_thousand_different_updates(Args))]              
     end 
    }.

%% ====================================================================
%% Test API functions
%% ====================================================================

test_start_stop(_Foo) ->
    ?debugMsg("starting start_link_test"),
    case doe_ets:start_link() of
        {ok, _Pid} -> ok;
        Failure -> ?debugFmt("starting doe_ets failed: ~p~n", [Failure]), 
                   ?assert(false)
    end,
    Ret = (catch doe_ets:stop()),
    ?debugFmt("stop returned: ~p", [Ret]),
    sleep(100),
    ?assertNot(lists:member(doe_ets, erlang:registered())).


%% ====================================================================
%% Test internal functions
%% ====================================================================

test_make_area(_Args) ->
    ?debugMsg("starting make_area_test"),
    TestCases = [
                 {{0.000001, 0.000001, 0.000001}, 
                  {0.9999, 0.9999, 0.9999}, 
                  [1]},
                 {{0.1, 0.1, 0.1}, 
                  {0.16, 0.16, 0.16}, 
                  [1,0]},
                 {{0.5, 0.5, 0.5}, 
                  {0.26, 0.26, 0.26}, 
                  [1,7]},
                 {{0.1, 0.1, 0.1}, 
                  {0.01, 0.01, 0.01}, 
                  [1,0,0,0,7,7]},
                 {{0.1, 0.2, 0.3}, 
                  {0.01, 0.01, 0.01}, 
                  [1,0,4,2,3,5]},
                 {{0.01, 0.01, 0.01}, 
                  {0.5, 0.5, 0.5}, 
                  [1]},
                 {{1.0e-7, 1.0e-7, 1.0e-7}, 
                  {1.0e-6, 1.0e-6, 1.0e-6}, 
                  [1,0,0,0,0,0,0,0,0,0,0]},
                 {{0.01, 0.01, 0.01}, {0.1, 0.2, 0.3}, [1,0]}
                ],
    lists:all(fun do_make_area_test/1, TestCases).

    
test_make_new_treeid(_PId) -> 
    ?debugMsg("starting make_new_treeid_test"),
    TreesTId = ets:new(trees, [set, {read_concurrency, true}]),
    % ?debugMsg("do first make_new_id"),
    ?assertEqual(1, doe_ets:make_new_id(TreesTId)),
    doe_ets:do_make_tree(TreesTId, []),
    ?assertEqual(2, doe_ets:make_new_id(TreesTId)),
    doe_ets:do_make_tree(TreesTId, []),
    ?assertEqual(3, doe_ets:make_new_id(TreesTId)),
    ets:delete(TreesTId).


test_make_new_obj_id(_PId) -> 
    ?debugMsg("starting make_new_new test"),
    ObjsTId = ets:new(objs, [set]),
    ?assertEqual(1, doe_ets:make_new_id(ObjsTId)),
    doe_ets:do_make_obj(ObjsTId, [1]),
    ?assertEqual(2, doe_ets:make_new_id(ObjsTId)),
    doe_ets:do_make_obj(ObjsTId, [1]),
    ?assertEqual(3, doe_ets:make_new_id(ObjsTId)),
    ets:delete(ObjsTId).


test_create_and_remove_obj() -> 
    ?debugMsg("starting create_and_remove_obj test"),
    {ok, TreeId} = doe_ets:new_tree([]),
    {ok, ObjId, _Spec1} = doe_ets:new_obj(TreeId, {0.1, 0.1, 0.1}, {0.1, 0.1, 0.1}),
    % ?debugFmt("Spec1: ~p~n", [Spec1]),
    _Spec2 = doe_ets:update_position(TreeId, ObjId, {0.1, 0.2, 0.3}, {0.1, 0.1, 0.1}),
    % ?debugFmt("Spec2: ~p~n", [Spec2]),
    Spec3 = doe_ets:update_position(TreeId, ObjId, {0.1, 0.2, 0.3}, {0.1, 0.2, 0.3}),
    ?debugFmt("Spec3: ~p~n", [Spec3]),
    doe_ets:remove_obj(ObjId),
    ?debugMsg("end create_and_remove_obj_test").

test_create_and_get_obj() ->
    ?debugMsg("starting create_and_remove_obj test"),
    {ok, TreeId} = doe_ets:new_tree([]),
    {ok, ObjId, AreaSpec} = doe_ets:new_obj(TreeId, {0.5, 0.5, 0.5}, {0.1, 0.1, 0.1}),
    ?assertMatch({ok, _List}, doe_ets:get_members(AreaSpec)),
    {ok, Members} = doe_ets:get_members(AreaSpec),
    ?assert(lists:member(ObjId, Members)).


test_run_a_thousand_updates(_PId) ->
    ?debugMsg("starting run_a_throusand_updates_test"),
    {ok, TreeId} = doe_ets:new_tree([]),
    {ok, ObjId, _} = doe_ets:new_obj(TreeId, {0.1, 0.1, 0.1}, {0.1, 0.1, 0.1}),
    test_avg(doe_ets_tests, 
             do_update, 
             [TreeId, ObjId, {0.1, 0.1, 0.1}, {0.1, 0.1, 0.1}], 
             10000),
    doe_ets:remove_obj(ObjId).


test_run_a_thousand_different_updates(_PId) ->
    ?debugMsg("starting run_a_thousand_different_updates_test"),
    {ok, TreeId} = doe_ets:new_tree([]),
    {ok, ObjId, _} = doe_ets:new_obj(TreeId, {0.1, 0.1, 0.1}, {0.01, 0.01, 0.01}),
    test_avg(doe_ets_tests, 
             do_update, 
             [TreeId, ObjId, {0.1, 0.1, 0.1}, {0.01, 0.01, 0.01}], 
             10000),
    doe_ets:remove_obj(ObjId).


%% ====================================================================
%% utility functions for tests
%% ====================================================================

do_make_area_test({Position, BBSize, Expected}) -> 
      Ret = doe_ets:make_area_code(1, Position, BBSize, 10),
%     ?debugFmt("generated : ~p~n", [Ret]).
      ?assertEqual(Expected, Ret),
      true.

do_update(Count, TreeId, ObjId, NewPos, NewSize) ->
		doe_ets:update_position(TreeId, ObjId, vec_inc(NewPos, 2.0e-5 * Count), NewSize).

vec_inc({Vec1, Vec2, Vec3}, Inc) ->
		{Vec1+Inc,Vec2+Inc,Vec3+Inc}.

test_avg(M, F, A, N) when N > 0 ->
    L = test_loop(M, F, A, N, []),
    Length = length(L),
    Min = lists:min(L),
    Max = lists:max(L),
    Med = lists:nth(round((Length / 2)), lists:sort(L)),
    Avg = round(lists:foldl(fun(X, Sum) -> X + Sum end, 0, L) / Length),
    ?debugFmt("Range: ~b - ~b mics~n"
          "Median: ~b mics~n"
          "Average: ~b mics~n",
          [Min, Max, Med, Avg]),
    io:format("Range: ~b - ~b mics~n"
          "Median: ~b mics~n"
          "Average: ~b mics~n",
          [Min, Max, Med, Avg]),
    Med.

test_loop(_M, _F, _A, 0, List) ->
    List;
test_loop(M, F, A, N, List) ->
    {T, _Result} = timer:tc(M, F, [N|A]),
    test_loop(M, F, A, N - 1, [T|List]).

%% sleep for number of miliseconds
sleep(T) ->
	receive 
		after T -> ok 
	end.
