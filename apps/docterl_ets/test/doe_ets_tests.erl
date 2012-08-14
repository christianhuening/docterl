-module(doe_ets_tests).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").

-export([do_update/5]).


do_make_area_test({Position, BBSize, Expected}) -> 
			Ret = doe_ets:make_area_code(1, Position, BBSize, 10),
%			?debugFmt("generated : ~p~n", [Ret]).
			?assertEqual(Expected, Ret),
			true.

make_area_test_() -> 
    { "test the computation of the area spec",
      { setup,
        fun fixStart/0,
        fun fixStop/1,
        fun(_Foo) -> [
                      ?_test(
                      begin
                          TestCases = [
                                       {{0.1, 0.1, 0.1}, 
                                        {0.01, 0.01, 0.01}, 
                                        [1,0,0,0,7,7]},
                                       {{0.1, 0.1, 0.1}, 
                                        {0.16, 0.16, 0.16}, 
                                        [1,0]},
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
                          lists:all(fun do_make_area_test/1, TestCases)
                      end)
                     ] end }}.

make_new_treeid_test_() -> 
    { "test lookup of tree ids in table",
      { setup,
        fun fixStart/0,
        fun fixStop/1,
        fun(_Foo) -> [
                      ?_test(
                      begin
                          TreesTId = ets:new(trees, [set, {read_concurrency, true}]),
													% ?debugMsg("do first make_new_id"),
                          ?assertEqual(1, doe_ets:make_new_id(TreesTId)),
                          doe_ets:do_make_tree(TreesTId, []),
                          ?assertEqual(2, doe_ets:make_new_id(TreesTId)),
                          doe_ets:do_make_tree(TreesTId, []),
                          ?assertEqual(3, doe_ets:make_new_id(TreesTId)),
                          ets:delete(TreesTId)										
                      end)
                     ] end }}.


make_new_new_test_() -> 
{ "test lookup of new ids in table",
  { setup,
    fun fixStart/0,
    fun fixStop/1,
    fun(_Foo) -> [
                  ?_test(
                  begin
                      ObjsTId = ets:new(objs, [set]),
                      ?assertEqual(1, doe_ets:make_new_id(ObjsTId)),
                      doe_ets:do_make_obj(ObjsTId, [1]),
                      ?assertEqual(2, doe_ets:make_new_id(ObjsTId)),
                      doe_ets:do_make_obj(ObjsTId, [1]),
                      ?assertEqual(3, doe_ets:make_new_id(ObjsTId)),
                      ets:delete(ObjsTId)                                       
                  end)
                 ] end }}.

create_and_remove_obj_test_() -> 
    {"create and remove an object",
     { setup,
       fun fixStartServer/0,
       fun fixStopServer/1,
       fun(_Foo) -> [
                     ?_test(
                     begin
                         {ok, TreeId} = doe_ets:new_tree([]),
                         {ok, ObjId, _Spec1} = doe_ets:add_obj(TreeId, {0.1, 0.1, 0.1}, {0.1, 0.1, 0.1}),
                         % ?debugFmt("Spec1: ~p~n", [Spec1]),
                         _Spec2 = doe_ets:update_position(TreeId, ObjId, {0.1, 0.2, 0.3}, {0.1, 0.1, 0.1}),
                         % ?debugFmt("Spec2: ~p~n", [Spec2]),
                         _Spec3 = doe_ets:update_position(TreeId, ObjId, {0.1, 0.2, 0.3}, {0.1, 0.2, 0.3}),
                         % ?debugFmt("Spec3: ~p~n", [Spec3]),
                         doe_ets:remove_obj(TreeId, ObjId),
                         doe_ets:stop()
                     end)
                    ] end }}.

run_a_thousand_updates_test_() ->
		{"run update multiple times and measure runtim",
     { setup,
       fun fixStartServer/0,
       fun fixStopServer/1,
       fun(_Foo) -> [
                     ?_test(
                     begin
                         {ok, TreeId} = doe_ets:new_tree([]),
                         {ok, ObjId, _} = doe_ets:add_obj(TreeId, {0.1, 0.1, 0.1}, {0.1, 0.1, 0.1}),
                         test_avg(doe_ets_tests, 
                                  do_update, 
                                  [TreeId, ObjId, {0.1, 0.1, 0.1}, {0.1, 0.1, 0.1}], 
                                  10000),
                         doe_ets:remove_obj(TreeId, ObjId)
                     end)
                    ] end }}.

run_a_thousand_different_updates_test_() ->
        {"run update multiple times and measure runtim",
     { setup,
       fun fixStartServer/0,
       fun fixStopServer/1,
       fun(_Foo) -> [
                     ?_test(
                     begin
                         {ok, TreeId} = doe_ets:new_tree([]),
                         {ok, ObjId, _} = doe_ets:add_obj(TreeId, {0.1, 0.1, 0.1}, {0.01, 0.01, 0.01}),
                         test_avg(doe_ets_tests, 
                                  do_update, 
                                  [TreeId, ObjId, {0.1, 0.1, 0.1}, {0.01, 0.01, 0.01}], 
                                  10000),
                         doe_ets:remove_obj(TreeId, ObjId)
                     end)
                    ] end }}.

do_update(Count, TreeId, ObjId, NewPos, NewSize) ->
		doe_ets:update_position(TreeId, ObjId, vec_inc(NewPos, 0.00002 * Count), NewSize).

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


fixStart() ->
%% 	application:start(sasl),
  ok.

fixStop(_Pid) ->
%% 	application:stop(sasl),
	ok.

fixStartServer() ->
%%  application:start(sasl),
    _StartRet = doe_ets:start_link(),
    % ?debugFmt("StartRet: ~p~n", [StartRet]),
  ok.

fixStopServer(_Pid) ->
%%  application:stop(sasl),
    doe_ets:stop(),
    ok.

%% sleep for number of miliseconds
%% sleep(T) ->
%% 	receive 
%% 		after T -> ok 
%% 	end.
