-module(octree_ets_tests).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").

do_make_area_test({Position, BBSize, Expected}) -> 
			Ret = octree_ets:make_area_code(1, Position, BBSize),
%			?debugFmt("generated : ~p~n", [Ret]).
			?assertEqual(Expected, Ret),
			true.

make_area_test_() -> { "test the computation of the area spec",
					    { setup,
					      fun fixStart/0,
					      fun fixStop/1,
						  fun(_Foo) -> [
										?_test(
										begin
												TestCases = [
																		 {{0.1, 0.1, 0.1}, {0.01, 0.01, 0.01}, [1,0,0,0,7,7]},
																		 {{0.1, 0.1, 0.1}, {0.16, 0.16, 0.16}, [1,0]},
																		 {{0.1, 0.2, 0.3}, {0.01, 0.01, 0.01}, [1,0,4,2,3,5]},
																		 {{0.01, 0.01, 0.01}, {0.5, 0.5, 0.5}, [1]},
																		 {{0.01, 0.01, 0.01}, {0.1, 0.2, 0.3}, [1,0]}
																		 ],
												lists:all(fun do_make_area_test/1, TestCases)
										end)
									   ] end }}.

make_new_treeid_test_() -> { "test lookup of tree ids in table",
					    { setup,
					      fun fixStart/0,
					      fun fixStop/1,
						  fun(_Foo) -> [
										?_test(
										begin
												ets:new(trees, [set, named_table, {read_concurrency, true}]),
												?assertEqual(1, octree_ets:make_tree_id()),
												octree_ets:do_make_tree([]),
												?assertEqual(2, octree_ets:make_tree_id()),
												octree_ets:do_make_tree([]),
												?assertEqual(3, octree_ets:make_tree_id()),
												ets:delete(trees)										
										end)
									   ] end }}.


fixStart() ->
%% 	application:start(sasl),
  ok.

fixStop(_Pid) ->
%% 	application:stop(sasl),
	ok.
					
%% sleep for number of miliseconds
sleep(T) ->
	receive 
		after T -> ok 
	end.
