-module(octree_ets_tests).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").

make_area_test_() -> { "test the computation of the area spec",
					    { setup,
					      fun fixStart/0,
					      fun fixStop/1,
						  fun(_Foo) -> [
										?_test(
										begin
											Ret = octree_ets:make_area_code(1, {0.1, 0.1, 0.1}, {0.01, 0.01, 0.01}),
											?debugFmt("generated : ~p~n", [Ret])
										end)
									   ] end }}.


fixStart() ->
	application:start(sasl).

fixStop(_Pid) ->
	application:stop(sasl).
					

%% sleep for number of miliseconds
%% sleep(T) ->
%% 	receive 
%% 		after T -> ok 
%% 	end.
