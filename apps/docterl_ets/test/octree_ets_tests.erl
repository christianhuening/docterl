-module(octree_ets_tests).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").

do_make_area_test({Position, BBSize, Expected}) -> 
			Ret = octree_ets:make_area_code(1, Position, BBSize, 10),
%			?debugFmt("generated : ~p~n", [Ret]).
			?assertEqual(Expected, Ret),
			true.


do_update(_, _, _, _, {MegaSecs, Secs, MicroSecs}, 0) ->
    {NowMegaSecs, NowSecs, NowMicroSecs} = now(),
		?debugFmt("1000 updates took ~p~n", [{NowMegaSecs - MegaSecs, NowSecs - Secs, NowMicroSecs - MicroSecs}]), 
		ok;
		
do_update(TreeId, ObjId, NewPos, NewSize, StartTime, Remainder) ->
		octree_ets:update_position(TreeId, ObjId, vec_inc(NewPos, 0.0001), NewSize),
		do_update(TreeId, ObjId, NewPos, NewSize, StartTime, Remainder-1).

vec_inc({Vec1, Vec2, Vec3}, Inc) ->
		{Vec1+Inc,Vec2+Inc,Vec3+Inc}.

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
