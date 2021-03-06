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
                                       {{0.0000001, 0.0000001, 0.0000001}, 
                                        {0.000001, 0.000001, 0.000001}, 
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
                          ?assertEqual(1, octree_ets:make_tree_id(TreesTId)),
                          octree_ets:do_make_tree(TreesTId, []),
                          ?assertEqual(2, octree_ets:make_tree_id(TreesTId)),
                          octree_ets:do_make_tree(TreesTId, []),
                          ?assertEqual(3, octree_ets:make_tree_id(TreesTId)),
                          ets:delete(TreesTId)										
                      end)
                     ] end }}.


make_new_objid_test_() -> 
{ "test lookup of obj ids in table",
  { setup,
    fun fixStart/0,
    fun fixStop/1,
    fun(_Foo) -> [
                  ?_test(
                  begin
                      ObjsTId = ets:new(objs, [set]),
                      ?assertEqual(1, octree_ets:make_obj_id(ObjsTId)),
                      octree_ets:do_make_obj(ObjsTId, [1]),
                      ?assertEqual(2, octree_ets:make_obj_id(ObjsTId)),
                      octree_ets:do_make_obj(ObjsTId, [1]),
                      ?assertEqual(3, octree_ets:make_obj_id(ObjsTId)),
                      ets:delete(ObjsTId)                                       
                  end)
                 ] end }}.

create_and_remove_obj_test_() -> 
    {"create and remove an object",
     { setup,
       fun fixStart/0,
       fun fixStop/1,
       fun(_Foo) -> [
                     ?_test(
                     begin
                         StartRet = octree_ets:start_link(),
                         ?debugFmt("StartRet: ~p~n", [StartRet]),
                         TreeId = octree_ets:make_tree([]),
                         {ok, ObjId, Spec1} = octree_ets:add_obj(TreeId, {0.1, 0.1, 0.1}, {0.1, 0.1, 0.1}),
                         ?debugFmt("Spec1: ~p~n", [Spec1]),
                         Spec2 = octree_ets:update_position(TreeId, ObjId, {0.1, 0.2, 0.3}, {0.1, 0.1, 0.1}),
                         ?debugFmt("Spec2: ~p~n", [Spec2]),
                         Spec3 = octree_ets:update_position(TreeId, ObjId, {0.1, 0.2, 0.3}, {0.1, 0.2, 0.3}),
                         ?debugFmt("Spec3: ~p~n", [Spec3]),
                         octree_ets:remove_obj(TreeId, ObjId)
                     end)
                    ] end }}.

run_a_thousand_updates_test_() ->
		{"run update multiple times and measure runtim",
     { setup,
       fun fixStart/0,
       fun fixStop/1,
       fun(_Foo) -> [
                     ?_test(
                     begin
                         StartRet = octree_ets:start_link(),
                         ?debugFmt("StartRet: ~p~n", [StartRet]),
                         TreeId = octree_ets:make_tree([]),
                         {ok, ObjId, Spec1} = octree_ets:add_obj(TreeId, {0.1, 0.1, 0.1}, {0.1, 0.1, 0.1}),
												 do_update(TreeId, ObjId, {0.1, 0.1, 0.1}, {0.1, 0.1, 0.1}, now(), 1000),
                         octree_ets:remove_obj(TreeId, ObjId)
                     end)
                    ] end }}.

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
