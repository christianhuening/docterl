-module(doe_event_mgr_tests).

-include_lib("eunit/include/eunit.hrl").

info_test_() -> 
    { setup, fun() -> ok end, 
      fun() -> ?debugFmt("~n############################################~n      starting ~p~n############################################~n  ", [?MODULE]) end }.


event_new_tree_test_() -> 
    { "test the transmission of the proper events: 1. create tree",
      { setup,
        fun fixStart/0,
        fun fixStop/1,
        fun(_Foo) -> [
                      ?_test(
                      begin
                          doe_event_mgr:add_handler(doe_test_handler, []),
                          % execute some actions, the event will capture them
                          {ok, TreeId} = docterl_ets:new_tree([]),
                          sleep(100),
                          ?assertEqual({local_new_tree, TreeId, []}, doe_test_handler:get_last_event())
                      end)
                     ] end }}.

event_new_obj_test_() -> 
    { "test the transmission of the proper events: 2. new obj",
      { setup,
        fun fixStart/0,
        fun fixStop/1,
        fun(_Foo) -> [
                      ?_test(
                      begin
                          doe_event_mgr:add_handler(doe_test_handler, []),
                          % execute some actions, the event will capture them
                          {ok, TreeId} = docterl_ets:new_tree([]),
                          doe_event_mgr:subscribe([TreeId, 0, 0]),
                          {ok, ObjId, [TreeId, 0, 0]} = docterl_ets:add_obj(TreeId, {0.1, 0.1, 0.1}, {0.1, 0.1, 0.1}),
                          sleep(100),
                          ?assertEqual({local_new_obj, ObjId, [TreeId, 0, 0], []}, doe_test_handler:get_last_event())
                      end)
                     ] end }}.

event_update_area_test_() -> 
    { "test the transmission of the proper events: 3. update obj position (same area)",
      { setup,
        fun fixStart/0,
        fun fixStop/1,
        fun(_Foo) -> [
                      ?_test(
                      begin
                          OldPos = {0.1, 0.1, 0.1},
                          NewPos = {0.101, 0.101, 0.101},
                          Size = {0.1, 0.1, 0.1},
                          doe_event_mgr:add_handler(doe_test_handler, []),
                          % execute some actions, the event will capture them
                          {ok, TreeId} = docterl_ets:new_tree([]),
                          {ok, ObjId, [TreeId, 0, 0]} = docterl_ets:add_obj(TreeId, OldPos, Size),
                          _Ret = docterl_ets:update_position(TreeId, ObjId, NewPos, Size),
%%                           ?debugFmt("update_position returned: ~p~n", [Ret]),
                          sleep(100),
                          ?assertEqual({local_update_position, ObjId, [TreeId, 0, 0], NewPos, Size}, 
                                       doe_test_handler:get_last_event())
                      end)
                     ] end }}.

event_update_multi_area_test_() -> 
    { "test the transmission of the proper events: 4. update obj position, with area change",
      { setup,
        fun fixStart/0,
        fun fixStop/1,
        fun(_Foo) -> [
                      ?_test(
                      begin
                          OldPos = {0.1, 0.1, 0.1},
                          NewPos = {0.1, 0.1, 0.26},
                          Size = {0.1, 0.1, 0.1},
                          doe_event_mgr:add_handler(doe_test_handler, []),
                          % execute some actions, the event will capture them
                          {ok, TreeId} = docterl_ets:new_tree([]),
                          {ok, ObjId, [TreeId, 0, 0]} = docterl_ets:add_obj(TreeId, OldPos, Size),
                          _Ret = docterl_ets:update_position(TreeId, ObjId, NewPos, Size),
%%                           ?debugFmt("update_position returned: ~p~n", [Ret]),
                          sleep(100),
                          ?assertEqual({local_update_position, ObjId, [TreeId, 0, 4], NewPos, Size}, 
                                       doe_test_handler:get_last_event())
                      end)
                     ] end }}.

fixStart() ->
%%     application:start(sasl),
    doe_ets:start_link(),
    doe_event_mgr:start_link(),    
    ok.

fixStop(_Pid) ->
%%     application:stop(sasl),
    doe_ets:stop(),
    doe_event_mgr:stop(),
    ok.

%% sleep for number of miliseconds
sleep(T) ->
	receive 
		after T -> ok 
	end.
