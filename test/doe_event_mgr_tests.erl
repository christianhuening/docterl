-module(doe_event_mgr_tests).

-include_lib("eunit/include/eunit.hrl").

info_test_() -> 
    { setup, fun() -> ok end, 
      fun() -> ?debugFmt("~n############################################~n      starting ~p~n############################################~n  ", [?MODULE]) end }.

internal_func_test_() -> 
    { "test internal functions",
      setup,
      
      fun() ->
              %%     application:start(sasl),
              doe_ets:start_link(),
              doe_event_mgr:start_link(),    
              ok
      end,
      
      fun(_Args) ->
              %%     application:stop(sasl),
              doe_ets:stop(),
              doe_event_mgr:stop(),
              ok
      end,
      fun(_Foo) -> [
                    ?_test(test_notify_subs())
                   ] 
      end }.


event_mgt_test_() -> 
    { "test the transmission of the proper events",
      setup,
      
      fun() ->
              %%     application:start(sasl),
              doe_ets:start_link(),
              doe_event_mgr:start_link(),    
              ok
      end,
      
      fun(_Args) ->
              %%     application:stop(sasl),
              doe_ets:stop(),
              doe_event_mgr:stop(),
              ok
      end,
      fun(_Foo) -> [
                    ?_test(test_event_new_tree()),
                    ?_test(test_event_new_obj()),
                    ?_test(test_event_update_area()),
                    ?_test(test_event_update_multi_area())
                   ] 
      end }.

test_notify_subs() ->
    doe_event_mgr:add_handler(doe_test_handler, []),
    doe_event_mgr:notify_subs([node()], {dummy_event}),
    sleep(100),
    ?assertMatch({dummy_event}, doe_test_handler:get_last_event()).


% test the transmission of the proper events: 1. new tree",
test_event_new_tree() -> 
    doe_event_mgr:add_handler(doe_test_handler, []),
    % execute some actions, the event will capture them
    {ok, TreeId} = docterl_ets:new_tree([]),
    sleep(100),
    ?assertEqual({local_new_tree, TreeId, []}, doe_test_handler:get_last_event()).

% test the transmission of the proper events: 2. new obj
test_event_new_obj() -> 
    doe_event_mgr:add_handler(doe_test_handler, []),
    % execute some actions, the event will capture them
    {ok, TreeId} = docterl_ets:new_tree([]),
    doe_event_mgr:subscribe([TreeId, 0, 0]),
    {ok, ObjId, [TreeId, 0, 0]} = docterl_ets:add_obj(TreeId, {0.1, 0.1, 0.1}, {0.1, 0.1, 0.1}),
    sleep(100),
    ?assertEqual({local_add_obj, ObjId, [TreeId, 0, 0], []}, doe_test_handler:get_last_event()).

% test the transmission of the proper events: 3. update obj position (same area)
test_event_update_area() -> 
    OldPos = {0.1, 0.1, 0.1},
    NewPos = {0.101, 0.101, 0.101},
    Size = {0.1, 0.1, 0.1},
    doe_event_mgr:add_handler(doe_test_handler, []),
    % execute some actions, the event will capture them
    {ok, TreeId} = docterl_ets:new_tree([]),
    {ok, ObjId, [TreeId, 0, 0]} = docterl_ets:add_obj(TreeId, OldPos, Size),
    _Ret = docterl_ets:update_position(TreeId, ObjId, NewPos, Size),
%%    ?debugFmt("update_position returned: ~p~n", [Ret]),
    sleep(100),
    ?assertEqual({local_update_position, ObjId, [TreeId, 0, 0], NewPos, Size}, 
                 doe_test_handler:get_last_event()).

% test the transmission of the proper events: 4. update obj position, with area change
test_event_update_multi_area() -> 
    OldPos = {0.1, 0.1, 0.1},
    NewPos = {0.1, 0.1, 0.26},
    Size = {0.1, 0.1, 0.1},
    doe_event_mgr:add_handler(doe_test_handler, []),
    % execute some actions, the event will capture them
    {ok, TreeId} = docterl_ets:new_tree([]),
    {ok, ObjId, [TreeId, 0, 0]} = docterl_ets:add_obj(TreeId, OldPos, Size),
    _Ret = docterl_ets:update_position(TreeId, ObjId, NewPos, Size),
%%     ?debugFmt("update_position returned: ~p~n", [Ret]),
    sleep(100),
    ?assertEqual({local_update_position, ObjId, [TreeId, 0, 4], NewPos, Size}, 
                 doe_test_handler:get_last_event()).

%% sleep for number of miliseconds
sleep(T) ->
	receive 
		after T -> ok 
	end.

