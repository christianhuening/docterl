-module(doe_id_mgr_tests).

-include_lib("eunit/include/eunit.hrl").

info_test_() -> 
    { setup, fun() -> ok end, 
      fun() -> ?debugFmt("~n############################################~n      starting ~p~n############################################~n  ", [?MODULE]) end }.

start_app_test_() -> 
    { "start the application",
      { setup,
        fun() ->
                %% application:start(sasl),
                ok
        end, 
        fun(_Pid) ->
                application:stop(docterl_ets),
                %% application:stop(sasl),
                sleep(100),
                ok
        end,
        fun(_Foo) -> [
                      ?_test(start_mgr_test()),
                      ?_test(get_tree_id_test()),
                      ?_test(get_obj_id_test())
                     ] end }}.

start_mgr_test() ->
    doe_id_mgr:start_link(),
    ?assertEqual(1, doe_id_mgr:get_block_size()).

get_tree_id_test() -> ok.

get_obj_id_test() -> ok.

% sleep for number of miliseconds
sleep(T) ->
 receive 
   after T -> ok 
 end.
