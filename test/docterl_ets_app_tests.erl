-module(docterl_ets_app_tests).

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
                      ?_test(
                      begin
                          ?assertEqual(ok, application:start(docterl_ets)),
                          {ok, TreeId} = docterl_ets:new_tree([]),
                          {ok, _ObjId, _AreaSpec} = docterl_ets:add_obj(TreeId, {0.1, 0.1, 0.1}, {0.1, 0.1, 0.1})
                      end)
                     ] end }}.


% sleep for number of miliseconds
sleep(T) ->
 receive 
   after T -> ok 
 end.
