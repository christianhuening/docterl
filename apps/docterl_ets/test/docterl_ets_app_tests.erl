-module(docterl_ets_app_tests).

-include_lib("eunit/include/eunit.hrl").



start_app_test_() -> 
    { "start the application",
      { setup,
        fun fixStart/0,
        fun fixStop/1,
        fun(_Foo) -> [
                      ?_test(
                      begin
                          ?assertEqual(ok, application:start(docterl_ets)),
                          {ok, TreeId} = docterl_ets:new_tree([]),
                          {ok, _ObjId, _AreaSpec} = docterl_ets:add_obj(TreeId, {0.1, 0.1, 0.1}, {0.1, 0.1, 0.1})
                      end)
                     ] end }}.

fixStart() ->
    application:start(sasl),
    ok.

fixStop(_Pid) ->
    application:stop(sasl),
    application:stop(docterl_ets),
    ok.

% sleep for number of miliseconds
%% sleep(T) ->
%%  receive 
%%    after T -> ok 
%%  end.
