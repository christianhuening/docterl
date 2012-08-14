-module(doe_event_mgr_tests).

-include_lib("eunit/include/eunit.hrl").



event_change_area_test_() -> 
    { "test the transmission of the proper events: 1. change area",
      { setup,
        fun fixStart/0,
        fun fixStop/1,
        fun(_Foo) -> [
                      ?_test(
                      begin
													doe_ets:start_link(),
													doe_event_mgr:start_link(),
													doe_event_mgr:add_handler(doe_test_handler, []),
													% execute some actions, the event will capture them
													{ok, TreeId} = docterl_ets:new_tree([]),
													?assertEqual(doe_test_handler:get_last_event(), {new_tree, TreeId})
                      end)
                     ] end }}.

fixStart() ->
%% 	application:start(sasl),

  ok.

fixStop(_Pid) ->
%% 	application:stop(sasl),
	ok.
					
%% sleep for number of miliseconds
%% sleep(T) ->
%% 	receive 
%% 		after T -> ok 
%% 	end.
