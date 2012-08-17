%% Author: sage
%% Created: 14.08.2012
%% Description: TODO: Add description to docterl_ets_tests
-module(docterl_ets_tests).

-include_lib("eunit/include/eunit.hrl").

info_test_() -> 
    { setup, fun() -> ok end, 
      fun() -> ?debugFmt("~n############################################~n      starting ~p~n############################################~n  ", [?MODULE]) end }.

basic_api_test_() ->
    { "test the api functions of the docter_ets module",
      setup,
      fun() -> 
              %%  application:start(sasl),
              doe_ets:start_link(),
              doe_event_mgr:start_link(),    
              ok
      end,
      fun(_Args) ->
              %%  application:stop(sasl),
              doe_ets:stop(),
              doe_event_mgr:stop(),
              sleep(100),
              ok
      end,
      fun(_Args) -> [?_test(test_new_tree()),
                     ?_test(test_new_obj())] 
              end
      }.

test_new_tree() -> 
    ?assert({ok, _TreeId} = docterl_ets:new_tree([])).

test_new_obj() -> 
    Pos = {0.1, 0.1, 0.1},
    Size = {0.1, 0.1, 0.1},
    {ok, TreeId} = docterl_ets:new_tree([]),
    {ok, ObjId, _AreaSpec} = docterl_ets:add_obj(TreeId, Pos, Size),
    ?assertMatch({ObjId, _}, docterl_ets:get_obj(ObjId)).


%% remove_obj_test_() -> 
%%     { "remove an object",
%%       { setup,
%%         fun fixStart/0,
%%         fun fixStop/1,
%%         fun(_Foo) -> [
%%                       ?_test(
%%                       begin
%%                           {ok, TreeId} = docterl_ets:new_tree([]),
%%                           {ok, ObjId, AreaSpec} = docterl_ets:add_obj(TreeId, {0.1, 0.1, 0.1}, {0.1, 0.1, 0.1}),
%%                           docterl_ets:remove_obj(TreeId, ObjId),
%%                           {ok, Members} = doe_ets:get_members(AreaSpec),
%%                           ?assertNot(lists:member(ObjId, Members))
%%                       end)
%%                      ] end }}.

%% _test_() -> 
%%     { "",
%%       { setup,
%%         fun fixStart/0,
%%         fun fixStop/1,
%%         fun(_Foo) -> [
%%                       ?_test(
%%                       begin
%%                           {ok, TreeId} = docterl_ets:new_tree([]),
%%                           {ok, _ObjId, _AreaSpec} = docterl_ets:add_obj(TreeId, {0.1, 0.1, 0.1}, {0.1, 0.1, 0.1})
%%                       end)
%%                      ] end }}.

fixStart() ->
    %%  application:start(sasl),
    doe_ets:start_link(),
    doe_event_mgr:start_link(),    
    ok.

fixStop(_Pid) ->
    %%  application:stop(sasl),
    doe_ets:stop(),
    doe_event_mgr:stop(),
    sleep(100),
    ok.

%% sleep for number of miliseconds
sleep(T) ->
 receive 
   after T -> ok 
 end.
