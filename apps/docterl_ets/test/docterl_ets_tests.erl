%% Author: sage
%% Created: 14.08.2012
%% Description: TODO: Add description to docterl_ets_tests
-module(docterl_ets_tests).

-include_lib("eunit/include/eunit.hrl").



new_tree_test_() -> 
    { "create a new tree",
      { setup,
        fun fixStart/0,
        fun fixStop/1,
        fun(_Foo) -> [
                      ?_test(
                      begin
                          {ok, TreeId} = docterl_ets:new_tree([])
                      end)
                     ] end }}.

new_obj_test_() -> 
    { "create a new obj",
      { setup,
        fun fixStart/0,
        fun fixStop/1,
        fun(_Foo) -> [
                      ?_test(
                      begin
                          {ok, TreeId} = docterl_ets:new_tree([]),
                          {ok, _ObjId, _AreaSpec} = docterl_ets:add_obj(TreeId, {0.1, 0.1, 0.1}, {0.1, 0.1, 0.1})
                      end)
                     ] end }}.

remove_obj_test_() -> 
    { "remove an object",
      { setup,
        fun fixStart/0,
        fun fixStop/1,
        fun(_Foo) -> [
                      ?_test(
                      begin
                          {ok, TreeId} = docterl_ets:new_tree([]),
                          {ok, ObjId, AreaSpec} = docterl_ets:add_obj(TreeId, {0.1, 0.1, 0.1}, {0.1, 0.1, 0.1}),
                          docterl_ets:remove_obj(TreeId, ObjId),
                          {ok, Members} = doe_ets:get_members(AreaSpec),
                          ?assertNot(lists:member(ObjId, Members))
                      end)
                     ] end }}.

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
    ok.
