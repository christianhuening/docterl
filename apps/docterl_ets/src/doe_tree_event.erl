%%% -------------------------------------------------------------------
%%% Author  : sage
%%% Description :
%%%
%%% Created : 10.08.2012
%%% -------------------------------------------------------------------
-module(doe_tree_event).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-define(SERVER, ?MODULE).

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0, add_handler/2, delete_handler/2, new_obj/2, update_area/3]).

%% ====================================================================
%% External functions
%% ====================================================================
start_link() -> 
    gen_event:start_link({local, ?SERVER}).


add_handler(Handler, Args) -> 
    gen_event:add_handler(?SERVER, Handler, Args).

delete_handler(Handler, Args) -> 
    gen_event:delete_handler(?SERVER, Handler, Args).


%
% this only sets the initial position. Changes in position
% will be handled by the area update.
%
new_obj(ObjId, AreaSpec) -> 
    gen_event:notify(?SERVER, {new_obj, ObjId, AreaSpec}).

%
% the old content is resent, so that the recipient need not query 
% an addition data source to get it.
%
update_area(AreaSpec, OldContent, NewContent) -> 
    gen_event:notify(?SERVER, {update_area, AreaSpec, OldContent, NewContent}).


%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

