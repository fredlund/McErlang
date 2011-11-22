%%%----------------------------------------------------------------------
%%% File    : sys_event.erl
%%% Author  : Håkan Huss <hakan@erlang.ericsson.se>
%%% Purpose : The system event manager.
%%% Created :  3 Aug 1999 by Håkan Huss <hakan@erlang.ericsson.se>
%%%----------------------------------------------------------------------

-module(sys_event).
-author('hakan@erlang.ericsson.se').
-vsn("1.0").

%% External exports
-export([start_link/1, add_handler/2]).
-export([initialized/3, open/1, close/1, move/2, stopping/1,
	 approaching/2, stopped_at/2, passing/2,
	 f_button_pressed/1, e_button_pressed/2,
	 controller_started/2]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link(Handlers) ->
    Ret = gen_event:start_link({local, sys_event}),
    lists:foreach(fun({Name, Arg}) -> add_handler(Name, Arg) end, Handlers),
    Ret.

add_handler(Module, Args) ->
    gen_event:add_handler(sys_event, Module, Args).

%%----------------------------------------------------------------------
%% initialized(ENo, State, Floor)
%%  An elevator has been initialized.
%%----------------------------------------------------------------------
initialized(ENo, State, Floor) ->
%%    elevator_at_floor(ENo,Floor),
    gen_event:notify(sys_event, {reset, ENo, State, Floor}).

%%----------------------------------------------------------------------
%% open(ENo)
%%  The doors of an elevator have opened.
%%----------------------------------------------------------------------
open(ENo) ->
%%    elevator_moving(ENo),
    gen_event:notify(sys_event, {open, ENo}).

%%----------------------------------------------------------------------
%% close(ENo)
%%  The doors of an elevator have closed.
%%----------------------------------------------------------------------
close(ENo) ->
    gen_event:notify(sys_event, {close, ENo}).

%%----------------------------------------------------------------------
%% move(ENo, Dir)
%%  An elevator has started moving in the direction Dir.
%%
%% Types:
%%  Dir = up | down
%%----------------------------------------------------------------------
move(ENo, Dir) ->
    gen_event:notify(sys_event, {move, ENo, Dir}).

%%----------------------------------------------------------------------
%% stop(ENo)
%%  An elevator will stop at the next floor.
%%----------------------------------------------------------------------
stopping(ENo) ->
    gen_event:notify(sys_event, {stopping, ENo}).

%%----------------------------------------------------------------------
%% approaching(ENo, Floor)
%%  An elevator is nearing a floor.
%%----------------------------------------------------------------------
approaching(ENo, Floor) ->
    gen_event:notify(sys_event, {approaching, ENo, Floor}).

%%----------------------------------------------------------------------
%% stopped_at(ENo, Floor)
%%  An elevator has stopped at a floor.
%%----------------------------------------------------------------------
stopped_at(ENo, Floor) ->
%%    elevator_at_floor(ENo,Floor),
    gen_event:notify(sys_event, {stopped_at, ENo, Floor}).


%%----------------------------------------------------------------------
%% passing(ENo, Floor)
%%  An elevator is passing a floor.
%%----------------------------------------------------------------------
passing(ENo, Floor) ->
    gen_event:notify(sys_event, {passing, ENo, Floor}).

%%----------------------------------------------------------------------
%% e_button_pressed(ENo, Floor)
%%  A floor button in an elevator has been pressed.
%%----------------------------------------------------------------------
e_button_pressed(ENo, Floor) ->
    gen_event:notify(sys_event, {e_button, ENo, Floor}).

%%----------------------------------------------------------------------
%% f_button_pressed(Floor)
%%  A call button has been pressed on a floor.
%%----------------------------------------------------------------------
f_button_pressed(Floor) ->
    gen_event:notify(sys_event, {f_button, Floor}).

%%----------------------------------------------------------------------
%% controller_started(ENo, EPid)
%%  An elevator control process has been started (or restarted).
%%----------------------------------------------------------------------
controller_started(ENo, EPid) ->
    gen_event:notify(sys_event, {controller_started, ENo, EPid}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       
      
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%elevator_at_floor(ENo,Floor) ->
%%  mce_erl:probe_state
%%    ({stopped_at,Floor},
%%     ordsets:add_element(ENo,at_floor(Floor))),
%%  mce_erl:probe_state({last_floor,ENo},Floor).
%%
%%elevator_moving(ENo) ->
%%  Floor = mce_erl:get_probe_state({last_floor,ENo}),
%%  mce_erl:probe_state
%%    ({stopped_at,Floor},ordsets:del_element(ENo,at_floor(Floor))).
%%
%%at_floor(Floor) ->
%%  case mce_erl:get_probe_state({stopped_at,Floor}) of
%%    undefined -> ordsets:new();
%%    Other -> Other
%%  end.
