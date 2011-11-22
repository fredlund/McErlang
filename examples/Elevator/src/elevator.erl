%%%----------------------------------------------------------------------
%%% File    : elevator.erl
%%% Author  : Håkan Huss <hakan@erlang.ericsson.se>
%%% Purpose : Elevator interface functions for elevator exercise
%%% Created :  3 Aug 1999 by Håkan Huss <hakan@erlang.ericsson.se>
%%%----------------------------------------------------------------------

-module(elevator).
-author('hakan@erlang.ericsson.se').
-vsn("1.0").

-behaviour(gen_fsm).

%% External exports
-export([start_link/1]).
-export([reset/3, move/2, stop/1, open/1, close/1,
	 approaching/2, at_floor/2, get_state/1]).

%% gen_fsm callbacks
-export([init/1, handle_sync_event/4, handle_event/3,
	 handle_info/3, terminate/3, code_change/4]).
-export([uninitialized/2, open/2, closed/2, moving/2, stopping/2]).

%%%----------------------------------------------------------------------
%%% 
%%% The elevator is represented by an FSM with four states:
%%%  open:     Standing at a floor with the doors open
%%%  closed:   Standing at a floor with the doors closed
%%%  moving:   Moving
%%%  stopping: Moving, but will stop at the next floor
%%%
%%% In addition to the state, the FSM has information about its number and
%%% a floor. The floor is the current floor if the elevator is standing
%%% still, otherwise the last floor it passed.
%%%
%%% When the FSM starts, it's in the state uninitialized, where it only reacts
%%% to the event {reset, Floor}, which causes it to enter the state closed.
%%%
%%% Apart from this, the states, events and corresponding actions are:
%%%
%%%
%%%      State   |     open     |    closed    |    moving     |   stopping
%%% Event        |              |              |               |
%%% -------------+--------------+--------------+---------------+--------------
%%% open         |      N/A     | Open doors   |      N/A      |      N/A
%%%              |              | -> open      |               |
%%% -------------+--------------+--------------+---------------+--------------
%%% close        | Close doors  |      N/A     |      N/A      |      N/A
%%%              | Inform sched |              |               |
%%%              | -> closed    |              |               |
%%% -------------+--------------+--------------+---------------+--------------
%%% {move, Dir}  |      N/A     | Start moving |      N/A      |      N/A
%%%              |              | -> moving    |               |
%%% -------------+--------------+--------------+---------------+--------------
%%% {approaching,|      N/A     |      N/A     | Inform sched  |      N/A
%%%  Floor}      |              |              | -> moving     |
%%%              |              |              | -> stopping   |
%%% -------------+--------------+--------------+---------------+--------------
%%% {at, Floor}  |      N/A     |      N/A     | Update floor, | Update floor,
%%%              |              |              | inform sched  | inform sched
%%%              |              |              | -> moving     | -> open
%%%
%%% (sched = scheduler)
%%%
%%% There is also a synchronous event to retrieve the state of an
%%% elevator, get_state, which is handled in all states. It returns
%%% the tuple {State, Floor}.
%%%
%%%----------------------------------------------------------------------

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link(ENo) ->
    gen_fsm:start_link(elevator, [ENo], []).

reset(Elev, State, Floor) ->
    gen_fsm:send_event(Elev, {reset, State, Floor}).

move(Elev, Dir) ->
    gen_fsm:send_event(Elev, {move, Dir}).

stop(Elev) ->
    gen_fsm:send_event(Elev, stop).

open(Elev) ->
    gen_fsm:send_event(Elev, open).

close(Elev) ->
    gen_fsm:send_event(Elev, close).

approaching(Elev, Floor) ->
    gen_fsm:send_event(Elev, {approaching, Floor}).

at_floor(Elev, Floor) ->
    gen_fsm:send_event(Elev, {at_floor, Floor}).

get_state(Elev) ->
    gen_fsm:sync_send_all_state_event(Elev, get_state).


%%%----------------------------------------------------------------------
%%% Callback functions from gen_fsm
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% init([ENo])
%%  ENo = int()
%%
%% Initializes an elevator FSM for elevator number ENo.
%%----------------------------------------------------------------------
init([ENo]) ->
    sys_event:controller_started(ENo, self()),
    {ok, uninitialized, {ENo, unknown_floor}}.

%%----------------------------------------------------------------------
%% State callbacks
%%----------------------------------------------------------------------
uninitialized({reset, State, Floor}, {ENo, unknown_floor}) ->
    scheduler:set_controller(ENo, self()),
    sys_event:initialized(ENo, State, Floor),
    {next_state, State, {ENo, Floor}};
uninitialized(_Other, Data) ->
    {next_state, uninitialized, Data}.

open(close, {ENo, Floor}) ->
    sys_event:close(ENo),
    scheduler:closed(ENo, Floor),
    {next_state, closed, {ENo, Floor}}.

closed(open, {ENo, Floor}) ->
    sys_event:open(ENo),
    timer:apply_after(1000, elevator, close, [self()]),
    {next_state, open, {ENo, Floor}};
closed({move, Dir}, {ENo, Floor}) ->
    sys_event:move(ENo, Dir),
    {next_state, moving, {ENo, Floor}}.

moving({approaching, NewFloor}, {ENo, Floor}) ->
    sys_event:approaching(ENo, NewFloor),
    case scheduler:approaching(ENo, NewFloor) of
	{ok, stop} ->
	    sys_event:stopping(ENo),
	    {next_state, stopping, {ENo, Floor}};
	{ok, continue} ->
	    {next_state, moving, {ENo, Floor}};
	_Other ->
	    sys_event:stopping(ENo),
	    {next_state, stopping, {ENo, Floor}}
    end;
moving({at_floor, NewFloor}, {ENo, Floor}) ->
    scheduler:passing(ENo, NewFloor),
    {next_state, moving, {ENo, NewFloor}}.

stopping({at_floor, NewFloor}, {ENo, Floor}) ->
    sys_event:stopped_at(ENo, NewFloor),
    sys_event:open(ENo),
    scheduler:open(ENo, NewFloor),
    timer:apply_after(1000, elevator, close, [self()]),
    {next_state, open, {ENo, NewFloor}}.

%%----------------------------------------------------------------------
%% The get_state event.
%%----------------------------------------------------------------------
handle_sync_event(get_state, From, State, {ENo, Floor}) ->
    {reply, {ENo, State, Floor}, State, {ENo, Floor}}.

%%----------------------------------------------------------------------
%% No all_state_events should be sent.
%%----------------------------------------------------------------------
handle_event(Event, State, {ENo, Floor}) ->
    {stop, {unknown_event, Event, {ENo, State, Floor}}}.

%%----------------------------------------------------------------------
%% No messages should be sent.
%%----------------------------------------------------------------------
handle_info(Msg, State, {ENo, Floor}) ->
    {stop, {unknown_message, Msg, {ENo, State, Floor}}}.

%%----------------------------------------------------------------------
%% Code change is a no-op (no previous version exists).
%%----------------------------------------------------------------------
code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%----------------------------------------------------------------------
%% Cleanup.
%%----------------------------------------------------------------------
terminate(_Reason, _State, {ENo, _Floor}) ->
    ok.
