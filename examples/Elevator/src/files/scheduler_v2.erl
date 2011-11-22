%%%----------------------------------------------------------------------
%%% File    : scheduler.erl
%%% Author  : Håkan Huss <hakan@erlang.ericsson.se>
%%% Purpose : Elevator scheduler.
%%% Created :  4 Aug 1999 by Håkan Huss <hakan@erlang.ericsson.se>
%%%----------------------------------------------------------------------

-module(scheduler).
-author('hakan@erlang.ericsson.se').
-vsn("1.0").

-behaviour(gen_server).

%% External exports
-export([start_link/0]).
-export([set_controller/2, f_button_pressed/1, e_button_pressed/2]).
-export([approaching/2, passing/2, open/2, closed/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(sched_elevator, {number, pid, state, floor, stoplist = []}).

%%----------------------------------------------------------------------
%% start_link()
%%  Starts the scheduler.
%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, scheduler}, scheduler, [], []).

%%----------------------------------------------------------------------
%% set_controller(ENo, EPid)
%%  Informs the scheduler that the control porcess EPid has been 
%%  started for elevator ENo.
%%----------------------------------------------------------------------
set_controller(ENo, EPid) ->
    gen_server:cast(scheduler, {set_controller, ENo, EPid}).

%%----------------------------------------------------------------------
%% f_button_pressed(Floor)
%%  Informs the scheduler that the call button on the floor Floor
%%  has been pressed.
%%----------------------------------------------------------------------
f_button_pressed(Floor) ->
    sys_event:f_button_pressed(Floor),
    gen_server:cast(scheduler, {f_button, Floor}).

%%----------------------------------------------------------------------
%% e_button_pressed(ENo, Floor)
%%  Informs the scheduler that the elevator button for the floor Floor
%%  has been pressed in elevator ENo.
%%----------------------------------------------------------------------
e_button_pressed(ENo, Floor) ->
    sys_event:e_button_pressed(ENo, Floor),
    gen_server:cast(scheduler, {e_button, ENo, Floor}).

%%----------------------------------------------------------------------
%% passing(Floor)
%%  Informs the scheduler that elevator ENo is passing the floor Floor.
%%----------------------------------------------------------------------
passing(ENo, Floor) ->
    gen_server:cast(scheduler, {passing, ENo, Floor}).

%%----------------------------------------------------------------------
%% passing(Floor)
%%  Informs the scheduler that elevator ENo has stopped at the floor Floor.
%%----------------------------------------------------------------------
open(ENo, Floor) ->
    gen_server:cast(scheduler, {open, ENo, Floor}).

%%----------------------------------------------------------------------
%% closed(ENo, Floor)
%%  Informs the scheduler that elevator ENo has closed the doors on floor
%%  Floor.
%%----------------------------------------------------------------------
closed(ENo, Floor) ->
    gen_server:cast(scheduler, {closed, ENo, Floor}).

%%----------------------------------------------------------------------
%% approaching(ENo, Floor)
%%  Informs the scheduler that elevator ENo is approaching the floor Floor,
%%  expects a reply indicating whether or not to stop.
%%  Uses catch to handle the possibility of a timeout.
%%----------------------------------------------------------------------
approaching(ENo, Floor) ->
    catch gen_server:call(scheduler, {approaching, ENo, Floor}, 200).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% init([])
%%  Initializes the scheduler by retrieving the elevator states for all
%%  elevators (if running supervised).
%%----------------------------------------------------------------------
init([]) ->
    Elevs =
	case whereis(elev_sup) of
	    undefined ->
		[];
	    _Pid ->
		lists:map(fun ({ENo, EPid, _, _}) ->
				  {_ENo, State, Floor} =
				      transform(elevator:get_state(EPid)),
				  #sched_elevator{number = ENo,
						  pid = EPid,
						  state = State,
						  floor = Floor}
			  end,
			  supervisor:which_children(elev_sup))
	end,
    {ok, Elevs}.

%%----------------------------------------------------------------------
%% Request for information on whether to stop or not.
%%----------------------------------------------------------------------
handle_call({approaching, ENo, NewFloor}, _From, Elevs) ->
   NewElevs = change_floor(ENo, NewFloor, Elevs),
   case get_stoplist(ENo, Elevs) of
        [NewFloor|_] ->
           {reply, {ok, stop}, NewElevs};
        [] -> 
           {reply, {ok, stop}, NewElevs};
        _ -> 
           {reply, {ok, continue}, NewElevs}
    end.




%%----------------------------------------------------------------------
%% Update contorl process information
%%----------------------------------------------------------------------
handle_cast({set_controller, ENo, EPid}, Elevs) ->
    {_ENo,  State, Floor} = transform(elevator:get_state(EPid)),
    New = #sched_elevator{number = ENo,
			  pid = EPid,
			  state = State,
			  floor = Floor},
    case lists:keysearch(ENo, #sched_elevator.number, Elevs) of
	false ->
	    {noreply, [New | Elevs]};
	{value, #sched_elevator{stoplist = SL}} ->
	    {noreply,
	     lists:keyreplace(ENo, #sched_elevator.number, Elevs,
			      New#sched_elevator{stoplist = SL})}
    end;

%%----------------------------------------------------------------------
%% Call button handling.
%%----------------------------------------------------------------------
handle_cast({f_button, Floor}, Elevs) ->
    NewElevs = schedule_elevator(Floor, Elevs),
    {noreply, NewElevs};

%%----------------------------------------------------------------------
%% Elevator button handling.
%%----------------------------------------------------------------------

handle_cast({e_button, ENo, Floor}, Elevs) ->
    {value, Elev} = lists:keysearch(ENo, #sched_elevator.number, Elevs),
    NewElev = add_stop(Floor, Elev),
    {noreply, lists:keyreplace(ENo, #sched_elevator.number, Elevs, NewElev)};

%%----------------------------------------------------------------------
%% Doors have closed, start moving?
%%----------------------------------------------------------------------
handle_cast({closed, ENo, Floor}, Elevs) ->
    case lists:keysearch(ENo, #sched_elevator.number, Elevs) of
	{value, Elev} when Elev#sched_elevator.state == open,
			   Elev#sched_elevator.stoplist == [] ->
	    {noreply, lists:keyreplace(ENo, #sched_elevator.number, Elevs,
				       Elev#sched_elevator{state = closed})};
	{value, Elev} ->
	    #sched_elevator{pid = EPid, state = open,
			    stoplist = [NextFloor | Rest]} = Elev,
	    elevator:move(EPid, direction(Floor, NextFloor)),
	    {noreply, lists:keyreplace(ENo, #sched_elevator.number, Elevs,
				       Elev#sched_elevator{state = moving})}
    end;

%%----------------------------------------------------------------------
%% Update state.
%%----------------------------------------------------------------------
handle_cast({passing, ENo, NewFloor}, Elevs) ->
    {value, Elev} = lists:keysearch(ENo, #sched_elevator.number, Elevs),
    {noreply, lists:keyreplace(ENo, #sched_elevator.number, Elevs,
			       Elev#sched_elevator{floor = NewFloor})};

handle_cast({open, ENo, NewFloor}, Elevs) ->
    {value, Elev} = lists:keysearch(ENo, #sched_elevator.number, Elevs),
    NewStoplist = case Elev#sched_elevator.stoplist of
		      [NewFloor | Rest] ->	% The first is the normal case,
			  Rest;			% the rest can occur if
		      [] ->			% the scheduler has been
			  [];			% restarted.
		      Other ->
			  Other
		  end,
    {noreply, lists:keyreplace(ENo, #sched_elevator.number, Elevs,
			       Elev#sched_elevator{floor = NewFloor,
						   state = open,
						   stoplist = NewStoplist})}.

%%----------------------------------------------------------------------
%% No messages should be sent.
%%----------------------------------------------------------------------
handle_info(Msg, State) ->
    {stop, {unknown_message, {Msg, State}}, State}.

%%----------------------------------------------------------------------
%% Code change is a no-op (no previous version exists).
%%----------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------------------------
%% Cleanup.
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

transform({ENo, open, Floor}) ->
    {ENo, open, Floor};
transform({ENo, closed, Floor}) ->
    {ENo, closed, Floor};
transform({ENo, stopping, Floor}) ->
    {ENo, moving, Floor};
transform({ENo, moving, Floor}) ->
    {ENo, moving, Floor}.

direction(From, To) when From =< To ->
    up;
direction(From, To) when From > To ->
    down.

get_stoplist(ENo, Elevs) ->
    {value, #sched_elevator{stoplist = SL}} =
	lists:keysearch(ENo, #sched_elevator.number, Elevs),
    SL.

change_floor(ENo, NewFloor, Elevs) ->
    {value, Elev} = lists:keysearch(ENo, #sched_elevator.number, Elevs),
    lists:keyreplace(ENo,
		     #sched_elevator.number,
		     Elevs,
		     Elev#sched_elevator{floor = NewFloor}).

%%----------------------------------------------------------------------
%% add_stop(Floor, Elev)
%%  Make sure the Elev stops at Floor.
%%----------------------------------------------------------------------

%% Elevator is already there and open, ignore request
%%
add_stop(Floor, Elev) when Elev#sched_elevator.floor == Floor,
			   Elev#sched_elevator.state == open ->
    Elev;

%% Elevator is there, but closed, open it.
%%
add_stop(Floor, Elev) when Elev#sched_elevator.floor == Floor,
			   Elev#sched_elevator.state == closed ->
    elevator:open(Elev#sched_elevator.pid),
    Elev#sched_elevator{state = open};

%% Elevator is idle, start it moving.
%%
add_stop(Floor, Elev) when Elev#sched_elevator.state == closed,
			   Elev#sched_elevator.stoplist == [] ->
    elevator:move(Elev#sched_elevator.pid,
		  direction(Elev#sched_elevator.floor, Floor)),
    Elev#sched_elevator{state = moving, stoplist = [Floor]};

%% Otherwise just add to the stoplist.
%%
add_stop(Floor, Elev) ->
    Elev#sched_elevator{stoplist = stoplist:add(Floor,
						Elev#sched_elevator.floor,
						Elev#sched_elevator.stoplist)}.

%%----------------------------------------------------------------------
%% schedule_elevator(Floor, Elevs)
%%  Schedule one of the elevators to go to Floor.
%%----------------------------------------------------------------------
schedule_elevator(Floor, Elevs) ->
    check_open_elevator(Floor, Elevs).

%% Check if there is an open elevator there already. If so, nothing
%% needs to be done.
%%
check_open_elevator(Floor, Elevs) ->
    case lists:any(fun (Elev) when Elev#sched_elevator.floor == Floor,
				   Elev#sched_elevator.state == open ->
			   true;
		       (_) ->
			   false
		   end,
		   Elevs) of
	true -> 
	    Elevs;
	false ->
	    check_closed_elevator(Floor, Elevs)
    end.

%% Check if there is a closed (idle) elevator there. If so, open it.
%% 
check_closed_elevator(Floor, Elevs) ->
    case lists:filter(fun (Elev) when Elev#sched_elevator.floor == Floor,
				      Elev#sched_elevator.state == closed ->
			      true;
			  (_) ->
			      false
		      end,
		      Elevs) of
	[Elev | _Rest] ->
	    elevator:open(Elev#sched_elevator.pid),
	    lists:keyreplace(Elev#sched_elevator.number,
			     #sched_elevator.number,
			     Elevs,
			     Elev#sched_elevator{state = open});
	[] ->
	    check_in_stoplist(Floor, Elevs)
    end.
	
%% Check if any elevator is scheduled to stop there already. If so,
%% nothing is done.
check_in_stoplist(Floor, Elevs) ->
    case lists:any(fun (#sched_elevator{stoplist = SL}) ->
			   lists:member(Floor, SL)
		   end,
		   Elevs) of
	true ->
	    Elevs;
	false ->
	    add_to_a_stoplist(Floor, Elevs)
    end.

%% Find the best elevator and schedule it to go there.
%%
add_to_a_stoplist(Floor, Elevs) ->
    [{_Time, Selected} | Rest] =
	lists:sort(
	  lists:map(fun (Elev) ->
			    {stoplist:time_to(Floor,
					      Elev#sched_elevator.floor,
					      Elev#sched_elevator.stoplist),
			     Elev}
		    end,
		    Elevs)),
    NewState = case Selected#sched_elevator.state of
		   closed ->
		       elevator:move(Selected#sched_elevator.pid,
				     direction(Selected#sched_elevator.floor,
					       Floor)),
		       moving;
		   State ->
		       State
	       end,
    SL = stoplist:add(Floor,
		      Selected#sched_elevator.floor,
		      Selected#sched_elevator.stoplist),
    lists:keyreplace(Selected#sched_elevator.number,
		     #sched_elevator.number,
		     Elevs,
		     Selected#sched_elevator{state = NewState,
					     stoplist = SL}).
