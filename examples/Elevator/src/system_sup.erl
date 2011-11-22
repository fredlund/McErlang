%%%----------------------------------------------------------------------
%%% File    : system_sup.erl
%%% Author  : Håkan Huss <hakan@erlang.ericsson.se>
%%% Purpose : Supervisor for the elevator system.
%%% Created : 30 Aug 1999 by Håkan Huss <hakan@erlang.ericsson.se>
%%%----------------------------------------------------------------------

-module(system_sup).
-author('hakan@erlang.ericsson.se').
-vsn("1.0").

-behaviour(supervisor).

%% External exports
-export([start_link/2]).

%% supervisor callbacks
-export([init/1]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link(NElevs, EventHandlers) ->
    supervisor:start_link({local, system_sup}, system_sup,
			  [NElevs, EventHandlers]).

%%%----------------------------------------------------------------------
%%% Callback functions from supervisor
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% init([NElevs, EventHandlers])
%%  Starts up the elevator control system with NElevs elevators.
%%  EventHandlers is a list of system event handlers that
%%  should be installed when sys_event starts up.
%%----------------------------------------------------------------------
init([NElevs, EventHandlers]) ->
    {ok,{{one_for_one, 4, 1000},
	 [{scheduler, {scheduler, start_link,[]},
          permanent, 1000, worker, [scheduler]},
	  {sys_event, {sys_event, start_link, [EventHandlers]},
          permanent, 1000, worker, [sys_event]},
	  {elev_sup,  {elev_sup, start_link, [NElevs]},
          permanent, infinity, supervisor, elev_sup}]}}.
