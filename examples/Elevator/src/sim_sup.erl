%%%----------------------------------------------------------------------
%%% File    : sim_sup.erl
%%% Author  : Håkan Huss <hakan@erlang.ericsson.se>
%%% Purpose : Top level supervisor for the elevator simulator system.
%%% Created : 30 Aug 1999 by Håkan Huss <hakan@erlang.ericsson.se>
%%%----------------------------------------------------------------------

-module(sim_sup).
-author('hakan@erlang.ericsson.se').
-vsn("1.0").

-behaviour(supervisor).

%% External exports
-export([start_link/3]).

%% supervisor callbacks
-export([init/1]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link(InitFloor, NFloors, NElevs) ->
    supervisor:start_link({local, sim_sup}, sim_sup,
			  [NElevs, [{display, [InitFloor, NFloors, NElevs]}]]).

%%%----------------------------------------------------------------------
%%% Callback functions from supervisor
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% init([NElevs, EventHandlers])
%%  Starts up the graphics supervisor and the system supervisor.
%%----------------------------------------------------------------------
init([NElevs, EventHandlers]) ->
    {ok,{{one_for_all, 3, 1000},
	 [{g_sup, {g_sup, start_link, []},
	   permanent, infinity, supervisor, [g_sup]},
	  {system_sup, {system_sup, start_link, [NElevs, EventHandlers]},
	   permanent, infinity, supervisor, [system_sup]}]}}.
