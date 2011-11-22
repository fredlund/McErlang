%%%----------------------------------------------------------------------
%%% File    : elevators.erl
%%% Author  : Håkan Huss <hakan@erlang.ericsson.se>
%%% Purpose : Elevator application module.
%%% Created : 30 Aug 1999 by Håkan Huss <hakan@erlang.ericsson.se>
%%%----------------------------------------------------------------------

-module(elevators).
-author('hakan@erlang.ericsson.se').
-vsn("1.0").

-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).

%%%----------------------------------------------------------------------
%%% Callback functions from application
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Start the top-level supervisor with the appropriate values.
%% Not complete.
%%----------------------------------------------------------------------
start(normal, []) ->
    % should obtain InitFloor, NFloors and NElevs from configuration
    % parameters
    {ok, InitFloor} = application:get_env(initfloor),
    {ok, NFloors} = application:get_env(nfloors),
    {ok, NElevs} = application:get_env(nelevs),
    sim_sup:start_link(InitFloor, NFloors, NElevs).

%%----------------------------------------------------------------------
%% Stop has nothing special to do.
%%----------------------------------------------------------------------
stop(_State) ->
    ok.
