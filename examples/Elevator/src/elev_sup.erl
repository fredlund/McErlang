%%%----------------------------------------------------------------------
%%% Copyright (c) 1999, Ericsson Utvecklings AB
%%% File    : elev_sup.erl
%%% Author  : Håkan Huss <haken@erlang.ericsson.se>
%%% Purpose : Elevator process supervisor.
%%% Created : 28 Aug 1999 by Håkan Huss <haken@erlang.ericsson.se>
%%%----------------------------------------------------------------------

-module(elev_sup).
-author('haken@erlang.ericsson.se').
-vsn("1.0").

-behaviour(supervisor).

%% External exports
-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link(NElevs) ->
    supervisor:start_link({local, elev_sup}, elev_sup, NElevs).

%%%----------------------------------------------------------------------
%%% Callback functions from supervisor
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% init
%%  To be completed.
%%----------------------------------------------------------------------
init(NElevs) ->
  
    ChildList = make_child_specs(NElevs),
    SupFlags = {one_for_one, 8, 1000},
    {ok,{SupFlags, ChildList}}.

%%----------------------------------------------------------------------
%% make_child_specs(N)
%%  Returns a list of child specifications for N elevator control
%%  processes. Use the number (N) as the child name.
%%  To be implemented.
%%----------------------------------------------------------------------
make_child_specs(N) ->
    make_child_specs(N, []).

make_child_specs(0, L) ->
   L;
make_child_specs(N, L) ->
     make_child_specs(N - 1, [{N, {elevator, start_link, [N]}, permanent, 
                     1000, worker, [elevator]}|L]). 
