%%%----------------------------------------------------------------------
%%% File    : g_sup.erl
%%% Author  : Håkan Huss <hakan@erlang.ericsson.se>
%%% Purpose : Supervisor for the elevator graphics control processes.
%%% Created : 30 Aug 1999 by Håkan Huss <hakan@erlang.ericsson.se>
%%%----------------------------------------------------------------------

-module(g_sup).
-author('hakan@erlang.ericsson.se').
-vsn("1.0").

-behaviour(supervisor).

%% External exports
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, g_sup}, g_sup, []).

%%%----------------------------------------------------------------------
%%% Callback functions from supervisor
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% The init function adds the display event handler to sys_event.
%% Note that all the graphical elevator processes are added dynamically
%% when the display event handler is initializing itself.
%%----------------------------------------------------------------------
init([]) ->
    {ok, {{one_for_one, 0, 1}, []}}.

