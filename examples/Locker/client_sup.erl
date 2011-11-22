%%% File    : client_sup.erl
%%% Author  : Clara Benac Earle <clara@bacardi>
%%% Purpose : supervisor for clients.
%%% Created : 12 Apr 2001 by Clara Benac Earle <clara@bacardi>
%%% Modified: 08 Jun 2001 by Thomas Arts <thomas@cslab.ericsson.se>

-module(client_sup).
-author('clara@bacardi').

-behaviour(supervisor).

-export([start_link/3]).

start_link(Locker,ResTypes,KillClients) ->   
    create_clients(Locker,ResTypes,1,KillClients).

create_clients(Locker,[],N,KillClients) -> {ok,self()};
create_clients(Locker,[{Resources,Type}|ResTypes],N,KillClients) ->
  NodeName = list_to_atom("node"++integer_to_list(N)),
  P = spawn(client, loop, [Locker,Resources,Type]),
  if KillClients -> spawn(client, killkillkill, [P]); true -> ok end,
  create_clients(Locker,ResTypes,N+1,KillClients).
