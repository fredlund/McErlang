%%% File    : client.erl
%%% Author  : Clara Benac Earle <clara@bacardi>
%%% Purpose : 
%%% Created : 12 Apr 2001 by Clara Benac Earle <clara@bacardi>
%%% Modified: 08 Jun 2001 by Thomas Arts <thomas@cslab.ericsson.se>

-module(client).
-author('clara@bacardi').

-export([start_link/3, loop/3, killkillkill/1]).

start_link(Locker,Resources,Type) ->   
    {ok,spawn_link(?MODULE, loop, [Locker,Resources,Type])}.

loop(Locker,Resources,Type) ->
    mce_erl:probe(request,Resources),
    gen_server:call(Locker, {request,Resources,Type}),
    mce_erl:probe(release,Resources),
    gen_server:call(Locker, release),
    loop(Locker,Resources,Type).

killkillkill(Pid) ->
    io:format("Matando a ~p~n",[Pid]),
    exit(Pid,kill).

