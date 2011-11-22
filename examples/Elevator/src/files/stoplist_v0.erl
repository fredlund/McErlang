%%% File    : stoplist.erl
%%% Author  : Håkan Huss <hakan@erlang.ericsson.se>
%%% Purpose : Contains functions to manipulate stop lists.
%%% Created :  5 Aug 1999 by Håkan Huss <hakan@erlang.ericsson.se>

-module(stoplist).
-author('hakan@erlang.ericsson.se').
-vsn("1.0").

-export([add/3, time_to/3]).

%%----------------------------------------------------------------------
%% add(Floor, LastFloor, StopList)
%%  Adds a stop at Floor to StopList, given that the last floor passed was
%%  LastFloor.
%%----------------------------------------------------------------------
add(Floor, _LastFloor, []) ->
    [Floor];
add(Floor, _LastFloor, [Floor | Rest]) ->
    [Floor | Rest];
add(Floor, LastFloor, [Other | Rest]) ->
    [Other | add(Floor, LastFloor, Rest)].

%%----------------------------------------------------------------------
%% time_to(ToFloor, FromFloor, StopList)
%%  Gives an estimate of how long it will take to get to ToFloor, given
%%  that we've passed FromFloor and have to stop at all floors in StopList.
%%  The actual value need not be a correct time, all we need is a
%%  number which can be used to select the best elevator to choose to go to
%%  a floor.
%%----------------------------------------------------------------------
time_to(ToFloor, FromFloor, StopList) ->
    length(StopList).
