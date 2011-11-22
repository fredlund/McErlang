%%% File    : locker_sup.erl
%%% Author  : Clara Benac Earle <clara@bacardi>
%%% Purpose : fourth version of the locker (shared locks) with supervision
%%% Created : 12 Apr 2001 by Clara Benac Earle <clara@bacardi>
%%% Modified: 08 Jun 2001 by Thomas Arts <thomas@cslab.ericsson.se>

-module(locker_sup).
-author('clara@bacardi').

-behaviour(supervisor).

-export([start/2]).
-export([init/1]).

start(ResTypes,KillClients) ->
    io:format("~p: In locker_sup:start, calling supervisor~n",[self()]),
    supervisor:start_link(?MODULE, {ResTypes,KillClients}).

init({ResTypes,KillClients}) ->
    io:format("~p: In locker_sup:init~n",[self()]),
    Resources = 
      lists:foldl(fun({Rs,T},Res) ->
                     Rs++Res
                  end,[],ResTypes),
    UniqResources = 
      lists:foldl(fun(R,Res) ->
                     adduniq(R,Res)
                  end,[],Resources),
    SharedResources =
      lists:foldl(fun({Rs,shared},Res) ->
                     Rs++Res;
                     ({Rs,_},Res) ->
                     Res
                  end,[],ResTypes),
  {ok,{{one_for_one,2,60}, 
       [{locker,{locker,start_link,
		 [UniqResources,
		  lists:foldl(fun(R,Res) ->
				  adduniq(R,Res)
			      end,[],SharedResources -- UniqResources)]},
	 temporary, 2000, worker, dynamic},
	{client_supervisor, 
	 {client_sup, start_link, [locker,ResTypes,KillClients]},
	 temporary, infinity, supervisor,dynamic}]}}.

adduniq(Element,[]) ->
    [Element];
adduniq(Element,[Element|Tail]) ->
    [Element|Tail];
adduniq(Element,[Head|Tail]) ->
    [Head|adduniq(Element,Tail)].


