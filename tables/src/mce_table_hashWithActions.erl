%% Copyright (c) 2009, Lars-Ake Fredlund
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%     %% Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%     %% Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%     %% Neither the name of the copyright holders nor the
%%       names of its contributors may be used to endorse or promote products
%%       derived from this software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ''AS IS''
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
%% BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR 
%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF 
%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

%% @author Lars-Ake Fredlund
%% @copyright 2006-2009 Lars-Ake Fredlund
%% @doc
%% @private

-module(mce_table_hashWithActions).

-export([init/1,permit_state/2,permit_state_alt/2,add_state/2,add_state_alt/2,add_trans/4,info/1,states_to_list/1,transitions_to_list/1,set_initial_state/2,get_initial_state/1]).
-export([delete_table/1]).
-export([states/1,transitions/1]).

-behaviour(mce_behav_table).

init(_) -> {ok, {dict:new(),void}}.

info({T,_}) -> length(dict:fetch_keys(T)).

states(Arg) -> info(Arg).
transitions({T,_}) ->   
  dict:fold
    (fun (_,Transitions,Count) ->
	 Count+length(Transitions)
     end, 0, T).

delete_table(_) -> ok. %% Data in process dict, will be overwritten...

permit_state(State, {T, _InitState}) ->
    not isInTable(State, T).

permit_state_alt(State, {T, InitState}) ->
    {not isInTable(State, T), State}.

add_trans(PrevState, State, Actions, {T,InitState}) ->
  case dict:find(PrevState,T) of
    error ->
      io:format
	("*** Error: ~p: Could not find source state~n~p in keys~n~p~n",
	 [?MODULE,PrevState,dict:fetch_keys(T)]),
      mce_result:throw_result_exc(mce_result:mk_internal_error(?MODULE));
    {ok,OldActions} ->
      {dict:store
       (PrevState,
	addActions({Actions,State},OldActions),T),
       InitState}
  end.

add_state(State, {T,InitState}) ->
  case dict:find(State,T) of
    {ok,_} -> io:format("~p: state~n~p~n already exists!~n",[?MODULE,State]);
    _ -> ok
  end,
  {dict:store(State,[],T),InitState}.

add_state_alt(State, T) ->
  add_state(State,T).

isInTable(State,T) ->
  dict:is_key(State,T).

addActions(Actions,OldActions) ->
  lists:umerge([Actions],OldActions).

states_to_list({T,_}) ->
  {ok, dict:fetch_keys(T)}.

transitions_to_list({T, _}) ->
    {ok,
     dict:fold(fun (FromState, Actions, L) ->
		       lists:map(fun ({Action, ToState}) ->
					 {FromState, Action, ToState}
				 end,
				 Actions) ++ L
	       end, [], T)}.

set_initial_state(State,{T,_}) -> {T,State}.

get_initial_state({_,State}) -> State.
		    
