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

-module(mce_buechi_gen).
-export([combine/2,ccond/5,parse_predspec/2,actions/1]).

-include("stackEntry.hrl").

%%-define(debug,true).
-include("macros.hrl").

combine(L,Predicates) ->
  ?LOG("ccond(~p)~n",[L]),
  lists:foldl
    (fun (Result,LR) ->
       case Result of
         false ->
           LR;
         {true,{NextState,PrivState}} ->
           [{NextState,{PrivState,Predicates}}|LR]
       end
     end,
     [],L).

ccond(State,Actions,PrivState,Cond,NextState) ->
  ?LOG("ccond(~p)~n",[Cond]),
  case Cond of
    {pred,P} ->
      apply_user_predicate(P,State,Actions,PrivState,NextState);
    {bool,true} ->
      {true,{NextState,PrivState}};
    {bool,false} ->
      false;
    {cnot,CCondition} ->
      case ccond(State,Actions,PrivState,CCondition,NextState) of
        false -> {true,{NextState,PrivState}};
        {true,_} -> false
      end;
    {cand,{Condition1,Condition2}} ->
      case ccond(State,Actions,PrivState,Condition1,NextState) of
        false -> false;
        {true,{_,PrivState1}} ->
          ccond(State,Actions,PrivState1,Condition2,NextState)
      end
  end.

apply_user_predicate(P,State,Actions,PrivState,NextState) ->
  ?LOG
    ("apply_user_predicate(~p,~n~p,~n,~p,~p,~p)~n",
     [P,State,Actions,PrivState,NextState]),
  case P of
    {Module,Fun} ->
      try apply(Module,Fun,[State,Actions,PrivState]) of Result -> 
	  return_app_result(Result,NextState,PrivState)
      catch _:Error -> 
          io:format
	    ("*** apply(~p,~p,[State,Actions,PrivState]) failed with error~n ~p~n",
	     [Module,Fun,Error]),
          io:format("Stacktrace:~n~p~n",[erlang:get_stacktrace()]),
	  throw(mce_buechi_gen)					
      end;
    Fun when is_function(Fun) ->
      try apply(Fun,[State,Actions,PrivState]) of Result -> 
	  return_app_result(Result,NextState,PrivState)
      catch _:Error -> 
          io:format
	    ("*** apply(~p,[State,Actions,PrivState]) failed with error~n~p~n",
	     [Fun,Error]),
          io:format("Stacktrace:~n~p~n",[erlang:get_stacktrace()]),   
	  throw(mce_buechi_gen)					
      end
  end.
      
return_app_result(false,_NextState,_PrivState) -> 
  false;
return_app_result(true,NextState,PrivState) -> 
  {true,{NextState,PrivState}};
return_app_result({true,NewPrivState},NextState,_PrivState) ->
  {true,{NextState,NewPrivState}}.

parse_predspec(Predicates,PredSpec) ->
  if
    is_list(PredSpec) ->
      Result = 
	lists:foldr
	  (fun (Predicate,Collected) ->
	       case lists:keysearch(Predicate,1,PredSpec) of
		 {value,{_,PredicateFun}} -> 
		   [PredicateFun|Collected];
		 false ->
		   io:format
		     ("*** Error: predicate ~p not specified in ~p~n",
		      [Predicate,PredSpec]),
		   throw(predicate_spec)
	       end
	   end, [], Predicates),
      list_to_tuple(Result);
    true ->
      io:format
	("*** Malformed predicate specification ~p~n",
	 [PredSpec]),
      throw(predicate_spec)
  end.

actions(Entry) ->
  Entry#stackEntry.actions.

  
       


