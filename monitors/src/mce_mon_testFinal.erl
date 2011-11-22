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

-module(mce_mon_testFinal).
-export([init/1,stateChange/3,monitorType/0]).
-behaviour(mce_behav_monitor).
-include("state.hrl").
-include("stackEntry.hrl").
-include("process.hrl").
-include("node.hrl").

%%% Tests that the initial process eventually terminates without an exception
%%% and returns the value given as argument to the init function.

init({FinalValue,[{no_deadlocks}]}) ->
  {ok,{init,FinalValue,false}};
init(FinalValue) ->
  {ok,{init,FinalValue,true}}.

stateChange(State, {init,FinalValue,CheckDeadlocks}, _) ->
    [P] = mce_erl:allProcesses(State),
    {ok, {running,P#process.pid,FinalValue,CheckDeadlocks}};
stateChange(State,MonState = {running,Pid,FinalValue,_CheckDeadlocks},Stack) ->
    {Element, _} = mce_behav_stackOps:pop(Stack),
    Actions = Element#stackEntry.actions,
    searchActions(Actions, State, Pid, FinalValue, MonState).

searchActions([],State,_Pid,FinalValue,MonState={_,_,_,CheckDeadlocks}) ->
  if CheckDeadlocks -> check_deadlocked(State,MonState,FinalValue);
     true -> false
  end;
searchActions([Action|Rest],State,Pid,FinalValue,MonState) ->
  case {mce_erl_actions:get_source(Action),
	mce_erl_actions:is_terminated(Action)} of
    {Pid,true} ->
      case mce_erl_actions:get_terminated_reason(Action) of
	FinalValue ->
	  io:format("Found correct final value ~p~n",[FinalValue]),
	  skip;
	OtherValue -> 
	  io:format
	    ("*** ERROR in monitor ~p:~n  process ~p terminated with "++
	     "value~n  ~p;~n"++
	     "expected value ~p~n",
	     [?MODULE,Pid,OtherValue,FinalValue]),
	  {badValue,OtherValue}
      end;
    _ -> searchActions(Rest,State,Pid,FinalValue,MonState)
  end.

check_deadlocked(State,MonState,FinalValue) ->
  case is_deadlocked(State) of
    true ->
      io:format
	("*** ERROR in monitor ~p:~n  The system~n    ~p~n is deadlocked;~n"++
	 "  expected a final value ~p~n",
	 [?MODULE,State,FinalValue]),
      deadlocked;
    false ->
      {ok,MonState}
  end.

is_deadlocked(State) ->
    State#state.ether =:= [] 
    andalso
    case mce_utils:find
      (fun (P) -> P#process.status =/= blocked end,
       mce_erl:allProcesses(State)) of
      {ok, _} -> false;
      no -> true
    end.

monitorType() -> safety.

      
  
