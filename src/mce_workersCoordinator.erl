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

-module(mce_workersCoordinator).
-export([init/5,starting/5]).

-include("monState.hrl").
-include("monitor.hrl").
-include("stackEntry.hrl").
-include("../languages/erlang/src/include/state.hrl").


%%-define(debug,true).
-include("macros.hrl").

init(NumProcesses,WorkerModule,WorkerArguments,BootArg,_Table) 
  when is_integer(NumProcesses), NumProcesses>0 ->
  Pid =
    spawn
      (?MODULE,
       starting,
       [self(),NumProcesses,WorkerModule,WorkerArguments,BootArg]),
  loop(Pid).

loop(Pid) ->
  receive
    {Pid,ReturnValue} ->
      ReturnValue;
    {ask,MceMonitor} ->
      MceMonitor!alive,
      loop(Pid);
    finish ->
      exit(Pid,kill),
      mce_result:mk_timeout();
    Other ->
      mce_result:mk_internal_error
	(mce_result:mk_value_error
	 ({strange_message,Other}))
  end.

starting(ParentPid, NumProcesses, WorkerModule, WorkerArguments, BootArg) ->
  process_flag(trap_exit, true),
  {_, AllProcesses} =
    mce_utils:do
      ({NumProcesses, []},
       fun ({N, _}) -> N > 0 end,
       fun ({N, Procs}) ->
	   {Pid,_} = erlang:spawn_monitor(WorkerModule, start, WorkerArguments),
	   link(Pid),
	   {N - 1, [Pid| Procs]}
       end),
  [FirstProcess| RestProcesses] = AllProcesses,
  ?LOG("Ordering ~p to check...~n", [FirstProcess]),
  FirstProcess ! {check, self(), BootArg, RestProcesses},
  Result =
    supervise(WorkerModule, FirstProcess, RestProcesses, [], 
	      WorkerModule:initPrivate()),
  ParentPid ! {self(), Result}.

supervise(WorkerModule, Leader, Working, Unassigned, Private) ->
  ?LOG("~p: supervise(~p,~p,~p)~n", [self(), Leader, Working, Unassigned]),
  receive
    {waiting, Pid, Private1, Waiting} ->
      ?LOG("~p: supervise: ~p is waiting~n", [self(), Pid]),
      NewPrivate = WorkerModule:updatePrivate(Pid, Private1, Private),
      if Pid =:= Leader ->
	  Working1 = minus(Working, Waiting),
	  elect_new_leader
	    (WorkerModule, Working1,
	     [Leader| Waiting ++ Unassigned], NewPrivate);
	 true ->
	  assign_to_leader
	    (WorkerModule, Leader, [Pid], lists:delete(Pid, Working),
	     Unassigned, NewPrivate)
      end;
    {'DOWN', _, _, Pid, Reason} ->
      io:format("supvise: worker process ~p crashed "++
		"because of ~p; aborting...~n",
		[Pid, Reason]),
      terminateWorkers([Leader| Working ++ Unassigned]),
      mce_result:mk_internal_error
	(mce_result:mk_value_error({worker_crashed,Pid,Reason}));
    {abort,Pid,Value} ->
      io:format
	("supvise: worker process ~p orders abort~n",
	 [Pid]),
      terminateWorkers([Leader| Working ++ Unassigned]),
      Value;
    Other ->
      io:format("~p: supervise got strange message ~p~n", [self(), Other]),
      mce_result:mk_internal_error
	(mce_result:mk_value_error({strange_message,Other}))
  end.


assign_to_leader(WorkerModule,Leader,Pids,Working,Unassigned,Private) ->
  ?LOG("trying to assign ~p to leader ~p, working is ~p, unassigned ~p~n",
       [Pids,Leader,Working,Unassigned]),
  Leader!{coord, self(), Pids},
  wait_for_assign(WorkerModule,Leader,Pids,Working,Unassigned,Private).

wait_for_assign(WorkerModule, Leader, Waiting, Working, Unassigned, Private) ->
  receive
    {waiting, Leader, Private1, Waiting1} ->
      NewPrivate = WorkerModule:updatePrivate(Leader, Private1, Private),
      Working1 = minus(Working, Waiting1),
      wait_for_assign
	(WorkerModule, Leader, Waiting, Working1,
	 Waiting1 ++ Unassigned, NewPrivate);
    {waiting, Other, Private1, []} ->
      NewPrivate =
	WorkerModule:updatePrivate(Other, Private1, Private),
      wait_for_assign
	(WorkerModule, Leader, Waiting,
	 lists:delete(Other, Working), [Other| Unassigned], NewPrivate);
    {coord_ok, _} ->
      if Unassigned =:= [] ->
	  supervise(WorkerModule, Leader, Waiting ++ Working, [], Private);
	 true ->
	  assign_to_leader
	    (WorkerModule, Leader, Unassigned, Waiting ++ Working, [], Private)
      end;
    {coord_nok, _} ->
      elect_new_leader
	(WorkerModule, Working, [Leader| Waiting ++ Unassigned], Private);
    Other ->
      io:format("assign_to_leader: got strange message ~p~n", [Other]),
      mce_result:mk_internal_error
	(mce_result:mk_value_error({strange_message,Other}))
  end.


elect_new_leader(WorkerModule,[],Unassigned,Private) ->
  Counters =
    WorkerModule:terminate(Private),
  StoredStates =
    lists:foldl
      (fun ({_Pid,{_Explored,Stored}},Acc) -> Stored+Acc end, 0, Counters),
  ExploredStates =
    lists:foldl
      (fun ({_Pid,{Explored,_Stored}},Acc) -> Explored+Acc end, 0, Counters),
  terminateWorkers(Unassigned),
  mce_result:add_explored_states
    (ExploredStates,
     mce_result:add_stored_states
       (StoredStates,mce_result:mk_ok()));
elect_new_leader(WorkerModule,[Candidate|Rest],Unassigned,Private) ->
  ?LOG("trying to elect new leader from ~p~n",[[Candidate|Rest]]),
  Candidate!{coord, self(), Unassigned},
  wait_for_leader(WorkerModule,Candidate,Rest,Unassigned,[],Private).

wait_for_leader(WorkerModule, Candidate, Rest, Waiting, Unassigned, Private) ->
  receive
    {waiting, Candidate, Private1, []} ->
      NewPrivate =
	WorkerModule:updatePrivate(Candidate, Private1, Private),
      wait_for_leader
	(WorkerModule, Candidate, Rest, Waiting, Unassigned, NewPrivate);
    {waiting, Other, Private1, []} ->
      NewPrivate =
	WorkerModule:updatePrivate(Other, Private1, Private),
      wait_for_leader
	(WorkerModule, Candidate, lists:delete(Other, Rest), Waiting,
	 [Other| Unassigned], NewPrivate);
    {coord_ok, _} ->
      if Unassigned =:= [] ->
	  supervise(WorkerModule, Candidate, Waiting ++ Rest, [], Private);
	 true ->
	  assign_to_leader
	    (WorkerModule, Candidate, Unassigned, Waiting ++ Rest, [], Private)
      end;
    {coord_nok, _} ->
      elect_new_leader
	(WorkerModule, Rest, [Candidate| Waiting ++ Unassigned], Private);
    Other ->
      io:format("elect_new_leader: got strange message ~p~n", [Other]),
      mce_result:mk_internal_error
	(mce_result:mk_value_error({strange_message,Other}))
  end.


terminateWorkers(Workers) ->
  lists:foreach(fun (KillPid) -> exit(KillPid,kill) end, Workers).


minus(L,[]) -> L;
minus(L,[Wait|RestWaiting]) -> 
  minus(lists:delete(Wait,L),RestWaiting).

