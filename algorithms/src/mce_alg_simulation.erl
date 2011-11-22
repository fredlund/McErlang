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

%%% Implementation of a simple simulation algorithm (and for debugging).

-module(mce_alg_simulation).
-export([default_conf/0,init/8,run/3]).

-include("monState.hrl").
-include("monitor.hrl").
-include("stackEntry.hrl").
-include("mce_opts.hrl").

-behaviour(mce_behav_algorithm).

%%-define(debug,true).
-include("macros.hrl").


default_conf() ->
  #mce_opts{output=true,is_simulation=true,small_pids=false}.

init(Conf, S, _, Stack, Monitor, _, _, Scheduler) ->
  check_conf_sanity(Conf),
  if
    Conf#mce_opts.sim_external_world ->
      (Conf#mce_opts.mce_monitor)!disable_deadlines;
    true ->
      ok
  end,
  safety = mce_behav_monitorOps:monitorType(Monitor),
  SM = #monState{state=S, monitor=Monitor},
  {ok,
   {?MODULE,
    run,
    [mce_behav_stackOps:push(#stackEntry{state=SM}, Stack), Scheduler, Conf]}}.

num_states(Stack) ->
  Depth = stackDepth(Stack),
  {Depth,Depth}.

run(Stack, Scheduler, Conf) ->
  mce_conf:monitor_protocol(Stack,fun num_states/1),
  {Entry, _} = mce_behav_stackOps:pop(Stack),
  State = Entry#stackEntry.state,
  Mon = State#monState.monitor,
  Sys = State#monState.state,
  ExternalIOPossible = mce_conf:external_io_possible(Conf),
  case mce_behav_monitorOps:stateChange(Sys, Mon, Stack) of
    {ok, NewMon} ->
      {Transitions, NewStack} =
	if ExternalIOPossible ->
	    {io,IOResult,Transes} = mce_conf:transitions(Sys, Conf),
	    {Transes, updStack(IOResult,Stack)};
	   true ->
	    {mce_conf:transitions(Sys, Conf), Stack}
	end,
      SchedulerCommits = mce_behav_schedulerOps:willCommit(Scheduler),
      try
	mce_behav_schedulerOps:choose(Transitions, Scheduler, NewMon, Conf) of
	{ok, {Result, NewSched}} ->
	  {Actions, NewSys} =
	    if SchedulerCommits -> Result;
	       true ->
		try
		  mce_conf:commit(Result, NewMon, Conf)
		catch
		  {result_exc,ResultExc} ->
		    case mce_result:stack(ResultExc) of
		      void ->
			mce_result:throw_result_exc
			  (mce_result:add_monitor
			   (NewMon,
			    mce_result:add_stack
			    (gen_error_stack(Sys,NewMon,Stack),ResultExc)));
		      _ ->
			mce_result:throw_result_exc
			  (mce_result:add_monitor
			   (NewMon,ResultExc))
		    end
		end
	    end,
	  NewEntry =
	    #stackEntry{state=#monState{state=NewSys, monitor=NewMon},
			actions=Actions,
			depth=stackDepth(Stack) + 1},
	  NewerStack =
	    mce_behav_stackOps:push(NewEntry, NewStack),
	  maybe_print_actions(Actions,NewerStack),
	  run(NewerStack, NewSched, Conf);
	no_transitions ->
	  %% If the simulation communicating with the external world
	  %% we may just be waiting for more input and
	  %% have to recheck.
	  if ExternalIOPossible ->
	      run(NewStack, Scheduler, Conf);
	     true ->
	      {NewEntry, _} = mce_behav_stackOps:pop(NewStack),
	      mce_result:add_monitor
		((NewEntry#stackEntry.state)#monState.monitor,
		 mce_result:add_state
		 ((NewEntry#stackEntry.state)#monState.state,
		  mce_result:add_stack(NewStack,mce_result:mk_ok())))
	  end;
	stopping ->
	  %% Scheduler wants to stop
	  {NewEntry, _} = mce_behav_stackOps:pop(NewStack),
	  mce_result:add_monitor
	    ((NewEntry#stackEntry.state)#monState.monitor,
	     mce_result:add_state
	     ((NewEntry#stackEntry.state)#monState.state,
	      mce_result:add_stack(NewStack,mce_result:mk_ok())))
      catch
	{result_exc,Result} ->
	  case mce_result:stack(Result) of
	    void ->
	      mce_result:throw_result_exc
		(mce_result:add_monitor
		 (NewMon,
		  mce_result:add_stack
		  (gen_error_stack(Sys,NewMon,Stack),Result)));
	    _ ->
	      mce_result:throw_result_exc
		(mce_result:add_monitor(NewMon,Result))
	  end
      end;
    MonError ->
      mce_result:mk_badmon(MonError,Stack,void,Conf)
  end.

gen_error_stack(State, Monitor, Stack) ->
  ?LOG("generating error stack~n",[]),
  Depth = stackDepth(Stack),
  Actions = lists:reverse(get(actions)),
  mce_behav_stackOps:push
    (#stackEntry
     {depth=Depth + 1,
      state=#monState{state=mce_erl:void_state(State), monitor=Monitor},
      actions=Actions},
     Stack).

stackDepth(Stack) ->
  case mce_behav_stackOps:is_empty(Stack) of
    true -> 0;
    false -> {Entry, _} = mce_behav_stackOps:pop(Stack), Entry#stackEntry.depth
  end.

updStack({unchanged, _, _}, Stack) ->
  Stack;
updStack({changed, Received, NewState}, Stack) ->
  {Entry,_} = mce_behav_stackOps:pop(Stack),
  State = Entry#stackEntry.state,
  Actions =
    case mce_conf:is_recording_actions() of
      true -> lists:map(fun ({Pid,Msg}) -> {Pid, io, Msg, []} end, Received);
      false -> []
    end,
  mce_behav_stackOps:push
    (Entry#stackEntry
     {state=State#monState{state=NewState},
      actions=Actions,
      depth=Entry#stackEntry.depth+1},
     Stack).

maybe_print_actions(Actions, Stack) ->
  case mce_conf:sim_actions() of
    true ->
      mce_conf:format
	(normal,
	 "~nActions:~n~s~n",
	 [mce_erl_debugger:print_actions(Actions,Stack)]);
    false ->
      ok
  end.

check_conf_sanity(Conf) ->
  case Conf#mce_opts.shortest of
    true ->
      io:format("Warning: algorithm mce_alg_simulation cannot calculuate "
		++ "the shortest path to error~n",[]);
    _ ->
      ok
  end.
