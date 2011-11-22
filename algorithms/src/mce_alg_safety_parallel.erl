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

%%% Implementation of a simple depth-first verification algorithm for
%%% safety properties, simulation and debugging.

-module(mce_alg_safety_parallel).
-export([init/8,start/4,initPrivate/0,updatePrivate/3,
	 terminate/1,default_conf/0]).

-behaviour(mce_behav_algorithm).

-include("monState.hrl").
-include("monitor.hrl").
-include("stackEntry.hrl").
-include("state.hrl").
-include("mce_opts.hrl").


%%-define(debug,true).
-include("macros.hrl").


default_conf() -> #mce_opts{small_pids=true}.

init(Conf, S, NumProcesses, Stack, Monitor, Abstraction, Table, _)
  when is_integer(NumProcesses), NumProcesses > 0 ->
  check_conf_sanity(Conf),
  safety = mce_behav_monitorOps:monitorType(Monitor),
  SM = #monState{state=S, monitor=Monitor},
  {ok,
   {mce_workersCoordinator,
    init,
    [NumProcesses,
     ?MODULE,
     [Stack, Abstraction, Table, Conf],
     {SM, []},
     Table]}}.

start(Stack, Abstraction, Table, Conf) ->
  mce_conf:prepare_run(Conf),
  wait(Stack, Abstraction, Table, Conf).

initPrivate() ->
  {void,[]}.

updatePrivate(Pid,{Table,Counters},{_,L}) ->
  {Table,updatePrivate1(Pid,Counters,L)}.
updatePrivate1(Pid,Counters,[]) ->
  [{Pid,Counters}];
updatePrivate1(Pid,{NStates1,AStates1},[{Pid,{NStates,AStates}}|Rest]) ->
  [{Pid,{NStates+NStates1,AStates+AStates1}}|Rest];
updatePrivate1(Pid,Counter,[First|Rest]) ->
  [First|updatePrivate1(Pid,Counter,Rest)].

terminate({_Table,Counters}) ->
  io:format("Execution has terminated; counters are:~n  ~p~n",[Counters]),
  Counters.

wait(Stack, Abstraction, Table, Conf) ->
  receive
    {check, Pid, {State, Actions}, Waiting} ->
      try
	?LOG("~p: got an order to execute with Waiting=~p~n",
	     [self(), Waiting]),
	put(nStates, 0), put(aStates, 0),
	put(pathLimit, mce_conf:pathLimit(Conf)),
	case
	  run(mce_behav_stackOps:push
	      (#stackEntry{depth=1, state=State, actions=Actions},
	       Stack),
	      Abstraction, Table, Waiting, Pid, Conf)
	  of
	  {ok, {_, _, NewWaiting}} ->
	    ?LOG("~p waiting with ~p~n", [self(), NewWaiting]),
	    Pid ! {waiting, self(), 
		   {Table, {get(nStates),get(aStates)}}, 
		   NewWaiting},
	    wait(Stack, Abstraction, Table, Conf);
	  Result ->
	    case mce_result:is_mce_result(Result) of
	      true -> 
		?LOG("~p: bad monitor~n", [self()]),
		Pid ! {abort,self(),Result},
		exit(abort);
	      false ->
		mce_conf:format
		  (normal,"~p: strange return value ~p~n", [self(), Result]),
		throw(bad)
	    end
	end
      catch
	{result_exc,R} ->
	  Pid ! {abort,self(),R},
	  exit(abort);
	Exception:Reason ->
	  Pid!
	    {abort,self(),
	     mce_result:mk_internal_error
	     (mce_result:mk_exception_error
	      (Exception,Reason,erlang:get_stacktrace()))},
	  exit(abort)
      end;
    {coord, Pid, _} ->
      Pid ! {coord_nok, self()},
      wait(Stack, Abstraction, Table, Conf);
    Other ->
      mce_conf:format(normal,"??? Strange message ~p received~n",[Other]),
      exit(error)
  end.

run(Stack, Abstraction, Table, Waiting, CoordPid, Conf) ->
  Depth = stackDepth(Stack),
  {Entry, Rest} = mce_behav_stackOps:pop(Stack),
  report_path_length(Depth),
  case skip_long_path(Depth) of
    true -> {ok, {Table, Abstraction, Waiting}};
    false ->
      NStates = get(nStates),
      put(nStates, NStates + 1),
      State = Entry#stackEntry.state,
      Actions = Entry#stackEntry.actions,
      case addTransAndState
	(getPrevState(Stack), Actions, State, Abstraction, Table) of
	{exists, Abstraction1, Table1} ->
	  {ok, {Table1, Abstraction1, Waiting}};
	{new, AbsState, AbsActions, Abstraction1, Table1} ->
	  Mon = State#monState.monitor,
	  Sys = State#monState.state,
	  case mce_behav_monitorOps:stateChange(Sys, Mon, Stack) of
	    skip ->
	      run(Rest, Abstraction1, Table1, Waiting, CoordPid, Conf);
	    {ok, NewMon} ->
	      check_transitions
		(NStates,
		 transitions(Sys, NewMon, Stack, Conf),
		 mce_behav_stackOps:push
		 (Entry#stackEntry
		  {actions=Actions,
		   abs_system=AbsState,
		   abs_actions=AbsActions},
		  Rest),
		 Abstraction1,
		 Table1,
		 Waiting, CoordPid, Conf);
	    MonError ->
	      ?LOG("*** Found a counterexample at depth ~p:~n  ~p ***~n",
		   [Depth, MonError]),
	      Result = mce_result:mk_badmon(MonError,Stack,Table1,Conf),
	      case mce_conf:shortest(Conf) of
		true ->
		  remember_shortest_path(Depth, Result),
		  set_path_limit(Depth),
		  run(Rest, Abstraction1, Table1, Waiting, CoordPid, Conf);
		false ->
		  mce_result:throw_result_exc(Result)
	      end
	  end
      end
  end.

check_transitions(NStates, Transitions, Stack, Abstraction, Table, 
		  Waiting, CoordPid, Conf) ->
  if NStates rem 100 =:= 0 ->
      check_transitions
	(Transitions, Stack, Abstraction, Table,
	 check_coord(Waiting),
	 CoordPid, Conf);
     true ->
      check_transitions
	(Transitions, Stack, Abstraction, Table,
	 Waiting, CoordPid, Conf)
  end.

check_transitions([], _Stack, Abstraction, Table, Waiting, _CoordPid, _Conf) ->
  {ok, {Table, Abstraction, Waiting}};
check_transitions([{Actions, State}| Rest],
		  Stack, Abstraction, Table, Waiting, CoordPid, Conf) ->
  Depth = stackDepth(Stack),
  if Waiting =/= [], Rest =/= [] ->
      [Pid| RestPids] = Waiting,
      ?LOG("~p: Ordering ~p to check...~n", [self(), Pid]),
      Pid ! {check, CoordPid, {State, Actions}, []},
      check_transitions
	(Rest, Stack, Abstraction, Table, RestPids, CoordPid, Conf);
     Rest =:= [] ->
      run(mce_behav_stackOps:push
	  (#stackEntry{depth=Depth + 1, state=State,
		       actions=Actions}, Stack),
	  Abstraction, Table,
	  Waiting, CoordPid, Conf);
     true ->
      case   
	run(mce_behav_stackOps:push
	    (#stackEntry{depth=Depth + 1,
			 state=State,
			 actions=Actions}, Stack),
	    Abstraction, Table,
	    Waiting, CoordPid, Conf) of
	{ok, {NewTable,NewAbstraction,NewWaiting}} ->
	  check_transitions(Rest, Stack, NewAbstraction, NewTable,
			    NewWaiting, CoordPid, Conf);
	Error -> Error
      end
  end.

%%%%%%%

check_coord(Waiting) ->
  receive
    {coord, Pid, MoreWaiting} ->
      Pid!{coord_ok,self()},
      Waiting++MoreWaiting;
    Other ->
      mce_conf:format
	(normal,"~p: check_coord: got strange message~n  ~p~n",[self(),Other]),
      exit(check_coord)
  after 0 ->
      Waiting
  end.

getPrevState(Stack) ->
  {_, Rest} = mce_behav_stackOps:pop(Stack),
  case mce_behav_stackOps:is_empty(Rest) of
    true -> none;
    false ->
      {Entry, _} = mce_behav_stackOps:pop(Rest), Entry#stackEntry.abs_system
    end.

stackDepth(Stack) ->
    case mce_behav_stackOps:is_empty(Stack) of
      true -> 0;
      false ->
	{Entry, _} = mce_behav_stackOps:pop(Stack), Entry#stackEntry.depth
    end.

addTransAndState(PrevState, Actions, State, Abstraction, Table) ->
  {AbsActions, Abstraction1} =
    mce_behav_abstractionOps:abstract_actions(Actions, Abstraction),
  {AbsState, Abstraction2} =
    mce_behav_abstractionOps:abstract_state(State, Abstraction1),
  {NewElement, SavedState} =
    mce_behav_tableOps:permit_state_alt(AbsState, Table),
  Table1 =
    if PrevState =:= none ->
	put(initialState, AbsState),
	Table;
       true ->
	mce_behav_tableOps:add_trans(PrevState, AbsState, AbsActions, Table)
    end,
  if NewElement ->
      report_generated_states(),
      Table2 = mce_behav_tableOps:add_state_alt(SavedState, Table1),
      {new, AbsState, AbsActions, Abstraction2, Table2};
     not NewElement ->
      {exists, Abstraction2, Table1}
  end.



%% Check if we should skip the rest of this path (too long)
skip_long_path(Depth) ->
  case get(pathLimit) of
    undefined -> false;
    Len -> Depth>=Len
  end.

%% Report on path length
report_path_length(Depth) ->
  if Depth rem 500 =:= 0, Depth > 0 ->
      Report =
	case get(path_depth) of
	  undefined ->
	    true;
	  N ->
	    Depth > N
	end,
      if Report ->
	  mce_conf:format
	    (normal,"Path depth at ~p entries~n", [Depth]),
	  put(path_depth, Depth);
	 true ->
	  ok
      end;
     true ->
      ok
  end.

%% report_generated_states
report_generated_states() ->
  NumStates = get(aStates),
  if
    (NumStates>0) and (NumStates rem 10000 =:= 0) ->
      CheckedStates = get(nStates),
      mce_conf:format
	(normal,"Generated states ~p; checked states ~p; relation ~p~n",
	 [NumStates,CheckedStates,CheckedStates/NumStates]);
    true ->
      ok
  end,
  put(aStates,NumStates+1).

%% Remember shortest path
remember_shortest_path(Depth,Result) ->
  put(shortest,{Depth,Result}).

%% Set a limiter on the maximum length of paths explored
set_path_limit(Depth) ->
  put(pathLimit,Depth).

%% Compute transitions from current state; coupled with monitor
transitions(Sys, Monitor, Stack, Conf) ->
  lists:map
    (fun (T) ->
	 try
	   mce_conf:commit(T, Monitor, Conf)
	   of
	   {Actions, NSys} ->
	     {Actions, #monState{state=NSys, monitor=Monitor}}
	 catch
	   {result_exc,Result} ->
	     case mce_result:stack(Result) of
	       void ->
		 mce_result:throw_result_exc
		   (mce_result:add_stack
		    (gen_error_stack(Sys,Monitor,Stack),Result));
	       _ ->
		 mce_result:throw_result_exc(Result)
	     end
	 end
     end,
     mce_conf:transitions(Sys, Conf)).

gen_error_stack(State, Monitor, Stack) ->
  Depth = stackDepth(Stack),
  Actions = lists:reverse(get(actions)),
  mce_behav_stackOps:push
    (#stackEntry
     {depth=Depth+1,
      state=#monState{state=mce_erl:void_state(State), monitor=Monitor},
      actions=Actions},
     Stack).


check_conf_sanity(Conf) ->
  %% Sim_external_world cannot be true
  %% Sim_external_world cannot be true
  case Conf#mce_opts.sim_external_world of
    true ->
      io:format("Error: sim_external_world=true is not compatible with "++
		"the selected algorithm~n",[]),
      throw(conf_error);
    _ ->
      ok
  end,
  case Conf#mce_opts.shortest of
    true ->
      io:format("Warning: algorithm mce_alg_safety_parallel cannot calculuate "
		++ "the shortest path to error (on the todo list)~n",[]);
    _ ->
      ok
  end.
