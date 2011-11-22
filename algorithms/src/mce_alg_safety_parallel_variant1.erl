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

-module(mce_alg_safety_parallel_variant1).
-export([init/7,start/6,wait/4]).

-include("monState.hrl").
-include("monitor.hrl").
-include("stackEntry.hrl").
-include("state.hrl").


%%%-define(debug,true).
-include("macros.hrl").

init(NumProcesses, S, Stack, Monitor, Abstraction, Table, _) ->
    process_flag(trap_exit, true),
    safety = mce_behav_monitorOps:monitorType(Monitor),
    SM = #monState{state=S, monitor=Monitor},
    {_, AllProcesses} =
    mce_utils:do({NumProcesses - 1, []},
		 fun ({N, _}) -> N > 0 end,
		 fun ({N, Procs}) ->
		     Pid =
		       spawn_link
			 (?MODULE, wait, [self(), Stack, Abstraction, Table]),
		     {N - 1, [Pid| Procs]}
		 end),
    {ok,
     {?MODULE,
      start,
      [mce_behav_stackOps:push(#stackEntry{state=SM, depth=1}, Stack), Abstraction, Table,
       AllProcesses,
       [],
       AllProcesses]}}.

wait(ParentPid, Stack, Abstraction, Table) ->
    put(nStates, 0),
    put(aStates, 0),
    put(language, mcerl),
    receive
      {state, State, Actions} ->
	  try
	    case
	      mce_alg_safety:run
	      (mce_behav_stackOps:push
	       (#stackEntry{depth=1, state=State, actions=Actions},
		Stack),
	       Abstraction, Table)
		of
	      {ok, {_, _}} ->
		  ParentPid ! {waiting, self()},
		  wait(ParentPid, Stack, Abstraction, Table);
	      {badmon, {counterExample, MonError, Stack}, _} ->
		  io:format("~p: bad monitor~n", [self()]),
		  ParentPid ! {counterExample, self(), MonError, stackState(Stack)},
		  exit(counterExample);
	      Other ->
		  io:format("~p: strange receive value ~p~n", [self(), Other]),
		  throw(bad)
	    end
	  catch
	    exit:Error ->
		io:format("~p: got an exit ~p~n", [self(), Error]),
		io:format("Stacktrace: ~p~n", [erlang:get_stacktrace()]),
		throw(bad);
	    error:ErrorMsg ->
		io:format("~p: got an error ~p~n", [self(), ErrorMsg]),
		io:format("Stacktrace: ~p~n", [erlang:get_stacktrace()]),
		throw(bad);
	    %%        throw:{badcode,Stack} ->
	    %%	  io:format("~p: badcode~n",[self()]),
	    %%	  ParentPid!{error,self(),stackState(Stack)};
	    Throw ->
		io:format("~p: got a throw ~p~n", [self(), Throw]),
		io:format("Stacktrace: ~p~n", [erlang:get_stacktrace()]),
		throw(bad)
	  end
    end.

stackState(Stack) ->
    {Entry, _} = mce_behav_stackOps:pop(Stack),
    Entry#stackEntry.state.

start(Stack,Abstraction,Table,Waiting,Working,AllProcesses) ->
  put(nStates,0),
  put(aStates,0),
  {ok,{NTable,_NAbstraction,{_NewWaiting,NewWorking}}} =
    run(Stack,Abstraction,Table,Waiting,Working,AllProcesses),
  io:format("Waiting for ~p processes to finish~n",[NewWorking]),
  wait_for_all(NewWorking,AllProcesses),
  io:format
    ("*** Run ending. ~p states explored, stored states ~p.~n",
     [get(nStates),get(aStates)]),
  ?LOG("No more states to explore. Finishing.~n",[]),
  {ok,NTable}.

run(Stack, Abstraction, Table, Waiting, Working, AllProcesses) ->
    Depth = stackDepth(Stack),
    {Entry, Rest} = mce_behav_stackOps:pop(Stack),
    report_path_length(Depth),
    case skip_long_path(Depth) of
      true -> {ok, {Table, Abstraction, {Waiting, Working}}};
      false ->
	  put(nStates, get(nStates) + 1),
	  State = Entry#stackEntry.state,
	  Actions = Entry#stackEntry.actions,
	  case addTransAndState(getPrevState(Stack), Actions, State, Abstraction, Table) of
	    {exists, Abstraction1, Table1} ->
		{ok, {Table1, Abstraction1, {Waiting, Working}}};
	    {new, AbsState, AbsActions, Abstraction1, Table1} ->
		Mon = State#monState.monitor,
		Sys = State#monState.state,
		case mce_behav_monitorOps:stateChange(Sys, Mon, Stack) of
		  skip ->
		      run(Rest, Abstraction1, Table1, Waiting, Working, AllProcesses);
		  {ok, NewMon} ->
		      check_transitions(transitions(Sys, NewMon, Stack),
					mce_behav_stackOps:push(Entry#stackEntry{actions=Actions, abs_system=AbsState, abs_actions=AbsActions},
								Rest),
					Abstraction1,
					Table1,
					Waiting, Working,
					AllProcesses);
		  MonError ->
		      ?LOG("*** Found a counterexample at depth ~p:~n  ~p ***~n",
			   [Depth, MonError]),
		      case search_shortest_path() of
			true ->
			    remember_shortest_path(Depth, MonError, Stack),
			    set_path_limit(Depth),
			    run(Rest, Abstraction1, Table1, Waiting, Working, AllProcesses);
			false ->
			    {badmon, {counterExample, MonError, Stack}, Table1}
		      end
		end
	  end
    end.

check_transitions([], _Stack, Abstraction, Table, Waiting, Working, _AllProcesses) ->
    {ok, {Table, Abstraction, {Waiting, Working}}};
check_transitions([{Actions, State}],
		  Stack, Abstraction, Table, Waiting, Working, AllProcesses) ->
    Depth = stackDepth(Stack),
    run(mce_behav_stackOps:push(#stackEntry{depth=Depth + 1, state=State, actions=Actions}, Stack),
	Abstraction, Table,
	Waiting, Working, AllProcesses);
check_transitions([{Actions, State}| Rest],
		  Stack, Abstraction, Table, Waiting, Working, AllProcesses) ->
    Depth = stackDepth(Stack),
    {NewWaiting, NewWorking} =
	compute_new_waiting(Waiting, Working, AllProcesses),
    case NewWaiting of
      [Pid| RestPids] ->
	  Pid ! {state, State, Actions},
	  check_transitions(Rest, Stack, Abstraction, Table, RestPids, [Pid| NewWorking],
			    AllProcesses);
      _ ->
	  {ok, {NewTable, NewAbstraction, {NewerWaiting, NewerWorking}}} =
	      run(mce_behav_stackOps:push(#stackEntry{depth=Depth + 1, state=State, actions=Actions}, Stack),
		  Abstraction, Table,
		  Waiting, Working, AllProcesses),
	  check_transitions(Rest, Stack, NewAbstraction, NewTable,
			    NewerWaiting, NewerWorking, AllProcesses)
    end.

%%%%%%%

compute_new_waiting(Waiting,Working,AllProcesses) ->
  receive
    {waiting,Pid} ->
      compute_new_waiting
	([Pid|Waiting],
	 lists:delete(Pid,Working),
	 AllProcesses);
    {counterExample,Pid,MonError,State} ->
      io:format("~p reported counterexample~n",[Pid]),
      lists:foreach(fun (P) -> exit(P,kill) end, AllProcesses),
      throw({failedMon, MonError, State});
    {error,Pid,State} ->
      io:format("~p reported error~n",[Pid]),
      lists:foreach(fun (P) -> exit(P,kill) end, AllProcesses),
      throw({badcode,State})
  after 0 ->
      {Waiting,Working}
  end.

getPrevState(Stack) ->
    {_, Rest} = mce_behav_stackOps:pop(Stack),
    case mce_behav_stackOps:is_empty(Rest) of
      true -> none;
      false -> {Entry, _} = mce_behav_stackOps:pop(Rest), Entry#stackEntry.abs_system
    end.

stackDepth(Stack) ->
    case mce_behav_stackOps:is_empty(Stack) of
      true -> 0;
      false -> {Entry, _} = mce_behav_stackOps:pop(Stack), Entry#stackEntry.depth
    end.

addTransAndState(PrevState, Actions, State, Abstraction, Table) ->
    {AbsActions, Abstraction1} =
	mce_behav_abstractionOps:abstract_actions(Actions, Abstraction),
    {AbsState, Abstraction2} =
	mce_behav_abstractionOps:abstract_state(State, Abstraction1),
    {NewElement, SavedState} = mce_behav_tableOps:permit_state_alt(AbsState, Table),
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
		  io:format("Path depth at ~p entries~n", [Depth]),
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
      io:format("Generated states ~p; checked states ~p; relation ~p~n",
		[NumStates,CheckedStates,CheckedStates/NumStates]);
    true ->
      ok
  end,
  put(aStates,NumStates+1).

%% search_shortest_path
search_shortest_path() ->  
  get(shortest)=/=undefined.

%% Remember shortest path
remember_shortest_path(Depth,MonError,Stack) ->
  put(shortest,{Depth,{counterExample,MonError,Stack}}).

%% Set a limiter on the maximum length of paths explored
set_path_limit(Depth) ->
  put(pathLimit,Depth).

%% Compute transitions from current state; coupled with monitor
transitions(Sys, Monitor, Stack) ->
    Language = get(language),
    lists:map(fun (T) ->
		      try
			Language:commit(T)
		      of
			{Actions, NSys} -> {Actions, #monState{state=NSys, monitor=Monitor}}
		      catch
			badcode -> throw({badcode, Stack})
		      end
	      end,
	      Language:transitions(Sys)).

wait_for_all([],AllProcesses) ->
  lists:foreach(fun (P) -> exit(P,kill) end, AllProcesses);
wait_for_all(Remaining,AllProcesses) ->
  receive
    {waiting,Pid} ->
      io:format("~p finished~n",[Pid]),
      wait_for_all(lists:delete(Pid,Remaining),AllProcesses);
    {counterExample,Pid,MonError,State} ->
      io:format("~p reported counterexample~n",[Pid]),
      lists:foreach(fun (P) -> exit(P,kill) end, AllProcesses),
      throw({failedMon, MonError, State});
    {error,Pid,State} ->
      io:format("~p reported error~n",[Pid]),
      lists:foreach(fun (P) -> exit(P,kill) end, AllProcesses),
      throw({badcode,State})
  end.

  
  
