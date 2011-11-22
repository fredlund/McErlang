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

-module(mce_alg_safety_parallel_variant2).
-export([init/7,start/2, startup/3]).

-include("monState.hrl").
-include("monitor.hrl").
-include("stackEntry.hrl").
-include("state.hrl").


%%-define(debug,true).
-include("macros.hrl").

init(NumProcesses, S, Stack, Monitor, Abstraction, Table, _)
    when NumProcesses > 0 ->
    process_flag(trap_exit, true),
    clean_state(),
    safety = mce_behav_monitorOps:monitorType(Monitor),
    SM = #monState{state=S, monitor=Monitor},
    {_, AllProcesses} =
	mce_utils:do({NumProcesses, []},
		     fun ({N, _}) -> N > 0 end,
		     fun ({N, Procs}) ->
			     Pid = spawn_link(?MODULE, startup, [Stack, Abstraction, Table]),
			     {N - 1, [Pid| Procs]}
		     end),
    {ok,
     {?MODULE,
      start,
      [SM, AllProcesses]}}.

clean_state() ->
  timer:sleep(1000),
  loop().

loop() ->
  receive _ -> loop() after 0 -> ok end.

startup(Stack,Abstraction,Table) ->
  put(language,mcerl),
  wait(Stack,Abstraction,Table).

wait(Stack, Abstraction, Table) ->
    receive
      {check, Pid, {State, Actions}, Waiting} ->
	  try
	    ?LOG("~p: got an order to execute with Waiting=~p~n",
		 [self(), Waiting]),
	    put(nStates, 0),
	    put(aStates, 0),
	    case
	      run(mce_behav_stackOps:push(#stackEntry{depth=1, state=State, actions=Actions},
					  Stack),
		  Abstraction, Table, Waiting, Pid)
		of
	      {ok, {_, _, NewWaiting}} ->
		  ?LOG("~p waiting with ~p~n", [self(), NewWaiting]),
		  Pid ! {waiting, self(), {get(nStates), get(aStates)}, NewWaiting},
		  wait(Stack, Abstraction, Table);
	      {badmon, {counterExample, MonError, Stack}, _} ->
		  ?LOG("~p: bad monitor~n", [self()]),
		  Pid ! {counterExample, self(), MonError, stackState(Stack)},
		  exit(counterExample);
	      Other ->
		  io:format("~p: strange return value ~p~n", [self(), Other]),
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
	    %%	  Pid!{error,self(),stackState(Stack)};
	    Throw ->
		io:format("~p: got a throw ~p~n", [self(), Throw]),
		io:format("Stacktrace: ~p~n", [erlang:get_stacktrace()]),
		throw(bad)
	  end;
      {coord, Pid, _} ->
	  Pid ! {coord_nok, self()},
	  wait(Stack, Abstraction, Table);
      Other ->
	  io:format("~p wait: got strange message~n  ~p~n", [self(), Other]),
	  exit(wait)
    end.

stackState(Stack) ->
    {Entry, _} = mce_behav_stackOps:pop(Stack),
    Entry#stackEntry.state.

start(State,AllProcesses) ->
  [FirstProcess|RestProcesses] = AllProcesses,
  ?LOG("Ordering ~p to check...~n",[FirstProcess]),
  FirstProcess!{check,self(),{State,[]},RestProcesses},
  supervise(FirstProcess,RestProcesses,[],[]).

supervise(Leader, Working, Unassigned, Counters) ->
    ?LOG("~p: supervise(~p,~p,~p)~n", [self(), Leader, Working, Unassigned]),
    receive
      {waiting, Pid, Counters1, Waiting} ->
	  ?LOG("~p: supervise: ~p is waiting~n", [self(), Pid]),
	  NewCounters = updCounters(Pid, Counters1, Counters),
	  if Pid =:= Leader ->
		 Working1 = minus(Working, Waiting),
		 elect_new_leader(Working1, [Leader| Waiting ++ Unassigned], NewCounters);
	     true ->
		 assign_to_leader(Leader, [Pid], lists:delete(Pid, Working),
				  Unassigned, NewCounters)
	  end;
      {'EXIT', Pid, Reason} ->
	  ?LOG("supvise: worker process ~p crashed because of ~p; aborting...~n",
	       [Pid, Reason]),
	  lists:foreach(fun (KillPid) -> exit(KillPid, kill) end,
			[Leader| Working ++ Unassigned]);
      Other ->
	  io:format("~p: supervise got strange message ~p~n", [self(), Other]),
	  exit(supervise)
    end.

minus(L,[]) -> L;
minus(L,[Wait|RestWaiting]) -> 
  minus(lists:delete(Wait,L),RestWaiting).

assign_to_leader(Leader,Pids,Working,Unassigned,Counters) ->
  ?LOG("trying to assign ~p to leader ~p, working is ~p, unassigned ~p~n",
	    [Pids,Leader,Working,Unassigned]),
  Leader!{coord, self(), Pids},
  wait_for_assign(Leader,Pids,Working,Unassigned,Counters).

wait_for_assign(Leader, Waiting, Working, Unassigned, Counters) ->
    receive
      {waiting, Leader, Counters1, Waiting1} ->
	  NewCounters = updCounters(Leader, Counters1, Counters),
	  Working1 = minus(Working, Waiting1),
	  wait_for_assign(Leader, Waiting, Working1, Waiting1 ++ Unassigned, NewCounters);
      {waiting, Other, Counters1, []} ->
	  NewCounters = updCounters(Other, Counters1, Counters),
	  wait_for_assign(Leader, Waiting, lists:delete(Other, Working), [Other| Unassigned],
			  NewCounters);
      {coord_ok, _} ->
	  if Unassigned =:= [] ->
		 supervise(Leader, Waiting ++ Working, [], Counters);
	     true ->
		 assign_to_leader(Leader, Unassigned, Waiting ++ Working, [], Counters)
	  end;
      {coord_nok, _} ->
	  elect_new_leader(Working, [Leader| Waiting ++ Unassigned], Counters);
      Other ->
	  io:format("assign_to_leader: got strange message ~p~n", [Other]),
	  exit(assign_to_leader)
    end.

elect_new_leader([],Unassigned,Counters) ->
  io:format("Computation finished; counters are:~n  ~p~n",[Counters]),
  lists:foreach(fun (Pid) -> exit(Pid,kill) end, Unassigned);
elect_new_leader([Candidate|Rest],Unassigned,Counters) ->
  ?LOG("trying to elect new leader from ~p~n",
	    [[Candidate|Rest]]),
  Candidate!{coord, self(), Unassigned},
  wait_for_leader(Candidate,Rest,Unassigned,[],Counters).

wait_for_leader(Candidate, Rest, Waiting, Unassigned, Counters) ->
    receive
      {waiting, Candidate, Counters1, []} ->
	  NewCounters = updCounters(Candidate, Counters1, Counters),
	  wait_for_leader(Candidate, Rest, Waiting, Unassigned, NewCounters);
      {waiting, Other, Counters1, []} ->
	  NewCounters = updCounters(Other, Counters1, Counters),
	  wait_for_leader(Candidate, lists:delete(Other, Rest), Waiting, [Other| Unassigned],
			  NewCounters);
      {coord_ok, _} ->
	  if Unassigned =:= [] ->
		 supervise(Candidate, Waiting ++ Rest, [], Counters);
	     true ->
		 assign_to_leader(Candidate, Unassigned, Waiting ++ Rest, [], Counters)
	  end;
      {coord_nok, _} ->
	  elect_new_leader(Rest, [Candidate| Waiting ++ Unassigned], Counters);
      Other ->
	  io:format("elect_new_leader: got strange message ~p~n", [Other]),
	  exit(elect_to_leader)
    end.

updCounters(Pid,Counter,[]) -> [{Pid,Counter}];
updCounters(Pid,{NStates1,AStates1},[{Pid,{NStates,AStates}}|Rest]) ->
  [{Pid,{NStates+NStates1,AStates+AStates1}}|Rest];
updCounters(Pid,Counter,[First|Rest]) ->
  [First|updCounters(Pid,Counter,Rest)].

run(Stack, Abstraction, Table, Waiting, CoordPid) ->
    Depth = stackDepth(Stack),
    {Entry, Rest} = mce_behav_stackOps:pop(Stack),
    report_path_length(Depth),
    case skip_long_path(Depth) of
      true -> {ok, {Table, Abstraction, Waiting}};
      false ->
	  put(nStates, get(nStates) + 1),
	  State = Entry#stackEntry.state,
	  Actions = Entry#stackEntry.actions,
	  case addTransAndState(getPrevState(Stack), Actions, State, Abstraction, Table) of
	    {exists, Abstraction1, Table1} ->
		{ok, {Table1, Abstraction1, Waiting}};
	    {new, AbsState, AbsActions, Abstraction1, Table1} ->
		Mon = State#monState.monitor,
		Sys = State#monState.state,
		case mce_behav_monitorOps:stateChange(Sys, Mon, Stack) of
		  skip ->
		      run(Rest, Abstraction1, Table1, Waiting, CoordPid);
		  {ok, NewMon} ->
		      check_transitions(transitions(Sys, NewMon, Stack),
					mce_behav_stackOps:push(Entry#stackEntry{actions=Actions, abs_system=AbsState, abs_actions=AbsActions},
								Rest),
					Abstraction1,
					Table1,
					Waiting, CoordPid);
		  MonError ->
		      ?LOG("*** Found a counterexample at depth ~p:~n  ~p ***~n",
			   [Depth, MonError]),
		      case search_shortest_path() of
			true ->
			    remember_shortest_path(Depth, MonError, Stack),
			    set_path_limit(Depth),
			    run(Rest, Abstraction1, Table1, Waiting, CoordPid);
			false ->
			    {badmon, {counterExample, MonError, Stack}, Table1}
		      end
		end
	  end
    end.

check_transitions([], _Stack, Abstraction, Table, Waiting, _CoordPid) ->
    {ok, {Table, Abstraction, Waiting}};
check_transitions([{Actions, State}| Rest],
		  Stack, Abstraction, Table, Waiting, CoordPid) ->
    Depth = stackDepth(Stack),
    NewWaiting = check_coord(Waiting),
    if NewWaiting =/= [], Rest =/= [] ->
	   [Pid| RestPids] = NewWaiting,
	   ?LOG("~p: Ordering ~p to check...~n", [self(), Pid]),
	   Pid ! {check, CoordPid, {State, Actions}, []},
	   check_transitions(Rest, Stack, Abstraction, Table, RestPids, CoordPid);
       Rest =:= [] ->
	   run(mce_behav_stackOps:push(#stackEntry{depth=Depth + 1, state=State, actions=Actions}, Stack),
	       Abstraction, Table,
	       NewWaiting, CoordPid);
       true ->
	   {ok, {NewTable, NewAbstraction, NewerWaiting}} =
	       run(mce_behav_stackOps:push(#stackEntry{depth=Depth + 1, state=State, actions=Actions}, Stack),
		   Abstraction, Table,
		   NewWaiting, CoordPid),
	   check_transitions(Rest, Stack, NewAbstraction, NewTable,
			     NewerWaiting, CoordPid)
    end.

%%%%%%%

check_coord(Waiting) ->
  receive
    {coord, Pid, MoreWaiting} ->
      Pid!{coord_ok,self()},
      Waiting++MoreWaiting;
    Other ->
      io:format("~p: check_coord: got strange message~n  ~p~n",[self(),Other]),
      exit(check_coord)
  after 0 ->
      Waiting
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
			{Actions, NSys} ->
			    {Actions, #monState{state=NSys, monitor=Monitor}}
		      catch
			badcode -> throw({badcode, Stack})
		      end
	      end,
	      Language:transitions(Sys)).
