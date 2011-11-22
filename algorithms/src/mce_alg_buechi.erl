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
%%% liveness properties.

-module(mce_alg_buechi).
-export([default_conf/0,init/8,start/5]).

-behaviour(mce_behav_algorithm).

-include("monState.hrl").
-include("monitor.hrl").
-include("stackEntry.hrl").
-include("executable.hrl").
-include("process.hrl").
-include("state.hrl").
-include("mce_opts.hrl").


%%%-define(debug,true).
-include("macros.hrl").


default_conf() ->
  #mce_opts{monitor={mce_mon_buechi_true,void},small_pids=true}.

wants_processFairness({process_fairness,true}) -> true;
wants_processFairness({process_fairness,false}) -> false;
wants_processFairness(_) -> true.

init(Conf, S, Args, Stack, Monitor, Abstraction, Table, _) ->
  check_conf_sanity(Conf),
  Magic = 0,
  buechi = mce_behav_monitorOps:monitorType(Monitor),
  SM = {#monState{state=S, monitor=Monitor}, Magic},
  {ok,
   {?MODULE,
    start,
    [Args,
     mce_behav_stackOps:push(#stackEntry{state=SM}, Stack),
     Abstraction,
     Table,
     Conf]}}.

start(Args,Stack,Abstraction,Table,Conf) ->
  put(nStates,0), put(aStates,0),
  Enabled =
    case wants_processFairness(Args) of
      true ->
	mce_conf:format(verbose,"Process fairness enabled~n",[]),
	sets:new();
      false ->
	mce_conf:format(verbose,"Process fairness disabled~n",[]),
	void
    end,
  {ok,{NTable,_NAbstraction}} =
    run(Stack,Abstraction,Table,void,Enabled,Conf),
  mce_conf:format
    (normal,"*** Run ending. ~p states explored, stored states ~p.~n",
     [get(nStates),get(aStates)]),
  ?LOG("No more states to explore. Finishing.~n",[]),
  mce_result:add_stored_states
    (get(aStates),
     mce_result:add_explored_states
     (get(nStates),
      mce_result:mk_ok(NTable))).

num_states(_) ->
  ExploredStates =
    case get(nStates) of
      undefined -> void;
      Oth1 -> Oth1
    end,
  StoredStates =
    case get(aStates) of
      undefined -> void;
      Oth2 -> Oth2
    end,
  {StoredStates,ExploredStates}.

run(Stack, Abstraction, Table, LookingForAcceptingState, Enabled, Conf) ->
  mce_conf:monitor_protocol(Table,fun num_states/1),
  NStates = get(nStates),
    put(nStates, NStates + 1),
  {Entry, Rest} = mce_behav_stackOps:pop(Stack),
  {State, Magic} = Entry#stackEntry.state,
  Depth = stackDepth(Stack),
  report_path_length(Entry, Depth),
  ?LOG("~p: At state ~p~nwith Magic ~p, actions=~p~n",
       [Depth, State, Magic, Entry#stackEntry.actions]),
  Actions = Entry#stackEntry.actions,
  Mon = State#monState.monitor,
  Sys = State#monState.state,
  {AbsActions, Abstraction1} =
    mce_behav_abstractionOps:abstract_actions(Actions, Abstraction),
  {TmpAbsState, Abstraction2} =
    mce_behav_abstractionOps:abstract_state(State, Abstraction1),
  AbsState = {TmpAbsState, Magic},
  OldStack =
    mce_behav_stackOps:push
      (Entry#stackEntry{abs_system=AbsState, abs_actions=AbsActions}, Rest),
  %% Add transition (if table implementation does)
  PrevState = getPrevState(OldStack),
  Table1 =
    if PrevState =:= none ->
	put(initialState, AbsState),
	Table;
       true ->
	mce_behav_tableOps:add_trans(PrevState, AbsState, AbsActions, Table)
    end,
  case mce_behav_monitorOps:stateChange(Sys, Mon, OldStack) of
    skip ->
      {ok, {Table1,Abstraction2}};
    [] ->
      check_counterExample
	(Magic,LookingForAcceptingState,TmpAbsState,Enabled,
	 State,OldStack,Conf,Table1),
      {ok, {Table1,Abstraction2}};
    NewMonStates when is_list(NewMonStates) ->
      ?LOG("New monitor states: ~p~n", [NewMonStates]),
      {NewElement, SavedState} =
	mce_behav_tableOps:permit_state_alt(AbsState, Table1),
      if not NewElement ->
	  check_counterExample
	    (Magic,LookingForAcceptingState,TmpAbsState,Enabled,
	     State,OldStack,Conf,Table1),
	  {ok,{Table1,Abstraction2}};
	 NewElement ->
	  Table15 = mce_behav_tableOps:add_state_alt(SavedState, Table1),
	  put(aStates, get(aStates) + 1),
	  put(lastState, AbsState),
	  report_generated_states(),
	  Table2 =
	    if NStates =:= 0 ->
		mce_behav_tableOps:set_initial_state(AbsState, Table15);
	       true -> Table15
	    end,
	  {BareTransitions,NewMonStateTransitions} =
	    calculate_transitions
	      (NewMonStates,Sys,Mon,OldStack,Magic,Table2,Conf),
	  ?LOG("NewMonStateTransitions is~n~p~n", [NewMonStateTransitions]),
	  NewEnabled =
	    if Enabled =:= void ->
		Enabled;
	       Magic =:= 1 ->
		CausingObject =
		  get_causing_object(Entry#stackEntry.transition),
		AllEnabled =
		  all_enabled(BareTransitions),
		Result =
		  sets:del_element
		    (CausingObject, sets:intersection(Enabled, AllEnabled)),
		?LOG(("CausingObject=~p~n OldEnabled=~p~n AllEnabled=~p~n " ++
		      "NewEnabled=~p~n"),
		     [CausingObject, sets:to_list(Enabled),
		      sets:to_list(AllEnabled), sets:to_list(Result)]),
		Result;
	       true ->
		Enabled
	    end,
	  {ok, {Table3, Abstraction3}} =
	    lists:foldl
	      (fun ({T,{TActions,TState}},{ok,{NewTable,NewAbstraction}}) ->
		   run(mce_behav_stackOps:push
		       (#stackEntry
			{depth=Depth + 1, state=TState, actions=TActions,
			 transition=T}, OldStack),
		       NewAbstraction, NewTable,
		       LookingForAcceptingState, NewEnabled, Conf)
	       end,
	       {ok,{Table2,Abstraction2}},
	       NewMonStateTransitions),
	  %% Process fairness active?
	  EnabledPids =
	    case Enabled of
	      void -> Enabled;
	      _ -> all_enabled(BareTransitions)
	    end,
	  case {Magic, mce_behav_monitorOps:stateType(Mon)} of
	    {0, accepting} ->
	      ?LOG("Starting magic run from state~n ~p~n", [State]),
	      run(mce_behav_stackOps:push
		  (Entry#stackEntry
		   {depth=Depth + 1, state={State, 1}}, OldStack),
		  Abstraction3, Table3, TmpAbsState,
		  EnabledPids, Conf);
	    _ ->
	      {ok, {Table3, Abstraction3}}
	  end
      end;
    MonError ->
      ?LOG(("~n~nMonitor returns error ~p at state~n" ++
	    "~p~nand monitor state  ~p~n"),
	   [Other, Sys, Mon]),
      mce_result:throw_result_exc
	(mce_result:mk_badmon(MonError,OldStack,Table1,Conf))
  end.

check_counterExample(Magic,LookingForAcceptingState,AbsState,Enabled,
		     State,Stack,Conf,Table) ->
  CounterExampleFound =
    Magic =:= 1 andalso
    LookingForAcceptingState =:= AbsState 
    andalso (Enabled =:= void orelse sets:size(Enabled) =:= 0),
  if CounterExampleFound ->
      %% We have found a counterexample, report it
      ?LOG("Found a counterexample!~n", []),
      ?LOG("Sys=~p~n", [Sys]),
      ?LOG("State: ~p~n", [State]),
      mce_result:throw_result_exc
	(mce_result:mk_badloop(State,strip_magic(Stack),Conf,Table));
     true ->
      ?LOG("State already seen...forgetting it~n", []),
      ok
  end.

calculate_transitions(NewMonStates,Sys,Mon,OldStack,Magic,Table,Conf) ->
  BareTransitions =
    mce_conf:transitions(Sys, Conf),
  NewTransitions =
    case BareTransitions of
      [] -> [void]; %% A stuttering transition here
      _ -> BareTransitions
    end,
  NewMonStateTransitions =
    lists:flatmap
      (fun (T) ->
	   lists:map
	     (fun (NewMonState) ->
		  {NewActions, NewState} =
		    case T of
		      void ->
			{[mce_actions:mk(stutter)], Sys};
		      _ ->
			try
			  mce_conf:commit(T, NewMonState, Conf) of
			  Result -> Result
			catch
			  {result_exc,Result} ->
			    mce_result:throw_result_exc
			      (mce_result:add_table
			       (Table,
				mce_result:add_stack
				(gen_error_stack
				 (Sys,Mon,strip_magic(OldStack)),
				 Result),Conf))
			end
		    end,
		  {T,
		   {NewActions,
		    {#monState{state=NewState, monitor=NewMonState},
		     Magic}}}
	      end,
	      NewMonStates)
       end, NewTransitions),
  {BareTransitions,NewMonStateTransitions}.

gen_error_stack(State, Monitor, Stack) ->
  Depth = stackDepth(Stack),
  Actions = lists:reverse(get(actions)),
  mce_behav_stackOps:push
    (#stackEntry
     {depth=Depth+1,
      state=#monState
      {state=mce_erl:void_state(State),monitor=Monitor},
      actions=Actions},
     Stack).

get_causing_object({exec,Exec,_}) ->
  (Exec#executable.process)#process.pid;
get_causing_object({commMove,{FromNode,ToNode,_,_},_}) ->
  {FromNode,ToNode};
get_causing_object(_) -> void.
  
all_enabled(Transitions) ->
  lists:foldl
    (fun (T,L) ->
	 case T of
	   {exec,Exec,_} -> 
	     P = Exec#executable.process,
	     case is_active(P#process.status) of
	       true ->
		 sets:add_element(P#process.pid,L);
	       false ->
		 L
	     end;
	   {commMove,{FromNode,ToNode,_,_},_} ->
	     sets:add_element({FromNode,ToNode},L);
	   _ -> L
	 end
     end,sets:new(),Transitions).

is_active(runnable) -> true;
is_active({timer,_}) -> true;
is_active(receivable) -> true;
is_active(_) -> false.

stackDepth(Stack) ->
  case mce_behav_stackOps:is_empty(Stack) of
    true -> 0;
    false -> {Entry,_} = mce_behav_stackOps:pop(Stack), Entry#stackEntry.depth
  end.

getPrevState(Stack) ->
  {_, Rest} = mce_behav_stackOps:pop(Stack),
  case mce_behav_stackOps:is_empty(Rest) of
    true -> none;
    false ->
      {Entry,_}=mce_behav_stackOps:pop(Rest),Entry#stackEntry.abs_system
  end.

strip_magic(Stack) ->
  case mce_behav_stackOps:is_empty(Stack) of
    true ->
      Stack;
    false ->
      {Entry, Rest} = mce_behav_stackOps:pop(Stack),
      {State, _Magic} = Entry#stackEntry.state,
      mce_behav_stackOps:push(Entry#stackEntry{state=State}, strip_magic(Rest))
  end.

%% Report on path length
report_path_length(Entry, Depth) ->
  if Depth rem 500 =:= 0, Depth > 0 ->
      Report =
	case get(path_depth) of
	  undefined ->
	    true;
	  N ->
	    Depth > N
	end,
      if Report ->
	  mce_conf:format(normal,"Path depth at ~p entries~n", [Depth]),
	  put(path_depth, Depth);
	 true ->
	  ok
      end;
     true ->
      ok
  end,
  if Depth rem 20000 =:= 0, Depth > 0 ->
      mce_conf:format(normal,"State:~n  ~s~n",
		[mce_erl_pretty:pretty(Entry#stackEntry.state)]);
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
  end.

check_conf_sanity(Conf) ->
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
      io:format("Warning: algorithm mce_alg_buechi cannot calculuate the "++
		"shortest path to error (on the todo list)~n",[]);
    _ ->
      ok
  end.
      
