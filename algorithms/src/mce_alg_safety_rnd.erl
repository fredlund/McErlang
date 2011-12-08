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

%%% Implementation of a simple random search verification algorithm for
%%% safety properties.

-module(mce_alg_safety_rnd).

-export([default_conf/0,init/8,start/4,run/5]).

-behaviour(mce_behav_algorithm).

-include("monState.hrl").
-include("monitor.hrl").
-include("stackEntry.hrl").
-include("state.hrl").
-include("mce_opts.hrl").


%%-define(debug,true).
-include("macros.hrl").


default_conf() -> #mce_opts{small_pids=true,random=true}.

init(Conf, S, Init, Stack, Monitor, Abstraction, Table, _) ->
  check_conf_sanity(Conf),
  safety = mce_behav_monitorOps:monitorType(Monitor),
  SM = #monState{state=S, monitor=Monitor},
  StartStack =
    case mce_behav_stackOps:is_empty(Stack) of
      true -> 
	mce_behav_stackOps:push(#stackEntry{state=SM}, Stack);
      false ->
	Depth = stackDepth(Stack),
	mce_behav_stackOps:push
	  (#stackEntry{state=SM,
		       depth=Depth+1,
		       actions=[mce_actions:mk(continuing)]}, Stack)
    end,
  case Init of
    {seed,{A,B,C}} ->
      random:seed(A,B,C);
    {new_size,N} ->
      put(state_limit,N);
    _ ->
      ok
  end,
  {ok,
   {?MODULE,
    start,
    [StartStack, Abstraction, Table, Conf]}}.

start(Stack, Abstraction, Table, Conf) ->
  put(nStates, 0),
  put(aStates, 0),
  put(newStates, 0),
  put(pathLimit, mce_conf:pathLimit(Conf)),
  {ok, {NTable, _NAbstraction}} = 
    run(Stack, Abstraction, Table, gb_trees:empty(), Conf),
  ?LOG("No more states to explore. Finishing.~n", []),
  report_states(normal),
  add_state_count(mce_result:mk_ok(NTable)).

add_state_count(Result) ->
  mce_result:add_stored_states
    (get(aStates),
     mce_result:add_explored_states
     (get(nStates),
      Result)).

report_states(Reason) ->
  ReasonString = 
    case Reason of
      normal -> "normally";
      exception -> "due to an exception in user code"
    end,
  mce_conf:format
    (normal,
     "~n*** Run ending ~s. ~p states explored, stored states ~p, new states ~p.~n",
     [ReasonString,get(nStates), get(aStates), get(newStates)]).

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

run(Stack, Abstraction, Table, New, Conf) ->
  mce_conf:monitor_protocol(Table,fun num_states/1),
  Depth = stackDepth(Stack),
  {Entry, Rest} = mce_behav_stackOps:pop(Stack),
  report_path_length(Entry, Depth),
  case skip_long_path(Depth) of
    true -> {ok, {Table, Abstraction}};
    false ->
      NStates = get(nStates),
      State = Entry#stackEntry.state,
      Actions = Entry#stackEntry.actions,
      Mon = State#monState.monitor,
      Sys = State#monState.state,
      case mce_behav_monitorOps:stateChange(Sys, Mon, Stack) of
	skip -> continue_running(New, Abstraction, Table, Conf);
	{ok, NewMon} ->
	  case addTransAndState
	    (getPrevState(Stack), Actions,
	     State, Abstraction, Table, New) of
	    {exists, _Abstraction1, _Table1} ->
	      io:format("Fatal error: state already exists???~n",[]),
	      throw(bad);
	    {new, AbsState, AbsActions, Abstraction1, Table1} ->
	      Table2 =
		if NStates =:= 0 ->
		    mce_behav_tableOps:set_initial_state(AbsState, Table1);
		   true -> Table1
		end,
	      NewStack =
		mce_behav_stackOps:push
		  (Entry#stackEntry
		   {abs_system=AbsState, abs_actions=AbsActions},
		   Rest),
	      try transitions(Sys, NewMon, NewStack, Table2, Conf) of
		Result ->
		  check_transitions
		    (Result, NewStack, Abstraction1, Table2, New, Conf)
	      catch
		{result_exc,Result} ->
		  ?LOG("*** Found a counterexample at depth ~p ***~n", [Depth]),
		  case mce_result:is_code_error(Result) of
		    true ->
		      case mce_conf:shortest(Conf) of
			true ->
			  remember_shortest_path(Depth, Result),
			  set_path_limit(Depth),
			  continue_running(New, Abstraction1, Table2, Conf);
			false ->
			  mce_result:throw_result_exc
			    (add_state_count(mce_result:add_monitor(NewMon,Result)))
		      end;
		    false ->
		      mce_result:throw_result_exc
			(add_state_count(mce_result:add_monitor(NewMon,Result)))
		  end
	      end
	  end;
	MonError ->
	  ?LOG("*** Found a counterexample at depth ~p:~n  ~p ***~n",
	       [Depth, MonError]),
	  Result = mce_result:mk_badmon(MonError,Stack,Table,Conf),
	  case mce_conf:shortest(Conf) of
	    true ->
	      remember_shortest_path(Depth, Result),
	      set_path_limit(Depth),
	      continue_running(New, Abstraction, Table, Conf);
	    false ->
	      mce_result:throw_result_exc(add_state_count(Result))
	  end
      end
  end.

      
continue_running(New, Abstraction, Table, Conf) ->
  case gb_trees:is_empty(New) of
    true -> {ok, {Table, Abstraction}};
    false ->
      {State,Stack} = get_random_keyvalue(New),
      NewNew = gb_trees:delete(State,New),
      {Entry,_} = mce_behav_stackOps:pop(Stack),
      case newState(Entry#stackEntry.state,Abstraction,Table) of
	true -> run(Stack, Abstraction, Table, NewNew, Conf);
	false -> continue_running(NewNew, Abstraction, Table, Conf)
      end
  end.

check_transitions([], _Stack, Abstraction, Table, New, Conf) ->
  continue_running(New, Abstraction, Table, Conf);
check_transitions(Transitions, Stack, Abstraction, Table, New, Conf) ->
  Depth = stackDepth(Stack),
  UniqueTransitions =
    lists:usort
      (fun ({_,State1}, {_,State2}) ->  State1 =< State2 end,
       Transitions),
  AllMaybeNewTransitions =
    lists:filter
      (fun ({_Actions, State}) ->
	   put(nStates, get(nStates) + 1),
	   newState(State,Abstraction,Table)
       end, UniqueTransitions),
  AllNewTransitions =
    lists:filter
      (fun ({_Actions, State}) ->
	   not(gb_trees:is_defined(State,New))
       end, 
       AllMaybeNewTransitions),
  AllStacks = 
    lists:map
     (fun ({Actions, State}) ->
	  {State,
	   mce_behav_stackOps:push
	     (#stackEntry{depth=Depth+1, state=State, actions=Actions}, Stack)}
      end, AllNewTransitions),
  case AllStacks of
    [] -> continue_running(New, Abstraction, Table, Conf);
    Elements ->
      Len = length(Elements),
      {First,[{_,Chosen}|Rest]} = lists:split(random:uniform(Len)-1,Elements),
      New1 =
	lists:foldl
	  (fun ({State1,Stack1},T) ->
	       put(newStates,get(newStates)+1),
	       gb_insert(State1,Stack1,T)
	   end, New, First),
      New2 =
	lists:foldl
	  (fun ({State1,Stack1},T) ->
	       gb_insert(State1,Stack1,T)
	   end, New1, Rest),
      run(Chosen,Abstraction,Table,New2,Conf)
  end.

%%%%%%%

gb_insert(State,Stack,T) ->
  case get(state_limit) of
    N when is_integer(N) ->
      case gb_trees:size(T) of
	M when M>=N ->
	  {OldState,_} = get_random_keyvalue(T),
	  T1 = gb_trees:delete(State,T),
	  T2 = gb_trees:insert(State,Stack,T);
	_ -> gb_trees:insert(State,Stack,T)
      end;
    undefined -> gb_trees:insert(State,Stack,T)
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
    false -> {Entry, _} = mce_behav_stackOps:pop(Stack), Entry#stackEntry.depth
  end.

newState(State,Abstraction,Table) ->
  {AbsState, _Abstraction1} =
    mce_behav_abstractionOps:abstract_state(State, Abstraction),
  {NewElement, _SavedState} =
    mce_behav_tableOps:permit_state_alt(AbsState, Table),
  NewElement.

addTransAndState(PrevState, Actions, State, Abstraction, Table, New) ->
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
      report_generated_states(New),
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
      mce_conf:format(normal,"State:~n  ~p~n", [Entry]);
     true ->
      ok
  end.

%% report_generated_states
report_generated_states(New) ->
  NumStates = get(aStates),
  CheckedStates = get(nStates),
  Limit =
    case get(state_limit) of
      N when is_integer(N) -> N;
      _ -> 10000000000
    end,
  NewSize = gb_trees:size(New),
  if
    New =/= Limit, New>0, (New rem 10000) =:= 0 ->
      mce_conf:format
	(normal,"Generated states ~p; checked states ~p; relation ~p; new ~p~n",
	 [NumStates,CheckedStates,CheckedStates/NumStates,NewSize]);
    true ->
      if
	(NumStates>0) and (NumStates rem 10000 =:= 0) ->
	  mce_conf:format
	    (normal,
	     "Generated states ~p; checked states ~p; relation ~p; new ~p~n",
	     [NumStates,CheckedStates,CheckedStates/NumStates,NewSize]);
	true ->
	  ok
      end
  end,
  put(aStates,NumStates+1).

%% Remember shortest path
remember_shortest_path(Depth,Result) ->
  put(shortest,{Depth,Result}).

%% Set a limiter on the maximum length of paths explored
set_path_limit(Depth) ->
  put(pathLimit,Depth).

%% Compute transitions from current state; coupled with monitor
transitions(Sys, Monitor, Stack, Table, Conf) ->
  lists:map
    (fun (T) ->
	 try
	   mce_conf:commit(T, Monitor, Conf)
	   of
	   {Actions, NSys} ->
	     {Actions, #monState{state=NSys, monitor=Monitor}}
	 catch
	   {result_exc,Result} ->
	     report_states(exception),
	     case mce_result:stack(Result) of
	       void ->
		 mce_result:throw_result_exc
		   (add_state_count
		    (mce_result:add_table
		     (Table,
		      mce_result:add_monitor
		      (Monitor,
		       mce_result:add_stack
		       (gen_error_stack(Sys,Monitor,Stack),Result)),Conf)));
	       _ ->
		 mce_result:throw_result_exc
		   (add_state_count(mce_result:add_monitor(Monitor,Result)))
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
  case Conf#mce_opts.sim_external_world of
    true ->
      io:format("Error: sim_external_world=true is not compatible with "++
		"the selected algorithm~n",[]),
      throw(conf_error);
    _ ->
      ok
  end.

get_random_keyvalue(T) ->
  {Size,Tree} = T,
  get_value(Tree,Size).

get_value(Tree,Max) ->
  get_value1(Tree,Max).

get_value1(nil,_) ->
  io:format("null value??~n",[]),
  throw(bad);
get_value1({Key,Value,Left,Right},Size) ->
   case random:uniform(Size) of
     1 -> {Key,Value};
     Choice ->
       case {Left,Right} of
 	{nil,nil} -> {Key,Value};
 	{nil,Right} -> get_value1(Right,Size);
 	{Left,nil} ->  get_value1(Left,Size);
 	_ ->
 	  IsEven = (Choice rem 2)==0,
 	  if IsEven -> get_value1(Right,Size);
 	     true -> get_value1(Left,Size)
 	  end
       end
  end.

