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

-module(mce_alg_safety_variant1).
-export([default_conf/0,init/8,start/4,run/4]).


-behaviour(mce_behav_algorithm).

-include("monState.hrl").
-include("monitor.hrl").
-include("stackEntry.hrl").
-include("state.hrl").
-include("abstraction.hrl").
-include("table.hrl").
-include("stack.hrl").
-include("mce_opts.hrl").


%%-define(debug,true).
-ifdef(debug).
-define(LOG(X,Y), io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-else.
-define(LOG(X,Y), true).
-endif.


default_conf() -> #mce_opts{small_pids=true}.

init(Conf, S, _, Stack, Monitor, Abstraction, Table, _) ->
  safety = mce_behav_monitorOps:monitorType(Monitor),
  SM = #monState{state=S, monitor=Monitor},
  {ok,
   {?MODULE,
    start,
    [mce_behav_stackOps:push(#stackEntry{state=SM}, Stack),
     Abstraction,
     Table,
     Conf]}}.

start(Stack, Abstraction, Table, Conf) ->
  put(nStates, 0), put(aStates, 0), put(pathLimit, mce_conf:pathLimit(Conf)),
  case mce_conf:shortest(Conf) of
    true -> erase(shortest);
    _ -> ok
  end,
  run(Stack, Abstraction, Table, Conf).

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

run(Stack, Abstraction, Table, Conf) ->
  mce_conf:monitor_protocol(Table,fun num_states/1),
  case mce_behav_stackOps:is_empty(Stack) of
    true ->
      io:format("*** Run ending. ~p states explored, stored states ~p.~n",
		[get(nStates), get(aStates)]),
      ?LOG("No more states to explore. Finishing.~n", []),
      mce_result:add_stored_states
	(get(aStates),
	 mce_result:add_explored_states
	   (get(nStates),
	    mce_result:mk_ok(Table)));
    false ->
      {Entry, Rest} = mce_behav_stackOps:pop(Stack),
      Depth = Entry#stackEntry.depth,
      case Entry#stackEntry.transitions of
	%% All transitions from current state explored
	[] -> run(Rest, Abstraction, Table, Conf);
	%% Transitions from current state remains to explore
	[T = {Actions, State}| Tail] ->
	  NewEntry =
	    #stackEntry{depth=Depth + 1, state=State,
			transition=T, actions=Actions},
	  run(mce_behav_stackOps:push
	      (NewEntry,
	       mce_behav_stackOps:push
	       (Entry#stackEntry{transitions=Tail},Rest)),
	      Abstraction, Table, Conf);
	%% A new state encountered; check monitor and generate its
	%% list of transitions
	void ->
	  report_path_length(Depth),
	  case skip_long_path(Depth) of
	    true ->
	      run(Rest, Abstraction, Table, Conf);
	    false ->
	      NStates = get(nStates),
	      put(nStates, NStates + 1),
	      State = Entry#stackEntry.state,
	      Actions = Entry#stackEntry.actions,
	      case addTransAndState(getPrevState(Stack),
				    Actions, State, Abstraction, Table) of
		{exists, Abstraction1, Table1} ->
		  run(Rest, Abstraction1, Table1, Conf);
		{new, AbsState, AbsActions, Abstraction1, Table1} ->
		  Table2 =
		    if NStates =:= 0 ->
			mce_behav_tableOps:set_initial_state(AbsState, Table1);
		       true ->
			Table1
		    end,
		  Mon = State#monState.monitor,
		  Sys = State#monState.state,
		  case mce_behav_monitorOps:stateChange(Sys, Mon, Stack) of
		    skip ->
		      run(Rest, Abstraction1, Table2, Conf);
		    {ok, NewMon} ->
		      NewEntry =
			Entry#stackEntry{actions=Actions,
					 abs_system=AbsState,
					 abs_actions=AbsActions},
		      NewStack =
			mce_behav_stackOps:push(NewEntry, Rest),
		      Transitions =
			transitions(Sys, NewMon, NewStack, Conf),
		      run(mce_behav_stackOps:push
			  (NewEntry#stackEntry{transitions=Transitions}, Rest),
			  Abstraction1, Table2, Conf);
		    MonError ->
		      ?LOG("*** Found a counterexample at depth ~p:~n ~p ***~n",
			   [Depth, MonError]),
		      Result =
			mce_result:mk_badmon(MonError,Stack,Table2,Conf),
		      case mce_conf:shortest(Conf) of
			true ->
			  remember_shortest_path(Depth, Result),
			  set_path_limit(Depth),
			  run(Rest, Abstraction1, Table2, Conf);
			false ->
			  mce_result:throw_result_exc(Result)
		      end
		  end
	      end
	  end
      end
  end.


%%%%%%%

getPrevState(Stack) ->
  {_, Rest} = mce_behav_stackOps:pop(Stack),
  case mce_behav_stackOps:is_empty(Rest) of
    true -> none;
    false ->
      {Entry, _} = mce_behav_stackOps:pop(Rest), Entry#stackEntry.abs_system
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

stackDepth(Stack) ->
  case mce_behav_stackOps:is_empty(Stack) of
    true -> 0;
    false -> {Entry, _} = mce_behav_stackOps:pop(Stack), Entry#stackEntry.depth
  end.
