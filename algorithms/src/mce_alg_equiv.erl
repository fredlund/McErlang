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

%% To fix: 
%%    - weak transition relation
%%    - split bisimulation into simulation
%%    - added state check (for testing equivalences)
%%    - what about branching bisimulation
%%    - make error traces optional and fix so that they work in the debugger

-module(mce_alg_equiv).
-export([default_conf/0,init/8,start/2]).

-behaviour(mce_behav_algorithm).

-include("monState.hrl").
-include("monitor.hrl").
-include("stackEntry.hrl").
-include("state.hrl").
-include("mce_opts.hrl").


%%-define(debug,true).
-include("macros.hrl").

-record(mstate,{stable,srtable,gtable,rtable,ttable,transtable,ltable,dtable,classifier}).

default_conf() -> #mce_opts{small_pids=true}.

init(Conf, InitState, Argument, Stack, Monitor, Abstraction, Table, _) ->
  check_conf_sanity(Conf),
  {ok,
   {?MODULE,
    start,
    [{InitState, mce_erl_opsem:initialState(Argument,Conf)},Conf]}}.

check_conf_sanity(_) ->
  ok.

init_mstate() ->
  StateTable =
    ets:new(stable,[set,public]),
  StateTableReversed =
    ets:new(stable_reversed,[set,public]),
  GTable =
    ets:new(gtable,[set,public]),
  RTable = 
    ets:new(rtable,[set,public]),
  TTable =
    ets:new(ttable,[bag,public]),
  TransTable =
    ets:new(transtable,[set,public]),
  LTable =
    ets:new(ltable,[set,public]),
  DTable =
    ets:new(dtable,[bag,public]),
  #mstate{stable = StateTable, srtable = StateTableReversed, gtable = GTable, rtable = RTable, ttable = TTable, transtable = TransTable, ltable = LTable, dtable = DTable}.

start(InitState, Conf) ->
  put(stateNumber,0),
  put(explored,0),
  put(gstates,0),
  put(rstates,0),
  put(actionNumber,0),
  MState = init_mstate(),
  Result =
    try preorder(InitState, InitState, MState, Conf) of
      Value -> io:format("Systems are equivalent~n"), Value
    catch _ -> io:format("Systems are not equivalent~n"), false end,
  io:format
    ("Explored=~p  Saved states=~p size(G)=~p  size(R)=~p Actions=~p~n",
     [get(explored),get(stateNumber),get(gstates),get(rstates),get(actionNumber)]),
  io:format
    ("stable size=~.2fMW srtable size=~.2fMW gtable size=~.2f MW rtable size=~.2f MW ttable size=~.2f MW transtable size=~.2f MW ltable size=~.2f MW ltable size=~.2f MW~n",
     [ets:info(MState#mstate.stable,memory)/(1024*1024),
      ets:info(MState#mstate.srtable,memory)/(1024*1024),
      ets:info(MState#mstate.gtable,memory)/(1024*1024),
      ets:info(MState#mstate.rtable,memory)/(1024*1024),
      ets:info(MState#mstate.ttable,memory)/(1024*1024),
      ets:info(MState#mstate.transtable,memory)/(1024*1024),
      ets:info(MState#mstate.ltable,memory)/(1024*1024),
      ets:info(MState#mstate.dtable,memory)/(1024*1024)]),
  Result.

num_states() ->
  get(explored).

report_states(MState) ->
  Explored = get(explored),
  if
    Explored>0, Explored rem 10000 == 0 ->
      io:format
	("Explored=~p  Saved states=~p size(G)=~p  size(R)=~p Actions=~p~n",
	 [Explored,get(stateNumber),get(gstates),get(rstates),get(actionNumber)]),
      io:format
	("stable size=~.2fMW srtable size=~.2fMW gtable size=~.2f MW rtable size=~.2f MW ttable size=~.2f MW transtable size=~.2f MW ltable size=~.2f MW dtable size=~.2f MW~n",
	 [ets:info(MState#mstate.stable,memory)/(1024*1024),
	  ets:info(MState#mstate.srtable,memory)/(1024*1024),
	  ets:info(MState#mstate.gtable,memory)/(1024*1024),
	  ets:info(MState#mstate.rtable,memory)/(1024*1024),
	  ets:info(MState#mstate.ttable,memory)/(1024*1024),
	  ets:info(MState#mstate.transtable,memory)/(1024*1024),
	  ets:info(MState#mstate.ltable,memory)/(1024*1024),
	  ets:info(MState#mstate.dtable,memory)/(1024*1024)]);
    true -> ok
  end.

preorder(InitState, State, MState, Conf) ->
  put(explored,get(explored)+1),
  report_states(MState),
  mce_conf:monitor_protocol(void,fun num_states/0),
  {P,Q} = State,
  SymbolicState = {Ps,Qs} = {get_state_number(P,MState),get_state_number(Q,MState)},
  io:format("preorder(~p)~n",[SymbolicState]),
  case is_member_r(SymbolicState, MState) of
    true -> {false,MState};
    false ->
      case is_member_g(SymbolicState, MState) of
	true -> {true,MState};
	false ->
	  case check_compatibles(P,Q,Conf,MState) of
	    false -> {false,MState};
	    true ->
	      MState1 = add_to_g(SymbolicState, MState),
	      {Related, NewMState, ReversedAssumptions} =
		case
		  match_transitions
		  (State, SymbolicState, true, InitState, MState1, Conf) of
		  {true, MState2, []} ->
		    match_transitions
		      (State, SymbolicState, false, InitState, MState2, Conf);
		  Other -> Other
		end,
	      if
		Related -> {true,NewMState};
		true ->
		  io:format("backtracking on ~p~n",[SymbolicState]),
		  %% Backtrack on previous choices using ReversedAssumptions
		  NewMState1 =
		    add_to_r(SymbolicState,State,InitState,NewMState,Conf),
		  NewMState2 =
		    remove_transitions_with_state(SymbolicState,NewMState1),
		  NewMState3 =
		    remove_from_g(SymbolicState,NewMState2),
		  reverse_assumptions
		    (State, SymbolicState, ReversedAssumptions, InitState, NewMState3, Conf)
	      end
	  end
      end
  end.

reverse_assumptions(State,SymbolicState, [], InitState, MState, Conf) ->
  {is_member_g(SymbolicState,MState),MState};
reverse_assumptions(State,SymbolicState,[{PrevSymState1,Label,LR,Target,PrevSymState2}|Rest],
		    InitState, MState, Conf) ->
  MState1 = inc_trans_table({Target,Label,PrevSymState2,LR},MState),
  PrevState1 = find_real_state(PrevSymState1,MState),
  PrevState2 = find_real_state(PrevSymState2,MState),
  SymbolicTarget = get_state_number(Target,MState1),
  case search({Label,Target,SymbolicTarget}, PrevState1, PrevSymState1, PrevState2, PrevSymState2, LR, InitState, MState1, Conf) of
    {false,MState2} ->
      FailedState =
	make_state(PrevState1,PrevState2,LR),
      FailedSymState =
	make_state(PrevSymState1,PrevSymState2,LR),
      AdditionalReversedAssumptions = 
	find_removals(PrevSymState1, PrevSymState2, LR, MState2),
      NewMState1 = add_to_r(FailedSymState,FailedState,InitState,MState2,Conf),
      NewMState2 = remove_transitions_with_state(FailedState,NewMState1),
      NewMState3 = remove_from_g(FailedState,NewMState2),
      reverse_assumptions
	(State,SymbolicState,AdditionalReversedAssumptions++Rest,InitState, NewMState3, Conf);
    {true,MState2} ->
      reverse_assumptions(State,SymbolicState, Rest, InitState, MState2, Conf)
  end.

check_compatibles(P,Q,Conf,MState) ->
  PTransitions = compute_transitions(P,Conf,MState),
  QTransitions = compute_transitions(Q,Conf,MState),
  AllLabelsP = ordsets:from_list(labels(PTransitions)),
  AllLabelsQ = ordsets:from_list(labels(QTransitions)),
  ordsets:is_subset(AllLabelsQ,AllLabelsP) andalso ordsets:is_subset(AllLabelsP,AllLabelsQ).

labels(Labels) ->
  lists:map (fun ({Label,_}) -> Label end, Labels).

match_transitions(State, SymbolicState, LR, InitState, MState, Conf) ->
  {P,Q} = State,
  {Ps,Qs} = SymbolicState,
  if
    LR ->
      match_transitions1
	(compute_transitions(P, Conf, MState), P, Ps, Q, Qs, LR, InitState, MState, Conf);
    true ->
      match_transitions1
	(compute_transitions(Q, Conf, MState), Q, Qs, P, Ps, LR, InitState, MState, Conf)
  end.

match_transitions1([], P, Ps, Q, Qs, LR, InitState, MState, Conf) ->
  {true, MState, []};
match_transitions1([Transition={Label,Target}|Rest], P, Ps, Q, Qs, LR, InitState, MState, Conf) ->
  SymbolicTarget = get_state_number(Target,MState),
  SymbolicTransition = {Label,Target,SymbolicTarget},
  io:format("matching_transition ~p from ~p~n",[{Label,SymbolicTarget},Ps]),
  MState1 = maybe_init_trans_table({Qs,SymbolicTarget,Label,LR}, MState),
  case search(SymbolicTransition, P, Ps, Q, Qs, LR, InitState, MState1, Conf) of
    {false,MState2} ->
      {false, MState2, find_removals(Ps, Qs, LR, MState2)};
    {true,MState2} ->
      match_transitions1(Rest, P, Ps, Q, Qs, LR, InitState, MState2, Conf)
  end.

find_removals(Ps, Qs, LR, MState) ->
  Transitions = find_transitions(make_state(Ps,Qs,LR),MState),
  lists:map 
    (fun ({{TLabel,_},TargetState}) ->
	 {Ps,
	  TLabel,LR,
	  find_real_state(get_state(TargetState,LR),MState),
	  get_other_state(TargetState,LR)}
     end,
     Transitions).
	       
get_state({P,Q},LR) -> if LR -> P; true -> Q end.
get_other_state({P,Q},LR) -> if LR -> Q; true -> P end.

compute_transitions(State, Conf, MState) ->
  Transitions =
    lists:map 
      (fun (T) -> mce_conf:commit(T, Conf) end,
       mce_conf:transitions(State, Conf)),
  %%io:format("Transitions for~n~p~nare~n~p~n",[State,Transitions]),
  classifier(Transitions,MState).

compute_transitions(State, Label, Conf, MState) ->
  lists:filter
    (fun ({Label1,State1}) -> Label==Label1 end, 
     compute_transitions(State,Conf,MState)).

classifier(Transes,MState) ->
  lists:map
    (fun ({Actions,State}) ->
	 ActionList =
	   lists:foldl
	     (fun (Action,Saved) ->
		  case mce_erl_actions:is_send(Action) orelse mce_erl_actions:is_probe(Action) of
		    true ->
		      [Action|Saved];
		    _ ->
		      Saved
		  end
	      end, [], Actions),
	 SymbolicAction = get_action_num(ActionList,MState),
	 {SymbolicAction,State}
     end, Transes).

search(Transition={Label,Target,SymbolicTarget}, P, Ps, Q, Qs, LR, InitState, MState, Conf) ->
  AllTransitions = compute_transitions(Q, Label, Conf, MState),
  ?LOG("Matching Transition~n~p~nfrom~n~p~n",[Transition,P]),
  ?LOG("Match candidates:~n~p~n",[AllTransitions]),
  NumTransitions = length(AllTransitions),
  CurrentNumber = get_trans_table_number({Qs,SymbolicTarget,Label,LR},MState),
  io:format("NumTransitions=~p CurrentNumber=~p~n",[NumTransitions,CurrentNumber]),
  if
    CurrentNumber>NumTransitions ->
      MState1 =
	all_failed_transitions(Ps, Qs, Label, LR, SymbolicTarget, MState),
      {false,MState};
    true ->
      search1
	(lists:nthtail(CurrentNumber-1,AllTransitions), Transition,
	 P, Ps, Q, Qs, LR, InitState, MState, Conf)
  end.

search1([], Transition={Label,Target,SymbolicTarget}, P, Ps, Q, Qs, LR, InitState, MState, Conf) ->
  all_failed_transitions(Ps, Qs, Label, LR, SymbolicTarget, MState),
  {false, MState};
search1([{QLabel,QTarget}|Rest], Transition={Label,Target,SymbolicTarget}, P, Ps, Q, Qs, LR, InitState, MState, Conf) ->
  add_failed_transition(Ps, Qs, Label, SymbolicTarget, QTarget, LR, MState),
  case preorder(InitState, make_state(Target,QTarget,LR), MState, Conf) of
    {false,MState1} ->
      inc_trans_table({Qs,SymbolicTarget,Label,LR}, MState),
      search1(Rest, Transition, P, Ps, Q, Qs, LR, InitState, MState1, Conf);
    {true,MState1} ->
      QSymbolicTarget = get_state_number(QTarget,MState),
      {true,
       add_transition
       (make_state(SymbolicTarget,QSymbolicTarget,LR),
	{Label,LR},
	make_state(Ps,Qs,LR),
	MState1)}
  end.
	  
make_state(P,Q,true) -> {P,Q};
make_state(P,Q,false) -> {Q,P}.


get_state_number(State,MState) ->
  case ets:lookup(MState#mstate.stable,State) of
    [] ->
      StateNumber = get(stateNumber),
      put(stateNumber,StateNumber+1),
      ets:insert(MState#mstate.stable,{State,StateNumber}),
      ets:insert(MState#mstate.srtable,{StateNumber,State}),
      StateNumber;
    [{_,StateNumber}] ->
      StateNumber
  end.

find_real_state(StateNumber,MState) ->
  case ets:lookup(MState#mstate.srtable,StateNumber) of
    [{_,State}] ->
      State
  end.

add_to_r(SymbolicState,State,InitState,MState,Conf) ->
  if
    State==InitState ->
      generate_counterexample(SymbolicState,MState,Conf),
      throw(not_related);
    true ->
      put(rstates,get(rstates)+1),
      ets:insert(MState#mstate.rtable,{SymbolicState}),
      MState
  end.

generate_counterexample(SymbolicState,MState,Conf) ->
  io:format("Generating counterexample from ~p~n",[SymbolicState]),
  AllDeadTransitions = 
    ets:match
      (MState#mstate.ttable,
       {{SymbolicState,dest},'_','_'}),
  io:format
    ("Dead transitions:~n~p~n",
     [AllDeadTransitions]),
  {ok,Table} = mce_behav_tableOps:init(mce_table_hashWithActions,void),
  Table1 = mce_behav_tableOps:add_state(SymbolicState, Table),
  Table2 = generate_counterexample(0, SymbolicState, MState, Conf, Table1),
  String = mce_dot:from_table(Table2),
  io:format("Result:~n~s~n",[String]).

generate_counterexample(N,SymbolicState,MState,Conf,Table) ->
  io:format("~sFollowing state ~p~n",[nString(N),SymbolicState]),
  {P,Q} = SymbolicState,
  case ets:lookup(MState#mstate.dtable,SymbolicState) of
    [] ->
      io:format
	("~sState ~p fails immediately~n",[nString(N),SymbolicState]),
      io:format
	("~sLeft transitions: ~p~n",
	 [nString(N),
	  lists:map
	  (fun ({Label,State}) -> {Label,get_state_number(State,MState)} end,
	   compute_transitions(find_real_state(P,MState),Conf,MState))]),
      io:format
	("~sRight transitions: ~p~n",
	 [nString(N),
	  lists:map
	  (fun ({Label,State}) -> {Label,get_state_number(State,MState)} end,
	   compute_transitions(find_real_state(Q,MState),Conf,MState))]),
      Table;
    [{_,{all_failed,Failures}}] ->
      io:format
	("~sfound failure transitions labelled by ~p~n",
	 [nString(N),lists:map(fun ({_,Label,_,_,_}) -> Label end, Failures)]),
      Table1 =
	lists:foldl
	  (fun ({_,Label,_,_,DestState},Tab) ->
	       NewTab =
		 case mce_behav_tableOps:permit_state(DestState,Tab) of
		   true ->
		     mce_behav_tableOps:add_state(DestState, Tab);
		   false ->
		     Tab
		 end,
	       mce_behav_tableOps:add_trans(SymbolicState,DestState,Label,NewTab)
	   end, Table, Failures),
      lists:foldl
	(fun ({_,_,_,_,DestState},Tab) ->
	     generate_counterexample(N+2,DestState,MState,Conf,Tab)
	 end,
	 Table1,
	 Failures);
    [Failures] -> 
      io:format("Strange: no terminated failures:~n~p~n...~n",[Failures]),
      throw(bad)
  end.
			 
nString(N) ->
  lists:duplicate(N,32).

is_member_r(State,MState) ->
  ets:lookup(MState#mstate.rtable,State)=/=[].

add_to_g(State,MState) ->
  put(gstates,get(gstates)+1),
  ets:insert(MState#mstate.gtable,{State}),
  MState.

remove_from_g(State,MState) ->
  put(gstates,get(gstates)-1),
  ets:delete(MState#mstate.gtable,State),
  MState.

is_member_g(State,MState) ->
  ets:lookup(MState#mstate.gtable,State)=/=[].

remove_transitions_with_state(State,MState) ->
  io:format("size of ttable is ~p~n",[ets:info(MState#mstate.ttable,size)]),
  AllSources = ets:lookup(MState#mstate.ttable,{State,source}),
  AllDests = ets:lookup(MState#mstate.ttable,{State,dest}),
  ets:delete(MState#mstate.ttable,{State,source}),
  ets:delete(MState#mstate.ttable,{State,dest}),
  lists:foreach
    (fun ({_,Label,Dest}) ->
	 ets:delete_object(MState#mstate.ttable,{{Dest,dest},Label,State})
     end, AllSources),
  lists:foreach
    (fun ({_,Label,Source}) ->
	 ets:delete_object(MState#mstate.ttable,{{Source,source},Label,State})
     end, AllDests),
  MState.

find_transitions({State,Label},MState) ->
  ets:lookup(MState#mstate.ttable,{State,source}).

add_transition(From,Label,To,MState) ->
  ets:insert(MState#mstate.ttable,{{From,source},Label,To}),
  ets:insert(MState#mstate.ttable,{{To,dest},Label,From}),
  %%io:format("Added transitions info ~p ~p ~p~n",[From,Label,To]),
  MState.

add_failed_transition(Ps, Qs, Label, SymbolicTarget, QTarget, LR, MState) ->
  FromState = make_state(Ps,Qs,LR),
  ToState = make_state(SymbolicTarget,get_state_number(QTarget,MState),LR),
  io:format
    ("Asserting that ~p --~p--> ~p with ~p is a failure~n",
     [Ps,Label,SymbolicTarget,Qs]),
  ets:insert(MState#mstate.dtable,{FromState,Label,SymbolicTarget,LR,ToState}),
  MState.

all_failed_transitions(Ps, Qs, Label, LR, SymbolicTarget, MState) ->
  io:format
    ("Asserting that ~p --~p--> ~p with ~p is a total failure~n",
     [Ps,Label,SymbolicTarget,Qs]),
  FromState = make_state(Ps,Qs,LR),
  AllFailures =
    ets:match_object
      (MState#mstate.dtable,{FromState,Label,SymbolicTarget,LR,'_'}),
  io:format
    ("Size is ~p~n",[length(AllFailures)]),
  ets:delete(MState#mstate.dtable,FromState),
  ets:insert(MState#mstate.dtable,{FromState,{all_failed,AllFailures}}),
  MState.

maybe_init_trans_table(Transition,MState) ->
  case ets:lookup(MState#mstate.transtable,Transition) of
    [] ->
      %%io:format("Added transitions info ~p~n",[Transition]),
      ets:insert(MState#mstate.transtable,{Transition,1});
    _ ->
      ok
  end,
  MState.

get_trans_table_number(Transition,MState) ->
  case ets:lookup(MState#mstate.transtable,Transition) of
    [{_,Number}] ->
      Number
  end.

inc_trans_table(Transition,MState) ->
  case ets:lookup(MState#mstate.transtable,Transition) of
    [{_,Number}] ->
      ets:insert(MState#mstate.transtable,{Transition,Number+1})
  end,
  MState.

get_action_num(Action,MState) ->
  case ets:lookup(MState#mstate.ltable,Action) of
    [] ->
      ActionNumber = get(actionNumber),
      put(actionNumber,ActionNumber+1),
      ets:insert(MState#mstate.ltable,{Action,ActionNumber}),
      ActionNumber;
    [{_,ActionNumber}] ->
      ActionNumber
  end.

  
