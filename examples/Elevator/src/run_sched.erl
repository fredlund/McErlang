-module(run_sched).

-behaviour(scheduler).

-export([init/1,choose/4,willCommit/1]).

init(StartState) -> {ok,StartState}.

willCommit(_) -> true.
  
choose(Transitions,SchedState,Monitor,Conf) ->
  FilteredTransitions =
    lists:filter
      (fun reqFilter/1,
       lists:map
       (fun (T) -> mce_erl_opsem:commit(T,Monitor,Conf) end, Transitions)),
  N =
    length(FilteredTransitions),
  if 
    N=:=0 -> no_transitions;
    true -> {ok,{lists:nth(random:uniform(N),FilteredTransitions),SchedState}}
  end.

reqFilter({Actions,_}) ->
  not(lists:any(fun start_verification/1, Actions)).

start_verification(Action) ->
  try mce_erl_actions:get_probe_label(Action)=:=start_verification
  catch _:_ -> false end.

      
  

