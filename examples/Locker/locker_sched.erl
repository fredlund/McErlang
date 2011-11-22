-module(locker_sched).

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

  case length(FilteredTransitions) of
    N when N>0 ->
      SelectedNumber = utils:uniform(N),
      {ok,{lists:nth(SelectedNumber,FilteredTransitions),SchedState}};
    0 -> no_transitions
  end.

reqFilter({Actions,_}) -> not(lists:any(fun send2locker/1, Actions)).

send2locker(Action) ->
  case mce_erl_actions:is_send(Action) of
    true -> mce_erl_actions:get_send_pid(Action)=:=locker;
    false -> false
  end.

      
  

