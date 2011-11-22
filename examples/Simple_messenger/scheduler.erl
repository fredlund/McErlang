-module(scheduler).

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
      SelectedNumber = random:uniform(N),
      {ok,{lists:nth(SelectedNumber,FilteredTransitions),SchedState}};
    0 -> no_transitions
  end.

reqFilter({Actions,_}) -> not(lists:any(fun start_server/1, Actions)).

start_server(Action) ->
  try after_starting_server = mce_erl_actions:get_probe_label(Action), true
  catch _:_ -> false end.

      
  

