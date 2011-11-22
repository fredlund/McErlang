%% @author Lars-Ake Fredlund
%% @copyright 2006-2009 Lars-Ake Fredlund
%% @doc
%% @private

-module(stop_after_relevant_order).
-language(erlang).
-export([init/1,stateChange/3,monitorType/0]).

-include("state.hrl").
-include("process.hrl").
-include("node.hrl").
-include("stackEntry.hrl").

-behaviour(mce_behav_monitor).

monitorType() -> 
  safety.

init(State) ->
  {ok,orddict:new()}.

stateChange(_,MonState,Stack) -> 
  try
    lists:foldl
    (fun (Action,CollectedState) ->
	 case interpret_action(Action) of
	   {f_button,Floor} ->
	     orddict:append(Floor,any,MonState);
	   {e_button,Elevator,Floor} ->
	     orddict:append(Floor,Elevator,MonState);
	   {stopped_at,Elevator,Floor} ->
	     has_permission_to_stop(Elevator,Floor,MonState);
	   _ ->
	     CollectedState
	 end
     end, MonState, actions(Stack)) of NewMonState -> {ok,NewMonState}
  catch Error -> {failed_monitor,Error} end.

has_permission_to_stop(Elevator,Floor,MonState) ->
  case orddict:find(Floor,MonState) of
    error -> throw({no_stop_order,Elevator,Floor,MonState});
    {ok,Orders} ->
      case lists:member(Elevator,Orders) of
	true -> orddict:store(Floor,lists:delete(Elevator,Orders),MonState);
	false ->
	  case lists:member(any,Orders) of
	    true -> orddict:store(Floor,lists:delete(any,Orders),MonState);
	    false -> throw({no_stop_order,Elevator,Floor,MonState})
	  end
      end
  end.

interpret_action(Action) ->
  try 
    send = mce_erl_actions:type(Action),
    {notify,Msg} = mce_erl_actions:get_send_msg(Action),
    Msg
  catch _:_ -> unknown end.
	  
actions(Stack) ->
  {Entry,_} = mce_behav_stackOps:pop(Stack),
  Entry#stackEntry.actions.


