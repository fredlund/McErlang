%% @author Lars-Ake Fredlund
%% @copyright 2006-2009 Lars-Ake Fredlund
%% @doc
%% @private

-module(skeleton_monitor).
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
  {ok,
   %% Initialise the monitor state next
   void}.

%% Modify this function -- can you use MonState to store the floor orders?
stateChange(_,MonState,Stack) -> 
  try
    lists:foldl
    (fun (Action,CollectedState) ->
	 case interpret_action(Action) of
	   {f_button,Floor} ->
	     %% Change CollectedState to take into account an floor button press
	     CollectedState;
	   {e_button,Elevator,Floor} ->
	     %% Change CollectedState to take into account an elevator button
	     CollectedState;
	   {stopped_at,Elevator,Floor} ->
	     %% Check if elevator has stop order by looking at CollectedState
	     %% ...
	     CollectedState;
	   _ ->
	     CollectedState
	 end
     end, MonState, actions(Stack)) of NewMonState -> {ok,NewMonState}
  catch Error -> {failed_monitor,Error} end.

interpret_action(Action) ->
  try 
    send = mce_erl_actions:type(Action),
    {notify,Msg} = mce_erl_actions:get_send_msg(Action),
    Msg
  catch _:_ -> unknown end.
	  
actions(Stack) ->
  {Entry,_} = mce_behav_stackOps:pop(Stack),
  Entry#stackEntry.actions.


