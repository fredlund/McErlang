%% @author Lars-Ake Fredlund
%% @copyright 2006-2009 Lars-Ake Fredlund
%% @doc
%% @private

-module(stop_after_order).
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
  {ok,ordsets:new()}.

stateChange(_,MonState,Stack) -> 
  try
    lists:foldl
    (fun (Action,CollectedState) ->
	 case interpret_action(Action) of
	   {f_button,Floor} ->
	     format
	       ("f_button: add(~p,~p)~n",
		[Floor,CollectedState]),
	     ordsets:add_element(Floor,CollectedState);
	   {e_button,Elevator,Floor} ->
	     format
	       ("e_button(~p) add(~p,~p)~n",
		[Elevator,Floor,CollectedState]),
	     ordsets:add_element(Floor,CollectedState);
	   {stopped_at,Elevator,Floor} ->
	     format
	       ("stopped_at(~p,~p) state(~p)~n",
		[Elevator,Floor,CollectedState]),
	     case ordsets:is_element(Floor,CollectedState) of
	       true -> MonState;
	       false -> throw({no_stop_order,Elevator,Floor,CollectedState})
	     end;
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

format(Format,Args) ->
  ok.
  %%mce_erl:apply(io,format,[Format,Args]).

