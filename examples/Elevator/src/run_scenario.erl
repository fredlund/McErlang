%%%----------------------------------------------------------------------
%%% File    : sim_sup.erl
%%% Author  : Lars-Ake Fredlund
%%%----------------------------------------------------------------------

-module(run_scenario).

%% External exports
-export([run_scenario/1,run_scenario/3,analyze_error/0]).
-export([find_mce_opts/0]).

-include("stackEntry.hrl").

%% A scenario is a tuple {MaxFloor,MaxElevators,Experiment=[Command]}
%% where a Command is either a floor button press :
%%   {scheduler,f_button_pressed,[Floor]}
%% or an elevator button was pressed:
%%   {scheduler,e_button_pressed,[Elevator,Floor]};
%%
%% The floors are enumerated 1...MaxFloor, and elevators 1..MaxElevators
%%
%% An example invocation:
%%    run_scenario({3,2,[{scheduler,f_button_pressed,[1]}]},
%%                       {scheduler,e_button_pressed,[2,1]}]}).
%%
run_scenario(MaxFloors,MaxElevators,Experiment) ->
  run_scenario({MaxFloors,MaxElevators,Experiment}).

run_scenario({MaxFloors,MaxElevators,Experiment}) ->
  sim_sup:start_link(1,MaxFloors,MaxElevators),
  mce_erl:pause
    ({fun () -> 
	  mce_erl:probe(start_verification), 
	  run_experiment(Experiment) 
      end, []}).

run_experiment(Experiment) ->
  lists:foreach
    (fun ({Module,FunName,Args}) ->
	 receive
	   after 1 -> apply(Module,FunName,Args)
	 end
     end, Experiment).

analyze_error() ->
  Error = mce_erl:apply(erlang,get,[result]),
  Stack = mce_result:stack(Error),
  Actions = allActions(Stack),
  printActions(Actions).

allActions(Stack) ->
  case mce_behav_stackOps:is_empty(Stack) of
    true ->
      [];
    false ->
      {Entry,Rest} = mce_behav_stackOps:pop(Stack),
      allActions(Rest)++Entry#stackEntry.actions
  end.

printActions(Actions) ->
  lists:foreach
    (fun (Action) ->
	 case interpret_action(Action) of
	   unknown ->
	     ok;
	   {stopped_at,Elevator,Floor} -> 
	     output("Elevator ~p stopped at floor ~p~n",
	               [Elevator,Floor]);
           {open,Elevator} ->
	     output("Elevator ~p opened its doors~n",[Elevator]);
           {close,Elevator} ->
	     output("Elevator ~p closed its doors~n",[Elevator]);
	   {stopping,Elevator} -> 	   
	     output("Elevator ~p is stopping~n",
	               [Elevator]);
	   {approaching,Elevator,Floor} -> 	   
	     output("Elevator ~p is approaching floor ~p~n",
	               [Elevator,Floor]);
	   {passing,Elevator,Floor} -> 	   
	     output("Elevator ~p is passing floor ~p~n",
	               [Elevator,Floor]);
	   {move,Elevator,Direction} -> 	   
	     output("Elevator ~p is moving ~p~n",
	               [Elevator,Direction]);
           {e_button,Elevator,Floor} ->		       
	     output("Floor button ~p pressed in Elevator ~p~n",
	               [Floor,Elevator]);
           {f_button,Floor} ->
	     output("Floor button ~p pressed~n",[Floor]);
	   NotifyMsg ->
	     output("~p~n",[NotifyMsg])		       
	 end
     end, Actions).

interpret_action(Action) ->
  try 
    send = mce_erl_actions:type(Action),
    {notify,Msg} = mce_erl_actions:get_send_msg(Action),
    Msg
  catch _:_ -> unknown end.

output(Format,Args) ->
  mce_erl:apply(io,format,[Format,Args]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Locate "mce_opts.hrl"; do rr(run_scenario:find_mce_opts) to define.
find_mce_opts() ->
  mce:find_mce_opts().
  %% McErlangDirectory = mce:get_mcerlang_home(),
  %% McErlangDirectory++"/src/include/mce_opts.hrl".
