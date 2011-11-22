-module(check_scenarios).

-export([check_scenario/2,check_scenarios/2]).
-export([analyze_error/0]).

-include("mce_opts.hrl").
-include("stack.hrl").
-include("stackEntry.hrl").

%% Check a single scenario
check_scenario(Scenario,Monitor) ->
  case succeeds(Scenario,Monitor,normal) of
    true -> 
       ok;
    {no,Depth} ->
       output("~nScenario~n  ~p~nfails at depth ~p.~n",[Scenario,Depth])
  end.
				 
%%% Check a number of scenarios against a monitor
check_scenarios(Scenarios,Monitor) ->
  case 
    lists:foldl
    (fun (Scenario,Result) ->
	 output(".",[]),
	 case succeeds(Scenario,Monitor,error) of
	   true -> Result;
	   {no,Depth} ->
	     output("~nScenario~n  ~p~nfails at depth ~p.~n",[Scenario,Depth]),
	     case Result of
	       true -> {false,Scenario,Depth,get_result()};
	       OldResult={false,Scenario1,Depth1,Result1} ->
		 if Depth<Depth1 -> {false,Scenario,Depth,get_result()};
		    true -> OldResult
		 end
	     end
	 end
     end, true, Scenarios) of
    true ->
      output("~nAll tests succeeded.~n",[]);
    {false,Scenario,Depth,Result} ->
      save_result(Result),
      output
	("~nOne or more tests failed: shortest counterexample ~p frames "++
	 "~nfor scenario~n  ~p.~n",
	 [Depth,Scenario])
  end.

%%% Pretty print stack leading to error
analyze_error() ->
  Error = mce_erl:apply(erlang,get,[result]),
  Stack = mce_result:stack(Error),
  Actions = allActions(Stack),
  printActions(Actions).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

succeeds(Scenario,Monitor,ChatterLevel) ->
  scenario_succeeds(Scenario,Monitor,ChatterLevel),
  Result = mce_erl:apply(erlang,get,[result]),
  case mce_result:is_error(Result) of
    true ->
      case mce_result:is_code_error(Result) of
	true ->
	  {no,failure_depth(mce_result:stack(Result))};
	false -> 
	  Error = mce_result:error(Result),
	  case mce_result:error_type(Error) of
	    exception ->
	      output
		("Internal exception due to ~p~n",
		 [mce_result:exception_reason(Error)]),
	      output
		("Stacktrace:~n  ~p~n",
		 [mce_result:exception_stacktrace(Error)]);
	    value ->
	      output
		("Internal exception with value ~p~n",
		 [mce_result:error_value(Error)])
	  end,
	  throw(bad)
      end;
    false -> true
  end.

scenario_succeeds(Scenario,Monitor,ChatterLevel) ->
  McAlgorithm =
    case mce_erl:apply(Monitor,monitorType,[]) of
      safety -> {mce_alg_safety,void};
      buechi -> {mce_alg_buechi,void}
    end,
  mce:start
    (#mce_opts
     {program={run_scenario,run_scenario,[Scenario]},
      chatter=ChatterLevel,
      algorithm={mce_alg_combine,
		 {#mce_opts{algorithm={mce_alg_simulation,void},
			    sim_actions=false,
			    is_simulation=false,
			    record_actions=true,
			    chatter=ChatterLevel,
			    scheduler={run_sched,void}},
		  #mce_opts{algorithm=McAlgorithm,
			    shortest=true,
			    monitor={Monitor,void},
			    chatter=ChatterLevel,
			    is_infinitely_fast=true}}}}).

failure_depth(void) -> 0;
failure_depth(Stack) ->
  case mce_behav_stackOps:is_empty(Stack) of
    true -> -1;
    false ->
      {Entry, _} = mce_behav_stackOps:pop(Stack),
      Entry#stackEntry.depth
  end.
  
output(Format,Args) ->
  mce_erl:apply(io,format,[Format,Args]).

get_result() ->
  mce_erl:apply(erlang,get,[result]).

save_result(Result) ->
  mce_erl:apply(erlang,put,[result,Result]).


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



