-module(run).

-export([run/0,debug/0]).
-export([check_scenario/2]).
-export([check_scenario/1,check_scenario_inf/1]).
-export([gen_dot/0]).

-include("mce_opts.hrl").
-include("stack.hrl").
-include("stackEntry.hrl").

%% Starts simulating the Elevator example
run() ->
  mce:start
    (#mce_opts
     {program={sim_sup,start_link,[1,3,2]},
      sim_external_world=true,  %% Has to be true to enable graphic I/O
      algorithm={mce_alg_simulation,void}}).
  
%% Debug the Elevator example (run it in the McErlang debugger)
debug() ->
  DefaultScenario = scenarios:default_scenario(),
  mce:start
    (#mce_opts
     {program={run_scenario,run_scenario,[DefaultScenario]},
      sim_external_world=true,
      algorithm={mce_alg_debugger,void},
      sim_actions=true,
      is_infinitely_fast=true}).

%%% Model check a scenario against a monitor
check_scenario(Scenario,Monitor) ->
  McAlgorithm =
    case mce_erl:apply(Monitor,monitorType,[]) of
      safety -> {mce_alg_safety,void};
      buechi -> {mce_alg_buechi,void}
    end,
  mce:start
    (#mce_opts
     {program={run_scenario,run_scenario,[Scenario]},
      chatter=error,
      algorithm={mce_alg_combine,
		 {#mce_opts{algorithm={mce_alg_simulation,void},
			    sim_actions=false,
			    is_simulation=false,
			    record_actions=true,
			    chatter=error,
			    scheduler={run_sched,void}},
		  #mce_opts{algorithm=McAlgorithm,
			    shortest=true,
			    monitor={Monitor,void},
			    chatter=error,
			    is_infinitely_fast=true}}}}).

%%% Model check a scenario against a monitor
check_scenario(Scenario) ->
  mce:start
    (#mce_opts
     {program={run_scenario,run_scenario,[Scenario]},
      algorithm={mce_alg_safety,void}}).

%%% Model check a scenario against a monitor
gen_dot() ->
  mce:start
    (#mce_opts
     {program={run_scenario,run_scenario,[scenarios:default_scenario()]},
      is_infinitely_fast=true,
      monitor=
      {mce_mon_wrap,
       {fun (_,N,_) -> if N>5000 -> finished; true -> {ok,N+1} end end, 0}},
      algorithm={mce_alg_simulation,void}}).

%%% Model check a scenario against a monitor
check_scenario_inf(Scenario) ->
  mce:start
    (#mce_opts
     {program={run_scenario,run_scenario,[Scenario]},
      algorithm={mce_alg_safety,void},
      is_infinitely_fast=true}).






