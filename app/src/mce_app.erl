%%% File    : mce_app.erl
%%% Author  : Hans Svensson <>
%%% Description : Callback module for McErlang-application
%%%               Provides a couple of options setting functions + interface
%%% Created : 15 Mar 2010 by Hans Svensson <>

%% Copyright (c) 2009, Hans Svensson
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



%% @author Hans Svensson <hanssv@chalmers.se>
%% @copyright 2009 Hans Svensson
%% @version 2.0

%% @doc McErlang Application - mce_app_server
%%
%% <p> The McErlang application is one way to use McErlang for model
%% checking. The application provides an interface to McErlang
%% configuration, and it keeps the configuration in its state in order
%% to easily run several different model checking runs with the same
%% configuration. This is feature is most notably used when invoking
%% McErlang from QuickCheck.</p>

-module(mce_app).

%%
%% @type program() = {Mod, Fun, Args} | Function
%%    Mod = atom()
%%    Fun = atom()
%%    Args = [term()]
%%    Function = function(). 

%% @type random_seed() = {integer(),integer(),integer()}.

%% @type mce_result(). Result record of McErlang, see the McErlang documentation.

-type program() :: {Mod :: atom(), Fun :: atom(), Args :: [term()]} | (Function :: function()).
-type random_seed() :: {integer(),integer(),integer()}.
% -type mce_result() :: 'mce_result'.record().

-behavior(application).

-include("mce_opts.hrl").

-export([start/0,start/2,stop/1]).

-export([verify/1, 
	 simulate_then_verify/1, simulate_then_verify/2, 
	 simulate/1, simulate/2, rerun/0, debug/1,
	 get_res/0, show_settings/0]).

-export([set_deadlock_checking/0, set_chatter_level/1, 
	 set_monitor/1, set_small_pids/1,
	 set_verification_algorithm/1,
	 set_shortest_path/1, set_random_explore/1, set_table_bound/1,
	 set_table/1, set_output/1, set_is_infinitely_fast/1,
	 set_time_limit/1, set_distributed_semantics/1
	]).

%% @spec start() -> 'ok' | {'error', term()}
%%
%% @doc Start the McErlang application.
-spec start() -> 'ok' | {'error', any()}.
start() ->
        %% For some reason the normal application group leader does not permit
        %% input so we remember the standard group leader and use that in 
        %% the application
        register(normal_group_leader,group_leader()),
	application:start(mcerlang).

%% @hidden
start(_Type,_StartArgs) ->
	mce_app_supv:start_link([]).

%% @spec stop(_State :: term()) -> 'ok'
%%
%% @doc Stop the McErlang application.
-spec stop(_State :: any()) -> 'ok'.
stop(_State) ->
	ok.

%% @spec show_settings() -> 'ok'
%%
%% @doc Print all the settings (in non-sorted order).
-spec show_settings() -> 'ok'.
show_settings() ->
	[io:format("~p = ~p\n",[Setting,Option]) 
	 || {Setting,Option} <- get_settings()],
	ok.

%% @hidden
default_settings() ->
	[{check_deadlock,false},
	 {table,{mce_table_hash_bounded,[256]}},
	 {verification_algorithm,mce_alg_safety},
	 {monitor,mce_mon_test},
	 {small_pids,false},
	 {chatter,none},
	 {shortest,false},
	 {output,false},
	 {is_infinitely_fast,false},
	 {time_limit,undefined},
	 {distributed_semantics,false},
	 {random,false}].

%% @hidden
%% Getting the Settings (a proplist)
get_settings() ->
	case application:get_env(mcerlang,settings) of
		{ok, Settings} ->
			Settings;
		_ -> 
			default_settings()
	end.

%% @hidden
get_settings_option(Setting) ->
	proplists:lookup(Setting,get_settings()).

%% @hidden
get_settings_option(Setting,Default) ->
	case get_settings_option(Setting) of
		none ->
			Default;
		{Setting,Option} -> 
			Option
	end.

%% Setting the settings (a proplist)
%% @hidden
set_settings(Settings) ->
	application:set_env(mcerlang,settings,Settings).

%% @hidden
set_settings_option(Setting,Option,OkValues,Default) ->
	Settings = get_settings(),
	NewSettings =
		case check_option(Option,OkValues) of
			true ->
				[{Setting,Option}  | proplists:delete(Setting,Settings)];
			_ ->
				io:format("*** Warning: Invalid option for ~p '~p'" ++ 
						  ", setting it to ~p\n",
						  [Setting,Option,Default]),
				[{Setting,Default} | proplists:delete(Setting,Settings)]
		end,
	set_settings(NewSettings).

%% @hidden
check_option(Option,OkValues) when is_list(OkValues) ->
	lists:member(Option,OkValues);
check_option(Option,OkFun) when is_function(OkFun) ->
	OkFun(Option).

%% @doc Control setting for which verification algorithm (module) is used.
%% Popular choices are <code>mce_alg_safety</code>,
%% <code>mce_alg_simulation</code>,
%% <code>mce_alg_safety_parallel</code>, etc.
%%
%% Allowed values: <code> atom() | {atom(),any()} </code>
-spec set_verification_algorithm(Option :: atom() | {atom(),any()}) -> 'ok'.
set_verification_algorithm(Option) ->
  set_settings_option
    (verification_algorithm,Option,
     fun (AlgModule) when is_atom(AlgModule) -> true;
	 ({AlgModule,_Option}) when is_atom(AlgModule) -> true;
	 (_) -> false
     end,
     undefined).

%% @doc If called the monitor <code>mce_mon_deadlock</code> is used
%% during model checking.
-spec set_deadlock_checking() -> 'ok'.
set_deadlock_checking() ->
  set_monitor(mce_mon_deadlock).

%% @doc Control setting for which monitor (module) is used.
%% Popular choices are <code>mce_mon_deadlock</code>,
%% <code>mce_mon_test</code>, etc.
%%
%% Allowed values: <code> atom() | {atom(),any()} </code>
-spec set_monitor(Option :: atom() | {atom(),any()}) -> 'ok'.
set_monitor(Option) ->
  set_settings_option
    (monitor, Option,
     fun (MonModule) when is_atom(MonModule) -> true;
	 ({MonModule,_Option}) when is_atom(MonModule) -> true;
	 (_) -> false
     end,
    mce_mon_test).

%% @doc Control setting for chatter level (verbosity), if set to
%% <code>none</code> the model checker is (almost) silent, if set to
%% <code>all</code> you will get to see everything you (did not) want
%% to see!
%%
%% Allowed values: <code> none | error | normal | all  </code>
%% @spec set_chatter_level(Option :: atom()) -> 'ok'
-spec set_chatter_level(Option :: atom()) -> 'ok'.
set_chatter_level(Option) ->
	set_settings_option(chatter, Option, [none,error,normal,all], none).

%% @doc Control setting for small pids (aggressive pid reuse), if set to
%% <code>true</code> the option <code>small_pids = true</code> is used
%% during model checking.
%%
%% Allowed values: <code> true | false </code>
%% @spec set_small_pids(Option :: boolean()) -> 'ok'
-spec set_small_pids(Option :: boolean()) -> 'ok'.
set_small_pids(Option) ->
	set_settings_option(small_pids, Option, [true,false], false).

%% @doc Control setting for shortest path search, if set to
%% <code>true</code> the model checker will continue to search for a
%% shorter (simpler) counter example when it reaches a bad state.
%%
%% Allowed values: <code> true | false </code>
%% @spec set_shortest_path(Option :: boolean()) -> 'ok'
-spec set_shortest_path(Option :: boolean()) -> 'ok'.
set_shortest_path(Option) ->
	set_settings_option(shortest, Option, [true,false], false).

%% @doc Control setting for random exploration of state space, if set
%% to <code>true</code> the model checker will randomly select a
%% branch in the state space during exploration. The normal behavior
%% (shortest path set to <code>false</code>) is a deterministic choice
%% (leftmost first)
%%
%% Allowed values: <code> true | false </code>
%% @spec set_random_explore(Option :: boolean()) -> 'ok'
-spec set_random_explore(Option :: boolean()) -> 'ok'.
set_random_explore(Option) ->
	set_settings_option(random, Option, [true,false], false).

%% @doc Control setting for output (from checked code), if set to
%% <code>true</code> the model checker will allow certain output from
%% the checked software. Otherwise no output from the checked software
%% is visible. See the McErlang documentation for exact definition of
%% what is treated as output.
%%
%% Allowed values: <code> true | false </code>
%% @spec set_output(Option :: boolean()) -> 'ok'
-spec set_output(Option :: boolean()) -> 'ok'.
set_output(Option) ->
	set_settings_option(output, Option, [true,false], false).

%% @doc Control setting for "is_infinitely_fast" (delaying of timeout
%% actions), if set to <code>true</code> the option
%% <code>is_infinitely_fast = true</code> is used during model
%% checking. See the McErlang documentation for a detailed explanation
%% of this option.
%%
%% Allowed values: <code> true | false </code>
%% @spec set_is_infinitely_fast(Option :: boolean()) -> 'ok'
-spec set_is_infinitely_fast(Option :: boolean()) -> 'ok'.
set_is_infinitely_fast(Option) ->
	set_settings_option(is_infinitely_fast, Option, [true,false], false).

%% @doc Control setting for distributed semantics (every process is
%% conceptually running on a separate node, this will have a big
%% impact on message ordering), if set to <code>true</code> the option
%% distributed semantics is used during model checking. See the
%% McErlang documentation for a full description of the distributed
%% semantics implemented by McErlang.
%%
%% Allowed values: <code> true | false </code>
%% @spec set_distributed_semantics(Option :: boolean()) -> 'ok'
-spec set_distributed_semantics(Option :: boolean()) -> 'ok'.
set_distributed_semantics(Option) ->
	set_settings_option(distributed_semantics, Option, [true,false], false).

%% @doc Control setting for model checking timeout. Time is measured
%% in <em>seconds</em>. If no counter example is found withing
%% <code>Option</code> seconds, the model checking is aborted with
%% status <code>inconclusive</code>
%%
%% Allowed values: <code> integer() </code>
%% @spec set_time_limit(Option :: integer()) -> 'ok'
-spec set_time_limit(Option :: integer()) -> 'ok'.
set_time_limit(Option) ->
	set_settings_option(time_limit,Option,fun erlang:is_integer/1,undefined).

%% @doc Control setting for state table used during model
%% checking. The allowed table types are (the format is the one used
%% by McErlang):
%% <ul>
%% <li> Hash table - <code>{mce_table_hash,[]}</code></li>
%% <li> Size bounded hash table -
%% <code>{mce_table_hash_bounded,[SizeBound]}</code> (SizeBound in MByte)</li>
%% <li> Void table - <code>{mce_table_void,[]}</code></li>
%% <li> Hash table with actions (transitions) -
%% <code>{mce_table_hashWithActions,[]}</code></li>
%% <li> Bit table - <code>{mce_table_bit,[TableSize]}</code>
%% (TableSize in Bytes)</li>
%% <li> Bit state hash table -
%% <code>{mce_table_bitHash,[TableSize]}</code> (TableSize in Bytes)
%% </li>
%% </ul>
%%
%% @spec set_table({TableType :: atom(),Arguments :: [term()]}) -> 'ok'
-spec set_table({TableType :: atom(), Args :: [any()]}) -> 'ok'.
set_table(Option) ->
	set_settings_option(table, Option, fun is_table/1, 
						{mce_table_hash_bounded,[256]}).

%% @hidden
is_table({TableType,Args}) when is_list(Args) ->
	lists:member(TableType,
				 [mce_table_bit,
				  mce_table_hash,
				  mce_table_hash_bounded,
				  mce_table_void,
				  mce_table_bitHash,
				  mce_table_hashWithActions]);
is_table(_) -> false.


%% @doc Control a special setting for table size bound for hash tables
%% usedn in model checking. The bound is in MBytes. Settin this value
%% only works if the current table type is
%% <code>mce_table_hash_bounded</code>.
%%
%% Allowed values: <code> integer() </code>
%% @spec set_table_bound(Option :: integer()) -> 'ok'
-spec set_table_bound(Value :: integer()) -> 'ok'.
set_table_bound(Value) when is_integer(Value) ->
	Settings = get_settings(),
	NewSettings = 
		case table() of
			{mce_table_hash_bounded,_} ->
				[{table,{mce_table_hash_bounded,[Value]}}
				  | proplists:delete(table,Settings)];
			_ ->
				io:format("*** Error: Bound can only be set for table " ++ 
						  " of type mce_hash_table_bounded, option ignored!\n"),
				Settings
		end,
	set_settings(NewSettings);
set_table_bound(_Value) ->
	io:format("*** Error: Bound should be an integer, option ignored!\n").


%% @doc Invoke the McErlang model checker on the last configuration
%%
%% <code>rerun</code> will use the last configuration, and invoke
%% McErlang with this configuration.
%%
%% @spec rerun() -> 'ok'
-spec rerun() -> 'ok'.
rerun() ->
    gen_server:call(mcerlang, re_verify, infinity).

%% Should make sure app is running!?
%% @doc Invoke the McErlang model checker on <code>Program</code>.
%%
%% Will use the currently set configuration and do a full model
%% checking run, using the <code>mce_alg_safety</code>-algorithm
%% during the model checking.
%%
%% @spec verify(Program :: program()) -> 'ok'
-spec verify(Program :: program()) -> 'ok'.
verify(Program) ->
  gen_server:call(mcerlang, {verify, verify_conf(Program)}, infinity).

%% @doc Invoke the McErlang model checker with combined strategy on
%% <code>Program</code>.
%%
%% Identical to <code>simulate_then_verify(Program,erlang:now())</code>.
%%
%% @spec simulate_then_verify(Program :: program()) -> 'ok'
-spec simulate_then_verify(Program :: program()) -> 'ok'.
simulate_then_verify(Program) ->
    simulate_then_verify(Program, now()).

%% @doc Invoke the McErlang model checker with combined strategy on
%% <code>Program</code>.
%%
%% Will use the currently set configuration and do a full model
%% checking run, using the <code>mce_alg_combine</code>-algorithm
%% during the model checking. This algorithm will first simulate using
%% the <code>sim_sched</code>-scheduler, that simulates until it
%% findes the probe action <code>'end_sequential'</code> (see McErlang
%% documentation for more information on probe actions). After
%% simulation has finished, McErlang starts model checking from the
%% end state using the currently set parameters. The simulation will
%% use the random seed <code>Seed</code> to guide the simulation.
%%
%% @spec simulate_then_verify(Program :: program(),Seed :: random_seed()) -> 'ok'
-spec simulate_then_verify(Program :: program(), Seed :: random_seed()) -> 'ok'.
simulate_then_verify(Program, Seed) ->
    gen_server:call(mcerlang, {verify, combined_conf(Program, Seed)}, infinity).

%% @doc Invoke the McErlang model checker with simulation strategy on
%% <code>Program</code>.
%%
%% Identical to <code>simulate(Program,erlang:now())</code>.
%%
%% @spec simulate(Program :: program()) -> 'ok'
-spec simulate(Program :: program()) -> 'ok'.
simulate(Program) ->
	simulate(Program, now()).

%% @doc Invoke the McErlang model checker with simulation strategy on
%% <code>Program</code>.
%%
%% McErlang will simulate using the
%% <code>mce_alg_simulation</code>-algorithm, that simulates using a
%% fully random scheduler. The simulation will use the seed
%% <code>Seed</code> to seed the random generator.
%%
%% @spec simulate(Program :: program(),Seed :: random_seed()) -> 'ok'
-spec simulate(Program :: program(), Seed :: random_seed()) -> 'ok'.
simulate(Program, Seed) ->
    gen_server:call(mcerlang, {verify, simulation_conf(Program, Seed)}, infinity).

%% @doc Invoke the McErlang model checker with simulation strategy on
%% <code>Program</code>.
%%

%% @spec debug(Program :: program()) -> 'ok'
-spec debug(Program :: program()) -> 'ok'.
debug(Program) ->
    gen_server:call(mcerlang, {verify, debug_conf(Program)}, infinity).


%% @doc Retreives the result of the last model checker run.
%%
%% @spec get_res() -> mce_result() | 'undefined'
-spec get_res() -> mce_result:mce_result() | 'undefined'.
get_res() ->
	gen_server:call(mcerlang,last_res,infinity).

%% @hidden
basic_conf() ->
	#mce_opts{output = output(),
			  table = table(),
			  record_actions = true, %% Record API calls
			  small_pids = small_pids(), % dont reuse pids!
			  chatter = chatter(),     % silence!!
			  shortest = shortest(),
			  is_infinitely_fast = is_infinitely_fast(),
			  time_limit = time_limit(),
			  distributed_semantics = distributed_semantics(),
		          algorithm = verification_algorithm(),
			  %% , pathLimit = 20
			  random = random(),
			  monitor = monitor()}.

%% @hidden
verify_conf(Program) ->
	(basic_conf())#mce_opts{
	  program = Program  %% Erlang function call to model check
	 }.

%% @hidden
simulation_conf(Program,Seed) ->
	(basic_conf())#mce_opts{
	  program = Program,  %% Erlang function call to model check
	  seed = Seed,
	  algorithm = {mce_alg_simulation, void},
	  sim_actions = false,
	  is_simulation = false}.

%% @hidden
debug_conf(Program) ->
	(basic_conf())#mce_opts{
	  program = Program,  %% Erlang function call to model check
	  algorithm = {mce_alg_debugger, void},
	  sim_actions = true}.


%% @hidden
combined_conf(Program,Seed) ->	
  #mce_opts{program = Program,         %% Erlang function call to model check
	    output = output(),
	    record_actions = true,   %% Record API calls
	    chatter = chatter(),      %% Silence!?
	    seed = Seed,
	    algorithm = {mce_alg_combine,
			 {#mce_opts{algorithm = {mce_alg_simulation, void},
				    sim_actions = false,
				    seed = Seed,
				    is_simulation = false,
				    scheduler = {sim_sched, void}},
			  (basic_conf())#mce_opts{
			   }}}}.

%%%%% Configuration help functions %%%%%
%% @hidden
monitor() ->
  get_settings_option(monitor,mce_mon_test).

%% @hidden
chatter() ->
	get_settings_option(chatter,none).

%% @hidden
small_pids() ->
	get_settings_option(small_pids,false).

%% @hidden
shortest() ->
	get_settings_option(shortest,false).

%% @hidden
random() ->
	get_settings_option(random,false).

%% @hidden
table() ->
	get_settings_option(table,{mce_table_hash_bounded,[256]}).

%% @hidden
output() ->
	get_settings_option(output,false).

%% @hidden
is_infinitely_fast() ->
	get_settings_option(is_infinitely_fast,false).

%% @hidden
distributed_semantics() ->
	get_settings_option(distributed_semantics,false).

%% @hidden
time_limit() ->
	get_settings_option(time_limit,undefined).

%% @hidden
verification_algorithm() ->
	get_settings_option(verification_algorithm,mce_alg_safety).
