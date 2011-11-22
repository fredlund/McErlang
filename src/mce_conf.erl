%% Copyright (c) 2009, Lars-Ake Fredlund
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

%% @author Lars-Ake Fredlund
%% @copyright 2006-2009 Lars-Ake Fredlund
%% @doc
%% @private

%% Handling of execution configuration

-module(mce_conf).

-export([get_conf/0,put_conf/1]).
-export([resolve_conf/1,resolve_user_conf/1,resolve_user_confs/1]).
-export([default_conf/0]).
-export([algorithm_init_args/1]).
-export([transitions/2,commit/2,commit/3,prepare_run/1]).
-export([get_debugger/1,external_io_possible/1]).
-export([sim_actions/1,notice_exits/1,fail_on_exit/1,terminate/1,random/1]).
-export([wants_rpc/1,program/1,is_simulation/1,pathLimit/1,shortest/1]).
-export([get_debugger/0,external_io_possible/0]).
-export([sim_actions/0,notice_exits/0,fail_on_exit/0,terminate/0,random/0]).
-export([wants_rpc/0,program/0,is_simulation/0,pathLimit/0,shortest/0]).
-export([get_scheduler/0,get_scheduler/1]).
-export([is_recording_actions/0,is_recording_actions/1]).
-export([small_pids/0,small_pids/1,is_infinitely_fast/1,is_infinitely_fast/0]).
-export([chatter/0,chatter/1]).
-export([distributed_semantics/0,distributed_semantics/1]).
-export([sends_are_sefs/0,sends_are_sefs/1]).
-export([format/3,format/2]).
-export([get_compile_info/0]).
-export([monitor_protocol/2]).

-include("mce_opts.hrl").
-include("stackEntry.hrl").

%%-define(debug,true).

-ifdef(debug).
-define(LOG(X,Y),
	io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-else.
-define(LOG(X,Y), true).
-endif.


resolve_conf(L) ->
  Fields = record_info(fields,mce_opts),
  ResolvedConf = resolve(fields_to_funs(Fields),L++[default_conf()]),
  Language = 
    if
      ResolvedConf#mce_opts.language=:=undefined ->
	mce_conf:format
	  (error,"*** ERROR: no language defined for modelchecker~n",[]),
	throw(no_language);
      true ->
	ResolvedConf#mce_opts.language
    end,
  Conf1 =
    if ResolvedConf#mce_opts.transitions=:=undefined ->
	ResolvedConf#mce_opts{transitions=Language:get_transitions_fun()};
       true ->
	ResolvedConf
    end,
  Conf2 =
    if Conf1#mce_opts.commit=:=undefined ->
	Conf1#mce_opts{commit=Language:get_commit_fun()};
       true ->
	Conf1
    end,
  %% Check if there is a time limit
  if
    is_integer(Conf2#mce_opts.time_limit), Conf2#mce_opts.time_limit>0 ->
      (Conf2#mce_opts.mce_monitor)!
	{new_algorithm_deadline,Conf2#mce_opts.time_limit};
    true ->
      ok
  end,
  Result = Conf2,
  ?LOG
     ("~nresolve with spec~n~s~nresolved to~n~s~n",
      [mce_erl_pretty:pretty(L),mce_erl_pretty:pretty(Result)]),
  Result.

fields_to_funs(Fields) ->
  fields_to_funs(Fields,2).
fields_to_funs([],_) ->
  [];
fields_to_funs([_FieldName|Rest],N) ->
  [{fun (Rec) -> element(N,Rec) end,
    fun (Element,Rec) -> setelement(N,Rec,Element) end}|
   fields_to_funs(Rest,N+1)].

resolve(FieldFuns,[FirstConf|RestConfs]) ->
  lists:foldl
    (fun (NewConf,FoldConf) ->
	 lists:foldl
	   (fun ({Get,Update},Conf) ->
		case Get(Conf) of
		  undefined ->
		    Update(Get(NewConf),Conf);
		  _ ->
		    Conf
		end
	    end, FoldConf, FieldFuns)
     end, FirstConf, RestConfs).

resolve_user_conf(UserConf) ->
  resolve_user_confs([UserConf]).

resolve_user_confs(UserConfs) ->
  Conf0 =
    mce_conf:resolve_conf(UserConfs),
  case Conf0#mce_opts.algorithm of
    AlgorithmModule when is_atom(AlgorithmModule) ->
      ConfSpec = UserConfs++[AlgorithmModule:default_conf()],
      resolve_conf(ConfSpec);
    {AlgorithmModule, _} ->
      ConfSpec = UserConfs++[AlgorithmModule:default_conf()],
      resolve_conf(ConfSpec);
    Other ->
      mce_conf:format
	(error,"*** Error: Algorithm ~p incorrectly specified~n", [Other]),
      throw(resolve_user_confs)
  end.

default_conf() ->
  #mce_opts{random=false,             
	    shortest=false,   
	    algorithm={mce_alg_safety,ok},
	    monitor={mce_mon_test,ok},
	    terminate=false,
	    abstraction={mce_abs_norm,ok},    
	    scheduler={mce_sched_rnd,false},
	    table={mce_table_hash,[]},       
	    stack={mce_stack_list,[]},    
	    small_pids=false,           
	    notice_exits=true,         
	    fail_on_exit=true,         
	    output=false,
	    sim_actions=false,        
	    sim_keep_stack=true,      
	    sim_external_world=false, 
	    record_actions=true,
	    start_debugger=false,
	    sends_are_sefs=false,
	    save_result=true,
	    save_table=false,
	    language=mce_erl_opsem,      
	    debugger=mce_erl_debugger,
	    distributed_semantics=false,
	    is_infinitely_fast=false,
	    chatter=normal,
	    rpc=false,
	    mce_monitor=void,
	    %% funinfo=get(mcErlangDir)++"/configuration/funinfo.txt",
	    funinfo=mce:find_funinfo(),
	    is_simulation=false
	   }.

algorithm_init_args(Conf) ->
  {AlgorithmModule, AlgorithmArgs} =
    case Conf#mce_opts.algorithm of
      Result when is_atom(Result) -> {Result,void};
      Result = {_,_} ->	Result;
      _ ->
	mce_conf:format
	  (error,"*** Error: algorithm specification ~p not wellformed~n",
	   Conf#mce_opts.algorithm),
	throw(algorithm_init_args)
    end,
  {StackModule, StackArgs} = 
    permit_empty(stack,Conf#mce_opts.stack),
  {MonModule, MonArgs} = 
    permit_empty(monitor,Conf#mce_opts.monitor),
  {AbsModule, AbsArgs} =
    permit_empty(abstraction,Conf#mce_opts.abstraction),
  {TableModule, TableArgs} = 
    permit_empty(table,Conf#mce_opts.table),
  {SchedulerModule, SchedulerArgs} =
    permit_empty(scheduler,Conf#mce_opts.scheduler),
  {ok, Stack} =
    case Conf#mce_opts.saved_stack of
      undefined -> 
	mce_behav_stackOps:init(StackModule, StackArgs);
      Other ->
	_ = mce_behav_stackOps:pop(Other), 
	{ok,Other}
    end,
  {ok, Monitor} = mce_behav_monitorOps:init(MonModule, MonArgs),
  {ok, Abstraction} = mce_behav_abstractionOps:init(AbsModule, AbsArgs),
  {ok, Table} = mce_behav_tableOps:init(TableModule, TableArgs),
  {ok, Scheduler} = mce_behav_schedulerOps:init(SchedulerModule, SchedulerArgs),
  {AlgorithmModule,
   [AlgorithmArgs,Stack,Monitor,Abstraction,Table,Scheduler]}.

permit_empty(_SpecName,Name) when is_atom(Name) -> {Name,void};
permit_empty(_SpecName,{Name,Args}) when is_atom(Name) -> {Name,Args};
permit_empty(SpecName,Other) ->
  io:format
    ("~p specification ~p malformed in configuration record~n",
     [SpecName,Other]),
  throw({bad_spec,{SpecName,Other}}).

transitions(State,Conf) -> 
  Fun = Conf#mce_opts.transitions,
  Fun(State,Conf).

commit(Transition,Conf) ->
  commit(Transition,void,Conf).

commit(Transition,Monitor,Conf) ->
  Fun = Conf#mce_opts.commit,
  Fun(Transition,Monitor,Conf).

prepare_run(Conf) ->
  %% Set up seed
  {S1, S2, S3} =
    case Conf#mce_opts.seed of
      {V1, V2, V3} -> {V1, V2, V3};
      _ -> now()
    end,
  mce_random:seed(S1, S2, S3),
  if Conf#mce_opts.random =:= true ->
      format_with_conf
	(Conf,normal,"The seed is ~p~n", [mce_random:rnd_state()]);
     true ->
      ok
  end,
  %% Setup support for online translation of function calls
  erlang:put
    (compile_info,
     mce_erl_compile_info:read_file(Conf#mce_opts.funinfo)),
  
  %% Setup support for mcerlang:format
  if Conf#mce_opts.output =:= true ->
      erlang:put(output_enabled, true);
     true ->
      erlang:erase(output_enabled)
  end,
  %% Erase old saved stacktraces
  erlang:erase(stack_trace),
  %% Also we reset the mc_monitor variable
  erase(mc_monitor),
  %% Erase shortest setting
  erase(shortest),
  %% Setup support for recording actions
  if Conf#mce_opts.record_actions =:= true ->
      erlang:put(record, true);
     true ->
      erlang:erase(record)
  end,
  %% If external IO enabled and language=:=erlang start the rcv_agent
  if
    Conf#mce_opts.sim_external_world =:= true, 
    Conf#mce_opts.language =:= mce_erl_opsem ->
      mce_erl_rcv_agent:do_start(self());
    true ->
      ok
  end,
  format_with_conf
    (Conf,verbose,"~nPrepared run with conf:~n~s~n~n",
     [mce_erl_pretty:pretty(Conf)]),
  %% Finally save the conf to the process registry
  put_conf(Conf).

external_io_possible() ->
  external_io_possible(get_conf()).
external_io_possible(Conf) ->      
  Conf#mce_opts.sim_external_world=:=true.

get_debugger() ->
  get_debugger(get_conf()).
get_debugger(Conf) ->
  Conf#mce_opts.debugger.

sim_actions() ->
  sim_actions(get_conf()).
sim_actions(Conf) ->
  Conf#mce_opts.sim_actions=:=true.

is_recording_actions() ->
  get(record)=:=true.
is_recording_actions(Conf) ->
  Conf#mce_opts.record_actions=:=true.

notice_exits() ->
  notice_exits(get_conf()).
notice_exits(Conf) ->
  Conf#mce_opts.notice_exits=:=true.

fail_on_exit() ->
  fail_on_exit(get_conf()).
fail_on_exit(Conf) ->
  Conf#mce_opts.fail_on_exit=:=true.

terminate() ->
  terminate(get_conf()).
terminate(Conf) ->
  Conf#mce_opts.terminate=:=true.

random() ->
  random(get_conf()).
random(Conf) ->
  Conf#mce_opts.random=:=true.

wants_rpc() ->
  wants_rpc(get_conf()).
wants_rpc(Conf) ->
  Conf#mce_opts.rpc=:=true.

program() ->
  program(get_conf()).
program(Conf) ->
  Conf#mce_opts.program.

is_simulation() ->
  is_simulation(get_conf()).
is_simulation(Conf) ->
  Conf#mce_opts.is_simulation=:=true.

pathLimit() ->
  pathLimit(get_conf()).
pathLimit(Conf) ->
  Conf#mce_opts.pathLimit.

shortest() ->
  shortest(get_conf()).
shortest(Conf) ->
  Conf#mce_opts.shortest=:=true.

small_pids() ->
  small_pids(get_conf()).
small_pids(Conf) ->
  Conf#mce_opts.small_pids=:=true.

chatter() ->
  chatter(get_conf()).
chatter(Conf) ->
  Conf#mce_opts.chatter.

distributed_semantics() ->
  distributed_semantics(get_conf()).
distributed_semantics(Conf) ->
  Conf#mce_opts.distributed_semantics.

is_infinitely_fast() ->
  is_infinitely_fast(get_conf()).
is_infinitely_fast(Conf) ->
  Conf#mce_opts.is_infinitely_fast=:=true.

sends_are_sefs() ->
  sends_are_sefs(get_conf()).
sends_are_sefs(Conf) ->
  Conf#mce_opts.sends_are_sefs=:=true.

get_conf() ->
  Conf = get(mc_conf),
  if
    is_record(Conf,mce_opts) ->
      Conf;
    true ->
      %% io:format("*** Error: conf variable not set~n",[]),
      %% try throw(no_conf) 
      %% catch _:_ ->
         %% io:format("Stack trace:~n~p~n",[erlang:get_stacktrace()]) 
      %%end,
      throw(no_conf)
  end.

put_conf(Conf) when is_record(Conf,mce_opts) ->
  put(mc_conf,Conf),
  Conf.

get_compile_info() ->     
  erlang:get(compile_info).

get_scheduler() ->
  get_scheduler(get_conf()).

get_scheduler(Conf) ->
  Conf#mce_opts.scheduler.

format(Level,String) ->
  format_with_conf(get_conf(),Level,String).

format(Level,String,Arguments) ->
  format_with_conf(get_conf(),Level,String,Arguments).

format_with_conf(Conf,Level,String) ->
  format_with_conf(Conf,Level,String,[]).

format_with_conf(Conf,Level,String,Arguments) ->
  case permit_output(chatter(Conf),Level) of
    true -> io:format(String,Arguments);
    false -> ok
  end.

permit_output(undefined,Level) ->
  permit_output(normal,Level);
permit_output(ChatterLimit,undefined) ->
  permit_output(ChatterLimit,normal);
permit_output(none,_) ->
  false;
permit_output(all,_) ->
  true;
permit_output(verbose,_) ->
  true;
permit_output(ChatterLimit,ChatterLimit) ->
  true;
permit_output(normal,error) ->
  true;
permit_output(verbose,normal) ->
  true;
permit_output(verbose,error) ->
  true;
permit_output(_,_) ->
  false.

monitor_protocol(Arg,F) ->
  receive
    finish -> 
      {StoredStates,ExploredStates} = F(Arg),
      mce_result:throw_result_exc
	(mce_result:add_explored_states
	 (ExploredStates,
	  mce_result:add_stored_states
	  (StoredStates,mce_result:mk_timeout())));
    {ask,MceMonitor} -> 
      MceMonitor!alive
    after 0 -> ok
  end.
