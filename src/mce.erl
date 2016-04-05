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
%% The mce module contains functions for starting the McErlang model checker.

-module(mce).
-export([start/1,apply/3,apply/4,shell/0,shell/1]).
-export([debug/3,debug/4]).
-export([get_mcerlang_home/0]).
-export([save_result/1,restore_result/1]).
-export([all_mcerlang_modules/1]).
-export([mcerlang_version/0]).
-export([result/0]).
-export([find_mce_opts/0, find_funinfo/0]).

-include("monState.hrl").
-include("mce_opts.hrl").
-include("table.hrl").

%%-define(debug,true).

-ifdef(debug).
-define(LOG(X,Y),
	io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-else.
-define(LOG(X,Y), true).
-endif.

%% @doc Runs Module:FunName(Arguments) under McErlang.
apply(Module,FunName,Arguments) ->
  start(#mce_opts{program={Module,FunName,Arguments}}).

%% @doc Runs Module:FunName(Arguments) under McErlang.
%% Options is an #mce_opts structure.
apply(Module,FunName,Arguments,Options) ->
  start(Options#mce_opts{program={Module,FunName,Arguments}}).

%% @doc Applies the McErlang debugger on Module:FunName(Arguments).
debug(Module,FunName,Arguments) ->
  start(#mce_opts
	{program={Module,FunName,Arguments},
	 algorithm={mce_alg_debugger,void}}).

%% @doc Applies the McErlang debugger on Module:FunName(Arguments).
%% Options is an #mce_opts structure.
debug(Module,FunName,Arguments,Options) ->
  start(Options#mce_opts
	{program={Module,FunName,Arguments},
	 algorithm={mce_alg_debugger,void}}).

%% @doc Starts an experimental McErlang shell.
%% Used mostly for debugging McErlang.
shell() ->
  shell(#mce_opts{}).

%% @doc Starts an experimental McErlang shell.
%% Used mostly for debugging McErlang.
%% Opts is an #mce_opts structure.
shell(Opts) ->
  %% McErlangDirectory = get_mcerlang_home(),
  %% FunInfo = McErlangDirectory ++ "/configuration/funinfo.txt",
  FunInfo = mce:find_funinfo(),
  spawn
    (?MODULE,
     start,
     [Opts#mce_opts{algorithm={mce_alg_simulation, void},
		    program={mce_erl_shell, start, [self()]},
		    notice_exits=true,
		    fail_on_exit=false,
		    funinfo=FunInfo,
		    sim_external_world=true}]),
  mce_erl_shell_eval:shell_eval().

%% @doc Returns the version number of McErlang.
mcerlang_version() ->
  try mce_version:version()  catch _:_ -> "1.0 (unknown)" end.

-record(mceMonitor,
	{pid=void,
	 ref=void,
	 conf=void,
	 shutting_down=false,
	 deadline_running=void,
	 response_deadline=void,
	 ask_deadline=void,
	 alg_deadline=void}).

%% @doc The main function for starting McErlang.
%% UserConf is an #mce_opts structure which determines which program
%% to verify, which correctness monitor to use, ...
start(UserConf) ->
  case is_record(UserConf,mce_opts) of
    true -> ok;
    false ->
      io:format
	("*** Error: the argument~n    ~p~n"++
	   "to mce:start is not a valid mce_opts record.~n"++
	   "Maybe you need to recompile the code "++
	   "that calls mce:start (or McErlang).~n~n",
	 [UserConf]),
	  save_result
	    (UserConf,
	     mce_result:mk_user_error({not_mce_opts,UserConf})),
      throw(not_mce_opts_record)
  end,
  checkProgram(UserConf),
  UConf = UserConf#mce_opts{mce_monitor=self()},
  case mce_result:get_result() of
    {ok,OldRes} -> delete_table(OldRes);
    _ -> ok
  end,
  mce_conf:put_conf(UConf),
  MonitorPid =
    self(),
  process_flag(trap_exit,true),
  {McePid,Ref} =
    spawn_monitor(fun () -> start_running(MonitorPid,UConf) end),
  link(McePid),
  monitor_mce
    (#mceMonitor
     {pid=McePid,
      ref=Ref,
      conf=UConf,
      ask_deadline=now_plus_seconds(5)}).

monitor_mce(MonInfo) ->
  MonRef = MonInfo#mceMonitor.ref,
  {DeadlineType,FirstDeadline} =
    compute_first_deadline(MonInfo),
  ?LOG
    ("First deadline is ~p of type ~p~n",
     [FirstDeadline,DeadlineType]),
  NewMonInfo = 
    MonInfo#mceMonitor{deadline_running=DeadlineType},
  ?LOG("monitor_mce: ~p~n",[MonInfo]),
  receive
    {conf,NewConf} ->
      ?LOG("got conf~n",[]),
      monitor_mce(NewMonInfo#mceMonitor{conf=NewConf});
    {new_algorithm_deadline,Secs} ->
      ?LOG("got algorithm deadline ~p~n",[Secs]),
      monitor_mce
	(NewMonInfo#mceMonitor{alg_deadline=now_plus_seconds(Secs)});
    {result,Result} ->
      ?LOG("got result~n",[]),
      erlang:demonitor(NewMonInfo#mceMonitor.ref),
      maybe_save_result(NewMonInfo#mceMonitor.conf,Result),
      maybe_run_debugger(NewMonInfo#mceMonitor.conf,Result),
      ok;
    disable_deadlines ->
      monitor_mce
	(NewMonInfo#mceMonitor
	 {ask_deadline=void,
	  response_deadline=void,
	  alg_deadline=void});
    alive ->
      ?LOG("got alive~n",[]),
      monitor_mce
	(NewMonInfo#mceMonitor
	 {ask_deadline=now_plus_seconds(5),response_deadline=void});
    {'EXIT',_,_} ->
      ?LOG("got EXIT~n",[]),
      monitor_mce(MonInfo);
    {'DOWN',MonRef,_Type,_Object,Info} ->
      ?LOG("got DOWN~n",[]),
      case Info of
	normal ->
	  ?LOG
	    ("McErlang process terminated normally; waiting for result...~n",
	     []),
	  monitor_mce(NewMonInfo);
	{Reason,Stack} ->
	  mce_conf:format
	    (normal,
	     "*** Error: McErlang process crashed because of ~p "++
	       "with stack~n~s~n~n",
	     [Reason,mce_erl_debugger:printStackTrace(2,Stack)]),
	  maybe_save_result
	    (NewMonInfo#mceMonitor.conf,
	     mce_result:mk_internal_error({crashed,Reason,Stack})),
	  ok;
	_ ->
	  mce_conf:format
	    (normal,
	     "*** Error: McErlang process crashed because of ~p~n~n",
	     [Info]),
	  maybe_save_result
	    (NewMonInfo#mceMonitor.conf,
	     mce_result:mk_internal_error({crashed,Info})),
	  ok
      end;
    UnknownMsg ->
      ?LOG("got unknown message ~p~n",[UnknownMsg]),
      monitor_mce(NewMonInfo)
  after
    erlang:max(FirstDeadline,300) ->
      check_deadlines(NewMonInfo)
  end.

compute_first_deadline(MonInfo) ->
  TimerDeadline = 
    MonInfo#mceMonitor.alg_deadline,
  AskDeadline = 
    MonInfo#mceMonitor.ask_deadline,
  ResponseDeadline =
    MonInfo#mceMonitor.response_deadline,
  deadline_min
    ([{alg,TimerDeadline},{ask,AskDeadline},{response,ResponseDeadline}]).
  
check_deadlines(MonInfo) ->
  ?LOG
    ("Deadline of type ~p not met~n",
     [MonInfo#mceMonitor.deadline_running]),
  case MonInfo#mceMonitor.deadline_running of
    alg ->
      ?LOG("ordering finish~n",[]),
      MonInfo#mceMonitor.pid!finish,
      monitor_mce
	(MonInfo#mceMonitor
	 {alg_deadline=void,response_deadline=now_plus_seconds(2)});
    ask ->
      ?LOG("ordering ask~n",[]),
      MonInfo#mceMonitor.pid!{ask,self()},
      monitor_mce
	(MonInfo#mceMonitor
	 {ask_deadline=void,response_deadline=now_plus_seconds(5)});
    response ->
      ?LOG("ordering exit~n",[]),
      mce_conf:format
	(normal,
	 "*** Error: McErlang process is not responding...terminating~n~n",
	 []),
      erlang:demonitor(MonInfo#mceMonitor.ref),
      exit(MonInfo#mceMonitor.pid,kill),
      maybe_save_result
	(MonInfo#mceMonitor.conf,
	 mce_result:mk_internal_error(not_responding)),
      ok
  end.

now_plus_seconds(Seconds) ->
  {MegaSecs,Secs,MicroSecs} = erlang:now(),
  NewSeconds =
    Secs+Seconds,
  NewMegaSecs =
    NewSeconds div 1000000,
  RemSeconds =
    NewSeconds rem 1000000,
  {MegaSecs+NewMegaSecs,RemSeconds,MicroSecs}.

now_time_to_milliseconds({NowMegaSecs,NowSecs,NowMicroSecs},{MegaSecs,Secs,MicroSecs}) ->
  {DiffMicroSecs,NewSecs} =
    if
      MicroSecs>NowMicroSecs -> {MicroSecs-NowMicroSecs,Secs};
      true -> {(1000000+MicroSecs)-NowMicroSecs,Secs-1}
    end,
  {DiffSecs,NewMegaSecs} =
    if
      NewSecs>NowSecs -> {NewSecs-NowSecs,MegaSecs};
      true -> {(1000000+NewSecs)-NowSecs,MegaSecs-1}
    end,
  DiffMegaSecs =
    NewMegaSecs-NowMegaSecs,
  DiffMegaSecs*1000000000+DiffSecs*1000+(DiffMicroSecs div 1000).

deadline_min(L) ->
  Now = erlang:now(),
  lists:foldl
    (fun ({DType,Deadline},{AccType,Acc}) ->
	 RealDeadline = 
	   if Deadline==void -> void;
	      true -> now_time_to_milliseconds(Now,Deadline)
	   end,
	 if RealDeadline==void -> {AccType,Acc};
	    Acc==infinity -> {DType,RealDeadline};
	    RealDeadline<Acc -> {DType,RealDeadline};
	    true -> {AccType,Acc}
	 end
     end, {void,infinity}, L).

start_running(MonitorPid,UserConf) ->
  case mce_result:get_result() of
    {ok,OldRes} -> delete_table(OldRes);
    _ -> ok
  end,
  mce_conf:put_conf(UserConf),
  mce_conf:format
    (normal,
     "Starting McErlang model checker version~n\"~s\" ...~n~n",
     [mcerlang_version()]),
  %% The following should maybe happen in prepare_run(),
  %% but we cannot yet handle the use in mce_alg_combine of
  %% two simulation algorithms following each other (which needs
  %% continous counters).
  mce_erl_references:reset_counters(),
  put(mcErlangDir,get_mcerlang_home()),
  Conf = mce_conf:resolve_user_conf(UserConf),
  hello(Conf#mce_opts.algorithm, Conf#mce_opts.program, Conf#mce_opts.monitor),
  Language = Conf#mce_opts.language,
  InitState = Language:initialState(Conf#mce_opts.program,Conf),
  {AlgorithmModule, InitArgs} = mce_conf:algorithm_init_args(Conf),
  {ok, {Module, Fun, Args}} =
    erlang:apply(AlgorithmModule, init, [Conf, InitState| InitArgs]),
  mce_conf:prepare_run(Conf),
  mce_conf:format
    (normal,"Conf to run with is~n~s~n~n", [mce_erl_pretty:pretty(Conf)]),
  BaseMeasurements =
    return_measurements(),
  MonitorPid!{conf,Conf},
  mce_conf:put_conf(Conf),
  PrelimResult = 
    try erlang:apply(Module, Fun, Args) of
      R ->
	case mce_result:is_mce_result(R) of
	  true ->
	    case get(shortest) of
	      {Len, ShortestResult} ->
		mce_conf:format
		  (normal,
		   "~n~n*** Property violation detected at minimum depth "++
		   "~p~n",
		   [Len]),
		ShortestResult;
	      _ -> R
	    end;
	  _ ->
	    mce_conf:format
	      (normal,
	       "~n~n*** Internal error encountered in McErlang:~n"++
	       "algorithm returned value:~n~s~n~n",
	       [mce_erl_pretty:pretty(R)]),
	    throw(fatal)
	end
    catch Exception:Reason ->
	mce_conf:format(normal,"~n",[]),
	case mce_result:is_result_exc(Exception,Reason) of
	  true ->
	    mce_result:result_from_exc(Exception,Reason);
	  false ->
	    mce_result:mk_internal_error
	      (mce_result:mk_exception_error
		 (Exception,Reason,erlang:get_stacktrace()))
	end
    end,
  DiffMeasurements = diff_measurements(BaseMeasurements),
  Result = add_measurements(DiffMeasurements,PrelimResult),
  mce_erl_rcv_agent:shutDown(),
  mce_conf:format(normal,"~n",[]),
  case mce_result:type(Result) of
    badmon ->
      mce_conf:format(normal,"~n~n*** Monitor failed~n", []),
      mce_conf:format
	(normal,"monitor error:~n~p~n",
	 [mce_result:error_value(mce_result:error(Result))]),
      maybe_print_statistics(Result);
    badloop ->
      LoopState = mce_result:state(Result),
      mce_conf:format
	(normal,"~n~n*** Counterexample loop detected through state~n~s~n",
	 [mce_erl_pretty:pretty(LoopState)]),
      mce_conf:format
	(normal,"~n~nMonitor is ~s~n",
	 [mce_erl_pretty:pretty(LoopState#monState.monitor)]),
      maybe_print_statistics(Result);
    ok ->
      mce_conf:format(normal,"~n~nExecution terminated normally.~n", []),
      maybe_print_statistics(Result);
    user_error ->
      mce_conf:format
	(normal,"~n~n*** User code generated error~n~s~n~n",
	 [print_error(Result)]),
      maybe_print_statistics(Result);
    mon_error ->
      mce_conf:format
	(normal,
	 "~n~n*** Error in monitor ~p encountered:~n~n~s~n~n",
	 [Conf#mce_opts.monitor,print_error(Result)]);
    internal_error ->
      mce_conf:format
	(normal,
	 "~n~n*** Internal error encountered in McErlang:~n~n~s~n~n" ++
	 "Consider sending us a bug report containing the file "++
	 "\"mcerlang.mcrlbug\"~ngenerated by the following command:\n"++
	 "  mce:save_result(\"mcerlang.mcrlbug\").~n~n",
	 [print_error(Result)]);
	inconclusive ->
	  mce_conf:format
	(error,"~n~n*** Inconclusive (~p)\n\n",[mce_result:inconclusive_reason(Result)]);
    Other ->
      mce_conf:format
	(error,"~n~n*** Unknown return status encountered in McErlang:~n~p~n~n",
	 [Other]),
      throw(fatal)
  end,
  maybe_report_measurements(DiffMeasurements),
  %% delete_table(Result),
  MonitorPid!{result,Result}.

%% @doc Returns a structure documenting the result of
%% a McErlang run. The module {@link mce_result} contains functions
%% to analyse the result.
result() ->
  case mce_result:get_result() of
    {ok,Result} ->
      Result;
    _ ->
      mce_conf:format
	(error,"~*** Error: Cannot retrieve verification result~n",[]),
      throw(fatal)
  end.

print_error(Result) ->
  Error = mce_result:error(Result),
  case mce_result:error_type(Error) of
    value ->
      io_lib:write(mce_result:error_value(Error));
    exception ->
      "exception "++io_lib:write(mce_result:exception_type(Error))++
	" due to reason "++io_lib:write(mce_result:exception_reason(Error))
	++"\nStack trace:\n"
	++(mce_erl_debugger:printStackTrace
	   (2,mce_result:exception_stacktrace(Error)))
  end.

delete_table(Result) ->
  case mce_result:table(Result) of
    void ->
	ok;
    Table ->
	mce_behav_tableOps:delete_table(Table),
  	erlang:garbage_collect(self()),
	ok
  end.

return_measurements() ->
  {Reductions,_} = erlang:statistics(exact_reductions),
  {Runtime,_} = erlang:statistics(runtime),
  {Wallclock,_} = erlang:statistics(wall_clock),
  {Reductions,Runtime,Wallclock}.

diff_measurements({Reductions,Runtime,Wallclock}) ->
  {NewReductions,NewRuntime,NewWallclock} = return_measurements(),
  DiffReductions = NewReductions-Reductions,
  DiffRuntime = NewRuntime - Runtime,
  DiffWallclock = NewWallclock - Wallclock,
  {DiffReductions,DiffRuntime,DiffWallclock}.

maybe_report_measurements({DiffReductions,DiffRuntime,DiffWallclock}) ->
  mce_conf:format
    (normal,
     "Reductions: ~.2f mreds; Runtime: ~.3f seconds; ~.2f elapsed seconds ~n",
     [(DiffReductions/1000)/1000,DiffRuntime/1000,DiffWallclock/1000]).

add_measurements({DiffReductions,DiffRuntime,DiffWallclock},Result) ->
  mce_result:add_reductions(DiffReductions,Result),
  mce_result:add_runtime(DiffRuntime,Result),
  mce_result:add_wallclock(DiffWallclock,Result).

maybe_print_statistics(Result) ->
  Stack =
    mce_result:stack(Result),
  StackLen =
    mce_behav_stackOps:depth(Stack),
  NumStates =
    case mce_result:stored_states(Result) of
      void ->
	case mce_result:table(Result) of
	  void ->
	    0;
	  Table when is_record(Table,table) ->
	    mce_behav_tableOps:states(Table)
	end;
      N -> N
    end,
  NumTransitions =
    case mce_result:explored_states(Result) of
      void ->
	case mce_result:table(Result) of
	  void ->
	    0;
	  T when is_record(T,table) ->
	    mce_behav_tableOps:transitions(T)
	end;
      NT -> NT-1
    end,
  StackLenStr =
    if StackLen>0 ->
	io_lib:format("Stack depth ~p entries.~n",[StackLen]);
       true -> ""
    end,
  NumStatesStr =
    if is_integer(NumStates), NumStates>0 ->
	io_lib:format("~p states",[NumStates]);
       true -> ""
    end,
  NumTransitionsStr =
    if is_integer(NumTransitions), NumTransitions>0 ->
	io_lib:format
	  ("~p transitions",[NumTransitions]);
       true -> ""
    end,
  TableStr =
    case {length(NumStatesStr)>0,length(NumTransitionsStr)>0} of
      {true,true} ->
	io_lib:format
	  ("Table has ~s and ~s.~n",
	   [NumStatesStr,NumTransitionsStr]);
      {true,false} ->
	io_lib:format
	  ("Table has ~s.~n",
	   [NumStatesStr]);
      _ ->
	""
    end,
  mce_conf:format
    (normal,
     TableStr++StackLenStr++"~n",[]).

maybe_run_debugger(Conf, Result) ->
    if Conf#mce_opts.start_debugger =:= true -> mce_erl_debugger:start(Result);
       true -> ok
    end.

maybe_save_result(Conf, Result) ->
  if
    Conf#mce_opts.save_result =:= true ->
      mce_conf:format(normal,"Access result using mce:result()~n",[]),
      case mce_result:is_error(Result) of
	true ->
	  case mce_result:type(Result) of
	    internal_error -> ok;
	    _ ->
	      mce_conf:format
		(normal,
		 "To see the counterexample "++
		   "type \"mce_erl_debugger:start(mce:result()). \"~n",
		 [])
	  end;
	false -> ok
      end,
      save_result(Conf, Result);
    true -> Result
  end.

save_result(Conf, Result) ->
  VersionResult =
    mce_result:add_erlang_version
      (erlang:system_info(system_version),
       mce_result:add_mcerlang_version
	 (mce_version:version(),Result)),
  ResultWithConf = 
    case mce_result:conf(VersionResult) of
      void -> mce_result:add_conf(Conf,VersionResult);
      _ -> VersionResult
    end,
  mce_result:save_result(ResultWithConf),
  ResultWithConf.

hello(Algorithm,Program,Monitor) ->
  {Alg,AOpts} = 
    case Algorithm of
      _ when is_atom(Algorithm) -> {Algorithm,void};
      {_,_} -> Algorithm;
      _ ->
	io:format
	  ("Algorithm specification ~p not recognizable~n",
	   [Algorithm]),
	throw({bad_algorithm,Algorithm})
    end,
  {Mon,MOpts} =
    case Monitor of
      _ when is_atom(Monitor) -> {Monitor,void};
      {_,_} -> Monitor;
      _ ->
	io:format
	  ("Monitor specification ~p not recognizable~n",
	   [Monitor]),
	throw({bad_monitor,Monitor})
    end,
  OptString =
    if 
      AOpts==void -> 
	" ";
      true -> 
	io_lib:format(" with options ~n~s~n",[mce_erl_pretty:pretty(AOpts)])
    end,
  mce_conf:format
    (normal,
     "Starting algorithm ~p~son program~n~s~n"++
     "with monitor ~p(~p)~n~n",
     [Alg,OptString,printProgram(Program),Mon,MOpts]).

checkProgram(Conf) ->
  checkProgram(Conf,Conf#mce_opts.program).

checkProgram(_Conf,{Fun,Arguments}) when is_function(Fun), is_list(Arguments) ->
  ok;
checkProgram(_Conf,{FunName,Module,Arguments}) when is_atom(FunName), is_atom(Module), is_list(Arguments) ->
  ok;
checkProgram(_Conf,Fun) when is_function(Fun) ->
  ok;
checkProgram(Conf,Other) ->
  io:format("*** Error: malformed program~n~p~n",[Other]),
  save_result
    (Conf,
     mce_result:mk_user_error({bad_program_spec,Conf})),
  throw(bad_program_spec).

printProgram({Fun,Arguments}) when is_function(Fun), is_list(Arguments) ->
  io_lib:format("~p(~s)",[Fun,printArguments(Arguments)]);
printProgram({FunName,Module,Arguments}) when is_atom(FunName), is_atom(Module), is_list(Arguments) ->
  io_lib:format("~p:~p(~s)",[FunName,Module,printArguments(Arguments)]);
printProgram(Fun) when is_function(Fun) ->
  io_lib:format("~p",[Fun]).

printArguments([]) -> "";
printArguments([Arg]) ->
  io_lib:format("~p",[Arg]);
printArguments([Arg|Rest]) ->
  io_lib:format("~p",[Arg])++","++printArguments(Rest).

%% @doc Returns the directory where McErlang resides as a string.
get_mcerlang_home() ->
  case os:getenv("MCERLANG_HOME") of
    false ->
      case code:lib_dir(mcerlang) of
	{error,_} ->
	  mce_conf:format
	    (error,"*** Error: Can't find location of McErlang on disk~n"),
	  mce_conf:format
	    (error,"Either set environment variable MCERLANG_HOME,~nor" ++ 
	     " make sure McErlang is installed as an application!~n"),
	  mce_conf:format
	    (error,
	     "Starting McErlang from a supplied script "++
	     "will set MCERLANG_HOME?~n~n"),
	  throw(mcerlang_home_not_found);
	App_dir ->
	  {app_dir,App_dir}
      end;
    Other ->
      {env_var,Other}
  end.

%% @doc Writes the result structure to a file named by the filename argument.
save_result(FileName) ->
  case result() of
    undefined ->
      mce_conf:format
	(error,"No result to save~n");
    Result ->
      NewResult =
	mce_result:add_table(void,Result,mce_conf:get_conf()),
      AllBinaries = all_mcerlang_modules(mce_result:conf(NewResult)),
      BinaryResult = mce_result:add_binaries(AllBinaries,NewResult),
      ok = file:write_file(FileName,term_to_binary(BinaryResult))
  end.

%% @doc Reads the result structure from the filename argument.
restore_result(FileName) ->
  {ok,Binary} = file:read_file(FileName),
  Result = binary_to_term(Binary),
  case mce_result:is_mce_result(Result) of
    false ->
      mce_conf:format(error,"No result stored in file~n");
    true ->
      load_binaries(Result),
      Result
  end.

load_binaries(Result) ->
  case mce_result:binaries(Result) of
    void ->
      ok;
    L when is_list(L) ->
      lists:foreach
	(fun ({Module,Binary,FileName}) ->
	     case code:load_binary(Module,FileName,Binary) of
	       {module,Module} ->
		 ok;
	       {error,Reason} ->
		 io:format
		   ("Could not load binary module ~p due to ~p~n",
		    [Module,Reason])
	     end
	 end, L)
  end.

all_mcerlang_modules(Conf) ->
  AllLoaded = code:all_loaded(),
  ConfModules =
    get_conf_modules(Conf),
  AllMcErlangModules =
    lists:filter
      (fun ({Module,_Path}) ->
	   mce_erl:compiled_with_mcerlang(Module) orelse
           (lists:member(Module,ConfModules))
       end,
       AllLoaded),
  AllBinaries = 
    lists:map
      (fun ({Module,_Path}) -> 
	   case code:get_object_code(Module) of
	     Result={_Module,_Binary,_FileName} ->
	       Result;
	     error ->
	       io:format("Could not find binary for ~p~n",[Module]),
	       throw(bad)
	   end
       end, AllMcErlangModules),
  AllBinaries.
	       
get_conf_modules(Conf) ->
  if
    Conf=:=void ->
      [];
    true ->
      {MonitorModule,_} = Conf#mce_opts.monitor,
      {AbstractionModule,_} = Conf#mce_opts.abstraction,
      {SchedulerModule,_} = Conf#mce_opts.scheduler,
      {ProgramModule,_,_} = Conf#mce_opts.program,
      [MonitorModule,AbstractionModule,SchedulerModule,ProgramModule]
  end.
      
%% @doc Returns the location of the "mce_opts.hrl" file as a string.
%% To work with McErlang it is highly recommend to let the Erlang shell
%% know about this record definition using
%% the command "rr(mce:find_mce_opts)." typed in a shell.
find_mce_opts() ->
  %% McErlangDirectory = mce:get_mcerlang_home(),
  %% McErlangDirectory++"/src/include/mce_opts.hrl".
  case mce:get_mcerlang_home() of
	  {app_dir,Dir} ->
		  filename:join([Dir,"include","mce_opts.hrl"]);
	  {env_var,Dir} ->
		  filename:join([Dir,"src","include","mce_opts.hrl"])
  end.
      
find_funinfo() ->	   
  case mce:get_mcerlang_home() of
	  {app_dir,Dir} ->
		  filename:join([Dir,"include","funinfo.txt"]);
	  {env_var,Dir} ->
		  filename:join([Dir,"configuration","funinfo.txt"])
  end.

