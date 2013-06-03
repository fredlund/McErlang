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

-module(mce_erl_debugger).
-export([init/0,init/1,start/0,start/1,
	 saveState/2,restoreState/1]).

-export([eval_local_func/3,eval_nonlocal_func/2]).

-export([print_action/1,print_actions/2]).

-export([printTransitions/1,quit/1,help/1,choose/2,
	 printState/1,printNode/2,printEther/1,printProcess/2,
	 printStackTrace/2,
	 printNodeNames/1,printProcessNames/1,
	 back/1,back/2,forward/1,forward/2,
	 gotoStart/1,gotoFinal/1,goto/2,
	 getState/1,getNode/2,getProcess/2,
	 printFrame/1,where/1,where/2,showExecution/1,
	 run/1,step/1,run/2,step/2]).


-include("process.hrl").
-include("state.hrl").
-include("executable.hrl").
-include("node.hrl").
-include("stackEntry.hrl").
-include("monState.hrl").
-include("stack.hrl").
-include("mce_opts.hrl").


%%-define(debug,true).
-include("../../../src/include/macros.hrl").
-include("emacros.hrl").


%% @private
init() ->
  {SchedulerModule, SchedulerArgs} = mce_conf:get_scheduler(),
  {ok,Scheduler} = mce_behav_schedulerOps:init(SchedulerModule, SchedulerArgs),
  init(Scheduler).

%% When calling exportable functions in the debugger we still need a decent
%% debugger context; this routine sets it up.
init_debugger_from(Stack) ->
  init(void),
  gotoFinal(Stack).

%% @private
init(Scheduler) ->
  initDebuggerState(Scheduler).

start() ->
  Result = get(result),
  case mce_result:is_mce_result(Result) of
    true -> start(Result);
    false -> 
      io:format
	("*** Cannot start debuger: result value is not well formed~n", []),
      throw(no_stack)
  end.

start(Object) ->
  case mce_result:is_mce_result(Object) of
    true ->
      case mce_result:stack(Object) of
	void ->
	  io:format
	    ("*** Cannot start debuger: result value has no stack~n", []),
	  throw(no_stack);
	Stack ->
	  case mce_result:conf(Object) of
	    void ->
	      io:format
		("*** Cannot start debugger: "++
		 "result value has no configuration~n", []),
	      throw(no_conf);
	    Conf ->
	      io:format
		("Starting debugger with a stack trace~n"++
		 "Execution terminated with status:~n  ~s.~n~n",
		 [mce_result:print_result_type(Object)]),
	      case mce_result:is_error(Object) of
		true ->
		  io:format
		    ("************************************************************~n"++
		     "*** To show the counterexample type \"showExecution(). \"  ***~n"++
		     "*** To show it step by step type:                        ***~n"++
		     "***         \"gotoStart(). \"                              ***~n" ++
		     "***         \"printTransitions(). \"                       ***~n" ++
		     "************************************************************~n~n"),
		  case mce_result:type(Object) of
		    user_error -> ok;
		    internal_error -> ok;
		    _ -> io:format("Next transitions ignoring the monitor:~n")
		  end;
		false -> ok
	      end,
	      verify_stack(Stack),
	      mce_conf:put_conf(Conf#mce_opts{sim_actions=true}),
	      init(),
	      gotoFinal(Stack),
	      start_debugger(Stack)
	  end
      end;
    false ->
      if
	is_record(Object,stack) ->
	  verify_stack(Object),
	  init(),
	  gotoFinal(Object),
	  start_debugger(Object);
	true ->
	  io:format
	    ("*** Cannot start debugger: strange argument ~p~n",
	     [Object]),
	  throw(argument)
      end
  end.

restoreState(FileName) ->
  {ok,Binary} = file:read_file(FileName),
  case binary_to_term(Binary) of
    {debugger_state,{Stack,_Mode,Scheduler,Conf,RndState,Pos}} ->
      init(Scheduler),
      upd_pos(Pos),
      mce_conf:put_conf(Conf),
      mce_random:put_rnd_state(RndState),
      start_debugger(Stack);
    _ ->
      io:format
	("*** Cannot start debugger: no state to restore in ~p~n",
	 [FileName]),
      throw(restoreState)
  end.

verify_stack(Stack) ->
  verify_stack(Stack,true).
verify_stack(Stack,AtTop) ->
  case mce_behav_stackOps:is_empty(Stack) of
    true -> true;
    false ->
      {Entry,Rest} = mce_behav_stackOps:pop(Stack),
      Actions = Entry#stackEntry.actions,
      if
	Actions=/=[] -> verify_stack(Rest,false);
	AtTop -> verify_stack(Rest,false);
	Entry#stackEntry.depth=:=0 -> verify_stack(Rest,false);
	true -> 
	  io:format
	    ("Cannot start debugger: result stack has no actions.~n"++
	     "Enable recording of actions by setting the option "++
	     "recording_action=true~n"),
	  throw(no_actions)
      end
  end.

start_debugger(Stack) ->
  debugger_loop(Stack).

at_top_of_stack(Stack) ->
  Pos = get_pos(),
  TopPos = top_pos(Stack),
  if
    Pos =:= TopPos -> true;
    TopPos > Pos -> false
  end.

top_pos(Stack) ->
  get_depth(Stack).

is_legal_pos(Pos,Stack) ->
  Pos>=0 andalso Pos=<top_pos(Stack).

get_stack_at_position(Stack) ->
  get_stack_at_position(Stack,get_pos()).
get_stack_at_position(Stack,N) when N>=0 ->
  StackPos = get_depth(Stack),
  if
    StackPos=:=N ->
      Stack;
    StackPos<N ->
      throw(get_stack_position);
    StackPos>N ->
      get_stack_at_position(element(2,mce_behav_stackOps:pop(Stack)),N)
  end.

debugger_loop(Stack) ->
  Mode = get_mode(),
  case do_step(Mode) of
    true ->
      try pick(Stack) of
	  {ok,NewStack} ->
	    upd_mode
	      (case Mode of
		 {step,N} -> {step,N-1};
		 _ -> Mode
	       end),
	    debugger_loop(NewStack)
      catch
	no_transitions ->
	  upd_mode(stop), debugger_loop(Stack);
	Exception:Reason ->
	  case mce_result:is_result_exc(Exception,Reason) of
	    true ->
	      Result =
		mce_result:result_from_exc(Exception,Reason),
	      io:format
		("Execution terminated with status: ~n  ~s.~n~n",
		 [print_error(mce_result:error(Result))]),
	      upd_mode(stop),
	      debugger_loop(mce_result:stack(Result));
	    false ->
	      io:format
		("*** Error: exception ~p due to ~p; "++
		 "stacktrace:~n  ~p~n~n",
		 [Exception,Reason,erlang:get_stacktrace()]),
		upd_mode(stop),
	      debugger_loop(Stack)
	  end
      end;

    false ->
      io:format("~nAt stack frame ~p: transitions:~n~n",[get_pos()]),
      {Transitions, NewStack, _} = get_transitions(Stack),
      print_alternatives(NewStack, 1, Transitions),
      try readcmd(NewStack) of
	  {NewerStack,NewMode} when is_record(NewerStack,stack) -> 
	    upd_mode(NewMode),
	    debugger_loop(NewerStack)
      catch 
	quit ->
	  ok;
	Exception:Reason -> 
	  case mce_result:is_result_exc(Exception,Reason) of
	    true ->
	      Result =
		mce_result:result_from_exc(Exception,Reason),
	      io:format
		("Execution terminated with status: ~n  ~s.~n~n",
		 [print_error(mce_result:error(Result))]),
	      upd_mode(stop),
	      debugger_loop(mce_result:stack(Result));
	    false ->
	      io:format
		("*** Error: exception ~p due to ~p; stacktrace:~n  ~p~n~n",
		 [Exception,Reason,erlang:get_stacktrace()]),
	      upd_mode(stop),
	      debugger_loop(NewStack)
	  end
      end
  end.

do_step(run) ->
  true;
do_step({step,N}) when is_integer(N), N>0 ->
  true;
do_step(_) ->
  false.

readcmd(Stack) ->
  OldPos = get_pos(),
  put(debuggedState, Stack),
  put(dbgEvalState, normal),
  Bindings =
    case get(dbgBindings) of
      undefined -> [];
      OldBindings -> OldBindings
    end,
  case parse_eval(get_pos(),Bindings) of
    {value, RetVal, NewBindings} ->
      put(dbgBindings, NewBindings),
      {Transitions, NewStack, _} =
	get_transitions(Stack),
      {OrderedTransitions,Default} =
	order_transitions(Transitions,Stack,get_pos()),
      N = length(OrderedTransitions),
      if is_integer(RetVal), RetVal >= 0, RetVal =< N ->
	  if
	    RetVal=:=1, Default -> 
	      ActionsStr=print_actions(2,get_actions(get_pos(),Stack),Stack),
	      forward(NewStack,1),
	      maybe_print_actions_str(ActionsStr),
	      {NewStack,stop};
	    true ->
	      {ok,NewerStack} = pick(RetVal, OrderedTransitions, NewStack), 
	      {NewerStack,stop}
	  end;
	 true ->
	  if is_integer(RetVal), RetVal > N ->
	      io:format("*** not that many transitions ***~n~n", []);
	     true ->
	      case RetVal of
		void -> io:format("~n"), ok;
		{value,V} -> io:format("~s~n", [mce_erl_pretty:pretty(V)]);
		V -> io:format("~s~n", [mce_erl_pretty:pretty(V)])
	      end
	  end,
	  case get(dbgEvalState) of
	    quit ->
	      throw(quit);
	    {newStack, NewerStack} ->
	      {NewerStack, stop};
	    step ->
	      {NewStack, {step,1}};
	    run ->
	      {NewStack, run};
	    {step, NS} when NS > 0 ->
	      {NewStack, {step, NS}};
	    {alternative, I} when is_integer(I), I >= 0, I =< N->
	      if
		I=:=1, Default -> 
		  ActionsStr=
		    print_actions(2,get_actions(get_pos(),Stack),Stack),
		  forward(NewStack,1),
		  maybe_print_actions_str(ActionsStr),
		  {NewStack,stop};
		true ->
		  {ok,NewerStack} = pick(I, OrderedTransitions, NewStack), 
		  {NewerStack,stop}
	      end;
	    normal ->
	      case get_pos()=/=OldPos of
		true ->
		  {Transitions,NewStack,_} = get_transitions(Stack),
		  print_alternatives(NewStack,1,Transitions);
		false ->
		  ok
	      end,
	      readcmd(NewStack);
	    Other ->
	      io:format("~nCommand ~p not recognized~n", [Other]),
	      readcmd(NewStack)
	  end
      end;
    fail ->
      io:format("~n"),
      readcmd(Stack);
    Return ->
      io:format("~n"),
      io:format("Strange return value ~p~n", [Return]),
      readcmd(Stack)
  end.

get_transitions(Stack) ->
  {SaveConf,SavedConf} =
    case at_top_of_stack(Stack) of
      true ->
	case mce_conf:external_io_possible() of
	  true ->
	    {true,
	     mce_conf:put_conf
	     ((mce_conf:get_conf())#mce_opts{sim_external_world=false})};
	  false ->
	    {false,void}
	end;
      false -> {false,void}
    end,
  System =
    get_sys(get_stack_at_position(Stack)),
  Result =
    case mce_conf:transitions(System, mce_conf:get_conf()) of
      {io,IOResult,Transes} ->
	{Transes, updStack(IOResult,Stack), true};
      Transes ->
	{Transes, Stack, false}
    end,
  if SaveConf -> mce_conf:put_conf(SavedConf); true -> ok end,
  Result.

updStack({unchanged, _, _}, Stack) ->
  Stack;
updStack({changed, Received, NewState}, Stack) ->
  {Entry,_} = mce_behav_stackOps:pop(Stack),
  State = Entry#stackEntry.state,
  Actions = lists:map(fun ({Pid,Msg}) -> {Pid, io, Msg, []} end, Received),
  mce_behav_stackOps:push
    (Entry#stackEntry
     {state=State#monState{state=NewState},
      actions=Actions,
      depth=Entry#stackEntry.depth+1}, 
     Stack).

pick(Stack) ->
  {Transitions,NewStack, _} = get_transitions(Stack),
  pick(void,Transitions,NewStack).

pick(Pick, Transitions, Stack) ->
  PickStack = zapStackToPos(Stack),
  if Transitions=:=[] -> throw(no_transitions);
     true -> ok
  end,
  Monitor = get_monitor(PickStack),
  State = get_sys(PickStack),
  try take_transition(Pick, Transitions, PickStack, Monitor) of
    {Actions, Sys} ->
      maybe_print_actions(Actions, PickStack),
      Depth = get_depth(PickStack),
      NewEntry =
	#stackEntry
	{actions=Actions,
	 depth=Depth + 1,
	 state=#monState{state=Sys, monitor=Monitor}},
      NewStack = mce_behav_stackOps:push(NewEntry, PickStack),
      case mce_behav_monitorOps:stateChange(Sys, Monitor, NewStack) of
	{ok, NewMon} ->
	  upd_pos(Depth+1),
	  {ok,
	   mce_behav_stackOps:push
	   (NewEntry#stackEntry{state=#monState{state=Sys, monitor=NewMon}},
	    PickStack)};
	skip ->
	  upd_pos(Depth+1),
	  io:format("Monitor wants to skip this path; " ++
		    "since we are in simulation mode we continue...~n", []),
	  {ok,NewStack};
	MonError ->
	  mce_result:throw_result_exc
	    (mce_result:mk_badmon(MonError,PickStack,void,mce_conf:get_conf()))
      end
  catch
    {result_exc,Result} ->
      case mce_result:stack(Result) of
	void ->
	  mce_result:throw_result_exc
	    (mce_result:add_stack
	     (gen_error_stack(State,Monitor,PickStack),Result));
	_ ->
	  mce_result:throw_result_exc(Result)
      end
  end.

zapStackToPos(Stack) ->
  case at_top_of_stack(Stack) of
    true -> Stack;
    false ->
      {_,Rest} = mce_behav_stackOps:pop(Stack),
      zapStackToPos(Rest)
  end.

gen_error_stack(State, Monitor, Stack) ->
  Depth = get_depth(Stack),
  Actions = lists:reverse(get(actions)),
  mce_behav_stackOps:push
    (#stackEntry
     {depth=Depth + 1,
      state=#monState{state=mce_erl:void_state(State), monitor=Monitor},
      actions=Actions},
     Stack).

take_transition(Pick, Transitions, Stack, Monitor) ->
  case Pick of
    N when is_integer(N), N > 0 ->
      Transition = lists:nth(N, Transitions),
      ?LOG("picked transition ~p~n", [Transition]),
      Result = mce_conf:commit(Transition, Monitor, mce_conf:get_conf()),
      ?LOG("commit result ~p~n", [Result]),
      Result;
    void ->
      Scheduler = get_scheduler(),
      case mce_behav_schedulerOps:choose(Transitions, Scheduler, Monitor, mce_conf:get_conf())
	of
	{ok, {Result, NewScheduler}} ->
	  upd_scheduler(NewScheduler),
	  case mce_behav_schedulerOps:willCommit(Scheduler) of
	    true -> Result;
	    false -> mce_conf:commit(Result, Monitor, mce_conf:get_conf())
	  end;
	no_transitions ->
	  throw(no_transition);
	stopping ->
	  throw(no_transition)
      end
  end.

maybe_print_actions(Actions, Stack) ->
  maybe_print_actions_str(print_actions(2,Actions,Stack)).

maybe_print_actions_str(ActionsStr) ->
  case mce_conf:sim_actions() of
    true ->
      io:format("~nActions:~n~s~n",[ActionsStr]);
    false -> 
      ok
  end.

print_alternatives(Stack,N,Transitions) -> 
  {OrderedTransitions,Default} = order_transitions(Transitions,Stack,get_pos()),
  print_alternatives(Stack,N,OrderedTransitions,Default).

print_alternatives(Stack,_,[],Default) -> 
  io:format("~n");
print_alternatives(Stack,N,[Transition|Rest],Default) -> 
  io:format("~p: ",[N]),
  print_alternative(Stack,Transition),
  if N=:=1, Default -> io:format("~n   {chosen transition}~n~n"); 
     true -> io:format("~n~n")
  end,
  print_alternatives(Stack,N+1,Rest,Default).

print_alternative(Stack,{commMove,{FromObj,ToObj,{signal,_,Signal},_},_}) ->
  io:format
    ("node ~p:~n    receive signal ~p~n   from ~p",
     [ToObj, Signal, FromObj]);
print_alternative(Stack, {exec, Exec, _State}) ->
  P = Exec#executable.process,
  case P#process.status of
    runnable ->
      io:format("process ~s:~n~s",
		[symbolicProcessName(P#process.pid, Stack),
		 print_expr(4,P#process.expr)]);
    receivable ->
      {true, {_, Val, _, _}, _} = mce_erl_opsem:checkReceive(P),
      io:format("process ~s:~n~s",
		[symbolicProcessName(P#process.pid, Stack),
		 print_recv(4, Val, P#process.expr)]);
    {timer, TimerDeadline} ->
      io:format("Timer at ~p: process ~s:~n~s",
		[TimerDeadline,
		 symbolicProcessName(P#process.pid, Stack),
		 print_expr(4, getTimeOutCall(P#process.expr))]);
    exiting ->
      io:format("process ~s terminating due to ~p~n",
		[symbolicProcessName(P#process.pid, Stack),
		 mce_erl:exiting_reason(P#process.expr)]);
    sendable ->
      io:format("process ~s:~n~s",
		[symbolicProcessName(P#process.pid, Stack),
		 print_expr(4,P#process.expr)]);
    dead ->
      io:format("process ~s terminates",
		[symbolicProcessName(P#process.pid, Stack)]);
    Other ->
      io:format
	("*** weird status of executable~n ~p ~nhas wierd status ~p", 
	 [Exec, Other])
  end;
print_alternative(Stack, Other) ->
  io:format("Don't know how to print alternative~n  ~p~n", [Other]).

getTimeOutCall({?RECVTAG,{_,{_,TimeOutCall}}}) ->
  TimeOutCall;
getTimeOutCall({?CONTEXTTAG,{Innermost,_}}) ->
  getTimeOutCall(Innermost).

print_expr(Indent,{?CONTEXTTAG,{Innermost,Context}}) ->
  combine_strs
    (print_expr(Indent,Innermost),
     print_call_context(Indent+2,lists:reverse(Context)),
     newline());
print_expr(Indent,{?RECVTAG,{{Mod,RcvFun,Args},_}}) ->
  indent(Indent)++
    combine_strs
      (io_lib:format("receive",[]),
       print_cont_call(Indent+2,Mod,RcvFun,Args),
       newline());
print_expr(Indent,{?SENDTAG,{SEF,{Mod,F,Args}}}) ->
  indent(Indent)++
    io_lib:format
      ("run ~s~n~s(with side effect ~s)",
       [print_run_expr(Indent,Mod,F,Args),
        indent(Indent),print_sef(SEF)]);
print_expr(Indent,{Mod,F,Args}) ->
  indent(Indent)++
    io_lib:format
      ("run ~s",
       [print_run_expr(Indent,Mod,F,Args)]);
print_expr(Indent,{Fun,Args}) ->
  indent(Indent)++io_lib:format("run ~p(~p)",[Fun,Args]);
print_expr(Indent,Expr) ->
  indent(Indent)++io_lib:format("print_expr: strange expression ~p",[Expr]).

print_sef({M,F,Arity}) when is_atom(M),is_atom(F),is_integer(Arity) ->
  io_lib:format("~p:~p/~p",[M,F,Arity]);
print_sef(SEF) ->
  io_lib:format("~p",[SEF]).

print_run_expr(Indent,Mod,Fun,Args) ->
  Result = try Mod:mce_erl_real_name(Fun) catch _:_ -> {Fun,void} end,
  case Result of
    {Fun,_} ->
      io_lib:format
	("function ~p:~p(~p)",[Mod,Fun,Args]);
    {OtherFun,void} ->
      io_lib:format
	("function ~p:~p(~p)",[Mod,OtherFun,Args]);
    {OtherFun,LineNo} ->
      combine_strs(
	io_lib:format
	("in function ~p near line ~p of \"~p.erl\"",
	 [OtherFun,LineNo,Mod]),
	print_active_vars(Indent,Args),
	newline())
  end.

print_recv(Indent,Val,Expr) ->
  combine_strs
    (indent(Indent)++io_lib:format("receive ~p",[Val]),
     case Expr of
       {?CONTEXTTAG,{{?RECVTAG,{{Mod,RcvFun,Args},_}},Context}} ->
	 combine_strs
	   (print_cont_call(Indent+2,Mod,RcvFun,Args),
	    print_call_context(Indent+4,lists:reverse(Context)),
	    newline());
       {?RECVTAG,{{Mod,RcvFun,Args},_}} ->
	 print_cont_call(Indent+2,Mod,RcvFun,Args)
     end,
     newline()).

print_cont_call(Indent,Mod,Fun,Args) ->
  Result = try Mod:mce_erl_real_name(Fun) catch _:_ -> {Fun,void} end,
  indent(Indent)++
    case Result of
      {Fun,_} ->
	io_lib:format
	  ("using contination function ~p:~p(~p)",[Mod,Fun,Args]);
      {OtherFun,void} ->
	io_lib:format
	  ("using continuation function ~p:~p(~p)",[Mod,OtherFun,Args]);
      {OtherFun,LineNo} ->
	combine_strs(
	  io_lib:format
	  ("in function ~p near line ~p of \"~p.erl\"",
	   [OtherFun,LineNo,Mod]),
	  print_active_vars(Indent,Args),
	  newline())
    end.

print_context_cont_call(Indent,{Fun,Args}) ->
  indent(Indent)++
    io_lib:format("called by ~s(~s)~n",[Fun,Args]);
print_context_cont_call(Indent,{Mod,Fun,Args}) ->
  Result = try Mod:mce_erl_real_name(Fun) catch _:_ -> {Fun,void} end,
  indent(Indent)++
    case Result of
      {Fun,_} ->
	io_lib:format
	  ("called by ~p:~p(~p)",[Mod,Fun,Args]);
      {OtherFun,void} ->
	io_lib:format
	  ("called by ~p:~p(~p)",[Mod,OtherFun,Args]);
      {OtherFun,LineNo} ->
	combine_strs(
	  io_lib:format
	  ("called by ~p near line ~p of \"~p.erl\"",
	   [OtherFun,LineNo,Mod]),
	  print_active_vars(Indent,Args),
	  newline())
    end.

print_call_context(Indent,[]) -> "";
print_call_context(Indent,[Element|Rest]) ->
  case Element of
    {?LETTAG,{_,Cont}} ->
      combine_strs
	(print_context_cont_call(Indent,Cont),
	 print_call_context(Indent+2,Rest),
	 newline());
    _ ->
      print_call_context(Indent,Rest)
  end.

print_active_vars(Indent,[]) -> "";
print_active_vars(Indent,ActiveVars) ->
  io_lib:format
    ("~swith active data ~s",
     [indent(Indent),
      combine_strls
      (lists:map
       (fun (E) -> io_lib:format("~p",[E]) end,ActiveVars),comma_space())]).

parse_eval(Pos,Bindings) ->
  try parse_eval_1(Pos,Bindings)
  catch
    Argument ->
      io:format
	("** exception throw: ~p~n    Stacktrace:~n      ~p~n",
	 [Argument, erlang:get_stacktrace()]),
      fail;
      exit:Reason ->
      io:format
	("** exception exit: ~p~n    Stacktrace:~n      ~p~n",
	 [Reason, erlang:get_stacktrace()]),
      fail;
      error:Reason ->
      io:format
	("** exception error: ~p~n    Stacktrace:~n      ~p~n",
	 [Reason, erlang:get_stacktrace()]),
      fail
  end.

parse_eval_1(Pos,Bindings) ->
  %% If we are running inside an application the normal group_leader cannot
  %% be used
  OurGroupLeader =
    group_leader(),
  GroupLeader = 
    case whereis(normal_group_leader) of
      undefined ->
	OurGroupLeader;
      NormalGroupLeader ->
	NormalGroupLeader
    end,
  {ok,Terms,_} =
    io:parse_erl_exprs(GroupLeader,io_lib:format("stack(@~p)> ",[Pos])),
  eval_exprs(Terms,Bindings).

eval_expr(Term,Bindings) ->
  erl_eval:expr
    (Term,Bindings,
     {eval,fun eval_local_func/3},
     {value,fun eval_nonlocal_func/2}).

eval_exprs(Terms,Bindings) ->
  erl_eval:exprs
    (Terms,Bindings,
     {eval,fun eval_local_func/3},
     {value,fun eval_nonlocal_func/2}).

%% @private
eval_local_func(FunName, UnevalArgs, Bindings) ->
  {NewBindings,Arguments} = eval_arguments(UnevalArgs,Bindings),
  RetVal = eval_command(FunName,Arguments),
  {value,RetVal,NewBindings}.

%% @private
eval_nonlocal_func(Fun, EvalArgs) ->
  apply(Fun,EvalArgs).

eval_arguments(UnevalArgs,Bindings) ->
  lists:foldl
    (fun (UnevalArg,{NewBindings,Values}) ->
	 {value,Value,NewerBindings} = eval_expr(UnevalArg,NewBindings),
	 {NewerBindings,[Value|Values]}
     end,
     {Bindings,[]},
     UnevalArgs).

eval_command(FunName, Args) ->
  DebuggedState = get(debuggedState),
  try
    apply(?MODULE, FunName, [DebuggedState|Args])
  catch
    Term ->
      io:format
	("~n~s throws ~p.~nBacktrace:~n  ~p~n",
	 [app_str(FunName,Args), Term, erlang:get_stacktrace()]),
      throw(eval_error);
      exit:Reason ->
      io:format
	("~n~s exits with ~p.~nBacktrace:~n  ~p~n",
	 [app_str(FunName,Args), Reason, erlang:get_stacktrace()]),
      throw(eval_error);
      error:Reason ->
      io:format
	("~n~s fails with ~p.~nBacktrace:~n  ~p~n",
	 [app_str(FunName,Args), Reason, erlang:get_stacktrace()]),
      throw(eval_error)
  end.

app_str(ModFun,Args) ->
  io_lib:format
    ("~p(~s)",
     [ModFun,
      combine_strls
      (lists:map(fun (A) -> io_lib:format("~p",[A]) end,Args),
       comma_space())]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% User commands

%% @private
help(_) ->
  io:format("commands: Integer, choose(Alternative), quit(), help(), printTransitions(),~n"),
  io:format("          printState(), printEther(), printNode(NodeName), printProcess(Pid)~n"),
  io:format("          getState(), getNode(NodeName), getProcess(Pid)~n"),
  io:format("          goto(StackFrame), gotoStart(), gotoFinal()~n"),
  io:format("          back(), back(NumFrames), forward(), forward(NumFrames)~n"),
  io:format("          showExecution~n"),
  io:format("~n"),
  void.

%% @private
printTransitions(Stack) ->  
  {Transitions,NewStack,_} = get_transitions(Stack),
  print_alternatives(NewStack,1,Transitions),
  void.

order_transitions(Transitions,Stack,Pos) ->
  case at_top_of_stack(Stack) of
    true ->
      {Transitions,false};
    false ->
      case get_actions(get_pos(),Stack) of
	[Act|_] ->
	  Source = mce_erl_actions:get_source(Act),
	  find_action_in_transitions(Source,Act,Transitions,[]);
	_ ->
	  {Transitions,false}
      end
  end.
      
find_action_in_transitions(Source,Act,[],Seen) ->
  {lists:reverse(Seen),no};
find_action_in_transitions(Source,Act,[T|Rest],Seen) ->
  TSource = transition_source(T),
  if
    TSource=/=Source ->
      find_action_in_transitions(Source,Act,Rest,[T|Seen]);
    true ->
      case compatible_act_and_transition(Act,T) of
	true -> {[T|lists:reverse(Seen,Rest)],true};
	false -> find_action_in_transitions(Source,Act,Rest,[T|Seen])
      end
  end.

transition_source({exec,Exec,_}) ->
  (Exec#executable.process)#process.pid;
transition_source({commMove,{FromNode,ToNode,_,_},_}) ->
  ToNode;
transition_source(_) -> void.

compatible_act_and_transition(Act,T) ->
  case T of
    {commMove,_,_} -> 
      true;
    {exec,Exec,_} ->
      Status = (Exec#executable.process)#process.status,
      case Status of
	{timer,_} ->
	  mce_erl_actions:is_run(Act);
	receivable ->
	  mce_erl_actions:is_recv(Act);
	_ ->
	  true
      end
  end.

%% @private
quit(_Stack) ->
  put(dbgEvalState,quit),
  void.

%% @private
getState(Stack) ->
  get_sys(get_stack_at_position(Stack)).

%% @private
forward(Stack) ->
  forward(Stack,1),
  void.

%% @private
forward(Stack,M) ->
  Top = top_pos(Stack),
  P = get_pos(),
  if
    P+M>Top ->
      io:format("Cannot go forward ~p frames (final frame at ~p).~n~n",[M,Top]);
    true ->
      upd_pos(P+M)
  end,
  void.

%% @private
back(Stack) ->
  back(Stack,1),
  void.

%% @private
back(Stack,M) ->
  P = get_pos(),
  if
    P<M ->
      io:format("Cannot go back ~p frames (currently at ~p).~n~n",[M,P]);
    true ->
      upd_pos(P-M)
  end,
  void.

%% @private
goto(Stack,M) ->
  Top = top_pos(Stack),
  case is_legal_pos(M,Stack) of
    true -> upd_pos(M);
    false -> io:format("Cannot go to stack frame ~p (outside of stack).~n",[M])
  end,
  void.

%% @private
gotoFinal(Stack) ->
  Top=top_pos(Stack),
  upd_pos(Top),
  void.

%% @private
gotoStart(Stack) ->
  goto(Stack,0),
  void.

%% @private
run(_) ->
  put(dbgEvalState,run),
  void.

%% @private
run(Stack,N) ->
  step(Stack,N),
  void.

%% @private
step(_) ->
  put(dbgEvalState,{step,1}),
  void.

%% @private
step(_,N) ->
  put(dbgEvalState,{step,N}),
  void.

%% @private
printFrame(Stack) ->
  print_frame(get_pos(),Stack),
  void.

%% @private
where(Stack) ->
  where(Stack,3).

%% @private
where(Stack, N) ->
  printToFrame(Stack, get_pos(), N),
  void.

%% @private
choose(State,Alternative) ->
  {Transitions,NewStack,_} = get_transitions(State),
  N = length(Transitions),
  if is_integer(Alternative), Alternative>0, Alternative<N+1 ->
      put(dbgEvalState,{alternative,Alternative});
     true ->
      io:format("Alternative ~p not valid~n",[Alternative]),
      fail
  end,
  void.

%% @private
getNode(Stack, NodeName) ->
  case mce_erl_sysOS:findNode(NodeName, getState(Stack)) of
    {ok, {Node, _}} -> {ok, Node};
    _ -> fail
  end.

%% @private
printNode(Stack, NodeName) ->
  case getNode(Stack, NodeName) of
    {ok, Node} ->
      io:format("~s~n", [mce_erl_pretty:pretty(Node)]);
    _ ->
      io:format("Node ~p does not exist~n", [NodeName])
  end,
  void.

%% @private
printEther(Stack) ->
  io:format
    ("~s~n",
     [mce_erl_pretty:pretty((getState(Stack))#state.ether)]),
  void.

%% @private
printNodeNames(Stack) ->
  print_node_names((getState(Stack))#state.nodes),
  void.

%% @private
saveState(Stack,FileName) ->
  SavedDebuggerState =
    {debugger_state,
     {Stack,
      get_mode(),
      get_scheduler(),
      mce_conf:get_conf(),
      mce_random:rnd_state(),
      get_pos()}},
  ok = file:write_file(FileName,term_to_binary(SavedDebuggerState)),
  void.

%% @private
getProcess(Stack,Pid) ->
  if
    not(is_record(Stack,stack)) -> 
      fail;
    true ->
      PidSearch =
	case mcerlang:is_pid(Pid) of
	  true -> {ok, Pid};
	  false -> 
	    case Pid of
	      {Name,NodeName} ->
		case getNode(Stack,NodeName) of
		  {ok, Node} -> searchForRegistered(Name,Node);
		  _ -> fail
		end;
	      _ ->
		fail
	    end
	end,
      case PidSearch of
	{ok, PidS} ->
	  case getNode(Stack,mcerlang:node(PidS)) of
	    {ok, Node2} -> searchForPid(PidS,Node2);
	    _ -> fail
	  end;
	_ ->
	  fail
      end
  end.

%% @private
printProcess(Stack, Pid) ->
  case getProcess(Stack, Pid) of
    {ok, Process} ->
      io:format("~s~n", [mce_erl_pretty:pretty(Process)]);
    _ ->
      io:format("Process ~p does not exist~n", [Pid])
  end,
  void.


%% @private
showExecution(Stack) ->
  io:format("Stack:~n"),
  show_stack1(Stack,0,top_pos(Stack)),
  io:format("~n").

show_stack1(Stack,I,N) when I>N -> ok;
show_stack1(Stack,I,N) ->
  print_frame(I,Stack),
  io:format("~n"),
  show_stack1(Stack,I+1,N).

%% @private
printProcessNames(Stack) ->
  lists:foreach
    (fun (Node) ->
	 lists:foreach
	   (fun (Process) ->
		io:format
		  ("~s ",
		   [symbolicProcessName(Process#process.pid,Stack)])
	    end,
	    Node#node.processes)
     end,
     (getState(Stack))#state.nodes),
  io:format("~n"),
  void.

%% @private
printState(Stack) ->
  io:format("~s~n", [mce_erl_pretty:pretty(getState(Stack))]),
  void.

%% @private
print_node_names(Nodes) ->
  lists:foreach(fun (Node) -> io:format("~p ",[Node#node.name]) end, Nodes),
  io:format("~n").

symbolicPid({_,Node,Pid}) ->
  "<"++io_lib:write(Node)++","++io_lib:write(Pid)++">";
symbolicPid(Other) ->
  io_lib:write(Other).

symbolicProcessName(Pid,Stack) ->
  case getProcess(Stack,Pid) of
    {ok, Process} ->
      RealPid = Process#process.pid,
      NodeName = mcerlang:node(RealPid),
      {ok, Node} = getNode(Stack, NodeName),
      case searchForRegisteredPid(RealPid, Node) of
	{ok, Name} ->
	  symbolicPid(RealPid)++" \"registered as "++io_lib:write(Name)++"\"";
	_ ->
	  symbolicPid(Pid)
      end;
    %%++"::module "++io_lib:write(SpawnedModuleName)
    _ ->
      ?LOG("Process ~p not found in state~n~s~n",
	   [Pid, mce_erl_pretty:pretty(get_sys(Stack))]),
      symbolicPid(Pid)
  end.

searchForRegistered(Name, Node) ->
  case mce_utils:find
    (fun ({HdName, _}) -> Name =:= HdName end, Node#node.registered)
    of
    {ok, {_, Pid}} -> {ok, Pid};
    _ -> fail
  end.

searchForRegisteredPid(Pid, Node) ->
  case mce_utils:find
    (fun ({_, HdPid}) -> Pid =:= HdPid end, Node#node.registered)
    of
    {ok, {Name, _}} -> {ok, Name};
    _ -> fail
  end.

searchForPid(Pid, Node) ->
  mce_utils:find(fun (P) -> P#process.pid =:= Pid end, Node#node.processes).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      

printToFrame(_, _, 0) -> ok;
printToFrame(Stack, Pos, N) ->
  case is_legal_pos(Pos,Stack) of
    true ->
      print_frame(Pos,Stack),
      io:format("~n~n"), 
      printToFrame(Stack,Pos-1,N-1);
    false ->
      ok
  end.

print_frame(Pos,Stack) ->
  Actions = get_actions(Pos,Stack),
  io:format("~p: ", [Pos]),
  OldPos = get_pos(),
  upd_pos(Pos),
  ?LOG("print_frame(~p)~n",[Pos]),
  io:format("~s~n",[int_print_actions(Actions,Stack,Pos)]),
  upd_pos(OldPos).

print_actions(Actions,Stack) ->
  init_debugger_from(Stack),
  int_print_actions(Actions,Stack,get_pos()).

int_print_actions(Actions,Stack,Pos) ->
  StackAtFrame = get_stack_at_position(Stack,Pos),
  ?LOG
    ("int_print_actions(~p) Stack depth=~p~n",
     [Pos,get_depth(StackAtFrame)]),
  case Actions of
    [Act|_] ->
      io_lib:format
	("~s:~n~s~n",
	 [getActionsActor(Actions,StackAtFrame),
	  print_actions(4,Actions,StackAtFrame)]);
    _ ->
      io_lib:format("~n",[])
  end.

print_actions(Indent,Actions,Stack) ->
  StackAtFrame = get_stack_at_position(Stack,get_pos()),
  combine_strls
    (lists:map
     (fun (Action) -> printAction(Indent,Action,StackAtFrame) end,
      Actions), newline()).

getActionsActor(Actions,Stack) ->
  case Actions of
    [] -> "";
    [Act|_] ->
      Source = mce_erl_actions:get_source(Act),
      case mce_erl_actions:get_name(Act) of
	input -> "node "++io_lib:write(Source);
	_ -> "process "++symbolicProcessName(Source,Stack)
      end
  end.

print_action(Action) ->
  io_lib:format
    ("~s: ~s~n",
     [getActionsActor([Action],void),
      printAction(0,Action,void)]).

source_str(Indent,Action,Stack) ->
  case mce_erl_find_action_source:find_transition_cause([Action],Stack) of
    {ObjectType,Source,SourceStack} ->
	io_lib:format
	  ("~n~ssent by ~p ~p in stack frame ~p",
	   [indent(Indent+2),ObjectType,Source,get_depth(SourceStack)-1]);
    _ ->
      ""
  end.

printAction(Indent,Action,Stack) ->
  Source = mce_erl_actions:get_source(Action),
    case mce_erl_actions:get_name(Action) of
      recv ->
	SourceStr = source_str(6,Action,Stack),
	Msg = mce_erl_actions:get_recv_msg(Action),
	case digOutExpr(Source,Stack) of
	  {ok,Expr} -> print_recv(4,Msg,Expr)++SourceStr;
	  _ -> indent(Indent)++"receive "++io_lib:write(Msg)++SourceStr
	end;
      run ->
	print_expr(Indent,mce_erl_actions:get_run_expr(Action));
      input ->
	SourceStr = source_str(6,Action,Stack),
	indent(Indent)++
	  "received signal "++
	  io_lib:write(mce_erl_actions:get_input_signal(Action))++
	  " from node "++io_lib:write(mce_erl_actions:get_input_from(Action))++
	  SourceStr;
      magic_run ->
	indent(Indent)++
	  "--- counterexample loop starts ---";
      stutter ->
	indent(Indent)++
	"--- system blocked; only stuttering transition runnable ---";
      continuing ->
	indent(Indent)++
	"--- continuing ---";
      send -> 
	Pid = mce_erl_actions:get_send_pid(Action),
	SendToPid = 
	  if
	    is_atom(Pid) ->
	      {Pid,mcerlang:node(Source)};
	    true ->
	      Pid
	  end,
	Name =
	  symbolicProcessName(SendToPid,Stack),
	  indent(Indent)++
	  Name++"!"++
	  io_lib:print
	    (mce_erl_actions:get_send_msg(Action),
	     Indent+length(Name)+1,80,-1);
      io ->
	indent(Indent)++
	"external io "++io_lib:write(mce_erl_actions:get_io_msg(Action));
      output ->
	indent(Indent)++
	  "sent signal "++io_lib:write(mce_erl_actions:get_output_signal(Action))++
	  " to node "++io_lib:write(mce_erl_actions:get_output_to(Action));
      died ->
	indent(Indent)++
	"process "++symbolicPid(Source)++" died due to reason "++
	  io_lib:write(mce_erl_actions:get_died_reason(Action));
      spawn ->
	indent(Indent)++
	io_lib:write(spawn)++"("++
	  print_arglist(mce_erl_actions:get_spawn_arguments(Action))++") --> "++
	  io_lib:write(mce_erl_actions:get_spawn_result(Action));
      crashing ->
	indent(Indent)++
	  "process "++symbolicPid(Source)++" crashed with exception "++
	  io_lib:write(mce_erl_actions:get_crashing_exception(Action))++
	  " due to reason "++
	  io_lib:write(mce_erl_actions:get_crashing_reason(Action))++
	  " in stacktrace "++
	  printStackTrace(Indent+2,mce_erl_actions:get_crashing_trace(Action));
      terminated ->
	indent(Indent)++
	"process "++symbolicPid(Source)++" was terminated";
      probe ->
	Term = mce_erl_actions:get_probe_term(Action),
	  indent(Indent)++
	"*** PROBE "++
	  io_lib:write(mce_erl_actions:get_probe_label(Action))++
	  (case Term of
	     void -> "";
	     _ -> ":"++io_lib:write(Term)
	   end)++
	  " ***";
      api_call ->
	indent(Indent)++
	io_lib:write(mce_erl_actions:get_api_call_fun(Action))++
	  "("++print_arglist(mce_erl_actions:get_api_call_arguments(Action))++
	  ") --> "++io_lib:write(mce_erl_actions:get_api_call_result(Action));
      deliver ->
	"";
      Other ->
	indent(Indent)++
	"??? Strange action type "++io_lib:write(Other)++" for action "++
	  io_lib:write(Action)
    end.

print_arglist(ArgList) ->
  combine_strls
    (lists:map(fun (Arg) -> io_lib:format("~p",[Arg]) end,ArgList),
     comma()).

indent(N) when N=<0 -> "";
indent(N) -> " "++indent(N-1).

combine_strs(S1,S2,C) ->
  if
    S1=:="" -> S2;
    S2=:="" -> S1;
    true -> S1++C++S2
  end.

combine_strls(SL,C) ->
  case SL of
    [] -> "";
    [Element] -> Element;
    [Element|Rest] -> Element++C++combine_strls(Rest,C)
  end.

newline() ->
  "\n".

comma_newline() ->
  ",\n".

comma_space() ->
  ", ".

comma() ->
  ",".

digOutExpr(Pid,Stack) ->
  case getProcess(Stack,Pid) of
    {ok,Process} ->
      {ok,Process#process.expr};
    _ ->
      no
  end.

printStackTrace(Indent,[]) ->
  "";
printStackTrace(Indent,[{Module,Fun,ArityOrArguments}|Rest]) -> 
  combine_strs
    (indent(Indent)++printStackEntry(Module,Fun,ArityOrArguments,[]),
     printStackTrace(Indent,Rest),
     newline());
printStackTrace(Indent,[{Module,Fun,ArityOrArguments,Location}|Rest]) -> 
  combine_strs
    (indent(Indent)++printStackEntry(Module,Fun,ArityOrArguments,Location),
     printStackTrace(Indent,Rest),
     newline()).

printStackEntry(Module,Fun,ArityOrArguments,Location) ->
  LocationStr =
    case Location of
      [{file,FileName},{line,LineNo}] ->
	io_lib:format("file ~s, line ~p~n",[FileName,LineNo]);
      Other ->
	""
    end,
  if
    is_list(ArityOrArguments) ->
      io_lib:format
	("~s/~p(~s) ~s",
	 [get_modfun(Module,Fun),length(ArityOrArguments),
	  print_arglist(ArityOrArguments),
	  LocationStr]);
    is_integer(ArityOrArguments) ->
      io_lib:format
	("~s/~p ~s",
	 [get_modfun(Module,Fun),ArityOrArguments,LocationStr])
  end.

get_modfun(Mod,Fun) ->
  {PrePart,FunPart,AnonymousPart} =
    if 
      is_atom(Fun) ->
	FunStr = atom_to_list(Fun),
	case string:str(FunStr,"-anonymous-") of
	  0 -> {"",Fun,""};
	  N when N>5 ->
	    FunPartStr = string:substr(FunStr,2,N-4),
	    APart = string:substr(FunStr,N-2),
	    {"-",list_to_atom(FunPartStr),APart}
	end;
      true -> {"",Fun,""}
    end,
  PrePart++
  case try Mod:mce_erl_real_name(FunPart) catch _:_ -> {FunPart,void} end of
    {FunPart,_} ->
      io_lib:format("~p:~p",[Mod,FunPart]);
    {OtherFun,void} ->
      io_lib:format("~p:~p",[Mod,OtherFun]);
    {OtherFun,LineNo} ->
      io_lib:format("~p:~p near line ~p",[Mod,OtherFun,LineNo])
  end++AnonymousPart.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_sys(Stack) when is_record(Stack,stack) ->
  {Entry, _} = mce_behav_stackOps:pop(Stack),
  SysMon = Entry#stackEntry.state,
  SysMon#monState.state.

has_actions(Stack) when is_record(Stack,stack) ->
  case get_actions(Stack) of
    [_|_] -> true;
    _ -> false
  end.

get_actions(Pos,Stack) ->
  try get_stack_at_position(Stack,Pos+1) of St ->
      (element(1,mce_behav_stackOps:pop(St)))#stackEntry.actions
  catch _:_ -> [] end.

get_actions(Stack) ->
  {Entry, _} = mce_behav_stackOps:pop(Stack),
  Entry#stackEntry.actions.

get_monitor(Stack) ->
  {Entry, _} = mce_behav_stackOps:pop(Stack),
  SysMon = Entry#stackEntry.state,
  SysMon#monState.monitor.

get_depth(Stack) ->
  case mce_behav_stackOps:is_empty(Stack) of
    true -> -1;
    false ->
      {Entry, _} = mce_behav_stackOps:pop(Stack),
      Entry#stackEntry.depth
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(debuggerState,{mode=step,scheduler=void,pos=void}).

upd_mode(SteppingMode) ->
  putDebuggerState((getDebuggerState())#debuggerState{mode=SteppingMode}).

upd_scheduler(Scheduler) ->
  putDebuggerState((getDebuggerState())#debuggerState{scheduler=Scheduler}).

upd_pos(Pos) when is_integer(Pos), Pos>=0 ->
  putDebuggerState((getDebuggerState())#debuggerState{pos=Pos}).

initDebuggerState(Scheduler) ->
  putDebuggerState(#debuggerState{mode=stop,scheduler=Scheduler}).

get_mode() ->
  (getDebuggerState())#debuggerState.mode.

get_scheduler() ->
  (getDebuggerState())#debuggerState.scheduler.

get_pos() ->
  (getDebuggerState())#debuggerState.pos.

putDebuggerState(State) ->
  put(debuggerState,State).

getDebuggerState() ->  
  case get(debuggerState) of
    State when is_record(State,debuggerState) -> State;
    _ -> io:format("Debugger state not initialized??",[]), exit(debugger)
  end.

print_error(Error) ->
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
