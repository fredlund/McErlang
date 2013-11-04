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

%% Runtime system (handling process coordination) among processes

-module(mce_erl_opsem).
-export([initialState/2,initialState/3,
	 transitions/2,commit/2,commit/3,checkReceive/1,
	 doStep/2,updProcStatusFromQueue/1,record_action/1]).
-export([get_transitions_fun/0,get_commit_fun/0]).

-include("process.hrl").
-include("state.hrl").
-include("executable.hrl").
-include("node.hrl").
-include("mce_opts.hrl").

%%-define(debug,true).
-include("macros.hrl").
-include("emacros.hrl").




%%% Returns the initial state

initialState(Program,Conf) ->
  initialState(list_to_atom("node0@" ++ net_adm:localhost()), Program, Conf).

initialState(NodeName, Expr, Conf) ->
  ?LOG("Booting system using node ~p and application ~p~n", [NodeName, Expr]),
  InitialProcesses =
    case mce_conf:wants_rpc(Conf) of
      true ->
	[mce_erl_process:makeRunnable(Expr,NodeName,Conf),
	 mce_erl_process:makeRunnable({mce_erl_rpc,start,[]},NodeName,Conf)];
      _ ->
	[mce_erl_process:makeRunnable(Expr,NodeName,Conf)]
    end,
  #state{nodes =[#node{name=NodeName,processes=InitialProcesses}]}.

%%% Computes all the transitions of a state

transitions(State, Conf) ->
  %% In simulation mode we may have gotten external i/o waiting
  %% to be read here, and communicated to simulated processes.
  case mce_conf:external_io_possible(Conf) of
    true ->
      checkOverflow(),
      IOResult = {_,_,CommState} = moveIO(unchanged, [], State),
      {io,IOResult,allPossibilities(CommState,Conf)};
    _ ->
      allPossibilities(State, Conf)
  end.

%%% Commits to a transition

commit(Alternative, Monitor, Conf) ->
  put(mc_monitor, Monitor),
  (?MODULE):doStep(Alternative, Conf).

commit(Alternative, Conf) ->
  commit(Alternative, void, Conf).

moveIO(Flag, Received, State) ->
  receive
    {rcv, {Pid, Msg}} ->
      NewState = mce_erl_sysOS:addMsgToState(Pid, Msg, State),
      moveIO(changed, 
	     [{Pid,Msg}|Received], 
	     NewState);
    Msg when is_tuple(Msg), element(1, Msg) =:= gs ->
      case handleGS(State, Msg) of
	{changed, Recv, NewState} ->
	  moveIO(changed, [Recv|Received], NewState);
	_ ->
	  moveIO(Flag, Received, State)
      end;
    Msg ->
      io:format("mcerl: dont understand message ~p received~n", [Msg]),
      moveIO(Flag, Received, State)
  after 0 -> {Flag,lists:reverse(Received),State}
  end.

checkOverflow() ->
  case process_info(self(),message_queue_len) of
    {message_queue_len,N} ->
      if
	N>20, N rem 20 =:= 0 ->
	  io:format("Warning: external queue is of size ~p~n",[N]),
	  ok;
	true ->
	  ok
      end;
    Other ->
      io:format("Warning: cannot read external queue length~n"),
      ok
  end.

handleGS(State, Msg) ->
  Dict = State#state.dict,
  case mce_utils:find
    (fun (T) -> case T of {gs, _} -> true; _ -> false end end,Dict)	of
    {ok, {gs, {Pid, _}}} ->
      io:format
	("Have handler ~p for gs message; will handle~n  ~p~n",
	 [Pid,Msg]),
      {changed, {Pid,Msg}, mce_erl_sysOS:addMsgToState(Pid, Msg, State)};
    _ ->
      io:format("Did not find handler for gs message~n  ~p~n", [Msg]),
      false
  end.

%%% These are meant for simulation. Should not cause side effects
%%% (i.e. IO) except in the transition actually chosen, EXCEPT
%%% when the debugger is enabled.

allPossibilities(State, Conf) ->
  Nodes =
    try
      State#state.nodes
    catch
      Error:Pattern ->
	io:format("Have error ~p:~p at state~n  ~p~n", [Error, Pattern, State]),
	throw(bad)
    end,
  AllCommMovesPossibilities =
    allCommMovesPossibles(orddict:to_list(State#state.ether), [], State),
  AllRunPossibilities =
    allNodeMoves(fun enumerateAllPossibles/3, Nodes, [], State),
  AllTerminatePossibilities =
    case mce_conf:terminate(Conf) of
      true -> allNodeMoves(fun allTerminatePossibles/3, Nodes, [], State);
      _ -> []
    end,
  AllPossibilities =
    AllCommMovesPossibilities ++
    AllTerminatePossibilities ++
    AllRunPossibilities,
  TimeRestrictedPossibilities =
    timeRestrict(AllPossibilities, Conf),
  ?LOG("all transitions=~n~p~n",[TimeRestrictedPossibilities]),
  case mce_conf:random(Conf) of
    true ->
      randomise(TimeRestrictedPossibilities);
    _ ->
      TimeRestrictedPossibilities
  end.

timeRestrict(Possibilities, Conf) ->
  Now =
    case mce_conf:is_simulation(Conf) of
      true ->
	erlang:now();
      _ ->
	infinity
    end,
  {RestrictedPossibilities, FailedPossibleTimers} =
    lists:foldl
      (fun (Entry, {Poss, FailedPoss}) ->
	   case Entry of
	     {exec, Exec, SavedState} ->
	       Process = Exec#executable.process,
	       case Process#process.status of
		 {timer, TimerDeadline} ->
		   if Now =:= infinity ->
		       {[Entry| Poss], FailedPoss};
		      TimerDeadline =:= infinity ->
		       {Poss, FailedPoss};
		      true ->
		       case compareTimes_ge(Now, TimerDeadline)
		       of
			 true ->
			   {[Entry| Poss], FailedPoss};
			 false ->
			   {Poss, [Entry| FailedPoss]}
		       end
		   end;
		 _ -> {[Entry| Poss], FailedPoss}
	       end;
	     Other ->
	       {[Entry| Poss], FailedPoss}
	   end
       end, {[], []}, Possibilities),
  if RestrictedPossibilities =:= [],
     FailedPossibleTimers =/= [] ->
      %% No process is ready to run, but there are timers enabled
      %% that will eventually fire, lets wait until the first one
      %% fires
      {Deadline, Entry} = getFirstProcessToFire(FailedPossibleTimers),
      {FirstTimerProcess, Others, State} = Entry,
      WaitTime = timer:now_diff(Deadline, Now) div 1000,      
      ?LOG
	 ("Will wait ~p milliseconds~n;first=~p~n",
	  [WaitTime,Entry]),
      case mce_conf:is_infinitely_fast(Conf) of
	false ->
	  timer:sleep(WaitTime);
	true ->
	  ok
      end,
      [Entry];
     true ->
      possibly_strip_timer_transitions(RestrictedPossibilities,Conf)
  end.

possibly_strip_timer_transitions(Transitions,Conf) ->
  case mce_conf:is_infinitely_fast(Conf) of
    false ->
      Transitions;
    true ->
      {NonTimerTransitions,TimerTransitions} =
	lists:foldl
	  (fun (Entry, {NT,T}) ->
	       case Entry of
		 {exec, Exec, SavedState} ->
		   Process = Exec#executable.process,
		   case Process#process.status of
		     {timer, TimerDeadline} ->
		       if 
			 TimerDeadline=:=0 ->
			   {[Entry|NT],T};
			 true ->
			   {NT,[Entry|T]}
		       end;
		     _ ->
		       {[Entry|NT],T}
		   end;
		 _ ->
		   {[Entry|NT],T}
	       end
	   end, {[],[]}, Transitions),
      if
	NonTimerTransitions=/=[] ->
	  NonTimerTransitions;
	true ->
	  TimerTransitions
      end
  end.

getFirstProcessToFire(Ps) ->
  getFirstProcessToFire(Ps,none,none).

getFirstProcessToFire([],Entry,T) when T=/=none ->
  {T,Entry};
getFirstProcessToFire([Entry|Rest],SavedEntry,SavedTime) ->
  {exec,Exec,SavedState} = Entry,
  Process = Exec#executable.process,
  case Process#process.status of
    {timer,TimerDeadline} ->
      if
	SavedTime=/=none ->
	  case compareTimes_ge(TimerDeadline,SavedTime) of
	    true ->
	      getFirstProcessToFire(Rest,SavedEntry,SavedTime);
	    false ->
	      getFirstProcessToFire(Rest,Entry,TimerDeadline)
	  end;
	true -> getFirstProcessToFire(Rest,Entry,TimerDeadline)
      end
  end.

allNodeMoves(F,[],_,State) -> [];
allNodeMoves(F,[Node|RestNodes],Seen,State) ->
  NodeMoves =
    lists:map
      (fun ({Process,OtherProcesses,State}) ->
	   {exec,
	    #executable{node=Node#node{processes=OtherProcesses},
			otherNodes=Seen++RestNodes,
			process=Process},
	    State}
       end,
       F(Node#node.processes,[],State)),
  NodeMoves++allNodeMoves(F,RestNodes,[Node|Seen],State).

allCommMovesPossibles([],Seen,_) -> [];
allCommMovesPossibles([NodeSpec|Rest],Seen,S) ->
  case NodeSpec of
    {Key={FromObj,ToObj},[Sig|RestSigs]} ->
      RemainingEther =
	if RestSigs=:=[] -> Seen++Rest;
	   true -> Seen++[{Key,RestSigs}|Rest]
	end,
      [{commMove,{FromObj,ToObj,Sig,RemainingEther},S}|
       allCommMovesPossibles(Rest,Seen++[NodeSpec],S)];
    _ ->
      throw(badEther)
  end;
allCommMovesPossibles(Ether,Seen,State) ->
  io:format("Ether is ~p~n",[Ether]),
  throw(badEther).

%%allTerminatePossibles([],_,State) -> [];
%%allTerminatePossibles([P|Rest],Seen,State) ->
%%  [{P#process{status=dead},Seen++Rest,State}|
%%   allTerminatePossibles(Rest,[P|Seen],State)].

allTerminatePossibles([],_,State) -> [];
allTerminatePossibles([P|Rest],Seen,State) ->
  case (P#process.flags)#processFlags.do_terminate of
    true ->
      [{P#process{status=dead},Seen++Rest,State}|
       allTerminatePossibles(Rest,[P|Seen],State)];
    false ->
      allTerminatePossibles(Rest,[P|Seen],State)
  end.

enumerateAllPossibles([], _, State) -> [];
enumerateAllPossibles([P| Rest], Seen, State) ->
  case P#process.status of
    blocked ->
      enumerateAllPossibles(Rest, [P| Seen], State);
    choice ->
      Others = Seen ++ Rest,
      Values =
	lists:map(fun (LetExp) ->
		      NewP = P#process{status=runnable, expr=LetExp},
		      {NewP, Others, State}
		  end,
		  digOutChoice(P#process.expr)) ++
	enumerateAllPossibles(Rest, [P| Seen], State);
    _ ->
      [{P, Seen ++ Rest, State}|
       enumerateAllPossibles(Rest, [P| Seen], State)]
  end.

digOutChoice(E={?CHOICETAG, _}) ->
  mce_erl:choice_alternatives(E);
digOutChoice({?CONTEXTTAG, {E={?CHOICETAG, _}, Context}}) ->
  lists:map
    (fun (Alternative) -> mce_erl:mk_context(Alternative, Context) end,
     mce_erl:choice_alternatives(E)).

compareTimes_ge({M1, S1, Mic1}, {M2, S2, Mic2}) ->
  M1 > M2
    orelse M1 =:= M2 andalso S1 > S2
    orelse M1 =:= M2 andalso S1 =:= S2 andalso Mic1 >= Mic2.

timeStampPlus(MilliSeconds,T2) ->
  TimeStamp = milliSecondsToTimeStamp(MilliSeconds),
  addTimeStamps(TimeStamp,T2).

milliSecondsToTimeStamp(MilliSeconds) ->
  Seconds = MilliSeconds div 1000,
  MegaSeconds = Seconds div 1000000,
  {MegaSeconds, Seconds rem 1000000, MilliSeconds rem 1000 * 1000}.

addTimeStamps({M1,S1,Mic1},{M2,S2,Mic2}) ->
  Mic=Mic1+Mic2,
  MicRem = Mic rem 1000000,
  MicDiv = Mic div 1000000,
  S = S1+S2+MicDiv,
  SRem = S rem 1000000,
  SDiv = S div 1000000,
  M = M1+M2+SDiv,
  {M,SRem,MicRem}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

doStep({commMove, CommSpec, State}, Conf) ->
  initActions(),
  Result = mce_erl_node:doDispatchSignal(CommSpec, State),
  {getActions(), Result};
doStep({exec, Exec, SavedState}, Conf) ->
  initActions(),
  try
    Result = doStep1(Exec, SavedState, Conf),
    ?LOG("doStep: result=~p~n", [Result]),
    {getActions(), Result}
  catch
    {user, Error, Reason} ->
      Process = mce_erl_state:getProcess(mce_erl_state:getState()),
      ?LOG("Process~n  ~p~nmaybe stopped because of exception ~p:~p~n",
	   [Process, Error, Reason]),
      Pid = (Exec#executable.process)#process.pid,
      handle_exception(Process, Pid, Error, Reason, Conf)
  end.

maybe_notice_exit(Exception, Reason, Conf) ->
  NormalExit =
    (Exception=:=exit) and (Reason=:=normal),
  FailOnExit =
    mce_conf:fail_on_exit(Conf),
  NoticeExit =
    mce_conf:notice_exits(Conf),
  WillExit =
    FailOnExit and (not(NormalExit)),
  PrintExit =
    NoticeExit and (not(NormalExit or WillExit)),
  Trace =
    if
      PrintExit ->
	erlang:get_stacktrace();
      WillExit ->
	Tr=erlang:get_stacktrace(),
	put(stack_trace,Tr),
	Tr;
      true ->
	[]
    end,
  TraceStr =
    if
      PrintExit ->
	"Stack trace:\n"++(mce_erl_debugger:printStackTrace(2,Trace))++"\n";
      true ->
	""
    end,
  if
    not(PrintExit) -> ok;
    Exception=:=exit ->
      io:format
	("Process~n  ~p~n exited because of: ~p~n~s~n",
	 [mce_erl_state:getProcess(mce_erl_state:getState()),Reason,TraceStr]);
    Exception=:=error ->
      io:format
	("~nProcess~n  ~p~nexited because of error: ~p~n~s~n",
	 [mce_erl_state:getProcess(mce_erl_state:getState()),Reason,TraceStr]);
    Exception=:=throw ->
      io:format
	("~nProcess~n  ~p~nthrew an uncaught exception: ~p~n~s~n",
	 [mce_erl_state:getProcess(mce_erl_state:getState()),Reason,TraceStr])
  end.

handle_exception(Process,Pid,Exception,Reason,Conf) ->
  case mce_conf:fail_on_exit(Conf) of
    true ->
      case Exception of
	exit ->
	  if Reason =/= normal ->
	      mce_erl_actions:record(mce_erl_actions:mk_died(Pid,Reason)),
	      signal_user_error(Exception,Reason);
	     true ->
	      {getActions(), exceptionReturn(Process,Reason,Conf)}
	  end;
	_ ->
	  mce_erl_actions:record(mce_erl_actions:mk_died(Pid,Reason)),
	  signal_user_error(Exception,Reason)
      end;
    false ->
      if
	Exception =:= throw ->
	  {getActions(), exceptionReturn(Process,{nocatch, Reason},Conf)};
	true ->
	  {getActions(), exceptionReturn(Process,Reason,Conf)}
      end
  end.

signal_user_error(Exception,Reason) ->
  StackTrace = 
    case get(stack_trace) of
      Tr when is_list(Tr) -> Tr;
      _ -> []
    end,
  mce_result:throw_result_exc
    (mce_result:mk_user_error
     (mce_result:mk_exception_error
      (Exception,Reason,StackTrace))).

exceptionReturn(Process,Reason,Conf) ->
  State = mce_erl_state:getState(),
  case mce_erl_state:getNode(State) of
    none ->
      mce_erl_sysOS:mkStateFromOtherNodes(State);
    _ ->
      case mce_conf:sends_are_sefs() of
	false ->
	  mce_erl_sysOS:mkStateFromCurrentExecutable
	    (mce_erl_sys:inform(Reason, State));
	true ->
	  mce_erl_sysOS:mkStateFromCurrentExecutableWithProcess
	    (putProcess(Process,mce_erl:exiting(Reason),Conf),
	     State)
      end
  end.

doStep1(Exec, SavedState, Conf) ->
  P = Exec#executable.process,
  ?LOG("Going to step ~p with status ~p for a while...~n",
       [P, P#process.status]),
  ?LOG("Executable is ~p~n", [Exec]),
  case P#process.status of
    runnable ->
      mce_erl_state:setState
	(mce_erl_sysOS:setCurrentRunContext(Exec, SavedState)),
      mce_erl_actions:record
	(mce_erl_actions:mk_run
	 (P#process.pid,P#process.expr)),
      runUserCode(P#process.expr, Conf);
    sendable ->
      mce_erl_state:setState
	(mce_erl_sysOS:setCurrentRunContext(Exec, SavedState)),
      doSend(P#process.pid, P#process.expr, SavedState, Conf);
    exiting ->
      mce_erl_state:setState
	(mce_erl_sysOS:setCurrentRunContext(Exec, SavedState)),
      doExit(P#process.pid, P#process.expr, SavedState, Conf);
    {timer, _} ->
      doRunTimer(P, Exec, SavedState, Conf);
    receivable ->
      doReceive(Exec, SavedState, Conf);
    dead ->
      mce_erl_state:setState
	(mce_erl_sysOS:setCurrentRunContext(Exec, SavedState)),
      mce_erl_actions:record
	(mce_erl_actions:mk_terminated
	 (P#process.pid,[])),
      handleTerminated()
  end.

doRunTimer(P, Exec, SavedState, Conf) ->
  %% Timer has expired we can just run the code
  {TimeOutCall, Context} = getTimeOutCall(P#process.expr),
  mce_erl_state:setState(mce_erl_sysOS:setCurrentRunContext(Exec, SavedState)),
  mce_erl_actions:record
    (mce_erl_actions:mk_run
     (P#process.pid,TimeOutCall)),
  runUserCode(mce_erl:mk_context(TimeOutCall, Context), Conf).

getTimeOutCall({?RECVTAG,{_,{_Timer,TimeOutCall}}}) ->
  {TimeOutCall,[]};
getTimeOutCall({?CONTEXTTAG,{Innermost,Context}}) ->
  {?RECVTAG,{_,{_Timer,TimeOutCall}}} = Innermost,
  {TimeOutCall,Context}.

doExit(Pid, Expr, SavedState, Conf) ->
  Reason = mce_erl:exiting_reason(Expr),
  State = mce_erl_state:getState(),
  mce_erl_actions:record(mce_erl_actions:mk_died(Pid,Reason)),
  mce_erl_sysOS:mkStateFromCurrentExecutable
    (mce_erl_sys:inform(Reason, State)).

doSend(Pid, Expr, SavedState, Conf) ->
  ?LOG("doSend(~p)~n",[Expr]),
  {Innermost, Context} =
    case Expr of
      {?CONTEXTTAG,{E,Cntxt}} ->
	{mce_erl:send_sef_fun(E), Cntxt};
      E ->
	{mce_erl:send_sef_fun(E), []}
    end,
  mce_erl_actions:record(mce_erl_actions:mk_run(Pid,Innermost)),
  runUserCode1(Innermost,Context,Conf).

runUserCode(Expr, Conf) ->
  {Innermost, Context} =
    case Expr of
      {?CONTEXTTAG, Cnt} -> Cnt;
      _ -> {Expr, []}
    end,
  runUserCode1(Innermost,Context,Conf).

runUserCode1(Innermost,Context,Conf) ->
  ?LOG("will execute stack ~p~nwith context ~p~n",
       [Innermost, Context]),
  try
    mce_erl_stacks:execStack(Innermost, Context)
    of
    Value ->
      State = mce_erl_state:getState(),
      ?LOG(("Call ~p~nwith context ~p got back value~n~p," ++
	    "actions are~n~p~nstate ~p~n"),
	   [Innermost, Context, Value, get(actions), State]),
      P = mce_erl_state:getProcess(State),
      case mce_erl_stacks:isTagged(Value) of
	false ->
	  case mce_conf:sends_are_sefs() of
	    false ->
	      ?LOG("Process ~p terminates with value~n~p~n",
		   [P#process.pid, Value]),
	      mce_erl_actions:record
		(mce_erl_actions:mk_terminated
		 (P#process.pid,Value)),
	      NewS = mce_erl_sys:inform(normal, State),
	      %% Note that we have to recompute links and pid map
	      %% here because it can potentially change in
	      %% mce__erl_sys:inform.
	      mce_erl_sysOS:mkStateFromCurrentExecutable(NewS);
	    true ->
	      mce_erl_sysOS:mkStateFromCurrentExecutableWithProcess
		(putProcess(P,mce_erl:exiting(normal),Conf),
		 State)
	  end;
	true ->
	  {NewValue, NewContext} = mce_erl_stacks:parseStack(Value),
	  NewExec = mce_erl:mk_context(NewValue, NewContext),
	  ?LOG("NewValue: ~p ~nNewContext:~n ~p~nNewExec=~p~n",
	       [NewValue, NewContext, NewExec]),
	  mce_erl_sysOS:mkStateFromCurrentExecutableWithProcess
	    (putProcess(P, NewExec, Conf), State)
      end
  catch
    Error:Reason ->
      maybe_notice_exit(Error, Reason, Conf),
      throw({user, Error, Reason})
  end.

handleTerminated() ->
  State = mce_erl_state:getState(),
  mce_erl_sysOS:mkStateFromCurrentExecutable
    (mce_erl_sys:inform({'EXIT', crashed}, State)).

isTagged({?TRYTAG,_}) ->
  true;
isTagged({?LETTAG,_}) ->
  true;
isTagged({?CHOICETAG,_}) ->
  true;
isTagged({?SENDTAG,_}) ->
  true;
isTagged({?EXITINGTAG,_}) ->
  true;
isTagged({?RECVTAG,_}) ->
  true;
isTagged(_) ->
  false.

doReceive(Exec, SavedState, Conf) ->
  P = Exec#executable.process,
  if P#process.status =:= receivable ->
      {true, {ContFun, Hd, Args, NewQueue}, Context} =
	checkReceive(P),
      NewP =
	P#process{expr={?RECVTAG, {{Hd, Args}, Context}},
		  status=runnable, queue=NewQueue},
      NewExec =
	Exec#executable{process=NewP},
      mce_erl_state:setState
	(mce_erl_sysOS:setCurrentRunContext(NewExec, SavedState)),
      mce_erl_actions:record
	(mce_erl_actions:mk_recv
	 (P#process.pid,Hd)),
      runUserCode(mce_erl:mk_context({ContFun, []}, Context), Conf)
  end.

putProcess(P, Exec, Conf) ->
  {Innermost, Context} =
    case Exec of
      {?CONTEXTTAG, Value} -> Value;
      _ -> {Exec, []}
    end,
  case Innermost of
    {?RECVTAG, {Fun, Timer}} ->
      case checkReceiveContext(Innermost, Context, P) of
	false ->
	  case Timer of
	    {infinity, _} ->
	      P#process{status=blocked, expr=Exec};
	    {Time, _} ->
	      Deadline =
		case mce_conf:is_simulation(Conf) of
		  true ->
		    timeStampPlus(Time, erlang:now());
		  false -> 
		    if Time=/=0 -> infinity; true -> 0 end
		end,
	      P#process{status={timer, Deadline}, expr=Exec}
	  end;
	_ ->
	  P#process{status=receivable, expr=Exec}
      end;
    {?CHOICETAG, _} ->
      P#process{status=choice, expr=Exec};
    {?SENDTAG, _} ->
      P#process{status=sendable, expr=Exec};
    {?EXITINGTAG, _} ->
      P#process{status=exiting, expr=Exec};
    _ ->
      io:format("Internal fault: putProcess(_,~p,_)~n", [Exec]),
      exit(putProcess)
  end.

%%% A new data value have been appended to the queue of P;
%%% we have to check whether the new value is readable and do
%%% a status update.
updProcStatusFromQueue(P) ->
  case updateableStatus(P#process.status) of
    true ->
      case checkReceive(P) of
        false -> P;
        _ -> P#process{status=receivable}
      end;
    false -> 
      P
  end.

updateableStatus(Status) ->
  case Status of
    {inputable,_,_} ->
      true;
    blocked ->
      true;
    {timer,_} ->
      true;
    _ ->
      false
  end.

checkReceive(P) ->
  checkReceiveContext(P#process.expr,P).

checkReceiveContext({?CONTEXTTAG,{Innermost,Context}},P) ->
  checkReceiveContext(Innermost,Context,P);
checkReceiveContext(Expr,P) ->
  checkReceiveContext(Expr,[],P).

checkReceiveContext({?RECVTAG,{Fun,_}},Context,P) ->
  case checkReceiveFun(Fun,P#process.queue) of
    {true,Result} -> {true,Result,Context};
    Other -> Other
  end.

checkReceiveFun(Fun,Queue) ->
  checkReceiveFun(Fun,Queue,[]).
checkReceiveFun(Fun,[],Saved) -> false;
checkReceiveFun(Fun={Mod,RcvFun,Args},[Hd|Rest],Saved) ->
  case apply(Mod,RcvFun,[Hd|Args]) of
    false -> checkReceiveFun(Fun,Rest,[Hd|Saved]);
    {true,ContFun} -> {true,{ContFun,Hd,Args,lists:reverse(Saved,Rest)}}
  end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


initActions() ->
  put(actions,[]).

record_action(Action) ->
  ?LOG("record_action(~p)~n", [Action]),
  put(actions,[Action|get(actions)]).

getActions() ->
  lists:reverse(get(actions)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

random_partition(List) ->
  random_partition(List, [], []).

random_partition([], Left, Right) -> {Left, Right};
random_partition([X| Xs], Left, Right) ->
  U = mce_random:uniform(2),
  if U =:= 1 ->
      random_partition(Xs, Left, [X| Right]);
     true ->
      random_partition(Xs, [X| Left], Right)
  end.

randomise([]) ->
  [];
randomise([X]) ->
  [X];
randomise(Bigger) ->
  {Left, Right} = random_partition(Bigger),
  randomise(Left) ++ randomise(Right).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_transitions_fun() -> fun mce_erl_opsem:transitions/2.

get_commit_fun() -> fun mce_erl_opsem:commit/3.
