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

%% @doc The equivalent of the erlang: module for McErlang.

-module(mcerlang).
-export([
	 spawn/1,spawn_link/1,spawn/2,spawn_link/2,
	 spawn/3,spawn/4, spawn_link/3,spawn_link/4,
	 spawn_monitor/1,spawn_monitor/3,
	 spawn_opt/2,spawn_opt/3,spawn_opt/4,spawn_opt/5,
	 apply/3,
	 self/0,send/2,
	 format/1,format/2,
	 register/2,unregister/1,whereis/1,
	 link/1,unlink/1,process_flag/2,
	 exit/1,exit/2,
	 is_pid/1,monitor/2,demonitor/1,demonitor/2,
	 monitor_node/2,
	 node/0,node/1,
	 make_ref/0, is_reference/1,
	 
	 process_info/1,process_info/2,

	 send_after/3,start_timer/3,cancel_timer/1,read_timer/1,
	 
	 get/0,get/1,put/2,erase/0,erase/1,
	 nget/0,nget/1,nput/2,nerase/0,nerase/1,
	 gget/0,gget/1,gput/2,gerase/0,gerase/1,
	 
	 registered/0, processes/0, nodes/0, 
	 is_alive/0, is_process_alive/1,
	 ping/1,
	 
	 gen_udp_open/1,gen_udp_open/2,
	 gs_start/0,gs_start/1,gs_config/2,gs_create/3,
	 
	 bringNodeUp/1, bringNodeDown/1,
	 setCurrentApplication/3]).

%%-define(debug,true).
-include("../../../src/include/macros.hrl").
-include("emacros.hrl").

-include("system.hrl").
-include("process.hrl").
-include("node.hrl").

-define(IN_MCE,
		begin
			State = erlang:get(globalState),
			if
				is_record(State,system) ->
					true;
				true ->
					false
			end
		end).

-define(call_fun(MCE,ERL),
		case ?IN_MCE of
			true  -> MCE;
			false -> ERL
		end).

spawn(Fun) ->
	?call_fun(mce_spawn(Fun),
			  erlang:spawn(Fun)).
mce_spawn(Fun) ->
  ensure_function(Fun),
  spawn_internal(void,{Fun,[]},[]).

spawn_link(Fun) ->
	?call_fun(mce_spawn_link(Fun),
			  erlang:spawn_link(Fun)).
mce_spawn_link(Fun) ->
  ensure_function(Fun),
  spawn_internal(void,{Fun,[]},[link]).

spawn(Node,Fun) ->
	?call_fun(mce_spawn(Node,Fun),
			  erlang:spawn(Node,Fun)).
mce_spawn(Node,Fun) ->
  ensure_function(Fun),
  ensure_atom(Node),
  spawn_internal(Node,{Fun,[]},[]).

spawn_link(Node,Fun) ->
	?call_fun(mce_spawn_link(Node,Fun),
			  erlang:spawn_link(Node,Fun)).
mce_spawn_link(Node,Fun) ->
  ensure_function(Fun),
  ensure_atom(Node),
  spawn_internal(Node,{Fun,[]},[link]).

spawn(Module,F,Args) ->
	?call_fun(mce_spawn(Module,F,Args),
			  erlang:spawn(Module,F,Args)).
mce_spawn(Module,F,Args) ->
  ensure_atom(Module),
  ensure_atom(F),
  ensure_list(Args),
  spawn_internal(void,{Module,F,Args},[]).

spawn(Node,Module,F,Args) ->
	?call_fun(mce_spawn(Node,Module,F,Args),
			  erlang:spawn(Node,Module,F,Args)).
mce_spawn(Node,Module,F,Args) ->
  ensure_atom(Module),
  ensure_atom(F),
  ensure_list(Args),
  ensure_atom(Node),
  spawn_internal(Node,{Module,F,Args},[]).

spawn_link(Module,F,Args) ->
	?call_fun(mce_spawn_link(Module,F,Args),
			  erlang:spawn_link(Module,F,Args)).
mce_spawn_link(Module,F,Args) ->
  ensure_atom(Module),
  ensure_atom(F),
  ensure_list(Args),
  spawn_internal(void,{Module,F,Args},[link]).

spawn_link(Node,Module,F,Args) ->
	?call_fun(mce_spawn_link(Node,Module,F,Args),
			  erlang:spawn_link(Node,Module,F,Args)).
mce_spawn_link(Node,Module,F,Args) ->
  ensure_atom(Module),
  ensure_atom(F),
  ensure_list(Args),
  ensure_atom(Node),
  spawn_internal(Node,{Module,F,Args},[link]).

spawn_monitor(Fun) ->
  ensure_function(Fun),
  spawn_internal(void,{Fun,[]},[monitor]).

spawn_monitor(Module,F, Args) ->
  ensure_atom(Module),
  ensure_atom(F),
  ensure_list(Args),
  spawn_internal(void,{Module,F,Args},[monitor]).

spawn_opt(Fun,Options) ->
  ensure_function(Fun),
  ensure_list(Options),
  spawn_internal(void,{Fun,[]},Options).

spawn_opt(Node,Fun,Options) ->
  ensure_function(Fun),
  ensure_list(Options),
  ensure_atom(Node),
  spawn_internal(Node,{Fun,[]},Options).

spawn_opt(Module,F,Args,Options) ->
  ensure_atom(Module),
  ensure_atom(F),
  ensure_list(Args),
  ensure_list(Options),
  spawn_internal(void,{Module,F,Args},Options).

spawn_opt(Node,Module,F,Args,Options) ->
  ensure_atom(Module),
  ensure_atom(F),
  ensure_list(Args),
  ensure_list(Options),
  ensure_atom(Node),
  spawn_internal(Node,{Module,F,Args},Options).

spawn_internal(Node, Fun, Args) ->
  ?LOG("spawn_internal(~p,~p,~p)~n", [Node, Fun, Args]),
  State = mce_erl_state:getState(),
  ?LOG("State is ~p~n", [State]),
  %% Monitor is not allowed in spawn_opt when spawning remote nodes???
  SelfPid = mce_erl_sysOS:self(State),
  case Node =/= void andalso lists:member(monitor, Args) of
    true -> erlang:error(badarg);
    _ -> ok
  end,
  NewFun =
    case Fun of
      {FunModule,FunName,FunArgs} ->
	{NewModule,NewFunction} =
	  mce_erl_compile_info:map_fun(FunModule,FunName,FunArgs),
	{NewModule,NewFunction,FunArgs};
      _ -> Fun
    end,
  case Node =:= void orelse mce_erl_sysOS:can_send_locally(Node, State) of
    true ->
      case mce_erl_sysOS:createLocalProc
	(mce_erl_sysOS:node(State), NewFun, Args, State)
	of
	{ok, {S1, Pid}} ->
	  ?LOG("Setting state after spawning to~n  ~p~n", [S1]),
	  mce_erl_state:setState(S1),
	  mce_erl_actions:record
	    (mce_erl_actions:mk_spawn(SelfPid,[Fun,Args],Pid)),
	  Pid;
	{ok, {S1, MonitorRef, Pid}} ->
	  ?LOG("Setting state after spawning to~n  ~p~n", [S1]),
	  mce_erl_state:setState(S1),
	  mce_erl_actions:record
	    (mce_erl_actions:mk_spawn(SelfPid,[Fun,Args],Pid)),
	  {Pid, MonitorRef}
      end;
    false ->
      Link =
	case lists:member(link, Args) of
	  true -> {yes, SelfPid};
	  false -> no
	end,
      mce_erl_actions:record
	(mce_erl_actions:mk_spawn
	 (SelfPid,[Fun,Args],waitingForPid)),
      S1 =
	mce_erl_sysOS:ensureNodeAlive(Node, State),
      S2 =
	mce_erl_sysOS:deliverSignalToNode
	  (SelfPid, Node, {spawn, Fun, Link, SelfPid}, S1),
      ?LOG("Node ~p not local; setting state to~n  ~p~n", [Node, S2]),
      mce_erl_state:setState(S2),
      mce_erl_systemCode:waitForSpawningNode
	(lists:member(link, Args),
	 lists:member(monitor, Args))
  end.

apply(Module,Function,Args) ->
	?call_fun(mce_apply(Module,Function,Args),
			  erlang:apply(Module,Function,Args)).
mce_apply(Module,Function,Args) ->
  {NewModule,NewFunction} = mce_erl_compile_info:map_fun(Module,Function,Args),
  erlang:apply(NewModule,NewFunction,Args).

self() ->
	?call_fun(mce_self(),
			  erlang:self()).
mce_self() ->
  mce_erl_sysOS:self(mce_erl_state:getState()).

send(Pid, Msg) ->
	?call_fun(mce_send(Pid, Msg),
			  erlang:send(Pid, Msg)).
mce_send(Pid, Msg) ->
  State = mce_erl_state:getState(),
  ResolvedPid = resolvePid(Pid, State),
  SelfPid = mce_erl_sysOS:self(State),
  mce_erl_actions:record
    (mce_erl_actions:mk_send(SelfPid,Pid,ResolvedPid,Msg)),
  mce_erl_state:setState
    (mce_erl_sysOS:unconditionalSend(SelfPid,ResolvedPid,Msg,State)),
  Msg.

resolvePid(Pid, State) ->
  case Pid of
    _ when is_atom(Pid) ->
      case mce_erl_sysOS:resolvePid(Pid, State) of
	{ok, ResolvedPid} ->
	  ResolvedPid;
	no ->
	  ?LOG("resolvePid(~p): bad pid~n",[Pid]),
	  erlang:error(badarg)
      end;
    {Name, Node} when is_atom(Name), is_atom(Node) ->
      Pid;
    Other ->
      case (?MODULE):is_pid(Other) of
	true ->
	  Pid;
	_ ->
	  ?LOG("resolvePid(~p): is not a pid~n",[Pid]),
	  erlang:error(badarg)
      end
  end.

register(Name, Pid) ->
	?call_fun(mce_register(Name, Pid),
			  erlang:register(Name, Pid)).
mce_register(Name, Pid) ->
  ensure_pid(Pid),
  ensure_atom(Name),
  State = mce_erl_state:getState(),
  ?LOG("~p: register(~p,~p)~n",
       [mce_erl_sysOS:self(State), Name, Pid]),
  case mce_erl_sysOS:is_local_node(?MODULE:node(Pid), State) of
    true ->
      ok;
    false ->
      erlang:error(badarg)
  end,
  ?LOG("~p: registering process ~p under the name ~p~n",
       [(?MODULE):self(), Pid, Name]),
  mce_erl_actions:record
    (mce_erl_actions:mk_api_call
     (mce_erl_sysOS:self(State),register,[Name,Pid],true)),
  case mce_erl_sysOS:can_send_locally(?MODULE:node(Pid), State) of
    true ->
      case mce_erl_sysOS:doRegister(Name, Pid, State) of
	{ok, S1} ->
	  mce_erl_state:setState(S1),
	  true;
	Other ->
	  erlang:error(badarg)
      end;
    false ->
      mce_erl_state:setState
	(mce_erl_sysOS:deliverSignalToNode
	 ((?MODULE):node(Pid),
	  {register, Name, Pid, mce_erl_sysOS:self(State)}, State)),
      mce_erl_systemCode:waitForReply()
  end.

unregister(Name) ->
	?call_fun(mce_unregister(Name),
			  erlang:unregister(Name)).
mce_unregister(Name) ->
  ensure_atom(Name),
  State = mce_erl_state:getState(),
  Node = ?MODULE:node(),
  mce_erl_actions:record
    (mce_erl_actions:mk_api_call
     (mce_erl_sysOS:self(State),unregister,[Name],true)),
  case mce_erl_sysOS:can_send_locally(Node, State) of
    true ->
      case mce_erl_sysOS:unRegister(Name, State) of
	{ok, NewState} ->
	  mce_erl_state:setState(NewState),
	  true;
	_ ->
	  erlang:error(badarg)
      end;
    false ->
      mce_erl_state:setState
	(mce_erl_sysOS:deliverSignalToNode
	 (Node, {unregister, Name, mce_erl_sysOS:self(State)}, State)),
      mce_erl_systemCode:waitForReply()
  end.
      
whereis(Name) ->
  ?call_fun(mce_whereis(Name),erlang:whereis(Name)).

mce_whereis(Name) ->
  ensure_atom(Name),
  State = mce_erl_state:getState(),
  Node = ?MODULE:node(),
  case mce_erl_sysOS:can_send_locally(Node, State) of
    true ->
      case mce_erl_sysOS:getPidByRegisteredName(Name, State) of
	{ok, RPid} -> RPid;
	_ -> undefined
      end;
    false ->
      mce_erl_state:setState
	(mce_erl_sysOS:deliverSignalToNode
	 (Node, {whereis, Name, mce_erl_sysOS:self(State)}, State)),
      mce_erl_systemCode:waitForReply()
  end.

format(Format, Args) ->
	?call_fun(mce_format(Format, Args),
			  io:format(Format, Args)).
mce_format(Format, Args) ->
  State = mce_erl_state:getState(),
  case output_enabled() of
    true -> io:format(Format, Args);
    _ -> ok
  end.

format(Format) ->
	?call_fun(mce_format(Format),
			  io:format(Format)).
mce_format(Format) ->
  State = mce_erl_state:getState(),
  case output_enabled() of
    true -> io:format(Format);
    _ -> ok
  end.

link(Pid) ->
	?call_fun(mce_link(Pid),
			  erlang:link(Pid)).
mce_link(Pid) ->
  ensure_pid(Pid),
  State = mce_erl_state:getState(),
  Own_Pid = mce_erl_sysOS:self(State),
  ?LOG("Linking processes ~p and ~p~n", [Own_Pid, Other_Pid]),
  mce_erl_actions:record
    (mce_erl_actions:mk_api_call(Own_Pid,link,[Pid],true)),
  case mce_erl_sysOS:can_send_locally(?MODULE:node(Pid), State) of
    true ->
      case mce_erl_sysOS:getProcessByPid(Pid, State) of
	{ok, Other_P} ->
	  Other_Pid = Other_P#process.pid,
	  if Own_Pid =:= Other_Pid -> true;
	     true ->
	      S1 = mce_erl_sysOS:addLink(Other_Pid, Own_Pid, State),
	      S2 = mce_erl_sysOS:addLink(Own_Pid, Other_Pid, S1),
	      mce_erl_state:setState(S2),
	      link
	  end;
	_ ->
	  LocalProc = mce_erl_state:getProcess(State),
	  Flags = LocalProc#process.flags,
	  if Flags#processFlags.trap_exit ->
	      mce_erl_state:setState
		(mce_erl_sysOS:sendMsgToPid(Own_Pid,{'EXIT',Pid,noproc},State));
	     true ->
	      (?MODULE):exit(noproc)
	  end
      end;
    %% The pid we are linking to is not local
    false ->
      S1 =
	mce_erl_sysOS:addLink(Own_Pid, Pid, State),
      S2 =
	mce_erl_sysOS:deliverSignalToNode
	  ((?MODULE):node(Pid), {link, Own_Pid, Pid}, S1),
      mce_erl_state:setState(S2),
      true
  end.

%%% Buggy. See erlang documentation for necessary correction.
unlink(Pid) ->
	?call_fun(mce_unlink(Pid),
			  erlang:unlink(Pid)).
mce_unlink(Pid) ->
  ensure_pid(Pid),
  State = mce_erl_state:getState(),
  SelfPid = mce_erl_sysOS:self(State),
  case mce_erl_sysOS:can_send_locally(?MODULE:node(Pid), State) of
    true ->
      case mce_erl_sysOS:getProcessByPid(Pid, State) of
	{ok, Other_P} ->
	  Other_Pid = Other_P#process.pid,
	  ?LOG("Unlinking processes ~p and ~p~n", [SelfPid, Other_Pid]),
	  mce_erl_actions:record
	    (mce_erl_actions:mk_api_call(SelfPid,unlink,[Pid],true)),
	  if SelfPid =:= Other_Pid ->
	      true;
	     true ->
	      S1 = mce_erl_sysOS:removeLink(Other_Pid, SelfPid, State),
	      S2 = mce_erl_sysOS:removeLink(SelfPid, Other_Pid, S1),
	      mce_erl_state:setState(S2),
	      true
	  end;
	_ ->
	  true
      end;
    %% The pid we are unlinking from is not local
    false ->
      mce_erl_actions:record
	(mce_erl_actions:mk_api_call(SelfPid,unlink,[Pid],true)),
      S1 =
	mce_erl_sysOS:removeLink(SelfPid, Pid, State),
      S2 =
	mce_erl_sysOS:deliverSignalToNode
	  (SelfPid, (?MODULE):node(Pid), {unlink, SelfPid, Pid}, S1),
      mce_erl_state:setState(S2),
      true
  end.

process_flag(Var, Value) ->
	?call_fun(mce_process_flag(Var, Value),
			  erlang:process_flag(Var, Value)).
mce_process_flag(trap_exit, Value) ->
  State = mce_erl_state:getState(),
  ?LOG("Setting process flag trap_exit to ~p~n", [Value]),
  P = mce_erl_state:getProcess(State),
  Trap_status = (P#process.flags)#processFlags.trap_exit,
  mce_erl_state:setState
    (mce_erl_state:setFlags(#processFlags{trap_exit=Value}, State)),
  mce_erl_actions:record
    (mce_erl_actions:mk_api_call
     (mce_erl_sysOS:self(State),
      process_flag,[trap_exit,Value],Trap_status)),
  Trap_status;
mce_process_flag(do_terminate, Value) ->
  State = mce_erl_state:getState(),
  ?LOG("Setting process flag do_terminate to ~p~n", [Value]),
  P = mce_erl_state:getProcess(State),
  Do_Terminate_status = (P#process.flags)#processFlags.do_terminate,
  mce_erl_state:setState
    (mce_erl_state:setFlags(#processFlags{do_terminate=Value}, State)),
  mce_erl_actions:record
    (mce_erl_actions:mk_api_call
     (mce_erl_sysOS:self(State),
      process_flag,[do_terminate,Value],Do_Terminate_status)),
  Do_Terminate_status;
mce_process_flag(priority, high) ->
  %io:format("WARNING: process_flag(priority,...) not supported, continuing~n"),
  ?LOG("WARNING: process_flag(priority,...) not supported, continuing~n",[]),
  normal.

exit(Reason) ->
  erlang:exit(Reason).

exit(Pid, Reason) ->
	?call_fun(mce_exit(Pid, Reason),
			  erlang:exit(Pid, Reason)).
mce_exit(Pid, Reason) ->
  State = mce_erl_state:getState(),
  ?LOG("~p: exit(~p,~p) called~n", [mce_erl_sysOS:self(State), Pid, Reason]),
  SelfPid = mce_erl_sysOS:self(State),
  mce_erl_actions:record
    (mce_erl_actions:mk_api_call
     (SelfPid,exit,[Pid,Reason],true)),
  case SelfPid =:= Pid of
    true -> (?MODULE):exit(Reason);
    false ->
      case mce_erl_sysOS:can_send_locally(?MODULE:node(Pid), State) of
	true ->
	  S =
	    mce_erl_state:setState
	      (mce_erl_sysOS:exitDeliverNow(Pid, Pid, Reason, State)),
	  %% Check if we died too...
	  case mce_erl_state:getProcess(S) of
	    none -> erlang:exit(Reason);
	    _ -> true
	  end;
	false ->
	  mce_erl_state:setState
	    (mce_erl_sysOS:deliverSignalToNode
	     (SelfPid, (?MODULE):node(Pid), {exit, Pid, Reason}, State)),
	  true
      end
  end.

ensure_pid(P) ->
  case (?MODULE):is_pid(P) of
    true ->
      true;
    false ->
      erlang:error(badarg)
  end.
ensure_atom(A) when is_atom(A) -> ok;
ensure_atom(_) -> erlang:error(badarg).
ensure_list(L) when is_list(L) -> ok;
ensure_list(_) -> erlang:error(badarg).
ensure_function(F) when is_function(F) -> ok;
ensure_function(_) -> erlang:error(badarg).
ensure_boolean(true) -> ok;
ensure_boolean(false) -> ok;
ensure_boolean(_) -> erlang:error(badarg).

is_pid({pid,_,Value}) when is_integer(Value) -> true;
is_pid(T) -> false.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Start monitoring a process
monitor(process, Item) ->
	?call_fun(mce_monitor(process, Item),
			  erlang:monitor(process, Item)).
mce_monitor(process, Item) ->
  ?LOG("in monitor(process,~p)~n", [Item]),
  State = mce_erl_state:getState(),
  ParsedItem = parseItem(Item, State),
  ?LOG("Parsed ~p~n", [ParsedItem]),
  {Node, NodeItem, IsLocal} =
    case ParsedItem of
      {pid, N, _} ->
	case mce_erl_sysOS:can_send_locally(N, State) of
	  true -> {N, ParsedItem, true};
	  false -> {N, ParsedItem, false}
	end;
      {Name, N} ->
	case mce_erl_sysOS:can_send_locally(N, State) of
	  true -> {N, ParsedItem, true};
	  false -> {N, ParsedItem, false}
	end
    end,
  SelfPid = mce_erl_sysOS:self(State),
  MonitorRef =
    mce_erl_references:mkNewMonitorRef({SelfPid, Node, NodeItem}, State),
  MonitorItem = {monitor, SelfPid, Node, NodeItem, MonitorRef},
  mce_erl_actions:record
    (mce_erl_actions:mk_api_call
     (SelfPid,monitor,[process,NodeItem],MonitorItem)),
  case IsLocal of
    true ->
      ?LOG("Monitor ~p~n", [MonitorItem]),
      {IsAlive, Pid} =
	case (?MODULE):is_pid(NodeItem) of
	  true ->
	    case mce_erl_sysOS:getProcessByPid(NodeItem, State) of
	      {ok, Other_P} -> {true, NodeItem};
	      Other -> {false, NodeItem}
	    end;
	  false ->
	    RegisteredName =
	      case NodeItem of
		{RegName, _} -> RegName;
		RegName -> RegName
	      end,
	    case mce_erl_sysOS:getPidByRegisteredName(RegisteredName, State) of
	      {ok, RPid} ->
		case mce_erl_sysOS:getProcessByPid(RPid, State) of
		  {ok, _} -> {true, RPid};
		  Other -> {false, RegisteredName}
		end;
	      Other -> {false, RegisteredName}
	    end
	end,
      case IsAlive of
	true ->
	  mce_erl_state:setState
	    (mce_erl_sysOS:addMonitor
	     (MonitorRef, SelfPid, Pid, NodeItem, State));
	false ->
	  ?LOG("Pid ~p is already dead, will send DOWN~n", [Pid]),
	  DOWNMessage = {'DOWN', MonitorRef, process, ParsedItem, noproc},
	  mce_erl_state:setState
	    (mce_erl_sysOS:unconditionalSend
	     (?MODULE:node(SelfPid), SelfPid, DOWNMessage, State))
      end,
      MonitorRef;
    false ->
      mce_erl_state:setState
	(mce_erl_sysOS:deliverSignalToNode
	 (SelfPid, Node, {monitor, MonitorItem}, State)),
      MonitorRef
  end.

parseItem(P, State) ->
  case (?MODULE):is_pid(P) of
    true -> P;
    false ->
      case P of
	{RegName, Node} when is_atom(RegName), is_atom(Node) -> {RegName, Node};
	RegName when is_atom(RegName) -> {RegName, mce_erl_sysOS:node(State)};
	_ -> erlang:error(badarg)
      end
  end.

%%% Cancel monitoring of a process
demonitor_internal(MonitorRef, Flushing) ->
  case MonitorRef of
    {monitorRef, {MonitoringPid, Node, NodeItem}, _} ->
      State = mce_erl_state:getState(),
      SelfPid = mce_erl_sysOS:self(State),
      if SelfPid =/= MonitoringPid ->
	  erlang:error(badarg);
	 true ->
	  mce_erl_actions:record
	    (mce_erl_actions:mk_api_call
	     (SelfPid,demonitor,[MonitorRef,Flushing],MonitorRef)),
	  S = mce_erl_sysOS:deleteMonitor(MonitorRef, State),
	  S1 = mce_erl_sysOS:deleteDownsInNode(MonitorRef, S),
	  if Flushing ->
	      mce_erl_state:setState
		(mce_erl_sysOS:deleteDownsInQueue(MonitorRef, S1));
	     true ->
	      mce_erl_state:setState(S1)
	  end
      end;
    _ -> erlang:error(badarg)
  end.

demonitor(MonitorItem) ->
	?call_fun(mce_demonitor(MonitorItem),
			  erlang:demonitor(MonitorItem)).
mce_demonitor(MonitorItem) ->
  demonitor_internal(MonitorItem,false).

demonitor(MonitorItem,Args) ->
	?call_fun(mce_demonitor(MonitorItem,Args),
			  erlang:demonitor(MonitorItem,Args)).
mce_demonitor(MonitorItem,[]) ->
  demonitor_internal(MonitorItem,false);
mce_demonitor(MonitorItem,[flush]) ->
  demonitor_internal(MonitorItem,true).

monitor_node(Node, Flag) ->
	?call_fun(mce_monitor_node(Node, Flag),
			  erlang:monitor_node(Node, Flag)).
mce_monitor_node(Node, Flag) ->
  ensure_boolean(Flag),
  if is_atom(Node) ->
      State = mce_erl_state:getState(),
      SelfPid = mce_erl_sysOS:self(State),
      mce_erl_actions:record
	(mce_erl_actions:mk_api_call
	 (SelfPid,monitor_node,[Node,Flag],true)),
      if Flag ->
	  mce_erl_state:setState
	    (mce_erl_sysOS:deliverSignalToNode
	     (SelfPid, Node, {monitor_node, SelfPid}, State)),
	  true;
	 true ->
	  S = mce_erl_sysOS:deleteMonitorNode(Node, SelfPid, State),
	  S1 = mce_erl_sysOS:deleteNodeDownsInNode(Node, SelfPid, S),
	  mce_erl_state:setState
	    (mce_erl_sysOS:deleteNodeDownsInQueue(Node, SelfPid, S1)),
	  true
      end;
     true -> erlang:error(badarg)
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

node() ->
	?call_fun(mce_node(),
			  erlang:node()).
mce_node() -> mce_erl_sysOS:node(mce_erl_state:getState()).

node(Arg) -> 
  mce_node(Arg).

mce_node({pid,Node,_}) ->
  Node;
mce_node({monitorRef,{_,Node,_},_}) -> 
  Node;
mce_node(Arg) ->
  ?call_fun(erlang:error(badarg),
	    erlang:node(Arg)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setCurrentApplication(Module, Function, Arguments) ->
  State = mce_erl_state:getState(),
  P = mce_erl_state:getProcess(State),
  NewP = P#process{expr={Module, Function, Arguments}},
  mce_erl_state:setState(mce_erl_state:setProcess(NewP, State)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Process dictionary -- we should probably sort it to normalize
%%% the process.

put(Key, Value) ->
	?call_fun(mce_put(Key, Value),
			  erlang:put(Key, Value)).
mce_put(Key, Value) ->
  State = mce_erl_state:getState(),
  P = mce_erl_state:getProcess(State),
  {OldValue, NewDict} = mce_erl_sysOS:put(Key, Value, State),
  NewP = P#process{dict=NewDict},
  mce_erl_state:setState(mce_erl_state:setProcess(NewP, State)),
  mce_erl_actions:record
    (mce_erl_actions:mk_api_call
     (mce_erl_sysOS:self(State),put,[Key,Value],OldValue)),
  OldValue.

nput(Key, Value) ->
  State = mce_erl_state:getState(),
  N = mce_erl_state:getNode(State),
  {OldValue, NewDict} = mce_erl_sysOS:nput(Key, Value, State),
  NewN = N#node{dict=NewDict},
  mce_erl_state:setState(mce_erl_state:setNode(NewN, State)),
  mce_erl_actions:record
    (mce_erl_actions:mk_api_call
     (mce_erl_sysOS:self(State),nput,[Key,Value],OldValue)),
  OldValue.

gput(Key, Value) ->
  State = mce_erl_state:getState(),
  {OldValue, NewDict} = mce_erl_sysOS:gput(Key, Value, State),
  mce_erl_state:setState(mce_erl_state:setDict(NewDict, State)),
  mce_erl_actions:record
    (mce_erl_actions:mk_api_call
     (mce_erl_sysOS:self(State),gput,[Key,Value],OldValue)),
  OldValue.

get() -> mce_erl_sysOS:get(mce_erl_state:getState()).

nget() -> mce_erl_sysOS:nget(mce_erl_state:getState()).

gget() -> mce_erl_sysOS:gget(mce_erl_state:getState()).

get(Key) ->
	?call_fun(mce_get(Key),
			  erlang:get(Key)).
mce_get(Key) ->
  mce_erl_sysOS:get(Key, mce_erl_state:getState()).

nget(Key) ->
  mce_erl_sysOS:nget(Key, mce_erl_state:getState()).

gget(Key) ->
  mce_erl_sysOS:gget(Key, mce_erl_state:getState()).

erase() ->
	?call_fun(mce_erase(),
			  erlang:erase()).
mce_erase() ->
  State = mce_erl_state:getState(),
  P = mce_erl_state:getProcess(State),
  Dict = mce_erl_sysOS:get(State),
  mce_erl_actions:record
    (mce_erl_actions:mk_api_call
     (mce_erl_sysOS:self(State),erase,[],Dict)),
  mce_erl_state:setState(mce_erl_state:setProcess(P#process{dict=[]}, State)),
  Dict.

nerase() ->
  State = mce_erl_state:getState(),
  N = mce_erl_state:getNode(State),
  Dict = mce_erl_sysOS:nget(State),
  mce_erl_actions:record
    (mce_erl_actions:mk_api_call
     (mce_erl_sysOS:self(State),nerase,[],Dict)),
  mce_erl_state:setState(mce_erl_state:setNode(N#node{dict=[]}, State)),
  Dict.

gerase() ->
  State = mce_erl_state:getState(),
  Dict = mce_erl_state:getDict(State),
  mce_erl_actions:record
    (mce_erl_actions:mk_api_call
     (mce_erl_sysOS:self(State),gerase,[],Dict)),
  mce_erl_state:setState(mce_erl_state:setDict([], State)),
  Dict.

erase(Key) ->
	?call_fun(mce_erase(Key),
			  erlang:erase(Key)).
mce_erase(Key) ->
  State = mce_erl_state:getState(),
  P = mce_erl_state:getProcess(State),
  {Value, NewDict} = mce_erl_sysOS:erase(Key, State),
  mce_erl_state:setState
    (mce_erl_state:setProcess(P#process{dict=NewDict}, State)),
  mce_erl_actions:record
    (mce_erl_actions:mk_api_call
     (mce_erl_sysOS:self(State),erase,[Key],Value)),
  Value.

nerase(Key) ->
  State = mce_erl_state:getState(),
  N = mce_erl_state:getNode(State),
  {Value, NewDict} = mce_erl_sysOS:nerase(Key, State),
  mce_erl_state:setState(mce_erl_state:setNode(N#node{dict=NewDict}, State)),
  mce_erl_actions:record
    (mce_erl_actions:mk_api_call
     (mce_erl_sysOS:self(State),nerase,[Key],Value)),
  Value.

gerase(Key) ->
  State = mce_erl_state:getState(),
  {Value, NewDict} = mce_erl_sysOS:nerase(Key, State),
  mce_erl_state:setState(mce_erl_state:setDict(NewDict, State)),
  mce_erl_actions:record
    (mce_erl_actions:mk_api_call
     (mce_erl_sysOS:self(State),gerase,[Key],Value)),
  Value.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% If necessary boot a node; if node is already alive, do nothing.
%%%
%% @spec bringNodeUp(NodeName::atom()) -> any()
%% @doc
%% Starts a new node with the name `NodeName'.
bringNodeUp(NodeName) ->
  State = mce_erl_state:getState(),
  mce_erl_state:setState(mce_erl_sysOS:ensureNodeAlive(NodeName, State)),
  ok.

bringNodeDown(NodeName) ->
  State = mce_erl_state:getState(),
  OwnNode = mce_erl_sysOS:node(State),
  mce_erl_state:setState(mce_erl_sysOS:bringDownNode(NodeName, State)),
  if NodeName =:= OwnNode#node.name ->
      erlang:exit(nodeCrash);
     true ->
      ok
  end.

ping(Node) ->
	?call_fun(mce_ping(Node),
			  net_adm:ping(Node)).
mce_ping(Node) ->
  bringNodeUp(Node),
  pong.

is_alive() ->
	?call_fun(mce_is_alive(),
			  erlang:is_alive()).
mce_is_alive() ->
  true.

nodes() ->
	?call_fun(mce_nodes(),
			  erlang:nodes()).
mce_nodes() ->
  State = mce_erl_state:getState(),
  lists:map(fun (N) -> N#node.name end,
	    [mce_erl_state:getNode(State)| mce_erl_state:getOtherNodes(State)]).

processes() ->
	?call_fun(mce_processes(),
			  erlang:processes()).
mce_processes() ->
  State = mce_erl_state:getState(),
  Node = mce_erl_state:getNode(State),
  Processes =
    case mce_erl_state:getProcess(State) of
      none -> mce_erl_state:getOtherProcesses(State);
      P -> [P| mce_erl_state:getOtherProcesses(State)]
    end,
  lists:map(fun (P) -> P#process.pid end, Processes).

registered() ->
	?call_fun(mce_registered(),
			  erlang:registered()).
mce_registered() ->
  State = mce_erl_state:getState(),
  Node = mce_erl_state:getNode(State),
  lists:map(fun ({Name, _}) -> Name end,
	    Node#node.registered).

is_process_alive(Pid) ->
	?call_fun(mce_is_process_alive(Pid),
			  erlang:is_process_alive(Pid)).
mce_is_process_alive(Pid) ->
  State = mce_erl_state:getState(),
  Node = mce_erl_state:getNode(State),
  NodeProcesses = Node#node.processes,
  Processes =
    case mce_erl_state:getProcess(State) of
      none -> NodeProcesses;
      P -> [P| NodeProcesses]
    end,
  case mce_utils:find(fun (Proc) -> Proc#process.pid =:= Pid end, Processes)
    of
    {ok, _} ->
      true;
    _ ->
      false
  end.

make_ref() ->
	?call_fun(mce_make_ref(),
			  erlang:make_ref()).
mce_make_ref() ->
  State = mce_erl_state:getState(),
  mce_erl_references:mkReference(mce_erl_sysOS:self(State), State).

is_reference(Arg) ->
	?call_fun(mce_is_reference(Arg),
			  erlang:is_reference(Arg)).
mce_is_reference({monitorRef,_,_}) ->
  true;
mce_is_reference({mc_erl_ref,_,_}) ->
  true;
mce_is_reference(_) ->
  false.

process_info(Pid) ->
	?call_fun(mce_process_info(Pid),
			  erlang:process_info(Pid)).
mce_process_info(Pid) ->
  case (?MODULE):is_pid(Pid) of
    false ->
      erlang:error(badarg, [Pid]);
    true ->
      State = mce_erl_state:getState(),
      case mce_erl_sysOS:is_local_node((?MODULE):node(Pid), State) of
	false ->
	  erlang:error(badarg, [Pid]);
	true ->
	  case mce_erl_sysOS:getProcessByPid(Pid, State) of
	    {ok, P} ->
	      process_info_argument(State, P, current_function) ++
		process_info_argument(State, P, dictionary) ++
		process_info_argument(State, P, error_handler) ++
		process_info_argument(State, P, group_leader) ++
		process_info_argument(State, P, heap_size) ++
		process_info_argument(State, P, initial_call) ++
		process_info_argument(State, P, links) ++
		process_info_argument(State, P, message_queue_len) ++
		process_info_argument(State, P, messages) ++
		process_info_argument(State, P, priority) ++
		process_info_argument(State, P, reductions) ++
		process_info_argument(State, P, registered_name) ++
		process_info_argument(State, P, stack_size) ++
		process_info_argument(State, P, status) ++
		process_info_argument(State, P, trap_exit);
	    _ ->
	      undefined
	  end
      end
  end.

process_info(Pid, Argument) ->
	?call_fun(mce_process_info(Pid, Argument),
			  erlang:process_info(Pid, Argument)).
mce_process_info(Pid, Argument) ->
  case (?MODULE):is_pid(Pid) of
    false ->
      erlang:error(badarg, [Pid]);
    true ->
      State = mce_erl_state:getState(),
      case mce_erl_sysOS:is_local_node((?MODULE):node(Pid), State) of
	false ->
	  erlang:error(badarg, [Pid]);
	true ->
	  case mce_erl_sysOS:getProcessByPid(Pid, State) of
	    {ok, P} ->
	      case process_info_argument(State, P, Argument) of
		[Result] -> Result;
		Other -> Other
	      end;
	    _ ->
	      undefined
	  end
      end
  end.

dig_function(E) ->
  case E of
    {?RECVTAG,{Fun, _}} -> Fun;
    {?CHOICETAG,{Label,FunList}} -> {mcerlang,choice,[Label,FunList]};
    {?CONTEXTTAG,{Expr,_}} -> dig_function(Expr);
    {?SENDTAG,{Fun,_}} -> Fun;
    {Module,Fun,Args} -> {Module,Fun,Args};
    {Fun,Args} -> {Fun,Args};
    Other ->
      io:format("dig_function(~p)~n",[Other]),
      throw(dig_function)
  end.

process_info_argument(State, P, registered_name) ->
  RegisterMap = (mce_erl_state:getNode(State))#node.registered,
  search_regmap(P#process.pid, RegisterMap);
process_info_argument(State, P, current_function) ->
  [{current_function, dig_function(P#process.expr)}];
process_info_argument(State, P, dictionary) ->
  [];
process_info_argument(State, P, error_handler) ->
  [];
process_info_argument(State, P, group_leader) ->
  [];
process_info_argument(State, P, heap_size) ->
  [{heap_size, 1}];
process_info_argument(State, P, initial_call) ->
  [{initial_call, {void, void, []}}];
process_info_argument(State, P, links) ->
  [];
process_info_argument(State, P, message_queue_len) ->
  [{message_queue_len, length(P#process.queue)}];
process_info_argument(State, P, messages) ->
  [{messages, P#process.queue}];
process_info_argument(State, P, priority) ->
  [{priority, normal}];
process_info_argument(State, P, reductions) ->
  [{reductions, 0}];
process_info_argument(State, P, stack_size) ->
  [{stack_size, 1}];
process_info_argument(State, P, status) ->
  SelfPid = mce_erl_sysOS:self(mce_erl_state:getState()),
  PPid = P#process.pid,
  case SelfPid =:= PPid of
    true -> [{status, running}];
    _ ->
      case P#process.status of
	{timer, _} -> [{status, timer}];
	receivable -> [{status, runnable}];
	blocked -> [{status, waiting}];
	{choice, _} -> [{status, runnable}];
	Other -> [{status, {strange, Other}}]
      end
  end;
process_info_argument(State, P, trap_exit) ->
  Flags = P#process.flags,
  [{trap_exit, Flags#processFlags.trap_exit}];
process_info_argument(State, P, backtrace) ->
  [];
process_info_argument(State, P, last_calls) ->
  [];
process_info_argument(State, P, memory) ->
  [{memory, 0}];
process_info_argument(State, P, monitored_by) ->
  [];
process_info_argument(State, P, monitors) ->
  [].

search_regmap(Pid,[]) -> [];
search_regmap(Pid,[{Name,Pid}|Rest]) -> [{registered_name, Name}];
search_regmap(Pid,[_|Rest]) -> search_regmap(Pid, Rest).

output_enabled() ->
  erlang:get(output_enabled).

gen_udp_open(Port) ->
  ?call_fun(mce_gen_udp_open(Port),erlang:gen_udp_open(Port)).
mce_gen_udp_open(Port) ->
  gen_udp_open(Port,[]).

gen_udp_open(Port, Options) ->
  ?call_fun(mce_gen_udp_open(Port, Options), erlang:gen_udp_open(Port, Options)).
mce_gen_udp_open(Port, Options) ->
  mce_erl_rcv_agent:gen_udp_open(Port, Options).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gs_start(Args) ->
  ?call_fun(mce_gs_start(Args),gs:start(Args)).
mce_gs_start(_) ->
  gs_start().

gs_start() ->
  ?call_fun(mce_gs_start(),gs:start()).
mce_gs_start() ->
  case mce_conf:is_simulation() of
    true ->
      IdV =
	case (?MODULE):gget(gs) of
	  undefined ->
	    io:format("calling gs~n",[]),
	    Id = gs:start(),
	    io:format("gs is ~p~n",[Id]),
	    (?MODULE):gput(gs, {(?MODULE):self(), Id}),
	    Id;
	  {_, Id} ->
	    Id
	end,
      io:format("Gs record is ~p~n", [(?MODULE):gget(gs)]),
      IdV;
    false ->
      void
  end.

gs_config(Arg1,Arg2) -> 
	?call_fun(mce_gs_config(Arg1,Arg2),gs:config(Arg1,Arg2)).
mce_gs_config(Arg1,Arg2) ->
  case mce_conf:is_simulation() of
    true ->
      mce_erl:apply(gs,config,[Arg1,Arg2]);
    false ->
      ok
  end.

gs_create(Arg1,Arg2,Arg3) -> 
  ?call_fun(mce_gs_create(Arg1,Arg2,Arg3),gs:create(Arg1,Arg2,Arg3)).
mce_gs_create(Arg1,Arg2,Arg3) ->
  case mce_conf:is_simulation() of
    true ->
      mce_erl:apply(gs,create,[Arg1,Arg2,Arg3]);
    false ->
      ok
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Todo: timer references should probably be recognised as refs

send_after(Time,Dest,Message) ->
	?call_fun(mce_send_after(Time,Dest,Message),
			  erlang:send_after(Time,Dest,Message)).
mce_send_after(Time,Dest,Message) ->
  ?MODULE:spawn(mce_erl_systemCode,send_after,[Time,Dest,Message]).

start_timer(Time,Dest,Message) ->
	?call_fun(mce_start_timer(Time,Dest,Message),
			  erlang:start_timer(Time,Dest,Message)).
mce_start_timer(Time,Dest,Message) ->
  ?MODULE:send_after(Time,Dest,Message).

cancel_timer(Timer) ->
	?call_fun(mce_cancel_timer(Timer),
			  erlang:cancel_timer(Timer)).
mce_cancel_timer(Timer) ->
  try ?MODULE:send(Timer,cancel_timer), 20 catch _ -> false end.

read_timer(Timer) ->
	?call_fun(mce_read_timer(Timer),
			  erlang:read_timer(Timer)).
mce_read_timer(Timer) ->
  20.
					   
  

