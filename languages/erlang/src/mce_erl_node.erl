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

%%% Code for handling the node "net" processing

-module(mce_erl_node).
-export([doDispatchSignal/2]).

%%-define(debug,true).
-include("../../../src/include/macros.hrl").
-include("emacros.hrl").

-include("process.hrl").

%%% We have received a signal from some remote node, we will now do a local
%%% send of it to the receiving process (which is the same).
%%% We could possibly save states here by deciding to immediately execute
%%% an enabled receiving process as well.
doDispatchSignal({FromObj, ToObj, Signal, NewEther}, SavedState) ->
  ToNode =
    calculate_node(ToObj),
  mce_erl_actions:record
    (mce_erl_actions:mk_input(FromObj,FromObj,ToObj,Signal)),
  {signal, FromNode, S} = Signal,
  case mce_erl_sysOS:findNode(ToNode, SavedState) of
    {ok, {Node, RestNodes}} ->
      State = mce_erl_sysOS:setCurrentNodeContext(Node, RestNodes, SavedState),
      S1 = mce_erl_state:setEther(NewEther, State),
      S2 = handleSignal(FromObj, ToObj, ToNode, S, S1),
      mce_erl_sysOS:mkStateFromCurrentNode(S2);
    _ ->
      State = mce_erl_sysOS:setCurrentContext(SavedState),
      S1 = mce_erl_state:setEther(NewEther, State),
      S2 = handleUndeliverable(FromNode, ToNode, S, S1),
      mce_erl_sysOS:mkStateFromOtherNodes(S2)
  end.

%%% Someone has sent a signal to a node that doesn't exist.
%%% Note that here we have neither a process nor a node context.
handleUndeliverable(FromNode, ToNode, Signal, State) ->
  %%io:format("handleUndeliverable(~p,~p)~n",[ToNode,Signal]),
  case Signal of
    {monitor_node, MonitoringPid} ->
      mce_erl_sysOS:deliverSignalToNode
	(ToNode,
	 FromNode,
	 {nodedown, ToNode, MonitoringPid},
	 State);
    {link, FromPid, ToPid} ->
      %% Linking processes, remote part
      mce_erl_sysOS:deliverSignalToNode
	(ToNode,
	 FromNode,
	 {hasExited, ToPid, FromPid, noproc},
	 State);
    {monitor, MonitorItem} ->
      {monitor, MonitoringPid, _SelfNode, NodeItem, MonitorRef} = MonitorItem,
      DOWNMessage = {'DOWN', MonitorRef, process, NodeItem, noproc},
      mce_erl_sysOS:deliverSignalToNode
	(ToNode,
	 FromNode,
	 {message, MonitoringPid, DOWNMessage},
	 State);
    _ ->
      ?LOG("Signal ~p sent to non-existing node ~p~n", [Signal, ToNode]),
      State
  end.

handleSignal(FromObj, ToObj, Node, Signal, State) ->
  %%io:format("handleSignal(~p,~p)~n",[Node,Signal]),
  case Signal of
    {spawn, Recipe, Linking, Pid} ->
      %% Spawning a local pid
      {ok, {S1, NewPid}} =
	mce_erl_sysOS:createLocalProc(Node, Recipe, [], State),
      S2 =
	mce_erl_sysOS:deliverSignalToNode
	  (Node,
	   mce_erl_sysOS:node(Pid, State),
	   {message, Pid, {hasSpawned, NewPid}}, S1),
      ?LOG("Have Spawned process ~p due to ~p~n", [NewPid, Signal]),
      case Linking of
	{yes, RemotePid} -> mce_erl_sysOS:addLink(NewPid, RemotePid, S2);
	_ -> S2
      end;
    {message, Pid, Message} ->
      %% We have to deliver the message to a local pid
      case mce_erl_sysOS:localResolvePid(Pid,State) of
	{ok,RealPid} ->
	  mce_erl_actions:record
	    (mce_erl_actions:mk_deliver(FromObj,ToObj,RealPid,Message)),
	  mce_erl_sysOS:sendMsgToPid(RealPid,Message,State);
	_ ->
	  State
      end;
    {monitor_node, MonitoringPid} ->
      mce_erl_sysOS:addMonitorNode(MonitoringPid, State);
    {link, FromPid, ToPid} ->
      %% Linking processes, remote part
      %% *** Have to check if remote process is alive ***
      case mce_erl_sysOS:getProcessByPid(ToPid, State) of
	{ok, _} ->
	  mce_erl_sysOS:addLink(ToPid, FromPid, State);
	_ ->
	  mce_erl_sysOS:deliverSignalToNode
	    (Node,
	     mce_erl_sysOS:node(FromPid, State),
	     {hasExited, ToPid, FromPid, noproc}, State)
      end;
    {unlink, FromPid, ToPid} ->
      %% Unlinking processes, remote part
      mce_erl_sysOS:removeLink(FromPid, ToPid, State);
    {exit, Pid, Reason} ->
      %% An order to exit a local process
      mce_erl_sysOS:exitDeliverNow(Pid, Pid, Reason, State);
    {nodedown, DownNode, MonitoringPid} ->
      mce_erl_sysOS:unconditionalSend
	(Node,MonitoringPid, {nodedown, DownNode}, State);
    {hasExited, TerminatedPid, ReportToPid, Reason} ->
      %% A linked process has been terminated
      mce_erl_sysOS:exitDeliverNow(ReportToPid, TerminatedPid, Reason, State);
    {monitor, MonitorItem} ->
      %% Monitor a local process
      {monitor, MonitoringPid, _SelfNode, NodeItem, MonitorRef} = MonitorItem,
      MonitoredProc =
	case NodeItem of
	  {pid, _, _} ->
	    mce_erl_sysOS:getProcessByPid(NodeItem, State);
	  {RegName, _} ->
	    case mce_erl_sysOS:getPidByRegisteredName(RegName, State) of
	      {ok, RegPid} ->
		mce_erl_sysOS:getProcessByPid(RegPid, State);
	      _ ->
		no
	    end;
	  _ ->
	    io:format("got strange monitorSignal ~p~n", [Signal]),
	    throw(bad)
	end,
      case MonitoredProc of
	{ok, P} ->
	  mce_erl_sysOS:addMonitor
	    (MonitorRef, MonitoringPid, P#process.pid, NodeItem, State);
	_ ->
	  DOWNMessage = {'DOWN', MonitorRef, process, NodeItem, noproc},
	  mce_erl_sysOS:unconditionalSend
	    (Node, MonitoringPid, DOWNMessage, State)
      end;
    {register, Name, Pid, QueryPid} ->
      case mce_erl_sysOS:doRegister(Name, Pid, State) of
	{ok, S1} ->
	  send_reply(Node,QueryPid,{value,true},S1);
	_ ->
	  send_reply(Node,QueryPid,{exception,error,badarg},State)
      end;
    {unregister, Name, QueryPid} ->
      case mce_erl_sysOS:unRegister(Name, State) of
	{ok,S1} ->
	  send_reply(Node,QueryPid,{value,true},S1);
	_ ->
	  send_reply(Node,QueryPid,{exception,error,badarg},State)
      end;
    {whereis, Name, QueryPid} ->
      Result =
	case mce_erl_sysOS:getPidByRegisteredName(Name, State) of
	  {ok, RPid} -> RPid;
	  _ -> undefined
	end,
      send_reply(Node,QueryPid,{value,Result},State);
    Other ->
      io:format("*** handleSignal got unknown signal:~n  ~p~n", [Other]),
      throw({nyi, Signal})
  end.

send_reply(FromNode,Pid,Reply,State) ->
  mce_erl_sysOS:deliverSignalToNode
    (FromNode,
     mce_erl_sysOS:node(Pid, State),
     {message, Pid, {nodeReply,Reply}}, 
     State).

calculate_node(FromObject) ->
  case mcerlang:is_pid(FromObject) of
    true ->
      mcerlang:node(FromObject);
    false when is_atom(FromObject) ->
      FromObject
  end.
