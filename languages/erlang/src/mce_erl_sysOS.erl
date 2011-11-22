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

%%% Runtime system (handling process coordination) among processes
%%% 
%%% None of these functions are callable from a user-context, i.e.,
%%% they all accept a state parameter as argument, and potentially return an 
%%% updated state.

-module(mce_erl_sysOS).
-export([unconditionalSend/4,localResolvePid/2,createLocalProc/4,
	 deliverSignalToNode/3, deliverSignalToNode/4,
	 ensureNodeAlive/2, self/1,
	 exitDeliverNow/4, 
	 doRegister/3, unRegister/2, 
	 removeProcess/2, 
	 addMonitor/5,
	 addMonitorNode/2,
	 deleteMonitor/2, deleteMonitorNode/3,
	 getMonitoredBy/2,getLinkedTo/2,
	 getPidByRegisteredName/2,
	 getProcessByPid/2,
	 findNode/2,
	 
	 resolvePid/2,
	 
	 dget/2,
	 get/1,get/2,put/3,erase/2,
	 nget/1,nget/2,nput/3,nerase/2,
	 gget/1,gget/2,gput/3,gerase/2,
	 dget/3,dput/4,derase/3,
	 
	 bringDownNode/2,
	 
	 addLink/3,
	 removeLink/3,
	 removeLinks/2,
	 node/1,node/2,
	 is_local_node/2,
	 can_send_locally/2,
	 setCurrentRunContext/2,
	 setCurrentNodeContext/3,
	 setCurrentContext/1,
	 deleteDownsInNode/2, deleteDownsInQueue/2,
	 deleteNodeDownsInNode/3, deleteNodeDownsInQueue/3,
	 mkStateFromOtherNodes/1,
	 mkStateFromCurrentNode/1,
	 mkStateFromCurrentExecutableWithProcess/2,
	 mkStateFromCurrentExecutable/1,
	 sendMsgToPid/3,
	 addMsgToState/3]).

-export([getSignals/1]).

-include("system.hrl").
-include("process.hrl").
-include("node.hrl").
-include("executable.hrl").
-include("state.hrl").

%%-define(debug,true).
-include("../../../src/include/macros.hrl").
-include("emacros.hrl").

%%% Callable from a node context (without a process context) if Args is empty
createLocalProc(Node, Fun, Args, State) ->
  P = mce_erl_process:makeRunnableInSystem(Fun, Node, State),
  S1 = mce_erl_state:addProcess(P, State),
  Pid = P#process.pid,
  ?LOG("spawn: spawned new process ~p with pid ~p~n", [Fun, P#process.pid]),
  NextState =
    case lists:member(link, Args) of
      true ->
	S2 = addLink(Pid, self(S1), S1),
	addLink(self(S1), Pid, S2);
      false ->
	S1
    end,
  case lists:member(monitor, Args) of
    true ->
      MonitorRef =
	mce_erl_references:mkNewMonitorRef({self(S1), Node, Pid}, NextState),
      MonitorItem =
	{monitor, self(S1), Node, Pid, MonitorRef},
      FinalState =
	(?MODULE):addMonitor(MonitorRef, self(S1), Pid, Pid, NextState),
      {ok, {FinalState, MonitorRef, Pid}};
    false ->
      {ok, {NextState, Pid}}
  end.

ensureNodeAlive(Node, State) ->
  case getNodeById(Node, State) of
    {ok, N} -> State;
    _ ->
      mce_erl_actions:record
	(mce_erl_actions:mk_api_call
	 (mce_erl_sysOS:self(State),createNode,[Node],ok)),
      case mce_conf:wants_rpc() of
	true ->
	  mce_erl_state:addNode
	    (#node{name=Node,
		   processes =
		   [mce_erl_process:makeRunnable
		    ({mce_erl_rpc, start, []}, Node)]},
	     State);
	_ ->
	  mce_erl_state:addNode(#node{name=Node, processes=[]}, State)
      end
  end.

self(State) ->
  (mce_erl_state:getProcess(State))#process.pid.

resolvePid(Pid,State) ->
  case Pid of
    {pid,_,_} ->
      {ok, Pid};
    {Name,Node} ->
      case is_local_node(Node,State) of
	true -> resolvePid(Name,State);
	false -> {ok, Pid}
      end;
    Other ->
      case getPidByRegisteredName(Pid,State) of
	{ok,RPid} -> {ok, RPid};
	BadPid -> no
      end
  end.

localResolvePid(Pid,State) ->
  case Pid of
    {pid,_,_} -> {ok,Pid};
    {Name,Node} -> getPidByRegisteredName(Name,State);
    Name when is_atom(Name)-> getPidByRegisteredName(Name,State);
    _ -> no
  end.

deliverSignalToNode(Node, Signal, State) ->
  deliverSignalToNode((?MODULE):node(State), Node, Signal, State).
%%% Safe to call without a node or process context
deliverSignalToNode(FromObject,ToNode,Signal,State) ->
  FromNode = calculate_node(FromObject),
  Sig = {signal,FromNode,Signal},
  ?LOG("~p =~p=> ~p~n",[FromObject,Sig,ToNode]),
  sendSignalToNode(FromObject,ToNode,Sig,State).

unconditionalSend(FromObject,Pid,Msg,State) ->
  case resolvePid(Pid,State) of
    {ok, ResolvedPid} ->
      {Local_Pid,ToNode} =
	case ResolvedPid of
	  {pid,PidNode,_} -> {can_send_locally(PidNode,State),PidNode};
	  {Name,Node} -> {can_send_locally(Node,State),Node}
	end,
      if Local_Pid ->
	  ?LOG("~p!~p",[ResolvedPid,Msg]),
	  sendMsgToPid(ResolvedPid,Msg,State);
	 true ->
	  ?LOG("~p!{message,~p}",[ResolvedPid,Msg]),
	  deliverSignalToNode(FromObject,ToNode,{message,ResolvedPid,Msg},State)
      end;
    no -> State 
  end.

exitDeliverNow(Pid, TerminatedPid, Reason, State) ->
  ?LOG("exitDeliverNow(~p,~p,~p,~n~p)~n", [Pid, TerminatedPid, Reason, State]),
  case getProcessByPid(Pid, State) of
    {ok, P} ->
      TrappingExits =
	(P#process.flags)#processFlags.trap_exit,
      Deliver =
	TrappingExits and not (Reason =:= kill),
      Kill =
	(Reason =/= normal) and not Deliver,
      ?LOG("In exit/2(~p,~p), Trapping_exits=~p, Deliver=~p, Kill=~p~n",
	   [Pid, Reason, TrappingExits, Deliver, Kill]),
      if Deliver ->
	  sendMsgToPid(Pid, {'EXIT', TerminatedPid, Reason}, State);
	 Kill ->
	  mce_erl_sys:inform(Pid, Reason, (?MODULE):removeProcess(Pid, State));
	 true -> State
      end;
    _ -> State
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

node(State) -> (mce_erl_state:getNode(State))#node.name.
node({pid,Node,_},State) -> Node.

can_send_locally(Node,State) ->
  case mce_conf:distributed_semantics() of
    true -> false;
    false -> is_local_node(Node,State)
  end.

is_local_node(Node, State) ->
  Node =:= (?MODULE):node(State).

insertSignal(FromObject,ToObject,Signal,Signals) ->
  Key = {FromObject,ToObject},
  orddict:update
    (Key, fun(OldSignals) -> OldSignals++[Signal] end, [Signal], Signals).

sendMsgToPid(Pid, Msg, State) ->
  Process = mce_erl_state:getProcess(State),
  if is_record(Process, process), Process#process.pid =:= Pid ->
      mce_erl_state:setProcess(addMsgToProcess(Process, Msg), State);
     true ->
      mce_erl_state:setOtherProcesses(addMsgToProcess(Pid, Msg, mce_erl_state:getOtherProcesses(State)), State)
  end.

addMsgToProcess(Process, Msg) ->
  P = Process#process{queue=Process#process.queue ++ [Msg]},
  ?LOG("Going to add message ~p to process~n  ~p~n, new process is~n ~p~n",
       [Msg, Process, mce_erl_opsem:updProcStatusFromQueue(P)]),
  mce_erl_opsem:updProcStatusFromQueue(P).

addMsgToProcess(_Pid,_Msg,[]) -> [];
addMsgToProcess(Pid,Msg,[P|Rest]) ->
  case P#process.pid=:=Pid of
    true ->
      [addMsgToProcess(P,Msg)|Rest]; 
    false ->
      [P|addMsgToProcess(Pid,Msg,Rest)]
  end.

addMsgToProcessWithNodes(_Pid, _Msg, []) -> [];
addMsgToProcessWithNodes(Pid, Msg, [Node| Rest]) ->
  case (?MODULE):node(Pid, void) =:= Node#node.name of
    true ->
      NewNode =
	Node#node{processes =
		  addMsgToProcess(Pid, Msg, Node#node.processes)},
      [NewNode| Rest];
    false ->
      [Node| addMsgToProcessWithNodes(Pid, Msg, Rest)]
  end.
addMsgToState(Pid,Msg,State) ->
  State#state{nodes=addMsgToProcessWithNodes(Pid,Msg,State#state.nodes)}.

getProcessByPid(Pid, State) ->
  OtherProcesses =
    mce_erl_state:getOtherProcesses(State),
  Processes =
    case mce_erl_state:getProcess(State) of
      none ->
	OtherProcesses;
      P ->
	[P| OtherProcesses]
    end,
  mce_utils:find(fun (P) -> Pid =:= P#process.pid end, Processes).

getPidByRegisteredName(Name, State) ->
  Registered = (mce_erl_state:getNode(State))#node.registered,
  case mce_utils:find(fun ({HdName, _}) -> Name =:= HdName end, Registered)
    of
    {ok, {_, Pid}} -> {ok, Pid};
    Other -> Other
  end.

sendSignalToNode(FromObject, ToNode, Signal, State) ->
  case mce_conf:distributed_semantics() of
    true ->
      ToObject =
	compute_comm_endpoint(ToNode,Signal),
      NewEther =
	insertSignal
	  (FromObject, ToObject, Signal, mce_erl_state:getEther(State)),
      mce_erl_actions:record
	(mce_erl_actions:mk_output
	 (FromObject,FromObject,ToObject,Signal)),
      mce_erl_state:setEther(NewEther, State);
    false ->
      FromNode =
	calculate_node(FromObject),
      NewEther =
	insertSignal(FromNode, ToNode, Signal, mce_erl_state:getEther(State)),
      mce_erl_actions:record
	(mce_erl_actions:mk_output
	 (FromNode,FromNode,ToNode,Signal)),
      mce_erl_state:setEther(NewEther, State)
  end.

calculate_node(FromObject) ->
  case mcerlang:is_pid(FromObject) of
    true ->
      mcerlang:node(FromObject);
    false when is_atom(FromObject) ->
      FromObject
  end.

compute_comm_endpoint(ToNode,Signal) ->
  case Signal of 
    {message, Pid, _Message} ->
      case Pid of
	{pid,_,_} -> Pid;
	{_,PidNode} -> PidNode
      end;
%%    {spawn, _Recipe, _Linking, _Pid} ->
%%      ToNode;
%%    {monitor_node, MonitoringPid} ->
%%      ToNode;
%%    {link, FromPid, ToPid} ->
%%      ToPid;
%%    {unlink, FromPid, ToPid} ->
%%      ToPid;
%%    {exit, Pid, Reason} ->
%%      Pid;
%%    {nodedown, _DownNode, MonitoringPid} ->
%%      MonitoringPid;
%%    {hasExited, _TerminatedPid, ReportToPid, _Reason} ->
%%      ReportToPid;
%%    {monitor, MonitorItem} ->
%%      {monitor, _MonitoringPid, _SelfNode, NodeItem, _MonitorRef} = MonitorItem,
%%      case NodeItem of
%%	{pid, _, _} ->
%%	  NodeItem;
%%	{RegName,Node} ->
%%	  Node
%%      end;
    _ ->
      ToNode
  end.

doRegister(Name, Pid, State) ->
  Node = mce_erl_state:getNode(State),
  RegisterMap = Node#node.registered,
  case searchAndRegister(Name, Pid, RegisterMap) of
    {ok, NewRegisterMap} ->
      {ok, mce_erl_state:setNode(Node#node{registered=NewRegisterMap}, State)};
    Other ->
      ?LOG("doRegister(~p,~p) fails with ~p in state~n  ~p~n",
	   [Name, Pid, Other, State]),
      Other
  end.

unRegister(Name,State) ->
  Node = mce_erl_state:getNode(State),
  RegisterMap =
    Node#node.registered,
  case unRegister_1(Name,RegisterMap,[]) of
    {true,NewRegisterMap} ->
      {ok,mce_erl_state:setNode(Node#node{registered=NewRegisterMap},State)};
    _ ->
      no
  end.

unRegister_1(_Name,[],L) ->
  {false,lists:reverse(L)};
unRegister_1(Name,[{RName,RPid}|Rest],L) ->
  if
    Name =:= RName ->
      {true,lists:reverse(L,Rest)};
    true ->
      unRegister_1(Name,Rest,[{RName,RPid}|L])
  end.

searchAndRegister(Name,Pid,[]) -> {ok, [{Name,Pid}]};
searchAndRegister(Name,Pid,[{HdName,HdPid}|Rest]) ->
  if
    Name=:=HdName -> 
      io:format
	("Name clash for ~p;~nold pid ~p;~nnew pid ~p~n",
	 [Name,HdPid,Pid]),
      name_clash;
    Pid=:=HdPid -> 	
      io:format("Pid clash: ~p=~p~n",[Pid,HdPid]),
      pid_clash;
    true ->
      case searchAndRegister(Name,Pid,Rest) of
	{Result, ResultPidMap} -> {Result, [{HdName,HdPid}|ResultPidMap]};
	Other -> Other
      end
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% For every link there are two records stored.
%% In the node datastructure there is a link field that
%% contains (sorted on the first element) tuples of a pid 
%% and all processes that pid is linked to.

addLink(Pid1, Pid2, State) ->
  Node = mce_erl_state:getNode(State),
  Links = Node#node.links,
  NewLinks = addLink1(Pid1, Pid2, Links),
  mce_erl_state:setNode(Node#node{links=NewLinks}, State).

addLink1(Pid1,Pid2,[]) -> [{Pid1,[Pid2]}];
addLink1(Pid1,Pid2,[{Pid1,Links}|Rest]) ->
  [{Pid1,lists:umerge([Pid2],Links)}|Rest];
addLink1(Pid1,Pid2,[{Pid3,L}|Rest]) ->
  if
    Pid1<Pid3 -> [{Pid1,[Pid2]},{Pid3,L}|Rest];
    true -> [{Pid3,L}|addLink1(Pid1,Pid2,Rest)]
  end.

removeLink(Pid1, Pid2, State) ->
  Node = mce_erl_state:getNode(State),
  Links = Node#node.links,
  NewLinks = removeLink1(Pid1, Pid2, Links),
  mce_erl_state:setNode(Node#node{links=NewLinks}, State).

removeLink1(Pid1,Pid2,[]) -> [];
removeLink1(Pid1,Pid2,L=[{PidA,Links}|Rest]) ->
  if Pid1=:=PidA -> 
      NewLinks = lists:delete(Pid2,Links),
      case NewLinks of
	[] -> Rest;
	_ -> [{PidA,NewLinks}|Rest]
      end;
     Pid1<PidA -> L;
     true -> [{PidA,Links}|removeLink1(Pid1,Pid2,Rest)]
  end.

%% Remove all the local (in this node) links that mention Pid
removeLinks(Pid, State) ->
  Node =
    mce_erl_state:getNode(State),
  Links =
    Node#node.links,
  NewLinks =
    lists:filter(fun ({Pid1, Links}) -> Pid1 =/= Pid end, Links),
  NewerLinks =
    removeLinkRef(Pid, NewLinks),
  mce_erl_state:setNode(Node#node{links=NewerLinks}, State).

removeLinkRef(Pid,[]) -> [];
removeLinkRef(Pid,[{PidA,Links}|Rest]) ->
  NewLinks = lists:delete(Pid,Links),
  case NewLinks of
    [] -> removeLinkRef(Pid,Rest);
    _ -> [{PidA,NewLinks}|removeLinkRef(Pid,Rest)]
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Remove a process in the current node
removeProcess(Pid, State) ->
  case is_current_process(Pid, State) of
    true ->
      voidProcess(State);
    false ->
      NewOtherProcesses =
	lists:filter(fun (P) -> P#process.pid =/= Pid end,
		     mce_erl_state:getOtherProcesses(State)),
      mce_erl_state:setOtherProcesses(NewOtherProcesses, State)
  end.

is_current_process(Pid, State) ->
  case mce_erl_state:getProcess(State) of
    none ->
      false;
    P ->
      P#process.pid =:= Pid
  end.

addMonitor(MonitorRef, MonitoringPid, MonitoredPid, RegisterType, State) ->
  Node = mce_erl_state:getNode(State),
  NewMonitors =
    mce_utils:slist_insert
      ({monitor, MonitorRef, MonitoringPid, MonitoredPid, RegisterType},
       Node#node.monitors),
  mce_erl_state:setNode(Node#node{monitors=NewMonitors}, State).

addMonitorNode(MonitoringPid, State) ->
  Node = mce_erl_state:getNode(State),
  NewMonitors =
    mce_utils:slist_insert(MonitoringPid, Node#node.node_monitors),
  mce_erl_state:setNode(Node#node{node_monitors=NewMonitors}, State).

deleteMonitor(MonitorRef, State) ->
  Node = mce_erl_state:getNode(State),
  case deleteMonitor(MonitorRef, Node#node.monitors, []) of
    {true, NewMonitors} ->
      mce_erl_state:setNode(Node#node{monitors=NewMonitors}, State);
    _ ->
      mce_erl_state:setOtherNodes
	(lists:map
	 (fun (OtherNode) ->
	      case
		deleteMonitor(MonitorRef, Node#node.monitors, [])
		of
		{true, NewMonitors} ->
		  OtherNode#node{monitors=NewMonitors};
		_ ->
		  OtherNode
	      end
	  end,
	  mce_erl_state:getOtherNodes(State)),
	 State)
  end.

deleteMonitor(MonitorRef,[],Others) -> {false, []};
deleteMonitor(MonitorRef,[{monitor,MonitorRef,_,_,_}|Rest],Others) ->
  NewMonitors = lists:reverse(Others)++Rest,
  {true,NewMonitors};
deleteMonitor(MonitorRef,[Nope|Rest],Others) ->
  deleteMonitor(MonitorRef,Rest,[Nope|Others]).

deleteMonitorNode(NodeName, Self, State) ->
  ThisNode = mce_erl_state:getNode(State),
  if NodeName =:= ThisNode#node.name ->
      mce_erl_state:setNode
	(ThisNode#node
	 {node_monitors =
	  deleteMonitor(Self, ThisNode#node.node_monitors, [])}, State);
     true ->
      mce_erl_state:setOtherNodes
	(lists:map
	 (fun (OtherNode) ->
	      if
		NodeName =:= OtherNode#node.name ->
		  OtherNode#node
		    {node_monitors=
		     deleteNodeMonitor(Self, OtherNode#node.node_monitors)};
		true ->
		  OtherNode
	      end
	  end,
	  mce_erl_state:getOtherNodes(State)),
	 State)
  end.

deleteNodeMonitor(Self,[]) -> [];
deleteNodeMonitor(Self,[Self|Rest]) ->
  deleteNodeMonitor(Self,Rest);
deleteNodeMonitor(Self,[Other|Rest]) ->
  [Other|deleteNodeMonitor(Self,Rest)].

getLinkedTo(Pid, State) ->
  Node = mce_erl_state:getNode(State),
  case mce_utils:find(fun ({Pid1, L}) ->
			  if Pid =:= Pid1 -> true;
			     Pid < Pid1 -> never;
			     true -> false
			  end
		      end, Node#node.links)
    of
    {ok, {_, L}} -> L;
    _ -> []
  end.

getMonitoredBy(Pid, State) ->
  Node = mce_erl_state:getNode(State),
  lists:filter
    (fun
       ({monitor, _, _, MonitoredPid, RegisterType}) -> MonitoredPid =:= Pid
     end,
     Node#node.monitors).

getNodeById(Nid, State) ->
  AllNodes = [mce_erl_state:getNode(State)| mce_erl_state:getOtherNodes(State)],
  mce_utils:find(fun (N) -> Nid =:= N#node.name end, AllNodes).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Install a new runnable context
setCurrentRunContext(Exec,State)
  when is_record(State,state), is_record(Exec,executable) ->
  #system{dict=State#state.dict,
	  ether=State#state.ether,
	  executable=Exec}.

%%% Install a new runnable context
setCurrentContext(State)
  when is_record(State,state) ->
  Exec = #executable{otherNodes=State#state.nodes},
  #system{dict=State#state.dict,
	  ether=State#state.ether,
	  executable=Exec}.

%%% Setup for executing code in the context of a node (but not process)
setCurrentNodeContext(Node,RestNodes,State) 
  when is_record(State,state) ->
  Exec = #executable{node=Node,otherNodes=RestNodes,process=none},
  #system{dict=State#state.dict,
	  ether=State#state.ether,
	  executable=Exec}.

%%% Save state after executing in node context
mkStateFromCurrentNode(State) ->
  Node = mce_erl_state:getNode(State),
  NewNode = Node#node{processes=mce_erl_state:getOtherProcesses(State)},
  Nodes = [NewNode| mce_erl_state:getOtherNodes(State)],
  #state{dict=mce_erl_state:getDict(State),
	 nodes=Nodes,
	 ether=mce_erl_state:getEther(State)}.

%%% Save state after executing without a node context
mkStateFromOtherNodes(State) ->
  #state{dict=mce_erl_state:getDict(State),
	 nodes=mce_erl_state:getOtherNodes(State),
	 ether=mce_erl_state:getEther(State)}.

%%% Save state after executing in process context
mkStateFromCurrentExecutableWithProcess(Process, State) ->
  Processes = [Process| mce_erl_state:getOtherProcesses(State)],
  Node = mce_erl_state:getNode(State),
  NewNode = Node#node{processes=Processes},
  Nodes = [NewNode| mce_erl_state:getOtherNodes(State)],
  #state{dict=mce_erl_state:getDict(State),
	 nodes=Nodes,
	 ether=mce_erl_state:getEther(State)}.

%%% Save state after executing in process context, where the current process
%%% has died
mkStateFromCurrentExecutable(State) ->
  Processes = mce_erl_state:getOtherProcesses(State),
  Node = mce_erl_state:getNode(State),
  NewNode = Node#node{processes=Processes},
  Nodes = [NewNode| mce_erl_state:getOtherNodes(State)],
  #state{dict=mce_erl_state:getDict(State),
	 nodes=Nodes,
	 ether=mce_erl_state:getEther(State)}.

findNode(Node,State) ->
  findNode(Node,State#state.nodes,[]).

findNode(NodeId,[],Seen) -> no;
findNode(NodeId,[N|RestN],Seen) ->
  if NodeId=:=N#node.name -> {ok, {N,Seen++RestN}};
     true -> findNode(NodeId,RestN,Seen++[N])
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Given an executable context, switch the current process to the
%%% the process identified by Pid
switchContextOnPid(Pid, ExecState)
  when is_record(ExecState, system) ->
  switchPidInNode
    (Pid, switchContextOnNode((?MODULE):node(Pid, ExecState), ExecState)).

%%% Given an executable context, switch the current node to the argument node
switchContextOnNode(NodeId, ExecState)
  when is_record(ExecState, system) ->
  N = mce_erl_state:getNode(ExecState),
  if is_record(N, node), NodeId =:= N#node.name ->
      ExecState;
     true ->
      case
	filter_node(NodeId, mce_erl_state:getOtherNodes(ExecState))
	of
	{ok, {Node, OtherNodes}} ->
	  NewOtherNodes =
	    if N =:= none ->
		OtherNodes;
	       true ->
		Processes =
		  case mce_erl_state:getProcess(ExecState) of
		    none -> mce_erl_state:getOtherProcesses(ExecState);
		    P -> [P| mce_erl_state:getOtherProcesses(ExecState)]
		  end,
		[N#node{processes=Processes}| OtherNodes]
	    end,
	  assignNodes(Node, NewOtherNodes, ExecState);
	_ ->
	  ExecState
      end
  end.

%%% Set node as current entity
assignNodes(Node,OtherNodes,ExecState)
  when is_record(ExecState,system) ->
  voidProcess
    (ExecState#system{executable=#executable{node=Node,otherNodes=OtherNodes}}).

%%% Given an executable context, switch running process to the one with Pid
%%% (required to reside in the same node as the current context)
switchPidInNode(Pid, ExecState) when is_record(ExecState, system) ->
  case filter_pid(Pid, processes(ExecState)) of
    {ok, {P, OtherProcesses}} ->
      mce_erl_state:setProcess
	(P, mce_erl_state:setOtherProcesses(OtherProcesses, ExecState));
    _ ->
      ExecState
  end.

processes(ExecState) ->
  P = mce_erl_state:getProcess(ExecState),
  if is_record(P, process) ->
      [P| mce_erl_state:getOtherProcesses(ExecState)];
     true ->
      mce_erl_state:getOtherProcesses(ExecState)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

deleteDownsInNode(MonitorRef,State) ->
  NewEther =
    lists:foldr
      (fun ({{FromNode,ToNode},Signals},Ether) ->
	   NewSignals = 
	     lists:filter
	       (fun ({signal, _, Signal}) ->
		    case Signal of
		      {'DOWN',MonitorRef,_,_,_} -> false;
		      _ -> true
		    end
		end,
		Signals),
	   if NewSignals=:=[] -> Ether;
	      true -> [{{FromNode,ToNode},NewSignals}|Ether]
	   end
       end,
       [], getSignals(State)),
  setSignals(NewEther,State).

deleteDownsInQueue(MonitorRef, State) ->
  P = mce_erl_state:getProcess(State),
  NewQueue =
    lists:filter(fun (Signal) ->
		     case Signal of
		       {'DOWN', MonitorRef, _, _, _} -> false;
		       _ -> true
		     end
		 end,
		 P#process.queue),
  mce_erl_state:setProcess(P#process{queue=NewQueue}, State).

deleteNodeDownsInNode(Node,Self,State) ->
  NewEther =
    lists:foldr
      (fun ({{FromNode,ToNode},Signals},Ether) ->
	   NewSignals = 
	     lists:filter
	       (fun ({signal, _, Signal}) ->
		    case Signal of
		      {nodedown,Node} -> false;
		      _ -> true
		    end
		end,
		Signals),
	   if NewSignals=:=[] -> Ether;
	      true -> [{{FromNode,ToNode},NewSignals}|Ether]
	   end
       end,
       [], getSignals(State)),
  setSignals(NewEther,State).

deleteNodeDownsInQueue(Node, Self, State) ->
  P = mce_erl_state:getProcess(State),
  NewQueue =
    lists:filter(fun (Signal) ->
		     case Signal of
		       {nodedown, Node, Self} -> false;
		       _ -> true
		     end
		 end,
		 P#process.queue),
  mce_erl_state:setProcess(P#process{queue=NewQueue}, State).

%% Bring down some node
%%
bringDownNode(NodeName, State) ->
  N = mce_erl_state:getNode(State),
  if %% Current node shall be killed
    N#node.name =:= NodeName ->
      NewState = killNode(none, N, mce_erl_state:getOtherNodes(State), State);
    %% Not current node shall be killed
    true ->
      case filter_node(NodeName, mce_erl_state:getOtherNodes(State)) of
	not_found ->
	  State;
	{ok, {Node, OtherNodes}} ->
	  killNode
	    ({pid, mce_erl_sysOS:self(State)}, Node, [N| OtherNodes], State)
      end
  end.

killNode(RestoreSpec, Node, OtherNodes, State) ->
  NodeName = Node#node.name,
  NodeState = switchContextOnNode(NodeName, State),
  ProcessExitState =
    lists:foldl(fun (P, FoldState) ->
		    S = switchContextOnPid(P#process.pid, FoldState),
		    mce_erl_sys:inform(P#process.pid, nodefailure, S)
		end,
		NodeState,
		Node#node.processes),
  NodeMonitors =
    Node#node.node_monitors,
  NodeExitState =
    lists:foldl
      (fun (Pid, FoldState) ->
	   (?MODULE):deliverSignalToNode
	     ((?MODULE):node(Pid, State),
	      {nodedown, NodeName, Pid},
	      FoldState)
       end,
       ProcessExitState,
       NodeMonitors),
  ExitState = voidProcess(voidNode(NodeExitState)),
  case RestoreSpec of
    none ->
      ExitState;
    {pid, Pid} ->
      switchContextOnPid(Pid, ExitState)
  end.

filter_node(Name,Nodes) ->
  filter_node(Name,Nodes,[]).
filter_node(_,[],_) -> not_found;
filter_node(Name,[N|Rest],Saved) -> 
  if Name =:= N#node.name -> {ok,{N,lists:reverse(Saved)++Rest}};
     true -> filter_node(Name,Rest,[N|Saved])
  end.

filter_pid(Name,Processes) ->
  filter_pid(Name,Processes,[]).
filter_pid(_,[],_) -> not_found;
filter_pid(Name,[P|Rest],Saved) -> 
  if Name =:= P#process.pid -> {ok,{P,lists:reverse(Saved)++Rest}};
     true -> filter_pid(Name,Rest,[P|Saved])
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

voidNode(State) ->
  mce_erl_state:setNode(none, State).

voidProcess(State) ->
  mce_erl_state:setProcess(none, State).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Dictionary functions
%%% 

dget(process, State) ->
  P = mce_erl_state:getProcess(State),
  P#process.dict;
dget(node, State) ->
  N = mce_erl_state:getNode(State),
  N#node.dict;
dget(global, State) ->
  mce_erl_state:getDict(State).

dget(DictType, Key, State) ->
  KeyValues = (?MODULE):dget(DictType, State),
  case mce_utils:find(fun ({K, V}) -> Key =:= K end, KeyValues)
    of
    {ok, {_, Value}} ->
      Value;
    _ ->
      undefined
  end.

get(State) ->
  dget(process,State).

nget(State) ->
  dget(node,State).

gget(State) ->
  dget(global,State).

get(Key,State) ->
  dget(process,Key,State).

nget(Key,State) ->
  dget(node,Key,State).

gget(Key,State) ->
  dget(global,Key,State).

dput(DictType, Key, Value, State) ->
  Dict = (?MODULE):dget(DictType, State),
  {OldValue, NewDict} = put(Dict, Key, Value, []).

put(Key,Value,State) ->
  dput(process,Key,Value,State).

nput(Key,Value,State) ->
  dput(node,Key,Value,State).

gput(Key,Value,State) ->
  dput(global,Key,Value,State).

put([], Key, Value, Seen) ->
  {undefined, lists:reverse(Seen) ++ [{Key, Value}]};
put([{K, V}| Rest], Key, Value, Seen) ->
  if Key =:= K ->
      {V, lists:reverse(Seen) ++ [{Key, Value}| Rest]};
     Key < K ->
      {undefined, lists:reverse(Seen) ++ [{Key, Value}, {K, V}| Rest]};
     true ->
      put(Rest, Key, Value, [{K, V}| Seen])
  end.

derase(DictType, Key, State) ->
  erase(Key, (?MODULE):dget(DictType, State), []).

erase(Key,State) ->
  derase(process,Key,State).

nerase(Key,State) ->
  derase(node,Key,State).

gerase(Key,State) ->
  derase(global,Key,State).

erase(Key, [], Seen) ->
  {undefined, lists:reverse(Seen)};
erase(Key, [{K, V}| Rest], Seen) ->
  if Key =:= K ->
      {V, lists:reverse(Seen) ++ Rest};
     true ->
      erase(Key, Rest, [{K, V}| Seen])
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getSignals(State) ->
  orddict:to_list(mce_erl_state:getEther(State)).

setSignals(Signals, State) ->
  mce_erl_state:setEther(Signals, State).
