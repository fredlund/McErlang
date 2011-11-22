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

-module(mce_erl_sys).
-export([doTell/4,inform/2,inform/3]).

-include("node.hrl").
-include("process.hrl").

%%-define(debug,true).
-include("../../../src/include/macros.hrl").
-include("emacros.hrl").


%%% The previously executing process has terminated due to Reason.
%%% We should inform linked and monitored processes.

inform(Reason, State) ->
  case mce_erl_state:getProcess(State) of
    none -> State;
    _ -> inform(mce_erl_sysOS:self(State), Reason, State)
  end.

%%% Pid has died due to Reason, we should inform its linked processes
%%% and monitors.
%%%
%%% When a process dies, it is not interested in receiving EXIT and DOWN
%%% messages from other nodes more, so we should probably remove them
%%% directly as well, to clean up the state (for get_new_pid etc).
%%%
inform(Pid, Reason, State) ->
  ?LOG("Got an inform(~p,~p) in state~n  ~p~n~n", [Pid, Reason, State]),
  Node = mce_erl_state:getNode(State),
  RegisterMap = Node#node.registered,
  S1 =
    mce_erl_state:setNode
      (Node#node{registered=removeProc(Pid, RegisterMap)}, State),
  S2 =
    mce_erl_sysOS:removeProcess(Pid, S1),
  mce_erl_actions:record(mce_erl_actions:mk_died(Pid,Reason)),
  TransmittedReason =
    if Reason == kill ->
	%% We don't want recursive unstoppable kills
	killed;
       true ->
	Reason
    end,
  NewState =
    lists:foldl
      (fun (PidL, S) ->
	   tellLinkTerminated(Pid, TransmittedReason, PidL, S)
       end,
       S2,
       mce_erl_sysOS:getLinkedTo(Pid, S2)),
  NewState2 =
    lists:foldl
      (fun (Monitor, S) ->
	   tellMonitorTerminated(Pid, TransmittedReason, Monitor, S)
       end,
       NewState,
       mce_erl_sysOS:getMonitoredBy(Pid, NewState)),
  %% Remove all links in current node that mentions the dead Pid
  %% (the other locally linked processes will still get the notification,
  %% and since we don't check whether a process is linked locally,
  %% there is no problem...:-)
  NewerState = mce_erl_sysOS:removeLinks(Pid, NewState2),
  %% Remove all monitors mentioning this process in other processes,
  %% and in the ether.
  remove_all_monitors_for(Pid, NewerState).

remove_all_monitors_for(Pid,State) -> 
  NewState = 
    modify_all_nodes
      (fun (N) ->
	   NewMonitors = 
	     lists:filter
	       (fun (Monitor) ->
		    {monitor,MonitorRef,MonitoringPid,
		     MonitoredPid,RegisterType} =
		      Monitor,
		    Pid=/=MonitoringPid
		end, 
		N#node.monitors),
	   %%io:format("~p terminated. monitors before~n ~p~nafter~n ~p~n",
	   %%[Pid,N#node.monitors,NewMonitors]),
	   N#node{monitors=NewMonitors}
       end, State),
  strip_monitors_in_ether(Pid,NewState).

strip_monitors_in_ether(Pid, State) ->
  FromNode =
    mce_erl_sysOS:node(Pid, State),
  NewEther =
    lists:foldr
      (fun
	 (Channel = {{FromNode1, ToNode1}, Signals}, Acc) ->
	   if FromNode =/= FromNode1 -> [Channel| Acc];
	      true ->
	       NewSignalList =
		 lists:filter
		   (fun ({signal, _, S}) ->
			case S of
			  {monitor, {_, MonitoringPid, MonitoredPid, _, _}} ->
			    Pid =/= MonitoringPid;
			  _ ->
			    true
			end
		    end,
		    Signals),
	       if NewSignalList =:= [] -> Acc;
		  true -> [{{FromNode1, ToNode1}, NewSignalList}| Acc]
	       end
	   end
       end,
       [],
       mce_erl_state:getEther(State)),
  mce_erl_state:setEther(NewEther, State).

modify_all_nodes(F, State) ->
  Node = mce_erl_state:getNode(State),
  Nodes = mce_erl_state:getOtherNodes(State),
  [NewNode| NewNodes] = lists:map(F, [Node| Nodes]),
  mce_erl_state:setOtherNodes(NewNodes, mce_erl_state:setNode(Node, State)).

removeProc(Pid,RegisterMap) ->
  lists:filter(fun ({_,Pid2}) -> Pid=/=Pid2 end, RegisterMap).

tellMonitorTerminated
(Pid, Reason,
 {monitor, MonitorRef, MonitoringPid, MonitoredPid, RegisterType}, State) ->
  %% Also remove monitorings when a down message arrives
  S = mce_erl_sysOS:deleteMonitor(MonitorRef, State),
  ?LOG("Will send a message to ~p with Msg ~n", [MonitoringPid]),
  mce_erl_sysOS:unconditionalSend
    (mcerlang:node(Pid),
     MonitoringPid, {'DOWN', MonitorRef, process, RegisterType, Reason}, S).

tellLinkTerminated(Pid, Reason, PidL, State) ->
  ?LOG("tellLinkTerminated(~p,~p,~p)~n", [Pid, Reason, PidL]),
  Node = mcerlang:node(PidL),
  case mce_erl_sysOS:can_send_locally(Node, State) of
    true ->
      doTell(PidL, Pid, Reason, State);
    false ->
      mce_erl_sysOS:deliverSignalToNode
	(Node, {hasExited, Pid, PidL, Reason},State)
  end.

%% A local process has crashed, we have to inform its linked environment.
doTell(Pid, TerminatedPid, Reason, State) ->
  ?LOG("Will tell ~p about termination of ~p because of~n~p~n",
       [Pid, TerminatedPid, Reason]),
  case mce_erl_sysOS:getProcessByPid(Pid, State) of
    {ok, P} ->
      ?LOG("Flags is ~p~nProc is ~p~n", [P#process.flags, P]),
      case (P#process.flags)#processFlags.trap_exit of
	true ->
	  ?LOG("Informing ~p; ~p terminated due to~n~p~n",
	       [Pid, TerminatedPid, Reason]),
	  mce_erl_sysOS:sendMsgToPid
	    (Pid, {'EXIT', TerminatedPid, Reason}, State);
	false ->
	  if Reason =/= normal ->
	      ?LOG("Removing ~p; ~p terminated: ~n~p~n",
		   [Pid, TerminatedPid, Reason]),
	      (?MODULE):inform(Pid, Reason, State);
	     true ->
	      State
	  end
      end;
    _ ->
      State
  end.
