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

-module(mce_mon_sane).
-export([init/1,stateChange/3,monitorType/0]).

-include("state.hrl").
-include("process.hrl").
-include("node.hrl").

-behaviour(mce_behav_monitor).

-record(saneState,
	{maxEther=15,
	 maxNodes=5,
	 maxProcessesPerNode=8,
	 maxRegisteredPerNode=8,
	 maxMonitors=10,
	 maxNodeMonitors=3,
	 maxLinks=10,
	 maxQueue=10}).

monitorType() -> safety.

init(default) -> init(#saneState{});
init(Conf) when is_record(Conf,saneState) -> {ok,Conf}.

stateChange(State, Conf, _) ->
    try
      checkSane(State, Conf),
      {ok, Conf}
    catch
      Reason ->
	  io:format("Monitor error ~p~n", [Reason]),
	  {exceeds, Reason}
    end.

checkSane(State,Conf) ->
  checkState(State,Conf),
  lists:foreach(fun (Node) -> checkNode(Node,Conf) end,
		State#state.nodes),
  lists:foreach(fun (Process) -> checkProcess(Process,Conf) end,
		allProcesses(State)).

checkState(State,Conf) ->
  checkListMeasure(ether,Conf#saneState.maxEther,State#state.ether,void),
  checkListMeasure(nodes,Conf#saneState.maxNodes,State#state.nodes,void).

checkNode(Node,Conf) ->
  checkListMeasure(processes,Conf#saneState.maxProcessesPerNode,
		   Node#node.processes,Node#node.name),
  checkListMeasure(registered,Conf#saneState.maxRegisteredPerNode,
		   Node#node.registered,Node#node.name),
  checkListMeasure(monitors,Conf#saneState.maxMonitors,
		   Node#node.monitors,Node#node.name),
  checkListMeasure(node_monitors,Conf#saneState.maxNodeMonitors,
		   Node#node.node_monitors,Node#node.name),
  checkListMeasure(node_links,Conf#saneState.maxLinks,
		   Node#node.links,Node#node.name).

checkProcess(Process,Conf) ->
  checkListMeasure(queue,Conf#saneState.maxQueue,
		   Process#process.queue,Process).

allProcesses(State) ->
  lists:flatmap(fun (Node) -> Node#node.processes end, State#state.nodes).
  
checkListMeasure(Name,Limit,List,Extras) ->
  ListLength = length(List),
  case ListLength>Limit of
    true ->
      throw({Name,Limit,ListLength,Extras});
    false ->
      ok
  end.
