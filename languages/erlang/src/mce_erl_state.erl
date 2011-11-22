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

%%% Interface functions for operating on the global state.

-module(mce_erl_state).
-export([initState/0,getState/0,setState/1,

	 getExecutable/1,setExecutable/2,
	 getEther/1,setEther/2,
	 getDict/1,setDict/2,

	 getNode/1,setNode/2,
	 getOtherNodes/1,setOtherNodes/2,
	 getProcess/1,setProcess/2,
	 getOtherProcesses/1,setOtherProcesses/2,
	 setFlags/2,

	 addProcess/2,
	 addNode/2]).

-include("state.hrl").
-include("system.hrl").
-include("executable.hrl").
-include("process.hrl").
-include("node.hrl").

%%-define(debug,true).
-include("../../../src/include/macros.hrl").
-include("emacros.hrl").


initState() ->
    setState(#system{}).

-spec(getState/0 :: () -> #system{}).
getState() ->
  State = get(globalState),
  ?LOG("~p: getState() -> ~p~n",[self(),State]),
  if
    is_record(State,system) ->
      ok;
    true ->
      io:format
	("~n****** FATAL ERROR: getState: State~n ~p ~n is not a system record; is the code really running under McErlang? ******~n",[State]),
      catch throw(bad),
      io:format
	("Stack trace:~n~s~n~n",
	 [mce_erl_debugger:printStackTrace
	  (2,erlang:get_stacktrace())]),
      exit(1)
  end,
  State.

-spec(setState/1 :: (#system{}) -> any()).
setState(State) ->
  ?LOG("setState(~p)~n",[State]),
  if
    is_record(State,system) ->
      ok;
    true ->
      io:format
	("~n****** FATAL ERROR: setState: State~n ~p ~n is not a system record; is the code really running under McErlang? ******~n",[State]),
      catch throw(bad),
      io:format
	("Stack trace:~n~s~n~n",
	 [mce_erl_debugger:printStackTrace
	  (2,erlang:get_stacktrace())]),
      exit(1)
  end,
  put(globalState,State),
  State.

getExecutable(State) ->
  State#system.executable.
setExecutable(Ex,State) ->
  State#system{executable=Ex}.

getEther(State) ->
  State#system.ether.
setEther(Ether,State) ->
  State#system{ether=Ether}.

getDict(State) ->
  State#system.dict.
setDict(Dict,State) ->
  State#system{dict=Dict}.

getNode(State) ->
  (getExecutable(State))#executable.node.
setNode(Node,State) ->
  Ex = getExecutable(State),
  setExecutable(Ex#executable{node=Node},State).

getOtherNodes(State) ->
  (getExecutable(State))#executable.otherNodes.
setOtherNodes(OtherNodes,State) ->
  Ex = getExecutable(State),
  setExecutable(Ex#executable{otherNodes=OtherNodes},State).

getProcess(State) ->
  Ex = getExecutable(State),
  Ex#executable.process.
setProcess(Process,State) ->
  Ex = getExecutable(State),
  setExecutable(Ex#executable{process=Process},State).

getOtherProcesses(State) ->
  Node = getNode(State),
  Node#node.processes.
setOtherProcesses(OtherProcesses,State) ->
  Node = getNode(State),
  setNode(Node#node{processes=OtherProcesses},State).
  
addNode(N,State) ->
  setOtherNodes([N|getOtherNodes(State)],State).
addProcess(P,State) ->
  setOtherProcesses([P|getOtherProcesses(State)],State).
setFlags(Flags,State) ->
  P = getProcess(State),
  setProcess(P#process{flags=Flags},State).

