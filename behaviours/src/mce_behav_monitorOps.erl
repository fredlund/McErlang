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

-module(mce_behav_monitorOps).

-include("monitor.hrl").
-include("monState.hrl").

-export([init/2,stateChange/3,monitorType/1]).
-export([state/1,stateType/1,stateType/2]).
-export([info/2]).

init(MonitorModule,MonitorArgs) ->
  {ok,Monitor} = MonitorModule:init(MonitorArgs),
  {ok,#monitor{module=MonitorModule,contents=Monitor}}.

stateChange(State,Monitor,Stack) ->
  MonResult =
    try
      (Monitor#monitor.module):stateChange
      (State,Monitor#monitor.contents,Stack)
    catch Exception:Reason ->
	mce_result:throw_result_exc
	  (mce_result:mk_mon_error
	   (mce_result:mk_exception_error
	    (Exception,Reason,erlang:get_stacktrace()),
	    #monState{state=State,monitor=Monitor},
	    Stack))
    end,
  case MonResult of
    {ok, NewMonitor} ->
      {ok,Monitor#monitor{contents=NewMonitor}};
    States when is_list(States) ->
      lists:map
	(fun (Result) ->
	     case Result of
	       {_State,_Arg} ->
		 Monitor#monitor{contents=Result};
	       Other ->
		 io:format("Bad monitor return value ~p~n",[Other]),
		 exit(badmonitor)
	     end
	 end,
	 States);
    Other ->
      Other
  end.

monitorType(Monitor) ->
  (Monitor#monitor.module):monitorType().

info(Monitor,Arg) ->
  try (Monitor#monitor.module):info(Monitor#monitor.contents,Arg)
  catch _:_ -> error end.

%% For buechi automatons only
state(Monitor) ->
  case Monitor#monitor.contents of
    {State,_} ->
      State
  end.

stateType(Monitor) ->
  stateType(state(Monitor),Monitor).
  
stateType(State,Monitor) ->
  (Monitor#monitor.module):stateType(State).

    
  
