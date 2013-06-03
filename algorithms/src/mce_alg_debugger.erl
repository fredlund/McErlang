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


%%% Implementation of a simple simulation algorithm (and for debugging).

-module(mce_alg_debugger).
-export([default_conf/0,init/8,run/3]).

-include("monState.hrl").
-include("monitor.hrl").
-include("stackEntry.hrl").
-include("mce_opts.hrl").

-behaviour(mce_behav_algorithm).

%%-define(debug,true).
-include("macros.hrl").

default_conf() ->
  #mce_opts{output=true,sim_actions=true,small_pids=false}.

init(Conf, S, _, Stack, Monitor, _, _, Scheduler) ->
  safety = mce_behav_monitorOps:monitorType(Monitor),
  SM = #monState{state=S, monitor=Monitor},
  Debugger = mce_conf:get_debugger(Conf),
  Debugger:init(Scheduler),
  {ok,
   {?MODULE,
    run,
    [mce_behav_stackOps:push(#stackEntry{state=SM}, Stack), Scheduler, Conf]}}.

run(Stack, _Scheduler, Conf) ->
  (Conf#mce_opts.mce_monitor)!disable_deadlines,
  Debugger = mce_conf:get_debugger(Conf),
  Debugger:start(Stack),
  mce_result:mk_ok().
