%%% File    : sim_sched.erl
%%% Author  : Hans Svensson <>
%%% Description : A scheduler for the sequential part of a run_parallel_command run
%%% Created :  3 Mar 2010 by Hans Svensson <>

%% Copyright (c) 2009, Hans Svensson
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

%% @author Hans Svensson <hanssv@chalmers.se>
%% @copyright 2009 Hans Svensson
%% @version 2.0

%% @doc McErlang Application - sim_sched
%%
%% <p> The <code>mce_behav_scheduler</code>-callback module for the
%% McErlang application. The <code>scheduler</code> randomly selects a
%% path through the state space in normal McErlang simulation
%% style. The scheduler is terminated upon reaching a <b>probe
%% action</b> named <code>end_sequential</code>. The scheduler is used
%% by the combined model checking mode, where the first (sequential)
%% part is simulated and the second part is model checked.</p>
-module(sim_sched).

-export([init/1,choose/4,willCommit/1]).

-behavior(mce_behav_scheduler).

%% @hidden
init(S) -> {ok,S}.

%% @hidden
willCommit(_) -> true.

%% @hidden
choose(Trans,SchedState,Mon,Conf) ->
	FilteredTrans =
		lists:filter(fun filterFun/1,
					 lists:map(fun(T) ->
									   mce_erl_opsem:commit(T,Mon,Conf)
							   end,Trans)),
	case length(FilteredTrans) of
		0 -> no_transitions;
		N -> Selected = random:uniform(N),
			 {ok,{lists:nth(Selected,FilteredTrans),SchedState}}
	end.

%% @hidden
filterFun({Actions,_}) ->
	not lists:any(fun sim_section_ended/1,Actions).

%% @hidden
sim_section_ended(Action) ->
	case mce_erl_actions:is_probe(Action) of
		true -> mce_erl_actions:get_probe_label(Action)=:=end_sequential;
		false -> false
	end.

			

