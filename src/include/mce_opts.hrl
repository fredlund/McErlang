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

-record(mce_opts,
	{transitions=undefined,        %% Function for computing next transition
	 commit=undefined,             %% Function for committing to transition
	 sim_external_world=undefined, %% The simulation interfaces with the 
	 %% external world, i.e., don't halt it when
	 %% there are no transitions but recheck.
	 random=undefined,             %% Randomize the order of transitions
	 pathLimit=undefined,          %% Limit paths to maximum depth
	 shortest=undefined,           %% Compute shortest path to failure
	 terminate=undefined,          %% Randomly terminate processes
	 is_simulation=undefined,
	 algorithm=undefined,          %% Traversal algorithm
	 monitor=undefined,            %% Default monitor
	 abstraction=undefined,        %% Default abstraction
	 scheduler=undefined,          %% Default scheduler
	 table=undefined,              %% Default state table
	 stack=undefined,              %% Default stack implementation 
	 small_pids=undefined,         %% Try to reuse pids
	 notice_exits=undefined,       %% Warn when user code crashes
	 fail_on_exit=undefined,       %% Fail if user code crashes
	 seed=undefined,               %% Initial seed
	 sim_actions=undefined,        %% Print actions during simulation
	 output=undefined,             %% Output from mcerlang:format
	 sim_keep_stack=undefined,     %% Keep stack during simulation
	 start_debugger=undefined,     %% Start debugger directly upon failure
	 save_result=undefined,        %% Save result of McErlang run
	 language=undefined,           %% Default language of verified programs
	 distributed_semantics,        %% Every process is a node semantics
	 sends_are_sefs,               %% Context-switch on send actions
	 chatter=undefined,            %% Level of verbosity
	 saved_stack=undefined,
	 time_limit=undefined,         %% Time limit for verification run
	                               %% in seconds
	 program=undefined,
	 parent=undefined,
	 record_actions=undefined,
	 is_infinitely_fast=undefined,
	 funinfo=undefined,
	 save_table=undefined,
	 debugger=undefined,           %% Default debugger 
	 %% (we should have something more general instead)
	 rpc=undefined,                 %% Support rpc calls
	 node=undefined,                

	 %% private date not for public use
	 mce_monitor=undefined
	}).


	 
