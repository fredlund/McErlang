%%% File    : mce_app_supv.erl
%%% Author  : Hans Svensson <>
%%% Description : 
%%% Created : 22 Feb 2010 by Hans Svensson <>

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

%% @doc McErlang Application - mce_app_supv
%%
%% <p> The <code>supervisor</code>-callback module for the McErlang
%% application. The <code>supervisor</code> starts (and possibly
%% restarts) the application. No initial arguments are used, the
%% restart strategy is <code>one_for_one</code>, the termination
%% timeout is 5000.  </p>
-module(mce_app_supv).

-behavior(supervisor).

-export([start_link/1,init/1]).

%% @hidden
start_link(Args) ->
	supervisor:start_link({local,?MODULE},?MODULE,Args).

%% @hidden
init(_Args) ->
	{ok,
	 {{one_for_one,1,5},
	  [{mce_app,
		{mce_app_server, start_link, []},
		permanent,
		5000,
		worker,
		[mce_app_server]}
	   ]}}.
