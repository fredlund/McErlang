%%%-------------------------------------------------------------------
%%% File    : mce_server.erl
%%% Author  : Hans Svensson <>
%%% Description : 
%%%
%%% Created : 22 Feb 2010 by Hans Svensson <>
%%%-------------------------------------------------------------------

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

%% @doc McErlang Application - mce_app_server
%%
%% <p> The <code>gen_server</code>-callback module for the McErlang
%% application. The <code>gen_server</code> keeps a little bit of a
%% state for user convenience. The state includes:
%% <ul>
%% <li> The last McErlang result (in the form of a McErlang <code>#mce_result</code>-record</li>
%% <li> The program and options used for last McErlang run (to enable re-running) </li>
%% </ul>
%% </p>

-module(mce_app_server).

-behaviour(gen_server).

-include("mce_opts.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

-record(state, {opts = undefined, %% Also contains the program
				last_res = undefined
			   }).

-define(SERVER,mcerlang).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
%% @hidden
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
%% @hidden
init([]) ->
	{ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
%% @hidden
handle_call({verify, MceOpt}, _From, State) ->
    Res = verify(MceOpt),
    {reply, ok, add_result(State, Res)};
handle_call(re_verify, _From, State) ->
    Res = verify(State#state.opts),
    {reply, ok, add_result(State, Res)};
handle_call(last_res, _From, State) ->
    {reply, State#state.last_res, State};
handle_call(last_options, _From, State) ->
    {reply, State#state.opts, State};
handle_call(_Request, _From, State) ->
    io:format("Unknown request: ~p\n", [_Request]),
    Reply = ok,
    {reply, Reply, State}.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
%% @hidden
handle_cast(_Msg, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
%% @hidden
handle_info(_Info, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
%% @hidden
terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
%% @hidden
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% Invoke model checker on function call represented by Program
%% @hidden
verify(undefined) ->
	undefined;
verify(MceOpt) ->
	mce:start(MceOpt),
        mce:result().

%% @hidden
add_result(State, Res) ->
	State#state{last_res = Res}.
