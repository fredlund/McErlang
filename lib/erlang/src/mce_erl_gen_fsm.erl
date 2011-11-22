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

-module(mce_erl_gen_fsm).

%%-define(debug,true).

-ifdef(debug).
-define(LOG(X,Y), io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-else.
-define(LOG(X,Y), true).
-endif.

-export([start_link/3,start_link/4,start/3,start/4,
	 doStart/4, doStart/3,
	 send_event/2,send_all_state_event/2,
	 sync_send_event/2,sync_send_event/3,
	 sync_send_all_state_event/2, sync_send_all_state_event/3,
	 reply/2]).

start_link({local, Name}, Module, Args, _Options) -> 
  Pid = spawn_link(?MODULE, doStart, [Name, Module, Args, mcerlang:self()]),
  waitForStart(Pid).

start_link(Module, Args, _Options) ->
  Pid = spawn_link(?MODULE, doStart, [Module, Args, mcerlang:self()]),
  waitForStart(Pid).

start({local, Name}, Module, Args, _Options) ->
  Pid = spawn(?MODULE, doStart, [Name, Module, Args, mcerlang:self()]),
  waitForStart(Pid).

start(Module, Args, _Options) ->
  Pid = spawn(?MODULE, doStart, [Module, Args, mcerlang:self()]),
  waitForStart(Pid).

waitForStart(Pid) ->
  receive
    {ok,started} -> {ok, Pid}
  end.

doStart(Name, Module, Args, ParentPid) ->
  {ok, StateName, StateData} = apply(Module, init, [Args]),
  ?LOG("ev_gen_server: going to register name ~p for ~p~n",
	    [Name,self()]),  
  register(Name,self()),
  ParentPid!{ok,started},
  loop(StateName, StateData, Module, infinity).

doStart(Module, Args, ParentPid) ->
  {ok, StateName, StateData} = apply(Module, init, [Args]),
  ParentPid!{ok,started},
  loop(StateName, StateData, Module, infinity).

loop(StateName, StateData, Module, Timeout) ->
  receive
    Msg ->
      ?LOG("ev_gen_fsm2: module ~p got message ~p~n",[Module,Msg]),
      case Msg of
	{asynch_send_event, Event} -> 
	  ?LOG("Got asynch_event(~p), going to execute ~p:~p~n",
	       [Event,Module,StateName]),
	  check(Module,
		apply(Module, StateName, [Event,StateData]),
		void,
		StateName);
	{asynch_send_all_event, Event} ->
	  ?LOG("Got asynch_all ~p, going to execute ~p:handle_event~n",
	       [Event,Module]),
	  check(Module,
		apply(Module, handle_event, [Event,StateName,StateData]),
		void,
		      StateName);
	{sync_send_event, Event, From} -> 
	  ?LOG("Got synch_event(~p), going to execute ~p:~p~n",
	       [Event,Module,StateName]),
	  check(Module,
		apply(Module, StateName, [Event,From,StateData]),
		From,
		StateName);
	{sync_send_all_event, Event, From} ->
	  ?LOG("Got synch_all ~p, going to execute ~p:handle_event~n",
	       [Event,Module]),
	  check(Module,
		apply(Module, handle_sync_event, 
		      [Event,From,StateName,StateData]),
		From,
		StateName);
	Other ->
	  %%io:format("Got exit ~p~n",[Msg]),
	  check(Module,
		apply(Module, handle_info, [Other,StateName,StateData]),
		void,
		StateName)
      end
  after Timeout -> 
      check(Module,
	    apply(Module, StateName, [timeout,StateData]),
	    void,
	    StateName)
  end.

check(Module,{next_state,NextStateName,NewStateData},_ReplyId,_StateName) -> 
  loop(NextStateName, NewStateData, Module, infinity);
check(Module,{next_state,NextStateName,NewStateData,Timeout},_ReplyId,_StateName) -> 
  loop(NextStateName, NewStateData, Module, Timeout);
check(Module,{reply,Reply,NextStateName,NewStateData},ReplyId,_StateName) -> 
  reply(ReplyId,Reply),
  loop(NextStateName, NewStateData, Module, infinity);
check(Module,{reply,Reply,NextStateName,NewStateData,Timeout},ReplyId,_StateName) -> 
  reply(ReplyId,Reply),
  loop(NextStateName, NewStateData, Module, Timeout);
check(Module,{stop,Reason,NewStateData},_ReplyId,StateName) -> 
  terminating(Module,Reason,StateName,NewStateData);
check(Module,{stop,Reason,Reply,NewStateData},ReplyId,StateName) -> 
  reply(ReplyId,Reply),
  terminating(Module,Reason,StateName,NewStateData);
check(Module,Result,_ReplyId,_StateName) ->
  io:format("~p: strange result ~p~n",[Module,Result]).

reply({Pid,Tag}, Reply) ->
  ?LOG("Sending reply ~p to ~p~n",[Reply,Pid]),
  Pid!{reply, Tag, Reply},
  true.

terminating(Module, Reason, StateName, StateData) ->
  apply(Module, terminate, [Reason,StateName,StateData]),
  ok.

sync_send_event(Server, Event) ->
  call(sync_send_event, Server, Event).

sync_send_event(Server, Event, Timeout) ->
  call(sync_send_event, Server, Event, Timeout).

sync_send_all_state_event(Server, Event) ->
  call(sync_send_all_event, Server, Event).

sync_send_all_state_event(Server, Event, Timeout) ->
  call(sync_send_all_event, Server, Event, Timeout).

call(MsgType, Server, Event) ->
  CallRef = erlang:make_ref(),
  ?LOG("Sending Message ~p to Server ~p~n",[Event,Server]),
  Server!{MsgType, Event, {self(),CallRef}},
  receive
    {reply, CallRef, ReturnData} -> ReturnData
  end.

call(MsgType, Server, Event, Timeout) ->
  CallRef = erlang:make_ref(),
  ?LOG("Sending Message ~p to Server ~p~n",[Event,Server]),
  Server!{MsgType, Event, {self(),CallRef}},
  receive
    {reply, CallRef, ReturnData} -> ReturnData
  after Timeout ->
	  throw(Timeout)
  end.

send_event(Server, Event) ->
  Server!{asynch_send_event, Event}, ok.

send_all_state_event(Server, Event) ->
  Server!{asynch_send_all_event, Event}, ok.

		  
  



  



