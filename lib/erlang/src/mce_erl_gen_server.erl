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

-module(mce_erl_gen_server).

%%-define(debug,true).

-ifdef(debug).
-define(LOG(X,Y), io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-else.
-define(LOG(X,Y), true).
-endif.

-export([start_link/3,start_link/4,start_link/5,
	 start/3,start/4,start/5,call/2,call/3,cast/2,reply/2,
	 doStart/4, doStart/3]).

%% New functionality: it is possible to specify a remote node where
%% the gen_server is started.
start_link(Node,{local, Name}, Module, Args, _Options) -> 
  Pid =
    spawn_link(Node, ?MODULE, doStart, [Name, Module, Args, mcerlang:self()]),
  waitForStart(Pid).

start_link({local, Name}, Module, Args, _Options) -> 
  Pid = spawn_link(?MODULE, doStart, [Name, Module, Args, mcerlang:self()]),
  waitForStart(Pid).

start_link(Module, Args, _Options) ->
  Pid = spawn_link(?MODULE, doStart, [Module, Args, mcerlang:self()]),
  waitForStart(Pid).

%% New functionality: it is possible to specify a remote node where
%% the gen_server is started.
start(Node, {local, Name}, Module, Args, _Options) ->
  Pid =
    spawn(Node, ?MODULE, doStart, [Name, Module, Args, mcerlang:self()]),
  waitForStart(Pid).

start({local, Name}, Module, Args, _Options) ->
  Pid =
    spawn(?MODULE, doStart, [Name, Module, Args, mcerlang:self()]),
  waitForStart(Pid).

start(Module, Args, _Options) ->
  Pid = spawn(?MODULE, doStart, [Module, Args, mcerlang:self()]),
  waitForStart(Pid).

waitForStart(Pid) ->
  receive
    {ok,started} -> {ok, Pid}
  end.

doStart(Name, Module, Args, ParentPid) ->
  {State,Timeout} =
    case apply(Module, init, [Args]) of
      {ok, AState} ->
	{AState,infinity};
      {ok, AState, infinity} ->
	{AState,infinity};
      {ok, AState, Time} when is_integer(Time), Time>0 ->
	{AState,Time}
    end,
  ?LOG("~p: going to register name ~p for ~p~n", [?MODULE,Name,self()]),
  RegisterReply =
    try register(Name,self()) of _ -> {ok,started}
    catch _ -> 
	try whereis(Name) of NamePid -> {error,{already_started,NamePid}}
	catch _ -> {error,unknown} end
    end,
  ParentPid!RegisterReply, 
  case RegisterReply of
    {ok, _} -> loop(State, Module, Timeout);
    _ -> exiting
  end.

doStart(Module, Args, ParentPid) ->
  {State,Timeout} =
    case apply(Module, init, [Args]) of
      {ok, AState} ->
	{AState,infinity};
      {ok, AState, infinity} ->
	{AState,infinity};
      {ok, AState, Time} when is_integer(Time), Time>0 ->
	{AState,Time}
    end,
  ParentPid!{ok,started},
  loop(State, Module, Timeout).

loop(State,Module,Timeout) ->
  receive
    Msg ->
      ?LOG("Module ~p got message ~p~n",[Module,Msg]),
      case Msg of
	{call, Data, ReplyId} -> 
	  ?LOG("Got call(~p,~p), going to execute ~p:~p~n",
	       [Data,ReplyId,Module,handle_call]),
	  checkResult(Module,
		      apply(Module, handle_call, [Data,ReplyId,State]),
		      ReplyId);
	{cast, Data} ->
	  ?LOG("Got cast ~p~n",[Msg]),
	  checkResult(Module,
		      apply(Module, handle_cast, [Data,State]),
		      void);
	Other ->
	  ?LOG("Got exit ~p~n",[Msg]),
	  checkResult(Module,
		      apply(Module, handle_info, [Other,State]),
		      void)
      end
  after Timeout -> 
      checkResult(Module,apply(Module, handle_info, [timeout,State]),void)
  end.

checkResult(Module,{reply,Data,State},ReplyId) -> 
  reply(ReplyId,Data),
  loop(State, Module, infinity);
checkResult(Module,{reply,Data,State,Timeout},ReplyId) 
  when is_integer(Timeout), Timeout>0 -> 
  reply(ReplyId,Data),
  loop(State, Module, Timeout);
checkResult(Module,{reply,Data,State,infinity},ReplyId) ->
  reply(ReplyId,Data),
  loop(State, Module, infinity);
checkResult(Module,{noreply,State},_ReplyId) -> 
  loop(State, Module, infinity);
checkResult(Module,{noreply,State,infinity},_ReplyId) -> 
  loop(State, Module, infinity);
checkResult(Module,{noreply,State,Timeout},_ReplyId) 
  when is_integer(Timeout), Timeout>0 -> 
  loop(State, Module, infinity);
checkResult(Module,{stop,Reason,Data,StopState},ReplyId) -> 
  reply(ReplyId,Data),
  terminating(Module,Reason,StopState);
checkResult(Module,{stop,Reason,StopState},_ReplyId) -> 
  terminating(Module,Reason,StopState);
checkResult(Module,Result,_ReplyId) ->
  io:format("~p: strange result ~p~n",[Module,Result]).

reply({Pid,Tag}, Reply) ->
  ?LOG("Sending reply ~p to ~p~n",[Reply,Pid]),
  Pid!{reply, Tag, Reply},
  true.

terminating(Module, Reason, State) ->
  apply(Module, terminate, [Reason,State]),
  ok.

call(Server, Data) ->
  call(Server, Data, infinity).

call(Server, Data, Timeout) ->
  CallRef = erlang:make_ref(),
  ?LOG("Sending Message ~p to Server ~p~n",[Data,Server]),
  Server!{call, Data, {self(),CallRef}},
  receive
    {reply, CallRef, ReturnData} -> ReturnData
  after Timeout ->
      throw(timeout)
  end.

cast(Server, Data) ->
  Server!{cast, Data}, ok.



