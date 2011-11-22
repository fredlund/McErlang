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

-module(mce_erl_rcv_agent).
-behaviour(gen_server).
-export
([init/1,handle_call/3,handle_info/2,terminate/2,
  makeRcvAgent/1,shutDown/0,do_start/1,
  gen_udp_open/2]).

-include("system.hrl").

%%-define(debug,true).

-ifdef(debug).
-define(LOG(X,Y), io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-else.
-define(LOG(X,Y), true).
-endif.

init([SimPid]) -> 
  io:format("Starting rcv agent process~n",[]),
  {ok,{SimPid,[]}}.

handle_call(Message,Other,State) ->
  ?LOG("Got message ~p in state~n  ~p~n",[Message,State]),
  Result = treat_call(Message,Other,State),
  ?LOG("Leaving result ~p~n",[Result]),
  Result.

treat_call({initialise,SimPid},_,_State) ->
  {reply,ok,{SimPid,[]}};

treat_call(shutDown,_,State) ->
  {stop,normal,shuttingDown,State};

treat_call({gen_udp_open,{Port,Options,Pid}},_,State) ->
  Result = gen_udp:open(Port,Options),
  case Result of 
      {ok,Socket} ->
	  {reply,Result,addUdpSocket(Pid,Socket,State)};
      Other ->
	  {reply,Other,State}
  end.

handle_info(Msg = {udp,Socket,_,_,_},State) ->
  case findUdpSocketOwner(Socket,State) of
      {ok, Pid} ->
	   %% do something
	  sendMsg(Pid,Msg,State),
	  {noreply, State};
      _ ->
	  io:format("Socket ~p not found~n",[Socket]),
	  {noreply, State}
  end.

terminate(Reason,State) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

addUdpSocket(Pid,Socket,{SimPid,State}) ->
    {SimPid,[{Pid,Socket}|State]}.

findUdpSocketOwner(Socket,{SimPid,SocketList}) ->
    findUdpSocketOwnerL(Socket,SocketList).

findUdpSocketOwnerL(Socket,[]) ->
    no;
findUdpSocketOwnerL(Socket,[{Pid,Socket}|_]) ->
    {ok, Pid};
findUdpSocketOwnerL(Socket,[_|Rest]) ->
    findUdpSocketOwnerL(Socket,Rest).

sendMsg(Pid,Msg,{SimPid,_}) ->
    SimPid!{rcv,{Pid,Msg}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

callRcvAgent(Message) ->
    try
      gen_server:call(rcvAgent, Message, infinity)
    catch
      error:Reason ->
	  io:format("Rcv agent process crashed:~n~p!~n", [Reason]),
	  report_errors(),
	  throw(rcvAgent);
      Reason ->
	  io:format("Rcv agent process throwed:~n~p!~n", [Reason]),
	  report_errors(),
	  throw(rcvAgent);
      exit:Reason ->
	  io:format("Rcv agent process exited:~n~p!~n", [Reason]),
	  report_errors(),
	  throw(rcvAgent)
    end.

report_errors() ->
  receive
    {'EXIT',Reason,From} ->
      io:format("crash reason:~n~p~n",[Reason]),
      report_errors()
  
  after 0 -> ok
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


makeRcvAgent(SimPid) ->
  case gen_server:start({local,rcvAgent},mce_erl_rcv_agent,[SimPid],[]) of
    {ok, Pid} ->
      io:format
	("Started receive agent process with pid ~p~n",[Pid]),
      callRcvAgent({initialise,SimPid});
    {error,{already_started,OldPid}} ->
      io:format("Warning: receive agent already running...~n",[]),
      shutDown(),
      timer:sleep(100),
      makeRcvAgent(SimPid);
    Result ->
      io:format
	("Couldn't start the receive agent process due to~n~p~n",[Result]),
      throw(rcvAgent)
  end.

shutDown() ->
  case whereis(rcvAgent) of
    undefined -> ok;
    _ -> callRcvAgent(shutDown)
  end.

do_start(SimPid) ->
  case whereis(rcvAgent) of
    Pid when is_pid(Pid) -> ok;
    _ -> makeRcvAgent(SimPid)
  end.

gen_udp_open(Port,Options) ->
    callRcvAgent({gen_udp_open,{Port,Options,mcerlang:self()}}).


