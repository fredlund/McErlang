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

%%% Does not implement restarts yet

-module(mce_erl_supervisor).

%%-define(debug,true).

-ifdef(debug).
-define(LOG(X,Y), io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-else.
-define(LOG(X,Y), true).
-endif.

-export([start_link/2,start_link/3,doStart/3,doStart/4]).
-export([start_child/2]).

start_link(Module, Arg) ->
  ?LOG("start_link(~p,~p)~n",[Module,Arg]),
  Pid = spawn_link(?MODULE, doStart, [Module, Arg, self()]),
  waitForStart(Pid).

start_link({local,Name}, Module, Arg) ->
  ?LOG("start_link(~p,~p,~p)~n",[{local,Name},Module,Arg]),
  Pid = spawn_link(?MODULE, doStart, [Name, Module, Arg, self()]),
  waitForStart(Pid).

waitForStart(Pid) ->
  receive
    {ok,started} -> {ok, Pid}
  end.

doStart(Name, Module, Arg, SupervisorPid) ->
  process_flag(trap_exit,true),
  ?LOG("~p: going to register name ~p for ~p~n",[?MODULE,Name,self()]),  
  register(Name, self()),
  Result = apply(Module, init, [Arg]),
  startChildren(Result, SupervisorPid, Module).

doStart(Module, Arg, SupervisorPid) ->
  process_flag(trap_exit,true),
  Result = apply(Module, init, [Arg]),
  startChildren(Result, SupervisorPid, Module).

loop(ModuleName) ->
  receive
    {no_match,of_anything} -> loop(ModuleName)
  end.

startChildren({ok,{{one_for_one,_MaxR,_MaxT},Children}},SuperPid,ModuleName) ->
  startCh(Children, SuperPid, ModuleName);
startChildren({ok,{_,Children}}, SuperPid, ModuleName) ->
  startCh(Children, SuperPid, ModuleName).

startCh([],SupervisorPid,ModuleName) ->  
  SupervisorPid!{ok,started}, 
  loop(ModuleName);
%% Terminate is a special option to reduce the state space
%% when the supervisor is just used to create the system and
%% not monitor it; it can safely be ignored.
startCh([terminate|_],_,ModuleName) ->
  loop(ModuleName);
startCh([ChildSpec|Rest],SupervisorPid,ModuleName) ->
  {ok,_Child} = start_child(ChildSpec),
  startCh(Rest,SupervisorPid,ModuleName).

start_child(ChildSpec) ->
  ?LOG("start_child(~p)~n",[ChildSpec]),
  case ChildSpec of
    {_Id,{Module,Fun,Args},temporary,_ShutDown,_Type,_Modules} ->
      Result = {ok,_Child} = apply(Module,Fun,Args),
      Result;
    {_Id,{Module,Fun,Args},permanent,_ShutDown,_Type,_Modules} ->
      Result = {ok,_Child} = apply(Module,Fun,Args),
      Result
  end.

start_child(_SupRef,ChildSpec) ->
  start_child(ChildSpec).

  

