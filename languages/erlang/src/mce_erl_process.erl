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

-module(mce_erl_process).
-export([makeRunnable/2,makeRunnable/3,makeRunnableInSystem/3]).

-include("process.hrl").

%%-define(debug,true).
-include("../../../src/include/macros.hrl").
-include("emacros.hrl").

makeRunnableInSystem(Expr, Node, State) ->
  Pid = mce_erl_references:getNewPidInSystem(Node, Expr, State, void),
  ?LOG("~nnewProc(~p,~p) --> ~p~n",[Expr,Node,Pid]),
  #process{status=runnable,expr=Expr,pid=Pid}.

makeRunnable(Expr, Node) ->
  Pid = mce_erl_references:getNewPid(Node, Expr),
  ?LOG("~nnewProc(~p,~p) --> ~p~n",[Expr,Node,Pid]),
  #process{status=runnable,expr=Expr,pid=Pid}.

makeRunnable(Expr, Node, Conf) ->
  Pid = mce_erl_references:getNewPid(Node, Expr, Conf),
  ?LOG("~nnewProc(~p,~p) --> ~p~n",[Expr,Node,Pid]),
  #process{status=runnable,expr=Expr,pid=Pid}.


  
  

    
