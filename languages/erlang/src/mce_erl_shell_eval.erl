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

-module(mce_erl_shell_eval).
-export([shell_eval/0,loop/0]).
-export([connect_to_node/2]).

%%-define(debug,true).
-include("../../../src/include/macros.hrl").
-include("emacros.hrl").

shell_eval() ->
  receive
    {mcerlPid,McErlangPid,EvalPid={pid,NodeId,_},EtsTable,CompileInfo} ->
      put(compile_info,CompileInfo),
      put(evalPids,EtsTable),
      put(mcErlangPid,McErlangPid),
      set_node(NodeId),
      add_eval_server(EvalPid);
    Other ->
      io:format("** error: got message ~p~n",[Other]),
      exit(Other)
  end,
  loop().

loop() ->
  Bindings =
    case get(shellBindings) of
      undefined -> [];
      OldBindings -> OldBindings
    end,
  case parse_eval(Bindings) of
    {value, RetVal, NewBindings} ->
      io:format("~s~n", [mce_erl_pretty:pretty(RetVal)]),
      put(shellBindings, NewBindings),
      loop();
    fail ->
      loop();
    Return ->
      io:format("ERROR: strange return value ~p~n", [Return])
  end.

parse_eval(Bindings) ->
  try
    parse_eval_1(Bindings)
  catch
    Argument ->
      io:format("** exception throw: ~p~n    Stacktrace:~n      ~p~n",
		[Argument, strip_stacktrace(erlang:get_stacktrace())]),
      fail;
      exit:Reason ->
      io:format("** exception exit: ~p~n    Stacktrace:~n      ~p~n",
		[Reason, strip_stacktrace(erlang:get_stacktrace())]),
      fail;
      error:Reason ->
      io:format("** exception error: ~p~n    Stacktrace:~n      ~p~n",
		[Reason, strip_stacktrace(erlang:get_stacktrace())]),
      fail
  end.

parse_eval_1(Bindings) ->
  io:format("~n", []),
  PromptAtom = list_to_atom("McErlang::" ++ atom_to_list(get_node()) ++ "> "),
  {ok, Terms, _} = io:parse_erl_exprs(PromptAtom),
  eval_exprs(Terms, Bindings).

eval_exprs(Terms,Bindings) ->
  erl_eval:exprs
    (Terms,Bindings,
     {value,
      fun (Fun,EvalArgs) -> eval_local_func(Fun,EvalArgs) end},
     {value,
      fun (Fun,EvalArgs) -> eval_nonlocal_func(Fun,EvalArgs) end}).

eval_local_func(Fun, EvalArgs) ->
  case {Fun,EvalArgs} of
    {connect_to_node,[NodeId]} ->
      connect_to_node(get(mcErlangPid),NodeId);
    _ ->
      io:format("Unknown local function ~p called~n",[Fun]),
      ok
  end.

eval_nonlocal_func(Fun, EvalArgs) ->
  ?LOG("eval_nonlocal(~p,~p)~n", [Fun, EvalArgs]),
  {Module, Name} =
    case Fun of
      {_ModName, _FunName} -> Fun;
      Other -> {erlang:fun_info(Fun, module),erlang:fun_info(Fun, name)}
    end,
  NewFun =
    mce_erl_compile_info:map_fun(Module,Name,EvalArgs),
  ?LOG("eval_nonlocal will evaluate function ~p~n",[NewFun]),
  Result =
    eval_command(NewFun, EvalArgs),
  ?LOG("eval_nonlocal gives result ~p~n", [Result]),
  Result.

eval_command(Fun,Args) ->
  case get_eval_server() of
    [] ->
      io:format
	("*** Error: no eval server for node ~p~n",[get_node()]),
      io:format
	("Possible error cause: specifying an anonymous function as argument "++
	 "to spawn~n",[]),
      %%io:format("servers available: ~p~n~n",[get_eval_servers()]),
      exit(eval_server);
    [EvalServer] ->
      eval_command(EvalServer,Fun,Args)
  end.  

eval_command(EvalServer,Fun,Args) ->
  McErlangPid = get(mcErlangPid),
  eval_command(EvalServer,McErlangPid,Fun,Args).

eval_command(EvalServer,McErlangPid,Fun,Args) ->
  case is_process_alive(McErlangPid) of
    false ->
      io:format("McErlang has crashed; exiting...~n",[]),
      exit(shell_eval);
    true ->
      ok
  end,
  ?LOG("Before ~p!{rcv,{~p,{apply,{~p,~p},~p}}}~n",
       [McErlangPid,EvalServer,Fun,Args,self()]),
  McErlangPid!{rcv,{EvalServer,{apply,{Fun,Args},self()}}},
  receive
    {shell_mcerl,Result} ->
      ?LOG("Got result ~p~n",[Result]),
      case Result of
	{throw,Value,Stacktrace} ->
	  erlang:raise(throw,Value,Stacktrace);
	{error,Reason,Stacktrace} ->
	  erlang:raise(error,Reason,Stacktrace);
	{exit,Reason,Stacktrace} ->
	  erlang:raise(exit,Reason,Stacktrace);
	Value -> Value
      end
  end.

strip_stacktrace([{erl_eval,do_apply,_}|_]) ->
  [];
strip_stacktrace([{erl_eval,local_func,_}|_]) ->
  [];
strip_stacktrace([{shell_eval,eval_local_func,[Name,Arguments]}|_]) ->
  [{?MODULE,Name,Arguments}];
strip_stacktrace([{shell_mcerl_try,do_apply_try,_}|_]) ->
  [];
strip_stacktrace([{shell_eval,eval_nonlocal_func,[Name,Arguments]}|_]) ->
  [{?MODULE,Name,Arguments}];
strip_stacktrace([Hd|Rest]) ->
  [Hd|strip_stacktrace(Rest)];
strip_stacktrace([]) ->
  [].

add_eval_server(Pid={pid,_,_}) ->
  ets:insert(get(evalPids),Pid).

del_eval_server(NodeId) ->
  ets:delete(get(evalPids),NodeId).

get_eval_server(NodeId) ->
  ets:lookup(get(evalPids),NodeId).

get_eval_servers() ->
  ets:tab2list(get(evalPids)).

get_eval_server() ->
  get_eval_server(get(nodeId)).

set_node(NodeId) ->
  put(nodeId,NodeId).

get_node() ->
  get(nodeId).

connect_to_node(McErlangPid,NodeId) ->
  set_node(NodeId),
  case get_eval_server() of
    [] ->
      boot_node_server(McErlangPid);
    [_] ->
      ok
  end.

boot_node_server(McErlangPid) ->
  case get_eval_server() of
    [_] ->
      io:format("Node server already running...~n",[]),
      ok;
    [] ->
      case get_eval_servers() of
	[] ->
	  io:format("No node server running; cannot boot new...~n",[]),
	  ok;
	[Server={pid,Node,_}|_] ->
	  add_eval_server
	    (eval_command
	     (Server,McErlangPid,{mcerlang,spawn},
	      [get_node(),shell_mcerl,loop,[self()]])),
	  ok
      end
  end.
