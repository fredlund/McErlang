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

-module(mce_erl_actions).

-export([get_name/1,get_source/1,get_label/1]).

-export([mk_send/3, mk_send/4, mk_probe/3, mk_probe/2, mk_input/4, mk_io/2, mk_recv/2]).
-export([mk_run/2,mk_run/3,mk_run/4,mk_output/4,mk_died/2,mk_spawn/3]).
-export([mk_crashing/4,mk_terminated/2,mk_api_call/4,mk_deliver/4]).

-export([type/1]).
-export([is_send/1,is_probe/1,is_input/1,is_io/1,is_recv/1,is_run/1,is_died/1]).
-export([is_output/1,is_spawn/1,is_crashing/1,is_terminated/1,is_api_call/1]).
-export([is_deliver/1]).

-export([get_send_pid/1,get_send_resolved_pid/1,get_send_msg/1,
	 get_probe/1,get_probe_label/1,get_probe_term/1,
	 get_input_from/1,get_input_to/1,get_input_signal/1,get_io_msg/1,
	 get_recv_msg/1,get_run_expr/1,get_output_from/1,get_output_to/1,
	 get_output_signal/1,get_died_reason/1,get_spawn_arguments/1,
	 get_spawn_result/1,get_crashing_exception/1,get_crashing_reason/1,
	 get_crashing_trace/1,get_terminated_reason/1,get_api_call_fun/1,
	 get_deliver_from/1,get_deliver_to/1,
	 get_deliver_pid/1,get_deliver_msg/1,
	 get_api_call_arguments/1,get_api_call_result/1]).

-export([record/1]).

%%-define(debug,true).
-include("../../../src/include/macros.hrl").
-include("emacros.hrl").

type(Action) ->
  Name = get_name(Action),
  case lists:member(Name,[send,probe,input,io,recv,run,output,died,spawn,
			  crashing,terminated,api_call,deliver]) of
    true ->
      Name;
    false ->
      non_erlang_action
  end.

get_name(Action) ->
  mce_actions:get_name(Action).
  
get_source(Action) ->
  mce_actions:get_source(Action).
  
get_label(Action) ->
  mce_actions:get_label(Action).

%% @private
mk_send(Source,Pid,Msg) ->
  mce_actions:mk(Source,send,{Pid,void,Msg}).

mk_send(Source,Pid,ResolvedPid,Msg) ->
  mce_actions:mk(Source,send,{Pid,ResolvedPid,Msg}).

is_send(Action) ->
  case mce_actions:is_action(Action) of
    true -> get_name(Action)=:=send;
    false -> false
  end.

get_send_pid(Action) ->
  element(1,mce_actions:get_argument(Action)).

get_send_resolved_pid(Action) ->
  element(2,mce_actions:get_argument(Action)).

get_send_msg(Action) ->
  element(3,mce_actions:get_argument(Action)).

%% @private
mk_probe(Source,Label) ->
  mce_actions:mk(Source,probe,Label,void).

%% @private
mk_probe(Source,Label,Term) ->
  mce_actions:mk(Source,probe,Label,Term).

is_probe(Action) ->
  case mce_actions:is_action(Action) of
    true -> get_name(Action)=:=probe;
    false -> false
  end.

get_probe_label(Action) ->
  mce_actions:get_label(Action).

get_probe_term(Action) ->
  mce_actions:get_argument(Action).

get_probe(Action) ->
  {get_probe_label(Action), get_probe_term(Action)}.

%% @private
mk_input(Source,From,To,Signal) ->
  mce_actions:mk(Source,input,{From,To,Signal}).

is_input(Action) ->
  case mce_actions:is_action(Action) of
    true -> get_name(Action)=:=input;
    false -> false
  end.

get_input_from(Action) ->
  element(1,mce_actions:get_argument(Action)).

get_input_to(Action) ->
  element(2,mce_actions:get_argument(Action)).

get_input_signal(Action) ->
  element(3,mce_actions:get_argument(Action)).

%% @private
mk_io(Pid,Msg) ->
  mce_actions:mk(Pid,io,Msg).

is_io(Action) ->
  case mce_actions:is_action(Action) of
    true -> get_name(Action)=:=io;
    false -> false
  end.

get_io_msg(Action) ->
  mce_actions:get_argument(Action).

%% @private
mk_recv(Pid,Msg) ->
  mce_actions:mk(Pid,recv,Msg).

is_recv(Action) ->
  case mce_actions:is_action(Action) of
    true -> get_name(Action)=:=recv;
    false -> false
  end.

get_recv_msg(Action) ->
  mce_actions:get_argument(Action).

%% @private
mk_run(Pid,Expr) ->
  mce_actions:mk(Pid,run,Expr).
  
%% @private
mk_run(Pid,Fun,Arguments) ->
  mce_actions:mk(Pid,run,{Fun,Arguments}).

is_run(Action) ->
  case mce_actions:is_action(Action) of
    true -> get_name(Action)=:=run;
    false -> false
  end.

%% @private
mk_run(Pid,Module,Fun,Arguments) ->
  mce_actions:mk(Pid,run,{{Module,Fun},Arguments}).

get_run_expr(Action) ->
  mce_actions:get_argument(Action).

%% @private
mk_output(Source,From,To,Signal) ->
  mce_actions:mk(Source,output,{From,To,Signal}).

is_output(Action) ->
  case mce_actions:is_action(Action) of
    true -> get_name(Action)=:=output;
    false -> false
  end.

get_output_from(Action) ->
  element(1,mce_actions:get_argument(Action)).

get_output_to(Action) ->
  element(2,mce_actions:get_argument(Action)).

get_output_signal(Action) ->
  element(3,mce_actions:get_argument(Action)).

%% @private
mk_died(Pid,Reason) ->
  mce_actions:mk(Pid,died,Reason).

is_died(Action) ->
  case mce_actions:is_action(Action) of
    true -> get_name(Action)=:=died;
    false -> false
  end.

get_died_reason(Action) ->
  mce_actions:get_argument(Action).

%% @private
mk_spawn(Source,Arguments,Result) ->
  mce_actions:mk(Source,spawn,{Arguments,Result}).

is_spawn(Action) ->
  case mce_actions:is_action(Action) of
    true -> get_name(Action)=:=spawn;
    false -> false
  end.

get_spawn_arguments(Action) ->
  element(1,mce_actions:get_argument(Action)).

get_spawn_result(Action) ->
  element(2,mce_actions:get_argument(Action)).
  
%% @private
mk_crashing(Pid,Exception,Reason,Trace) ->
  mce_actions:mk(Pid,crashing,{Exception,Reason,Trace}).
  
is_crashing(Action) ->
  case mce_actions:is_action(Action) of
    true -> get_name(Action)=:=crashing;
    false -> false
  end.

get_crashing_exception(Action) ->
  element(1,mce_actions:get_argument(Action)).

get_crashing_reason(Action) ->
  element(2,mce_actions:get_argument(Action)).

get_crashing_trace(Action) ->
  element(3,mce_actions:get_argument(Action)).

%% @private
mk_terminated(Pid,Reason) ->
  mce_actions:mk(Pid,terminated,Reason).

is_terminated(Action) ->
  case mce_actions:is_action(Action) of
    true -> get_name(Action)=:=terminated;
    false -> false
  end.

get_terminated_reason(Action) ->
  mce_actions:get_argument(Action).

%% @private
mk_api_call(Source,Fun,Arguments,Result) ->
  mce_actions:mk(Source,api_call,{Fun,Arguments,Result}).

is_api_call(Action) ->
  case mce_actions:is_action(Action) of
    true -> get_name(Action)=:=api_call;
    false -> false
  end.

get_api_call_fun(Action) ->
  element(1,mce_actions:get_argument(Action)).

get_api_call_arguments(Action) ->
  element(2,mce_actions:get_argument(Action)).

get_api_call_result(Action) ->
  element(3,mce_actions:get_argument(Action)).

is_deliver(Action) ->
  case mce_actions:is_action(Action) of
    true -> get_name(Action)=:=deliver;
    false -> false
  end.

mk_deliver(From,To,Pid,Message) ->
  mce_actions:mk(To,deliver,{From,To,Pid,Message}).

get_deliver_from(Action) ->
  element(1,mce_actions:get_argument(Action)).

get_deliver_to(Action) ->
  element(2,mce_actions:get_argument(Action)).

get_deliver_pid(Action) ->
  element(3,mce_actions:get_argument(Action)).

get_deliver_msg(Action) ->
  element(4,mce_actions:get_argument(Action)).

%% @private
record(Action) ->
  DoRecord =
    case mce_actions:get_name(Action)=:=probe of
      true -> true;
      false ->
	case erlang:get(record) of
	  true -> true;
	  _ -> false
	end
    end,
  ?LOG("~n~s~n",[mce_erl_debugger:print_action(Action)]),
  if DoRecord -> mce_erl_opsem:record_action(Action); 
     true -> ok
  end.
  
