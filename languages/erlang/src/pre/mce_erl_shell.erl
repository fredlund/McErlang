%% @author Lars-Ake Fredlund
%% @copyright 2006-2009 Lars-Ake Fredlund
%% @doc
%% @private

-module(mce_erl_shell).
-export([start/1,loop/1]).

%%-define(debug,true).
-include("../../../../src/include/macros.hrl").
-include("../include/emacros.hrl").

start(Pid) ->
  Self = self(),
  McErlangPid =
    mce_erl:apply(erlang, self, []),
  EtsTable =
    mce_erl:apply
      (ets, new, [evalServers, [public, set, {keypos, 2}]]),
  mce_erl:apply
    (erlang, put, [evalPids, EtsTable]),
  mce_erl:apply
    (erlang,
     send,
     [Pid, {mcerlPid,McErlangPid,Self,EtsTable,mce_conf:get_compile_info()}]),
  loop(Pid).

loop(ErlangPid) ->
  ?LOG("loop(~p)~n",[ErlangPid]),
  receive
    {apply, {Function, Args}, ReturnPid} ->
      ?LOG("Got apply ~p; return to ~p~n",[{Function,Args},ReturnPid]),
      Result =
	try
	  apply(Function, Args)
	catch
	  Error:Reason -> {Error, Reason, erlang:get_stacktrace()}
	end,
      ?LOG("Result is ~p~n",[Result]),
      mce_erl:apply(erlang, send, [ReturnPid, {shell_mcerl, Result}]),
      loop(ErlangPid);
    shutdown ->
      ok;
    Msg ->
      ?LOG("Got a message ~p~n",[Msg]),
      mce_erl:apply(erlang, send, [ErlangPid, Msg]),
      loop(ErlangPid)
  end.
