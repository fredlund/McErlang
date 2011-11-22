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

-module(mce_result).

-record(mce_result,
	{type=void,
	 conf=void,
	 error=void,
	 table=void,
	 mcerlang_version=void,
	 erlang_version=void,
	 binaries=void,
	 reductions=void,
	 runtime=void,
	 wallclock=void,
	 stack=void,
	 state=void,
	 monitor=void,
	 stored_states=void,
	 explored_states=void,
	 private=void}).

-include("mce_opts.hrl").

-export([is_mce_result/1,print_result_type/1]).
-export([type/1,conf/1,error/1,table/1,stack/1,state/1,monitor/1,private/1]).
-export([add_conf/2,add_error/2,add_table/3,add_monitor/2,
	 add_stack/2,add_state/2,add_private/2]).
-export([add_mcerlang_version/2,add_erlang_version/2,
	 mcerlang_version/1,erlang_version/1]).
-export([stored_states/1,explored_states/1]).
-export([add_stored_states/2,add_explored_states/2]).
-export([binaries/1,add_binaries/2]).

-export([is_error/1,is_correct/1,is_code_error/1]).

-export([reductions/1,runtime/1,wallclock/1]).
-export([add_reductions/2,add_runtime/2,add_wallclock/2]).

-export([is_inconclusive/1]).

-export([throw_result_exc/1,is_result_exc/2,result_from_exc/2]).

-export([mk_exception_error/2,mk_exception_error/3,mk_value_error/1]).
-export([error_type/1]).
-export([exception_type/1,exception_reason/1,exception_stacktrace/1]).
-export([error_value/1]).
-export([inconclusive_reason/1]).


-export([mk_badmon/2,mk_badmon/4,
	 mk_badloop/1,mk_badloop/2,mk_badloop/4, 
	 mk_ok/0,mk_ok/1,
	 mk_user_error/1,mk_user_error/2,
	 mk_user_error/4,
	 mk_mon_error/3,
	 mk_timeout/0,
	 mk_timeout/1,
	 mk_out_of_memory/0,
	 mk_out_of_memory/1,
	 save_result/1,
	 get_result/0,
	 mk_internal_error/1]).


save_result(Result) ->
  put(result,Result).

get_result() ->
  Result = get(result),
  case is_mce_result(Result) of
    true ->
      {ok, Result};
    false ->
      no
  end.

is_mce_result(Arg) ->
  is_record(Arg,mce_result).
type(Arg) ->
  Arg#mce_result.type.
conf(Arg) ->
  Arg#mce_result.conf.
error(Arg) ->
  Arg#mce_result.error.
table(Arg) ->
  Arg#mce_result.table.
stack(Arg) ->
  Arg#mce_result.stack.
monitor(Arg) ->
  Arg#mce_result.monitor.
state(Arg) ->
  Arg#mce_result.state.
private(Arg) ->
  Arg#mce_result.private.
erlang_version(Arg) ->
  Arg#mce_result.erlang_version.
mcerlang_version(Arg) ->
  Arg#mce_result.mcerlang_version.
binaries(Arg) ->
  Arg#mce_result.binaries.

is_correct(Result) -> not is_error(Result).

is_error(Result) ->
  case type(Result) of
    badmon ->
      true;
    badloop ->
      true;
    user_error ->
      true;
    mon_error ->
      true;
    internal_error ->
      true;
    _ ->
      false
  end.

is_inconclusive(Result) ->
  type(Result)==inconclusive.

inconclusive_reason(Result) ->
  error_value(error(Result)).

print_result_type(Result) ->
  case mce_result:type(Result) of
    badmon ->
      "monitor failure";
    badloop ->
      "non-progress detected in buechi automaton";
    user_error ->
      "user program raised an uncaught exception";
    internal_error ->
      "internal error in the McErlang model checker detected";
    ok ->
      "successful termination";
    inconclusive ->
      "inconclusive result"
  end.

is_code_error(Result) ->
  case type(Result) of
    user_error ->
      true;
    badmon ->
      true;
    badloop ->
      true;
    _ ->
      false
  end.

add_conf(Conf,Arg) ->
  Arg#mce_result{conf=Conf}.
add_error(Error,Arg) ->
  verify_error(Error),
  Arg#mce_result{error=Error}.
add_table(Table,Arg,Conf) ->
  if Conf#mce_opts.save_table -> Arg#mce_result{table=Table};
     true -> Arg
  end.
add_stack(Stack,Arg) ->
  Arg#mce_result{stack=Stack}.
add_monitor(Monitor,Arg) ->
  Arg#mce_result{monitor=Monitor}.
add_state(State,Arg) ->
  Arg#mce_result{state=State}.
add_private(Private,Arg) ->
  Arg#mce_result{private=Private}.
add_erlang_version(ErlangVersion,Arg) ->
  Arg#mce_result{erlang_version=ErlangVersion}.
add_mcerlang_version(McerlangVersion,Arg) ->
  Arg#mce_result{mcerlang_version=McerlangVersion}.
add_binaries(Binaries,Arg) ->
  Arg#mce_result{binaries=Binaries}.

add_reductions(Reductions,Arg) ->
  Arg#mce_result{reductions=Reductions}.
add_runtime(Runtime,Arg) ->
  Arg#mce_result{runtime=Runtime}.
add_wallclock(Wallclock,Arg) ->
  Arg#mce_result{wallclock=Wallclock}.

%% @private
mk_badmon(MonError, Table) ->
  add_table
    (Table,
     #mce_result
       {type=badmon,
	monitor=MonError,
	error=mk_value_error(MonError)},
     mce_conf:get_conf()).
%% @private
mk_badmon(MonError, Stack, Table, Conf) ->
  add_table
    (Table,
     #mce_result
       {type=badmon,stack=Stack,monitor=MonError,
	error=mk_value_error(MonError),conf=Conf},
     Conf).

%% @private
mk_badloop(LoopState) ->
  #mce_result{type=badloop,state=LoopState}.
%% @private
mk_badloop(LoopState, Table) ->
  add_table
    (Table,#mce_result{type=badloop,state=LoopState},mce_conf:get_conf()).
%% @private
mk_badloop(LoopState, Stack, Conf, Table) ->
  add_table
    (Table,
     #mce_result{type=badloop,stack=Stack,conf=Conf,state=LoopState},
     Conf).

%% @private
mk_ok() ->
  #mce_result{type=ok}.
%% @private
mk_ok(Table) ->
  add_table(Table,#mce_result{type=ok},mce_conf:get_conf()).

mk_timeout() ->
  #mce_result{type=inconclusive,error=mk_value_error(timeout)}.
mk_timeout(Table) ->
  add_table(Table,mk_timeout(),mce_conf:get_conf()).

mk_out_of_memory() ->
  #mce_result{type=inconclusive,error=mk_value_error(out_of_memory)}.
mk_out_of_memory(Table) ->
  add_table(Table,mk_out_of_memory(),mce_conf:get_conf()).

%% @private
mk_exception_error(Exception,Reason) ->
  {exception,{Exception,Reason}}.
%% @private
mk_exception_error(Exception,Reason,Stacktrace) ->
  {exception,{Exception,Reason,Stacktrace}}.
%% @private
mk_value_error(ErrorValue) ->
  {value,ErrorValue}.
error_type({Type,_Error}) ->
  Type.
exception_type({exception,Exception}) ->
  case Exception of
    {Type,_} -> Type;
    {Type,_,_} -> Type
  end.
exception_reason({exception,Exception}) ->
  case Exception of
    {_,Reason} -> Reason;
    {_,Reason,_} -> Reason
  end.
exception_stacktrace({exception,Exception}) ->
  case Exception of
    {_,_,Stacktrace} -> Stacktrace;
    _ -> []
  end.
error_value({value,ErrorValue}) ->
  ErrorValue.

%% @private
mk_user_error(Error) ->
  Err = convert_error(Error),
  #mce_result{type=user_error,error=Err}.
%% @private
mk_user_error(Error,State) ->
  Err = convert_error(Error),
  #mce_result{type=user_error,state=State,error=Err}.
%% @private
mk_user_error(Error,State,Stack,Conf) ->
  Err = convert_error(Error),
  #mce_result{type=user_error,state=State,error=Err,stack=Stack,conf=Conf}.
%% @private
mk_internal_error(Error) ->
  Err = convert_error(Error),
  #mce_result{type=internal_error,error=Err}.

mk_mon_error(Error,State,Stack) ->
  Err = convert_error(Error),
  #mce_result{type=mon_error,error=Err,stack=Stack,state=State}.
  
convert_error(Error) ->
  case Error of 
    {exception,_} -> Error;
    {value,_} -> Error;
    _ -> mk_value_error(Error)
  end.

verify_error({exception,_}) ->
  ok;
verify_error({value,_}) ->
  ok;
verify_error(Other) ->
  io:format("*** Error: mce_result:verify_error(~p)~n",[Other]),
  throw(verify_error).

%% @private
throw_result_exc(Result) when is_record(Result,mce_result) ->
  throw({result_exc,Result}).
is_result_exc(throw,{result_exc,Result}) when is_record(Result,mce_result) ->
  true;
is_result_exc(_,_) ->
  false.
result_from_exc(_Exc,{result_exc,Result}) ->
  Result.

reductions(Arg) ->
  Arg#mce_result.reductions.
runtime(Arg) ->
  Arg#mce_result.runtime.
wallclock(Arg) ->
  Arg#mce_result.wallclock.

add_stored_states(StoredStates,Arg) ->
  Arg#mce_result{stored_states=StoredStates}.
add_explored_states(ExploredStates,Arg) ->
  Arg#mce_result{explored_states=ExploredStates}.
stored_states(Arg) ->
  Arg#mce_result.stored_states.
explored_states(Arg) ->
  Arg#mce_result.explored_states.


