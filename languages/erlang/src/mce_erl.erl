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

%% @author Lars-Ake Fredlund, Hans Svensson
%% @copyright 2006-2009 Lars-Ake Fredlund
%% @doc

%% @type state(). A record representing the state of the system (dictionary,nodes,links). Specified in file state.hrl.
%% @type process(). A record representing a system process (status,pid, flags, ...). Specified in file process.hrl.

-module(mce_erl).

-export([innermost/1,
	 recv/1,recv_timeout/3,recv_timeout/1,recv_fun/1,is_recv/1,
	 choice/1,choice/2,choice_alternatives/1,is_choice/1,
	 pause/1,pause/2,pause_fun/1,pause_label/1,is_pause/1,
	 send_sef/1,send_sef/2,send_sef_fun/1,send_sef_label/1,is_send_sef/1,
	 exiting/1,is_exiting/1,exiting_reason/1,
         letexpr/2,letexpr_fun/1,letexpr_expr/1,is_letexpr/1,
	 tryexpr/3,tryexpr_arg/1,tryexpr_body/1,tryexpr_handler/1,
	 is_tryexpr/1,
	 is_context/1,mk_context/2,context_inner/1,context/1]).
-export([match_recv_timeout/1,match_recv_fun/1,
	 match_choice/1,match_choice_label/1,
	 match_letexpr_fun/1,match_letexpr_expr/1,
	 match_tryexpr_arg/1,match_tryexpr_body/1,match_tryexpr_handler/1,
	 match_pause_label/1,match_pause_fun/1,
	 match_send_sef_label/1,match_send_sef_fun/1,
	 match_context_inner/1,match_context/1]).
-export([probe/1,probe/2,is_probe/1,probe_label/1,match_probe_label/1]).
-export([callStack/1]).
-export([safe_call/4]).
-export([void_state/1]).
-export([compiled_with_mcerlang/1]).
-export([mcerlang_compiled_caller/1]).

-export([probe_state/1,probe_state/2,del_probe_state/1,get_probe_state/1]).
-export([has_probe_state/2,get_probe_state/2]).

-export([allProcesses/1]).
-export([apply/3]).

%%-define(debug,true).

-include("../../../src/include/macros.hrl").
-include("emacros.hrl").

-include("state.hrl").
-include("node.hrl").
-include("system.hrl").

-define(IN_MCE,
		begin
			State = erlang:get(globalState),
			if
				is_record(State,system) ->
					true;
				true ->
					false
			end
		end).

-define(call_fun(MCE,ERL),
		case ?IN_MCE of
			true  -> MCE;
			false -> ERL
		end).

%% @doc Creates a function reception statement structure, with infinite timeout, from the parameter function. <br/>
%% Example:<br/><tab/>1><b> recv(fun(Y)->[Y,Y] end). </b><br/>
%%  {'_recv_',{#Fun<erl_eval.6.13229925/>,{infinity,true}}}
%% @end
%% @spec (fun())->{term(),{fun(),{term(),term()}}}
recv(Fun) -> {?RECVTAG,{Fun,{infinity,true}}}.


%% @doc Creates a function reception statement structure whose function, timeout and command come from the parameters. <br/>
%% Example: <br/>1> <b>mce_erl:recv_timeout(fun(X)->[X] end,1000,fun()->exit()end).</b> <br/> {'_recv_',{#Fun<erl_eval.6.13229925/>,
%%        {1000,#Fun<erl_eval.20.67289768/>}}}
%% @end
%% @spec (fun(), any(), any())->{term(),{fun(),{any(),any()}}}
recv_timeout(Fun,TimeoutTime,Command) -> {?RECVTAG,{Fun,{TimeoutTime,Command}}}.

%% @doc Retrieves the function contained in a function reception statement structure <br/>
%% Example:<br/> 1> <b>mce_erl:recv_fun({'_recv_',{#Fun<erl_eval.6.13229925/>, {1000,#Fun<erl_eval.20.67289768/>}}}).</b> <br/>
%%  #Fun<erl_eval.6.13229925/>
%%      
%% @end
%% @spec (any())-> fun()
recv_fun({?RECVTAG,{Fun,_}}) -> Fun.

%% @doc Retrieves the timeout contained in a function reception statement structure. <br/>
%% Example:<br/> 1> <b>mce_erl:recv_timeout({'_recv_',{#Fun<erl_eval.6.13229925/>, {infinity,true}}}).</b> <br/>
%%  {infinity,true}
%%      
%% @end
%% @spec (any())-> {any(), any()}
recv_timeout({?RECVTAG,{_,Timeout}}) -> Timeout.

%% @doc Checks if the parameter statement structure  is a reception statement structure. <br/>
%% Example:<br/> 1> <b>mce_erl:is_recv({'_recv_',{#Fun, {infinity,true}}}).</b> <br/>
%%  true<br/>
%% <b> 2> mce_erl:is_recv({'_try_',_}).</b><br/>
%% false     
%% @end
%% @spec (any())-> bool()
is_recv({?RECVTAG,{_,_}}) -> true;
is_recv(_) -> false.

%% @doc Checks whether the parameter is a function reception statement construct and, if it is, returns its timeout value.<br/>
%% Example:<br/> 1> <b>mce_erl:match:recv_timeout({'_recv_',{#Fun, {infinity,true}}}).</b> <br/>
%%  {true, {infinity, true}}
%%   <br/> 2> <b>mce_erl:match:recv_timeout({'_try_', _}).</b> <br/>
%%  false   
%% @end
%% @spec (any())-> {true, {any(),any()}} | false
match_recv_timeout(E) -> match(E, fun is_recv/1, fun recv_timeout/1).

%% @doc Checks whether the parameter is a function reception statement construct and, if it is, returns that function.<br/>
%% Example:<br/> 1> <b>mce_erl:match:recv_fun({'_recv_',{#Fun, {infinity,true}}}).</b> <br/>
%%  {true, #Fun}
%%   <br/> 2> <b>mce_erl:match:recv_timeout({'_try_', _}).</b> <br/>
%%  false   
%% @end
%% @spec (any())-> {true, fun()} | false
match_recv_fun(E) -> match(E, fun is_recv/1, fun recv_fun/1).




%% @doc Creates a choice statement structure, with 'void' label, from the parameter function clauses. <br/>
%% @end
%% @spec ([any()])->{term(),{void,[any()]}}
choice(FunClauses) -> choice(void,FunClauses).

%% @doc Creates a choice statement structure with the parameter label and function clauses. <br/>
%% @end
%% @spec (term(),[any()])->{term(),{term(),[any()]}}
choice(Label,FunClauses) -> {?CHOICETAG,{Label,FunClauses}}.

%% @doc Retrieves the function clauses contained in a choice statement structure. <br/>
%% @end
%% @spec ({term(),{any(),[any]}})-> [any()]
choice_alternatives({?CHOICETAG,{_,FunClauses}}) -> FunClauses.

%% @doc Retrieves the label contained in a choice statement structure <br/>
%% @end.
%% @spec ({term(),{term(),[any]}})-> term()
choice_label({?CHOICETAG,{Label,_}}) -> Label.

%% @doc Checks if the parameter is a choice statement construct. <br/>
%% Example:<br/> 1> <b>mce_erl:is_choice({'_choice_',any()}).</b> <br/>
%%  true<br/>
%% <b> 2> mce_erl:is_choice({'_recv_',_}).</b><br/>
%% false     
%% @end
%% @spec ({term(), any()})-> bool()
is_choice({?CHOICETAG,_}) -> true;
is_choice(_) -> false.

%% @doc Checks whether the parameter is a choice statement construct and, if it is, returns its function clauses.<br/>
%% @end
%% @spec (any())-> {true, [any()]} | false
match_choice(E) -> match(E, fun is_choice/1, fun choice_alternatives/1).

%% @doc Checks whether the parameter is a choice statement construct and, if it is, returns its function clauses.<br/>
%% @end
%% @spec (any())-> {true, term()} | false
match_choice_label(E) -> match(E, fun is_choice/1, fun choice_label/1).





%% @doc Creates a choice statement structure, with 'void' label, from the parameter function clause. <br/>
%% @end
%% @spec (any())->{term(),{void,[any()]}}
pause(Fun) -> choice([Fun]).

%% @doc Creates a choice statement structure with the parameter label and function clause. <br/>
%% @end
%% @spec (term(),any())->{term(),{term(),[any()]}}
pause(Label,Fun = {M,F,A}) -> 
	?call_fun(
	   choice(Label,[Fun]),
	   erlang:apply(M,F,A)
	  ).	   

%% @doc Retrieves the only function clause contained in a choice statement structure. <br/>
%% @end
%% @spec ({term(),{any(),[any]}})-> any()
pause_fun(E) -> [Fun] = choice_alternatives(E), Fun.

%% @doc Retrieves the label contained in a choice statement structure <br/>
%% @end.
%% @spec ({term(),{term(),[any]}})-> term()
pause_label(E) -> choice_label(E).

%% @doc Checks if the parameter is a choice statement construct with only one function clause. <br/>
%% Example:<br/> 1> <b>mce_erl:is_pause({'_choice_',[any()]}).</b> <br/>
%%  true<br/>
%% <b> 2> mce_erl:is_choice({'_recv_',[any(),any()]}).</b><br/>
%% false     
%% @end
%% @spec ({term(), any()})-> bool()
is_pause(E) -> is_choice(E) andalso length(choice_alternatives(E))==1.

match_pause_label(E) -> match(E,fun is_pause/1, fun pause_label/1).
match_pause_fun(E) -> match(E, fun is_pause/1, fun pause_fun/1).


%% @doc Creates a send statement structure, with 'void' label, from the parameter function clause. <br/>
%% @end
%% @spec (any())->{term(),{void,any()}}
send_sef(Fun) -> {?SENDTAG,{void,Fun}}.

%% @doc Creates a send statement structure with the parameter label and function clause. <br/>
%% @end
%% @spec (term(),any())->{term(),{term(),any()}}
send_sef(Label,Fun = {M,F,A}) -> 
	?call_fun(
	   {?SENDTAG,{Label,Fun}},
	   erlang:apply(M,F,A)
	  ).	   

%% @doc Retrieves the only function clause contained in a choice statement structure. <br/>
%% @end
%% @spec ({term(),{any(),any}})-> any()
send_sef_fun({?SENDTAG,{_,Fun}}) -> Fun.

%% @doc Retrieves the label contained in a choice statement structure <br/>
%% @end.
%% @spec ({term(),{term(),[any]}})-> term()
send_sef_label({?SENDTAG,{Label,_}}) -> Label.

%% @doc Checks if the parameter is a send statement. <br/>
%% Example:<br/> 1> <b>mce_erl:is_send_sef({'_choice_',[any()]}).</b> <br/>
%%  true<br/>
%% <b> 2> mce_erl:is_choice({'_recv_',[any(),any()]}).</b><br/>
%% false     
%% @end
%% @spec ({term(), any()})-> bool()
is_send_sef({?SENDTAG,_}) -> true;
is_send_sef(_) -> false.

match_send_sef_label(E) -> match(E,fun is_send_sef/1, fun send_sef_label/1).
match_send_sef_fun(E) -> match(E, fun is_send_sef/1, fun send_sef_fun/1).

exiting(Reason) -> {?EXITINGTAG,Reason}.

is_exiting({?EXITINGTAG,_}) -> true;
is_exiting(_) -> false.

exiting_reason({?EXITINGTAG,Reason}) ->
  Reason.

letexpr(Expr,Fun) -> {?LETTAG,{Expr,Fun}}.
letexpr_fun({?LETTAG,{_,Fun}}) -> Fun.
letexpr_expr({?LETTAG,{Expr,_}}) -> Expr.
is_letexpr({?LETTAG,_}) -> true;
is_letexpr(_) -> false.
match_letexpr_fun(E) -> match(E,fun is_letexpr/1, fun letexpr_fun/1).
match_letexpr_expr(E) -> match(E, fun is_letexpr/1, fun letexpr_expr/1).

tryexpr(Arg,Body,Handler) -> {?TRYTAG,{Arg,{Body,Handler}}}.
tryexpr_arg({?TRYTAG,{Arg,_}}) -> Arg.
tryexpr_body({?TRYTAG,{_,{Body,_}}}) -> Body.
tryexpr_handler({?TRYTAG,{_,{_,Handler}}}) -> Handler.
is_tryexpr({?TRYTAG,_}) -> true;
is_tryexpr(_) -> false.
match_tryexpr_arg(E) -> match(E,fun is_tryexpr/1, fun tryexpr_arg/1).
match_tryexpr_body(E) -> match(E, fun is_tryexpr/1, fun tryexpr_body/1).
match_tryexpr_handler(E) -> match(E, fun is_tryexpr/1, fun tryexpr_handler/1).

probe(Label) -> 
  mce_erl_actions:record(mce_erl_actions:mk_probe(mcerlang:self(),Label)).
probe(Label,Term) ->
  mce_erl_actions:record(mce_erl_actions:mk_probe(mcerlang:self(),Label,Term)).
probe_state(Label) ->
  mcerlang:gput({probe_state,Label},void).
probe_state(Label,Term) ->
  mcerlang:gput({probe_state,Label},Term).
del_probe_state(Label) ->
  mcerlang:gerase({probe_state,Label}).
get_probe_state(Label) ->
  mcerlang:gget({probe_state,Label}).
  

is_probe(Action) ->
  mce_erl_actions:is_probe(Action).
probe_label(Action) ->
  mce_erl_actions:get_probe_label(Action).
match_probe_label(A) ->
  match(A,fun is_probe/1,fun probe_label/1).

has_probe_state(Label,State) ->
  KeyValues = State#state.dict,
  SearchLabel = {probe_state,Label},
  case mce_utils:find(fun ({K, _V}) -> SearchLabel =:= K end, KeyValues) of
    {ok, {_, _Value}} ->
      true;
    _ ->
      false
  end.
get_probe_state(Label,State) ->
  KeyValues = State#state.dict,
  SearchLabel = {probe_state,Label},
  {ok, {_, Value}} =
    mce_utils:find(fun ({K, _V}) -> SearchLabel =:= K end, KeyValues),
  Value.

innermost({?CONTEXTTAG,{Innermost,_}}) -> Innermost;
innermost(Other) -> Other.

%% @doc Returns all the processes contained in certain system state.
%% @end 
%% @spec (state())-> [process()]
allProcesses(State) ->
  lists:flatmap(fun (Node) -> Node#node.processes end,State#state.nodes).
  
match(E,TestFun,DestFun) ->
  case TestFun(E) of
    true -> {true,DestFun(E)};
    false -> false
  end.
      
apply(Module,Fun,Args) ->
  erlang:apply(Module,Fun,Args).

callStack({?CONTEXTTAG, {Innermost, Outer}}) ->
    digOutCalls(Outer) ++ digOutCalls([Innermost]);
callStack(Other) ->
    digOutCalls([Other]).

digOutCalls([]) -> [];
digOutCalls([Call|Rest]) ->
  case Call of
    F={_Module,_Fun,_Args} ->
      [F|digOutCalls(Rest)];
    F={_Fun,_Args} ->
      [F|digOutCalls(Rest)];
    _ ->
      digOutCalls(Rest)
  end.
			  
mk_context(Innermost,[]) ->
  Innermost;
mk_context(Innermost,Context) ->
  {?CONTEXTTAG,{Innermost,Context}}.
context_inner({?CONTEXTTAG,{Inner,_}}) -> Inner.
context({?CONTEXTTAG,{_,Context}}) -> Context.
is_context({?CONTEXTTAG,{_,_}}) -> true;
is_context(_) -> false.
match_context_inner(E) -> match(E, fun is_context/1, fun context_inner/1).
match_context(E) -> match(E, fun is_context/1, fun context/1).
    
%% safe_call(Module,Name,Args,Arity) ->  
%%   {NewModule,NewName} = mce_erl_compile_info:map_fun_arity(Module,Name,Arity),
%%   erlang:apply(NewModule,NewName,Args).

safe_call(Module,Name,Args,Arity) ->
   try mce_erl_compile_info:map_fun_arity(Module,Name,Arity) of
           {NewModule,NewName} ->
                   erlang:apply(NewModule,NewName,Args)
   catch _:_ ->
                   erlang:apply(Module,Name,Args)
   end.
  
%% @private
void_state(State) ->
  case State#state.nodes of
    [Node|_] ->
      State#state
	{nodes=
	 [Node#node
	  {processes=[],registered=[],monitors=[],node_monitors=[],links=[]}]};
    _ ->
      void
  end.

compiled_with_mcerlang(ModuleName) ->
  try ModuleName:mce_erl_real_name(void) of _ -> true
  catch _:_ -> 
      try ModuleName:module_info() of _ -> false
      catch _:_ ->
	  io:format
	    ("*** Warning: module ~p not loaded; "++
	     "compiled_with_mcerlang returns false~n",
	     [ModuleName]),
	  false
      end
  end.

mcerlang_compiled_caller(ModuleSelf) ->
  StackTrace = try throw(bad) catch _ -> erlang:get_stacktrace() end,
  case caller_module(StackTrace,ModuleSelf) of
    void -> false;
    Module ->
      Result = compiled_with_mcerlang(Module),
      Result
  end.

caller_module([],_) ->
  void;
caller_module([{mce_erl,_,_}|Rest],ModuleSelf) ->
  caller_module(Rest,ModuleSelf);
caller_module([{ModuleSelf,_,_}|Rest],ModuleSelf) ->
  caller_module(Rest,ModuleSelf);
caller_module([{Module,_,_}|_],_) ->
  Module.
      
      
      
