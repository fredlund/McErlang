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

-module(mce_erl_sef_analysis).

-export([analyze/2,has_sef/3,call_has_snd_for_sure/2]).
-export([module_calls/1,module_calls_modules/1]).

%%-define(debug,true).
-include("../../../src/include/macros.hrl").
-include("emacros.hrl").

-record(info_rec,{name,options=[],inherits=[]}).
-include("compile_rec.hrl").

module_calls(CoreCode) ->
  sets:to_list(calls(CoreCode)).

module_calls_modules(CoreCode) ->
  FunctionCalls = module_calls(CoreCode),
  sets:to_list
    (lists:foldl
     (fun ({Module,_,_},Result) ->
	  sets:add_element(Module,Result)
      end, sets:new(), FunctionCalls)).

calls(CoreCode) ->
  ModuleName = cerl:atom_val(cerl:module_name(CoreCode)),
  cerl_trees:fold
    (fun (C,Result) ->
	 case cerl:type(C) of
	   'apply' ->
	     Operator = cerl:apply_op(C),
	     case cerl:is_c_fname(Operator) of
	       true ->
		 sets:add_element
		   ({ModuleName,
		     cerl:fname_id(Operator),
		     cerl:fname_arity(Operator)},Result);
	       false -> Result
	     end;
	   'call' ->
	     Module = cerl:call_module(C),
	     Name = cerl:call_name(C),
	     case cerl:is_c_atom(Module) of
	       true ->
		 sets:add_element
		   ({cerl:atom_val(Module),
		     try_to_get_atom(Name),
		     cerl:call_arity(C)},
		    Result);
	       _ -> Result
	     end;
	   _ -> Result
	 end
     end, sets:new(), CoreCode).
     
try_to_get_atom(Term) ->
  case cerl:is_c_atom(Term) of
    true -> cerl:atom_val(Term);
    false -> '_void_'
  end.

analyze(CoreModules, CR) when is_record(CR, compile_rec) ->
  AllModulesCalls =
    lists:foldl
      (fun
	 ({_ModuleName, _, Core}, Functions) ->
	   init_sef_analysis(Core) ++ Functions
       end,
       [], CoreModules),
  Analysis =
    compute_sef_functions(AllModulesCalls, CR),
  ?LOG("Sef Analysis Result: ~n", []),
  %%print_funcalls(Analysis),
  ?LOG("~n~n", []),
  Analysis.

init_sef_analysis(CoreCode) ->
  ModuleName = cerl:atom_val(cerl:module_name(CoreCode)),
  Defs = cerl:module_defs(CoreCode),
  lists:map
    (fun ({Name,Fun}) ->
	 FullName={ModuleName,cerl:fname_id(Name),cerl:fname_arity(Name)},
	 ?LOG("Checking calls for function ~p~n",[FullName]),
	 Body = cerl:fun_body(Fun),
	 Calls = sets:to_list(init_core(ModuleName,Body)),
	 ?LOG("Calls are ~p~n",[Calls]),
	 {FullName,Calls}
     end, Defs).

init_core(ModuleName,C) ->
  case cerl:type(C) of
    'case' ->
      sets:union(init_core(ModuleName,cerl:case_arg(C)),
		 init_core_clause_calls(ModuleName,cerl:case_clauses(C)));
    'receive' ->
      sets:add_element(sef,sets:new());
    'let' ->
      sets:union
	(init_core(ModuleName,cerl:let_body(C)),
	 init_core(ModuleName,cerl:let_arg(C)));
    'try' ->
      sets:add_element(sef,sets:new());
    'apply' ->
      Operator = cerl:apply_op(C),
      case cerl:is_c_fname(Operator) of
	true ->
	  sets:add_element
	    ({ModuleName,
	      cerl:fname_id(Operator),
	      cerl:fname_arity(Operator)},sets:new());
	false ->
	  ?LOG("sef_analysis: WARNING: don't understand apply ~s==~p~n",
		     [pp(Operator),Operator]),
	  sets:add_element(sef,sets:new())
      end;
    'call' ->
      Module = cerl:call_module(C),
      Name = cerl:call_name(C),
      case {cerl:is_c_atom(Module), cerl:is_c_atom(Name)} of
	{true,true} ->
	  sets:add_element
	    ({cerl:atom_val(Module),cerl:atom_val(Name),cerl:call_arity(C)},
	     sets:new());
	_ ->
	  sets:add_element(sef,sets:new())
      end;
    'primop' ->
      sets:add_element
	({primop,cerl:atom_val(cerl:primop_name(C)),cerl:primop_arity(C)},
	 sets:new());
    'letrec' ->
      lists:foldl
	(fun ({_,F},S) ->
	     B = cerl:fun_body(F),
	     sets:union(init_core(ModuleName,B),S)
	 end, init_core(ModuleName,cerl:letrec_body(C)),
	 cerl:letrec_defs(C));
    _ ->
      sets:new()
  end.

init_core_clause_calls(ModuleName,Clauses) ->
  lists:foldl
    (fun (Clause,Calls) ->
	 sets:union(init_core(ModuleName,cerl:clause_body(Clause)), Calls)
     end, sets:new(),Clauses).

compute_sef_functions(AllCalls,CR) ->
  lfp(compute_init_funcalls(AllCalls,CR)).

lfp(FunCalls0_l) ->
  FunCalls0 = orddict:from_list(FunCalls0_l),
  FunCalls1_l = compute_new_funcalls(FunCalls0_l,FunCalls0),
  case compare(FunCalls0_l,FunCalls1_l) of
    true ->
      FunCalls0;
    false ->
      lfp(FunCalls1_l)
  end.

print_funcalls(L) ->
  lists:foreach(fun ({F,Calls}) -> io:format("~p: ~p~n",[F,Calls]) end, L).

compare(X,Y) -> X==Y.

compute_init_funcalls(FunCalls,CR) ->
  Initial = 
    lists:map
      (fun ({Function,Calls}) ->
	   {Function,collapse_sefs(check_init_calls(Calls,CR))}
       end,
       FunCalls),
  orddict:from_list(Initial).

check_init_calls(sef, _CR) -> sef;
check_init_calls(Calls, CR) ->
  lists:map
    (fun (sef) -> sef;
	 (Call={Module, Function, Arity}) ->
	 case check_for_direct_sef
	   (mce_erl_compile_info:get_function
	    (Module, Function, Arity, CR#compile_rec.compile_info),
	    CR) of
	   true -> sef;
	   false -> Call
	 end
     end, Calls).

compute_new_funcalls(FunCalls_l,FunCalls) ->
  lists:map
    (fun ({Function,sef}) -> {Function,sef};
	 ({Function,Calls}) ->
	 {Function,
	  collapse_sefs
	  (lists:map
	   (fun (Call) -> compute_new_call(Call,FunCalls) end,Calls))}
     end, FunCalls_l).

compute_new_call(Call,FunCall) ->
  case orddict:find(Call,FunCall) of
    {ok,sef} -> sef;
    _ -> Call
  end.

collapse_sefs(sef) -> sef;
collapse_sefs(Calls) ->
  lists:foldr
    (fun (Call,Result) ->
	 if
	   Call==sef -> sef;
	   Result==sef -> sef;
	   true -> [Call|Result]
	 end
     end, [], Calls).

check_for_direct_sef(error,_CR) ->
  false;
check_for_direct_sef({ok,I},CR) ->
  Options = I#info_rec.options,
  Has_rcv_sef =
    case orddict:find(rcv,Options) of
      {ok,Brcv} -> Brcv;
      error -> false
    end,
  Has_snd_sef =
    case orddict:find(snd,Options) of
      {ok,Bsnd} -> Bsnd and CR#compile_rec.sends_are_sefs;
      error -> false
    end,
  Has_rcv_sef or Has_snd_sef.

check_for_snd(error,_CR) -> false;
check_for_snd({ok,I},CR) ->
  Options = I#info_rec.options,
  Has_snd_sef =
    case orddict:find(snd,Options) of
      {ok,Bsnd} -> Bsnd and CR#compile_rec.sends_are_sefs;
      error -> false
    end,
  Has_snd_sef.

unknown_sef_resolve(CR) when is_record(CR,compile_rec) ->
  if CR#compile_rec.unknown_is_rcv ->
      ?LOG("~p is unknown, converting to sef~n", [Call]),
      true;
     CR#compile_rec.unknown_is_snd, CR#compile_rec.sends_are_sefs ->
      ?LOG("~p is unknown, converting to sef~n", [Call]),
      true;
     true ->
      false
  end.

pp(C) ->  
  try core_pp:format(cerl:to_records(C))
  catch Error:Reason ->
      io:format
	("*** invalid cerl term:~n  ~p~nError ~p:~p***~n",[C,Error,Reason]),
      throw(pp)
  end.

has_sef(ModuleName,C,CR) ->
  %% May get called without a proper side effect analysis
  CRN = 
    case CR#compile_rec.sef_analysis of
      void -> CR#compile_rec{sef_analysis=[]};
      _ -> CR
    end,
  case cerl:type(C) of
    'case' ->
      clauses_has_sef(ModuleName,cerl:case_clauses(C),CRN);
    'receive' ->
      true;
    'let' ->
      has_sef(ModuleName,cerl:let_body(C),CRN) or
	has_sef(ModuleName,cerl:let_arg(C),CRN);
    'letrec' ->
      has_sef
	(ModuleName,cerl:letrec_body(C),CRN) or
	lists:any
	  (fun ({_,F}) ->
	       has_sef(ModuleName,cerl:fun_body(F),CRN) 
	   end,
	   cerl:letrec_defs(C));
    'try' ->
      true;
    'apply' ->
      Operator = cerl:apply_op(C),
      case cerl:is_c_fname(Operator) of
	true ->
	  call_has_sef
	    ({ModuleName,
	      cerl:fname_id(Operator),
	      cerl:fname_arity(Operator)},CRN);
	false ->
	  ?LOG("sef_analysis: WARNING: don't understand apply ~s==~p~n",
	       [pp(Operator),Operator]),
	  true
      end;
    'call' ->
      Module = cerl:call_module(C),
      Name = cerl:call_name(C),
      case {cerl:is_c_atom(Module), cerl:is_c_atom(Name)} of
	{true,true} ->
	  call_has_sef
	    ({cerl:atom_val(Module),cerl:atom_val(Name),cerl:call_arity(C)},
	     CRN);
	_ ->
	  true
      end;
    'primop' ->
      call_has_sef
	({primop,cerl:atom_val(cerl:primop_name(C)),cerl:primop_arity(C)},
	 CRN);
    _ ->
      false
  end.

clauses_has_sef(ModuleName,Clauses,CR) ->
  lists:foldl
    (fun (Clause,Return) ->
	 Return or has_sef(ModuleName,cerl:clause_body(Clause),CR)
     end, false, Clauses).

call_has_sef(Call = {Module, Function, Arity}, CR) ->
  Dict = CR#compile_rec.sef_analysis,
  case orddict:find(Call, Dict) of
    {ok, Calls} ->
      Calls == sef;
    error ->
      case mce_erl_compile_info:get_function
	(Module, Function, Arity, CR#compile_rec.compile_info) of
	error -> unknown_sef_resolve(CR);
	I ->
	  ?LOG("Call ~p has information ~p~n", [Call, I]),
	  check_for_direct_sef(I, CR)
      end
  end.

call_has_snd_for_sure(Call = {Module, Function, Arity}, CR) ->
  check_for_snd
    (mce_erl_compile_info:get_function
     (Module, Function, Arity, CR#compile_rec.compile_info),
     CR).
  
