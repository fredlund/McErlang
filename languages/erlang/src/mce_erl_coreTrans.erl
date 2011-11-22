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

-module(mce_erl_coreTrans).

-export([normalize_core/1,translate_names/2,safe_calls/2,compile/2,pp/1,
	 check_core_erlang/1,changeGuards/1]).

-export([compile_core/1]).

%%-export([read/1,transform/2,output/2,pp_core/1,pp/2,pp_from/2,pp/1]).
%%-export([fix_guards/1,changeGuards/1,normalize_module/1,normalize_file/1,
%%	 normalize_core_file/1,remove_catch_seq/1,changeReceive/2]).
%%
%%-export([compile_normalize_file/1,compile_normalize_core_file/1]).
%%
%%-export([translate/2]).

%%-define(debug,true).
-include("../../../src/include/macros.hrl").
-include("emacros.hrl").

-include("compile_rec.hrl").

normalize_core(CoreCode) ->
    ModuleName = cerl:atom_val(cerl:module_name(CoreCode)),
    ?LOG("~nCore code for module ~p:~n~n", [ModuleName]),
    ?LOG("~s~n", [pp(CoreCode)]),
    C2 = remove_catch_seq(CoreCode),
    ?LOG("~nCore code for module ~p after simplify:~n~n", [ModuleName]),
    ?LOG("~s~n", [pp(C2)]),
    NC = normalize_module(C2),
    ?LOG("~nCore code for module ~p after normalize:~n~n", [ModuleName]),
    ?LOG("~s~n", [pp(NC)]),
    NormalizedCore = strip_annotations(NC),
    try
      check_reduced_core_module(NormalizedCore)
    catch
      core_lint ->
	  io:format("~nCore code for normalized module ~p:~n~n", [ModuleName]),
	  io:format("~s~n", [pp(NormalizedCore)]),
	  throw(compile_error)
    end,
    {ModuleName, normalized, NormalizedCore}.

compile(Cores, CR) when is_list(Cores) ->
  OutputDir = filename:absname(CR#compile_rec.output_dir)++"/",
  not_void(OutputDir, "output directory not known"),
  case filelib:ensure_dir(OutputDir) of
    ok -> ok;
    {error,Reason} ->
      io:format
	("*** Error: directory ~p for compiled files could not "++
	   "be created due to ~p~n",[OutputDir,Reason]),
      throw(bad)
  end,
  lists:foreach
    (fun ({ModuleName, CoreType, CoreModule}) ->
	 OutputFileName = OutputDir ++ "/" ++ atom_to_list(ModuleName),
	 CoreOutputFileName =
	   OutputDir ++ "/" ++ atom_to_list(ModuleName) ++ ".core",
	 OutputCore =
	   case CoreType of
	     erlang ->
	       print_if_or_log
		 (CR#compile_rec.verbose,
		  "Not translating erlang module ~p~n", [ModuleName]),
	       CoreModule;
	     normalized ->
	       print_if_or_log
		 (CR#compile_rec.verbose,
		  "Translating erlang McErlang module ~p~n", [ModuleName]),
	       ReceiveCore = changeReceive(CoreModule, CR),
	       case CR#compile_rec.normalize_core of
		 true ->
		   {_,_,NormalizedCore} = normalize_core(ReceiveCore),
		   NormalizedCore;
		 false ->
		   ReceiveCore
	       end
	   end,
	 try check_core_erlang(OutputCore)
	 catch
	   core_lint ->
	     io:format("~n*** Error: Core code output ~p:~n~n", [ModuleName]),
	     io:format("~s~n", [pp(OutputCore)]),
	     throw(compile_error)
	 end,
	 case file:open(CoreOutputFileName, [write]) of
	   {ok, CoreOutputFile} ->
	     io:format(CoreOutputFile, "~s~n", [pp(OutputCore)]),
	     ok = file:close(CoreOutputFile);
	   Error ->
	     io:format
	       ("*** Warning: failure ~p opening core file ~p for writing~n",
		[Error, CoreOutputFileName])
	 end,
	 output(cerl:to_records(OutputCore), OutputFileName)
     end, Cores).

not_void(void,Msg) ->
  io:format("*** Error: ~s~n",[Msg]),
  throw(compile);
not_void(_,_) ->
  ok.

output(CoreCode,File) ->
  FileName =
    if
      is_atom(File) -> atom_to_list(File)++".beam";
      is_list(File) -> File++".beam"
    end,
  case compile:forms(CoreCode,[from_core,return_errors,debug_info]) of
    {ok,_,Bin} ->
      ok = file:write_file(FileName,Bin);
    error ->
      io:format
	("*** Error: unable to generate beam code from HiPE for ~s~n",
	 [FileName]),
      io:format("Core code:~n"),
      io:format("~s~n",[core_pp:format(CoreCode)]),
      throw(compile);
    {error,Errors,_Warnings} ->
      io:format
	("*** Error: unable to generate beam code from HiPE for ~s~n",
	 [FileName]),
      [{_FileName,[ErrorInfo]}] = Errors,
      {_ErrorLine,Module,ErrorDescriptor} = ErrorInfo,
      ErrorString = apply(Module, format_error, [ErrorDescriptor]),
      io:format("*** Error: ~s ***~n",[ErrorString]),
      io:format("~nCore code:~n~n"),
      io:format("~s~n",[core_pp:format(CoreCode)]),
      throw(compile)
  end.
  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Transformation that transform catch and seq terms
%%% into the more "basic" forms try and let

remove_catch_seq(CoreCode) ->
  cerl_trees:map
    (fun (C) ->
	 case cerl:type(C) of
	   'seq' ->
	     Arg = cerl:seq_arg(C),
	     Body = cerl:seq_body(C),
	     Degree = degree(Arg),
	     Vars = genVars(Degree),
	     cerl:c_let(Vars,Arg,Body);
	   'catch' ->
	     E = cerl:catch_body(C),
	     Degree = degree(E),
	     Vars = genVars(Degree),
	     V1 = genVar(),
	     V2 = genVar(),
	     V3 = genVar(),
	     cerl:c_try
	       (E,
		Vars,
		cerl:c_values(Vars),
		[V1,V2,V3],
		cerl:c_case
		(V1,
		 [cerl:c_clause([cerl:c_atom(throw)],V2),
		  cerl:c_clause([cerl:c_atom(exit)],
				cerl:c_tuple([cerl:c_atom('EXIT'),V2])),
		  cerl:c_clause([cerl:c_atom(error)],
				cerl:c_tuple([cerl:c_atom('EXIT'),
					      cerl:c_tuple
					      ([V2,
						cerl:c_nil()])]))]));
	   _ -> C
	 end
     end, CoreCode).

strip_annotations(CoreCode) ->
  cerl_trees:map
    (fun (C) ->
	 Annotations =
	   lists:foldr
	     (fun (Annotation,Collected) ->
		  if
		    is_integer(Annotation) ->
		      [Annotation|Collected];
		    true ->
		      Collected
		  end
	      end, [], cerl:get_ann(C)),
	 cerl:set_ann(C,Annotations)
     end, CoreCode).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

degree(E) ->
  case cerl:type(E) of
    'values' ->
      cerl:values_arity(E);
    'let' ->
      degree(cerl:let_body(E));
    'seq' ->
      degree(cerl:seq_body(E));
    'case' ->
      case cerl:case_clauses(E) of
	[Clause|_] ->
	  degree(cerl:clause_body(Clause));
	[] ->
	  io:format("clause_body of degree~n  ~p~n",[E]),
	  exit(bad_clause)
      end;
    'receive' ->
      case cerl:receive_clauses(E) of
	[Clause|_] ->
	  degree(cerl:clause_body(Clause));
	[] ->
	  degree(cerl:receive_action(E))
      end;
    'letrec' ->
      degree(cerl:letrec_body(E));
    'try' ->
      degree(cerl:try_body(E));
    'catch' ->
      degree(cerl:catch_body(E));
    'primop' ->
      case cerl:atom_val(cerl:primop_name(E)) of
	'match_fail' -> 0;
	'raise' -> 0;
	'bs_context_to_binary' -> 0;
	_ -> 1
      end;
    _ ->
      1
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%% Transformation that replaces problematic guards (self() for instance)
%%% with new function calls (mcerlang:self()) and variable bindings
%%%
changeGuards(CoreCode) ->
  cerl_trees:map
    (fun (C) ->
	 case cerl:type(C) of
	   'case' ->
	     {VarsValues,NewClauses} =
	       changeGuardsInClauses(cerl:case_clauses(C)),
	     lists:foldl
	       (fun ({Var,Value},E) -> cerl:c_let([Var],Value,E) end,
		cerl:c_case(cerl:case_arg(C),NewClauses),
		VarsValues);
	   'receive' ->
	     {VarsValues,NewClauses} =
	       changeGuardsInClauses(cerl:receive_clauses(C)),
	     lists:foldl
	       (fun ({Var,Value},E) -> cerl:c_let([Var],Value,E) end,
		copy_ann
		(C,cerl:c_receive
		 (NewClauses,cerl:receive_timeout(C),cerl:receive_action(C))),
		VarsValues);
	   _ -> C
	 end
     end, CoreCode).

changeGuardsInClauses(Clauses) ->
  lists:foldr
    (fun (Clause,{VarsValues,NewClauses}) ->
	 Guard = cerl:clause_guard(Clause),
	 {NewGuard,{ExtraGuard,GuardsVarsValues}} = translateGuard(Guard),
	 NewClause =
	   cerl:c_clause
	     (cerl:clause_pats(Clause),
	      case is_c_true(ExtraGuard) of
		true -> NewGuard;
		false -> c_and(ExtraGuard,NewGuard)
	      end,
	      cerl:clause_body(Clause)),
	 {GuardsVarsValues++VarsValues,[NewClause|NewClauses]}
     end, {[],[]}, Clauses).

translateGuard(Guard) ->
  cerl_trees:mapfold
    (fun (C,{ExtraGuard,VarsValues}) ->
	 case cerl:type(C) of
	   'call' ->
	     Module = cerl:call_module(C),
	     Name = cerl:call_name(C),
	     case {cerl:is_c_atom(Module), cerl:is_c_atom(Name)} of
	       {true,true} ->
		 Search = {cerl:atom_val(Module),
			   cerl:atom_val(Name),
			   cerl:call_arity(C)},
		 case search_replace_guard_call(C,Search,cerl:call_args(C)) of
		   {replacementCall,{ReplacementModule,ReplacementName,Args}} ->
		     NewVar =
		       genVar(),
		     NewCall =
		       copy_ann
			 (C,
			  cerl:c_call
			  (ReplacementModule,ReplacementName,Args)),
		     {NewVar,{ExtraGuard,[{NewVar,NewCall}|VarsValues]}};
		   {newExpr,Other} -> 
		     {Other,{ExtraGuard,VarsValues}};
		   {newGuard,NewExtraGuard,Other} ->
		      {Other,
		       {case is_c_true(ExtraGuard) of
			  true -> NewExtraGuard;
			  false -> c_and(NewExtraGuard,ExtraGuard)
			end,VarsValues}}
		 end;
	       _ -> {C,{ExtraGuard,VarsValues}}
	     end;
	   _ -> {C,{ExtraGuard,VarsValues}}
	 end
     end, {c_true(),[]}, Guard).

search_replace_guard_call(C,Search,Args) ->
  case Search of
    {'erlang','self',0} ->
      {replacementCall,{cerl:c_atom(mcerlang),cerl:c_atom(self),Args}};
    {'erlang','node',0} -> 
      {replacementCall,{cerl:c_atom(mcerlang),cerl:c_atom(node),Args}};
    {'erlang','is_pid',1} -> 
      [X] = Args,
      {newExpr,c_is_pid(X)};
    {'erlang','is_port',1} -> 
     {newExpr, cerl:c_atom(false)};
    {'erlang','is_reference',1} -> 
      {newExpr,cerl:c_atom(false)};
    {'erlang','node',1} -> 
      [X] = Args,
      {newGuard,c_is_pid(X),c_element(cerl:c_int(2),X)};
    _ ->
      {newExpr,C}
  end.

c_true() ->
  cerl:c_atom(true).
c_false() ->
  cerl:c_atom(false).

is_c_true(X) ->
  case cerl:is_c_atom(X) of
    true -> cerl:atom_val(X)==true;
    false -> false
  end.

c_is_pid(X) ->
  V1 = genVar(),
  V2 = genVar(),
  V3 = genVar(),
  cerl:c_try
    (c_and(c_is_tuple(X),
	   c_and(c_eq(c_size(X),cerl:c_int(3)),
		 c_and(c_eq(c_element(cerl:c_int(1),X),cerl:c_atom(pid)),
		       c_is_integer(c_element(cerl:c_int(3),X))))),
     [V1],
     V1,
     [V2,V3],
     cerl:c_atom(false)).

c_and(X,Y) ->
  cerl:c_call(cerl:c_atom(erlang),cerl:c_atom('and'),[X,Y]).

c_eq(X,Y) ->
  cerl:c_call(cerl:c_atom(erlang),cerl:c_atom('=='),[X,Y]).

c_size(X) ->
  cerl:c_call(cerl:c_atom(erlang),cerl:c_atom(size),[X]).

c_element(N,X) ->
  cerl:c_call(cerl:c_atom(erlang),cerl:c_atom(element),[N,X]).

c_is_tuple(X) ->
  cerl:c_call(cerl:c_atom(erlang),cerl:c_atom(is_tuple),[X]).

c_is_integer(X) ->
  cerl:c_call(cerl:c_atom(erlang),cerl:c_atom(is_integer),[X]).

c_list([]) ->
  cerl:c_nil();
c_list([Hd|Tail]) ->
  cerl:c_cons(Hd,c_list(Tail)).

genVar() ->
  case get(genVar) of
    {num,N} ->
      put(genVar,{num,N+1}),
      cerl:c_var(list_to_atom("_var__"++integer_to_list(N)));
    _ ->
      put(genVar,{num,1}),
      cerl:c_var(list_to_atom("_var__"++integer_to_list(0)))
  end.

genAtom() ->
  case get(genAtom) of
    {num,N} ->
      put(genAtom,{num,N+1}),
      cerl:c_atom("atom__"++integer_to_list(N));
    _ ->
      put(genAtom,{num,1}),
      cerl:c_atom(list_to_atom("atom__"++integer_to_list(0)))
  end.

genVars(N) ->
    mce_utils:repeat(fun (Vars, I) when is_integer(I), I >= 0 ->
			     if I > 0 -> {ok, {[genVar()| Vars], I - 1}};
				true -> {final, Vars}
			     end
		     end, [], N).

pp(C) ->  
  try core_pp:format(cerl:to_records(C))
  catch Error:Reason ->
      io:format
	("*** invalid cerl term:~n  ~p~nError ~p:~p***~n",[C,Error,Reason]),
      throw(pp)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

change_to_proper_vars(Code) ->
  cerl_trees:map
    (fun (C) ->
	 case cerl:type(C) of
	   'module' ->
	     cerl:c_module
	       (cerl:module_name(C),
		cerl:module_exports(C),
		cerl:module_attrs(C),
		change_defs_to_proper_vars(cerl:module_defs(C)));
	   'fun' ->
	     change_fun_to_proper_vars(C);
	   'letrec' ->
	     cerl:c_letrec
	       (change_defs_to_proper_vars(cerl:letrec_defs(C)),
		cerl:letrec_body(C));
	   _ ->
	     C
	 end
     end, Code).

change_defs_to_proper_vars(Defs) ->
  lists:map
    (fun ({Def,Fun}) -> {Def,change_fun_to_proper_vars(Fun)} end, Defs).
				     
change_fun_to_proper_vars(Fun) ->
  Vars = cerl:fun_vars(Fun),
  SubstList = lists:map(fun (Var) -> {cerl:is_c_fname(Var), Var} end, Vars),
  {FunVars,FunBody} =
    lists:foldr
      (fun ({IsImproper,Var},{SubstVars,Body}) -> 
	   if
	     IsImproper ->
	       {FreshVar,NewBody} = fresh_subst(Var,Body),
	       {[FreshVar|SubstVars],NewBody};
	     true ->
	       {[Var|SubstVars],Body}
	   end
       end, {[],cerl:fun_body(Fun)}, SubstList),
  cerl:c_fun(FunVars,FunBody).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Transform that changes names in calls and applies

translate_names(Code, CompileInfo) ->
    ModuleName = cerl:atom_val(cerl:module_name(Code)),
    Defs = cerl:module_defs(Code),
    ?LOG("Translating names for module ~p~n", [ModuleName]),
    cerl_trees:map
      (fun (C) ->
	   case cerl:type(C) of
	     apply ->
	       Operator = cerl:apply_op(C),
	       case cerl:is_c_fname(Operator) of
		 true ->
		   case mce_erl_compile_info:func_mapping
		     (ModuleName,
		      cerl:fname_id(Operator),
		      cerl:fname_arity(Operator),
		      CompileInfo) of
		     {ok,{NewModuleName,NewFunctionName,_NewArity}} ->
		       ?LOG("call ~s got mapped to ~p:~p~n",
			    [pp(C), NewModuleName, NewFunctionName]),
		       copy_ann
			 (C,
			  cerl:c_apply
			  (cerl:c_fname
			   (NewFunctionName,
			    cerl:fname_arity(Operator)),
			   cerl:apply_args(C)));
			 _ -> C
		   end;
		 _ -> C
	       end;
	     call ->
	       ?LOG("Seeing call ~s~n", [pp(C)]),
	       Module = cerl:call_module(C),
	       Name = cerl:call_name(C),
	       case {cerl:is_c_atom(Module), cerl:is_c_atom(Name)} of
		 {true, true} ->
		   ?LOG("Calling compile_info~n", []),
		   case mce_erl_compile_info:func_mapping
		     (cerl:atom_val(Module),
		      cerl:atom_val(Name),
		      cerl:call_arity(C),
		      CompileInfo)
		     of
		     {ok, {NewModuleName, NewFunctionName, _Arity}} ->
		       ?LOG("call ~s got mapped to ~p:~p~n",
			    [pp(C), NewModuleName, NewFunctionName]),
		       copy_ann
			 (C,
			  cerl:c_call(cerl:c_atom(NewModuleName),
				      cerl:c_atom(NewFunctionName),
				      cerl:call_args(C)));
			 _ ->
		       ?LOG("~s preserved1~n", [pp(C)]), C
		   end;
		 _ -> ?LOG("~s preserved2~n", [pp(C)]), C
	       end;
	     _ -> C
	   end
       end, Code).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Transform applies into safe applies

safe_calls(Code, CompileInfo) ->
  ModuleName = cerl:atom_val(cerl:module_name(Code)),
  Defs = cerl:module_defs(Code),
  ?LOG("Making calls safe ~p~n", [ModuleName]),
  cerl_trees:map
    (fun (C) ->
	 case cerl:type(C) of
	   call ->
	     ?LOG("Seeing call ~s~n", [pp(C)]),
	     Module = cerl:call_module(C),
	     Name = cerl:call_name(C),
	     case {cerl:is_c_atom(Module), cerl:is_c_atom(Name)} of
	       {true, true} ->
		 ?LOG("~s preserved~n", [pp(C)]), C;
	       Other ->
		 Result =
		   copy_ann
		     (C,
		      cerl:c_call
		      (cerl:c_atom(mce_erl),
		       cerl:c_atom(safe_call),
		       [Module,Name,c_list(cerl:call_args(C)),
		        cerl:c_int(length(cerl:call_args(C)))])),
		 ?LOG("~s made safe~n", [pp(C)]), Result
	     end;
	   _ -> C
	 end
     end, Code).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Transformation that replaces receive expressions with recv,
%%% letexp, try and choice tuples.

-record(cRrec,{moduleName,compileRec}).

changeReceive(Code, CompileRec) ->
  ModuleName = cerl:atom_val(cerl:module_name(Code)),
  Exports = cerl:module_exports(Code),
  Attributes = cerl:module_attrs(Code),
  Defs = cerl:module_defs(Code),
  CR = #cRrec{moduleName=ModuleName, compileRec=CompileRec},
  ?LOG("Module name is ~p~n~n", [ModuleName]),
  {NewDefs, NewExports} =
    lists:foldl
      (fun ({FunName, Fun}, {Forms, Exports}) ->
	   ?LOG("Translating function ~s~n", [pp(FunName)]),
	   {NewExpr, NewForms} = transform_receive(Fun, CR),
	   NewMappedForms =
	     lists:map(fun ({Name,LineNo,Form}) ->
			   {Name,FunName,LineNo,Form} end, NewForms),
	   {[{FunName, FunName, cerl:c_atom(void), NewExpr} |
	     NewMappedForms ++ Forms],
	    lists:map(fun ({NewName, _, _}) -> NewName
		      end, NewForms) ++ Exports}
       end, {[], Exports}, Defs),
  FunMap =
    mk_funmap(NewDefs),
  NewUnmappedDefs =
    lists:map (fun ({Name,_,_,Form}) -> {Name,Form} end, NewDefs),
  change_to_proper_vars
    (cerl:c_module
     (cerl:c_atom(ModuleName),
      [cerl:c_fname(mce_erl_real_name,1)|
       NewExports],
      Attributes, 
      [{cerl:c_fname(mce_erl_real_name,1),FunMap}|
       NewUnmappedDefs])).

mk_funmap(Defs) ->
  NameVar = genVar(),
  DefaultVar = genVar(),
  cerl:c_fun
    ([NameVar],
     cerl:c_case
     (NameVar,
      lists:foldl
      (fun ({Name,DefinedIn,LineNo,_},Clauses) ->
	   if
	     Name=:=DefinedIn ->
	       Clauses;
	     true ->
	       ProperName = cerl:fname_id(Name),
	       ProperDefinedIn = cerl:fname_id(DefinedIn),
	       [cerl:c_clause
		([cerl:c_atom(ProperName)],
		 cerl:c_tuple([cerl:c_atom(ProperDefinedIn),LineNo]))|
		Clauses]
	   end
       end,
       [cerl:c_clause
	([DefaultVar],
	 cerl:c_tuple([DefaultVar,cerl:c_atom(void)]))],
       Defs))).

transform_receive(C,CR) ->
  transform_receive(C,[],CR).

transform_receive(C, Vars, CR) ->
  case cerl:type(C) of
    'call' ->
      Module = cerl:call_module(C),
      Name = cerl:call_name(C),
      case {cerl:is_c_atom(Module), cerl:is_c_atom(Name)} of
	{true,true} ->
	  case mce_erl_sef_analysis:call_has_snd_for_sure
	    ({cerl:atom_val(Module),cerl:atom_val(Name),cerl:call_arity(C)},
	     CR#cRrec.compileRec) of
	    false -> {C, []};
	    true -> 
	      PauseFunName =
		genAtom(),
	      NeededPauseVars =
		ordsets:intersection(Vars, cerl_trees:free_variables(C)),
	      PauseFun =
		{cerl:c_fname
		 (cerl:atom_val(PauseFunName),
		  length(NeededPauseVars)),
		 try_to_find_lineno([C,Module,Name]),
		 cerl:c_fun(to_vars(ordsets:to_list(NeededPauseVars)),C)},
	      PauseExpr =
		copy_ann
		  (C,
		   cerl:c_call
		   (cerl:c_atom(mce_erl_stacks),
		    cerl:c_atom(mkSend),
		    [cerl:c_tuple
		     ([Module,
		       Name,
		       cerl:c_int(cerl:call_arity(C))]),
		     cerl:c_tuple
		     ([cerl:c_atom(CR#cRrec.moduleName),
		       PauseFunName,
		       c_list(to_vars(ordsets:to_list(NeededPauseVars)))])])),
	       {PauseExpr,[PauseFun]}
	  end;
	_ -> {C,[]}
      end;
    'fun' ->
      NewVars =
	cerl:fun_vars(C),
      {NewBody, NewForms} =
	transform_receive(cerl:fun_body(C), add_vars(NewVars, Vars), CR),
      {cerl:c_fun(NewVars, NewBody), NewForms};
    'case' ->
      {NewClauses, NewForms} =
	transform_clauses(cerl:case_clauses(C), Vars, CR),
      {cerl:c_case(cerl:case_arg(C), NewClauses),
       NewForms};
    letrec ->
      NewVars =
	add_vars(cerl:letrec_vars(C), Vars),
      ?LOG("New LetRecVars=~p~n", [NewVars]),
      {NewDefs, NewForms} =
	transform_definitions(cerl:letrec_defs(C), NewVars, CR),
      {NewBody, NewerForms} =
	transform_receive(cerl:letrec_body(C), NewVars, CR),
      {cerl:c_letrec(NewDefs, NewBody), NewForms ++ NewerForms};
    'receive' ->
      mk_receive_fun(C, Vars, CR);
    'let' ->
      ?LOG("In let: vars=~p c=~s~n", [Vars, pp(C)]),
      {NewBody, BodyForms} =
	transform_receive
	  (cerl:let_body(C), add_vars(cerl:let_vars(C), Vars), CR),
      ?LOG("NewBody=~s~n", [pp(NewBody)]),
      case
	mce_erl_sef_analysis:has_sef
	(CR#cRrec.moduleName, cerl:let_arg(C), CR#cRrec.compileRec)
	of
	true ->
	  ?LOG("Has sef...~n", []),
	  TransformArg =
	    case cerl:let_vars(C) of
	      [_] -> cerl:let_arg(C);
	      _ ->
		?LOG("let: transform values argument~n  ~s~ninto tuple~n  ~s~n",
		     [pp(cerl:let_arg(C)),
		      pp(transform_values_to_tuple(cerl:let_arg(C)))]),
		transform_values_to_tuple(cerl:let_arg(C))
	    end,
	  {NewArg, ArgForms} =
	    transform_receive(TransformArg, Vars, CR),
	  ?LOG("NewArg=~s~n", [pp(NewArg)]),
	  {LetExpr, NewLetFuns} =
	    mk_let_expr(C, NewBody, NewArg, Vars, cerl:let_vars(C), CR),
	  ?LOG("LetExpr=~s~n", [pp(LetExpr)]),
	  {LetExpr, NewLetFuns ++ ArgForms ++ BodyForms};
	false ->
	  ?LOG("No sef...~n", []),
	  {NewArg, ArgForms} =
	    transform_receive(cerl:let_arg(C), Vars, CR),
	  ?LOG("NewArg=~s~n", [pp(NewArg)]),
	  {copy_ann(C,cerl:c_let(cerl:let_vars(C), NewArg, NewBody)),
	   ArgForms ++ BodyForms}
      end;
    'try' ->
      ?LOG("try: transform values argument~n  ~s~ninto tuple~n  ~s~n",
	   [pp(cerl:try_arg(C)),
	    pp(transform_values_to_tuple(cerl:try_arg(C)))]),
      TransformArg =
	case cerl:try_vars(C) of
	  [_] -> cerl:try_arg(C);
	  _ ->
	    ?LOG("try: transform values argument~n  ~s~ninto tuple~n  ~s~n",
		 [pp(cerl:try_arg(C)),
		  pp(transform_values_to_tuple(cerl:try_arg(C)))]),
	    transform_values_to_tuple(cerl:try_arg(C))
	end,
      {NewArg, ArgForms} =
	transform_receive(TransformArg, Vars, CR),
      NewBodyVars =
	cerl:try_vars(C),
      {NewBody, BodyForms} =
	transform_receive(cerl:try_body(C), add_vars(NewBodyVars,Vars), CR),
      NewHandlerVars =
	cerl:try_evars(C),
      {NewHandler, HandlerForms} =
	transform_receive(cerl:try_handler(C),add_vars(NewHandlerVars,Vars),CR),
      BodyFunName =
	genAtom(),
      BodyArg =
	genVar(),
      NeededBodyVars =
	ordsets:intersection(Vars, cerl_trees:free_variables(cerl:try_body(C))),
      BodyFun =
	{cerl:c_fname(cerl:atom_val(BodyFunName), length(NeededBodyVars) + 1),
	 try_to_find_lineno([cerl:try_body(C),C]),
	 cerl:c_fun([BodyArg|to_vars(ordsets:to_list(NeededBodyVars))],
		    cerl:c_case
		    (BodyArg,
		     [cerl:c_clause([case cerl:try_vars(C) of
				       [V] -> V;
				       _ -> cerl:c_tuple(cerl:try_vars(C))
				     end],
				    NewBody)]))},
      HandlerFunName =
	genAtom(),
      HandlerArg =
	genVar(),
      NeededHandlerVars =
	ordsets:intersection
	  (Vars, cerl_trees:free_variables(cerl:try_handler(C))),
      TryEvarsDecl =
	case cerl:try_evars(C) of
	  [EVar] -> EVar;
	  _ -> cerl:c_tuple(cerl:try_evars(C))
	end,
      HandlerFun =
	{cerl:c_fname
	 (cerl:atom_val(HandlerFunName), length(NeededHandlerVars) + 1),
	 try_to_find_lineno([cerl:try_handler(C),C]),
	 cerl:c_fun([HandlerArg| to_vars(ordsets:to_list(NeededHandlerVars))],
		    cerl:c_case(HandlerArg,
				[cerl:c_clause([TryEvarsDecl],
					       NewHandler)]))},
      BodyCont =
	cerl:c_tuple([cerl:c_atom(CR#cRrec.moduleName),
		      BodyFunName,
		      c_list(to_vars(ordsets:to_list(NeededBodyVars)))]),
      HandlerCont =
	cerl:c_tuple([cerl:c_atom(CR#cRrec.moduleName),
		      HandlerFunName,
		      c_list(to_vars(ordsets:to_list(NeededHandlerVars)))]),
      TryVar =
	genVar(),
      EVar1 =
	genVar(),
      EVar2 =
	genVar(),
      EVar3 =
	genVar(),
      TryExpr =
	copy_ann
	  (C,
	   cerl:c_try
	   (NewArg,
	    [TryVar],
	    cerl:c_call(cerl:c_atom(mce_erl_stacks),
			cerl:c_atom(tryValue),
			[TryVar, BodyCont, HandlerCont]),
	    [EVar1, EVar2, EVar3],
	    cerl:c_call
	    (cerl:c_atom(mce_erl_stacks),
	     cerl:c_atom(tryHandler),
	     [cerl:c_tuple([EVar1, EVar2, EVar3]), HandlerCont]))),
      {TryExpr,
       [BodyFun, HandlerFun| ArgForms ++ BodyForms ++ HandlerForms]};
    _ ->
      {C, []}
  end.
  
transform_definitions(Definitions,Vars,CR) ->
  lists:foldl
    (fun ({Var,Def},{NewDefs,NewForms}) ->
	 {NewDef,NewerForms} = transform_receive(Def,Vars,CR),
	 {[{Var,NewDef}|NewDefs],NewerForms++NewForms}
     end,
     {[],[]},
     Definitions).

transform_clauses(Clauses,Vars,CR) ->
  lists:foldr
    (fun (Clause,{NewClauses,NewForms}) ->
	 NewVars = add_vars(cerl:clause_vars(Clause),Vars),
	 ?LOG("Clause body: ~s~n",[pp(cerl:clause_body(Clause))]),
	 {NewBody,NewerForms} =
	   transform_receive(cerl:clause_body(Clause),NewVars,CR),
	 {[cerl:c_clause(cerl:clause_pats(Clause),
			 cerl:clause_guard(Clause),
			 NewBody)|
	   NewClauses],
	  NewerForms++NewForms}
     end, {[],[]}, Clauses).

mk_receive_fun(Receive, Vars, CR) ->
  ?LOG("mk_receive_fun(~s)~n", [pp(Receive)]),
  Clauses = cerl:receive_clauses(Receive),
  Timeout = cerl:receive_timeout(Receive),
  Action = cerl:receive_action(Receive),
  {NewClauses, ClausesForms} =
    transform_clauses(Clauses, Vars, CR),
  ?LOG("OldClauses=~p~nNewClauses=~p~n", [Clauses, NewClauses]),
  {NewAction, ActionForms} =
    case is_infinity_timer(Timeout) of
      true ->
	{cerl:c_atom(true), []};
      false ->
	ActionNeededVars =
	  ordsets:intersection(Vars, cerl_trees:free_variables(Action)),
	NewActionFunName = genAtom(),
	{NewAction0, ActionForms0} =
	  transform_receive(Action, Vars, CR),
	{cerl:c_tuple([cerl:c_atom(CR#cRrec.moduleName), NewActionFunName,
		       c_list(to_vars(ordsets:to_list(ActionNeededVars)))]),
	 [{cerl:c_fname(cerl:atom_val(NewActionFunName), 
			length(ActionNeededVars)),
	   try_to_find_lineno([Action,Receive]),
	   cerl:c_fun(to_vars(ordsets:to_list(ActionNeededVars)), NewAction0)}
	  | ActionForms0]}
    end,
  Variables = cerl_trees:free_variables(Receive),
  NewFunName = genAtom(),
  ?LOG("Vars is ~p, ReceiveVars is ~p, Intersection=~p~n",
       [Vars, cerl_trees:free_variables(Receive),
	ordsets:intersection(Vars, cerl_trees:variables(Receive))]),
  NeededVars =
    ordsets:intersection
      (Vars,
       cerl_trees:variables(cerl:c_receive(cerl:receive_clauses(Receive)))),
  RecvExpr =
    cerl:c_tuple([cerl:c_atom(?RECVTAG),
		  cerl:c_tuple([cerl:c_tuple
				([cerl:c_atom(CR#cRrec.moduleName),
				  NewFunName,
				  c_list(to_vars
					 (ordsets:to_list(NeededVars)))]),
				cerl:c_tuple([Timeout, NewAction])])]),
  ?LOG("RecvExpr: ~s~n", [pp(RecvExpr)]),
  ParmMsg = genVar(),
  ParmList =
    [ParmMsg| to_vars(ordsets:to_list(NeededVars))],
  NormalClauses =
    lists:map(fun (Clause) ->
		  cerl:c_clause(cerl:clause_pats(Clause),
				cerl:clause_guard(Clause),
				cerl:c_tuple
				([c_true(),
				  cerl:c_fun([], cerl:clause_body(Clause))]))
	      end, NewClauses),
  DefaultClauseVar =
    genVar(),
  DefaultClause =
    cerl:c_clause([DefaultClauseVar], c_false()),
  RecvFun =
    {cerl:c_fname(cerl:atom_val(NewFunName), length(ParmList)),
     try_to_find_lineno([Receive]),
     cerl:c_fun(ParmList,
		cerl:c_case(ParmMsg, NormalClauses ++ [DefaultClause]))},
  ?LOG("RecvFun: ~s~n",
       [pp(element(2, RecvFun))]),
  ?LOG("RecvFun name: ~s~n",
       [pp(cerl:c_fname(cerl:atom_val(NewFunName), length(ParmList)))]),
  {RecvExpr, [RecvFun| ActionForms ++ ClausesForms]}.

is_infinity_timer(Timer) ->
  case cerl:is_c_atom(Timer) of
    true ->
      case cerl:atom_val(Timer) of
	'infinity' -> true;
	_ -> false
      end;
    false -> false
  end.

mk_let_expr(C,LetBody,LetArg,Vars,LetVars,CR) ->
  NewFunName =
    genAtom(),
  ?LOG
    ("mk_let_expr: defining body function ~p~nbody=~s~narg=~s~nVars=~p~nLetVars=~p~n",
     [cerl:atom_val(NewFunName),pp(LetBody),pp(LetArg),Vars,LetVars]),
  NeededVars = 
    ordsets:intersection(Vars,cerl_trees:variables(LetBody)),
  ?LOG
    ("BodyVars=~p,NeededVars=~p~n",
     [cerl_trees:free_variables(LetBody),NeededVars]),
  LetExpr =
    cerl:c_call
      (cerl:c_atom(mce_erl_stacks),
       cerl:c_atom(mkLet),
       [LetArg,
	cerl:c_tuple
	([cerl:c_atom(CR#cRrec.moduleName),
	  NewFunName,
	  c_list(to_vars(ordsets:to_list(NeededVars)))])]),
   ?LOG("LetExpr=~s~n",[pp(LetExpr)]),
  ParmArg =
    genVar(),
  ParmList =
    [ParmArg|to_vars(ordsets:to_list(NeededVars))],
  LetVarsDecl =
    case LetVars of
      [LetVar] ->
	LetVar;
      _ ->
	cerl:c_tuple(LetVars)
    end,
  LetFunBody =
    cerl:c_fun
      (ParmList,
       cerl:c_case
       (ParmArg,
	[cerl:c_clause
	 ([LetVarsDecl],
	  LetBody)])),
  ?LOG("LetFunBody=~s~n",[pp(LetFunBody)]),
  LetFun =
    {cerl:c_fname(cerl:atom_val(NewFunName),length(ParmList)),
     try_to_find_lineno([LetArg,C]),
     LetFunBody},
  {LetExpr,[LetFun]}.

transform_values_to_tuple(C) ->
  case cerl:type(C) of
    'values' ->
      cerl:c_tuple(cerl:values_es(C));
    'case' ->
      cerl:c_case
	(cerl:case_arg(C),
	 transform_clauses_values_to_tuples(cerl:case_clauses(C)));
    'receive' ->      
      copy_ann
	(C,
	 cerl:c_receive
	 (transform_clauses_values_to_tuples(cerl:receive_clauses(C)),
	  cerl:receive_timeout(C),
	  transform_values_to_tuple(cerl:receive_action(C))));
    'let' ->
      copy_ann
	(C,
	 cerl:c_let
	 (cerl:let_vars(C),
	  cerl:let_arg(C), 
	  transform_values_to_tuple(cerl:let_body(C))));
    'try' ->
      copy_ann
	(C,
	 cerl:c_try
	 (cerl:try_arg(C),
	  cerl:try_vars(C),
	  transform_values_to_tuple(cerl:try_body(C)),
	  cerl:try_evars(C),
	  transform_values_to_tuple(cerl:try_handler(C))));
    'primop' ->
      C;
    _ ->
      cerl:c_tuple([C])
  end.

transform_clauses_values_to_tuples(Clauses) ->
  lists:map
    (fun (Clause) ->
	 cerl:c_clause
	   (cerl:clause_pats(Clause),
	    cerl:clause_guard(Clause),
	    transform_values_to_tuple(cerl:clause_body(Clause)))
     end,
     Clauses).

strip_vars(Vars) ->
  Vars.

to_vars(VL) ->
  lists:map (fun cerl:c_var/1, VL).

strip_var(V) ->
  if
    is_atom(V) -> V;
    true ->
      case cerl:is_c_var(V) of
	true ->
	  cerl:var_name(V);
	false ->
	  V
      end
  end.

add_vars(New,Vars) -> 
  ordsets:union(ordsets:from_list(lists:map (fun strip_var/1, New)), Vars).

try_to_find_lineno([]) ->
  cerl:c_atom(void);
try_to_find_lineno([C|Rest]) ->
  case cerl:get_ann(C) of
    [] ->
      try_to_find_lineno(Rest);
    [N|_] when is_integer(N) ->
      cerl:c_int(N);
    _ ->
      try_to_find_lineno(Rest)
  end.
      

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

normalize_module(Code) ->
  Name = cerl:module_name(Code),
  Exports = cerl:module_exports(Code),
  Attributes = cerl:module_attrs(Code),
  Defs = cerl:module_defs(Code),
  NewDefs =
    lists:map(fun ({FName,Fun}) -> {FName,normalize_expr(Fun)} end, Defs),
  cerl:c_module(Name,Exports,Attributes,NewDefs).

is_complex_head_expr(E) ->
    case cerl:type(E) of
      apply ->
	  true;
      call ->
	  true;
      'fun' ->
	  true;
      'let' ->
	  true;
      letrec ->
	  true;
      primop ->
	  not is_simple_expr(E);
      'receive' ->
	  true;
      'try' ->
	  true;
      _ ->
	  false
    end.
  
is_simple_expr(E) ->
  case cerl:type(E) of
    'binary' ->
      true;
    'bitstr' ->
      true;
    'literal' ->
      true;
    'var' -> 
      true;
    'fun' ->
      true;
    'cons' ->
      is_simple_expr(cerl:cons_hd(E)) andalso is_simple_expr(cerl:cons_tl(E));
    'tuple' ->
      is_simple_exprs(cerl:tuple_es(E));
    'values' ->
      is_simple_exprs(cerl:values_es(E));
    'primop' ->
      is_simple_exprs(cerl:primop_args(E));
    _ ->
      false
  end.

is_simple_exprs(Es) ->
  lists:foldl
    (fun (Elem,Coll) -> Coll andalso is_simple_expr(Elem) end, true, Es).

split_expr(E) ->  
  case cerl:type(E) of
    'apply' ->
      {[cerl:apply_op(E)|cerl:apply_args(E)],
       fun ([AOp|Args]) ->
	   copy_ann(E,cerl:c_apply(AOp,Args))
       end};
    'call' ->
      {[cerl:call_module(E),cerl:call_name(E)|cerl:call_args(E)],
       fun ([AModule,AOp|Args]) ->
	   copy_ann(E,cerl:c_call(AModule,AOp,Args))
       end};
     'primop' ->
      {cerl:primop_args(E),
       fun (Args) -> cerl:c_primop(cerl:primop_name(E),Args) end};
    'cons' ->
      {[cerl:cons_hd(E),cerl:cons_tl(E)],
       fun ([Hd,Tl]) -> cerl:c_cons(Hd,Tl)
       end};
    'tuple' ->
      {cerl:tuple_es(E),
       fun (Args) -> cerl:c_tuple(Args) end};
    'values' ->
      {cerl:values_es(E),
       fun (Args) -> cerl:c_values(Args) end};
    _ ->
      {[],fun ([]) -> e end}
  end.
      
normalize_expr(E) ->
  case split_expr(E) of
    {Args,F} when is_list(Args), Args=/=[] ->
      normalize_args(Args,F);
    _ ->
      case cerl:type(E) of
	'fun' ->
	  cerl:c_fun
	    (cerl:fun_vars(E),
	     normalize_expr(cerl:fun_body(E)));
	'let' ->
	  Arg =
	    normalize_expr(cerl:let_arg(E)),
	  Body =
	    normalize_expr(cerl:let_body(E)),
	  splice_in
	    (Arg,
	     fun (A) -> copy_ann(E,cerl:c_let(cerl:let_vars(E),A,Body)) end);
	'receive' ->
	  Clauses = normalize_clauses(cerl:receive_clauses(E)),
	  Action = normalize_expr(cerl:receive_action(E)),
	  Timeout = 
	    TimeoutExpr = cerl:receive_timeout(E),
	    case is_simple_expr(TimeoutExpr) of
	      true ->
		TimeoutExpr;
	      false ->
		V = genVar(),
		cerl:c_let([V],normalize_expr(TimeoutExpr),V)
	    end,
	  splice_in
	    (Timeout,
	     fun (T) -> copy_ann(E,cerl:c_receive(Clauses,T,Action)) end);
	'case' ->
	  EArg =
	    cerl:case_arg(E),
	  Arg = 
	    case is_simple_expr(EArg) of
	      true ->
		EArg;
	      false ->
		V = genVar(),
		cerl:c_let([V],normalize_expr(EArg),V)
	    end,
	  splice_in
	    (Arg,
	     fun (A) ->
		 cerl:c_case(A,normalize_clauses(cerl:case_clauses(E)))
	     end);
	'try' ->
	  copy_ann
	    (E,
	     cerl:c_try
	     (normalize_expr(cerl:try_arg(E)),
	      cerl:try_vars(E),
	      normalize_expr(cerl:try_body(E)),
	      cerl:try_evars(E),
	      normalize_expr(cerl:try_handler(E))));
	'letrec' ->
	  cerl:c_letrec
	    (lists:map
	     (fun ({V,D}) -> {V,normalize_expr(D)} end,
	      cerl:letrec_defs(E)),
	     normalize_expr(cerl:letrec_body(E)));
	_ -> E
      end
  end.

normalize_clauses(Clauses) ->
  lists:map
    (fun (C) ->
	 cerl:c_clause
	   (cerl:clause_pats(C),
	    normalize_expr(cerl:clause_guard(C)),
	    normalize_expr(cerl:clause_body(C)))
     end, Clauses).

splice_in(Arg,F) ->
  case cerl:type(Arg) of
    'let' ->
      cerl:c_let(cerl:let_vars(Arg),
		 cerl:let_arg(Arg),
		 splice_in(cerl:let_body(Arg),F));
    _ ->
      F(Arg)
  end.

normalize_args(Es,F) ->
  normalize_args(Es,[],F).
normalize_args([],ArgList,F) ->
  gen_new_term(lists:reverse(ArgList),F);
normalize_args([E|Es],ArgList,F) ->
  case is_complex_head_expr(E) of
    true ->
      NewVar = genVar(),
      NewE = cerl:c_let([NewVar],E,NewVar),
      normalize_args(Es,[normalize_expr(NewE)|ArgList],F);
    false ->
      normalize_args(Es,[normalize_expr(E)|ArgList],F)
  end.
      
gen_new_term(ArgList,F) ->	
  gen_new_term(ArgList,[],F).
gen_new_term([],Collected,F) ->
  do_gen_term(F,Collected,[]);
gen_new_term([E|Es],Collected,F) ->
  case cerl:type(E) of
    'let' ->
      cerl:c_let(cerl:let_vars(E),
		 cerl:let_arg(E),
		 gen_new_term([cerl:let_body(E)|Es],Collected,F));
    _ ->
      gen_new_term(Es,[E|Collected],F)
  end.
    
do_gen_term(F,[],Terms) ->
  F(Terms);
do_gen_term(F,[T|Rest],Terms) ->
  case is_simple_expr(T) of
    true ->
      do_gen_term(F,Rest,[T|Terms]);
    false ->
      V = genVar(),
      cerl:c_let([V],T,do_gen_term(F,Rest,[V|Terms]))
  end.
      
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_reduced_core_module(Code) ->
  check_core_erlang(Code),
  lists:foreach
    (fun ({Name,D}) -> 
	 try check_reduced_core_expr(D) 
	 catch Error:Reason ->
	     io:format
	       ("Core Erlang code for module ~p does not pass correctness "++
		"checks:~n~n",
		[Name]),
	     io:format("~s~n~n",[pp(Code)])
	 end
     end,
     cerl:module_defs(Code)).

check_core_erlang(Code) ->
  CodeRecords = cerl:to_records(Code),
  case core_lint:module(CodeRecords) of
    {error,Errors,_} ->
	  ?LOG("*** Core Erlang ERROR in module ~p~n",
	       [cerl:atom_val(cerl:module_name(Code))]),
	  print_lint_errors(cerl:atom_val(cerl:module_name(Code)),Errors),
	  ?LOG("~s~n~n",[pp(Code)]);
    {ok,_} ->
      ok
  end.

print_lint_errors(ModuleName,Errors) ->
    lists:foreach
      (fun ({_,FileErrors}) ->
	       lists:foreach
		 (fun ({core_lint,ErrorDescriptor}) ->
			  ErrorString = core_lint:format_error(ErrorDescriptor),
			  io:format
			    ("core_lint error in module ~p: ~s~n",
			     [ModuleName,ErrorString])
		  end, FileErrors)
       end, Errors).

check_reduced_core_expr(C) ->
  check_e(C).

check_e(E) ->
  case is_simple_expr(E) of
    true ->
      true;
    false ->
      case cerl:type(E) of
	'let' ->
	  check_n(cerl:let_arg(E)),
	  check_e(cerl:let_body(E));
	_ ->
	  check_n(E)
      end
  end.

check_n(E) ->
  case cerl:type(E) of
    'try' ->
      check_e(cerl:try_arg(E)),
      check_e(cerl:try_body(E)),
      check_e(cerl:try_handler(E));
    'case' ->
      check_s(cerl:case_arg(E)),
      check_clauses(cerl:case_clauses(E));
    'receive' ->
      check_s(cerl:receive_timeout(E)),
      check_e(cerl:receive_action(E)),
      check_clauses(cerl:receive_clauses(E));
    'call' ->
      check_s(cerl:call_module(E)),
      check_s(cerl:call_name(E)),
      lists:foreach(fun check_s/1,cerl:call_args(E));
    'primop' ->
      check_s(cerl:primop_name(E)),
      lists:foreach(fun check_s/1,cerl:primop_args(E));
    'apply' ->
      check_s(cerl:apply_op(E)),
      lists:foreach(fun check_s/1,cerl:apply_args(E));
    'letrec' ->
      lists:foreach(fun ({_,D}) -> check_e(D) end, cerl:letrec_defs(E)),
      check_e(cerl:letrec_body(E));
    'fun' ->
      check_e(cerl:fun_body(E));
    _ ->
      io:format("Expr ~s is not basic~n",[pp(E)]),
      throw(check_n)
  end.
      
check_s(E) ->
  case is_simple_expr(E) of
    true ->
      ok;
    false ->
      io:format("Expr ~s is not simple~n",[pp(E)]),
      throw(check_s)
  end.

check_clauses(Cl) ->
  lists:foreach
    (fun (Clause) ->
	 check_e(cerl:clause_guard(Clause)),
	 check_e(cerl:clause_body(Clause))
     end, Cl).

fresh_subst(Var,Term) ->
  FreshVar = genVar(),
  {FreshVar,subst(FreshVar,Var,Term)}.

subst(FreshVar,Var,Term) ->
  ?LOG("subst(~s,~s,~s)~n",[pp(FreshVar),pp(Var),pp(Term)]),
  NewTerm =
    case cerl:type(Term) of
      'alias' ->
	cerl:c_alias
	  (subst(FreshVar,Var,cerl:alias_var(Term)),
	   subst(FreshVar,Var,cerl:alias_pat(Term)));
      'apply' ->
	cerl:c_apply
	  (subst(FreshVar,Var,cerl:apply_op(Term)),
	   substs(FreshVar,Var,cerl:apply_args(Term)));
      'binary' ->
	cerl:c_binary
	  (substs(FreshVar,Var,cerl:binary_segments(Term)));
      'bitstr' ->
	cerl:c_bitstr
	  (subst(FreshVar,Var,cerl:bitstr_val(Term)),
	   subst(FreshVar,Var,cerl:bitstr_size(Term)),
	   subst(FreshVar,Var,cerl:bitstr_unit(Term)),
	   subst(FreshVar,Var,cerl:bitstr_type(Term)),
	   subst(FreshVar,Var,cerl:bitstr_flags(Term)));
      'call' ->
	cerl:c_call
	  (subst(FreshVar,Var,cerl:call_module(Term)),
	   subst(FreshVar,Var,cerl:call_name(Term)),
	   substs(FreshVar,Var,cerl:call_args(Term)));
      'case' ->
	cerl:c_case
	  (subst(FreshVar,Var,cerl:case_arg(Term)),
	   substs(FreshVar,Var,cerl:case_clauses(Term)));
      'clause' ->
	cerl:c_clause
	  (substs(FreshVar,Var,cerl:clause_pats(Term)),
	   subst(FreshVar,Var,cerl:clause_guard(Term)),
	   subst(FreshVar,Var,cerl:clause_body(Term)));
      'cons' ->
	cerl:c_cons
	  (subst(FreshVar,Var,cerl:cons_hd(Term)),
	   subst(FreshVar,Var,cerl:cons_tl(Term)));
      'fun' ->
	case lists:member(Var,cerl:fun_vars(Term)) of
	  true ->
	    Term;
	  false ->
	    cerl:c_fun(cerl:fun_vars(Term),
		       subst(FreshVar,Var,cerl:fun_body(Term)))
	end;
      'let' ->
	LetArg = 
	  subst(FreshVar,Var,cerl:let_arg(Term)),
	case lists:member(Var,cerl:let_vars(Term)) of
	  true ->
	    cerl:c_let(cerl:let_vars(Term), LetArg, cerl:let_body(Term));
	  false ->
	    cerl:c_let
	      (cerl:let_vars(Term),
	       LetArg,
	       subst(FreshVar,Var,cerl:let_body(Term)))
	end;
      'letrec' ->
	case lists:member(Var,cerl:letrec_vars(Term)) of
	  true ->
	    Term;
	  false ->
	    cerl:c_letrec
	      (lists:map
	       (fun ({Name,Def}) -> {Name,subst(FreshVar,Var,Def)} end,
		cerl:letrec_defs(Term)),
	       subst(FreshVar,Var,cerl:letrec_body(Term)))
	end;
      'literal' ->
	Term;
      'module' ->
	cerl:c_module
	  (cerl:module_name(Term),
	   cerl:module_exports(Term),
	   cerl:module_attrs(Term),
	   lists:map
	   (fun ({Name,Def}) -> {Name,subst(FreshVar,Var,Def)} end,
	    cerl:module_defs(Term)));
      'primop' ->
	Term;
      'receive' ->
	cerl:c_receive
	  (substs(FreshVar,Var,cerl:receive_clauses(Term)),
	   subst(FreshVar,Var,cerl:receive_timeout(Term)),
	   subst(FreshVar,Var,cerl:receive_action(Term)));
      'try' ->
	cerl:c_try
	  (subst(FreshVar,Var,cerl:try_arg(Term)),
	   cerl:try_vars(Term),
	   case lists:member(Var,cerl:try_vars(Term)) of
	     true -> cerl:try_body(Term);
	     false -> subst(FreshVar,Var,cerl:try_body(Term))
	   end,
	   cerl:try_evars(Term),
	   case lists:member(Var,cerl:try_evars(Term)) of
	     true -> cerl:try_handler(Term);
	     false -> subst(FreshVar,Var,cerl:try_handler(Term))
	   end);
      'tuple' ->
	cerl:c_tuple(substs(FreshVar,Var,cerl:tuple_es(Term)));
      'values' ->
	cerl:c_values(substs(FreshVar,Var,cerl:values_es(Term)));
      'var' ->
	case cerl:var_name(Term)==cerl:var_name(Var) of
	  true ->
	    FreshVar;
	  false ->
	    Term
	end;
      Other ->
	io:format
	  ("*** Error: subst:unexpected type ~p for expression ~p~n",
	   [Other,Term])
    end,
  copy_ann(Term,NewTerm).

substs(FreshVar,Var,Terms) ->
  lists:map (fun (Term) -> subst(FreshVar,Var,Term) end, Terms).

copy_ann(Term1,Term2) ->
  cerl:set_ann(Term2,cerl:get_ann(Term1)).

print_if_or_log(true,Format,Arguments) ->
  io:format(Format,Arguments);
print_if_or_log(false,Format,Arguments) ->
  if ?DEBUGVAL() -> io:format(Format,Arguments); true -> ok end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compile_core(FileName) ->
  case compile:file(FileName,[return_errors, from_core, clint]) of
    {error, Errors, _Warnings} ->
      io:format
	("*** Error: unable to translate ~p to HiPE Core Erlang ***~n",
	 [FileName]),
      lists:foreach
	(fun ({FileName, FileErrors}) ->
	     lists:foreach
	       (fun ({ErrorLine, Module, ErrorDescriptor}) ->
		    ErrorString = Module:format_error(ErrorDescriptor),
		    io:format("~s:~p: ~s~n",[FileName, ErrorLine, ErrorString]);
		    ({Module, ErrorDescriptor}) ->
		    ErrorString = Module:format_error(ErrorDescriptor),
		    io:format("~p: ~s~n",[FileName, ErrorString])
		end, FileErrors)
	 end, Errors),
      throw(compile);
    Other -> Other
  end.
    
  
