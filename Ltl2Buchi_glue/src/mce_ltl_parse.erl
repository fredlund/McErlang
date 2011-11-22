-module(mce_ltl_parse).
-export([string/1,ltl2module/2,ltl2module_and_load/2]).
-export([ltl_string2module/2,ltl_string2module_and_load/2]).

string(S) ->
  {ok,Tokens,_} = erl_scan:string(S),
  ConvertedTokens = mce_ltl_scan:scan(Tokens),
  case mce_ltl_parser:parse(ConvertedTokens) of
    {ok,Result} ->
      Result;
    {error,_} ->
      io:format("*** parse error at ltl expression ~p~n",[S]),
      throw(ltl_parse)
  end.

ltl_string2module(String,FileName) ->
  ltl2module(string(String),FileName).

ltl_string2module_and_load(String,ModuleName) ->
  ltl2module_and_load(string(String),ModuleName).

ltl2module(LtlFormula,FileName) ->
  ltl2mcerlang:convert(LtlFormula,FileName).

ltl2module_and_load(LtlFormula,ModuleName) ->
  TmpFileName = "/tmp/"++atom_to_list(ModuleName)++".erl",
  ltl2mcerlang:convert(LtlFormula,TmpFileName),
  {ok,ModuleName,Binary} =
    compile:file
      (TmpFileName,
       [verbose,report_errors,report_warnings,binary]),
  {module,ModuleName} =
    code:load_binary(ModuleName,TmpFileName,Binary),
  ModuleName.



  
  
  
