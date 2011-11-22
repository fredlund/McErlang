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

-module(mce_erl_compile).

%%-define(debug,true).
-include("macros.hrl").
-include("emacros.hrl").

-include("compile_rec.hrl").
-export([start/0,start/1]).
-export([file/1,file/2]).
-export([compile_file/1]).
-export([get_all_sources/1]).
-compile(export_all).

start() ->
  start([]).

start(_Args) ->
  try
    DefaultFunInfo = mce:find_funinfo(),
    StandardLibs = find_stdlibs(),
    StandardIncludes = find_stdincludes() ++ [{i,"."}],
    ArgumentList = init:get_arguments(),
    check_expected_arguments(ArgumentList),
    ParsedArguments =
    lists:reverse
    (parse_arguments(ArgumentList),
     [{funinfo, [DefaultFunInfo]},
      {unknown_is_snd, ["true"]},
      {unknown_is_rcv, ["true"]},
      {warn_if_blacklisted, ["true"]},
      {output_dir, ["ebin"]},
      {permit_blacklisted, ["false"]},
      {sends_are_sefs, ["true"]},
      {pa,[]},
      {pz,[]},
      {verbose, ["false"]},
      {normalize_core, ["false"]},
      {libs, StandardLibs}])++
    StandardIncludes,
    IsVerbose = oneBoolValue(verbose,ParsedArguments),
    ?LOG("Parsed arguments=~p~n", [ParsedArguments]),
    Sources =
    proplists:get_all_values(sources, ParsedArguments),
    LinearisedSources =
    lists:foldr
    (fun ({SourceItems, Options}, Collected) ->
	 lists:map(fun (Item) -> {Item, Options} end, SourceItems) ++ Collected
     end, [], Sources),
    Libraries =
    lists:foldr
    (fun (X,R) -> X++R end,
     [],
     proplists:get_all_values(libs,ParsedArguments)),
    ?LOG("All libraries: ~p~n",[Libraries]),
    AllLibSources =
    get_all_lib_sources(Libraries,IsVerbose),
    print_if_or_log
    (IsVerbose,
     "AllLibSources=~p~n",
     [dict:to_list(AllLibSources)]),
    add_paths
    (lists:map(fun oneValue/1, proplists:get_all_values(pa,ParsedArguments)),
     lists:map(fun oneValue/1, proplists:get_all_values(pz,ParsedArguments))),
    compile
    (#compile_rec
     {sources=LinearisedSources,
      verbose=IsVerbose,
      output_dir=oneValue(output_dir,ParsedArguments),
      %% mcerlangDir=McErlangDirectory,
      lib_dict=AllLibSources,
      normalize_core=oneBoolValue(normalize_core,ParsedArguments),
      unknown_is_snd=oneBoolValue(unknown_is_snd,ParsedArguments),
      includes=proplists:get_all_values(i,ParsedArguments),
      unknown_is_rcv=oneBoolValue(unknown_is_rcv,ParsedArguments),
      warn_if_blacklisted=oneBoolValue(warn_if_blacklisted,ParsedArguments),
      permit_blacklisted=oneBoolValue(permit_blacklisted,ParsedArguments),
      sends_are_sefs=oneBoolValue(sends_are_sefs,ParsedArguments),
      erlang_compile_options=[],
      compile_info =
      mce_erl_compile_info:read_file
      (oneValue(proplists:get_value(funinfo, ParsedArguments)))})
  catch
    Error:Reason ->
      io:format
	("*** Error: compiler crashed with ~p because of error: ~p~n",
	 [Error, Reason]),
      io:format("Stack trace:~n  ~p~n", [erlang:get_stacktrace()]),
      timer:sleep(1000),
      halt(1)
  end,
  ?LOG("Finished compilation process~n", []),
  halt().

add_paths(APaths,ZPaths) ->
  lists:foreach(fun (Path) -> add_path(Path,true) end, APaths),
  lists:foreach(fun (Path) -> add_path(Path,false) end, ZPaths).
  
add_path(Path,IsAPath) ->
  case if IsAPath -> code:add_path(Path); true -> code:add_pathz(Path) end of
    true ->
      ok;
    {error,What} ->
      io:format("Could not add path ~p; error ~p~n",[Path,What])
  end.

print_if_or_log(true,Format,Arguments) ->
  io:format(Format,Arguments);
print_if_or_log(false,Format,Arguments) ->
  if ?DEBUGVAL() -> io:format(Format,Arguments); true -> ok end.

parse_arguments(Arguments) ->
  parse_arguments(Arguments,[]).
parse_arguments([],P) -> P;
parse_arguments([{sources,Sources}|Rest],P) ->
  {Options,NewRest} = parse_source_options(Rest),
  parse_arguments(NewRest,[{sources,{Sources,Options}}|P]);
parse_arguments([_Argument={Option,Arg}|Rest],P) ->
  parse_arguments(Rest,[{Option,case Arg of [] -> ["true"]; _ -> Arg end}|P]).

parse_source_options(Arguments) ->
  parse_source_options(Arguments,[]).
parse_source_options([{recursive,_}|Rest],P) ->
  parse_source_options(Rest,[recursive|P]);
parse_source_options([{include_dirs,Includes}|Rest],P) ->
  parse_source_options(Rest,[{include_dirs,Includes}|P]);
parse_source_options(Rest,P) -> {P,Rest}.

compile_file(FileName) ->
  if is_atom(FileName) -> ok; is_list(FileName) -> ok end,
  FunInfo = mce:find_funinfo(),
  compile
    (#compile_rec
     {sources=[{sources, [FileName]}],
      output_dir="ebin",
      %% mcerlangDir=McErlangDirectory,
      compile_info=mce_erl_compile_info:read_file(FunInfo)}).

%% file(File)
%% file([File])
%% file(File,[Opt])
%% file([File],[Opt])

file(FileOrFiles) ->
  case is_string(FileOrFiles) orelse not(is_list(FileOrFiles)) of
    true -> file_int([FileOrFiles],[]);
    false -> file_int(FileOrFiles,[])
  end.
file(FileOrFiles,Options) ->
  case is_string(FileOrFiles) orelse not(is_list(FileOrFiles)) of
    true -> file_int([FileOrFiles],Options);
    false -> file_int(FileOrFiles,Options)
  end.

file_int(Files,Options) ->
  try
    DefaultFunInfo = mce:find_funinfo(),
    StandardLibs = find_stdlibs(),
    StandardIncludes =
      find_stdincludes() ++ [{i,"."}],
    OptionsStandard =
      Options++
      [{funinfo,DefaultFunInfo},
       {unknown_is_snd,true},
       {unknown_is_rcv,true},
       {outdir,"ebin"},
       {sends_are_sefs,false},
       {verbose,false},
       {normalize_core,false},
       {libs,StandardLibs}]++
      StandardIncludes,
    IsVerbose = 
      proplists:get_value(verbose,OptionsStandard),
    Libraries =
      lists:foldr
	(fun (X,R) -> X++R end,
	 [],
	 proplists:get_all_values(libs,OptionsStandard)),
    AllLibSources =
      get_all_lib_sources(Libraries,IsVerbose),
    ?LOG("Options are~n~p~n",[OptionsStandard]),
    ?LOG("includes=~n~p~n",[proplists:get_all_values(i,OptionsStandard)]),
    ErlangCompilerOptions =
      lists:filter
	(fun (Option) ->
	     OptName = 
	       case Option of
		 {Opt,_} -> Opt;
		 _ -> Option
	       end,
	     not(lists:member
		   (OptName,
		    [funinfo,unknown_is_snd,unknown_is_rcv,outdir,
		     sends_are_sefs,verbose,normalize_core,libs,i]))
	 end, 
	 Options),
    CR =
      #compile_rec
      {sources=lists:map(fun normalize_file_spec/1,Files),
       verbose=IsVerbose,
       erlang_compile_options=ErlangCompilerOptions,
       output_dir=proplists:get_value(outdir,OptionsStandard),
       %% mcerlangDir=McErlangDirectory,
       lib_dict=AllLibSources,
       normalize_core=proplists:get_value(normalize_core,OptionsStandard),
       unknown_is_snd=proplists:get_value(unknown_is_snd,OptionsStandard),
       unknown_is_rcv=proplists:get_value(unknown_is_rcv,OptionsStandard),
       includes=proplists:get_all_values(i,OptionsStandard),
       sends_are_sefs=proplists:get_value(sends_are_sefs,OptionsStandard),
       compile_info =
	 mce_erl_compile_info:read_file
	   (proplists:get_value(funinfo, OptionsStandard))},
    compile(CR),
    {ok,
     lists:map
       (fun (FileName) ->
	    list_to_atom(filename:rootname(filename:basename(FileName)))
	end,Files)}
  catch
    _:{compile_error,Reason} ->
      {error,Reason};
    Exception:Reason ->
      io:format
	("*** Error: compilation failed due to uncaught exception ~p "++
	   "for reason ~p~n",
	 [Exception,Reason]),
      io:format("Stack trace:~n  ~p~n", [erlang:get_stacktrace()]),
      {error,{exception,Exception,Reason}}
  end.

normalize_file_spec(File) ->
  {FileSpec,Options} =
    case File of
      {FSpec,Opts} -> {FSpec,Opts};
      _ -> {File,[]}
    end,
  FileString =
    case is_list(FileSpec) of
      true -> FileSpec;
      false -> atom_to_list(FileSpec)
    end,
  case has_suffix(FileString,".erl") of
    true -> {FileString,Options};
    false ->
      case has_suffix(FileString,".core") of
	true -> {FileString,Options};
	false -> {FileString++".erl",Options}
      end
  end.
    
has_suffix(String,Suffix) ->
  Length = length(String),
  SuffixLength = length(Suffix),
  if
    Length=<SuffixLength -> false;
    true ->
      LastMatch = string:rstr(String,Suffix),
      (LastMatch-1) == (Length-SuffixLength)
  end.

is_string(MaybeString) ->
  is_list(MaybeString) 
    andalso MaybeString=/=[]
    andalso lists:all(fun erlang:is_integer/1, MaybeString).
		  
compile(CR) when is_record(CR, compile_rec) ->
  AllSources =
    get_all_sources(CR),
  print_if_or_log(CR#compile_rec.verbose,"All sources=~p~n", [AllSources]),
  AllCores =
    lists:foldl
      (fun ({_ShortFileName, {FileSpec, Options}}, Cores) ->
	   ?LOG("Translating ~p to HiPE Core Erlang...~n",
		[FileSpec]),
	   ?LOG("Options are ~p~nIncludes are ~p~n",
		[Options, proplists:get_all_values(include_dirs, Options)]),
	   SpecifiedIncludes =
	     lists:foldl
	       (fun (IncludeSpec, Collected) -> IncludeSpec ++ Collected end,
		[], proplists:get_all_values(include_dirs, Options)),
	   Includes = SpecifiedIncludes++CR#compile_rec.includes,
	   ?LOG("Checking ~p under the include list~n~p~n",
		[FileSpec, Includes]),
	   %%_ = parse_file(FileSpec,Includes),
	   IncludeDirectives = [{i, X} || X <- Includes],
	   ?LOG("Include directives=~p~n", [IncludeDirectives]),
	   %% We now permit Core Erlang code as input per request
	   case is_core_file(FileSpec) of
	     true ->
	       case file:read_file(FileSpec) of
		 {ok,Bin} ->
		   case core_scan:string(binary_to_list(Bin)) of
		     {ok,Toks,_} ->
		       case core_parse:parse(Toks) of
			 {ok,Code} ->
			   ModuleName = cerl:atom_val(cerl:module_name(Code)),
			   mce_erl_coreTrans:check_core_erlang(Code),
			   compile_file(ModuleName, Code) ++ Cores;
			 {error,E} ->
			   print_errors_or_warnings([{FileSpec,[E]}]),
			   throw({compile_error,{translate,FileSpec}})
		       end;
		     {error,E,_} ->
		       print_errors_or_warnings([{FileSpec,[E]}]),
		       throw({compile_error,{translate,FileSpec}})
		   end;
		 {error,E} ->
		   print_errors_or_warnings([{FileSpec,[E]}]),
		   throw({compile_error,{translate,FileSpec}})
	       end;
	     false ->
	       case compile:file
		 (FileSpec,
		  CR#compile_rec.erlang_compile_options ++
		  IncludeDirectives ++
		  [{d,'McErlang'},debug_info,
		   return_errors,return_warnings,
		   to_core,binary]) of
		 {ok, ModuleName, CoreCode} ->
		   check_modulename(ModuleName,FileSpec),
		   Code = cerl:from_records(CoreCode),
		   print_module_calls(Code,CR),
		   mce_erl_coreTrans:check_core_erlang(Code),
		   compile_file(ModuleName, Code) ++ Cores;
		 {ok, ModuleName, CoreCode, Warnings} ->
		   check_modulename(ModuleName,FileSpec),
		   Code = cerl:from_records(CoreCode),
		   print_module_calls(Code,CR),
		   mce_erl_coreTrans:check_core_erlang(Code),
		   print_errors_or_warnings(Warnings),
		   compile_file(ModuleName, Code) ++ Cores;
		 error ->
		   io:format
		     ("*** Error: unable to translate ~p to HiPE Core Erlang ***~n",
		      [FileSpec]),
		   throw({compile_error,{translate,FileSpec}});
		 {error, Errors, _Warnings} ->
		   io:format
		     ("*** Error: unable to translate ~p to HiPE Core Erlang ***~n",
		      [FileSpec]),
		   print_errors_or_warnings(Errors),
		   throw({compile_error,{translate,FileSpec}})
	       end
	   end
       end, [], AllSources),
  AllTranslatedCores =
    lists:foldl
      (fun (Core, Cores) ->
	   case Core of
	     {_ModuleName, erlang, _CoreCode} ->
	       [Core| Cores];
	     {ModuleName, normalized, CoreCode} ->
	       C1 = mce_erl_coreTrans:changeGuards(CoreCode),
	       ?LOG("~nCore code for module ~p after guard change:~n~n",
		    [ModuleName]),
	       ?LOG("~s~n", [mce_erl_coreTrans:pp(C1)]),
	       TranslatedCore =
		 mce_erl_coreTrans:safe_calls
		   (mce_erl_coreTrans:translate_names
		    (C1, CR#compile_rec.compile_info), 
		    CR#compile_rec.compile_info),
	       ?LOG("~nCore code for module ~p after translation:~n~n",
		    [ModuleName]),
	       ?LOG("~s~n", [mce_erl_coreTrans:pp(TranslatedCore)]),
	       [{ModuleName, normalized, TranslatedCore}| Cores]
	   end
       end, [], AllCores),
  ?LOG("Beginning sef analysis~n", []),
  SefAnalysis =
    mce_erl_sef_analysis:analyze(AllTranslatedCores, CR),
  ?LOG("Translating receives...~n", []),
  mce_erl_coreTrans:compile
    (AllTranslatedCores, CR#compile_rec{sef_analysis=SefAnalysis}).

is_core_file(FileSpec) ->
  has_suffix(FileSpec,".core").

compile_file(ModuleName, CoreCode) ->
  Attributes = cerl:module_attrs(CoreCode),
  case has_erlang_attribute(Attributes) of
    true ->
      [{ModuleName, erlang, CoreCode}];
    false ->
      ?LOG(("Translating ~p to normalized HiPE Core Erlang " ++
	    "representation...~n"), [ModuleName]),
      [mce_erl_coreTrans:normalize_core(CoreCode)]
  end.

check_modulename(ModuleName, FileName) ->
  FileModule = list_to_atom(filename:rootname(filename:basename(FileName))),
  if
    ModuleName=/=FileModule ->
      io:format
	("~n~p: Module name '~p' does not match file name '~p'~n",
	 [FileName,ModuleName,FileModule]),
      throw({compile_error,{module_name_mismatch,ModuleName,FileModule}});
    true ->
      ok
  end.

print_errors_or_warnings(Anomalies) ->
  lists:foreach
    (fun ({FileName, FileAnomalies}) ->
	 if
	   FileAnomalies=/=[] ->
	     io:format("~n");
	   true ->
	     ok
	 end,
	 lists:foreach
	   (fun (AnomalieInfo) ->
		{AnomalieLine, Module, AnomalieDescriptor} =
		  AnomalieInfo,
		AnomalieString =
		  Module:format_error(AnomalieDescriptor),
		io:format("~s:~p: ~s~n",
			  [FileName, AnomalieLine, AnomalieString])
	    end, FileAnomalies)
     end, Anomalies).

has_erlang_attribute(Attributes) ->
  lists:any
    (fun ({Attribute,Value}) ->
	 case cerl:atom_val(Attribute) of
	   language ->
	     case cerl:concrete(Value) of
	       [erlang] -> true;
	       _ -> false
	     end;
	   _ -> false
	 end
     end, Attributes).

print_module_calls(Code,CR) ->
  print_if_or_log
    (CR#compile_rec.verbose,
     "Functions in module ~s have calls to modules:~n  ~p~n",
     [mce_erl_coreTrans:pp(cerl:module_name(Code)),
      mce_erl_sef_analysis:module_calls_modules(Code)]).

%% @private
get_all_sources(CR) ->
  dict:to_list
    (lists:foldl
     (fun ({SourceSpec,Options},Files) ->
	  case proplists:get_bool(recursive,Options) of
	    true ->
	      case is_directory(SourceSpec) of
		false ->
		  io:format
		    ("*** Error: ~p is not a directory ***~n",[SourceSpec]),
		  throw({compile_error,{not_directory,SourceSpec}});
		true ->
		  add_files(get_all_erlang_files(SourceSpec,true),Files,Options)
	      end;
	    false ->
	      add_files(get_file_or_directory(SourceSpec),Files,Options)
	  end
      end, CR#compile_rec.lib_dict, CR#compile_rec.sources)).

get_file_or_directory(SourceSpec) ->
  case is_directory(SourceSpec) of
    false ->
      case is_file(SourceSpec) of
	true ->
	  [SourceSpec];
	false ->
	  io:format
	    ("*** Error: file/directory ~p does not exist ***~n",[SourceSpec]),
	  throw({compile_error,{missing,file,SourceSpec}})
      end;
    true ->
      get_all_erlang_files(SourceSpec,false)
  end.

add_files(Files,FileDict,Options) ->
  lists:foldl
    (fun (File,FS) ->
	 dict:store(getBaseName(File),{File,Options},FS)
     end, FileDict, Files).

is_directory(SourceSpec) ->
  filelib:is_dir(SourceSpec).

is_file(SourceSpec) ->
  filelib:is_file(SourceSpec) and not filelib:is_dir(SourceSpec).

is_erlang_file(FileSpec) ->
  filename:extension(FileSpec)==".erl".

get_all_erlang_files(DirectorySpec,Recursively) ->
  filelib:fold_files
    (DirectorySpec,
     ".*",
     Recursively,
     fun (F,Files) ->
	 case is_erlang_file(F) of
	   true -> [F|Files];
	   false -> Files
	 end
     end,
     []).

%%parse_file(FileSpec,IncludePath) ->
%%  case epp:parse_file(FileSpec, IncludePath, []) of
%%    {ok,Forms} ->
%%      case erl_lint:module(Forms,FileSpec) of
%%	{ok,_Warnings} ->
%%	  ok;
%%	{error,Errors,_Warnings} ->
%%	  lists:foreach
%%	    (fun ({FileName,FileErrors}) ->
%%		 lists:foreach
%%		   (fun (ErrorInfo) ->
%%			{ErrorLine,Module,ErrorDescriptor} = ErrorInfo,
%%			ErrorString = Module:format_error(ErrorDescriptor),
%%			io:format
%%			  ("~s:~p: ~s~n",
%%			   [FileName,ErrorLine,ErrorString])
%%		    end, FileErrors)
%%	     end, Errors),
%%	  throw({compile_error,{parse_error,FileSpec}})
%%      end;
%%    {error,E} ->
%%      io:format("Error: ~p~n",[E]),
%%      throw({compile_error,{parse_error,FileSpec}})
%%  end.

getBaseName(FileName) ->
  filename:rootname(filename:basename(FileName)).

oneBoolValue(ArgumentName,Arguments) ->
  case oneValue(proplists:get_value(ArgumentName,Arguments)) of
    "true" -> true;
    "false" -> false;
    Value ->
      io:format
	("*** Error: argument ~p has value ~p in ~p~n",
	 [ArgumentName,Value,Arguments]),
      throw({compile_error,{argument,ArgumentName,Value}})
  end.

oneValue(ArgumentName,Arguments) ->
  oneValue(proplists:get_value(ArgumentName,Arguments)).
oneValue([T|_]) -> T;
oneValue(T) -> T.

check_expected_arguments(Arguments) ->
  lists:foreach
    (fun ({ArgName,_Value}) ->
	 case
	   lists:member
	   (ArgName,
	    [root,progname,home,sname,pa,noshell,output_dir,pz,
	     sources,verbose,unknown_is_snd,unknown_is_rcv,funinfo,
	     include_dirs,recursive,normalize_core,
	     warn_if_blacklisted,permit_blacklisted,
	     sends_are_sefs,verbose,libs]) of
	   true ->
	     ok;
	   false ->
	     io:format
	       ("*** Error: Unknown command argument \"~p\" specified.~n",
		[ArgName]),
	     throw({compile_error,{unknown_argument,ArgName}})
	 end
     end, Arguments).
		   
get_all_lib_sources(Libraries,IsVerbose) ->
  AllLibSources =
    lists:foldr
      (fun (Library, Dict) ->
	   case is_directory(Library) of
	     true ->
	       AllFiles =
		 get_all_erlang_files(Library, false),
	       lists:foldl
		 (fun (FileName, D1) ->
		      dict:store(getBaseName(FileName),
				 {FileName, []},
				 D1)
		  end, Dict, AllFiles);
	     false ->
	       case is_file(Library) of
		 true ->
		   dict:store(getBaseName(Library),
			      {Library, []},
			      Dict);
		 false ->
		   io:format("*** Error: cannot add library ~p; " ++
			     "it does not exist~n", [Library]),
		   throw({compile,{cannot_add_library,Library}})
	       end
	   end
       end, dict:new(), Libraries),
  print_if_or_log
    (IsVerbose,
     "AllLibSources=~p~n",
     [dict:to_list(AllLibSources)]),
  AllLibSources.

find_stdlibs() ->
  case mce:get_mcerlang_home() of
    {app_dir,Dir} ->
      [filename:join([Dir,"src","lib","erlang","src"])];
    {env_var,Dir} ->
      [filename:join([Dir,"lib","erlang","src"])]
  end.

find_stdincludes() ->
  case mce:get_mcerlang_home() of
    {app_dir,Dir} ->
      [{i,filename:join([Dir,"include"])},
       {i,filename:join([Dir,"include","erlang","src","include"])}];
    {env_var,Dir} ->
      [{i,filename:join([Dir,"src","include"])},
       {i,filename:join([Dir,"languages","erlang","src","include"])}]
  end.
