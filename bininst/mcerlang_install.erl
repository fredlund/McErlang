%% Install McErlang in the Erlang lib directory.  This program
%% should be run in the root directory of a McErlang distribution,
%% which ought to contain a mcerlang-xxx directory.

%% Inspired by eqc_install for the QuickCheck tool
%% (thanks to John Hughes for his kind assistance).

-module(mcerlang_install).
-export([install/0, install/1]).

install() ->
  Erlang = code:where_is_file("erlang.beam"),
  Ebin = filename:dirname(Erlang),
  Erts = filename:dirname(Ebin),
  Lib = filename:dirname(Erts),
  install(Lib).

install(Lib) ->
  io:format("Installation program for McErlang.~n~n",[]),
  {ok,Dir} = find_mcerlang_distribution(),
  ToDelete = conflicts(Lib,filename:basename(Dir)),
  Version = version(Dir),
  io:format("This will install ~s~nin the directory ~s~n",[Version,Lib]),
  if
    ToDelete=/=[] ->
      io:format
	("This will delete conflicting versions of McErlang, namely\n"++
	 "    ~p\n",
	 [ToDelete]);
    true ->
      ok
  end,
  case io:get_line("Proceed? ") of
    "y\n" ->
      delete_conflicts(ToDelete),
      install(Lib,Dir);
    _ ->
      io:format("Cancelling install--answer \"y\" at this point to proceed.\n"),
      throw(installation_cancelled)
  end.

conflicts(Lib,Dir) ->
  FullDir = Lib++"/"++Dir,
  case file:read_file_info(FullDir) of
    {ok,_} ->
      [FullDir];
    _ ->
      []
  end.

find_mcerlang_distribution() ->
  OwnLocation = filename:dirname(code:which(?MODULE)),
  {ok,Files} = file:list_dir(OwnLocation),
  MatchingFiles =
    lists:foldl
      (fun (FileName,AccFound) ->
	   case {string:str(FileName,"mcerlang-"),filelib:is_dir(FileName)} of
	     {N,true} when N=/=0 -> [FileName|AccFound];
	     _ -> AccFound
	   end
       end, [], Files),
  case MatchingFiles of
    [Dir] ->
      {ok,OwnLocation++"/"++Dir};
    [] -> 
      io:format
	("*** Error: cannot find McErlang to install.~n"++
	 "There should be a directory named ``mcerlang-...'' in ~s.~n",
	 [OwnLocation]),
      throw(mcerlang_not_found);
    [_|_] ->
      io:format
	("*** Error: multiple McErlang versions available to install in ~s.~n"++
	 [OwnLocation]),
      throw(mcerlang_not_found)
  end.

version(Dir) ->
  case code:is_loaded(mce_version) of
    {file,_} ->
      mce_version:version();
    false ->
      case code:load_abs(Dir++"/ebin/mce_version") of
	{module,_} ->
	  mce_version:version();
	{error,_} ->
	  "unknown version"
      end
  end.

install(Lib,Dir) ->
  copy_mcerlang(Lib,Dir),
  io:format("McErlang is installed successfully.\n",[]),
  code:add_paths([Lib++"/"++Dir++"/ebin"]).

copy_mcerlang(Lib,Dir) ->
  AppDir = filename:basename(Dir),
  case copy(Dir,Lib++"/"++AppDir) of
    ok ->
      ok;
    eaccess ->
      io:format
	("*** Error: failed to copy McErlang -- "++
	 "rerun as Administrator or superuser?\n",
	 []),
      exit(eaccess);
    {error,eaccess} ->
      io:format
	("*** Error: failed to copy McErlang -- "++
	 "rerun as Administrator or superuser?\n",
	 []),
      exit(eaccess);
    Error ->
      io:format
	("*** Error: failed to copy McErlang -- "++
	 "copy returned~n~p??~n",
	 [Error]),
      exit(Error)
  end.

copy(From,To) ->
  case file:list_dir(From) of
    {ok,Files} ->
      case file:make_dir(To) of
	ok ->
	  lists:foldl
	    (fun (File,ok) ->
		 FromFile = From++"/"++File,
		 ToFile = To++"/"++File,
		 copy(FromFile,ToFile);
		 (_,Status) ->
		 Status
	     end, ok, Files);
	OtherMkDir -> 
	  io:format
	    ("*** Error: failed to create directory ~s due to ~p~n",
	     [To,OtherMkDir]),
	  OtherMkDir
      end;
    _ -> 
      case file:copy(From,To) of
	ok -> ok;
	{ok,_} -> ok;
	OtherCopy -> 
	  io:format
	    ("*** Error: failed to copy ~s to ~s due to ~p~n",
	     [From,To,OtherCopy]),
	  OtherCopy
      end
  end.

delete_conflicts(ToDelete) ->
  lists:foreach
    (fun (Version) ->
	 delete_recursive(Version)
     end, ToDelete).

delete_recursive(F) ->
  case file:list_dir(F) of
    {ok,Files} ->
      lists:foreach
	(fun (File) -> delete_recursive(F++"/"++File) end,
	 Files),
      case file:del_dir(F) of
	ok ->
	  ok;
	Err ->
	  io:format
	    ("*** Error: could not delete directory ~s: ~p\n",
	     [F,Err]),
	  Err
      end;
    _ ->
      case file:delete(F) of
	ok ->
	  ok;
	Err ->
	  io:format
	    ("*** Error: could not delete file ~s: ~p\n",
	     [F,Err]),
	  Err
      end
  end.

