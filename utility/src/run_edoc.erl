-module(run_edoc).
-export([run_edoc/0]).

run_edoc() ->
  Files =
    os:cmd
      ("find . -name \"*.erl\" -print | grep -v examples | grep -v utility"),
  FileList =
    split(Files),
  edoc:files(FileList,[{dir,"doc/edoc"},{private,false},{application,'McErlang'}]),
  halt().

split("") ->
  [];
split(S) ->
  case string:str(S,"\n") of
    0 -> S;
    N -> 
      LeftStr = string:substr(S,1,N-1),
      RightStr = string:substr(S,N+1),
      [LeftStr|split(RightStr)]
  end.
