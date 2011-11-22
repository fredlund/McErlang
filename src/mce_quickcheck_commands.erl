-module(mce_quickcheck_commands).
-export([run/1]).

run([]) ->
  run([],orddict:new()).

run([],Store) ->
  Store;
run([Command|Rest],Store) ->
  run(Rest,run_command(Command,Store)).

run_command({set,Var,Call},Store) ->
  {call,ModuleName,FunName,Args} = Call,
  bind(Var,do_call(ModuleName,FunName,Args,Store),Store);
run_command({choice,CommandList},Store) ->
  run
    ([mce:choice
      (lists:map
       (fun (Commands) -> {?MODULE,run,[Commands]} end,
	CommandList))],
     Store);
run_command({par,CommandList},Store) ->
  run
    (lists:map
     (fun (Commands) ->
	  {set,void,{call,mcerlang,spawn,[?MODULE,run,[Commands]]}}
      end, CommandList),
     Store).

do_call(ModuleName,FunName,Args,Store) ->
  apply(ModuleName,FunName,replaceArgs(Args,Store)).

bind({var,N},Value,Store) ->
  orddict:store(N,Value,Store);
bind(void,_,Store) ->
  Store.

replaceArgs(Args,Store) ->
  lists:map(fun (Arg) -> replaceArg(Arg,Store) end, Args).

replaceArg({var,N},Store) ->
  {ok,Value} = orddict:find(N,Store),
  replaceArg(Value,Store);
replaceArg({call,ModuleName,FunName,Args},Store) ->
  Value = do_call(ModuleName,FunName,Args,Store),
  replaceArg(Value,Store);
replaceArg(Value,_Store) ->
  Value.

	 
