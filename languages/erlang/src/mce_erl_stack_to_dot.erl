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

-module(mce_erl_stack_to_dot).
-export([stats_from_result/2]).

-include("state.hrl").
-include("process.hrl").
-include("node.hrl").
-include("monState.hrl").
-include("monitor.hrl").
-include("stackEntry.hrl").

-record(statistics,{first_pid=void,links=[],monitors=[],monitor_nodes=[],spawns=[],comms=[],registers=[],limit=void,cmd}).
-record(procinfo,{pid,internal_name}).
-record(spawninfo,{pid,spawned,initial_function_call=void,trapping_exits=false}).

stats_from_result(Result,FileName) ->
  case mce_result:is_mce_result(Result) of
    false ->
      io:format("*** error: result is not well formed~n"),
      throw(bad);
    true ->
      Stack =
	mce_result:stack(Result),
      AllEvents =
	lists:reverse(all_actions(Stack)),
      if
	AllEvents=:=[] ->
	  io:format
	    ("*** error: no events in stack; "++
	     "ensure that the option record_actions is set to true~n"),
	  throw(bad);
	true ->
	  [FirstEvent|_] = AllEvents,
	  FirstPid =
	    case mce_erl_actions:get_source(FirstEvent) of
	      Pid={pid,_,_} ->
		Pid;
	      _ ->
		void
	    end,
	  Stats =
	    changeState(AllEvents,#statistics{first_pid=FirstPid}),
	  io:format("Stats are~n~p~n",[Stats]),
	  io:format("Spawns...~n",[]),
	  try_com
	    (FileName++"_spawned.dot",
	     fun() -> spawn_2_dot(Stats) end,"spawn"),
	  io:format("Comms...~n",[]),
	  try_com
	    (FileName++"_comms.dot",
	     fun() -> sends_2_dot(Stats) end,"communications"),
	  io:format("Links...~n",[]),
	  try_com
	    (FileName++"_linked.dot",
	     fun() -> links_2_dot(Stats) end,"link")
       end
   end.

 all_actions(Stack) ->
   case mce_behav_stackOps:is_empty(Stack) of
     true ->
       [];
     false ->
       {Entry,Rest} = mce_behav_stackOps:pop(Stack),
       (Entry#stackEntry.actions)++all_actions(Rest)
   end.

try_com(FileName,Command,DiagramType) ->
  {ok,File} = file:open(FileName,[write]),
  try
    io:format(File,"~n~s~n",[Command()]),
    ok = file:close(File)
  catch Error:Reason ->
    io:format
      ("error ~p:~p generating ~s diagram~nStacktrace:~n~p~n",
       [Error,Reason,DiagramType,erlang:get_stacktrace()]),
    file:close(File),
    throw(error)
  end.

changeState(void,Stats) ->
  Stats;
changeState([],Stats) ->
  Stats;
changeState([Action|Rest],Stats) ->
  changeState(Rest,changeActionState(Action,Stats)).

changeActionState(Action,Stats) ->
  Act = mce_erl_actions:type(Action),
  Pid = mce_erl_actions:get_source(Action),
  %%io:format("Examing action ~p~n",[Action]),
  case Act of
    spawn ->
      io:format
	("Found a spawn with args ~p~n",
	 [mce_erl_actions:get_spawn_arguments(Action)]),
      [SpawnArgs,Options] = mce_erl_actions:get_spawn_arguments(Action),
      SpawnedPid = mce_erl_actions:get_spawn_result(Action),
      NewSpawn =
	#spawninfo{pid=Pid,spawned=SpawnedPid,initial_function_call=SpawnArgs},
      NewStats =
	Stats#statistics
	  {spawns=[NewSpawn|Stats#statistics.spawns]},
      NewStats1 =
	case lists:member(link,Options) of
	  true ->
	    NewStats#statistics
	      {links=[{Pid,SpawnedPid}|
		      NewStats#statistics.links]};
	  false ->
	    NewStats
	end,
	case lists:member(monitor,Options) of
	  true ->
	    NewStats1#statistics
	      {monitors=[{Pid,SpawnedPid}|
			 NewStats#statistics.monitors]};
	  false ->
	    NewStats1
	end;
    send ->
      ToPid = mce_erl_actions:get_send_resolved_pid(Action),
      Stats#statistics
	{comms=updateSends(Pid,ToPid,Stats#statistics.comms)};
    %%% We have to check for replies to old spawns
    output ->
      Signal = mce_erl_actions:get_output_signal(Action),
      %%io:format("Signal is ~p~n",[Signal]),
      case Signal of
	{signal,_,{message,SpawnPid,{hasSpawned,SpawnedPid}}} ->
	  %%io:format("Found a remote spawn!!!~n",[]),
	  Stats#statistics
	    {spawns=
	     rewrite_spawns(SpawnPid,SpawnedPid,Stats#statistics.spawns),
	    links=
	     rewrite_pairs(SpawnPid,SpawnedPid,Stats#statistics.links),
	    monitors=
	     rewrite_pairs(SpawnPid,SpawnedPid,Stats#statistics.monitors)};
	_ ->
	  Stats
      end;
    api_call ->
      io:format
	("Found an api_call with args ~p~n",
	 [mce_erl_actions:get_api_call_arguments(Action)]),
      case mce_erl_actions:get_api_call_fun(Action) of
	register ->
	  [Name,RPid] = mce_erl_actions:get_api_call_arguments(Action),
	  Stats#statistics
	    {registers=[{RPid,Name}|Stats#statistics.registers]};
	link ->
	  [LinkTo] = mce_erl_actions:get_api_call_arguments(Action),
	  Stats#statistics
	    {links=[{Pid,LinkTo}|Stats#statistics.links]};
	monitor ->
	  [_,MonitorPid] = mce_erl_actions:get_api_call_arguments(Action),
	  Stats#statistics
	    {monitors=[{Pid,MonitorPid}|Stats#statistics.monitors]};
	monitor_node ->
	  [Node,Flag] = mce_erl_actions:get_api_call_arguments(Action),
	  if
	    Flag -> 
	      Stats#statistics
		{monitor_nodes=[{Pid,{node,Node}}|
				Stats#statistics.monitor_nodes]};
	    true ->
	      Stats
	  end;
	process_flag ->
	  case mce_erl_actions:get_api_call_arguments(Action) of
	    [trap_exit,true] ->
	      io:format("Adding trap to ~p~n",[Pid]),
	      Stats#statistics
		{spawns=add_trap_exit(Pid,Stats#statistics.spawns)};
	    _ ->
	      Stats
	  end;
	_ ->
	  Stats
      end;
    _ ->
      Stats
  end.

rewrite_spawns(_,_,[]) ->
  [];
rewrite_spawns(Pid,SpawnedPid,[Spawn|Rest]) ->
  if
    Spawn#spawninfo.pid==Pid, Spawn#spawninfo.spawned==waitingForPid ->
      [Spawn#spawninfo{spawned=SpawnedPid}|Rest];
    true ->
      [Spawn|rewrite_spawns(Pid,SpawnedPid,Rest)]
  end.
add_trap_exit(_,[]) ->
  [];
add_trap_exit(Pid,[Spawn|Rest]) ->
  if
    Spawn#spawninfo.spawned==Pid ->
      [Spawn#spawninfo{trapping_exits=true}|Rest];
    true ->
      [Spawn|add_trap_exit(Pid,Rest)]
  end.
rewrite_pairs(Pid1,Pid2,Pairs) ->
  lists:map (fun ({Pid3,Pid4}) ->
		 if
		   Pid1==Pid3, Pid4==waitingForPid -> {Pid1,Pid2};
		   true -> {Pid3,Pid4}
		 end
	     end, Pairs).

updateSends(FromPid,ToPid,[]) ->
  [{FromPid,ToPid}];
updateSends(FromPid,ToPid,[{FromPid,ToPid}|Rest]) ->
  [{FromPid,ToPid}|Rest];
updateSends(FromPid,ToPid,[Element|Rest]) ->
  [Element|updateSends(FromPid,ToPid,Rest)].

spawn_2_dot(MonitorState) ->
  Spawned =
    lists:map
      (fun (Spawn) -> {Spawn#spawninfo.pid,Spawn#spawninfo.spawned} end,
       MonitorState#statistics.spawns),
  events_2_dot
    (lists:map(fun ({E1,E2}) -> {E1,E2,void} end, Spawned),
     MonitorState#statistics.spawns,MonitorState).

sends_2_dot(MonitorState) ->
  Sends =
    MonitorState#statistics.comms,
  events_2_dot
    (lists:map(fun ({E1,E2}) -> {E1,E2,void} end, Sends),
     MonitorState#statistics.spawns,MonitorState).

links_2_dot(MonitorState) ->
  Links =
    lists:map
      (fun ({From,To}) -> {From,To,link} end, MonitorState#statistics.links),
  Monitors =
    lists:map
      (fun ({E1,E2}) -> {E1,E2,monitor} end, MonitorState#statistics.monitors),
  Monitor_Nodes =
    lists:map
      (fun ({E1,E2}) -> {E1,E2,monitor_node} end, MonitorState#statistics.monitor_nodes),
  events_2_dot
    (Links++Monitors++Monitor_Nodes,
     MonitorState#statistics.spawns,
     MonitorState).

events_2_dot(Events,Spawned,Stats) ->
  put(counter,0),
  Register =
    create_register(Stats#statistics.registers),
  io:format("Events=~p~nRegister=~p~n",[Events,Register]),
  Nodes =
    lists:foldl
      (fun ({Pid1,Pid2,_},Nodes) ->
	   maybe_add_node
	     (get_node(Pid1),
	      maybe_add_node
	      (get_node(Pid2),
	       Nodes))
       end, [], Events),
  io:format("Nodes=~p~n",[Nodes]),
  NodesProcesses =
    lists:foldl
      (fun ({Pid1,Pid2,_},NodesProcs) ->
	   maybe_add_proc
	     (Pid1,
	      maybe_add_proc(Pid2,NodesProcs,Spawned,Register),
	      Spawned,Register)
       end, [{Node,[]} || Node <- Nodes], Events),
  io:format("NodesProcesses=~p~n",[NodesProcesses]),
  ["digraph G {\ncompound=true;\n",
   (lists:map
    (fun (NP) -> cluster(NP,Register,Spawned,Stats) end,
     NodesProcesses)),
   (lists:map
    (fun (Event) -> transition(Event,NodesProcesses) end,
     adjust_events(Events,Register))),
   "}\n"].
  
get_node({pid,N,_}) ->
  N;
get_node({_,N}) ->
  N.

adjust_events(Events,Register) ->
  adjust_events(Events,Register,[]).
adjust_events([],_,NewEvents) ->
  io:format("Events are ~p~n",[NewEvents]),
  lists:usort(NewEvents);
adjust_events([{Pid,{node,NodeId},Flag}|Rest],Register,NewEvents) ->
  case resolve_pid(Pid,Register) of
    {ok,RealPid} ->
      adjust_events(Rest,Register,[{RealPid,{node,NodeId},Flag}|NewEvents]);
    _ ->
      io:format("*** failed to find pid ~p~n",[Pid]),
      adjust_events(Rest,Register,NewEvents)
  end;
adjust_events([{Pid1,Pid2,Flag}|Rest],Register,NewEvents) ->
  case resolve_pid(Pid1,Register) of
    {ok,RealPid1} ->
      case resolve_pid(Pid2,Register) of
	{ok,RealPid2} ->
	  adjust_events(Rest,Register,[{RealPid1,RealPid2,Flag}|NewEvents]);
	_ ->
	  io:format("*** failed to find pid ~p~n",[Pid2]),
	  adjust_events(Rest,Register,NewEvents)
      end;
    _ ->
      io:format("*** failed to find pid ~p~n",[Pid1]),
      adjust_events(Rest,Register,NewEvents)
  end.

cluster({{Node,Count},Procs},Register,Spawned,Stats) ->
  "subgraph cluster"++integer_to_list(Count)++" { \n"++
  (lists:map
   (fun (P) -> proc_id(P)++proc_label(P,Register,Spawned,Stats)++";\n" end, 
    Procs))++
    "label = \""++atom_to_list(Node)++"\";\n"++
    "}\n".

transition({Pid,{node,NodeId},Flag},NodesProcesses) ->
  (proc_id(find_proc(Pid,NodesProcesses)))++
    " -> "++
  (node_id(NodeId,NodesProcesses))++
    (attributes(Flag))++
    ";\n";
transition({Pid1,Pid2,Flag},NodesProcesses) ->
  (proc_id(find_proc(Pid1,NodesProcesses)))++
    " -> "++
  (proc_id(find_proc(Pid2,NodesProcesses)))++
    (attributes(Flag))++
    ";\n".

attributes(link) ->
  " [dir=both,arrowhead=vee,color=red]";
attributes(monitor) ->
  " [color=green]";
attributes(monitor_node) ->
  " [color=blue]";
attributes(_) ->
  "".

find_eq_processes(P,Register) ->
  Pid = P#procinfo.pid,
  case find_pid_in_register(Pid,Register) of
    {ok,Eqc} -> Eqc;
    _ -> [{Pid,void}]
  end.

arity(Eqc) ->
  length(Eqc).

find_node([{{pid,Node,_},_}|_]) ->
  Node.

find_name(Eqc) ->
  Names = lists:map (fun ({_,Name}) -> Name end, Eqc),
  case return_unique(Names) of
    void ->
      find_smallest_pid(Eqc);
    Other ->
      Other
  end.

find_smallest_pid(Eqc) ->
  find_smallest_pid(Eqc,void).

find_smallest_pid([],Value) ->
  Value;
find_smallest_pid([{{pid,_,N},_}|Rest],void) ->
  find_smallest_pid(Rest,{pid,N});
find_smallest_pid([{{pid,_,N},_}|Rest],Name={pid,N1}) -> 
  if
    N<N1 ->
      find_smallest_pid(Rest,{pid,N});
    true ->
      find_smallest_pid(Rest,Name)
  end.

find_module(Eqc,Spawned) ->
  Modules = lists:map (fun ({Pid,_}) -> find_module1(Pid,Spawned) end, Eqc),
  return_unique(Modules).

find_submodule(Eqc,Spawned) ->
  SubModules = lists:map (fun ({Pid,_}) -> find_submodule1(Pid,Spawned) end, Eqc),
  return_unique(SubModules).

find_trapping(Eqc,Spawned) ->
  Trapping = lists:map (fun ({Pid,_}) -> find_trapping1(Pid,Spawned) end, Eqc),
  return_unique(Trapping).

find_module1(Pid,Spawned) ->
  case find_spawned_proc(Pid,Spawned) of
    {ok,P} ->
      case P#spawninfo.initial_function_call of
	{Module,_,_} ->
	  mod_name_subst(Module);
	{Fun,_} ->
	  {module,Mname} = erlang:fun_info(Fun,module),
	  mod_name_subst(Mname);
	_ ->
	  void
      end;
    _ ->
      void
  end.

find_submodule1(Pid,Spawned) ->
  case find_spawned_proc(Pid,Spawned) of
    {ok,P} ->
      case P#spawninfo.initial_function_call of
	{Module,_,Args} ->
	  case lists:member(Module,
			    [mce_erl_gen_event,mce_erl_supervisor,
			     mce_erl_gen_fsm,mce_erl_gen_server]) of
	    true ->
	      hd(Args);
	    false ->
	      void
	  end;
	_ ->
	  void
      end;
    _ ->
      void
  end.

find_trapping1(Pid,Spawned) ->
  case find_spawned_proc(Pid,Spawned) of
    {ok,P} ->
      P#spawninfo.trapping_exits;
    _ ->
      false
  end.

return_unique(L) ->
  io:format("return_unique: ~p~n",[L]),
  case lists:usort(L) of
    [Element] ->
      io:format("element ~p~n",[Element]),
      Element;
    _ ->
      void
  end.

proc_label(P,Register,Spawned,Stats) ->
  Eqc = find_eq_processes(P,Register),
  Name = find_name(Eqc),
  Node = find_node(Eqc),
  io:format("Name of ~p is ~p~n",[Eqc,Name]),
  InitModule = find_module(Eqc,Spawned),
  SubModule = find_submodule(Eqc,Spawned),
  Trapping = find_trapping(Eqc,Spawned),
  TrapStr =
    if
      Trapping ->
	"shape=polygon,";
      true ->
	"shape=ellipse,"
    end,
  IsFirstPid =
    case Name of
      {pid,N} ->
	case Stats#statistics.first_pid of
	  {pid,Node,N} ->
	    true;
	  _ ->
	    false
	end;
      _ -> false
    end,
  ShapeStr =
    case {IsFirstPid,arity(Eqc)>1} of
      {false,true} ->
	TrapStr++"style=\"filled,dotted\",fillcolor=yellow";
      {false,false} ->
	TrapStr++"style=filled,fillcolor=yellow";
      {true,true} ->
	TrapStr++"style=\"filled,dotted\",fillcolor=green";
      {true,false} ->
	TrapStr++"style=filled,fillcolor=green"
    end,
  case Name of
    {pid,N1} ->
      Nstr = integer_to_list(N1),
      case {InitModule,SubModule} of
	{void,void} ->
	  " [label=\""++Nstr++"\", "++ShapeStr++"]";
	{M,void} ->
	  " [label=\""++Nstr++":"++atom_to_list(M)++"\", "++ShapeStr++"]";
	{M,Sub} ->
	  " [label=\""++Nstr++":"++atom_to_list(Sub)++" ("++atom_to_list(M)++")\","++ShapeStr++"]"
      end;	  
    _ ->
      case {InitModule,SubModule} of
	{void,void} ->
	  " [label=\""++atom_to_list(Name)++"\","++ShapeStr++"]";
	{M,_} ->
	  " [label=\""++atom_to_list(Name)++" ("++atom_to_list(M)++")\","++ShapeStr++"]"
      end
  end.

mod_name_subst(mce_erl_timer) ->
  timer;
mod_name_subst(mce_erl_gen_event) ->
  gen_event;
mod_name_subst(mce_erl_supervisor) ->
  supervisor;
mod_name_subst(mce_erl_gen_fsm) ->
  gen_fsm;
mod_name_subst(mce_erl_gen_server) ->
  gen_server;
mod_name_subst(M) ->
  M.

proc_id(Proc) ->
  integer_to_list(Proc#procinfo.internal_name).

node_id(NodeId,[{{NodeId,Counter},[P|_]}|_]) ->
  integer_to_list(P#procinfo.internal_name)++
  " [lhead=cluster"++integer_to_list(Counter)++"]".

find_proc(Pid,[{Node,Processes}|Rest]) ->
  case mcerlang:node(Pid)==node_name(Node) of
    true -> find_proc_in_node(Pid,Processes);
    false -> find_proc(Pid,Rest)
  end.

find_proc_in_node(Pid,[Proc|Rest]) ->
  if
    Pid==Proc#procinfo.pid ->
      Proc;
    true ->
      find_proc_in_node(Pid,Rest)
  end.

resolve_pid(Pid,Register) ->
  case find_pid_in_register(Pid,Register) of
    {ok,Eqc} ->
      {ok,find_representative_pid(Eqc)};
    _ ->
      case Pid of
	{pid,_,_} -> {ok,Pid};
	_ -> false
      end
  end.

maybe_add_node(NodeName,Nodes) ->
  case lists:any(fun ({Node,_}) -> NodeName==Node end, Nodes) of
    true -> Nodes;
    false -> [{NodeName,counter()}|Nodes]
  end.

node_name({NodeName,_}) ->
  NodeName.

maybe_add_proc(Pid,[{Node,Processes}|Rest],Spawned,Register) ->
  case resolve_pid(Pid,Register) of
    {ok,RealPid} ->
      case mcerlang:node(RealPid)==node_name(Node) of
	true ->
	  case search_pid(RealPid,Processes) of
	    true ->
	      [{Node,Processes}|Rest];
	    false ->
	      P = #procinfo{pid=RealPid,internal_name=counter()},
	      [{Node,[P|Processes]}|Rest]
	  end;
	false ->
	  [{Node,Processes}|
	   maybe_add_proc(RealPid,Rest,Spawned,Register)]
      end;
    _ -> [{Node,Processes}|Rest]
  end.

search_pid(_,[]) ->
  false;
search_pid(Pid,[P|Rest]) ->
  if
    Pid==P#procinfo.pid -> true;
    true -> search_pid(Pid,Rest)
  end.

find_spawned_proc(_,[]) -> 
  no;
find_spawned_proc(Pid,[Proc|Rest]) ->
  if
    Pid==Proc#spawninfo.spawned ->
      {ok,Proc};
    true ->
      find_spawned_proc(Pid,Rest)
  end.

counter() ->	  
  case get(counter) of
    undefined ->
      put(counter,1),
      0;
    N ->
      put(counter,N+1),
      N
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_register(RegisterEvents) ->
  create_register(RegisterEvents,[]).

find_pid_in_register(_Pid,[]) ->
  false;
find_pid_in_register(Pid,[{Node,EqClasses}|Rest]) ->
  case get_node(Pid)==Node of
    true ->
      SearchFun = 
	case Pid of
	  {pid,_,_} -> fun ({Pid1,_}) -> Pid==Pid1 end;
	  {Name,_} -> fun ({_,Name1}) -> Name==Name1 end
	end,
      find_pid_in_eqclasses(SearchFun,EqClasses);
    false ->
      find_pid_in_register(Pid,Rest)
  end.

find_representative_pid([{Pid,_}|_]) ->
  Pid.

find_pid_in_eqclasses(_,[]) ->
  false;
find_pid_in_eqclasses(SearchFun,[Class|Rest]) ->
  case lists:any(SearchFun,Class) of
    true ->
      {ok,Class};
    _ ->
      find_pid_in_eqclasses(SearchFun,Rest)
  end.

create_register([],R) ->
  R;
create_register([{Pid,Name}|Rest],R) ->
  create_register(Rest,add_register(Pid,Name,R)).

add_register(Pid,Name,[]) ->
  [{mcerlang:node(Pid),[[{Pid,Name}]]}];
add_register(Pid,Name,[{Node,EqClasses}|Rest]) ->
  case mcerlang:node(Pid)==Node of
    true ->
      [{Node,add_register_to_node(Pid,Name,EqClasses)}|Rest];
    false ->
      [{Node,EqClasses}|add_register(Pid,Name,Rest)]
  end.

add_register_to_node(Pid,Name,EqClasses) ->
  {AllRelatedPids,OtherClasses1} = find_same_pid(Pid,Name,EqClasses,[]),
  {AllRelatedNames,OtherClasses} = find_same_name(Pid,Name,OtherClasses1,[]),
  MergedClass = AllRelatedPids++AllRelatedNames,
  if
    MergedClass==[] ->
      [[{Pid,Name}]|EqClasses];
    true ->
      [MergedClass|OtherClasses]
  end.

find_same_pid(_Pid,_Name,[],Unrelated) ->
  {[],Unrelated};
find_same_pid(Pid,Name,[Class|Rest],Unrelated) ->
  case lists:any(fun ({Pid1,_}) -> Pid==Pid1 end, Class) of
    true ->
      {[{Pid,Name}|Class],Rest++Unrelated};
    false ->
      find_same_pid(Pid,Name,Rest,[Class|Unrelated])
  end.

find_same_name(_Pid,_Name,[],Unrelated) ->
  {[],Unrelated};
find_same_name(Pid,Name,[Class|Rest],Unrelated) ->
  case lists:any(fun ({_,Name1}) -> Name==Name1 end, Class) of
    true ->
      {[{Pid,Name}|Class],Rest++Unrelated};
    false ->
      find_same_name(Pid,Name,Rest,[Class|Unrelated])
  end.
    
