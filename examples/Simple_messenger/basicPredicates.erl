-module(basicPredicates).
-language(erlang).


-include("erlang/state.hrl").
-include("erlang/process.hrl").
-include("erlang/node.hrl").

-export([logged_on/1,logon/1,logoff/1,send_to/1,message_from/1,message_to/3,message_received/3]).

logged_on(Name) ->
  fun (State,_,_) -> mce_erl:has_probe_state({logged_on,Name},State) end.

logon(Name) ->
 fun (_,Actions,_) ->
  lists:any
    (fun (Action) ->
	 try
	   true = mce_erl_actions:is_probe(Action),
	   logon = mce_erl_actions:get_probe_label(Action),
	   Name = mce_erl_actions:get_probe_term(Action),
	   true
	 catch _:_ -> false end
     end, Actions)
 end.

logoff(Name) ->
  fun (State,Actions,_) ->
      lists:any
	(fun (Action) ->
	     try
	       true = mce_erl_actions:is_probe(Action),
	       logoff = mce_erl_actions:get_probe_label(Action),
	       Pid = getPid(Name,State),
	       Pid = mce_erl_actions:get_probe_term(Action),
	       true
	     catch _:_ -> false end
	 end, Actions)
  end.

send_to(ToName) ->
 fun (_,Actions,_) ->		
     lists:any
       (fun (Action) ->
	    try
	      true = mce_erl_actions:is_probe(Action),
	      message_to = mce_erl_actions:get_probe_label(Action),
	      ToName = mce_erl_actions:get_probe_term(Action),
	      true
	    catch _:_ -> false end
	end, Actions)
 end.



message_from(_Message) ->
  fun(_,Actions,_) ->		
      lists:any
	(fun (Action) ->
	     try
	       true = mce_erl_actions:is_probe(Action),
	       message_from = mce_erl_actions:get_probe_label(Action),
	       true
	     catch _:_ -> false end
	 end, Actions)
  end.

message_to(From,To,Msg) ->
 fun (State,Actions,_) ->		
     lists:any
       (fun (Action) ->
	    try
	      {message_to,{Pid,To,Msg}} = mce_erl_actions:get_probe(Action),
	      Pid == getPid(From,State)
	    catch _:_ -> false end
	end, Actions)
 end.

message_received(AtName,FromName,Message) ->
 fun (State,Actions,_) ->		
     lists:any
       (fun (Action) ->
	    try
	      true = mce_erl_actions:is_probe(Action),
	      message_received = mce_erl_actions:get_probe_label(Action),
	      Pid = getPid(AtName,State),
	      {Pid,FromName,Message} = mce_erl_actions:get_probe_term(Action),
	      true
	    catch _:_ -> false end
	end, Actions)
 end.

getPid(Name,State) ->
  AllProcesses = mce_erl:allProcesses(State), 
  {ok,Pid} = getMessengerPid(State),
  {ok,P} = find_proc(Pid,AllProcesses),
  {value,{PidName,Name}} = lists:keysearch(Name,2,getMessengerState(P)),
  PidName.


getMessengerPid(State) ->
  Nodes = State#state.nodes,
  search_nodes_for_messenger(Nodes).

search_nodes_for_messenger([]) ->
  none;
search_nodes_for_messenger([N|Rest]) ->
  Registered = N#node.registered,
  case mce_utils:find(fun ({HdName,_}) -> messenger==HdName end, Registered) of
    {ok, {_,Pid}} -> {ok, Pid};
    _Other -> search_nodes_for_messenger(Rest)
  end.	

find_proc(Pid,Processes) ->
  mce_utils:find(fun (P) -> Pid==P#process.pid end, Processes).

%% allProcesses(State) ->
%%   lists:flatmap
%%     (fun (Node) -> Node#node.processes end,
%%      State#state.nodes).

getMessengerState(P) -> 
  case mce_erl:match_recv_fun(P#process.expr) of
    {true,{_,_,[State]}} -> State;
    _ -> []
  end.

