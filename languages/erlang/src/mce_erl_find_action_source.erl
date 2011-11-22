-module(mce_erl_find_action_source).

-export([find_transition_cause/2]).

%%-define(debug,true).
-include("../../../src/include/macros.hrl").
-include("emacros.hrl").

-include("stackEntry.hrl").

find_transition_cause(Actions,Stack) ->
  case Actions of
    [Action|_] ->
      ?LOG
	("checking action ~p~ntype ~p~n",
	 [Action,mce_erl_actions:type(Action)]),
      case mce_erl_actions:type(Action) of
	recv ->
	  RecvRecognizer =
	    fun (Act) -> Act=:=Action end,
	  ActionRecognizer =
	    fun (Act) ->
		Msg = mce_erl_actions:get_recv_msg(Action),
		Source = mce_erl_actions:get_source(Action),
		case mce_erl_actions:type(Act) of
		  send ->
		    (mce_erl_actions:get_send_msg(Act)=:=Msg
		     andalso 
		     mce_erl_actions:get_send_resolved_pid(Act)=:=Source);
		  deliver ->
		    (mce_erl_actions:get_deliver_pid(Act)=:=Source)
		      andalso
			(mce_erl_actions:get_deliver_msg(Act)=:=Msg);
		  _ ->
		    false
		end
	    end,
	  search_for_action(ActionRecognizer,RecvRecognizer,Stack);
	input ->
	  Signal = mce_erl_actions:get_input_signal(Action),
	  From = mce_erl_actions:get_input_from(Action),
	  To = mce_erl_actions:get_input_to(Action),
	  ?LOG
	    ("Found input signal ~p, stack depth=~p~n",
	     [Action,get_depth(Stack)]),
	  InputRecognizer = 
	    fun (Act) ->
		case mce_erl_actions:type(Act) of
		  input ->
		    (From == mce_erl_actions:get_input_from(Act)) 
		      andalso
			(To == mce_erl_actions:get_input_to(Act)) 
		      andalso
			(Signal == mce_erl_actions:get_input_signal(Act));
		  _ ->
		    false
		end
	    end,
	  ActionRecognizer =
	    fun (Act) ->
		case mce_erl_actions:type(Act) of
		  output ->
		    ?LOG
		       ("~p=?=~p ~p=?=~p ~n~p~n=?=~n~p~n",
			[mce_erl_actions:get_output_from(Act),From,
			 mce_erl_actions:get_output_to(Act),To,
			 mce_erl_actions:get_output_signal(Act),Signal]),
		    (From == mce_erl_actions:get_output_from(Act)) 
		      andalso
			(To == mce_erl_actions:get_output_to(Act)) 
		      andalso
			(Signal == mce_erl_actions:get_output_signal(Act));
		  _ -> false
		end
	    end,
	  ?LOG("Search: from=~p to=~p Signal=~n~p~n",[From,To,Signal]),
	  search_for_action(ActionRecognizer,InputRecognizer,Stack);
	_ -> ok
      end;
    _ -> no
  end.

actions(Stack) ->
  case mce_behav_stackOps:is_empty(Stack) of
    true -> 
      [];
    false ->
      {Entry,_} = mce_behav_stackOps:pop(Stack),
      Entry#stackEntry.actions
  end.
      
search_for_action(ActionRec,IdRec,Stack) ->
  search_for_action(1,ActionRec,IdRec,Stack).

search_for_action(N,ActionRec,IdRec,Stack) ->
  ?LOG("Looking for match at depth ~p~n",[get_depth(Stack)]),
  Actions = actions(Stack),
  case actions(Stack) of
    [] ->
      case mce_behav_stackOps:is_empty(Stack) of
	true ->
	  io:format("Stack does not contain originating action~n",[]);
	false ->
	  {_,NewStack} = mce_behav_stackOps:pop(Stack),
	  search_for_action(N,ActionRec,IdRec,NewStack)
      end;
    [StackAction|RestActions] ->
      ActType =
	case mce_erl_actions:is_input(StackAction) of
	  true -> node;
	  false -> process
	end,
      case IdRec(StackAction) of
	true ->
	  search_for_action(N+1,ActionRec,RestActions,IdRec,ActType,Stack);
	false ->
	  search_for_action(N,ActionRec,Actions,IdRec,ActType,Stack)
      end
  end.
  
search_for_action(N,ActionRec,[],IdRec,ActType,Stack) ->
  case mce_behav_stackOps:is_empty(Stack) of
    true ->
      io:format("Stack does not contain originating action~n",[]);
    false ->
      {_,NewStack} = mce_behav_stackOps:pop(Stack),
      search_for_action(N,ActionRec,IdRec,NewStack)
  end;
search_for_action(N,ActionRec,[Action|Rest],IdRec,ActType,Stack) ->
  case ActionRec(Action) of
    true -> 
      if
	N==1 -> 
	  ?LOG
	    ("Found match for obj ~p and depth ~p~nwith action ~p~n",
	     [mce_erl_actions:get_source(Action), get_depth(Stack), Action]),
	  {ActType,mce_erl_actions:get_source(Action), Stack};
	true -> 
	  search_for_action(N-1,ActionRec,Rest,IdRec,ActType,Stack)
      end;
    false ->
      ?LOG("Action~n~p~ndoes not match~n",[Action]),
      search_for_action(N,ActionRec,Rest,IdRec,ActType,Stack)
  end.
  
get_depth(Stack) ->
  case mce_behav_stackOps:is_empty(Stack) of
    true -> -1;
    false ->
      {Entry, _} = mce_behav_stackOps:pop(Stack),
      Entry#stackEntry.depth
  end.

    
