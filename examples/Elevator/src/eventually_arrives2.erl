-module(eventually_arrives2).

-export([p_pred/3,q_pred/3,r_pred/3]).
-export([eventually_arrives2/0]).

eventually_arrives2() ->
  mce_ltl_parse:ltl_string2module_and_load
    ("always(fun run:p_pred/3 => next(eventually fun run:r_pred/3))",
     always_eventually).

p_pred(_,Actions,_) ->
  case
  mce_utils:findret
    (fun (Action) ->
	 case interpret_action(Action) of
	   {f_button,Floor} ->
	     {true,Floor};
	   {e_button,_,Floor} ->
	     {true,Floor};
	   _ ->
	     false
	 end
     end, Actions) of
    no -> false;
    Other -> Other
  end.

q_pred(_,Actions,Floor) ->
  lists:any
    (fun (Action) ->
	 case interpret_action(Action) of
	   {stopped_at,_,Floor} ->
	     true;
	   _ ->
	     false
	 end
     end, Actions).

r_pred(State,_,Floor) ->
  Elevators = mce_erl:get_probe_state({stopped_at,Floor}),
  (ordsets:is_set(Elevators)) andalso (ordsets:size(Elevators)>0).

interpret_action(Action) ->
  case mce_erl_actions:type(Action) of
    send ->
      case mce_erl_actions:get_send_msg(Action) of
	{notify,Msg} -> Msg;
	_ -> unknown
      end;
    _ -> unknown
  end.
