-module(eventually_arrives).
-export([get_prop/0]).
-export([go_to_floor/3,stopped_at_floor/3]).

get_prop() ->
  mce_ltl_parse:ltl_string2module_and_load
    ("always(fun eventually_arrives:go_to_floor/3 => next(eventually fun eventually_arrives:stopped_at_floor/3))",
     always_eventually).

go_to_floor(_,Actions,_) ->
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

stopped_at_floor(_,Actions,Floor) ->
  lists:any
    (fun (Action) ->
	 case interpret_action(Action) of
	   {stopped_at,_,Floor} ->
	     true;
	   _ ->
	     false
	 end
     end, Actions).

interpret_action(Action) ->
  try 
    send = mce_erl_actions:type(Action),
    {notify,Msg} = mce_erl_actions:get_send_msg(Action),
    Msg
  catch _:_ -> unknown end.



