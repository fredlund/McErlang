-module(test).

-export([test/0,go_to_floor/3]).
  
test() ->
   mce_ltl_parse:ltl_string2module_and_load
     ("always not(fun test:go_to_floor/3)",never_receives_order).

go_to_floor(_,Actions,_) ->
  lists:any
    (fun (Action) ->
	 case interpret_action(Action) of
	   {f_button,Floor} -> true;
	   {e_button,Elevator,Floor} -> true;
	   _ -> false
	 end
     end, Actions).

interpret_action(Action) ->
  try 
    send = mce_erl_actions:type(Action),
    {notify,Msg} = mce_erl_actions:get_send_msg(Action),
    Msg
  catch _:_ -> unknown end.
	  

