%% @doc
%% @private

-module(mce_erl_timer).

-export([apply_after/4,send_after/2,send_after/3,cancel/1]).

apply_after(Time, Module, Function, Arguments) ->
  return_timer(Time, fun () -> erlang:apply(Module,Function,Arguments) end).

send_after(Time, Message) ->
  send_after(Time, self(), Message).

send_after(Time, Pid, Message) ->
  return_timer(Time, fun () -> erlang:send(Pid,Message) end).

return_timer(Time, Fun) ->
  Reference = erlang:make_ref(),
  Pid =
  spawn
    (fun () -> 
	 receive
	   {cancel,_Client,Reference} -> ok
	 after Time -> Fun()
	 end
     end),
  {ok,{timer,Pid,Reference}}.

cancel({timer,Pid,Reference}) ->
  Pid!{cancel,self(),Reference},
  {ok,cancel}.
  
  
	   
	     
	 
  

