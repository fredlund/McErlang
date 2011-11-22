-module(example). 
-export([start/0]).

start() -> 
  spawn(fun() -> 
           register(echo,self()), echo() 
        end),
  spawn(fun() -> 
           echo!{msg,self(),'hello world'},
           receive
             {echo,Msg} -> Msg
           end
        end).

echo() ->
  receive
    {msg,Client,Msg} ->
      Client!{echo,Msg},
      echo()
  end.
