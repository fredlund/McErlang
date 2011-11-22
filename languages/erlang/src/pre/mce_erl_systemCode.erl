%% @author Lars-Ake Fredlund
%% @copyright 2006-2009 Lars-Ake Fredlund
%% @doc
%% @private

-module(mce_erl_systemCode).
-export([waitForSpawningNode/2]).
-export([waitForReply/0]).
-export([send_after/3]).

%%% Receives the message about a successful process spawning on a remote node
waitForSpawningNode(DoLink,DoMonitor) ->
  receive
    {hasSpawned,RemotePid} -> 
      if
	DoLink -> link(RemotePid);
	true -> ok
      end,
      if
	DoMonitor -> mcerlang:monitor(process,RemotePid);
	true -> ok 
      end,
      RemotePid
  end.

waitForReply() ->
  receive
    {nodeReply,Reply} ->
      check_reply(Reply)
  end.

check_reply({value,Value}) ->
  Value;
check_reply({exception,Type,Reason}) ->
  erlang:Type(Reason).

%%% Used for implementing erlang:send_after/3
send_after(Time,Dest,Msg) ->
  receive
    cancel_timer ->
      ok
    after Time ->
	Dest!Msg
  end.
      
