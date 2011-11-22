-module(scenario).
-export([start/1,start_clients/2,execute_commands/1]).

%% start([[{logon,clara},{message,fred,"hi"},logoff],
%%        [{logon,fred},{message,clara,"hello"},logoff]]).

start(Commands) ->
  spawn(server_node,messenger,start_server,[]),
  start_clients(Commands,1).

start_clients([],_N) -> ok;
start_clients([Commands|Rest],N) ->
  Node = list_to_atom("n"++integer_to_list(N)),
  spawn(Node,?MODULE,execute_commands,[Commands]),
  start_clients(Rest,N+1).

execute_commands(Commands) ->
  mce_erl:probe(after_starting_server),
  lists:foreach
    (fun (Command) ->
         case Command of
	   {logon, Client} ->
	     messenger:logon(Client);
	   {message,Receiver,Msg} ->
	     messenger:message(Receiver,Msg);
	   logoff ->
	     messenger:logoff()
         end
     end, Commands).



