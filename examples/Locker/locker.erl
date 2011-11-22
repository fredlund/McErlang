%%% File    : locker.erl
%%% Author  : Clara Benac Earle <clara@bacardi>
%%% Purpose : resource manager for exclusive and shared locks
%%% Created :  2 Apr 2001 by Clara Benac Earle <clara@bacardi>
%%% Modified: 08 Jun 2001 by Thomas Arts <thomas@cslab.ericsson.se>

-module(locker).
-author('clara@bacardi').

-behaviour(gen_server).

%%----------------------------------------------------------------------
%% record definitions

%% lock: one for each resource
-record(lock,{resource,         % name of the resource
              excl = none,      % none if no excl lock; pid() otherwise
              shared = [],      % [pid()]: pids holding shared locks
              pending = []}).   % [#pending{}] 

%% pending: requests that have not been served
%%          exclusive requests first, then shared requests,
%%          because exclusive requests have priority
-record(pending,{client,        % {pid(),ref()}: the gen_server query tag
                 type}).        % exclusive | shared

%%-----------------------------------------------------------------------

-import(lists,[member/2,map/2]).

%% External exports
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_info/2]).



%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
%%% Resources is a list of resources. 
 
start_link(Resources,Shared) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {Resources,Shared}, []).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------

% State is a triple containing
% 1) a lock record for every resource
% 2) the list of clients asking for an exclusive requests that
%    have not been served yet (incomming order, oldest first)
% 3) the list of clients asking for a shared requests that
%    have not been served yet (incomming order, oldest first)

init({Resources,_Shared}) ->
%    action:shared(Shared),
    process_flag(trap_exit,true),
    {ok,{map(fun(Name) ->
                #lock{resource = Name}
             end,Resources),[],[]}}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

handle_call({request,Resources,Type},Client,{Locks,Exclusives,Shared}) ->
    io:format("Locks ~p~n",[Locks]),
    io:format("Client ~p requesting ~p ~p~n",[Client,Resources,Type]),
    link(pid(Client)),
    case check_availables(Resources,Type,Locks) of
         true ->
           NewLocks = 
	  map(fun(Lock) -> claim_lock(Lock,Resources,Type,pid(Client)) end,Locks),
	{reply, ok, {NewLocks,Exclusives,Shared}}; 
%%%     {reply, ok, {Locks,Exclusives,Shared}};
        false ->
           NewLocks =
             map(fun(Lock) ->
                    add_pending(Lock,Resources,Type,Client)
                 end,Locks),
           case Type of
                exclusive ->
                  {noreply, {NewLocks,Exclusives ++ [Client],Shared}};
                shared ->
                  {noreply, {NewLocks,Exclusives,Shared ++ [Client]}}
           end
    end;

handle_call(release, Client, {Locks,Exclusives,Shared}) ->
  mce_erl:mcerlang_compiled_caller(?MODULE),
io:format("Client ~p releasing ~n",[Client]),
    Locks1 =
      map(fun(Lock) -> release_lock(Lock,pid(Client)) end,Locks),
    {Locks2,NewExclusives} =
      send_reply(exclusive,Locks1,Exclusives,[]),
    {Locks3,NewShared} =
      send_reply(shared,Locks2,Shared,[]),
    {reply,done, {Locks3,NewExclusives,NewShared}}.

send_reply(_Type,Locks,[],NewPendings) ->
    {Locks,NewPendings};
send_reply(Type,Locks,[Pending|Pendings],NewPendings) -> 
    case all_obtainable(Locks,Type,Pending) of
         true ->
           gen_server:reply(Pending,ok),
           send_reply(Type,
                      map(fun(Lock) ->
                             promote_pending(Lock,Type,Pending)
                          end,Locks),Pendings,NewPendings);
         false ->
           send_reply(Type,Locks,Pendings,NewPendings ++ [Pending])
    end.
 
handle_info({'EXIT',ClientPid,Reason}, {Locks,Exclusives,Shared}) ->
    io:format("locker got an exit message from ~p with reason ~p~n",
	      [ClientPid,Reason]),
    Locks1 =
      map(fun(Lock) -> release_lock(Lock,ClientPid) end,Locks),
    {Locks2,NewExclusives} =
      send_reply(exclusive,Locks1,Exclusives,[]),
    {Locks3,NewShared} =
      send_reply(shared,Locks2,Shared,[]),
    {noreply, {Locks3,NewExclusives,NewShared}}.
       

%%-----------------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------------

% Returns the pid of a {pid,tag} tuple passed to handle_call.
% 
%
pid({Pid,_}) -> Pid.

%+type check_availables([resource()],type(),[lock()]) -> bool().
%
% check availability of all requested resources

check_availables([],_Type,_Locks) -> true;
check_availables([Resource|Resources],Type,Locks) ->
  check_available(Resource,Type,Locks) and 
  check_availables(Resources,Type,Locks).

%+type check_available(resource(),type(),[lock()]) -> bool().
%
% a resource is available for exclusive access, if no client has
%   access to it, nor is exclusively waiting for it.
% a resource is available for shared access, if no client has
%   exclusive access to it, nor is exclusively waiting for it.

check_available(_Resource,_Type,[]) -> false;
check_available(Resource,Type,[Lock|Locks]) ->
  case Resource == Lock#lock.resource of
       true ->
         case {Type,Lock#lock.excl,Lock#lock.shared} of
              {exclusive,none,[]} -> 
                 no_exclusive(Lock#lock.pending);
              {shared,none,_} ->
                 Lock#lock.pending == []; % if not empty, then excl waiting
              _ ->
                 false
         end;
       false ->
         check_available(Resource,Type,Locks)
  end.

%+type no_exclusive([pending()]) -> bool().
%
% there is no client in the list that requests an exclusive lock
% Since the exclusive locks come first in the pending list,
% it suffices to only check the first one.

no_exclusive([]) -> true;
no_exclusive([Pending|_]) -> Pending#pending.type == shared.

%+type claim_lock(lock(),[resource()],type(),client()) -> lock().
%
% If the lock corresponds to a requested resource, then the client
% is either put in the exclusive or added to the shared field.

claim_lock(Lock,Resources,Type,Client) ->
  case {member(Lock#lock.resource,Resources),Type} of
       {true,exclusive} ->
         Lock#lock{excl = Client};
       {true,shared} ->
         Lock#lock{shared = [Client|Lock#lock.shared]};
       {false,_} ->
         Lock
  end.

%+type release_lock(lock(),client()) -> lock().
%

release_lock(Lock,Client) -> 
  case Lock#lock.excl of
    none ->
      Lock#lock{shared = delete(Client,Lock#lock.shared)};
    ExclClient ->
      if ExclClient==Client -> Lock#lock{excl = none};
	 true -> Lock
      end
  end.

delete(_Client,[]) -> [];
delete(Client,[Client|Cs]) -> Cs;
delete(Client,[C|Cs]) -> [C|delete(Client,Cs)].

%+type add_pending(lock(),[resource()],type(),client()) -> lock().
%
% recall that pending list has exclusive before shared 

add_pending(Lock,Resources,Type,Client) ->
  case member(Lock#lock.resource,Resources) of
       true ->
         Pendings = 
           insert_pending(Lock#lock.pending,Type,Client),
         Lock#lock{pending = Pendings};      
       false ->
         Lock
  end.

%+type insert_pending([pending()],type(),client()) -> [pending()].

insert_pending([],Type,Client) ->
  [#pending{client = Client,type = Type}];
insert_pending([Pending|Pendings],exclusive,Client) ->
  case Pending#pending.type of 
       exclusive ->
         [Pending|insert_pending(Pendings,exclusive,Client)];
       shared ->
         [#pending{client = Client,type = exclusive},Pending|Pendings]
  end;
insert_pending(Pendings,shared,Client) ->
   Pendings ++ [#pending{client = Client,type = shared}].       


%+type all_obtainable([lock()],type(),client()) -> bool().
%
% are all demanded locks obtainable for this client
% (requesting a lock of type Type). The lock is demanded if the
% client is pending for it

all_obtainable([],_Type,_Client) -> true;
all_obtainable([Lock|Locks],Type,Client) ->
  case member_pending(Client,Lock#lock.pending) of
       true ->
         obtainable(Lock,Type,Client) and all_obtainable(Locks,Type,Client);
       false -> %not demanded
         all_obtainable(Locks,Type,Client)
  end.

%+type obtainable(lock(),type(),client()) -> bool().
%
% a lock is obtainable for exclusive right, if
%   client is first in the pending list and no client
%   is holding the lock
% a lock is obtainable for shared right, if
%   client is first in the pending list and no client
%   is holding the exclusive lock

obtainable(Lock,exclusive,Client) ->
   (Lock#lock.excl == none) and (Lock#lock.shared == []) and 
   first_pending(Client,Lock#lock.pending);
obtainable(Lock,shared,Client) ->
   (Lock#lock.excl == none) and  
   first_pending(Client,Lock#lock.pending).

%+type first_pending(client(),[pending()]) -> bool().

first_pending(_Client,[]) -> false;
first_pending(Client,[Pending|_]) ->
  Client == Pending#pending.client.

%+type member_pending(client(),[pending()]) -> bool().

member_pending(_Client,[]) -> false;
member_pending(Client,[Pending|Pendings]) ->
  case Pending#pending.client of
    Client -> true;
    _ -> member_pending(Client,Pendings)
  end.


%+type promote_pending(lock(),type(),client()) -> lock().
%
% only when the client is the first pending process it is promoted

promote_pending(Lock,Type,Client) ->
  case first_pending(Client,Lock#lock.pending) of
    true ->
      case Type of
	exclusive ->
	  Lock#lock{excl = pid(Client),
		    pending=tl(Lock#lock.pending)};
	shared ->
	  Lock#lock{shared = [pid(Client)|Lock#lock.shared],
		    pending=tl(Lock#lock.pending)}
      end;
    false -> 
      Lock
  end.

  
