%% Copyright (c) 2009, Lars-Ake Fredlund
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%     %% Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%     %% Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%     %% Neither the name of the copyright holders nor the
%%       names of its contributors may be used to endorse or promote products
%%       derived from this software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ''AS IS''
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
%% BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR 
%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF 
%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

%% @author Lars-Ake Fredlund
%% @copyright 2006-2009 Lars-Ake Fredlund
%% @doc
%% @private

-module(mce_erl_gen_event).

%%-define(debug,true).

-ifdef(debug).
-define(LOG(X,Y), io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-else.
-define(LOG(X,Y), true).
-endif.

-export([start_link/0,start_link/1,start/0,start/1]).
-export([add_handler/3]).
-export([add_sup_handler/3]).
-export([notify/2,sync_notify/2]).
-export([call/3,call/4]).
-export([delete_handler/3,swap_handler/3,swap_sup_handler/3]).
-export([which_handlers/1,stop/1]).

-export([init/1,handle_event/2,handle_call/2,handle_info/2,terminate/2]).

-export([doStart/1,doStart/2]).

start_link() ->
  Pid = spawn_link(?MODULE, doStart, [mcerlang:self()]),
  waitForStart(Pid).

start_link({local, Name}) -> 
  Pid = spawn_link(?MODULE, doStart, [Name, mcerlang:self()]),
  waitForStart(Pid).

start() ->
  Pid = spawn(?MODULE, doStart, [mcerlang:self()]),
  waitForStart(Pid).

start({local, Name}) ->
  Pid = spawn(?MODULE, doStart, [Name, mcerlang:self()]),
  waitForStart(Pid).

waitForStart(Pid) ->
  receive
    {ok,started} -> {ok, Pid}
  end.

doStart(Name, ParentPid) ->
  ?LOG("~p: going to register name ~p for ~p~n", [?MODULE,Name,self()]),  
  RegisterReply =
    try register(Name,self()) of _ -> {ok,started}
    catch _ -> 
	try whereis(Name) of NamePid -> {error,{already_started,NamePid}}
	catch _ -> {error,unknown} end
    end,
  ParentPid!RegisterReply, 
  case RegisterReply of
    {ok, _} -> loop(init_state());
    _ -> exiting
  end.

doStart(ParentPid) ->
  ParentPid!started,
  loop(init_state()).

init_state() -> [].

loop(State) ->
  receive
    {add_handler,Client,CallRef,Handler,Args} ->
      {Result,NewState} =
	try_init_handler(normal,State,Handler,Args),
      make_reply(Result,Client,CallRef),
      ?LOG
	("~p: added handler ~p, new state is ~p~n",
	 [self(),Handler,NewState]),
      loop(NewState);
    {add_sup_handler,Client,CallRef,Handler,Args} ->
      {Result,NewState} =
	try_init_handler({sup_handler,Client},State,Handler,Args),
      make_reply(Result,Client,CallRef),
      loop(NewState);
    {notify,Event} ->
      NewState =
	lists:foldl
	  (fun (Handler,Collected) ->
	       {_Result,NewState} = try_handle_event(Handler,Event,Collected),
	       NewState
	   end,
	   [], State),
      loop(NewState);
    {sync_notify,Client,CallRef,Event} ->
      NewState =
	lists:foldl
	  (fun (Handler,Collected) ->
	       {_Result,NewState} = try_handle_event(Handler,Event,Collected),
	       NewState
	   end,
	   [], State),
      make_reply(ok,Client,CallRef),
      loop(NewState);
    {call,Client,CallRef,HandlerId,Request} ->
      case find_handler(HandlerId,State) of
	{ok,{Handler,OtherHandlers}} ->
	  {Result,NewState} = try_call(Handler,Request,OtherHandlers),
	  make_reply(Result,Client,CallRef),
	  loop(NewState);
	_ ->
	  make_reply({error,bad_module},Client,CallRef),
	  loop(State)
      end;
    {delete_handler,Client,CallRef,HandlerId,Args} ->
      case find_handler(HandlerId,State) of
	{ok,{Handler,OtherHandlers}} ->
	  Result = try_terminate(Handler,Args),
	  make_reply(Result,Client,CallRef),
	  loop(OtherHandlers);
	_ ->
	  make_reply({error,bad_module},Client,CallRef),
          loop(State)
      end;
    {swap_handler,Client,CallRef,Handler1,Handler2} ->
      {Result,NewState} = swap_handlers(normal,Handler1,Handler2,State),
      make_reply(Result,Client,CallRef),
      loop(NewState);
    {swap_sup_handler,Client,CallRef,Handler1,Handler2} ->
      {Result,NewState} = swap_handlers(sup_handler,Handler1,Handler2,State),
      make_reply(Result,Client,CallRef),
      loop(NewState);
    {which_handlers,Client,CallRef} ->
      make_reply
	(lists:map(fun ({_,Handler,_}) -> Handler end,State),Client,CallRef),
      loop(State);
    stop ->
      lists:foreach(fun (Handler) -> try_terminate(Handler,stop) end,State),
      ok;
    OtherMsg ->
      ?LOG("~p: Got other msg ~p~n",[self(),OtherMsg]),
      ?LOG("~p: Handlers are ~p~n",[self(),State]),
      NewState =
	lists:foldl
	  (fun (Handler,Collected) ->
	       {_Result,NewState} = try_handle_info(Handler,OtherMsg,Collected),
	       NewState
	   end,
	   [], State),
      loop(NewState)
  end.

make_reply(Reply,Client,CallRef) ->
  Client!{reply,CallRef,Reply}.

handler_module(Handler) when is_atom(Handler) -> Handler;
handler_module({Handler,_}) -> Handler.
handler_id({_,HandlerId,_}) -> HandlerId.

find_handler(HandlerId,Handlers) ->
  find_handler(HandlerId,Handlers,[]).
find_handler(_HandlerId,[],_) -> no;
find_handler(HandlerId,[OtherHandler|Rest],SeenHandlers) ->
  OtherHandlerId = handler_id(OtherHandler),
  if
    HandlerId=:=OtherHandlerId ->
      {ok,{OtherHandler,lists:reverse(SeenHandlers)++Rest}};
    HandlerId>OtherHandlerId ->
      find_handler(HandlerId,Rest,[OtherHandler|SeenHandlers]);
    true ->
      no
  end.

add_handler_int(Handler,Handlers) ->
  add_handler_int(Handler,Handlers,[]).
add_handler_int(Handler,[],SeenHandlers) ->
  lists:reverse(SeenHandlers,[Handler]);
add_handler_int(Handler,[OtherHandler|Rest],SeenHandlers) ->
  HandlerId = handler_id(Handler),
  OtherHandlerId = handler_id(OtherHandler),
  if
    HandlerId=:=OtherHandlerId ->
      {error,exists};
    HandlerId>OtherHandlerId ->
      add_handler_int(Handler,Rest,[OtherHandler|SeenHandlers]);
    true ->
      lists:reverse(SeenHandlers,[Handler,OtherHandler|Rest])
  end.

try_init_handler(Type,State,Handler,Args) ->
  ?LOG("~p: calling ~p:~p(~p)~n",[self(),handler_module(Handler),init,Args]),
  try (handler_module(Handler)):init(Args) of
      {ok,HandlerState} -> 
        RealType =
	  case Type of
	    {sup_handler,Client} ->
	      {sup_handler,erlang:monitor(process,Client)};
	    _ ->
	      Type
	  end,
        ?LOG("~p: adding ~p~n",[self(),{RealType,Handler,HandlerState}]),
        {ok,add_handler_int({RealType,Handler,HandlerState},State)};
      Other ->
        ?LOG("~p: handler_init returned ~p~n",[self(),Other]),
        {Other,State}
  catch _:Reason ->
		?LOG("~p: add handler exit due to ~p~n",[self(),Reason]),
		?LOG("~p~n",[erlang:get_stacktrace()]),
		{{'EXIT',Reason},State} 
  end.

try_handle_event(Hnd={Type,Handler,HandlerState},Event,State) ->
  try (handler_module(Handler)):handle_event(Event,HandlerState) of
      {ok,NewHandlerState} -> 
        {ok,add_handler_int({Type,Handler,NewHandlerState},State)};
      {swap_handler,Args1,NewHandlerState,Handler2,Args2} ->
        Result = try_terminate(Hnd,Args1,NewHandlerState),
        {Result,NewState} = 
	  try_init_handler(Type,State,Handler2,{Args2,Result}),
        {Result,NewState};
      remove_handler ->
        try_terminate(Hnd,remove_handler,HandlerState),
        {ok,State};
      Other ->
        try_terminate(Hnd,Other,HandlerState),
        {{error,Other},State}
  catch _:Reason -> 
    try_terminate(Hnd,Reason,HandlerState), 
    {{error,{'EXIT',Reason}},State} 
  end.

try_handle_info(Hnd={Type,Handler,HandlerState},OtherMsg,State) ->
  ?LOG
    ("Will call handle_info on ~p for ~p~n",
     [handler_module(Handler),OtherMsg]),
  try (handler_module(Handler)):handle_info(OtherMsg,HandlerState) of
      {ok,NewHandlerState} -> 
        {ok,add_handler_int({Type,Handler,NewHandlerState},State)};
      {swap_handler,Args1,NewHandlerState,Handler2,Args2} ->
        Result = try_terminate(Hnd,Args1,NewHandlerState),
        {Result,NewState} = 
	  try_init_handler(Type,State,Handler2,{Args2,Result}),
        {Result,NewState};
      remove_handler ->
        try_terminate(Hnd,remove_handler,HandlerState),
        {ok,State};
      Other ->
        try_terminate(Hnd,Other,HandlerState),
        {{error,Other},State}
  catch _:Reason -> 
    ?LOG("handle_info failed with ~p~n",[Reason]),
    try_terminate(Hnd,Reason,HandlerState), 
    {{error,{'EXIT',Reason}},State} 
  end.

try_call(Hnd={Type,Handler,HandlerState},Request,State) ->
  try (handler_module(Handler)):handle_call(Request,HandlerState) of
      {ok,Reply,NewHandlerState} -> 
        {Reply,add_handler_int({Type,Handler,NewHandlerState},State)};
      {swap_handler,Reply,Args1,NewHandlerState,Handler2,Args2} ->
        Result = try_terminate(Hnd,Args1,NewHandlerState),
        {Result,NewState} = 
	  try_init_handler(Type,State,Handler2,{Args2,Result}),
        case Result of
	  ok -> {Reply,NewState};
	  _ -> {Result,NewState}
	end;
      {remove_handler,Reply} ->
        try_terminate(Hnd,remove_handler,HandlerState),
        {Reply,State};
      Other ->
        try_terminate(Hnd,Other,HandlerState),
        {{error,Other},State}
  catch _:Reason -> 
    try_terminate(Hnd,Reason,HandlerState), 
    {{error,{'EXIT',Reason}},State} 
  end.

try_terminate(Handler={_,_,HandlerState},Reason) ->
  try_terminate(Handler,Reason,HandlerState).

swap_handlers(Type,{Handler1,Args1},{_Handler2,Args2},State) ->
  case find_handler(Handler1,State) of
    {ok,{Handler={_,_,_HandlerState},OtherHandlers}} ->
      Result = try_terminate(Handler,Args1),
      try_init_handler(Type,OtherHandlers,Handler1,{Args2,Result});
    _Other ->
      try_init_handler(Type,State,Handler1,{Args2,error})
  end.

try_terminate({Type,Handler,_},Reason,HandlerState) ->
  case Type of
    {sup_handler,MonitorRef} -> erlang:demonitor(MonitorRef);
    _ -> ok
  end,
  try (handler_module(Handler)):terminate(Reason,HandlerState) 
      of Result -> Result
  catch _:Reason -> {'EXIT',Reason} end.

add_handler(EventMgrRef, Handler, Args) ->
  make_call(EventMgrRef,add_handler,[Handler,Args]).

add_sup_handler(EventMgrRef, Handler, Args) ->
  make_call(EventMgrRef,add_sup_handler,[Handler,Args]).

notify(EventMgrRef,Event) ->
  EventMgrRef!{notify,Event},
  ok.

sync_notify(EventMgrRef,Event) ->
  make_call(EventMgrRef,sync_notify,[Event]).

call(EventMgrRef,Handler,Request) ->
  call(EventMgrRef,Handler,Request,infinity).

call(EventMgrRef,Handler,Request,Timeout) ->
  make_call(EventMgrRef,call,[Handler,Request],Timeout).
	
delete_handler(EventMgrRef, Handler, Args) ->
  make_call(EventMgrRef,delete_handler,[Handler,Args]).
      
swap_handler(EventMgrRef,Hnd1={_Handler1,_Args1},Hnd2={_Handler2,_Args2}) ->
  make_call(EventMgrRef,swap_handler,[Hnd1,Hnd2]).

swap_sup_handler(EventMgrRef,Hnd1={_Handler1,_Args1},Hnd2={_Handler2,_Args2}) ->
  make_call(EventMgrRef,swap_sup_handler,[Hnd1,Hnd2]).

which_handlers(EventMgrRef) ->
  make_call(EventMgrRef,which_handlers,[]).

stop(EventMgrRef) ->
  EventMgrRef!stop,
  ok.

make_call(EventMgr,CallType,Args) ->
  make_call(EventMgr,CallType,Args,infinity).
make_call(EventMgrRef,CallType,Args,Timeout) ->
  CallRef = erlang:make_ref(),
  EventMgrRef!(list_to_tuple([CallType,self(),CallRef|Args])),
  receive
    {reply,CallRef,Result} -> Result
  after Timeout -> throw(timeout)
  end.
	
init(Args) ->
  ?LOG("init(~p)~n",[Args]),
  {ok,Args}.
handle_event(_Event,State) ->
  %%?LOG("handle_event(~p,~p)~n",[Event,State]),
  {ok,State}.
handle_call(_Request,State) ->
  %%?LOG("handle_request(~p,~p)~n",[Request,State]),
  {ok,reply,State}.
handle_info(_Info,State) ->
  %%?LOG("handle_info(~p,~p)~n",[Info,State]),
  {ok,State}.
terminate(_Reason,_State) ->
  %%?LOG("terminate(~p,~p)~n",[Reason,State]),
  ok.
