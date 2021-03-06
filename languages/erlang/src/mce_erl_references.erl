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
%% @private Computing of new resources (pids, references) according to
%% two disciplines:
%%   - when simulation (or by choice) resource counters are not reused
%%   - when model checking (or by choice) resource counters are reused
%%     when it is safe to do so (they are free both in the current system
%%     and the current monitor state).
%%

-module(mce_erl_references).

-include("process.hrl").
-include("node.hrl").

-export([getNewPidInSystem/4,
	 getNewPid/2,
	 getNewPid/3,
	 mkNewMonitorRef/2,
	 mkReference/2,
 	 reset_counters/0]).


%%-define(debug,true).

-ifdef(debug).
-define(LOG(X,Y), io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-else.
-define(LOG(X,Y), true).
-endif.

checkTag(Term,TagRecognizer) ->
  case TagRecognizer(Term) of
    {yes,Value} -> [Value];
    no -> [];
    maybe -> foldFindTags(tuple_to_list(Term),TagRecognizer)
  end.

findTagsInTerm(Term,TagRecognizer) ->
  if
    is_tuple(Term) -> checkTag(Term,TagRecognizer);
    is_list(Term) -> foldFindTags(Term,TagRecognizer);
    true -> []
  end.

foldFindTags(L,TagRecognizer) ->
  foldFindTags(L,TagRecognizer,[]).

foldFindTags([],_TagRecognizer,Result) ->
  lists:umerge(Result);
foldFindTags([Elem|Rest],TagRecognizer,Result) ->
  case findTagsInTerm(Elem,TagRecognizer) of
    [] -> foldFindTags(Rest,TagRecognizer,Result);
    Other -> foldFindTags(Rest,TagRecognizer,[Other|Result])
  end;
foldFindTags(Elem,TagRecognizer,Result) ->
  %% Somebody is playing with non-proper lists
  foldFindTags([Elem],TagRecognizer,Result).

findSmallest(Values) ->
  findSmallest(1,Values).
findSmallest(N,[]) ->
  N;
findSmallest(N,[Hd|Rest]) ->
  if
    N<Hd -> N;
    true -> findSmallest(Hd+1,Rest)
  end.

findLargest([]) -> 0;
findLargest(L) -> lists:last(L).

reset_counters() ->
  Counters =
    [pid,mce_erl_ref,monitorRef],
  InternalCounters =
    lists:map(fun (Counter) -> {reference_counter,Counter} end, Counters),
  lists:foreach(fun (Counter) -> erase(Counter) end, InternalCounters).

getNewValue(Tag, SmallValues, TagConstructor, TagRecognizer) ->
  getNewValue
    (Tag, SmallValues, TagConstructor, TagRecognizer, mce_erl_state:getState()).
getNewValue(Tag,SmallValues,TagConstructor,TagRecognizer,Expr) ->
  case SmallValues of
    false ->
      MonExpr = {Expr,get(mc_monitor)},
      TagCounter = {reference_counter,Tag},
      OldCounter =
	case get(TagCounter) of
	  undefined -> 
	    (findLargest(findTagsInTerm(MonExpr,TagRecognizer)))+1;
	  Other -> 
	    Other
	end,
      put(TagCounter,OldCounter+1),
      NewValue = TagConstructor(OldCounter),
      ?LOG("Returning new value ~p~n",[NewValue]),
      NewValue;
    true ->
      MonExpr = {Expr,get(mc_monitor)},
      NewValue =
	TagConstructor(findSmallest(findTagsInTerm(MonExpr,TagRecognizer))),
      ?LOG("Returning new value ~p~n",[NewValue]),
      NewValue
  end.

getNewPid(Node,Expr) ->
  getNewPidInSystem(Node,Expr,void,void).

getNewPid(Node,Expr,Conf) ->
  getNewPidInSystem(Node,Expr,void,Conf).

getNewPidInSystem(Node,Expr,State,_Conf) ->
  TagConstructor =
    fun (Value) ->
	{pid, Node, Value}
    end,
  TagRecogniser =
    fun (Tag) ->
	case Tag of
	  {pid, Node, Value} -> {yes, Value};
	  {pid, _, _} -> no;
	  _ -> maybe
	end
    end,
  ?LOG("getNewPidInSystem(~p~n,~p~n,~p~n) with Small_pids=~p~n",
       [Node,Expr,State,small_values()]),
  getNewValue(pid,small_values(),TagConstructor,TagRecogniser,{Expr, State}).

mkNewMonitorRef(Value, State) ->
  MonTagChecker =
    fun (P) ->
	case P of
	  {monitorRef, OtherValue, X} ->
	    if Value == OtherValue -> {yes, X};
	       true -> no
	    end;
	  _ -> maybe
	end
    end,
  MonTagConstructor =
    fun (SmallestValue) ->
	{monitorRef, Value, SmallestValue}
    end,
  getNewValue(monitorRef,small_values(),MonTagConstructor,MonTagChecker,State).

mkReference(Pid, State) ->
  RefChecker =
    fun (P) ->
	case P of
	  {mce_erl_ref, OtherPid, X} ->
	    if Pid == OtherPid -> {yes, X};
	       true -> no
	    end;
	  _ -> maybe
	end
    end,
  RefConstructor =
    fun (SmallestValue) ->
	{mce_erl_ref, Pid, SmallestValue}
    end,
  getNewValue(mce_erl_ref, small_values(), RefConstructor, RefChecker, State).

small_values() ->
  mce_conf:small_pids(mce_conf:get_conf()).

 
