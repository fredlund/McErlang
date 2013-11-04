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

-module(mce_erl_ets).

%-define(debug,true).

-ifdef(debug).
-define(LOG(X,Y), io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-else.
-define(LOG(X,Y), true).
-endif.

-export([new/2,first/1,lookup/2,insert/2,delete/1,delete/2,next/2,insert_new/2]).
-export([delete_all_objects/1,foldl/3,delete_object/2,delete_object_count/2]).
-export([match_object/2,match_delete/2]).
-export([select_delete/2]).

new(Name,[]) ->
  new(Name,[set,protected,{keypos,1}]);
new(Name,Options) ->
  TableType = getTableType(Options),
  NamedTable = isNamedTable(Options),
  TableName =
    if
      NamedTable ->
	Name;
      true -> 
	{Name,self()}
    end,
  KeyPosElement = getKeyPosElement(Options),
  case TableType of
    Type when Type==ordered_set;Type==set ->
      mcerlang:nput(TableName,{ets,{KeyPosElement,[]}}),
      TableName;
    _ ->
      io:format("error: ets table ~p not of type set; nyi~n",[TableType]),
      throw(ev_ets)
  end.

getTableType([]) -> set;
getTableType([set|_]) -> set;
getTableType([ordered_set|_]) -> ordered_set;
getTableType([bag|_]) -> bag;
getTableType([duplicate_bag|_]) -> duplicate_bag;
getTableType([_|Rest]) -> getTableType(Rest).

isNamedTable([]) -> false;
isNamedTable([named_table|_]) -> true;
isNamedTable([_|Rest]) -> isNamedTable(Rest).

getKeyPosElement([]) -> 1;
getKeyPosElement([{keypos,Pos}|_]) -> Pos;
getKeyPosElement([_|Rest]) -> getKeyPosElement(Rest).

delete_all_objects(TableId) ->
  case mcerlang:nget(TableId) of
      {ets,{KeyPos,_Table}} ->
	  mcerlang:nput(TableId,{ets,{KeyPos,[]}}),
	  true;
      _ ->
	  io:format("***Warning: no ets table ~p exists~n",[TableId]),
	  true
  end.
	    
delete(TableId) ->
  case mcerlang:nget(TableId) of
    {ets,{_KeyPos,_Table}} ->
      mcerlang:nerase(TableId),
      true;
    _ ->
      io:format("***Warning: no ets table ~p exists~n",[TableId]),
      true
  end.

delete_object(TableId,Object) ->
    _NumDeletes = delete_object_count(TableId,Object),
    true.

delete_object_count(TableId,Object) ->
  case mcerlang:nget(TableId) of
      {ets,{KeyPos,Table}} ->
	  {NumDeletes,NewTable} =
	      lists:foldl
		(fun (Elem,{Count,Elements}) ->
			 if Elem=:=Object -> {Count+1,Elements};      
			    true -> {Count,[Elem|Elements]}
			 end
		 end, {0,[]}, Table),
	  mcerlang:nput(TableId,{ets,{KeyPos,lists:reverse(NewTable)}}),
	  NumDeletes;
      _ ->
	  io:format("***Warning: no ets table ~p exists~n",[TableId]),
	  true
  end.

delete(TableId,Key) ->
  case mcerlang:nget(TableId) of
      {ets,{KeyPos,Table}} ->
	  NewTable =
	      lists:filter
		(fun (Elem) ->
			 if element(KeyPos,Elem)=:=Key -> false;      
			    true -> true
			 end
		 end, Table),
	  mcerlang:nput(TableId,{ets,{KeyPos,NewTable}}),
	  true;
      _ ->
	  io:format("***Warning: no ets table ~p exists~n",[TableId]),
	  true
  end.

foldl(Function, Acc, TableId) ->
  case mcerlang:nget(TableId) of
    {ets,{_KeyPos,Table}} ->
	  lists:foldl(Function, Acc, Table);
    _ ->
      io:format("***Warning: no ets table ~p exists~n",[TableId]),
      true
  end.
    

insert(TableId,Objects) ->
  case mcerlang:nget(TableId) of
    {ets,{KeyPos,Table}} ->
      mcerlang:nput(TableId,{ets,{KeyPos,insertsInTable(Objects,KeyPos,Table)}}),
      true;
    _ ->
      io:format("***Warning: no ets table ~p exists~n",[TableId]),
      true
  end.

insert_new(TableId, Objects) ->
    case mcerlang:nget(TableId) of
      {ets, {KeyPos, _Table}} ->
	  case lists:foldl
	    (fun (Object, Result) ->
		 if not Result -> false;
		    true ->
		     Key = element(KeyPos, Object),
		     case mce_erl_ets:lookup(TableId, Key) of
		       [] -> true;
		       _ -> false
		     end
		 end
	     end, true,
	     if is_list(Objects) -> Objects; true -> [Objects] end)
	  of
	    false ->
	      false;
	    true ->
	      insert(TableId, Objects)
	  end
    end.

insertsInTable([],_KeyPos,Table) -> Table;
insertsInTable([Elem|Rest],KeyPos,Table) ->
  insertsInTable(Rest,KeyPos,insertInTable(Elem,KeyPos,Table));
insertsInTable(Elem,KeyPos,Table) ->
  insertInTable(Elem,KeyPos,Table).

insertInTable(Elem,_KeyPos,[]) -> [Elem];
insertInTable(Elem,KeyPos,[TElem|Rest]) ->
  ElemKey = element(KeyPos,Elem),
  TElemKey = element(KeyPos,TElem),
  if
    ElemKey == TElemKey ->
      [Elem|Rest];
    ElemKey < TElemKey ->
      [Elem,TElem|Rest];
    true ->
      [TElem|insertInTable(Elem,KeyPos,Rest)]
  end.

first(TableId) ->  
  case mcerlang:nget(TableId) of
    {ets,{KeyPos,Table}} ->
      case Table of 
	[] ->
	  '$end_of_table';
	[Elem|_] ->
	  element(KeyPos,Elem)
      end;
    _ ->
      io:format("***Warning: no ets table ~p exists~n",[TableId]),
      '$end_of_table'
  end.

next(TableId,Key) ->
  case mcerlang:nget(TableId) of
    {ets,{KeyPos,Table}} ->
      case find_element(Table,Key,KeyPos) of
	{ok,Element} ->
	  element(KeyPos,Element);
	_ ->
	  '$end_of_table'
      end;
    _ ->
      io:format("***Warning: no ets table ~p exists~n",[TableId]),
      '$end_of_table'
  end.

lookup(TableId,Key) ->
  case mcerlang:nget(TableId) of
    {ets,{KeyPos,Table}} ->
      case find_element(Table,Key,KeyPos) of
	{ok,Element} ->
	  [Element];
	_ ->
	  []
      end;
    _ ->
      io:format("***Warning: no ets table ~p exists~n",[TableId]),
      []
  end.
 
find_element([],_,_) -> no;
find_element([Elem|Rest],Key,KeyPos) ->
  ElemKey = element(KeyPos,Elem),
  if
    ElemKey==Key ->
      {ok,Elem};
    Key < ElemKey ->
      no;
    true ->
      find_element(Rest,Key,KeyPos)
  end.
      
match_object(TableId,Pattern) ->
  ?LOG("~n*** In match_object ***~n",[]),
  case mcerlang:nget(TableId) of
    {ets,{_KeyPos,Table}} ->
      Result = 
	lists:filter
	  (fun (Elem) ->
	       R = filter_match(Elem,Pattern),
	       ?LOG("filter_match(~p,~p) => ~p~n",
			 [Elem,Pattern,R]),
	       R
	   end, Table),
      ?LOG("Matching objects: ~p~n",[Result]),
      Result;
    _ ->
      ?LOG("***Warning: no ets table ~p exists~n",[TableId]),
      true
  end.

match_delete(TableId, Pattern) ->
    ?LOG("~n*** In match_delete ***~n", []),
    case mcerlang:nget(TableId) of
      {ets, {KeyPos, Table}} ->
	  NewTable =
	      lists:filter(fun (Elem) -> not filter_match(Elem, Pattern) end, Table),
	  ?LOG("Old table ~p, New table ~p~n", [Table, NewTable]),
	  mcerlang:nput(TableId, {ets, {KeyPos, NewTable}});
      _ ->
	  io:format("***Warning: no ets table ~p exists~n", [TableId]),
	  true
    end.

filter_match(Object,Pattern) ->
  case pattern_match(Object,Pattern) of
    no ->
      false;
    {ok,_} ->
      true
  end.

%% Does not handle totally general patterns for now (binaries etc)
pattern_match(Object,Pattern) ->
  Result = pattern_match(Object,Pattern,[]),
  ?LOG("pattern_match(~p,~p) ==> ~n~p~n",[Object,Pattern,Result]),
  case Result of
    no ->
      no;
    Bindings when is_list(Bindings) ->
      {ok,{Object,Bindings}}
  end.

pattern_match(Object,Pattern,Bindings) ->
  ?LOG("Matching ~p against ~p, with bindings ~p~n",
	    [Pattern,Object,Bindings]),
  if
    is_tuple(Pattern) ->
      if
	is_tuple(Object) ->
	  case {size(Object),size(Pattern)} of
	    {N,N} ->
	      pattern_match_list
		(lists:zip(tuple_to_list(Object),tuple_to_list(Pattern)),
		 Bindings);
	    _ ->
	     	no 
	  end;
	true -> no
      end;
    is_list(Pattern) ->
      if
	is_list(Object) ->
	  pattern_match_list
	    ([{hd(Object),hd(Pattern)},{tl(Object),tl(Pattern)}],
	     Bindings);
	true ->
	  no
      end;
    Pattern=:='_' ->
      Bindings;
    is_atom(Pattern) ->
      case atom_to_list(Pattern) of
	[97|_] ->
	  case add_to_bindings(Pattern,Object,Bindings) of
	    {ok,NewBindings} -> NewBindings;
	    _ -> no
	  end;
	_ -> if Object=:=Pattern -> Bindings; true -> no end
      end;
    true -> if Object=:=Pattern -> Bindings; true -> no end
  end.

pattern_match_list([],Bindings) ->
  Bindings;
pattern_match_list([{O,P}|Rest],Bindings) ->
  case pattern_match(O,P,Bindings) of
    no -> no;
    NewBindings -> pattern_match_list(Rest,NewBindings)
  end.

add_to_bindings(A,O,Bindings) ->
  case add_to_bindings_1(A,O,Bindings) of
    no -> no;
    NewBindings -> {ok,NewBindings}
  end.

add_to_bindings_1(A,O,[]) -> [{A,O}];
add_to_bindings_1(A,O,Bindings=[{AB,OB}|Rest]) ->
  if
    A=:=AB ->
      if
	O=:=OB ->
	  Bindings;
	true ->
	  no
      end;
    A < AB ->
      [{A,O}|Bindings];
    true ->
      case add_to_bindings_1(A,O,Rest) of
	no ->
	  no;
	NewBindings ->
	  [{AB,OB}|NewBindings]
      end
  end.
		  
	  

      
%%%% The select-family of functions
%match_delete(Table, Pattern) ->
%    ets:select_delete(Table,[{Pattern,[],[true]}]),
%    true.

%% Since it is implemented in terms of filter_match, there is
%% no support for full match_spec's
select_delete(_TableId,[]) ->
	0;
select_delete(TableId,[{Pattern,[],[true]} | Patterns]) ->
  ?LOG("~n*** In select_delete ***~n", []),
    case mcerlang:nget(TableId) of
      {ets, {KeyPos, Table}} ->
	NewTable =
	  lists:filter(fun (Elem) -> 
			   not filter_match(Elem, Pattern) 
		       end, Table),
	?LOG("Old table ~p, New table ~p~n", [Table, NewTable]),
	mcerlang:nput(TableId, {ets, {KeyPos, NewTable}}),
	length(Table) - length(NewTable) +
	  select_delete(TableId,Patterns);
      _ ->
	io:format("***Warning: no ets table ~p exists~n", [TableId]),
	0
    end.	
