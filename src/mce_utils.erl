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

-module(mce_utils).
-export([do/3,find/2,findret/2,foldl/3,slist_insert/2,slist_insert/3,repeat/3]).
-export([filtermap/2]).

do(State,CondFun,DoFun) ->
  case CondFun(State) of
    true -> do(DoFun(State),CondFun,DoFun);
    false -> State
  end.

find(_,[]) ->
    no;
find(F,[Hd|Rest]) ->
    case F(Hd) of
	true ->
	    {ok,Hd};
	never ->
	    no;
	false ->
	    find(F,Rest)
    end.

findret(_,[]) ->
  no;
findret(F,[Hd|Rest]) ->
  case F(Hd) of
    never ->
      no;
    false ->
      findret(F,Rest);
    Other ->
      Other
  end.

filtermap(_F,[]) ->
  [];
filtermap(F,[Hd|Rest]) ->
  case F(Hd) of
    false ->
      filtermap(F,Rest);
    true ->
      [Hd|filtermap(F,Rest)];
    Other ->
      [Other|filtermap(F,Rest)]
  end.

foldl(_,Acc,[]) -> 
    Acc;
foldl(F,Acc,[Hd|Rest]) ->
    case F(Hd,Rest,Acc) of
	{stop,Value} -> Value;
	Other -> foldl(F,Other,Rest)
    end.

%%% Insert into a sorted list with comparison function F;
%%% permits duplicates
%%%
slist_insert(_F,Elem,[]) -> [Elem];
slist_insert(F,Elem,[Hd|Tail]) ->
  case F(Elem,Hd) of
    greater ->
      [Hd|slist_insert(F,Elem,Tail)];
    _ ->
      [Elem,Hd|Tail]
  end.

%%% Insert into a sorted list with equality as comparison function
slist_insert(Elem,L) ->
  slist_insert
    (fun (E1,E2) ->
	 if E1=:=E2 -> equal;
	    E1<E2 -> less;
	    true -> greater
	 end
     end,
     Elem,L).
  
repeat(F,Acc,V) ->
  case apply(F,[Acc,V]) of
    {ok,{NewAcc,NewN}} ->
      repeat(F,NewAcc,NewN);
    {final,FinalValue} ->
      FinalValue
  end.
      
  
