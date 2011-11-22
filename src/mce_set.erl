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

-module(mce_set).

-export([new/0,union/2,intersection/2,add_element/2,add_element/3,to_list/1,
	 member/2,minus/2,print/2,sort/1]).


new() -> [].

union([],S) -> S;
union([V|Rest],S) ->
  case member(V,S) of
    true -> union(Rest,S);
    false -> union(Rest,[V|S])
  end.

minus(S1,S2) -> minus(S1,S2,[]).
minus([],S1,S2) -> S2;
minus([V|Rest],S1,S2) ->
  case member(V,S1) of
    true -> minus(Rest,S1,S2);
    false -> minus(Rest,S1,[V|S2])
  end.

intersection(S1,S2) ->
  intersection(S1,S2,[]).
intersection([],S2,S3) ->
  S3;
intersection([V|Rest],S2,S3) ->
  case member(V,S2) of
    true -> intersection(Rest,S2,[V|S3]);
    false -> intersection(Rest,S2,S3)
  end.

add_element(V,S) -> 
    case member(V,S) of
	true -> S;
	false -> [V|S]
    end.

add_element(_,V,S) ->
    add_element(V,S).

to_list(S) -> S.
sort(S) -> lists:sort(to_list(S)).

member(V, []) ->
    false;
member(V, [V1| Rest]) ->
    erl_syntax:variable_name(V) =:= erl_syntax:variable_name(V1) orelse
      member(V, Rest).

print(F,S) -> io_lib:format("{~s}",[printS(F,S)]).

printS(F,[]) -> io_lib:format("",[]);
printS(F,[E]) -> io_lib:format("~s",[F(E)]);
printS(F,[E|Rest]) -> io_lib:format("~s,~s",[F(E),printS(F,Rest)]).
    
    
