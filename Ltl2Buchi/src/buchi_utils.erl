%% Copyright (c) 2009, Hans Svensson
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

%% @author Hans Svensson <hanssv@chalmers.se>
%% @copyright 2009, Hans Svensson
%% @doc Module containing utility functions for Büchi automata
%% @see buchi
%%
%% @type buchi_automaton(). A tuple structure representing a Büchi automaton.
%% (See <A HREF="buchi.html"><code>buchi</code></A> for data type definition.)
-module(buchi_utils).

-export([size_of/1, 
		 reachable/1, reachable/2, 
		 in_cycle/2, 
		 strong_components/1,
		 cycles/1]).

%% Reference implementations (for testing only)
%% -export([cycles2/1,strong_components2/1,reachable2/1])

%% @doc Size of Büchi automaton.
%% Returns a (bad) measure of the size of the BA
%% @spec (buchi_automaton()) -> int()
size_of({States,_InitStates,Trans,_Accepts}) ->
    length(States) + length(Trans).

%% A dfs...
%% @doc Return all reachable states of Büchi automaton.
%% Does a depth first traversal of the automaton.
%% @spec (buchi_automaton()) -> [state()]
reachable(_B = {_States,InitStates,Trans,_Accept}) ->
	reachable(Trans,InitStates).

%% @doc Return all reachable states given a set of transitions and the 
%% initial states.
%% @see reachable/1
%% @spec ([transition()],[state()]) -> [state()]
%% @todo Optimize
reachable(Trans,InitStates) ->
    reachable(Trans,InitStates,[]).

reachable(_,[],Reached) ->	
    Reached;
reachable(Trans,[State|States],Reached) ->
    {States2,Reached2} = reachable(State,Trans,States,Reached),
    reachable(Trans,States2,Reached2).

reachable(State, [], States, Reached) ->
    {States, [State| Reached]};
reachable(State, [{S1, S2, _}| Trans], States, Reached) ->
    case S1 == State andalso not lists:member(S2, [State| States]) andalso
		not lists:member(S2, Reached)
		of
		true ->
			reachable(State, Trans, [S2| States], Reached);
		false ->
			reachable(State, Trans, States, Reached)
    end;
reachable(State, [{S1, S2}| Trans], States, Reached) ->
    case S1 == State andalso not lists:member(S2, [State| States]) andalso
		not lists:member(S2, Reached)
		of
		true ->
			reachable(State, Trans, [S2| States], Reached);
		false ->
			reachable(State, Trans, States, Reached)
    end.

%% @doc Checks if State is member of a cycle
%% @spec (buchi_automaton(),state()) -> bool()
in_cycle({_,_,Trans,_},S) ->
	Reachable = reachable(Trans,[S]),
	lists:member(S,[S2 || {S1,S2,_} <- Trans,lists:member(S1,Reachable)]).

%% @doc Returns all (maximal) cycles of an automata
%% @spec (buchi_automaton()) -> [[state()]]
cycles(B) ->
	Sccs = strong_components(B),
	[L || L = [S | _] <- Sccs,
		  length(L) > 1 orelse is_loop(B,S)].

is_loop({_,_,Trans,_},S) ->
	length([ok || {S1,S2,_} <- Trans, S1 == S andalso S2 == S]) > 0.


%% @doc Returns all strongly connected components of an automaton
%% @spec (buchi_automaton()) -> [[state()]]
strong_components({States,_InitStates,Trans,_Accept}) ->
	Vs = [{S,undefined,undefined} || S <- States],
	tarjan1(Vs,Trans,[],0,[]).

tarjan1(Vs,Trans,Stack,Index,Res) ->
	case [S || {S,undefined,_} <- Vs] of
		[] -> Res;
		[S | _] -> 
 			{NewVs,NewStack,NewIndex,NewRes} = tarjan2(S,Vs,Trans,Stack,Index,Res),
			tarjan1(NewVs,Trans,NewStack,NewIndex,NewRes)
	end.

tarjan2(S,Vs,Trans,Stack,Index,Res) ->
	OutTrans = [S2 || {S1,S2,_} <- Trans, S1 == S],
	{{_,_,SLow},NewVs,NewStack,NewIndex,NewRes} = 
		tarjan3({S,Index,Index},OutTrans,lists:keyreplace(S,1,Vs,{S,Index,Index}),
				Trans,[S|Stack],Index+1,Res),
	case SLow == Index of
		true -> 
			{Scc,NewStack2} = pop_until(S,NewStack),
			{NewVs,NewStack2,NewIndex,[lists:usort(Scc) | NewRes]};
		false -> 
			{NewVs,NewStack,NewIndex,NewRes}
	end.
	
tarjan3(V,[],Vs,_Trans,Stack,Index,Res) ->
	{V,Vs,Stack,Index,Res};
tarjan3(V = {S0,S0Id,S0Low},[S | Ss],Vs,Trans,Stack,Index,Res) ->
	case lists:keysearch(S,1,Vs) of
		{value,{S,undefined,_}} ->
 			{NewVs,NewStack,NewIndex,NewRes} = tarjan2(S,Vs,Trans,Stack,Index,Res),
			{_,{_,_,SLow}} = lists:keysearch(S,1,NewVs),
			VNew = {S0,S0Id,lists:min([SLow,S0Low])},
			tarjan3(VNew,Ss,lists:keyreplace(S0,1,NewVs,VNew),Trans,NewStack,NewIndex,NewRes);
		_ -> case lists:member(S,Stack) of
				 true -> 
					 {_,{_,SLow,_}} = lists:keysearch(S,1,Vs),
					 VNew = {S0,S0Id,lists:min([SLow,S0Low])},
					 tarjan3(VNew,Ss,lists:keyreplace(S0,1,Vs,VNew),Trans,Stack,Index,Res);
				 false ->
					 tarjan3(V,Ss,Vs,Trans,Stack,Index,Res)
			 end
	end.

pop_until(_,[]) ->
	{[],[]};
pop_until(S,[S | Ss]) ->
	{[S],Ss};
pop_until(S,[S2 | Ss]) ->
	{Scc,NewStack} = pop_until(S,Ss),
	{[S2|Scc],NewStack}.
					 
							   
						   
