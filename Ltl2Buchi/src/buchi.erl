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
%% @doc Module defining (non-labeled and non-generalized) Büchi automata
%%
%% @type buchi_automaton(). A tuple structure representing a Büchi automaton.<br/>
%% <code>B :: buchi_automaton() == 
%% <br/>&#160;&#160;       {States :: [state()],
%% <br/>&#160;&#160;&#160;  InitialStates :: [state()],
%% <br/>&#160;&#160;&#160;  Transitions :: [{state(),state(),label()}],
%% <br/>&#160;&#160;&#160;  AcceptingStates :: [state()]}</code>,<br/>
%% where <code>label()</code> is a list of propositions, see <A HREF="ltl.html">
%% <code>ltl</code></A>.

-module(buchi).

-export([is_buchi/1,
		 empty_buchi/0,
		 is_empty/1,
		 intersection/2, 
		 ltl_intersection/2, 
		 buchi2digraph/1]).


%% @doc Verifies the Büchi automaton data structure.
%% @spec (buchi_automaton()) -> bool()
is_buchi({States,IStates,Trans,Accept}) 
  when is_list(States), is_list(IStates), is_list(Trans), is_list(Accept) ->
	Id = fun(X) -> X end,
	lists:all(Id,[ is_integer(S) || S <- States]) andalso
		lists:all(Id,[is_integer(S) || S <- IStates]) andalso
		lists:all(Id,[is_tuple(T) andalso (tuple_size(T) == 3) || T <- Trans]) andalso
		lists:all(Id,[is_integer(S) || S <- Accept]);
is_buchi(_) -> false.
	

%% @doc The empty Büchi automaton.
%% @spec () -> buchi_automaton()
empty_buchi() -> {[],[],[],[]}.

%% Empty check
%% @doc Check Büchi automaton for emptiness.
%% @spec (buchi_automaton()) -> bool()
is_empty(B = {_States,_InitStates,_Trans,_Accept}) ->
    case reachable_loop_states(B) of
		[] -> true;
		_ -> false
    end.

%% @private
reachable_loop_states(B = {_States,InitStates,_Trans,Accept}) ->
    Reachable = buchi_utils:reachable(B),
    Res = [ V || V <- Accept,
				 lists:member(V,Reachable),
				 buchi_utils:in_cycle(B,V)],
	Res.


%% @doc Intersection of two Büchi automata.
%% Computes the product/intersection of two BA's,
%% the result is a non-generalized, non-labeled BA.
%% @spec (buchi_automaton(),buchi_automaton()) -> buchi_automaton()
intersection(B1,B2) ->
	case is_buchi(B1) of
		true -> 
			case is_buchi(B2) of
				true -> intersection2(B1,B2);
				false -> erlang:error({not_a_buchi_automaton,B2})
			end;
		false -> erlang:error({not_a_buchi_automaton,B1})
	end.

intersection2(_B1 = {_States1, InitStates1, Trans1, Accept1},
	      _B2 = {States2, InitStates2, Trans2, Accept2}) ->
    AllInitStates =
	[{S1, S2, 1} || S1 <- InitStates1, S2 <- InitStates2],
    AllAccept =
	[{F1, S2, 1} || F1 <- Accept1, S2 <- States2],
    AllTrans =
	[{{S1_1, S2_1, 1}, {S1_2, S2_2, 1}, St1}
	 || {S1_1, S1_2, St1} <- Trans1,
	    {S2_1, S2_2, St2} <- Trans2,
	    St1 == St2,
	    not lists:member(S1_1, Accept1)] ++
	  [{{S1_1, S2_1, 1}, {S1_2, S2_2, 2}, St1}
	   || {S1_1, S1_2, St1} <- Trans1,
	      {S2_1, S2_2, St2} <- Trans2,
	      St1 == St2,
	      lists:member(S1_1, Accept1)] ++
	    [{{S1_1, S2_1, 2}, {S1_2, S2_2, 2}, St1}
	     || {S1_1, S1_2, St1} <- Trans1,
		{S2_1, S2_2, St2} <- Trans2,
		St1 == St2,
		not lists:member(S2_1, Accept2)] ++
	      [{{S1_1, S2_1, 2}, {S1_2, S2_2, 1}, St1}
	       || {S1_1, S1_2, St1} <- Trans1,
		  {S2_1, S2_2, St2} <- Trans2,
		  St1 == St2,
		  lists:member(S2_1, Accept2)],
    Reachable = lists:usort(buchi_utils:reachable(AllTrans, AllInitStates)),
    case Reachable of
      [] -> {[], [], [], []};
      _ ->
	  StMap = lists:zip(lists:seq(1, length(Reachable)), Reachable),
	  Trans = [{stmap(S1, StMap), stmap(S2, StMap), St} || {S1, S2, St} <- AllTrans,
							       lists:member(S1, Reachable)],
	  Accept = [stmap(S, StMap) || S <- AllAccept,
				       lists:member(S, Reachable)],
	  States = lists:seq(1, length(Reachable)),
	  InitStates = [stmap(S, StMap) || S <- AllInitStates],
	  {States, InitStates, Trans, Accept}
    end.

%% @doc LTL intersection of Büchi automaton and LTL 
%% formula translated to Büchi automaton. 
%% The first BA is a System model (or an automata generated
%% from a witness) and the second BA is generated from an 
%% LTL formula. Maybe this operation has a better name!??
%% @spec (buchi_automaton(),buchi_automaton()) -> buchi_automaton()
ltl_intersection(B1,B2) ->
	case is_buchi(B1) of
		true -> 
			case is_buchi(B2) of
				true -> ltl_intersection2(B1,B2);
				false -> erlang:error({not_a_buchi_automaton,B2})
			end;
		false -> erlang:error({not_a_buchi_automaton,B1})
	end.

ltl_intersection2(_B1 = {_States1,InitStates1,Trans1,Accept1},
				  _B2 = {States2,InitStates2,Trans2,Accept2}) ->
    AllInitStates = 
		[{S1,S2,1} || S1 <- InitStates1, S2 <- InitStates2],
    AllAccept =
		[{F1,S2,1} || F1 <- Accept1, S2 <- States2],
    AllTrans = 
		[ {{S1_1,S2_1,1},{S1_2,S2_2,1},St2} || 
			{S1_1,S1_2,St1} <- Trans1, 
			{S2_1,S2_2,St2} <- Trans2, 
			is_sat(St2,St1),
			not lists:member(S1_1,Accept1) ] ++
		[ {{S1_1,S2_1,1},{S1_2,S2_2,2},St2} || 
			{S1_1,S1_2,St1} <- Trans1, 
			{S2_1,S2_2,St2} <- Trans2, 
			is_sat(St2,St1),
			lists:member(S1_1,Accept1) ] ++
		[ {{S1_1,S2_1,2},{S1_2,S2_2,2},St2} || 
			{S1_1,S1_2,St1} <- Trans1, 
			{S2_1,S2_2,St2} <- Trans2, 
			is_sat(St2,St1),
			not lists:member(S2_1,Accept2) ] ++
		[ {{S1_1,S2_1,2},{S1_2,S2_2,1},St2} || 
			{S1_1,S1_2,St1} <- Trans1, 
			{S2_1,S2_2,St2} <- Trans2, 
			is_sat(St2,St1),
			lists:member(S2_1,Accept2) ],
    Reachable = lists:usort(buchi_utils:reachable(AllTrans,AllInitStates)),
    case Reachable of
		[] -> {[],[],[],[]};
		_ ->
			StMap = lists:zip(lists:seq(1,length(Reachable)),Reachable),
			Trans = [{stmap(S1,StMap),stmap(S2,StMap),St} || {S1,S2,St} <- AllTrans,
															 lists:member(S1,Reachable)],

			Accept = [ stmap(S,StMap) || S <- AllAccept,
										 lists:member(S,Reachable)],
			States = lists:seq(1,length(Reachable)),
			InitStates = [ stmap(S,StMap) || S <- AllInitStates],
			{States,InitStates,Trans,Accept}
    end.

%%
%% (Internal) helper functions
%%
is_sat([], _) -> true;
is_sat([ltrue| Lits], Props) -> is_sat(Lits, Props); %% ???
is_sat([{lnot, Prop}| Lits], Props) ->
    not lists:member(Prop, Props) andalso is_sat(Lits, Props);
is_sat([Prop| Lits], Props) ->
    lists:member(Prop, Props) andalso is_sat(Lits, Props).

stmap(N, Ns) ->
    case lists:keysearch(N, 2, Ns) of {value, {N2, N}} -> N2 end.


%% Build buichi-digraph
%% @doc Translate  Büchi automaton into digraph.
%% @see //stdlib/digraph. digraph
%% @spec (buchi_automaton()) -> digraph()
buchi2digraph(B = {States, InitStates, Trans, Accept}) ->
    case is_buchi(B) of
      false ->
			erlang:error({not_a_buchi_automaton,B});
      true ->
			G = digraph:new([cyclic]),
			lists:foreach(fun(S) ->
								  Label = [ initial || lists:member(S,InitStates)] ++
									  [accepting || lists:member(S,Accept)],
								  digraph:add_vertex(G,S,Label)
						  end, States),
			lists:foreach(fun({S1,S2,Label}) ->
								  digraph:add_edge(G,S1,S2, Label)
						  end,Trans),
			{ok,G}
    end.
