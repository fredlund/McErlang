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
%% @doc Module for reduction of Büchi automata.
%% Exports a general <em>reduce</em> function, which is a collection
%% of various reduction techniques for Büchi automata.
%%
%% @type buchi_automaton(). A tuple structure representing a Büchi automaton.
%% (See <A HREF="buchi.html"><code>buchi</code></A> for data type definition.)
%% @todo Export a more general reduce (with options!?)

-module(buchi_reduce).

-export([reduce/1]).

-export([remove_unnecessary_trans/1,
		 remove_non_reachable/1,
		 remove_unnecessary_states_simp/1,
		 reduce_accept/1,
		 remove_never_accept/1,
		 remove_fixed_formula_balls/1,
		 basic_bisim_red/1,
		 strong_fair_sim_red/1]).

%%Exports for testing
%-export([expand_accept/1,
%		 reduce_group/1
		%% , remove_unnecessary_states/1
%		]).

%% @doc Reductions and optimization of Büchi-automaton
%% @spec  (buchi_automaton()) -> buchi_automaton()
reduce(B) ->
	reduce1(normalize_trans(B)).
	
reduce1(B) ->
 	B0 = remove_unnecessary_trans(B),
 	B1 = remove_non_reachable(B0),
 	B11 = remove_unnecessary_states_simp(B1),
 	B2 = reduce_accept(B11),
 	B3 = remove_never_accept(B2),
 	B4 = remove_fixed_formula_balls(B3),
	B5 = basic_bisim_red(B4),
 	B6 = strong_fair_sim_red(B5),
	case size_of(B6) < size_of(B) of
		true ->
%% 			io:format("B0: ~p\nB1: ~p\nB2: ~p\nB3: ~p\nB4: ~p\nB5: ~p\nB6: ~p\n",
%% 					  [B0,B1,B2,B3,B4,B5,B6]),
			reduce1(B6);
		false ->
			B6
	end.

%% Normalize transitions, sort labels and remove true from labels
normalize_trans({States,InitStates,Trans,Accept}) ->
	Trans1 = [ {S1,S2,lists:usort(St -- [ltrue,{lnot,lfalse}])} 
			   || {S1,S2,St} <- Trans],
	{States,InitStates,lists:usort(Trans1),Accept}.

%% Remove unnecessary transitions,
%% Sx -- [] --> Sy and Sx -- [a,b] --> Sy 
%% is reduced to only Sx -- [] --> Sy
%% and
%% Sx -- [a,b] --> Sy and Sx -- [a,!b] --> Sy
%% is reduced to only Sx -- [a] --> Sy, etc
%% @private
remove_unnecessary_trans(B = {_,_,[],_}) ->
	B;
remove_unnecessary_trans({States,InitStates,Trans,Accept}) ->
	Trans1 = remove_unnecessary_trans1(Trans,hd(Trans),[]),
 	{States,InitStates,Trans1,Accept}.

remove_unnecessary_trans1([],Tr,Grp) ->
	analyze_group(Tr,Grp);
remove_unnecessary_trans1([{X,Y,Lbl} | Trs],Tr = {X,Y,_},Grp) ->
	remove_unnecessary_trans1(Trs,Tr,[Lbl | Grp]);
remove_unnecessary_trans1([Tr = {_,_,Lbl} | Trs], Tr2, Grp) ->
	analyze_group(Tr2,Grp) ++ remove_unnecessary_trans1(Trs,Tr,[Lbl]).


analyze_group({X,Y,_},Grp) ->	
	[{X,Y,Lbl} || Lbl <- reduce_group(Grp)].

reduce_group(Grp) ->
	case lists:member([],Grp) of
		true -> [[]]; %Only the TRUE transition
		false ->
			Grp1 = simp(Grp),
			Remove = [Lbl2  
					  ||	Lbl1 <- Grp1,
							Lbl2 <- Grp1,
							Lbl1 /= Lbl2,
							Lbl1 -- Lbl2 == []],
			Grp1 -- Remove
	end.

simp(Grp) ->
	Grp1 = lists:usort(simp_2(simp_1(Grp))),
	case Grp1 == Grp of
		true -> Grp;
		false -> simp(Grp1)
	end.		

simp_1(Grp) ->
	simp_1_(Grp,Grp).
simp_1_(Grp,[]) ->
	Grp;
simp_1_(Grp,[Lbl | Grp2]) ->
	case simp_1__(Grp,Lbl,Grp2) of
		false ->
			simp_1_(Grp,Grp2);
		NewGrp ->
			simp_1(NewGrp)
	end.
simp_1__(_,_,[]) ->
	false;
simp_1__(Grp,Lbl,[Lbl2 | Grp2]) ->
	case simp_1(Lbl,Lbl2) of
		false ->
			simp_1__(Grp,Lbl,Grp2);
		NewLbl -> (Grp -- [Lbl,Lbl2]) ++ [NewLbl]
	end.

simp_1(Lbl1,Lbl2) ->
	case Lbl1 -- Lbl2 of
		[X] -> case Lbl2 -- Lbl1 of
				   [Y] -> case X == ltl:negate(Y) of
							  true  -> Lbl1 -- [X];
							  false -> false
						  end;
				   _ -> false
			   end;
		_ -> false
	end.

simp_2(Grp) ->
	Singletons = [S || S = [_] <- Grp],
	[lists:foldl(fun([S],L) -> simp_2(S,L) end, Lbl,Singletons) || Lbl <- Grp].
simp_2(X,Lbl) ->
	case lists:member(ltl:negate(X),Lbl) of
		true -> Lbl -- [ltl:negate(X)];
		false -> Lbl
	end.

%% Remove non reachable states
%% @private
remove_non_reachable(B = {States,InitStates,Trans,_Accept}) ->
    Reachable = buchi_utils:reachable(Trans,InitStates),
    RStates = States -- Reachable,
    remove(RStates,B).    

%% Reduce the number of accepting states (not necessarily a good thing!?)
%% @private
reduce_accept(B = {States,InitStates,Trans,Accept}) ->
	InCycle = lists:flatten(buchi_utils:cycles(B)),
	NotInCycle = Accept -- InCycle,
	{States,InitStates,Trans,Accept -- NotInCycle}.

expand_accept(B = {States,InitStates,Trans,Accept}) ->
	InCycle = lists:flatten(buchi:cycles(B)),
	NotInCycle = (States -- InCycle) -- Accept,
	AddAccept = [S || S <- NotInCycle,
					  accepts([S2 || {S1,S2,_} <- Trans, S1 == S],Accept)],
	{States,InitStates,Trans,Accept ++ AddAccept}.

accepts([],_) ->
	false;
accepts([S],Accept) ->
	lists:member(S,Accept);
accepts([S | Ss],Accept) ->
	lists:member(S,Accept) andalso
    accepts(Ss,Accept).

%% Remove states that can never lead to an accepting state
%% @private
remove_never_accept(B = {States,_InitStates,Trans,Accept}) ->
	%Strongly connected components containing an accepting state
	SCs = lists:filter(fun(SC) ->
							   SC -- Accept /= SC
					   end,buchi_utils:cycles(B)),
	RemCands = States -- lists:flatten(SCs),
	Rem = [ S || S <- RemCands,
				 not reaches_accept(buchi_utils:reachable(Trans,[S]),SCs)],
	remove(Rem,B).

reaches_accept(_Reachable,[]) ->
	false;
reaches_accept(Reachable,[SC | SCs]) ->
	case (Reachable -- SC) /= Reachable of
		true -> true;
		false -> reaches_accept(Reachable,SCs)
	end.

%% Remove unnecessary states
%% This one is dubious...
%% @private
remove_unnecessary_states_simp({States,InitStates,Trans,Accept}) ->
	Rems = [ {S1,S2} || S <- States,
						length([ T || T = {S1_,_,_} <- Trans,
									  S1_ == S]) == 1,
						length([ T || T = {_,S2_,_} <- Trans,
									  S2_ == S]) == 1,
						{S1,S2,Lbl} <- Trans,
						{S1_,S2_,Lbl_} <- Trans,
						{S1x,S2x,Lblx} <- Trans,
						S1x == S1_, S2x == S2, Lblx /= Lbl,
						S1 == S, S2_ == S, S1 /= S2, Lbl == Lbl_,
						lists:member({S2,S2,Lbl},Trans)],
%% 	io:format("Rems = ~p\n",[Rems]),
	NewTrans = [ case proplists:get_value(S2,Rems) of
 					 undefined -> {S1,S2,Lbl};
 					 Val -> {S1,Val,Lbl} 
 				 end || {S1,S2,Lbl} <- Trans],
 	NewInit = [ case proplists:get_value(S,Rems) of
 					 undefined -> S;
 					 Val -> Val 
 				 end || S <- InitStates],
 	remove([S || {S,_} <- Rems],{States,NewInit,NewTrans,Accept}).
	

remove_unnecessary_states(B = {States,InitStates,Trans,Accept}) ->
	InCycle = lists:flatten(buchi_utils:cycles(B)),
	NotInCycle = (States -- InCycle),
 	Candidates = [ S || S <- NotInCycle -- Accept,
 						length([T || T = {S1,_,_} <- Trans, S1 == S]) == 1],
 	ExtCands = [ {S,[],S2} || S <- Candidates,
 							   {S1_,S2,[]} <- Trans,
 							   S1_ == S, S /= S2],
 	OkOnes = [ {S,S2} || {S,Lbl,S2} <- ExtCands,
 						 {S2_,S2_,Lbl_} <- Trans,
 						 S2_ == S2,
 						 Lbl_ == Lbl ],
	io:format("InCycle: ~p\nCands: ~p\nECands. ~p\n",[InCycle,Candidates,ExtCands]),
 	NewTrans = [ case proplists:get_value(S2,OkOnes) of
 					 undefined -> {S1,S2,Lbl};
 					 Val -> {S1,Val,Lbl} 
 				 end || {S1,S2,Lbl} <- Trans],
 	NewInit = [ case proplists:get_value(S,OkOnes) of
 					 undefined -> S;
 					 Val -> Val 
 				 end || S <- InitStates],
 	remove([S || {S,_} <- OkOnes],{States,NewInit,NewTrans,Accept}).

%%%
%% Reduce fixed formula balls
%%%
%% @private
remove_fixed_formula_balls(B = {_States,_InitStates,Trans,Accept}) ->
	SCs = lists:filter(
			fun({_,_,_}) -> true; (_) -> false end,
			lists:map(fun(X) ->
							  is_fixed_formula_ball(X,Trans,Accept)
					  end,
					  buchi_utils:cycles(B))),
	collapse_balls(SCs,B).

is_fixed_formula_ball([_X],_Trans,_) -> false;
is_fixed_formula_ball(SC,Trans,Accept) ->
	case is_ball(SC,Trans) of
		true ->
			Lbls = lists:usort([ St || {S1,S2,St} <- Trans,
									   lists:member(S1,SC),
									   lists:member(S2,SC)]),
			Accepting = lists:any(fun(X) -> X end,
								  lists:map(fun(X) -> lists:member(X,Accept) end,SC)),
			case Lbls of
				[X] -> {SC,X,Accepting};
				_ -> false
			end;
		false -> false
	end.

is_ball(SC,Trans) ->			
	lists:all(fun(X) -> X end,
			  [ lists:member(S2,SC) || {S1,S2,_St} <- Trans,
									   lists:member(S1,SC)]).

collapse_balls([],B) -> B;
collapse_balls(SCs,{States,InitStates,Trans,Accept}) ->
	InTrans = 
		lists:map(
		  fun({SC,_,_}) ->
				  [ {S1,St} || {S1,S2,St} <- Trans, 
							   lists:member(S2,SC),
							   not lists:member(S1,SC)]
		  end,SCs),
	NewStates = lists:seq(length(States) + 1,
						  length(States) + length(SCs)),
	NewTrans = lists:flatmap(
				 fun({NS,{_SC,Lbl,_},InTrs}) ->
						 [{NS,NS,Lbl}] ++ [{S1,NS,St} || {S1,St} <- InTrs]
				 end,lists:zip3(NewStates,SCs,InTrans)),
	NewInitStates = lists:foldl(fun({NS,{SC,_,_}},InitS) ->
										case (InitS -- SC) /= InitS of
											true -> InitS ++ [NS];
											false -> InitS
										end
								end,InitStates,lists:zip(NewStates,SCs)),
	NewAccept = [ S || {S,{_,_,true}} <- lists:zip(NewStates,SCs) ],
	Rem = lists:flatten([SC || {SC,_} <- SCs]),
	_B = remove(Rem,{States ++ NewStates,
					NewInitStates,
					Trans ++ NewTrans, 
					Accept ++ NewAccept}).
							 

%%%
%% Basic bisimulation reduction
%%%
%% @private
basic_bisim_red(_B = {States,InitStates,Trans,Accept}) ->
	CInit0 = lists:reverse([{N,1} || N <- States]),
	CInit1 = lists:usort([{N,1} || N <- Accept] ++ [{N,2} || N <- States -- Accept]),
	Cs = bbr(CInit0,CInit1,States,Trans),
	NStates = lists:usort([ C || {_S,C} <- Cs]),
	NAccept = lists:usort([ lkp_c(S,Cs) || S <- Accept]),
	NInitStates = lists:usort([ lkp_c(S,Cs) || S <- InitStates]),
	NTrans = lists:usort([{lkp_c(S1,Cs),lkp_c(S2,Cs),St} || {S1,S2,St} <- Trans]),
	{NStates,NInitStates,NTrans,NAccept}.


bbr(C0,C1,Ss,Trs) ->
	case C0 == C1 of
		true -> C1;
		false ->
			C2a = [ {lkp_c(S,C1),
					 [{lkp_c(S2,C1),St} || {X,S2,St} <- Trs,X == S]} 
					|| S <- Ss],
			C2b = lists:usort(C2a),
			C2c = lists:zip(C2b,lists:seq(1,length(C2b))),

			Orig = lists:zip(Ss,C2a),
			C2 = [{S,proplists:get_value(C,C2c)} || {S,C} <- Orig],

			bbr(C1,C2,Ss,Trs)
	end.

%%%
%% Strong fair simulation reduction 
%%%
%% @private
strong_fair_sim_red(_B = {States,InitStates,Trans,Accept}) ->
%% 	io:format("B: ~w\n",[_B]),
	CInit0 = [{N,1} || N <- States],
	CInit1 = lists:usort([{N,1} || N <- Accept] ++ [{N,2} || N <- States -- Accept]),
	PoInit0 = [],
	PoInit1 = [{2,1,true},{1,1,true},{2,2,true},{1,2,false}],
	{Cs,Ns} = sfsr(CInit0,CInit1,PoInit0,PoInit1,States,Trans),
%% 	io:format("Res: ~p :: ~p\n",[Cs,Ns]),
	NStates = lists:usort([ C || {_S,C} <- Cs]),
	NAccept = lists:usort([ lkp_c(S,Cs) || S <- Accept]),
	NInitStates = lists:usort([ lkp_c(S,Cs) || S <- InitStates]),
%% 	io:format("New States: ~p\nNew accept: ~p\nNew Init: ~p\n",
%% 			  [NStates,NAccept,NInitStates]),
	NTrans = lists:usort([{lkp_c(S1,Cs),lkp_c(S2,Cs),St} 
						  || {S1,S2,St} <- Trans,
							 lists:member({lkp_c(S2,Cs),St},
										  proplists:get_value(S1,Ns))
								]),
	{NStates,NInitStates,NTrans,NAccept}.

sfsr([],_,_,_,_,_) ->
	{[],[]};
sfsr(C0,C1,Po0,Po1,Ss,Trs) ->
%% 	io:format("{ C0: ~p\n  C1: ~p\n  Po0: ~p\n  Po1: ~p\n  Ss: ~p}\n",[C0,C1,Po0,Po1,Ss]),
	N1 = i_max_neighbor_sets(Ss,Trs,C1,Po1),
	case C0 == C1 andalso po_length(Po0) == po_length(Po1) of
		true -> {C1,N1};
		false ->
			C2a = [{lkp_c(S,C1),proplists:get_value(S,N1)} || S <- Ss],
			C2b = lists:usort(C2a),
%% 			io:format("N1: ~p\nC2a: ~p\nC2b: ~p\n",[N1,C2a,C2b]),
			Po2a =
				[ case po(C2i_1,C1i_1,Po1) andalso 
					  i_dominates_set(N1i_1,N2i_1,Po1) of
					  true  -> {C2i,C1i,true};
					  false -> {C2i,C1i,false}
				  end || C1i = {C1i_1,N1i_1} <- C2b,
						 C2i = {C2i_1,N2i_1} <- C2b],
			C2c = lists:zip(C2b,lists:seq(1,length(C2b))),
			C2 = [{S,proplists:get_value(C,C2c)} || {S,C} <- lists:zip(Ss,C2a)],
			Po2 = lists:usort([{proplists:get_value(Ci1,C2c),
								proplists:get_value(Ci2,C2c),V} || {Ci1,Ci2,V} <- Po2a]),
			%%  						io:format("Po2a: ~p\nC2c: ~p\nPo2: ~p\n",[Po2a,C2c,Po2]),
			sfsr(C1,C2,Po1,Po2,Ss,Trs)
	end.

po_length(Po) ->
	length([true || {_,_,true} <- Po]).  


i_max_neighbor_sets(States,Trans,C,Po) ->
	[i_max_neighbor_set(S,Trans,C,Po) || S <- States].

%% i_max_neighbor_set(State,Trans,C,Po) ->
%% 	{State,
%% 	 lists:usort([ {lkp_c(S2,C),Lbl} 
%% 				   || Tr = {S1,S2,Lbl} <- Trans, State == S1,
%% 					  i_maximal(S1,{lkp_c(S2,C),Lbl},Trans -- [Tr],C,Po)])}.

i_max_neighbor_set(State,Trans,C,Po) ->
	Neighbors = [ {lkp_c(S2,C),Lbl} || {S1,S2,Lbl} <- Trans, State == S1],
	{State,i_max_nb_set(Neighbors,[],Po)}.

i_max_nb_set([],Set,_) ->
	lists:usort(Set);
i_max_nb_set([NBi | NBs],Set,Po) ->
	case [NBn || NBn <- NBs, i_dominates(NBn,NBi,Po)] of
		[] ->
			Dominated = [NBn || NBn <- NBs, i_dominates(NBi,NBn,Po)],
			i_max_nb_set(NBs -- Dominated,[NBi | Set],Po);
		_ -> 
			i_max_nb_set(NBs,Set,Po)
	end.

%% i_maximal
%% i_maximal(Q,{CQ1,Tau},Trans,C,Po) ->
%% %% 	io:format("Is ~p i_maximal for ~p (~p)",[{CQ1,Tau},Q,Trans]),
%% 	IDom = [ i_dominates({lkp_c(S2,C),Lbl},{CQ1,Tau},Po)
%% 			 || {S1,S2,Lbl} <- Trans,
%% 				Q == S1],
%% 	not lists:any(fun(X) -> X end,IDom).

i_dominates({C1,Sig},{C2,Tau},Po) ->
	case po(C2,C1,Po) of
		false -> %io:format("~p *NOT* i-dominates ~p\n",[{C1,Sig},{C2,Tau}]),
			false;
		true ->
			case Sig of
				[] -> true;
				_ ->
					case Sig -- Tau of
						[] -> %io:format("~p i-dominates ~p\n",[{C1,Sig},{C2,Tau}]),
							true;
						_ ->  %io:format("~p *NOT* i-dominates ~p\n",[{C1,Sig},{C2,Tau}]),
							false
					end
			end
	end.

i_dominates_set(Nq1,Nq2,Po) ->		
	Res = lists:all(
			fun(X) -> X end,
			[ lists:any(fun(X) -> X end,
						[ i_dominates({C_,Sigma},{C,Tau},Po)
						  || {C_,Sigma} <- Nq1])
			  || {C,Tau} <- Nq2]),
	Res.

po(X1,X2,[{X1,X2,B} | _]) -> B;
po(X1,X2,[_ | Po]) ->
	po(X1,X2,Po).

lkp_c(Elem,Lst) ->
	%% 	io:format("lkp_c(~p,~p)\n",[Elem,Lst]),
	{value,{_,X}} = lists:keysearch(Elem,1,Lst),
	X.

size_of({States,_InitStates,Trans,_Accepts}) ->
    length(States) + length(Trans).

%% Build buichi-digraph
buchi2digraph(_B = {States,_InitStates,Trans,Accept}) ->
    G = digraph:new([cyclic]),
	lists:foreach(fun(N) ->
						  digraph:add_vertex(G,N,lists:member(N,Accept))
				  end,States),
	lists:foreach(fun({S1,S2,St}) ->
						  digraph:add_edge(G,S1,S2,St)
				  end,Trans),
	{ok,G}.

%% Remove Rstates from B and adapt accordingly
remove(RStates,_B = {States,InitStates,Trans,Accepts}) ->
    NewStates = States -- RStates,
    case NewStates of
		[] -> {[],[],[],[]};
		_ ->
			StMap = lists:zip(lists:seq(1,length(NewStates)),NewStates),
			NewInitStates = [ stmap(S,StMap) || S <- InitStates,
												not lists:member(S,RStates) ],
			NewTrans = [ {stmap(S1,StMap),stmap(S2,StMap),St} 
						 || {S1,S2,St} <- Trans, 
							not lists:member(S1,RStates),
							not lists:member(S2,RStates)],
			NewAccept = [ stmap(S,StMap) || S <- Accepts, not lists:member(S,RStates) ],
			{lists:seq(1,length(NewStates)),
			 NewInitStates,NewTrans,NewAccept}
    end.							

stmap(N,Ns) ->
    case lists:keysearch(N,2,Ns) of
		{value,{N2,N}} -> N2
    end.
