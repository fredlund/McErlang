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

%%% File    : basic_ltl2buchi.erl
%%% Author  : Hans Svensson <>
%%% Description : Basic Ltl2Buchi translation algorithm
%%% Created : 18 Mar 2009 by Hans Svensson <>

-module(basic_ltl2buchi).

-compile(export_all).

translate(Phi) ->
	B0 = basic_ltl_to_buchi(Phi),
    degeneralize(lbl2nonlbl(B0)).

%% Recognizers for some ltl-formulas
is_until({until,_,_}) -> true;
is_until(_) -> false.

is_lit({lprop,_}) -> true;
is_lit({lnot,{lprop,_}}) -> true;
is_lit(ltrue) -> true;
is_lit(_) -> false.

%% Expand using tableau-rules
expand_tbl({until,Psi,Phi} = P) ->
	ltl:lor(expand_tbl(Phi),ltl:land(expand_tbl(Psi),ltl:next(P)));
expand_tbl({release,Psi,Phi} = P) ->
	ltl:land(expand_tbl(Phi),ltl:lor(expand_tbl(Psi),ltl:next(P)));
expand_tbl({always,Phi}) ->
	ltl:land(expand_tbl(Phi),ltl:next(ltl:always(Phi)));
expand_tbl({eventually,Phi}) ->
	ltl:lor(expand_tbl(Phi),ltl:next(ltl:eventually(Phi)));
expand_tbl({next,_Phi} = Phi) ->
	Phi;
expand_tbl({Op,Phi}) -> 
	{Op,expand_tbl(Phi)};
expand_tbl({Op,Phi1,Phi2}) ->
	{Op,expand_tbl(Phi1),expand_tbl(Phi2)};
expand_tbl(Phi) -> 
	Phi.

%% disjunctinve normal form 
%% ltl() -> [[ltl()]]
dnf({land,A,B}) ->
	{As,Bs} = {dnf(A),dnf(B)},
	[ A1 ++ B1 || A1 <- As, B1 <- Bs];
dnf({lor,A,B}) ->
	{As,Bs} = {dnf(A),dnf(B)},
	As ++ Bs;
dnf(X) -> [[X]].
		

%% Basic translation Wolper et. al.
-record(node,{id = undefined,
			  incoming = [],
			  new = [],
			  old = [],
			  next = []}).

set_add(Elem,List) ->
	case lists:member(Elem,List) of
		true -> List;
		false -> List ++ [Elem]
	end.

set_eq(List1,List2) ->
	lists:usort(List1) == 
		lists:usort(List2).

set_union(List1,List2) ->
	lists:usort(List1 ++ List2).

node_upd(Node,[]) ->
	[Node];
node_upd(#node{id = Id} = Node,[#node{id = Id} = _Node | Nodes]) ->
	[Node | Nodes];
node_upd(Node,[Node2 | Nodes]) ->
	[Node2 | node_upd(Node,Nodes)].
	
basic_ltl_to_buchi(Phi) ->
%% 	Phi1 = simplify(ltl_rewrite(pnf(Phi))),
	Phi1 = ltl2buchi:simplify(ltl:pnf(Phi)),
	Nodes = basic_ltl_to_buchi(
			  #node{id = make_ref(),
					incoming = [init],
					new = [Phi1]},
			  []),
%%  	io:format("Nodes: ~p\n",[Nodes]),
	case Nodes of
		[] -> {[],[],[],[]};
		_ ->
			Untils = lists:usort(
					   lists:filter(fun is_until/1,ltl_utils:subformulas(Phi1))),
			AcceptSets = lists:map(fun(P) ->
										   accept_states(P,Nodes)
								   end,Untils),
			InitStates = [N#node.id || N <- lists:filter(
											  fun(N) -> 
													  lists:member(init,N#node.incoming) 
											  end,Nodes)],
			STab = ets:new(buchi_tab_states,[set]),
			States = lists:zip3([N#node.id || N <- Nodes],
								lists:seq(1,length(Nodes)),
								[lists:filter(fun is_lit/1,N#node.old) || N <- Nodes]
							   ),
			lists:foreach(fun(X) -> ets:insert(STab,X) end,States),
			Trans = lists:flatmap(fun(N) ->
										  [{_,X,_}] = ets:lookup(STab,N#node.id),
										  mk_trans(STab,X,N#node.incoming)
								  end,Nodes),
			AcceptSets2 = lists:map(fun(S) -> trans_states(STab,S) end,AcceptSets),
			AcceptSets3 = case AcceptSets2 of
							  [] -> lists:seq(1,length(Nodes));
							  _ -> AcceptSets2
						  end,
			B = {[{X,Ls} || {_,X,Ls} <- States],
				 trans_states(STab,InitStates),
				 Trans,
				 AcceptSets3
				 },
			ets:delete(STab),
			B
	end.

basic_ltl_to_buchi(Node, Nodes) ->
%%        	io:format("Node: ~p\n",[Node]),
%%        	io:format("Nodes: ~p\n",[Nodes]),
    case Node#node.new of
      [] ->
	  case [N || N <- Nodes,
		     set_eq(N#node.old, Node#node.old),
		     set_eq(N#node.next, Node#node.next)]
	      of
	    [N] ->
		%% 					io:format("Merging node with node: ~p\n",[N]),
		node_upd(N#node{incoming=set_union(N#node.incoming, Node#node.incoming)},
			 Nodes);
	    [] ->
		Ref = make_ref(),
		%% 					io:format("Adding node to nodes, creating new node (~p) w. incoming: ~p\n",[Ref,Node#node.id]),
		basic_ltl_to_buchi(#node{id=Ref,
					 incoming=[Node#node.id],
					 new=Node#node.next},
				   set_add(Node, Nodes))
	  end;
      [Phi| Phis] ->
	  case Phi of
	    {land, Phi1, Phi2} ->
		basic_ltl_to_buchi(Node#node{new=Phis ++
						   [Phi1, Phi2] -- Node#node.old,
					     old=set_add(Phi, Node#node.old)},
				   Nodes);
	    {next, Phi1} ->
		basic_ltl_to_buchi(Node#node{new=Phis,
					     old=set_add(Phi, Node#node.old),
					     next=set_add(Phi1, Node#node.next)},
				   Nodes);
	    {until, Phi1, Phi2} ->
		case lists:member(Phi2, set_union(Node#node.new, Node#node.old)) of
		  true ->
		      basic_ltl_to_buchi(Node#node{new=Phis,
						   old=set_add(Phi, Node#node.old)},
					 Nodes);
		  false ->
		      {Node1, Node2} = split_node({[Phi1], [Phi], [Phi2]}, Phi, Phis, Node),
		      %% 							io:format("Node split into Node1: ~p\n" ++
		      %% 									  "                Node2: ~p\n",[Node1,Node2]),
		      basic_ltl_to_buchi(Node2,
					 basic_ltl_to_buchi(Node1, Nodes))
		end;
	    {release, Phi1, Phi2} ->
		case lists:member(Phi1, set_union(Node#node.new, Node#node.old)) andalso
		       lists:member(Phi2, set_union(Node#node.new, Node#node.old))
		    of
		  true ->
		      basic_ltl_to_buchi(Node#node{new=Phis,
						   old=set_add(Phi, Node#node.old)},
					 Nodes);
		  false ->
		      {Node1, Node2} = split_node({[Phi2], [Phi], [Phi1, Phi2]}, Phi, Phis, Node),
		      %% 							io:format("Node split into Node1: ~p\n" ++
		      %% 									  "                Node2: ~p\n",[Node1,Node2]),
		      basic_ltl_to_buchi(Node2,
					 basic_ltl_to_buchi(Node1, Nodes))
		end;
	    {lor, Phi1, Phi2} ->
		{Node1, Node2} = split_node({[Phi1], [], [Phi2]}, Phi, Phis, Node),
		%% 					io:format("Node split into Node1: ~p\n" ++
		%% 							  "                Node2: ~p\n",[Node1,Node2]),
		basic_ltl_to_buchi(Node2,
				   basic_ltl_to_buchi(Node1, Nodes));
	    Lit ->
			  case Lit == lfalse orelse
				  lists:member(ltl:negate(Lit), Node#node.old)
				  of
				  true -> Nodes;
				  false -> basic_ltl_to_buchi(Node#node{new=Phis,
														old=set_add(ltl:negate(ltl:negate(Lit)), Node#node.old)},
											  %% 											 old = case Lit of
											  %% 													   ltrue -> Node#node.old;
											  %% 													   _ -> set_add(ltl_neg(ltl_neg(Lit)),Node#node.old)
											  %% 												   end
											  Nodes)
		end
	  end
    end.

split_node({New1, Next1, New2}, Phi, New, Node) ->
    {Node#node{id=make_ref(),
	       new=New ++ New1 -- Node#node.old,
	       old=set_add(Phi, Node#node.old),
	       next=set_union(Next1, Node#node.next)},
     Node#node{id=make_ref(),
	       new=New ++ New2 -- Node#node.old,
	       old=set_add(Phi, Node#node.old)}}.

accept_states(_, []) ->
    [];
accept_states(Phi = {until, _, Phi2}, [Node| Nodes]) ->
    case not lists:member(Phi, Node#node.old) orelse
	   lists:member(Phi2, Node#node.old)
	of
      true -> [Node#node.id| accept_states(Phi, Nodes)];
      false -> accept_states(Phi, Nodes)
    end.

mk_trans(_STab,_N,[]) ->
	[];
mk_trans(STab,N,[init | Ns]) ->
	mk_trans(STab,N,Ns);
mk_trans(STab,N,[N2 | Ns]) ->
	[{N2,X,_}] = ets:lookup(STab,N2),
	[{X,N} | mk_trans(STab,N,Ns)].

trans_states(STab,SS) ->
	lists:map(fun(S) ->
					  [{_,X,_}] = ets:lookup(STab,S),
					  X
			  end,SS).

%%%
%% Some more general functions handling generalized/labeled BA
%%%


%%
%% Büchi recognizers
%%

%% True if the BA has its labels in the states
is_labeled({_,_,[{_,_} | _],_}) ->
    true;
is_labeled({[{_,_} | _],_,_,_}) ->
    true;
is_labeled({[],_,[],_}) ->
    true;
is_labeled(_) ->
    false.

is_nonlabeled({_,_,[{_,_,_} | _],_}) ->
    true;
is_nonlabeled({[X|_],_,[],_}) when is_integer(X) ->
    true;
is_nonlabeled({[],_,[],_}) ->
    true;
is_nonlabeled(_) ->
    false.

%% True if The BA is generalized
is_generalized({_,_,_,[Ac | _]}) ->
    is_list(Ac);
is_generalized(_) ->
    false.

%% generalized_buchi() -> buchi()
%% @doc Translate generalized Büchi automaton into non-generalized.
%% @spec (buchi_automaton()) -> buchi_automaton()
degeneralize(B) ->
    degeneralize2(make_unlabeled(B)).
degeneralize2(B = {States,InitStates,Trans,Accepts}) ->
    case is_generalized(B) of
		false -> B;
		true ->
			case length(Accepts) of
				1 -> {States,InitStates,Trans,hd(Accepts)};
				_ ->	 
					Accepts1 = remove_subsets(Accepts),
					%% 		io:format("Accpts: ~p\nAccepts1: ~p\n",[Accepts,Accepts1]),
					Trans1 = degen_trans(Trans,length(States),length(Accepts1),Accepts1),
					Reachable = lists:usort(buchi_utils:reachable(Trans1,InitStates)),
					Trans2 = [{S1,S2,St} || {S1,S2,St} <- Trans1, lists:member(S1,Reachable)],
					Accept = [S || S <- hd(Accepts1),lists:member(S,Reachable)],
					StMap = lists:zip(lists:seq(1,length(Reachable)),Reachable),
					Trans3 = lists:map(
							   fun({S1,S2,St}) -> 
									   {stmap(S1,StMap),stmap(S2,StMap),St} 
							   end,Trans2),
					States1 = [ New || {New,_Old} <- StMap],
					InitStates1 = lists:map(fun(S) -> stmap(S,StMap) end,InitStates),
					Accept1 = lists:map(fun(S) -> stmap(S,StMap) end,Accept),
					{States1,InitStates1,Trans3,Accept1}
			end
	end.

stmap(N, Ns) ->
    case lists:keysearch(N, 2, Ns) of {value, {N2, N}} -> N2 end.


offset(S,Ns) ->
    case S rem Ns of
		0 -> Ns;
		X -> X
    end.

degen_trans(Trans, NStates, NCopies, Accepts) ->
    %% 'Loops' can be combined, lists:member can only match some transitions...
    Trans1 = [{N * NStates + S1, N * NStates + S2, St}
	      || {S1, S2, St} <- Trans,
		 N <- lists:seq(0, NCopies - 1)],
    %% 	io:format("Trans1: ~p\n",[Trans1]),
    lists:foldl(fun ({N, F}, Ts) ->
			F2 = lists:map(fun (X) -> X + NStates * N end, F),
			%% 					 io:format("F2: ~p\n",[F2]),
			lists:map(fun ({S1, S2, St}) ->
					  case lists:member(S1, F2) of
					    true ->
						{S1, offset(S2, NStates) + NStates * ((N + 1) rem NCopies), St};
					    false ->
						{S1, S2, St}
					  end
				  end, Ts)
		end, Trans1, lists:zip(lists:seq(0, NCopies - 1), Accepts)).

%% @private
remove_subsets(Lists) ->
	lists:map(fun sets:to_list/1,
			  remove_subsets1(lists:map(fun sets:from_list/1,Lists),[])).

remove_subsets1([],Sets) ->
	Sets;
remove_subsets1([Set | Sets],Sets2) ->
	case sets:to_list(Set) == [] of
		true -> remove_subsets1(Sets,[Set | Sets2]);
		false ->
			case not lists:any(fun(X) -> X end,
							   lists:map(fun(X) -> sets:is_subset(X,Set) end,Sets ++ Sets2)) of
				true  -> remove_subsets1(Sets,[Set | Sets2]);
				false -> remove_subsets1(Sets,Sets2)
			end
	end.

make_unlabeled(B) ->
    case is_labeled(B) andalso not is_nonlabeled(B) of
      true ->
	  io:format("*WARNING*: Changed buchi automata from labeled into non-labeled!!\n"),
	  lbl2nonlbl(B);
      false ->
	  B
    end.

%% @doc Translate labeled Büchi automaton into non-labeled.
%% @spec (buchi_automaton()) -> buchi_automaton()
lbl2nonlbl({[],_InitStates,_Trans,_Accepts}) ->
    {[1],[1],[],[]};
lbl2nonlbl(B = {States,InitStates,Trans,Accepts}) ->
    case is_labeled(B) of
		true -> 
			NewStates = [1 | [ S+1 || {S,_} <- States]],
			NewTrans = [{S1+1,S2+1,element(2,lists:nth(S2,States))} || {S1,S2} <- Trans] ++
				[{1,S+1,element(2,lists:nth(S,States))} || S <- InitStates],
			NewAccepts = case is_generalized(B) of
							 true -> [ [ S+1 || S <- SS] || SS <- Accepts];
							 false -> [ S+1 || S <- Accepts ]
						 end,
			{NewStates,[1],lists:usort(NewTrans),NewAccepts};
		false ->
			B
    end.

%% non-labeled to labeled translation
%% Doesn't make sense really...
%% nonlbl2lbl(B = {_States,_InitStates,Trans,Accepts}) ->
%%     case is_nonlabeled(B) of
%% 	true -> 
%% 	    NewInitStates = [S-1 || {1,S,_} <- Trans ],
%% 	    NewStates = lists:usort([{S-1,St} || {_,S,St} <- Trans ]),
%% 	    NewTrans = [{S1-1,S2-1} || {S1,S2,_} <- Trans, S1 > 1],
%% 	    NewAccepts = case is_generalized(B) of
%% 			     true -> [ [ S-1 || S <- SS, S > 1] || SS <- Accepts];
%% 			     false -> [ S-1 || S <- Accepts, S > 1 ]
%% 			 end,
%% 	    {NewStates,NewInitStates,lists:usort(NewTrans),NewAccepts};
%% 	false -> 
%% 	    B
%%     end.	
