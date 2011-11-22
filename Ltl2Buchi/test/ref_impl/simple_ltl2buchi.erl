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

%%% File    : simple_ltl2buchi.erl
%%% Author  : Hans Svensson <>
%%% Description : A simple Ltl2Buchi translation algorithm
%%% Created : 18 Mar 2009 by Hans Svensson <>

-module(simple_ltl2buchi).

-compile(export_all).

translate(Phi) ->
	B0 = simple_to_buchi(Phi),
    basic_ltl2buchi:degeneralize(basic_ltl2buchi:lbl2nonlbl(B0)).

%% Simple BA production using covers (No paper..)
simple_covers([],Cvs) ->
	Cvs;
simple_covers([Phi | Phis], Cvs) ->
%%  	io:format("Input formula: ~p\n",[print_ltl(Phi)]),
	C = lists:usort(simple_cover(Phi)),
	case lists:member({Phi,C},Cvs) of
		true ->
			simple_covers(Phis,Cvs);
		false ->
			simple_covers(Phis ++ get_nexts(C),Cvs ++ [{Phi,C}])
	end.

simple_cover(Phi) ->
	ExpPhi = basic_ltl2buchi:expand_tbl(ltl2buchi:simplify(ltl:pnf(Phi))),
%%     	io:format("Expanded formula: ~p\n",[print_ltl(ExpPhi)]),
%%    	io:format("Expanded dnf:ed formula: ~p\n",[print_sets_ltl(dnf(ExpPhi))]),
%%     	io:format("Computed cover: ~p\n",[lists:map(fun print_cover2/1,split_conjuncts(dnf(ExpPhi)))]),
 	split_conjuncts(basic_ltl2buchi:dnf(ExpPhi)).

simple_to_buchi(Phi) ->
%% 	io:format("Original formula: ~p\n",[print_ltl(Phi)]),
	SimpPhi = ltl_utils:normalize(ltl2buchi:simplify(ltl:pnf(Phi))),
	Covs = simple_covers([SimpPhi],[]),
%% 	io:format("Cover: ~p\n",[print_covers(Covs)]),
	RawStates = lists:usort(lists:concat([S || {_,S} <- Covs])),
	case RawStates of
		[] -> {[],[],[],[]};
		_ ->
			States = lists:zip(lists:seq(1,length(RawStates)),
							   RawStates),
			{_,RawInitStates} = hd(Covs),
			InitStates = lists:map(fun(S) -> lkp(S,States) end,
								   RawInitStates),
			Trans = lists:flatmap(
					  fun({N,S}) ->
							  PhiX = get_next(S),
							  {value,{PhiX,SS}} = lists:keysearch(PhiX,1,Covs),
							  [ {N,lkp(S2,States)} || S2 <- SS ]
					  end,States),
			Untils = lists:usort(
					   lists:filter(fun basic_ltl2buchi:is_until/1,ltl_utils:subformulas(SimpPhi))),
			AcceptSets = lists:map(fun(P) ->
										   accept_states2(P,States)
								   end,Untils),
			AcceptSets2 = case AcceptSets of
							  [] -> lists:seq(1,length(RawStates));
							  _ -> AcceptSets
						  end,
%% 			{[{N,Vars -- [ltrue]} || 
%% 				 {N,{Vars,_}} <- States],
			{[{N,Vars} || 
				 {N,{Vars,_}} <- States],
			 InitStates,Trans,AcceptSets2}
	end.

accept_states2(_, []) ->
    [];
accept_states2(Phi = {until, Phi1, Phi2}, [{N, {Vars, Nexts}}| States]) ->
    case ltl_holds(Phi2, Vars ++ Nexts) orelse
	   not ltl_holds(Phi1, Vars ++ Nexts) orelse
	     not ltl_holds({next, Phi}, Nexts)
	of
      true -> [N| accept_states2(Phi, States)];
      false -> accept_states2(Phi, States)
    end.
					  
ltl_holds({land,Phi1,Phi2},Lits) -> 
	ltl_holds(Phi1,Lits) andalso ltl_holds(Phi2,Lits);
ltl_holds({lor,Phi1,Phi2},Lits) -> 
	ltl_holds(Phi1,Lits) orelse ltl_holds(Phi2,Lits);
%ltl_holds(ltrue,_) -> true;
ltl_holds(X,Lits) ->
	lists:member(X,Lits).

lkp(State,States) ->	
	{value,{N,_}} = lists:keysearch(State,2,States),
	N.
	
	
rewrite_true_false(Vs) ->
	lists:map(fun rewrite_true_false1/1,Vs).

rewrite_true_false1({lnot,ltrue}) -> lfalse;
rewrite_true_false1({lnot,lfalse}) -> ltrue;
rewrite_true_false1(V) -> V.

split_conjuncts(Xs) ->
	lists:filter(fun is_consistent/1,
				 lists:map(fun split_conjunct/1,Xs)).

split_conjunct(Xs) ->
	split_conjunct(Xs,[],[]).

split_conjunct([],Vars,[]) ->
	{lists:usort(rewrite_true_false(Vars)),[{next,ltrue}]};
split_conjunct([],Vars,Nexts) ->
	{lists:usort(rewrite_true_false(Vars)),lists:usort(Nexts)};
split_conjunct([{next,_Phi} = Phi | Xs],Vars,Nexts) ->
	split_conjunct(Xs,Vars,Nexts ++ [Phi]);
split_conjunct([Phi | Xs],Vars,Nexts) ->
	case lists:member(Phi,Vars) of
		true ->
			split_conjunct(Xs,Vars,Nexts);
		false ->
			split_conjunct(Xs,Vars ++ [Phi],Nexts)
	end.

is_consistent({Vars, _Nexts}) ->
    is_consistent(Vars);
is_consistent([]) ->
    true;
is_consistent([{lprop, _X} = V| Xs]) ->
    not lists:member({lnot, V}, Xs) andalso
      is_consistent(Xs);
is_consistent([{lnot, V}| Xs]) ->
    not lists:member(V, Xs) andalso
      is_consistent(Xs);
is_consistent([lfalse| _Xs]) ->
    false;
is_consistent([ltrue| Xs]) ->
    is_consistent(Xs).

get_nexts(Xs) ->
	lists:map(fun get_next/1,Xs).

get_next({_Vars, Nexts}) ->
    ltl:land([Phi || {next, Phi} <- Nexts]).


