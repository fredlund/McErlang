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

%%% File    : modella_ltl2buchi.erl
%%% Author  : Hans Svensson <>
%%% Description : Ltl2Buchi translation from MODELLA paper
%%%               UNFINISHED NOT WORKING!!
%%% Created : 18 Mar 2009 by Hans Svensson <>

-module(modella_ltl2buchi).

-compile(export_all).

%% Compute cover (from MoDeLLA paper)
comp_cover(Phi) ->
    Phi_exp = basic_ltl2buchi:expand_tbl(Phi),
    Cov0 = shannon_exp(Phi_exp),
    io:format("Cover after semantic branching: \n~p\n", [Cov0]),
    Cov1 = lists:map(fun ({Vs, P}) -> {Vs, basic_ltl2buchi:dnf(P)} end, Cov0),
    io:format("Applied dnf to each part of the cover:\n~p\n", [Cov1]),
    Cov2 = lists:map(fun ({Vs, Ps}) ->
			     {Vs, lists:map(fun xfactor/1, Ps)}
		     end, Cov1),
    io:format("Applied xfactor to each part of the cover:\n~p\n", [Cov2]),
    Cov = lists:foldl(fun ({Vs, Ps}, C) ->
			      Si = ltl:land(Vs ++ [xfactor(Ps)]),
			      case postponement_is_safe(Si, Phi) of
				true -> C ++ [Si];
				false ->
				    Subsi = [ltl:land(Vs ++ [P]) || P <- Ps],
				    C ++ Subsi
			      end
		      end, [], Cov2),
    io:format("Final cover:\n~p\n", [Cov]).
	
postponement_is_safe(_,_) ->
	false.

xfactor(Xs) when is_list(Xs) ->
    {next, ltl:land([P || {next, P} <- Xs])};
xfactor(X) -> X.

shannon_exp(Phi) ->
	sets:fold(
	  fun(V,Ps) ->
			  [ {Vs ++ [V], bool_simp(replace(V,ltrue,P))} || {Vs,P} <- Ps ] ++
				  [ {Vs ++ [{lnot,V}], bool_simp(replace(V,lfalse,P))} || {Vs,P} <- Ps ]
	  end,[{[],Phi}],top_vars(Phi)).
	
top_vars({land,A,B}) ->
	sets:union(top_vars(A),top_vars(B));
top_vars({lor,A,B}) ->
	sets:union(top_vars(A),top_vars(B));
top_vars({var,_X} = X) ->	
	sets:from_list([X]);
top_vars(_P) ->
	sets:new().

replace(Var,Val,Var) ->
	Val;
replace(Var,Val,{land,Phi1,Phi2}) ->
	{land,replace(Var,Val,Phi1),replace(Var,Val,Phi2)};
replace(Var,Val,{lor,Phi1,Phi2}) ->
	{lor,replace(Var,Val,Phi1),replace(Var,Val,Phi2)};
replace(_Var,_Val,Phi) -> 
	Phi.

bool_simp({land,Phi1,Phi2}) ->
	case {bool_simp(Phi1),bool_simp(Phi2)} of
		{ltrue,X} -> X;
		{lfalse,_} -> lfalse;
		{X,ltrue} -> X;
		{_X,lfalse} -> lfalse;
		{X,Y} -> {land,X,Y}
	end;
bool_simp({lor,Phi1,Phi2}) ->
	case {bool_simp(Phi1),bool_simp(Phi2)} of
		{ltrue,_} -> ltrue;
		{lfalse,X} -> X;
		{_,ltrue} -> ltrue;
		{X,lfalse} -> X;
		{X,Y} -> {lor,X,Y}
	end;
bool_simp({lnot,ltrue}) -> lfalse;
bool_simp({lnot,lfalse}) -> ltrue;
bool_simp({Op,Phi}) -> 
	{Op,bool_simp(Phi)};
bool_simp({Op,Phi1,Phi2}) ->          
	{Op,bool_simp(Phi1),bool_simp(Phi2)};
bool_simp(Phi) ->                     
	Phi.
