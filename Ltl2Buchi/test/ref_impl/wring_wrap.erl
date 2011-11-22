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

%%% File    : wring_wrap.erl
%%% Author  : Hans Svensson <>
%%% Description : Wrapper for wring (used to test LTL2Buchi impl.)
%%% Created :  3 Mar 2009 by Hans Svensson <>

-module(wring_wrap).

-compile(export_all).

%% ltl2wring({next,Phi}) ->
%% 	"X(" ++ ltl2wring(Phi) ++ ")"; 
%% ltl2wring({always,Phi}) ->
%% 	"G(" ++ ltl2wring(Phi) ++ ")"; 
%% ltl2wring({eventually,Phi}) ->
%% 	"F(" ++ ltl2wring(Phi) ++ ")"; 
%% ltl2wring({until,Psi,Phi}) ->
%% 	ltl2wring(Psi) ++ "U" ++ ltl2wring(Phi);
%% ltl2wring({release,Psi,Phi}) ->
%% 	ltl2wring(Psi) ++ "R" ++ ltl2wring(Phi);
%% ltl2wring({ltl_not,{var,X}}) ->
%% 	atom_to_list(X)++ "=0";
%% ltl2wring({ltl_not,{ltl_not,X}}) ->
%% 	ltl2wring(X);
%% ltl2wring({ltl_not,Phi}) ->
%% 	"(!(" ++ ltl2wring(Phi) ++ "))";
%% ltl2wring({var,X}) ->
%% 	atom_to_list(X)++ "=1";
%% ltl2wring(ltrue) ->
%% 	"TRUE";
%% ltl2wring(ltl_false) ->
%% 	"FALSE";
%% ltl2wring({ltl_and,Phi1,Phi2}) ->
%% 	"(" ++ ltl2wring(Phi1) ++ "*" ++ ltl2wring(Phi2) ++ ")";
%% ltl2wring({ltl_or,Phi1,Phi2}) ->
%% 	"(" ++ ltl2wring(Phi1) ++ "+" ++ ltl2wring(Phi2) ++ ")".
%% ltl2wring(X) when is_atom(X)->
%% 	atom_to_list(X);
%% ltl2wring(_X) ->
%% 	"".

run(Ltl) ->
%% 	StrLtl = ltl2wring(Ltl),
	StrLtl = ltl:pp(Ltl,wring),
	Cmd = "../test/ref_impl/scripts/run_wring.sh /tmp/foo '" ++ StrLtl ++ "'",
%% 	io:format("Cmd:~s\n",[Cmd]),
	Res = os:cmd(Cmd),
%%    	io:format("Res: \n~s\n",[Res]),
	parse_res(Res).

parse_res(Str) ->
	Lines = string:tokens(Str,"\n"),
	case lists:dropwhile(fun(X) -> X /= "States" end,Lines) of
		[] -> erlang:error("Wring_crash");
		_ -> ok
	end,
	Lines1 = tl(lists:dropwhile(fun(X) -> X /= "States" end,Lines)),
	States = lists:takewhile(fun(X) -> X /= "Arcs" end,Lines1),
	Lines2 = tl(lists:dropwhile(fun(X) -> X /= "Arcs" end,Lines1)),
	Arcs   = lists:takewhile(fun(X) -> X /= "Fair Sets" end,Lines2),
	Lines3 = tl(lists:dropwhile(fun(X) -> X /= "Fair Sets" end,Lines2)),
	FairS  = lists:takewhile(fun(X) -> X /= "End" end,Lines3),
	B1 = mk_buchi(lists:sort(parse_states(States)),
				  lists:sort(parse_arcs(Arcs)),
				  lists:map(fun lists:sort/1,parse_fairsets(FairS))),
	basic_ltl2buchi:degeneralize(
	  basic_ltl2buchi:lbl2nonlbl(B1)).

parse_states([]) ->
	[];
parse_states([S | Ss]) ->
	[Id,_,SLbls] = string:tokens(S,":"),
	N = tr_id(Id),
	Lbls = parse_labels(
			 string:tokens(
			   string:strip(
				 string:strip(
				   string:strip(SLbls),left,${),
				 right,$}),
			   ",")),
	[{N,Lbls} | parse_states(Ss)].

parse_labels([]) ->
    [];
parse_labels([Str| Lbls]) ->
    [SLbl, Val] = string:tokens(Str, "="),
    Lbl =
	case Val of
	  "0" ->
	      ltl:lnot(ltl:prop(list_to_atom(SLbl)));
	  "1" ->
	      ltl:prop(list_to_atom(SLbl))
	end,
    [Lbl| parse_labels(Lbls)].

parse_arcs([]) ->
	[];
parse_arcs([SArc | Arcs]) ->
	IsInit = 
		case SArc of
			[$- | _] -> %% Initial state
				true;
			_ ->
				false
		end,
	[Id,Tos] = string:tokens(lists:nthtail(3,SArc)," ->"),
	N = tr_id(Id),
	Ns = lists:map(fun tr_id/1, string:tokens(
								  string:strip(string:strip(Tos,left,${),right,$}),
								  ",")),
	[{N,Ns,IsInit} | parse_arcs(Arcs)].

tr_id([$n | X]) ->
	list_to_integer(X);
tr_id(_) ->
	9999.
	
parse_fairsets([]) ->
	[];
parse_fairsets([Fs | Fss]) ->
	Ns = lists:map(fun tr_id/1,
				   string:tokens(
					 string:strip(string:strip(Fs,left,${),right,$}),
					 ",")),
	[Ns | parse_fairsets(Fss)].

mk_buchi([],_Arcs,_FairS) ->
	{[],[],[],[]};
mk_buchi(States,Arcs,FairS) ->
%%  	io:format("States: ~p\nArcs: ~p\nFairsets: ~p\n",[States,Arcs,FairS]),
	TrTbl = lists:map(fun({{N,_},M}) -> {N,M} end, 
					  lists:zip(States,lists:seq(1,length(States)))),
	MStates = [	{proplists:get_value(N,TrTbl),Lbls} || {N,Lbls} <- States ],
	InitStates = [ proplists:get_value(N,TrTbl) || {N,_,true} <- Arcs ],
	Trans = [{proplists:get_value(S1,TrTbl),proplists:get_value(S2,TrTbl)}
			 || {S1,Sn,_} <- Arcs, S2 <- Sn],
	Accepts = case FairS of
				  [] -> [lists:seq(1,length(States))];
				  _ -> [[proplists:get_value(N,TrTbl) || N <- Fs]
						|| Fs <- FairS]
			  end,
	{MStates,InitStates,Trans,Accepts}.
	
							   
