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

%%% File    : java_wrap.erl
%%% Author  : Hans Svensson <>
%%% Description : Wrapper for java implementation of LTL2Buchi
%%% Created :  4 Mar 2009 by Hans Svensson <>

-module(ltl2buchi_wrap).
-compile(export_all).
-include_lib("xmerl/include/xmerl.hrl").

run(Ltl) ->
	StrLtl = ltl:pp(Ltl,java),
	Cmd = "../test/ref_impl/scripts/convertLTL.sh \"" ++ StrLtl ++ "\"",
%%	io:format("Cmd:~s\n",[Cmd]),
	Res = os:cmd(Cmd),
%%    	io:format("Res: \n~s\n",[Res]),
%% 	Acc = fun(#xmlText{value = " ", pos = P}, Acc, S) ->
%% 				  {Acc, P, S};  % new return format
%% 			 (X, Acc, S) ->
%% 				  {[X|Acc], S}
%% 		  end,
	{Doc,[]} = xmerl_scan:string(Res,[{space,normalize}]),
%% 	io:format("After xmer_scan:string~n",[]),
%%  	io:format("Document is ~n~p~n~n",[simpleXML(Doc)]),
%% 	printElement(0,Document),
%% 	Automaton = convert_graph(Document),
%% 	io:format("Automaton:~n  ~p~n",[Automaton]),
%% 	Automaton.
	mk_buchi(simpleXML(Doc)).

simpleXML(#xmlElement{name = guard, attributes = _Attrs, content = Content}) ->
	Guard = lists:concat([ V || #xmlText{value = V} <- Content]),
	{guard,[],parse_guard(Guard)};
simpleXML(#xmlElement{name = Name, attributes = Attrs, content = Content}) ->
	{Name,
	 [{N,V} || #xmlAttribute{name = N, value = V} <- Attrs],
	 lists:sort([simpleXML(El) || El <- Content, element(1,El) == xmlElement])}.
	
not_is_bop(X) ->
	X /= $| andalso X /= $&.

parse_guard([$! | Str]) ->
	Var = lists:takewhile(fun not_is_bop/1,Str),
	Rest = lists:dropwhile(fun not_is_bop/1,Str),
	case Rest of
		[] -> [{lnot,{lprop,list_to_atom(Var)}}];
		[$& | Guard] ->
			[{lnot,{lprop,list_to_atom(Var)}} | parse_guard(Guard)]
	end;
parse_guard(Str) ->
	Var = lists:takewhile(fun not_is_bop/1,Str),
	Rest = lists:dropwhile(fun not_is_bop/1,Str),
	case Rest of
		[] -> [{lprop,list_to_atom(Var)}];
		[$& | Guard] ->
			[{lprop,list_to_atom(Var)} | parse_guard(Guard)]
	end.
	
mk_buchi({graph,_,Nodes}) ->
	mk_buchi(Nodes,{[],[],[],[]}).
mk_buchi([],{Nodes,Inits,Trans,Accept}) ->
	{Nodes,Inits,Trans,Accept};
mk_buchi([{node,[{id,Id}],Content} | XMLNodes],
		 {Nodes,Inits,Trans,Accept}) ->
	N = list_to_integer(Id) + 1,
	NewInits = Inits ++ [ N || lists:member({init,[],[]},Content)],
	NewAccept = Accept ++ [ N || lists:member({accepting,[],[]},Content)],
	NewTrans = Trans ++
		[{N,list_to_integer(Id2) + 1,Lbl} 
		 || {transition,[{to,Id2}],[{guard,_,Lbl}]} <- Content]
		++
		[{N,list_to_integer(Id2) + 1,[]} 
		 || {transition,[{to,Id2}],[]} <- Content],
	mk_buchi(XMLNodes,{Nodes ++ [N],NewInits,NewTrans,NewAccept}).
