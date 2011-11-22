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

%% @doc Parse LTL strings. A reference implementation of a parser for
%% LTL expressions. Only implements the ltl_parse:string function that
%% parses a string. Uses yecc for the actual parsning, see
%% ltl_parser.yrl. Can be extended and/or replaced to suit special needs
%% for an example of this, see <A
%% HREF="https://babel.ls.fi.upm.es/trac/McErlang/">McErlang</A>
%%
%% ===Examples===
%% <pre>
%% 8&#62; ltl_parse:string("G p").
%% {always,{lprop,p}}
%%
%% 9&#62; ltl_parse:string("[] p").
%% {always,{lprop,p}}
%%
%% 10&#62; ltl_parse:string("F (p | q)").
%% {eventually,{lor,{lprop,p},{lprop,q}}}
%%
%% 11&#62; ltl_parse:string("&#60;&#62; (p | q)").
%% {eventually,{lor,{lprop,p},{lprop,q}}}
%%
%% 12&#62; ltl_parse:string("&#60;&#62; (p | X q)").
%% {eventually,{lor,{lprop,p},{next,{lprop,q}}}}
%% </pre>
%% @end

-module(ltl_parse).
-export([string/1]).

-include_lib("eunit/include/eunit.hrl").

%% @doc Parse a string containing an LTL expression.
%% @spec (string()) -> ltl_formula()
string(S) ->
    {ok, Toks, _} = erl_scan:string(S),
    %%   io:format("Tokens: ~p\n",[Ts]),
    LtlToks = ltl_scan(Toks),
    %% io:format("LtlTokens: ~p\n", [LtlToks]),
    {ok, Res} = ltl_parser:parse(LtlToks),
    Res.

ltl_scan([]) -> [];
ltl_scan([First| Rest]) ->
    case First of
		A = {atom, _N, _V} ->
			[A| ltl_scan(Rest)];
		{var, N, 'G'} ->
			[{always, N}| ltl_scan(Rest)];
		{var, N, 'F'} ->
			[{eventually, N}| ltl_scan(Rest)];
		{var, N, 'X'} ->
			[{next, N}| ltl_scan(Rest)];
		V = {var, _, _} ->
			[V| ltl_scan(Rest)];
		I = {integer, _, _} ->
			[I| ltl_scan(Rest)];
		{Sym, N} ->
			case lists:member(Sym, ltl_symbols()) of
				true ->
					[{map_ltl_symbol(Sym), N}| ltl_scan(Rest)];
				false ->
					case Sym of
						'[' ->
							Rest2 = ensure_rest(']', Rest),
							[{always, N}| ltl_scan(Rest2)];
						'<' ->
							Rest2 = ensure_rest('>', Rest),
							[{eventually, N}| ltl_scan(Rest2)];
						'<-' ->
							Rest2 = ensure_rest('>', Rest),
							[{equivalent, N}| ltl_scan(Rest2)];
						'<=' ->
							Rest2 = ensure_rest('>', Rest),
							[{equivalent, N}| ltl_scan(Rest2)];
						'&' ->
							case Rest of
								[{'&', _}| Rest2] ->
									[{'and', N}| ltl_scan(Rest2)];
								_ ->
									[{'and', N}| ltl_scan(Rest)]
							end;
						'=' ->
							case Rest of
								[{'>', _}| Rest2] ->
									[{implies, N}| ltl_scan(Rest2)];
								_ ->
									[{'=', N}| ltl_scan(Rest)]
							end;
						_ ->
							[Sym| ltl_scan(Rest)]
					end
			end;
		_ ->
			io:format("*** Error: token ~p not recognised~n", [First]),
			throw(bad_scan)
    end.

ensure_rest(Symbol,Tokens) ->
	case Tokens of
		[{Symbol,_}|RestTokens] ->
			RestTokens;
		_ ->
			io:format("*** Error: token ~p not found in ~p~n",[Symbol,Tokens]),
			throw(bad_scan)
	end.

ltl_symbols() ->
    ['+', '*', '!', '(', ')', '|', '||', 
	 '==', '->', '-', 'and', 'or', 'not'].

map_ltl_symbol('|') -> 'or';
map_ltl_symbol('||') -> 'or';
map_ltl_symbol('->') -> 'implies';
map_ltl_symbol('==') -> 'equivalent';
map_ltl_symbol('+') -> 'or';
map_ltl_symbol('*') -> 'and';
map_ltl_symbol('!') -> 'not';
map_ltl_symbol('-') -> 'not';
map_ltl_symbol(Symbol) -> Symbol.


%% Eunit tests

parse_simple_test_() ->
	[ ?_assert( string("p") =:= ltl:prop(p) ),
	  ?_assert( string("p=1") =:= ltl:prop(p) ),
	  ?_assert( string("! p") =:= ltl:lnot(ltl:prop(p))),
	  ?_assert( string("-p") =:= ltl:lnot(ltl:prop(p))),
	  ?_assert( string("G p") =:= ltl:always(ltl:prop(p))),
	  ?_assert( string("[] p") =:= ltl:always(ltl:prop(p))),
	  ?_assert( string("F p") =:= ltl:eventually(ltl:prop(p))),
	  ?_assert( string("<> p") =:= ltl:eventually(ltl:prop(p))),
	  ?_assert( string("X p") =:= ltl:next(ltl:prop(p))),

	  ?_assert( string("a & b") =:= ltl:land(ltl:prop(a),ltl:prop(b))),
	  ?_assert( string("a && b") =:= ltl:land(ltl:prop(a),ltl:prop(b))),

	  ?_assert( string("a | b") =:= ltl:lor(ltl:prop(a),ltl:prop(b))),
	  ?_assert( string("a || b") =:= ltl:lor(ltl:prop(a),ltl:prop(b))),

	  ?_assert( string("a -> b") =:= ltl:implication(ltl:prop(a),ltl:prop(b))),
	  ?_assert( string("a => b") =:= ltl:implication(ltl:prop(a),ltl:prop(b))),

	  ?_assert( string("a == b") =:= ltl:equivalent(ltl:prop(a),ltl:prop(b))),
	  ?_assert( string("a <-> b") =:= ltl:equivalent(ltl:prop(a),ltl:prop(b))),
	  ?_assert( string("a <=> b") =:= ltl:equivalent(ltl:prop(a),ltl:prop(b)))
	 ]. 



