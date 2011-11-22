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

%%%-------------------------------------------------------------------
%%% File    : ltl.erl
%%% Author  : Hans Svensson <>
%%% Description : LTL expressions and utility functions
%%%
%%% Created : 26 March 2009 by Hans Svensson <>
%%%-------------------------------------------------------------------

%% @author Hans Svensson <hanssv@chalmers.se>
%% @copyright 2009, Hans Svensson
%% @doc Module defining LTL expressions, and a pretty printer.
%% @reference See <a href="http://en.wikipedia.org/wiki/Linear_temporal_logic">
%% Linear Temporal Logic</a> for more information on LTL.
%% @end
%% @type ltl_formula(). An LTL formula
%% @type ltl_print_format() = normal | java | wring. Pretty print
%% format, for normal printing, use with Wring and use with Java 
%% implementation of LTL2Buchi.
%% @end

-module(ltl).

-export([prop/1, next/1, always/1, eventually/1,
		 until/2, release/2, lnot/1, land/2, lor/2,
		 implication/2, equivalent/2, ltrue/0, lfalse/0]).

-export([land/1,lor/1]).

-export([negate/1, pnf/1, pp/1, pp/2, print_ltl/1]).

%% @doc Form an LTL proposition.
%% @spec (atom()) -> ltl_formula()
prop(X) ->
    {lprop, X}.

%% @doc Next LTL expression.
%% next(P) = <b>X</b>
%% @spec (ltl_formula()) -> ltl_formula()
next(Phi) ->
	{next,Phi}.

%% @doc Always LTL expression.
%% always(P) = [] P
%% @spec (ltl_formula()) -> ltl_formula()
always(Phi) ->
	{always,Phi}.

%% @doc Eventually LTL expression.
%% eventually(P) = &lt;&gt; P
%% @spec (ltl_formula()) -> ltl_formula()
eventually(Phi) ->
	{eventually,Phi}.

%% @doc Until LTL expression.
%% until(P,Q) = P <b>U</b> Q
%% @spec (ltl_formula(),ltl_formula()) -> ltl_formula()
until(Psi,Phi) ->
	{until,Psi,Phi}.

%% @doc Release LTL expression.
%% release(P,Q) = P <b>R</b> Q
%% @spec (ltl_formula(),ltl_formula()) -> ltl_formula()
release(Psi,Phi) ->
	{release,Psi,Phi}.

%% @doc Negate LTL expression.
%% lnot(P) = ! P
%% @spec (ltl_formula()) -> ltl_formula()
lnot(Phi) ->
    {lnot, Phi}.

%% @doc And LTL expression.
%% land(a,b) = a &amp;&amp; b
%% @spec (ltl_formula(),ltl_formula()) -> ltl_formula()
land(Phi1, Phi2) ->
    {land, Phi1, Phi2}.

%% @doc And LTL expression.
%% [a,b,c] = a &amp;&amp; (b &amp;&amp; c), implemented in terms of 
%% {@link land/2. land/2}.
%% @spec ([ltl_formula()]) -> ltl_formula()
land([]) -> ltl_true;
land([Phi]) -> Phi;
land([Phi| Phis]) -> {land, Phi, land(Phis)}.

%% @doc Or LTL expression.
%% lor(a,b) = a || b
%% @spec (ltl_formula(),ltl_formula()) -> ltl_formula()
lor(Phi1, Phi2) ->
    {lor, Phi1, Phi2}.

%% @doc Or LTL expression.
%% [a,b,c] = a || (b || c), implemented in terms of 
%% {@link lor/2. lor/2}.
%% @spec ([ltl_formula()]) -> ltl_formula()
lor([]) -> lfalse;
lor([Phi]) -> Phi;
lor([Phi| Phis]) -> {lor, Phi, lor(Phis)}.

%% @doc Implication LTL expression.
%% F1 --&gt; F2 = !F1 || F2
%% @spec (ltl_formula(),ltl_formula()) -> ltl_formula()
implication(Phi1, Phi2) ->
    {lor, {lnot, Phi1}, Phi2}.

%% @doc Equivalence LTL expression.
%% F1 &lt;--&gt; F2 = (F1 --&gt; F2) &amp;&amp; (F2 --&gt; F1)
%% @spec (ltl_formula(),ltl_formula()) -> ltl_formula()
equivalent(Phi1, Phi2) ->
    land(implication(Phi1, Phi2), implication(Phi2, Phi1)).

%% @doc LTL constant - True.
%% @spec () -> ltl_formula()
ltrue() -> 
	ltrue.

%% @doc LTL constant - False.
%% @spec () -> ltl_formula()
lfalse() -> 
	lfalse.

%% @doc Negate an ltl_formula.
%% Simplifies double negations as well as negate(false) = true, and negate(true) = false.
%% @spec (ltl_formula()) -> ltl_formula()
negate(lfalse) -> ltrue;
negate(ltrue) -> lfalse;
negate({lnot, X}) -> X;
negate(Phi) -> lnot(Phi).

%% @doc Positive normal form.
%% Transform expression into positive normal form 
%% (i.e. pushing the negations inwards) also known as <em>Negation normal form</em>
%% <br/>See <a href="http://en.wikipedia.org/wiki/Negation_normal_form">Negation
%% normal form</a> for a formal definition.
%% @spec (ltl_formula()) -> ltl_formula()
pnf({lnot,{always,Phi}}) ->         {eventually,pnf({lnot,Phi})};
pnf({lnot,{eventually,Phi}}) ->     {always,pnf({lnot,Phi})};
pnf({lnot,{next,Phi}}) ->           {next,pnf({lnot,Phi})};
pnf({lnot,{lnot,Phi}}) ->           pnf(Phi);
pnf({lnot,{ltrue}}) ->              lfalse;
pnf({lnot,{lfalse}}) ->             ltrue;
pnf({lnot,{Op,Phi1,Phi2}}) ->       {dual_op(Op),pnf(lnot(Phi1)),pnf(lnot(Phi2))};
pnf({Op,Phi1,Phi2}) ->              {Op,pnf(Phi1),pnf(Phi2)};
pnf({Op,Phi}) ->                    {Op,pnf(Phi)};
pnf(Phi) ->                         Phi.

dual_op(land) -> lor;
dual_op(lor) -> land;
dual_op(release) -> until;
dual_op(until) -> release.


%% Print a set of ltl formulas, not beautiful ;-)
%% print_sets_ltl(Xs) when is_list(Xs) ->
%% 	lists:map(fun print_sets_ltl/1,Xs);
%% print_sets_ltl(X) ->
%% 	print_ltl(X).

%% print_covers(Xs) ->	
%% 	lists:map(fun print_cover1/1,Xs).

%% print_cover1({Phi, Xs}) ->
%%     [print_ltl(Phi) ++ " => "| lists:map(fun print_cover2/1, Xs)].
%% print_cover2({Vars,Nexts}) ->
%% 	 lists:map(fun print_ltl/1,Vars) ++ lists:map(fun print_ltl/1,Nexts).


%% @doc Pretty printing an LTL expression.
%% Generic printing of LTL expressions
%% @spec (ltl_formula()) -> string()
pp(F) ->
	pp(F,normal).

%% @doc Pretty printing an LTL expression.
%% The resulting string is formatted according to the specified 
%% format.
%% @spec (ltl_formula(),ltl_print_format()) -> ltl_formula()
pp({lprop, X}, S) ->
    pp_lprop(lists:flatten(io_lib:format("~p",[X])), S);
pp({lor, Phi1, Phi2}, S) ->
    format_expr1(Phi1, S, lor) ++ format_expr2(Phi2, S, lor);
pp({land, Phi1, Phi2}, S) ->
    format_expr1(Phi1, S, land) ++ format_expr2(Phi2, S, land);
pp({until, Psi, Phi}, S) ->
    format_expr1(Psi, S, until) ++ format_expr2(Phi, S, until);
pp({release, Psi, Phi}, S) ->
    format_expr1(Psi, S, release) ++ format_expr2(Phi, S, release);
pp({Op, Phi}, S) -> %% [],<>,!,X
    format_expr(Phi, S, Op);
pp(X, _S) when is_atom(X) ->
    atom_to_list(X).

format_expr(Phi, S, Op) ->
    case op_prio(Phi, S) >= op_prio(Op, S) of
      true -> ltl_sym(Op, S) ++ "(" ++ pp(Phi, S) ++ ")";
      false -> ltl_sym(Op, S) ++ pp(Phi, S)
    end.

format_expr1(Phi1, S, Op) ->
    case op_prio(Phi1, S) >= op_prio(Op, S) of
      true -> "(" ++ pp(Phi1, S) ++ ")" ++ ltl_sym(Op, S);
      false -> pp(Phi1, S) ++ ltl_sym(Op, S)
    end.

format_expr2(Phi2, S, Op) ->
    case op_prio(Phi2, S) >= op_prio(Op, S) of
      true -> "(" ++ pp(Phi2, S) ++ ")";
      false -> pp(Phi2, S)
    end.


%% @private
pp_lprop(X,wring) ->
	X ++ "=1";
pp_lprop(X,_) ->
	X.

%% @private
op_prio(X,wring) ->
	op_prio_wring(X);
op_prio(X,_) ->
	op_prio(X).

%% @private
op_prio(X) when is_tuple(X) ->
	op_prio(element(1,X));
op_prio(lor)     -> 10;
op_prio(land)    -> 10; 
op_prio(until)      -> 6;
op_prio(release)    -> 6;
op_prio(eventually) -> 5;
op_prio(always)     -> 5;
op_prio(next)       -> 4;
op_prio(lnot)    -> 3;
op_prio(lprop)        -> 1;
op_prio(_) -> 1.

%% @private
op_prio_wring(_) -> 1.

%% @private
ltl_sym(X,normal) -> normal_sym(X);
ltl_sym(X,java)   -> java_sym(X);
ltl_sym(X,wring)  -> wring_sym(X).

%% @private
normal_sym(lor)      -> " | ";
normal_sym(land)     -> " & ";
normal_sym(until)       -> " U ";
normal_sym(release)     -> " R ";
normal_sym(eventually)  -> "F ";
normal_sym(always)      -> "G ";
normal_sym(next)        -> "X ";
normal_sym(lnot)     -> "!".

%% @private
wring_sym(lor)      -> "+";
wring_sym(land)     -> "*";
wring_sym(until)       -> "U";
wring_sym(release)     -> "R";
wring_sym(eventually)  -> "F";
wring_sym(always)      -> "G";
wring_sym(next)        -> "X";
wring_sym(lnot)     -> "!".

%% @private
java_sym(lor)      -> " || ";
java_sym(land)     -> " && ";
java_sym(until)       -> " U ";
java_sym(release)     -> " V ";
java_sym(eventually)  -> "<> ";
java_sym(always)      -> "[] ";
java_sym(next)        -> "X ";
java_sym(lnot)     -> "!".

%% @spec (ltl_formula()) -> string()
%% @equiv pp(Phi)
print_ltl(Phi) -> pp(Phi,normal).

