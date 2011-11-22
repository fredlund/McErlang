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
%% @doc Module with utility functions for LTL expressions
%% @see ltl
%% @type ltl_formula(). An LTL formula
%% @end

-module(ltl_utils).

-export([normalize/1, subformulas/1]).

%% @doc Normalize an LTL expression.
%% Normalize (in effect sort nested ands and ors). Used in order
%% to avoid a more costly equivalence check in some algorithms.
%% @spec (ltl_formula()) -> ltl_formula()
normalize({land, Phi1, Phi2}) ->
    ltl:land(lists:usort([normalize(Phi1), normalize(Phi2)]));
normalize({lor, Phi1, Phi2}) ->
    ltl:lor(lists:usort([normalize(Phi1), normalize(Phi2)]));
normalize({Op, Phi}) -> {Op, normalize(Phi)};
normalize({Op, Phi1, Phi2}) -> {Op, normalize(Phi1), normalize(Phi2)};
normalize(Phi) -> Phi.
	

%% @doc The subformulas of an ltl_formula.
%% Example: (<b>X</b> a) <b>U</b> b gives: [(<b>X</b> a) <b>U</b> b, <b>X</b> a, b, a]
%% @spec (ltl_formula()) -> [ltl_formula()]
subformulas(X = {lprop,_}) ->              [X];
subformulas(Phi0 = {_Op,Phi}) ->         [Phi0 | subformulas(Phi)];
subformulas(Phi0 = {_Op,Phi1,Phi2}) ->   [Phi0 | subformulas(Phi1) ++ subformulas(Phi2)];
subformulas(Phi) ->                      [Phi].
