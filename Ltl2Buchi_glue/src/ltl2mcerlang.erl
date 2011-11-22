%% Copyright (c) 2009, Hans Svensson, Lars-Ake Fredlund
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

-module(ltl2mcerlang).

%%-define(debug,true).
-ifdef(debug).
-define(LOG(X,Y), io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-define(DEBUGVAL(),true).
-else.
-define(LOG(X,Y), ok).
-define(DEBUGVAL(),false).
-endif.

-export([convert/2,to_dot/2,buchi_to_mcerlang_digraph/1]).

convert(Formula,FileName) ->
  NegatedFormula = ltl:lnot(Formula),
  ?LOG("Negated formula for translation is ~p~n",[ltl:pp(NegatedFormula)]),
  Automaton = ltl2buchi:translate(NegatedFormula),
  G = buchi_to_mcerlang_digraph(Automaton),
  buchi2mcerlang:to_mcerlang_monitor(ltl:pp(NegatedFormula),G,FileName).


%%%
%% Convert tuple-based Buchi automata to digraph 
%% variant defined in buchi.erl
%%% 
buchi_to_mcerlang_digraph({States,InitStates,Trans,Accept}) ->
  %%io:format
  %%("States=~n~p~nInitStates=~n~p~nTrans=~n~p~nAccept=~n~p~n",
  %%[States,InitStates,Trans,Accept]),
  G = buchi2mcerlang:new(),
  lists:foreach(
    fun(S) ->
	buchi2mcerlang:add_state(S-1,
				 [initial   || lists:member(S,InitStates)] ++
				 [accepting || lists:member(S,Accept)],
				 G)
    end,States),
  lists:foreach(
    fun({S1,S2,Lbl}) ->
	buchi2mcerlang:add_transition(S1-1,
				      convert_label(Lbl),
				      S2-1,
				      G)
    end,Trans),
  G.

convert_label([]) ->
  buchi2mcerlang:boolean_condition(true);
convert_label([X]) ->
  convert_literal(X);
convert_label([X | Xs]) ->
  buchi2mcerlang:and_condition(convert_literal(X),convert_label(Xs)).

convert_literal({lnot,P}) ->
  buchi2mcerlang:neg_condition(convert_literal(P));
convert_literal(ltrue) ->
  buchi2mcerlang:pred_condition(true);
convert_literal(lfalse) ->
  buchi2mcerlang:pred_condition(false);
convert_literal({lprop,X}) ->
  buchi2mcerlang:pred_condition(X).

to_dot(Formula,FileName) ->
  %%   Automaton = convert(Formula),
  Automaton = ltl2buchi:translate(ltl:lnot(Formula)),
  file:write_file
    (FileName,
     mce_dot:from_buchi_automaton
     (ltl2mcerlang:buchi_to_mcerlang_digraph(Automaton),
      fun dot_pp_state/1,fun dot_pp_actions/1)).

dot_pp_actions({transition,{_,Cond,_}}) -> 
  "label=\""++pp_cond(Cond)++"\"".

pp_cond({bool,true}) ->
  "true";
pp_cond({bool,false}) ->
  "false";
pp_cond({pred,A}) ->
  pp_pred(A);
pp_cond({cnot,Cnd}) ->
  "!"++pp_cond(Cnd);
pp_cond({cand,{Cnd1,Cnd2}}) ->
  pp_cond(Cnd1)++"&"++pp_cond(Cnd2).

pp_pred({var,A}) ->
  atom_to_list(A);
pp_pred(Other) ->
  io_lib:format("~p",[Other]).

dot_pp_state({_,{_,{state,{Nr,Attributes}}}}) ->
  StateLabel =
    "label=\""++integer_to_list(Nr)++"\"",
  StateColor =
    case lists:member(accepting,Attributes) of
      true ->
	"style=filled,fillcolor=grey";
      false ->
	""
    end,
  StateShape =
    case lists:member(initial,Attributes) of
      true ->
	"shape=box";
      false ->
	""
    end,
  combine_attributes([StateLabel,StateColor,StateShape]).

combine_attributes([]) ->
  "";
combine_attributes([Attr]) ->
  Attr;
combine_attributes([""|Rest]) ->
  combine_attributes(Rest);
combine_attributes([Attr,""|Rest]) ->
  combine_attributes([Attr|Rest]);
combine_attributes([Attr1,Attr2|Rest]) ->
  Attr1++","++combine_attributes([Attr2|Rest]).

