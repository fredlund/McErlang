%% Copyright (c) 2009, Lars-Ake Fredlund
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

%% @author Lars-Ake Fredlund
%% @copyright 2006-2009 Lars-Ake Fredlund
%% @doc
%% @private

-module(mce_dot).
-language(erlang).

-export([from_table/1,from_table/3,
	 from_stack/1,from_stack/3,
	 from_buchi_automaton/1,from_buchi_automaton/3]).

-include("stackEntry.hrl").

pp_action() -> void.
pp_state() -> void.
id_fun(T) -> T.

from_table(Table) ->
  from_table(Table, pp_state(), pp_action()).
from_table(Table, StatePrinter, ActionPrinter) ->
  {ok, States} = mce_behav_tableOps:states_to_list(Table),
  {ok, Transitions} = mce_behav_tableOps:transitions_to_list(Table),
  {ok, {StateDict,TransitionDict}} =
    lists_to_dot_dicts(States,fun id_fun/1,Transitions,fun id_fun/1),
  build_dot_digraph(StateDict,TransitionDict,StatePrinter,ActionPrinter).

from_stack(Stack) ->
  from_stack(Stack, pp_state(), pp_action()).
from_stack(Stack, StatePrinter, ActionPrinter) ->
  States = lists:reverse(get_stack_states(Stack)),
  Transitions = get_stack_transitions(Stack),
  {ok, {StateDict,TransitionDict}} =
    lists_to_dot_dicts
      (States,
       fun (S) -> S#stackEntry.abs_system end,
       Transitions,
       fun id_fun/1),
  build_dot_digraph(StateDict,TransitionDict,StatePrinter,ActionPrinter).
  
from_buchi_automaton(Buchi_Automaton) ->
  from_buchi_automaton(Buchi_Automaton, pp_state(), pp_action()).
from_buchi_automaton(G, StatePrinter, ActionPrinter) ->
  Vertices =
    allVertices(G),
  Edges =
    allEdges(G),
  {ok, {StateDict,TransitionDict}} =
    lists_to_dot_dicts(Vertices,fun id_fun/1,Edges,fun id_fun/1),
  build_dot_digraph(StateDict,TransitionDict,StatePrinter,ActionPrinter).

get_stack_transitions(Stack) ->
  case get_stack_state(Stack) of
    {ok, {Entry,Rest}} ->
      get_stack_transitions(Entry,Rest);
    _ ->
      []
  end.
get_stack_transitions(TargetState,Stack) ->
  case get_stack_state(Stack) of
    {ok, {Entry,Rest}} ->
      [{Entry,TargetState,TargetState}|
       get_stack_transitions(Entry,Rest)];
    _ -> []
  end.

get_stack_states(Stack) ->
  case get_stack_state(Stack) of
    {ok, {Entry,Rest}} ->
      [Entry|get_stack_states(Rest)];
    _ -> []
  end.

get_stack_state(Stack) ->
    case mce_behav_stackOps:is_empty(Stack) of
      true ->
	  false;
      false ->
	  {Entry, Rest} = mce_behav_stackOps:pop(Stack),
	  case Entry#stackEntry.transitions of
	    void ->
		{ok, {Entry, Rest}};
	    _ ->
		get_stack_state(Rest)
	  end
    end.

allVertices(G) ->
  lists:map
    (fun (V) -> digraph:vertex(G,V) end,
     digraph:vertices(G)).

allEdges(G) ->
  lists:map
    (fun (E) ->
	 {E,FromVertex,ToVertex,L} = digraph:edge(G,E),
	 {digraph:vertex(G,FromVertex),L,digraph:vertex(G,ToVertex)}
     end,
     digraph:edges(G)).

lists_to_dot_dicts(StateList, State_KeyFun, 
		   TransitionList, Transition_KeyFun) ->
  {_,StateDict} =
    lists:foldl
      (fun (State,{SCounter,SDict}) ->
	   StateKey = apply(State_KeyFun,[State]),
	   case dict:find(StateKey,SDict) of
	     {ok,_} -> {SCounter,SDict};
	     _ -> {SCounter+1,dict:store(StateKey,{SCounter,State},SDict)}
	   end
       end, {0,dict:new()}, StateList),
  {_,TransitionDict} =
    lists:foldl
      (fun ({SourceState,Actions,TargetState},{ACounter,ADict}) ->
	   {ok, {SourceStateNum,_}} =
	     dict:find(apply(State_KeyFun,[SourceState]),StateDict),
	   {ok, {TargetStateNum,_}} =
	     dict:find(apply(State_KeyFun,[TargetState]),StateDict),
	   AKey = apply(Transition_KeyFun,
			[{SourceStateNum,Actions,TargetStateNum}]),
	   case dict:find(AKey,ADict) of
	     {ok,_} -> {ACounter,ADict};
	     _ -> {ACounter+1,dict:store(AKey,{ACounter,AKey},ADict)}
	   end
       end, {0,dict:new()}, TransitionList),
  {ok,{StateDict,TransitionDict}}.

build_dot_digraph(StateDict,TransitionDict,StatePrinter,ActionPrinter) ->
  ["digraph G {\n",
   dot_build(StateDict,TransitionDict,StatePrinter,ActionPrinter),
   "}\n"].

dot_build(VDict,EDict,StatePrinter,ActionPrinter) ->
%  io:format("Vertices...~n",[]),
  Vertices =
    lists:map(fun({_,{Nr,State}}) ->
           integer_to_list(Nr)++
	   attributes(StatePrinter,{Nr,State})++
           ";\n"
%%           " [" ++
%%           "    shape=box,\n    label=\""++
%%           dot_quote(State)++
%%           "\"];\n"
        end,dict:to_list(VDict)),
%  io:format("Edges...~n",[]),
  Edges = 
    lists:map(fun({_,{_,{From,Action,To}}}) ->
           integer_to_list(From)++
           " -> "++
           integer_to_list(To)++
	   attributes(ActionPrinter,Action)++
	   ";\n"
        end,dict:to_list(EDict)),
  Vertices++Edges.

attributes(void,_Object) ->
  "";
attributes(Printer,Object) ->
  case apply(Printer,[Object]) of
    "" -> "";
    Attributes -> dot_quote(" ["++Attributes++"]")
  end.

dot_quote(String) -> String.
%%%  lists:foldr(fun(C,S) ->
%%%           case C of
%%%                10 ->
%%%                  "\\n"++S;
%%%                13 ->
%%%                  "\\n"++S;
%%%                $< ->
%%%                  "&lt; "++S;
%%%                $> ->
%%%                  "&gt; "++S;
%%%                $& ->
%%%                  "&amp; "++S;
%%%                _ ->
%%%                  [C|S]
%%%           end
%%%        end, "", String).



