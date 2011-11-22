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
%% @end
% % @doc
%% @private 
%% @type monState(). A record representing the state of the system (dictionary,nodes,links) and the current monitor used. Specified in file monState.hrl.
%% @type action(). A record representing an action performable by certain process (process id, name, arguments...). Specified in file action.hrl.
% % @type abstraction(). A record representing an abstraction technique (module, contents). Declared in file abstraction.hrl.

-module(mce_abs_norm).
-export([init/1, abstract_actions/2,abstract_state/2]).
-behaviour(mce_behav_abstraction).

-include("monState.hrl").
-include("../../languages/erlang/src/include/state.hrl").
-include("../../languages/erlang/src/include/node.hrl").


init(_) ->
    {ok,ok}.

%% Returns a tuple containing the parameters. Thus, no abstraction is performed.
% %-spec(abstract_actions/2::([action()],abstraction())-> ({[action()],abstraction()})).	     
%% @doc Does not perform any action and returns a tuple containing the parameters unchanged.
%% @end
%% @spec ([action()], any())->{[action()], any()}
abstract_actions(A,AS) ->
    {A,AS}.


%% Returns a monState record whose state component is normalized.
% %-spec(abstract_state/2::(monState(),abstraction())-> ({monState(),abstraction()})).	     
%% @doc Performs an abstraction that consists in ordering alphabetically the 
%% processes within each of the nodes of the state.
%% @end
%% @spec (monState(), any())->{monState(), any()}
abstract_state(State,AS) -> 
  case catch {normalizeState(State),AS} of
    {'EXIT',Reason} ->
      io:format
	("abstract_state returns error ~p~nfor state ~p~n",
	 [Reason,State]),
      exit(abstract_state);
    Other ->
      Other
  end.



normalizeState(State) ->
  StateComp = State#monState.state,
  State#monState
    {state=
     StateComp#state
     {nodes=
      lists:sort
      (lists:map
       (fun (Node) -> Node#node{processes=lists:sort(Node#node.processes)} end,
	StateComp#state.nodes))}}.
