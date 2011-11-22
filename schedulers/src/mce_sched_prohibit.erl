%% Copyright (c) 2100, Lars-Ake Fredlund
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
%% @copyright 2006-2010 Lars-Ake Fredlund
%% @doc
%% @private

-module(mce_sched_prohibit).
-language(erlang).

-behaviour(mce_behav_scheduler).

-export([init/1,choose/4,willCommit/1]).

-record(flags,{actionFilter=void,random=false}).

init(Arguments) -> 
  Random =
    case proplists:get_value(random,Arguments) of
      true -> true;
      false -> false;
      _ -> false
    end,
  ActionFilter =
    case proplists:get_value(actionFilter,Arguments) of
      MF={_M,_F} -> MF;
      F when is_function(F) -> F;
      _ ->
	io:format
	  ("Bad specification of actionFilter in argument ~p to function "++
	     "~p:init ~p~n",
	   [Arguments,?MODULE]),
	throw(bad)
    end,
  {ok,#flags{random=Random,actionFilter=ActionFilter}}.

willCommit(_) -> true.
  
choose(Transitions,Flags,Monitor,Conf) ->
  FilteredTransitions =
    lists:filter
      (reqFilter(Flags),
       lists:map
       (fun (T) -> mce_erl_opsem:commit(T,Monitor,Conf) end, Transitions)),

  case length(FilteredTransitions) of
    N when N>0 ->
      SelectedNumber =
	case Flags#flags.random of
	  true -> mce_random:uniform(N);
	  false -> 1
	end,
      {ok,{lists:nth(SelectedNumber,FilteredTransitions),Flags}};
    0 -> no_transitions
  end.

reqFilter(Flags) ->
  ActForbid = Flags#flags.actionFilter,
  fun ({Actions,_}) -> 
      not(lists:any(ActForbid, Actions))
  end.


      
  

