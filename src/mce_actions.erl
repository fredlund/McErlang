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
%% @doc Provides mechanisms for interaction with 'action records'.

%% @type action(). A record representing an action performable by certain process (process id, name, arguments...). Specified in file action.hrl.
-module(mce_actions).

-include("actions.hrl").

-export([is_action/1]).
-export([mk/1,mk/2,mk/3,mk/4,mk/5]).
-export([get_name/1,get_source/1,get_argument/1]).
-export([get_label/1,get_time/1]).
-export([mk_magic_run/0,mk_stutter/0]).
-export([set_time/2]).

%% @doc Checks if the parameter is an action record.
%% @end
%% @spec (any())->bool()
is_action(Action) ->
  is_record(Action,action).

%% @private
mk(Action) when is_record(Action,action) ->
  Action;
mk(Name) ->
  #action{name=Name}.

%% @private
mk(Source,Name) -> 
  #action{name=Name,source=Source}.

%% @private
mk(Source,Name,Argument) -> 
  #action{name=Name,source=Source,argument=Argument}.

%% @private
mk(Source,Name,Label,Argument) -> 
  #action{name=Name,source=Source,label=Label,argument=Argument}.

%% @private
mk(Time,Source,Name,Label,Argument) -> 
  #action{time=Time,name=Name,source=Source,label=Label,argument=Argument}.

%% @doc Gets the 'name' component of an action record.
%% @end 
%% @spec (action())-> term()
get_name(#action{name=N}) ->
  N.

%% @doc Gets the 'source' component of an action record.
%% @end 
%% @spec (action())-> pid()
get_source(#action{source=S}) ->
  S.

%% @doc Gets the 'arguments' component of an action record.
%% @end 
%% @spec (action())-> any()
get_argument(#action{argument=A}) ->
  A.

%% @doc Gets the 'label' component of an action record.
%% @end 
%% @spec (action())-> term()
get_label(#action{label=L}) ->
  L.

%% @doc Gets the 'time' component of an action record.
%% @end 
%% @spec (action())-> time()
get_time(#action{time=T}) ->
  T.

%% @private
set_time(T,Action) when is_record(Action,action) ->
  Action#action{time=T}.

%% @private
mk_magic_run() ->
  mk(magic_run).

%% @private
mk_stutter() ->
  mk(stutter).


