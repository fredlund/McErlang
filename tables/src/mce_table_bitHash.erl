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

-module(mce_table_bitHash).

-export([init/1,permit_state/2,permit_state_alt/2,add_state/2,add_state_alt/2,add_trans/4,info/1,states_to_list/1,transitions_to_list/1,set_initial_state/2,get_initial_state/1]).
-export([delete_table/1]).
-export([states/1,transitions/1]).

init([TableSize]) ->
    {ok, {TableSize * 8, mce_bitarray:create(TableSize)}}.

info({Size,_}) ->
    Size.

delete_table(_) -> ok.

permit_state(State, {Size, T}) ->
    HashValue = erlang:phash2(State, Size),
    mce_bitarray:is_off(T, HashValue).

permit_state_alt(State, {Size, T}) ->
    HashValue = erlang:phash2(State, Size),
    {mce_bitarray:is_off(T, HashValue), HashValue}.

add_trans(_PrevState, _State, _Actions, T) -> T.

add_state(State, {Size, T}) ->
    HashValue = erlang:phash2(State, Size),
    {Size, mce_bitarray:on(T, HashValue)}.

add_state_alt(HashValue, {Size, T}) ->
    {Size, mce_bitarray:on(T, HashValue)}.

states_to_list(_) -> void.

transitions_to_list(_) -> void.

set_initial_state(_State,T) -> T.

get_initial_state(_) -> void.

states({Size,T}) ->
	states(0,Size,T).

states(Size,Size,_) -> 0;
states(N,Size,T) ->
  case mce_bitarray:is_on(T,N) of
	  true  -> 1 + states(N+1,Size,T);
	  false -> states(N+1,Size,T)
  end.
transitions(_) ->
  void.

	  

