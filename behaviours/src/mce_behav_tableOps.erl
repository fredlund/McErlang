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

-module(mce_behav_tableOps).

-include("table.hrl").

-export([init/2,permit_state/2,permit_state_alt/2,add_state/2,add_state_alt/2,add_trans/4,info/1,states_to_list/1,transitions_to_list/1,set_initial_state/2,get_initial_state/1]).
-export([delete_table/1]).
-export([states/1,transitions/1]).

init(TableModule,TableArgs) ->
  {ok,Table} = TableModule:init(TableArgs),
  {ok,#table{module=TableModule,contents=Table}}.

permit_state(State,Table) ->
  (Table#table.module):permit_state(State,Table#table.contents).

permit_state_alt(State,Table) ->
  (Table#table.module):permit_state_alt(State,Table#table.contents).

add_state(State,Table) ->
  Table#table
    {contents=((Table#table.module):add_state(State,Table#table.contents))}.

add_state_alt(State,Table) ->
  Table#table
    {contents=((Table#table.module):add_state_alt(State,Table#table.contents))}.

add_trans(PrevState,State,Actions,Table) ->
  Table#table
    {contents=((Table#table.module):
	       add_trans(PrevState,State,Actions,Table#table.contents))}.

info(Table) ->
  (Table#table.module):info(Table#table.contents).

states_to_list(Table) ->
  (Table#table.module):states_to_list(Table#table.contents).

transitions_to_list(Table) ->
  (Table#table.module):transitions_to_list(Table#table.contents).

set_initial_state(State,Table) ->
  Table#table
    {contents=
     (Table#table.module):set_initial_state(State,Table#table.contents)}.

get_initial_state(Table) ->
  (Table#table.module):get_initial_state(Table#table.contents).

delete_table(Table) ->
  (Table#table.module):delete_table(Table#table.contents).

states(Table) ->
  (Table#table.module):states(Table#table.contents).

transitions(Table) ->
  (Table#table.module):transitions(Table#table.contents).
