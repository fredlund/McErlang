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

-module(mce_erl_pretty).

-include("process.hrl").
-include("state.hrl").
-include("system.hrl").
-include("executable.hrl").
-include("node.hrl").
-include("compile_rec.hrl").
-include("../../../src/include/mce_opts.hrl").
-include("../../../src/include/monState.hrl").
-include("../../../src/include/monitor.hrl").
-include("../../../src/include/stackEntry.hrl").

-export([pretty/1,pretty/2,record_helper/3]).

record_helper(Tag,NoFields,R) ->
  case lookup_tag(Tag) of
    {ok,DefaultRecord,Tags} ->
      case length(Tags)=:=NoFields of
	true ->
	  {NewTags,NewFields} =
	    lists:unzip
	      (check_defaults
	       (tl(tuple_to_list(DefaultRecord)),Tags,tl(tuple_to_list(R)))),
	  {ok,NewTags,list_to_tuple([Tag|NewFields])};
	false ->
	  no
      end;
    Other -> Other
  end.

lookup_tag(Tag) ->
  case Tag of
    process ->
      {ok,#process{},record_info(fields,process)};
    state ->
      {ok,#state{},record_info(fields,state)};
    executable ->
      {ok,#executable{},record_info(fields,executable)};
    processFlags ->
      {ok,#processFlags{},record_info(fields,processFlags)};
    system ->
      {ok,#system{},record_info(fields,system)};
    monState ->
      {ok,#monState{},record_info(fields,monState)};
    monitor ->
      {ok,#monitor{},record_info(fields,monitor)};
    node ->
      {ok,#node{},record_info(fields,node)};
    stackEntry ->
      {ok,#stackEntry{},record_info(fields,stackEntry)};
    mce_opts ->
      {ok,#mce_opts{},record_info(fields,mce_opts)};
    compile_rec ->
      {ok,#compile_rec{},record_info(fields,compile_rec)};
    _ ->
      no
  end.

check_defaults([],[],[]) -> [];
check_defaults([Default|RestDefaults],[Tag|RestTags],[Field|RestFields]) ->
  if Default=/=Field ->
      [{Tag,Field}|check_defaults(RestDefaults,RestTags,RestFields)];
     true ->
      check_defaults(RestDefaults,RestTags,RestFields)
  end.

pretty(Term) ->
    mce_erl_io_lib_pretty:print(Term, 1, 80, -1, fun record_helper/3).

pretty(Term, Depth) ->
    mce_erl_io_lib_pretty:print(Term, 1, 80, Depth, fun record_helper/3).
