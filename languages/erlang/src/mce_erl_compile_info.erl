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


%% This module stores information about function side effects
%% and function remappings in a table. This information is used
%% during compilation to determine whether to expect a side effect
%% when calling a function, and if a function call needs to be remapped.
%%

-module(mce_erl_compile_info).

-export([new_conf/0,
	 add_item/3,
	 get_module/2,
	 get_function/4,
	 func_mapping/4,
	 map_fun/3,
	 map_fun_arity/3]).
-export([all_module_remappings/1,
	 all_function_remappings/1]).

%%-define(debug,true).
-include("../../../src/include/macros.hrl").
-include("emacros.hrl").

-export([read_file/1,add_file/2]).

-record(info_rec,{name,options=[],inherits=[]}).

new_conf() ->
  dict:new().

get_object(Name,Conf) ->
  dict:find(Name,Conf).

add_item(Name,Options,Conf) ->
  OldObject = get_object(Name,Conf),
  OldInherits = get_inherits(OldObject),
  Conf1 = update_object(Name,OldObject,Options,Conf),
  NewObject = get_object(Name,Conf1),
  NewInherits = get_inherits(NewObject),
  Conf2 = remove_inherits(Name,OldInherits,Conf1),
  add_inherits(Name,NewInherits,Conf2).

get_inherits(error) -> [];
get_inherits({ok,I}) ->
  case lists:keysearch(translated_to,1,I#info_rec.options) of
    {value,{_,Translated_to}} -> [Translated_to];
    _ -> []
  end.

remove_inherits(From,Inherits,Conf) ->
  lists:foldl
    (fun (I,ConfFold) ->
	 case get_object(I,ConfFold) of
	   error ->
	     ConfFold;
	   {ok,Obj} ->
	     dict:store
	       (Obj#info_rec.name,
		Obj#info_rec{inherits=lists:delete(From,Obj#info_rec.inherits)},
		ConfFold)
	 end
     end, Conf, Inherits).

add_inherits(From,Inherits,Conf) ->
  lists:foldl
    (fun (I,ConfFold) ->
	 case get_object(I,ConfFold) of
	   error ->
	     dict:store
	       (I,#info_rec{name=I,inherits=[From]},ConfFold);
	   {ok,Obj} ->
	     dict:store
	       (Obj#info_rec.name,
		Obj#info_rec{inherits=[From|Obj#info_rec.inherits]},
		ConfFold)
	 end
     end, Conf, Inherits).

update_object(Name,error,Options,Conf) ->
  dict:store(Name,new_object(Name,Options),Conf);
update_object(Name,{ok,Obj},Options,Conf) ->
  dict:store(Name,update_object_options(Obj,Options),Conf).
  
new_object(Name,Options) ->
  #info_rec{name=Name,options=make_options(Options)}.

update_object_options(Object,Options) ->
  OldOptions = Object#info_rec.options,
  Object#info_rec{options=update_options(Options,OldOptions)}.

make_options(Options) ->
  orddict:from_list(Options).

update_options(Options,Dict) ->
  lists:foldl
    (fun ({Key,Value},NewDict) ->
	 orddict:store(Key,Value,NewDict)
     end, Dict, Options).

get_obj_inh(Name,Conf) ->
  get_obj_inh(Name,Conf,[]).

get_obj_inh(Name,Conf,Seen) ->
  case get_object(Name,Conf) of
    Object={ok,Info} -> 
      ?LOG("get_obj_inh(~p) ==> ~p~n",[Name,Info]),
      NewSeen = [Name|Seen],
      lists:foldl
	(fun (Inherits,I) ->
	     case lists:member(Inherits,Seen) of
	       true -> I;
	       false -> merge_infos(get_obj_inh(Inherits,Conf,NewSeen),I)
	     end
	 end, Object, Info#info_rec.inherits);
    Other -> Other
  end.

get_module(ModuleName,Conf) ->
  get_obj_inh(ModuleName,Conf).

get_function(ModuleName,FunctionName,Arity,Conf) ->
  ?LOG("get_function ~p:~p/~p~n",[ModuleName,FunctionName,Arity]),
  Module = get_obj_inh(ModuleName,Conf),
  ?LOG("Module=~p~n",[Module]),
  Function = get_obj_inh({ModuleName,FunctionName,Arity},Conf),
  ?LOG("Function=~p~n",[Function]),
  ModuleInherits = 
    case Module of
      {ok,I} -> I#info_rec.inherits;
      _ -> []
    end,
  merge_infos
    ([Module]++
     (lists:map
      (fun (Inherits) ->
	   get_function(Inherits,FunctionName,Arity,Conf)
       end, ModuleInherits))++
     [Function]).

merge_infos(L) ->
  lists:foldl(fun (I,Saved) -> merge_infos(Saved,I) end, error, L).

merge_infos(error,I2) -> I2;
merge_infos(I1,error) -> I1;
merge_infos({ok,I1},{ok,I2}) ->
  {ok,
   I2#info_rec
   {options=
    orddict:merge
    (fun (_,_,V) -> V end,
     I1#info_rec.options,
     I2#info_rec.options)}}.

read_file(FileName) ->
  add_file(FileName,new_conf()).
  
add_file(FileName,Conf) ->
  {ok,F} = file:open(FileName,[read]),
  {ok,Term} = io:read(F,""),
  Result =
    lists:foldl
      (fun (Command,FoldConf) ->
	   case check_command(Command) of
	     {Name,Options} ->
	       add_item(Name,Options,FoldConf)
	   end
       end, Conf, Term),
  ok = file:close(F),
  Result.
	     
check_command({{ModuleName,FunctionName,Arity},Options}) ->
  {{ModuleName,FunctionName,Arity},check_options(function,Arity,Options)};
check_command({ModuleName,Options}) ->
  {ModuleName,check_options(module,void,Options)}.

check_options(ObjectType,Arity,Options) ->
  lists:map
    (fun (Option) ->
	 case Option of
	   blacklisted ->
	     {blacklisted,true};
	   mc_blacklisted ->
	     {mc_blacklisted,true};
	   rcv ->
	     {rcv,true};
	   snd ->
	     {snd,true};
	   {blacklisted,B} when is_boolean(B) ->
	     Option;
	   {mc_blacklisted,B} when is_boolean(B) ->
	     Option;
	   {snd,B} when is_boolean(B) ->
	     Option;
	   {rcv,B} when is_boolean(B) ->
	     Option;
	   {translated_to,ModuleName}
	   when is_atom(ModuleName),
		ObjectType==module ->
	     Option;
	   {argument_safe,B} when is_boolean(B) ->
	     Option;
	   {translated_to,{ModuleName,FunctionName}}
	   when is_atom(ModuleName),
		is_atom(FunctionName), 
		ObjectType==function ->
	     {translated_to,{ModuleName,FunctionName,Arity}};
	   _ ->
	     throw(options)
	 end
     end, Options).

func_mapping(Module,Name,Arity,Conf) ->
  case get_function(Module,Name,Arity,Conf) of
    error -> no;
    {ok,I} -> 
      case orddict:find(translated_to,I#info_rec.options) of
	Result={ok, {_NewModuleName,_NewFunctionName,_NewArity}} ->
	  Result;
	{ok,NewModuleName} ->
	  {ok, {NewModuleName,Name,Arity}};
	_ -> no
      end
  end.
	  
map_fun(Module,Name,Args) ->
  map_fun_arity(Module,Name,length(Args)).

map_fun_arity(Module,Name,Arity) ->
  case func_mapping(Module,Name,Arity,mce_conf:get_compile_info()) of
    {ok,{NewModuleName,NewFunctionName,_}} ->
      {NewModuleName,NewFunctionName};
    _ ->
      {Module,Name}
  end.

%% Returned the remapped module name
remap_module(ModuleName,Conf) ->
  %% We permit circular remappings
  remap_module(ModuleName,Conf,[]).

remap_module(ModuleName,Conf,Seen) ->
  case get_object(ModuleName,Conf) of
    {ok,I} ->
      case lists:keysearch(translated_to,1,I#info_rec.options) of
	{value,{_,NewModuleName}} when is_atom(NewModuleName) ->
	  case lists:member(NewModuleName,Seen) of
	    true -> ModuleName;
	    false -> remap_module(NewModuleName,Conf,[ModuleName|Seen])
	  end;
	_ -> ModuleName
      end;
    _ -> ModuleName
  end.

%% Returned a remapped function (or identity)
remap_function(ModuleName,FunName,Arity,Conf) ->
  %% We permit circular remappings
  remap_function(ModuleName,FunName,Arity,Conf,[]).

remap_function(ModuleName,FunName,Arity,Conf,Seen) ->
  case get_object({ModuleName,FunName,Arity},Conf) of
    {ok,I} ->
      case lists:keysearch(translated_to,1,I#info_rec.options) of
	{value,{_,{NewModuleName,NewFunName,NewArity}}} 
	  when is_atom(NewModuleName), is_atom(NewFunName) ->
	  case lists:member({NewModuleName,NewFunName,NewArity},Seen) of
	    true ->
	      {ModuleName,FunName,Arity};
	    false ->
	      remap_function
		(NewModuleName,NewFunName,NewArity,
		 Conf,
		 [{ModuleName,FunName,Arity}|Seen])
	  end;
	_ -> {ModuleName,FunName,Arity}
      end;
    _ -> {ModuleName,FunName,Arity}
  end.

%% Given a configuration, returns all module remappings
all_module_remappings(Conf) ->
  Remappings =
    dict:fold
      (fun (Key,Value,Acc) ->
	   if
	     %% Module definition
	     is_atom(Key) ->
	       case lists:keysearch(translated_to,1,Value#info_rec.options) of
		 {value,_} ->
		   case remap_module(Key,Conf) of
		     Conf -> Acc;
		     NewName -> [{Key,NewName}|Acc]
		   end;
		 _ -> Acc
	       end;
	     true -> Acc
	   end
       end, [], Conf),
  lists:usort(Remappings).

%% Given a configuration, returns all module remappings
all_function_remappings(Conf) ->
  Remappings =
    dict:fold
      (fun (Key,Value,Acc) ->
	   case Key of
	     %% Module definition
	     {Module,Fun,Arity} ->
	       case lists:keysearch(translated_to,1,Value#info_rec.options) of
		 {value,_} ->
		   case remap_function(Module,Fun,Arity,Conf) of
		     {Module,Fun,Arity} ->
		       Acc;
		     {NewModule,NewFun,NewArity} ->
		       [{{Module,Fun,Arity},{NewModule,NewFun,NewArity}}|Acc]
		   end;
		 _ -> Acc
	       end;
	     _ -> Acc
	   end
       end, [], Conf),
  lists:usort(Remappings).
