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

-module(mce_alg_combine).
-export([init/8,run/5,default_conf/0]).


-include("monState.hrl").
-include("monitor.hrl").
-include("stackEntry.hrl").
-include("mce_opts.hrl").
-include("state.hrl").


%%-define(debug,true).
-include("macros.hrl").


default_conf() -> #mce_opts{}.

init(Conf,S,{Alg1,Alg2},_,_,_,_,_) ->
  {Conf1,{ok,{Mod1,Fun1,Args1}}} =
    init_algorithm(Alg1,Conf,S),
  {ok,
   {?MODULE,run,[Mod1,Fun1,Args1,Alg2,Conf1]}}.

run(Mod1, Fun1, Args1, Alg2, Conf) ->
  mce_conf:prepare_run(Conf),
  Result = erlang:apply(Mod1, Fun1, Args1),
  case mce_result:is_mce_result(Result) of
    true ->
      case mce_result:state(Result) of
	void ->
	  mce_conf:format
	    (error,
	     "Cannot continue after simulation returns status~n~p~n", 
	     [void]),
	  mce_result:mk_internal_error
	    (mce_result:mk_value_error({simulation,returned,void}));
	FinalState ->
	  ?LOG
	    ("First algorithm returned state~n~p~n", [FinalState]),
	  Alg2Conf =
	    case mce_result:stack(Result) of
	      void ->
		Alg2;
	      Other ->
		case Alg2#mce_opts.algorithm of
		  {mce_alg_safety,_} -> Alg2#mce_opts{saved_stack=Other};
		  _ -> Alg2
		end
	    end,
	  {Conf2, {ok, {Mod2, Fun2, Args2}}} =
	    init_algorithm(Alg2Conf, Conf, FinalState),
	  %% A bug: we should probably use Alg2Conf instead of Conf2 here...
	  mce_conf:prepare_run(Conf2),
	  mce_conf:format
	    (normal,
	     "Switching to second verification algorithm~n",
	     []),
	  erlang:apply(Mod2, Fun2, Args2)
      end;
    false -> 
      mce_conf:format
	(error,
	 "Cannot continue after simulation returns status~n~p~n", 
	 [Result]),
      mce_result:mk_internal_error
	(mce_result:mk_value_error({simulation,returned,Result}))
  end.

init_algorithm(Conf, ParentConf, S) ->
  ResolvedConf =
    case Conf#mce_opts.algorithm of
      {AlgorithmModule, _} ->
	ConfSpec = [Conf,AlgorithmModule:default_conf(),ParentConf],
	mce_conf:resolve_conf(ConfSpec);
      Other ->
	mce_conf:format
	  (error,"*** Error: Algorithm ~p incorrectly specified~n", [Other]),
	throw(resolve_user_confs)
    end,
  {Module, Args} = mce_conf:algorithm_init_args(ResolvedConf),
  ReturnConf = ResolvedConf#mce_opts{parent=ParentConf},
  {ReturnConf, erlang:apply(Module, init, [ReturnConf, S| Args])}.
