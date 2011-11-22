-module(tests).
-language(erlang).

-export([module_tests/0]).
-export([does_request/3,does_release/3]).
-export([scenario/2,loop/1]).

-include("mce_opts.hrl").
-include("test.hrl").

module_tests() ->
  [
   #test{name=?MODULE_STRING++":"++"2excl_nofail"++"@examples/locker_v5",
	 precode=define_monprop(monprop),
	 runSpec =
	 #mce_opts
	 {program={?MODULE,scenario,[[{[a],exclusive},{[a],exclusive}],false]},
	  algorithm={mce_alg_buechi,void},
	  monitor={monprop,{void,[{'V',fun tests:does_release/3}]}}}},
   #test{name=?MODULE_STRING++":"++"2excl_fail"++"@examples/locker_v5",
	 precode=define_monprop1(monprop1),
	 runSpec =
	 #mce_opts
	 {program={?MODULE,scenario,
		   [[{[a],exclusive},{[a],exclusive}],true]},
	  algorithm={mce_alg_buechi,void},
	  monitor={monprop1,void}},
	 interpreteOutcome = {fun mce_test_observer:monitorFails/3,void}},
   #test{name=?MODULE_STRING++":"++"excl_shared_nofail"++"@examples/locker_v5",
	 precode=define_monprop(monprop2),
	 runSpec =
	 #mce_opts
	 {program={?MODULE,scenario,
		   [[{[a],exclusive},{[a],shared}],false]},
	  algorithm={mce_alg_buechi,void},
	  monitor={monprop2,{void,[{'V',fun tests:does_release/3}]}}}},
   #test{name=?MODULE_STRING++":"++"2excl_shared_nofail"++"@examples/locker_v5",
	 precode=define_monprop1(monprop3),
	 runSpec =
	 #mce_opts
	 {program={?MODULE,scenario,
		   [[{[a],exclusive},{[a],exclusive},{[a],shared}],false]},
	  algorithm={mce_alg_buechi,void},
	  monitor={monprop3,void}},
	 interpreteOutcome = {fun mce_test_observer:monitorFails/3,void}}
   ].

define_monprop(Name) ->
  fun () ->
      mce_ltl_parse:ltl2module_and_load
	(ltl:always
	 (ltl:implication
	  (ltl:prop(fun tests:does_request/3),
	   ltl:next(ltl:eventually(ltl:prop({var,'V'}))))),
	 Name)
  end.

define_monprop1(Name) ->
  fun () ->
      mce_ltl_parse:ltl2module_and_load
	(ltl:always
	 (ltl:implication
	  (ltl:prop(fun tests:does_request/3),
	   ltl:next(ltl:eventually(ltl:prop(fun tests:does_release/3))))),
	 Name)
  end.

does_request(_,Actions,_) ->
  lists_try_findret
    (fun (Action) ->
	 {request,Resources} = mce_erl_actions:get_probe(Action),
	 {ok,{mce_erl_actions:get_source(Action),Resources}}
     end, Actions).
      
does_release(_,Actions,{Pid,Resources}) ->
  lists_try_any
    (fun (Action) ->
	 {release,Resources} = mce_erl_actions:get_probe(Action),
	 Pid == mce_erl_actions:get_source(Action)
     end, Actions).

scenario(Configuration,KillClients) ->
  locker_sup:start(Configuration,KillClients),
  mce_erl:recv({?MODULE,loop,[]}).

lists_try_any(F,Actions) ->
  lists:any
    (fun (Action) -> try F(Action) catch _:_ -> false end end, Actions).

lists_try_findret(F,Actions) ->
  case 
    mce_utils:findret
    (fun (Action) -> try F(Action) catch _:_ -> false end end, Actions) of
    {ok, Result} -> {true,Result};
    _ -> false
  end.

loop(_) -> 
  false.

