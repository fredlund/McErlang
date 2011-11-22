-module(run).
-language(erlang).

-export([debug/0,safety/0,logon/0]).
-export([message_received1/0,message_received1_combine/0]).
-export([message_received2/0,message_received2_combine/0]).

-export([message_received3/0,message_received3_combine/0]).
-export([message_received4_combine/0]).


-export([fig_stat_2users/0,fig_stat_3users/0,
	 fig_stat_2users_combine/0,fig_stat_3users_combine/0]).
-export([exp1/0,exp2/0]).

-include("mce_opts.hrl").
-include("table.hrl").
-include("erlang/emacros.hrl").

debug() ->
  mce:start
    (#mce_opts
     {program={scenario,start,[[[{logon,clara},{message,fred,"hola"},logoff],
				[{logon,fred},logoff]]]},
      algorithm={mce_alg_debugger,void}}).

safety() ->
  mce:start
    (#mce_opts
     {program={scenario,start,[[[{logon,clara},{message,fred,"hola"},logoff],
				[{logon,fred}]]]},
      algorithm={mce_alg_safety,void}}).

logon() ->
  mce:start
    (#mce_opts
     {program={scenario,start,[[[{logon,clara},{message,fred,"hola"},logoff],
				[{logon,fred}]]]},
      monitor={monSendLogon,[]},
      algorithm={mce_alg_safety,void}}).

message_received1() ->
  mce:start
    (#mce_opts
     {program={scenario,start,[[[{logon,clara},{message,fred,"hi"},logoff],
				[{logon,fred},logoff]]]},
      monitor={mce_ltl_parse:ltl_string2module_and_load("not P until Q implies (eventually P implies eventually R)",
					     messenger_mon),
	       {void,[{'P',basicPredicates:message_to(clara,fred,"hi")},
		      {'Q',basicPredicates:logon(fred)},
		      {'R',basicPredicates:message_received(fred,clara,"hi")}]}},
      algorithm={mce_alg_buechi,void}}).

message_received1_combine() ->
  mce:start
    (#mce_opts
     {program={scenario,start,
	       [[[{logon,clara},{message,fred,"hi"},logoff],
		 [{logon,fred},logoff]]]},
      algorithm={mce_alg_combine,{#mce_opts{algorithm={mce_alg_simulation,void},
					    scheduler={scheduler,void}},
				  #mce_opts{algorithm={mce_alg_buechi,void},
					    monitor={mce_ltl_parse:ltl_string2module_and_load("not P until Q implies (eventually P implies eventually R)",
					     messenger_mon),
	       {void,[{'P',basicPredicates:message_to(clara,fred,"hi")},
		      {'Q',basicPredicates:logon(fred)},
		      {'R',basicPredicates:message_received(fred,clara,"hi")}]}}}}}}).


message_received2() ->
  mce:start
    (#mce_opts
     {program={scenario,start,[[[{logon,clara},{message,fred,"hi"},logoff],
				[{logon,fred},logoff]]]},
      monitor={mce_ltl_parse:ltl_string2module_and_load("not P until Q => (eventually P => eventually (R or S))",
					     messenger_mon),
	       {void,[{'P',basicPredicates:message_to(clara,fred,"hi")},
		      {'Q',basicPredicates:logon(fred)},
		      {'R',basicPredicates:message_received(fred,clara,"hi")},
		      {'S',basicPredicates:logoff(fred)}]}},
      algorithm={mce_alg_buechi,void}}).



message_received2_combine() ->
  mce:start
    (#mce_opts
     {program={scenario,start,
	       [[[{logon,clara},{message,fred,"hi"},logoff],
		 [{logon,fred},logoff],
		 [{logon,erik},{message,fred,"hello"},logoff]
		]]},
      algorithm={mce_alg_combine,{#mce_opts{algorithm={mce_alg_simulation,void},
					    scheduler={scheduler,void}},
				  #mce_opts{algorithm={mce_alg_buechi,void},
					    monitor={mce_ltl_parse:ltl_string2module_and_load("not P until Q => (eventually P => eventually (R or S))",
					     messenger_mon),
	       {void,[{'P',basicPredicates:message_to(clara,fred,"hi")},
		      {'Q',basicPredicates:logon(fred)},
		      {'R',basicPredicates:message_received(fred,clara,"hi")},
		      {'S',basicPredicates:logoff(fred)}]}}}}}}).


message_received3() ->
  mce:start
    (#mce_opts
     {program={scenario,start,[[[{logon,clara},{message,fred,"hi"},logoff],
				[{logon,fred},logoff],
				[{logon,erik},{message,fred,"hello"},logoff]
			       ]]},
      monitor={mce_ltl_parse:ltl_string2module_and_load("always ((P and T) => eventually (R or not T))",
					     messenger_mon),
	       {void,[{'P',basicPredicates:message_to(clara,fred,"hi")},
		      {'T',basicPredicates:logged_on(fred)},
		      {'R',basicPredicates:message_received(fred,clara,"hi")}]}},
      algorithm={mce_alg_buechi,void}}).

  
message_received3_combine() ->
  mce:start
    (#mce_opts
     {program={scenario,start,
	       [[[{logon,clara},{message,fred,"hi"},logoff],
		 [{logon,fred},logoff],
		 [{logon,erik},{message,fred,"hello"},logoff]
		]]},
      algorithm={mce_alg_combine,{#mce_opts{algorithm={mce_alg_simulation,void},
					    scheduler={scheduler,void}},
				  #mce_opts{algorithm={mce_alg_buechi,void},
					    monitor={mce_ltl_parse:ltl_string2module_and_load("always ((P and T) => eventually (R or not T))",
					     messenger_mon),
	       {void,[{'P',basicPredicates:message_to(clara,fred,"hi")},
		      {'T',basicPredicates:logged_on(fred)},
		      {'R',basicPredicates:message_received(fred,clara,"hi")}]}}}}}}).

message_received4_combine() ->
  mce:start
    (#mce_opts
     {program={scenario,start,
	       [[[{logon,clara},{message,fred,"hi"},logoff],
		 [{logon,fred},logoff],
		 [{logon,erik},{message,fred,"hello"},logoff]
		]]},
      algorithm={mce_alg_combine,{#mce_opts{algorithm={mce_alg_simulation,void},
					    scheduler={scheduler,void}},
				  #mce_opts{algorithm={mce_alg_buechi,void},
					    monitor={mce_ltl_parse:ltl_string2module_and_load("always ((P and T) => eventually (R or not T))",
					     messenger_mon),
	       {void,[{'P',basicPredicates:message_to(clara,fred,"hi")},
		      {'T',basicPredicates:logged_on(fred)},
		      {'R',basicPredicates:message_received(fred,clara,"hi")}]}}}}}}).

fig_stat_2users() ->
  mce:start
    (#mce_opts
     {program={scenario,start,
	       [[[{logon,clara},{message,fred,"hi"},logoff],
		 [{logon,fred},logoff]
		]]},
      monitor={mce_ltl_parse:ltl_string2module_and_load
	       ("always ((P and T) => eventually (R or not T))",
		messenger_mon),
	       {void,
		[{'P',basicPredicates:message_to(clara,fred,"hi")},
		 {'T',basicPredicates:logged_on(fred)},
		 {'R',basicPredicates:message_received(fred,clara,"hi")}]}},
      algorithm={mce_alg_buechi,void}}).

fig_stat_3users() ->
  mce:start
    (#mce_opts
     {program={scenario,start,
	       [[[{logon,clara},{message,fred,"hi"},logoff],
		 [{logon,fred},logoff],
		 [{logon,erik},{message,fred,"hello"},logoff]
		]]},
      monitor={mce_ltl_parse:ltl_string2module_and_load
	       ("always ((P and T) => eventually (R or not T))",
		messenger_mon),
	       {void,
		[{'P',basicPredicates:message_to(clara,fred,"hi")},
		 {'T',basicPredicates:logged_on(fred)},
		 {'R',basicPredicates:message_received(fred,clara,"hi")}]}},
      algorithm={mce_alg_buechi,void}}).
  
fig_stat_2users_combine() ->
  mce:start
    (#mce_opts
     {program={scenario,start,
	       [[[{logon,clara},{message,fred,"hi"},logoff],
		 [{logon,fred},logoff]
		]]},
      algorithm=
      {mce_alg_combine,
       {#mce_opts{algorithm={mce_alg_simulation,void},
		  scheduler={scheduler,void}},
	#mce_opts
	{algorithm={mce_alg_buechi,void},
	 monitor=
	 {mce_ltl_parse:ltl_string2module_and_load
	  ("always ((P and T) => eventually (R or not T))",
	   messenger_mon),
	  {void,
	   [{'P',basicPredicates:message_to(clara,fred,"hi")},
	    {'T',basicPredicates:logged_on(fred)},
	    {'R',basicPredicates:message_received(fred,clara,"hi")}]}}}}}}).

fig_stat_3users_combine() ->
  mce:start
    (#mce_opts
     {program={scenario,start,
	       [[[{logon,clara},{message,fred,"hi"},logoff],
		 [{logon,fred},logoff],
		 [{logon,erik},{message,fred,"hello"},logoff]
		]]},
      algorithm=
      {mce_alg_combine,
       {#mce_opts{algorithm={mce_alg_simulation,void},
		  scheduler={scheduler,void}},
	#mce_opts
	{algorithm={mce_alg_buechi,void},
	 monitor=
	 {mce_ltl_parse:ltl_string2module_and_load
	  ("always ((P and T) => eventually (R or not T))",
	   messenger_mon),
	  {void,
	   [{'P',basicPredicates:message_to(clara,fred,"hi")},
	    {'T',basicPredicates:logged_on(fred)},
	    {'R',basicPredicates:message_received(fred,clara,"hi")}]}}}}}}).

exp1() ->
  mce:start
    (#mce_opts
     {program={scenario,start,
	       [[[{logon,clara},{message,fred,"hi"},logoff],
		 [{logon,fred},logoff],
		 [{logon,erik},{message,fred,"hello"},logoff]
		]]},
      monitor={mce_ltl_parse:ltl_string2module_and_load("always((not P until Q) => (eventually P => eventually (R or S)))",
					     messenger_mon),
	       {void,[{'P',basicPredicates:message_to(clara,fred,"hi")},
		      {'Q',basicPredicates:logon(fred)},
		      {'R',basicPredicates:message_received(fred,clara,"hi")},
		      {'S',basicPredicates:logoff(fred)}]}},
      algorithm={mce_alg_buechi,void}}).

exp2() ->  
  mce:start
    (#mce_opts
     {program={scenario,start,
	       [[[{logon,clara},{message,fred,"hi"},logoff],
		 [{logon,fred},logoff],
		 [{logon,erik},{message,fred,"hello"},logoff]
		]]},
      monitor={mce_ltl_parse:ltl_string2module_and_load
	       ("always ((P and T) => eventually (R or not T))",
		messenger_mon),
	       {void,
		[{'P',basicPredicates:message_to(clara,fred,"hi")},
		 {'T',basicPredicates:logged_on(fred)},
		 {'R',basicPredicates:message_received(fred,clara,"hi")}]}},
      algorithm={mce_alg_buechi,void}}).
  
