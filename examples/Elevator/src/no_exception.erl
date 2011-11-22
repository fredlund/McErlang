-module(no_exception).
-language(erlang).
-export([init/1,stateChange/3,monitorType/0]).

-behaviour(mce_behav_monitor).

monitorType() -> safety.

init(State) -> {ok,void}.

stateChange(_,MonState,Stack) ->  {ok,MonState}.


