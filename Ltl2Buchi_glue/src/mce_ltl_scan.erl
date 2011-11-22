-module(mce_ltl_scan).
-export([scan/1]).

scan([]) -> [];
scan([FirstToken|RestTokens]) ->
  case FirstToken of
    {atom,N,V} ->
      case lists:member(V,atoms()) of
	true ->
	  [{map_atom(V),N}|scan(RestTokens)];
	false ->
	  [FirstToken|scan(RestTokens)]
      end;
    V={var,_,_} ->
      [V|scan(RestTokens)];
    I={integer,_,_} ->
      [I|scan(RestTokens)];
    {Symbol,N} ->
      case lists:member(Symbol,single_symbols()) of
	true ->
	  [{map_symbol(Symbol),N}|scan(RestTokens)];
	false ->
	  case Symbol of
	    '[' ->
	      RemainingTokens = ensure_rest(']',RestTokens),
	      [{'always',N}|scan(RemainingTokens)];
	    '&' ->
	      case RestTokens of
		[{'&',_}|RemainingTokens] ->
		  [{'and',N}|scan(RemainingTokens)];
		_ ->
		  [{'and',N}|scan(RestTokens)]
	      end;
	    '=' ->
	      case RestTokens of
		[{'>',_}|RemainingTokens] ->
		  [{'implies',N}|scan(RemainingTokens)];
		_ ->
		  io:format("*** Error: token ~p not recognised~n",[Symbol]),
		  throw(bad_scan)
	      end;
	    _ ->
	      io:format("*** Error: token ~p not recognised~n",[Symbol]),
	      throw(bad_scan)
	  end
      end;
    _ ->
      io:format("*** Error: token ~p not recognised~n",[FirstToken]),
      throw(bad_scan)
  end.

ensure_rest(Symbol,Tokens) ->
  case Tokens of
    [{Symbol,_}|RestTokens] ->
      RestTokens;
    _ ->
      io:format("*** Error: token ~p not found in ~p~n",[Symbol,Tokens]),
      throw(bad_scan)
  end.

single_symbols() ->
  ['.','/',':',',','+','*','!','(',')','||','==','|','=>','->','{','}',
   'and','or','not','fun','end','dot'].

map_symbol('|') -> 'or';
map_symbol('||') -> 'or';
map_symbol('&') -> 'and';
map_symbol('->') -> 'implies';
map_symbol('=>') -> 'implies';
map_symbol('==') -> 'equivalent';
map_symbol('+') -> 'or';
map_symbol('*') -> 'and';
map_symbol('!') -> 'not';
map_symbol(Symbol) -> Symbol.

atoms() ->
  ['fun','until','always','eventually','next','or','and','not','release',
   'u','g','x','f','r','v','implies','false','true'].

map_atom('u') -> 'until';
map_atom('g') -> 'always';
map_atom('x') -> 'next';
map_atom('f') -> 'eventually';
map_atom('r') -> 'release';
map_atom('v') -> 'release';
map_atom(Atom) -> Atom.
  

  
      
