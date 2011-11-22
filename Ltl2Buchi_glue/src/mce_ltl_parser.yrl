Rootsymbol formula_or_abstraction.
Nonterminals formula proposition formula_or_abstraction.
Terminals atom '(' ')' '{' '}' '/' ':' ',' until always next eventually or and not release equivalent implies false true var fun integer.
Right 30 equivalent.
Right 40 implies.
Left  50 or and.
Right 60 until release.
Right 70 eventually always.
Nonassoc 80 next.
Nonassoc 90 not.
Nonassoc 100 atom. 
formula_or_abstraction -> formula : '$1'.
formula -> '(' formula ')' : '$2'.
formula -> proposition : '$1'.
formula -> formula until formula : ltl:until('$1','$3').
formula -> always formula : ltl:always('$2').
formula -> next formula : ltl:next('$2').
formula -> eventually formula : ltl:eventually('$2').
formula -> formula or formula : ltl:lor('$1','$3').
formula -> formula and formula : ltl:land('$1','$3').
formula -> not formula : ltl:lnot('$2').
formula -> formula release formula : ltl:release('$1','$3').
formula -> formula implies formula : ltl:implication('$1','$3').
formula -> formula equivalent formula : ltl:equivalent('$1','$3').
formula -> true : ltl:ltrue().
formula -> false : ltl:lfalse().
proposition -> 'fun' atom ':' atom '/' integer: 
  ltl:prop({element(3,'$2'),element(3,'$4')}).
proposition -> '{' atom ',' atom '}': 
  ltl:prop({element(3,'$2'),element(3,'$4')}).
proposition -> var :  
  ltl:prop({'var',element(3,'$1')}).

		   

			 


