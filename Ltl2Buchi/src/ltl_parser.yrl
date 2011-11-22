%% Copyright (c) 2009, Hans Svensson
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

Rootsymbol formula.
Nonterminals form prop formula.
Terminals atom '(' ')' '=' integer until always next eventually or and not release equivalent implies false true var.
Right 40 implies.
Right 30 equivalent.
Left  50 or and.
Right 60 until release.
Right 70 eventually always.
Nonassoc 80 next.
Nonassoc 90 not.
Nonassoc 100 atom. 
formula -> form : '$1'.
form -> '(' form ')' : '$2'.
form -> true : ltl:ltrue().
form -> false : ltl:lfalse().
form -> prop : '$1'.
form -> not form : ltl:lnot('$2').
form -> next form : ltl:next('$2').
form -> always form : ltl:always('$2').
form -> eventually form : ltl:eventually('$2').
form -> form until form : ltl:until('$1','$3').
form -> form release form : ltl:release('$1','$3').
form -> form or form : ltl:lor('$1','$3').
form -> form and form : ltl:land('$1','$3').
form -> form implies form : ltl:implication('$1','$3').
form -> form equivalent form : ltl:equivalent('$1','$3').
prop -> atom '=' integer :  
  case element(3,'$3') of
    1 ->
      ltl:prop(element(3,'$1'));
	_ ->
	  ltl:lnot(ltl:prop(element(3,'$1')))
  end.
prop -> atom : 
  ltl:prop(element(3,'$1')).
prop -> var :  
  ltl:prop({'var',element(3,'$1')}).

		   

			 


