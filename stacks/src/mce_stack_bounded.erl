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

-module(mce_stack_bounded).

-record(bstack,
	{bottomSize=0,
	 bottom=[],
	 topSize=0,
	 top=[],
	 maxElems=0}).

-export([empty/1,push/2,pop/1,is_empty/1,size/1]).

empty(Size) ->
  if Size>0 ->
      {ok,#bstack{maxElems=Size}};
     true ->
      throw(size)
  end.

push(Element,Stack) ->
  case Stack#bstack.bottomSize+Stack#bstack.topSize < Stack#bstack.maxElems of

    %% Stack not full we can just add to top part
    true ->
      Stack#bstack{top=[Element|Stack#bstack.top],
		   topSize=Stack#bstack.topSize+1};

    %% Stack full we first have to remove from bottom part
    false ->
      case Stack#bstack.bottomSize>0 of

	%% We have elements in the bottom part; just remove the top element
	true ->
	  Stack#bstack{bottom=tl(Stack#bstack.bottom),
		       bottomSize=Stack#bstack.bottomSize-1,
		       top=[Element|Stack#bstack.top],
		       topSize=Stack#bstack.topSize+1};

	%% Bottom part empty, we move part of the top part to the bottom
	false ->
	  SplitPoint = Stack#bstack.topSize div 2,
	  RemainingElements = Stack#bstack.topSize-SplitPoint,

	  {Top,Bottom} = split(Stack#bstack.top,SplitPoint),
	  ReversedBottom = lists:reverse(Bottom),
	  NewStack = 
	    #bstack{top=Top,
		    topSize=SplitPoint,
		    bottom=ReversedBottom,
		    bottomSize=RemainingElements,
		    maxElems=Stack#bstack.maxElems},
	  push(Element,NewStack)
      end
  end.


pop(Stack) ->
  case Stack#bstack.topSize > 0 of

    %% Top part contains element; just remote it
    true ->
      Element = hd(Stack#bstack.top),
      NewStack = Stack#bstack{top=tl(Stack#bstack.top),
			      topSize=Stack#bstack.topSize-1},
      {Element,NewStack};

    %% Top part empty
    false ->
      case Stack#bstack.bottomSize > 0 of 

	%% Bottom part has elements; we move a portion of it to top
	true ->
	  SplitPoint = Stack#bstack.bottomSize div 2,
	  RemainingElements = Stack#bstack.bottomSize-SplitPoint,

	  {Bottom,Top} = split(Stack#bstack.bottom,SplitPoint),
	  ReversedTop = lists:reverse(Top),
	  NewStack = 
	    #bstack{top=ReversedTop,
		    topSize=RemainingElements,
		    bottom=Bottom,
		    bottomSize=SplitPoint,
		    maxElems=Stack#bstack.maxElems},
	  pop(NewStack);
	
	%% Bottom part empty, no way to pop an element
	false ->
	  throw(empty)
      end
  end.

is_empty(Stack) ->
  Stack#bstack.bottom =:= [] andalso Stack#bstack.top =:= [].

size(Stack) ->
  Stack#bstack.bottomSize+Stack#bstack.topSize.


split(L,SplitPoint) ->
  split(L,[],SplitPoint).
split([],Collected,0) ->
  {lists:reverse(Collected),[]};
split(L,Collected,0) ->
  {lists:reverse(Collected),L};
split([Head|Tail],Collected,N) when N>0 ->
  split(Tail,[Head|Collected],N-1).
  
	  
  
  
