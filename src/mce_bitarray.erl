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

%%% Will work only on very restricted combinations of machine architecture and
%%% operating systems; we should do a generic library as well.

-module(mce_bitarray).

-export([create/1, get/2, put/3, on/2, off/2, is_on/2, is_off/2, zero/3]).

%%% Creates a bit array with Size*8 bits
create(Size) when Size>0 ->
    hipe_bifs:bytearray(Size*8, 0).

get(Array, Index) ->
    BytePos = Index div 8,
    BitPos = Index rem 8,
    Byte = hipe_bifs:bytearray_sub(Array, BytePos),
    Byte band (1 bsl BitPos) bsr BitPos.

put(Array, Index, Value) ->
    BytePos = Index div 8,
    BitPos = Index rem 8,
    Byte = hipe_bifs:bytearray_sub(Array, BytePos),
    Bit = 1 bsl BitPos,
    if Value =:= 0 ->
	   hipe_bifs:bytearray_update(Array, BytePos, Byte band bnot Bit);
       Value =:= 1 ->
	   hipe_bifs:bytearray_update(Array, BytePos, Byte bor Bit)
    end,
    Array.

on(Array,Index) ->
    put(Array,Index,1).

off(Array,Index) ->
    put(Array,Index,0).

is_on(Array,Index) ->
    get(Array,Index)=:=1.

is_off(Array,Index) ->
    get(Array,Index)=:=0.

zero(Array,From,To) when To>From -> Array;
zero(Array,From,To) ->
    hipe_bifs:bytearray_update(Array,From,0),
    zero(Array,From+1,To).

    
