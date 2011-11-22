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

-record
(compile_rec,
 {sources=[],          %% List of sources
  mcerlangDir=void,    %% McErlang root directory
  output_dir=void,     %% Where to store compiled files
  fun_info=void,       %% Remappings of functions, declarations of side effects
  libs=void,           %% Library directories (for looking for modules)
  includes=void,       %% Include directories (applied for all source files)
  lib_dict=void,       
  verbose=false,       %% Enable more diagnostic output
  compile_info=void,   
  unknown_is_snd=true, %% If a function is not known it is assumed to have a snd
  unknown_is_rcv=true, %% If a function is not known it is assumed to have a rcv
  warn_if_blacklisted=true, %% Warn for blacklisted functions
  permit_blacklisted=false, %% Permit the calling of blacklisted functions
  sends_are_sefs=false,     %% Sends are considered side effects
  erlang_compile_options,   %% Options to pass to Erlang compiler
  normalize_core=false,     %% Normalize result of transformation before
                            %% core -> beam transformation (bug compatibility)
  sef_analysis=void
  }).

