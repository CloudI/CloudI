%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Socket==
%%% Basic socket functionality not provided internally by Erlang.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2013, Michael Truog <mjtruog at gmail dot com>
%%% All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%% 
%%%     * Redistributions of source code must retain the above copyright
%%%       notice, this list of conditions and the following disclaimer.
%%%     * Redistributions in binary form must reproduce the above copyright
%%%       notice, this list of conditions and the following disclaimer in
%%%       the documentation and/or other materials provided with the
%%%       distribution.
%%%     * All advertising materials mentioning features or use of this
%%%       software must display the following acknowledgment:
%%%         This product includes software developed by Michael Truog
%%%     * The name of the author may not be used to endorse or promote
%%%       products derived from this software without specific prior
%%%       written permission
%%% 
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
%%% CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
%%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
%%% OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
%%% CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
%%% DAMAGE.
%%%
%%% @author Michael Truog <mjtruog [at] gmail (dot) com>
%%% @copyright 2013 Michael Truog
%%% @version 1.2.4 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_socket).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([local/1,
         set/2,
         setsockopts/3]).

-include("cloudi_constants.hrl").

-on_load(init/0).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

-ifdef(CLOUDI_CORE_STANDALONE).
init() ->
    ok.
-else.
init() ->
    case cloudi_core_app:test() of
        true ->
            ok;
        false ->
            Path = [_ | _] = code:priv_dir(cloudi_core),
            erlang:load_nif(filename:join([Path,
                                           "libcloudi_socket_drv"]), [])
    end.
-endif.

-spec local(_SocketPath :: string()) ->
    ok | {error, atom()}.

local(_SocketPath) ->
    erlang:nif_error(not_loaded).

-spec set(_FileDescriptorOld :: integer(),
          _FileDescriptorNew :: integer()) ->
    ok | {error, atom()}.

set(_FileDescriptorOld, _FileDescriptorNew) ->
    erlang:nif_error(not_loaded).

-spec setsockopts(_FileDescriptor :: integer(),
                  _RecBufSize :: pos_integer(),
                  _SndBufSize :: pos_integer()) ->
    ok | {error, atom()}.

setsockopts(_FileDescriptor, _RecBufSize, _SndBufSize) ->
    erlang:nif_error(not_loaded).

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

