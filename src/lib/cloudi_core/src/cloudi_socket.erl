%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
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
%%% @version 1.2.2 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_socket).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([local_listen/2,
         local_accept/5]).

-on_load(init/0).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

init() ->
    Path = [_ | _] = code:priv_dir(cloudi_core),
    erlang:load_nif(filename:join([Path, "libcloudi_socket_drv"]), []).

local_listen(_FileDescriptor, _SocketPath) ->
    erlang:nif_error(not_loaded).

local_accept(_FileDescriptorIn, _FileDescriptorOut, _SocketPath,
             _ReceiveBufferSize, _SendBufferSize) ->
    erlang:nif_error(not_loaded).

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

