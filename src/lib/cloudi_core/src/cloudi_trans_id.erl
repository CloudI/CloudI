%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Transaction ID Usage==
%%% Convenience functions for using the v1 UUID used as the
%%% transaction id ("TransId" or "trans_id") that follows a CloudI
%%% service request for the lifetime (defined by the service request timeout)
%%% of the request and its response result.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2015, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2015 Michael Truog
%%% @version 1.5.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_trans_id).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([datetime/1,
         datetime/2,
         uuid/1,
         uuid/2,
         increment/1]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Provide an ISO8601 datetime in UTC based on the time stored in the transaction id.===
%% @end
%%-------------------------------------------------------------------------

-spec datetime(TransId :: cloudi_x_uuid:cloudi_x_uuid()) ->
    string().

datetime(TransId) ->
    cloudi_x_uuid:get_v1_datetime(TransId).

%%-------------------------------------------------------------------------
%% @doc
%% ===Provide an ISO8601 datetime in UTC based on the time stored in the transaction id with an offset in microseconds.===
%% @end
%%-------------------------------------------------------------------------

-spec datetime(TransId :: cloudi_x_uuid:cloudi_x_uuid(),
               MicroSecondsOffset :: integer()) ->
    string().

datetime(TransId, MicroSecondsOffset) ->
    cloudi_x_uuid:get_v1_datetime(TransId, MicroSecondsOffset).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the transaction id in a standard string format.===
%% @end
%%-------------------------------------------------------------------------

-spec uuid(TransId :: cloudi_x_uuid:cloudi_x_uuid()) ->
    string().

uuid(TransId) ->
    uuid(TransId, standard).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the transaction id in a string format.===
%% @end
%%-------------------------------------------------------------------------

-spec uuid(TransId :: cloudi_x_uuid:cloudi_x_uuid(),
           Format :: standard | nodash |
                     list_standard | list_nodash |
                     binary_standard | binary_nodash) ->
    string() | binary().

uuid(TransId, Format) ->
    cloudi_x_uuid:uuid_to_string(TransId, Format).

%%-------------------------------------------------------------------------
%% @doc
%% ===Increment the transaction id while preserving uniqueness.===
%% Increment the v1 UUID's clock_seq (14bit) integer for 16384 possible
%% v1 UUID values based on a single transaction id.
%% @end
%%-------------------------------------------------------------------------

-spec increment(TransId :: cloudi_x_uuid:cloudi_x_uuid()) ->
    cloudi_x_uuid:cloudi_x_uuid().

increment(TransId) ->
    cloudi_x_uuid:increment(TransId).

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

