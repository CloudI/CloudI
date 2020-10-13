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
%%% MIT License
%%%
%%% Copyright (c) 2015-2020 Michael Truog <mjtruog at protonmail dot com>
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a
%%% copy of this software and associated documentation files (the "Software"),
%%% to deal in the Software without restriction, including without limitation
%%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%%% and/or sell copies of the Software, and to permit persons to whom the
%%% Software is furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%%% DEALINGS IN THE SOFTWARE.
%%%
%%% @author Michael Truog <mjtruog at protonmail dot com>
%%% @copyright 2015-2020 Michael Truog
%%% @version 2.0.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_trans_id).
-author('mjtruog at protonmail dot com').

%% external interface
-export([datetime/1,
         datetime/2,
         from_string/1,
         increment/1,
         microseconds/0,
         microseconds/1,
         to_string/1,
         to_string/2]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Provide an ISO8601 datetime in UTC based on the time stored in the transaction id.===
%% @end
%%-------------------------------------------------------------------------

-spec datetime(TransId :: cloudi_x_uuid:cloudi_x_uuid()) ->
    cloudi_timestamp:iso8601().

datetime(TransId) ->
    cloudi_x_uuid:get_v1_datetime(TransId).

%%-------------------------------------------------------------------------
%% @doc
%% ===Provide an ISO8601 datetime in UTC based on the time stored in the transaction id with an offset in microseconds.===
%% @end
%%-------------------------------------------------------------------------

-spec datetime(TransId :: cloudi_x_uuid:cloudi_x_uuid(),
               MicroSecondsOffset :: integer()) ->
    cloudi_timestamp:iso8601().

datetime(TransId, MicroSecondsOffset) ->
    cloudi_x_uuid:get_v1_datetime(TransId, MicroSecondsOffset).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return a transaction id as a binary UUID (16 bytes).===
%% @end
%%-------------------------------------------------------------------------

-spec from_string(String :: cloudi_x_uuid:uuid_string()) ->
    cloudi_x_uuid:cloudi_x_uuid().

from_string(String) ->
    cloudi_x_uuid:string_to_uuid(String).

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

%%-------------------------------------------------------------------------
%% @doc
%% ===Microseconds since the UNIX epoch.===
%% The integer value returned uses the same time source used for creating
%% TransId v1 UUIDs, though the return values are not strictly
%% monotonically increasing on Erlang >= 18.0 if the OS time is changed
%% (TransId v1 UUID time values are strictly monotonically increasing
%%  when comparing TransId values created by the same service Erlang process).
%% (The UNIX epoch is 1970-01-01T00:00:00)
%% @end
%%-------------------------------------------------------------------------

-spec microseconds() ->
    non_neg_integer().

microseconds() ->
    cloudi_x_uuid:get_v1_time(erlang).

%%-------------------------------------------------------------------------
%% @doc
%% ===Microseconds since the UNIX epoch.===
%% The integer value returned is always increasing for each service process
%% that created the TransId (with the service process uniquely represented
%% in the node_id section of the v1 UUID).
%% (The UNIX epoch is 1970-01-01T00:00:00)
%% @end
%%-------------------------------------------------------------------------

-spec microseconds(TransId :: cloudi_x_uuid:cloudi_x_uuid()) ->
    non_neg_integer().

microseconds(TransId) ->
    cloudi_x_uuid:get_v1_time(TransId).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the transaction id in a standard string format.===
%% @end
%%-------------------------------------------------------------------------

-spec to_string(TransId :: cloudi_x_uuid:cloudi_x_uuid()) ->
    cloudi_x_uuid:uuid_string_list().

to_string(TransId) ->
    to_string(TransId, standard).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the transaction id in a string format.===
%% @end
%%-------------------------------------------------------------------------

-spec to_string(TransId :: cloudi_x_uuid:cloudi_x_uuid(),
                Format :: standard | nodash |
                          list_standard | list_nodash |
                          binary_standard | binary_nodash) ->
    cloudi_x_uuid:uuid_string().

to_string(TransId, Format) ->
    cloudi_x_uuid:uuid_to_string(TransId, Format).

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

