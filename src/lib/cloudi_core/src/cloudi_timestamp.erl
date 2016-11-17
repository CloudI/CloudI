%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Timestamp operations==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2015-2016, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2015-2016 Michael Truog
%%% @version 1.5.2 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_timestamp).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([timestamp/0,
         seconds/0,
         seconds_os/0,
         milliseconds/0,
         milliseconds_os/0,
         microseconds/0,
         microseconds_os/0,
         seconds_filter/3]).

-include("cloudi_core_i_constants.hrl").

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Return an Erlang VM timestamp.===
%% Not guaranteed to be strictly monotonically increasing
%% (on Erlang >= 18.0).
%% @end
%%-------------------------------------------------------------------------

-spec timestamp() -> erlang:timestamp().

-ifdef(ERLANG_OTP_VERSION_18_FEATURES).
timestamp() ->
    erlang:timestamp().
-else.
timestamp() ->
    erlang:now().
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Seconds since the UNIX epoch.===
%% (The UNIX epoch is 1970-01-01T00:00:00)
%% @end
%%-------------------------------------------------------------------------

-ifdef(ERLANG_OTP_VERSION_18_FEATURES).
seconds() ->
    erlang:system_time(seconds).
-else.
seconds() ->
    {MegaSeconds, Seconds, _} = erlang:now(),
    MegaSeconds * 1000000 + Seconds.
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Seconds since an undefined point in time, from the hardware.===
%% @end
%%-------------------------------------------------------------------------

-ifdef(ERLANG_OTP_VERSION_18_FEATURES).
seconds_os() ->
    os:system_time(seconds).
-else.
seconds_os() ->
    {MegaSeconds, Seconds, _} = os:timestamp(),
    MegaSeconds * 1000000 + Seconds.
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Milliseconds since the UNIX epoch.===
%% (The UNIX epoch is 1970-01-01T00:00:00)
%% @end
%%-------------------------------------------------------------------------

-ifdef(ERLANG_OTP_VERSION_18_FEATURES).
milliseconds() ->
    erlang:system_time(milli_seconds).
-else.
milliseconds() ->
    {MegaSeconds, Seconds, MicroSeconds} = erlang:now(),
    MegaSeconds * 1000000000 + Seconds * 1000 + MicroSeconds div 1000.
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Milliseconds since an undefined point in time, from the hardware.===
%% @end
%%-------------------------------------------------------------------------

-ifdef(ERLANG_OTP_VERSION_18_FEATURES).
milliseconds_os() ->
    os:system_time(milli_seconds).
-else.
milliseconds_os() ->
    {MegaSeconds, Seconds, MicroSeconds} = os:timestamp(),
    MegaSeconds * 1000000000 + Seconds * 1000 + MicroSeconds div 1000.
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Microseconds since the UNIX epoch.===
%% (The UNIX epoch is 1970-01-01T00:00:00)
%% @end
%%-------------------------------------------------------------------------

-ifdef(ERLANG_OTP_VERSION_18_FEATURES).
microseconds() ->
    erlang:system_time(micro_seconds).
-else.
microseconds() ->
    {MegaSeconds, Seconds, MicroSeconds} = erlang:now(),
    MegaSeconds * 1000000000000 + Seconds * 1000000 + MicroSeconds.
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Microseconds since an undefined point in time, from the hardware.===
%% @end
%%-------------------------------------------------------------------------

-ifdef(ERLANG_OTP_VERSION_18_FEATURES).
microseconds_os() ->
    os:system_time(micro_seconds).
-else.
microseconds_os() ->
    {MegaSeconds, Seconds, MicroSeconds} = os:timestamp(),
    MegaSeconds * 1000000000000 + Seconds * 1000000 + MicroSeconds.
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Filter a list of seconds since the UNIX epoch..===
%% The list is not ordered.
%% @end
%%-------------------------------------------------------------------------

seconds_filter(L, SecondsNow, MaxPeriod) ->
    seconds_filter(L, [], SecondsNow, MaxPeriod).

seconds_filter([], Output, _, _) ->
    Output;
seconds_filter([Seconds | L], Output, SecondsNow, MaxPeriod) ->
    if
        (SecondsNow < Seconds) orelse
        ((SecondsNow - Seconds) > MaxPeriod) ->
            seconds_filter(L, Output, SecondsNow, MaxPeriod);
        true ->
            seconds_filter(L, [Seconds | Output], SecondsNow, MaxPeriod)
    end.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

