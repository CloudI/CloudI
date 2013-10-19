%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI IP Address Parsing==
%%% Use a fixed width format for simpler parsing, testing, and to allow
%%% better pattern matching within service names.
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
%%% @version 1.3.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_ip_address).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([to_binary/1,
         to_string/1,
         from_binary/1,
         from_string/1]).

-type format_binary() ::
    <<_:120>> | <<_:312>>.
-type format_string() ::
    string().
-export_type([format_binary/0, format_string/0]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a binary string representation.===
%% @end
%%-------------------------------------------------------------------------

-spec to_binary(inet:ip_address()) -> format_binary().

to_binary({B1, B2, B3, B4}) ->
    cloudi_string:format_to_binary("~3..0b.~3..0b.~3..0b.~3..0b",
                                   [B1, B2, B3, B4]);
to_binary({S1, S2, S3, S4, S5, S6, S7, S8}) ->
    cloudi_string:format_to_binary("~4.16.0b:~4.16.0b:~4.16.0b:~4.16.0b:"
                                   "~4.16.0b:~4.16.0b:~4.16.0b:~4.16.0b",
                                   [S1, S2, S3, S4, S5, S6, S7, S8]).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a list string representation.===
%% @end
%%-------------------------------------------------------------------------

-spec to_string(inet:ip_address()) -> format_string().

to_string({B1, B2, B3, B4}) ->
    cloudi_string:format_to_list("~3..0b.~3..0b.~3..0b.~3..0b",
                                 [B1, B2, B3, B4]);
to_string({S1, S2, S3, S4, S5, S6, S7, S8}) ->
    cloudi_string:format_to_list("~4.16.0b:~4.16.0b:~4.16.0b:~4.16.0b:"
                                 "~4.16.0b:~4.16.0b:~4.16.0b:~4.16.0b",
                                 [S1, S2, S3, S4, S5, S6, S7, S8]).

%%-------------------------------------------------------------------------
%% @doc
%% ===Parse a binary string representation.===
%% @end
%%-------------------------------------------------------------------------

-spec from_binary(format_binary()) ->
    inet:ip_address().

from_binary(<<B1a, B1b, B1c, $., B2a, B2b, B2c, $.,
              B3a, B3b, B3c, $., B4a, B4b, B4c>>) ->
    {erlang:list_to_integer([B1a, B1b, B1c]),
     erlang:list_to_integer([B2a, B2b, B2c]),
     erlang:list_to_integer([B3a, B3b, B3c]),
     erlang:list_to_integer([B4a, B4b, B4c])};
from_binary(<<S1a, S1b, S1c, S1d, $:, S2a, S2b, S2c, S2d, $:,
              S3a, S3b, S3c, S3d, $:, S4a, S4b, S4c, S4d, $:,
              S5a, S5b, S5c, S5d, $:, S6a, S6b, S6c, S6d, $:,
              S7a, S7b, S7c, S7d, $:, S8a, S8b, S8c, S8d>>) ->
    {erlang:list_to_integer([S1a, S1b, S1c, S1d], 16),
     erlang:list_to_integer([S2a, S2b, S2c, S2d], 16),
     erlang:list_to_integer([S3a, S3b, S3c, S3d], 16),
     erlang:list_to_integer([S4a, S4b, S4c, S4d], 16),
     erlang:list_to_integer([S5a, S5b, S5c, S5d], 16),
     erlang:list_to_integer([S6a, S6b, S6c, S6d], 16),
     erlang:list_to_integer([S7a, S7b, S7c, S7d], 16),
     erlang:list_to_integer([S8a, S8b, S8c, S8d], 16)}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Parse a binary string representation.===
%% @end
%%-------------------------------------------------------------------------

-spec from_string(format_string()) ->
    inet:ip_address().

from_string([B1a, B1b, B1c, $., B2a, B2b, B2c, $.,
             B3a, B3b, B3c, $., B4a, B4b, B4c]) ->
    {erlang:list_to_integer([B1a, B1b, B1c]),
     erlang:list_to_integer([B2a, B2b, B2c]),
     erlang:list_to_integer([B3a, B3b, B3c]),
     erlang:list_to_integer([B4a, B4b, B4c])};
from_string([S1a, S1b, S1c, S1d, $:, S2a, S2b, S2c, S2d, $:,
             S3a, S3b, S3c, S3d, $:, S4a, S4b, S4c, S4d, $:,
             S5a, S5b, S5c, S5d, $:, S6a, S6b, S6c, S6d, $:,
             S7a, S7b, S7c, S7d, $:, S8a, S8b, S8c, S8d]) ->
    {erlang:list_to_integer([S1a, S1b, S1c, S1d], 16),
     erlang:list_to_integer([S2a, S2b, S2c, S2d], 16),
     erlang:list_to_integer([S3a, S3b, S3c, S3d], 16),
     erlang:list_to_integer([S4a, S4b, S4c, S4d], 16),
     erlang:list_to_integer([S5a, S5b, S5c, S5d], 16),
     erlang:list_to_integer([S6a, S6b, S6c, S6d], 16),
     erlang:list_to_integer([S7a, S7b, S7c, S7d], 16),
     erlang:list_to_integer([S8a, S8b, S8c, S8d], 16)}.

