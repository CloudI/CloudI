%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Quick Random Number Generation==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2012-2013, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2012-2013 Michael Truog
%%% @version 1.2.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(quickrand).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([seed/0,
         uniform/1,
         strong_uniform/1]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Seed random number generation.===
%% @end
%%-------------------------------------------------------------------------

-spec seed() ->
    'ok'.

seed() ->
    % to provide better seeding than erlang:now() or os:timestamp()
    <<B1:32/unsigned-integer,
      B2:32/unsigned-integer,
      B3:32/unsigned-integer,
      B4:32/unsigned-integer>> = try crypto:strong_rand_bytes(16)
    catch
        error:low_entropy ->
            error_logger:info_msg("quickrand: low_entropy!~n"),
            crypto:rand_bytes(16)
    end,
    random:seed(B1, B2, B3),
    random_wh06_int:seed(B1, B2, B3, B4),
    ok.

%%-------------------------------------------------------------------------
%% @doc
%% ===Quick uniform random number generation.===
%% Not meant for cryptographic purposes.
%% @end
%%-------------------------------------------------------------------------

-spec uniform(N :: 1..21267638781707063560975648195455661513) ->
    1..21267638781707063560975648195455661513.

uniform(N) when is_integer(N), N < 1 ->
    erlang:exit(badarg);

uniform(1) ->
    1;

uniform(N) when is_integer(N), N < 1000000 ->
    % os:timestamp/0 is currently the quickest source of uniform randomness
    {_, _, MicroSecs} = os:timestamp(),
    (MicroSecs rem N) + 1;

uniform(N) when is_integer(N), N =< 27817185604309 ->
    % 27817185604309 == 30269 * 30307 * 30323
    random:uniform(N);

uniform(N) when is_integer(N), N =< 21267638781707063560975648195455661513 ->
    % 21267638781707063560975648195455661513 ==
    %   2147483579 * 2147483543 * 2147483423 * 2147483123
    random_wh06_int:uniform(N).

%%-------------------------------------------------------------------------
%% @doc
%% ===Strong uniform random number generation.===
%% @end
%%-------------------------------------------------------------------------

-spec strong_uniform(N :: pos_integer()) ->
    pos_integer().

strong_uniform(N) when is_integer(N), N < 1 ->
    erlang:exit(badarg);

strong_uniform(1) ->
    1;

strong_uniform(N) when is_integer(N), N > 1 ->
    Bytes = erlang:byte_size(binary:encode_unsigned(N)),
    (binary:decode_unsigned(crypto:strong_rand_bytes(Bytes), big) rem N) + 1.

