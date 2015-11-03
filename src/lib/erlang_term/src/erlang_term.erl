%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Erlang Term Info==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2014, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2014 Michael Truog
%%% @version 0.1.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(erlang_term).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([byte_size/1,
         byte_size/2]).

-compile({no_auto_import,
          [byte_size/1,
           byte_size/2]}).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

byte_size(Term) ->
    byte_size(Term, erlang:system_info(wordsize)).

byte_size(Term, WordSize) ->
    byte_size_terms(Term, WordSize).

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

byte_size_terms(Term, WordSize)
    when is_list(Term) ->
    1 * WordSize +
    byte_size_terms_in_list(Term, WordSize);
byte_size_terms({}, WordSize) ->
    2 * WordSize;
byte_size_terms(Term, WordSize)
    when is_tuple(Term) ->
    2 * WordSize +
    byte_size_terms_in_tuple(1, erlang:tuple_size(Term), Term, WordSize);
byte_size_terms(Term, WordSize) ->
    byte_size_term(Term, WordSize).

byte_size_terms_in_list([], _) ->
    0;
byte_size_terms_in_list([Term | L], WordSize) ->
    1 * WordSize +
    byte_size_terms(Term, WordSize) +
    byte_size_terms_in_list(L, WordSize);
byte_size_terms_in_list(Term, WordSize) ->
    byte_size_terms(Term, WordSize). % element of improper list

byte_size_terms_in_tuple(Size, Size, Term, WordSize) ->
    byte_size_terms(erlang:element(Size, Term), WordSize);
byte_size_terms_in_tuple(I, Size, Term, WordSize) ->
    byte_size_terms(erlang:element(I, Term), WordSize) +
    byte_size_terms_in_tuple(I + 1, Size, Term, WordSize).

byte_size_term(Term, WordSize) ->
    DataSize = if
        is_binary(Term) ->
            BinarySize = erlang:byte_size(Term),
            if
                BinarySize > 64 ->
                    BinarySize;
                true ->
                    % in the heap size
                    0
            end;
        true ->
            0
    end,
    % stack/register size + heap size + data size
    (1 + erts_debug:flat_size(Term)) * WordSize + DataSize.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

internal_test() ->
    true = (7 == (1 + erts_debug:flat_size(<<1:520>>))),
    % doesn't work in console shell
    % (process heap size of binary is excluded
    %  when executed in the console shell)
    true = (11 == (1 + erts_debug:flat_size(<<1:512>>))),
    true = (4 == (1 + erts_debug:flat_size(<<1:8>>))),

    24 = byte_size(<<>>, 8),
    32 = byte_size(<<"abc">>, 8),
    32 = byte_size(<<$a, $b, $c>>, 8),
    8 = byte_size([], 8),
    24 = byte_size([0], 8),
    32 = byte_size([1|2], 8), % improper list
    16 = byte_size({}, 8),
    24 = byte_size({0}, 8),
    8 = byte_size(0, 8),
    8 = byte_size(erlang:self(), 8),
    8 = byte_size(atom, 8),
    ok.

-endif.

