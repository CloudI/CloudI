%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Erlang Term Info==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2014-2023 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2014-2023 Michael Truog
%%% @version 2.0.8 {@date} {@time}
%%%------------------------------------------------------------------------

-module(erlang_term).
-author('mjtruog at protonmail dot com').

%% external interface
-export([byte_size/1,
         byte_size/2]).

-compile({no_auto_import,
          [byte_size/1,
           byte_size/2]}).

-define(HEAP_BINARY_LIMIT, 64).

-ifdef(ERLANG_OTP_VERSION_16).
-else.
-define(ERLANG_OTP_VERSION_17_FEATURES, undefined).
-ifdef(OTP_RELEASE). % Erlang/OTP >= 21.0
-if(?OTP_RELEASE >= 25).
-define(ERLANG_OTP_VERSION_25_FEATURES, true).
-endif.
-endif.
-endif.
-ifdef(ERLANG_OTP_VERSION_17_FEATURES).
-define(BYTE_SIZE_TERMS_MAP,
    ;
byte_size_terms(Term)
    when is_map(Term) ->
    maps:fold(fun(K, V, Bytes) ->
        byte_size_terms(K) + byte_size_terms(V) + Bytes
    end, 0, Term)
    ;).
-define(INTERNAL_TEST_MAP,
    88 = byte_size(#{1=>1, 2=>2, 3=>3}, 8),
    136 = byte_size(#{1=>RefcBinary, 2=>2, 3=>3}, 8) -
          erlang:byte_size(RefcBinary),
    ).
-else.
-define(BYTE_SIZE_TERMS_MAP,
    ;).
-define(INTERNAL_TEST_MAP,
    ok,
    ).
-endif.
-ifdef(ERLANG_OTP_VERSION_25_FEATURES).
-define(INTERNAL_TEST_EXTERNAL_FUNCTION,
    72 = byte_size({fun module:function/0, []}, 8),
    ).
-define(INTERNAL_TEST_TUPLE_EMPTY,
    8 = byte_size({}, 8),
    32 = byte_size(#{}, 8),
    ).
-else.
-define(INTERNAL_TEST_EXTERNAL_FUNCTION,
    48 = byte_size({fun module:function/0, []}, 8),
    ).
-ifdef(ERLANG_OTP_VERSION_17_FEATURES).
-define(INTERNAL_TEST_TUPLE_EMPTY,
    16 = byte_size({}, 8),
    40 = byte_size(#{}, 8),
    ).
-else.
-define(INTERNAL_TEST_TUPLE_EMPTY,
    16 = byte_size({}, 8),
    ).
-endif.
-endif.

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

byte_size(Term) ->
    byte_size(Term, erlang:system_info(wordsize)).

byte_size(Term, WordSize) ->
    byte_size_term_local(Term, WordSize) +
    byte_size_terms(Term).

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

byte_size_terms(Term)
    when is_list(Term) ->
    byte_size_terms_in_list(Term);
byte_size_terms(Term)
    when is_tuple(Term) ->
    if
        Term == {} ->
            0;
        true ->
            byte_size_terms_in_tuple(1, erlang:tuple_size(Term), Term)
    end
?BYTE_SIZE_TERMS_MAP
byte_size_terms(Term) ->
    byte_size_term(Term).

byte_size_terms_in_list([]) ->
    0;
byte_size_terms_in_list([Term | L]) ->
    byte_size_terms(Term) +
    byte_size_terms_in_list(L);
byte_size_terms_in_list(Term) ->
    byte_size_terms(Term). % element of improper list

byte_size_terms_in_tuple(Size, Size, Term) ->
    byte_size_terms(erlang:element(Size, Term));
byte_size_terms_in_tuple(I, Size, Term) ->
    byte_size_terms(erlang:element(I, Term)) +
    byte_size_terms_in_tuple(I + 1, Size, Term).

byte_size_term(Term) ->
    byte_size_term_global(Term).

byte_size_term_local(Term, WordSize) ->
    % stack/register size + heap size
    (1 + erts_debug:flat_size(Term)) * WordSize.

byte_size_term_global(Term)
    when is_binary(Term) ->
    % global data storage within allocators
    BinarySize = erlang:byte_size(Term),
    if
        BinarySize > ?HEAP_BINARY_LIMIT ->
            % refc binary
            BinarySize;
        true ->
            % heap binary
            0
    end;
byte_size_term_global(_) ->
    0.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-ifdef(CLOUDI_TEST_TIMEOUT).
-define(TEST_TIMEOUT, ?CLOUDI_TEST_TIMEOUT). % seconds
-else.
-define(TEST_TIMEOUT, 10). % seconds
-endif.

module_test_() ->
    {timeout, ?TEST_TIMEOUT, [
        {"internal tests", ?_assertEqual(ok, t_basic())}
    ]}.

t_basic() ->
    RefcBinary = <<1:((?HEAP_BINARY_LIMIT + 1) * 8)>>,
    HeapBinary = <<1:(?HEAP_BINARY_LIMIT * 8)>>,
    true = (7 == (1 + erts_debug:flat_size(RefcBinary))),
    % doesn't work in console shell
    % (process heap size of binary is excluded
    %  when executed in the console shell)
    true = (11 == (1 + erts_debug:flat_size(HeapBinary))),
    true = (4 == (1 + erts_debug:flat_size(<<1:8>>))),

    24 = byte_size(<<>>, 8),
    32 = byte_size(<<"abc">>, 8),
    32 = byte_size(<<$a, $b, $c>>, 8),
    8 = byte_size([], 8),
    24 = byte_size([0|[]], 8),
    24 = byte_size([1|2], 8), % improper list
    24 = byte_size({0}, 8),
    32 = byte_size({0,1}, 8),
    8 = byte_size(0, 8),
    8 = byte_size(erlang:self(), 8),
    8 = byte_size(atom, 8),
    40 = byte_size({module, function, []}, 8),
    ?INTERNAL_TEST_EXTERNAL_FUNCTION
    ?INTERNAL_TEST_TUPLE_EMPTY
    ?INTERNAL_TEST_MAP
    ok.

-endif.

