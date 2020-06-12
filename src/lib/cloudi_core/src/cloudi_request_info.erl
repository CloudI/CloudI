%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI RequestInfo Creation and Parsing==
%%% RequestInfo is used for request meta-data which is normally
%%% key/value pairs that describe the context of the service request
%%% (e.g., HTTP header names and values for a HTTP request).  The default
%%% encoding provided below is a basic format for textual key/value data
%%% (i.e., neither the key or value should contain a null character, '\0')
%%% which is easily parsed in any programming language and is referred to as
%%% the 'text_pairs' format.  It is valid to have multiple entries for the
%%% same key within the RequestInfo data.  A key must be of size 1 or greater
%%% (`<<>>' will never exist as a key in text_pairs data).
%%%
%%% Use the 'binary_pairs' format if any of the key/value data is binary
%%% data (i.e., if any of the key or value data contains
%%% null characters, '\0').
%%%
%%% The ResponseInfo data is normally service request response meta-data
%%% (providing the response equivalent of RequestInfo for a request)
%%% and can utilize the same functions below.
%%%
%%% These module functions provide Erlang serialization of the 'text_pairs'
%%% format and the 'binary_pairs' format for use with the
%%% cloudi_key_value module.
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2014-2020 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2014-2020 Michael Truog
%%% @version 2.0.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_request_info).
-author('mjtruog at protonmail dot com').

%% external interface
-export([key_value_new/1,
         key_value_new/2,
         key_value_append/2,
         key_value_parse/1,
         key_value_parse/2]).

-type format() :: text_pairs | binary_pairs.
-export_type([format/0]).

-include("cloudi_core_i_constants.hrl").

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===New RequestInfo key/value data.===
%% RequestInfo is meant to contain key/value pairs that is request
%% meta-data.  Create the binary RequestInfo data with any supported
%% data structure.
%% @end
%%-------------------------------------------------------------------------

-spec key_value_new(RequestInfo :: cloudi_key_value:key_values()) ->
    Result :: binary().

key_value_new(RequestInfo) ->
    key_value_new(RequestInfo, text_pairs).

%%-------------------------------------------------------------------------
%% @doc
%% ===New RequestInfo key/value data.===
%% RequestInfo is meant to contain key/value pairs that is request
%% meta-data.  Create the binary RequestInfo data with any supported
%% data structure.
%% @end
%%-------------------------------------------------------------------------

-spec key_value_new(RequestInfo :: cloudi_key_value:key_values(),
                    Format :: format()) ->
    Result :: binary().

key_value_new(RequestInfo, text_pairs) ->
    text_pairs_list_to_binary(cloudi_key_value:to_list(RequestInfo));
key_value_new(RequestInfo, binary_pairs) ->
    binary_pairs_list_to_binary(cloudi_key_value:to_list(RequestInfo)).

%%-------------------------------------------------------------------------
%% @doc
%% ===Append RequestInfo key/value data.===
%% Use the same binary format.
%% @end
%%-------------------------------------------------------------------------

-spec key_value_append(RequestInfo :: cloudi_key_value:key_values(),
                       Existing :: binary()) ->
    Result :: binary().

key_value_append(RequestInfo, <<>>) ->
    key_value_new(RequestInfo);
key_value_append(RequestInfo, <<0:8, _/binary>> = BinaryPairs) ->
    <<0:8, Suffix/binary>> = key_value_new(RequestInfo, binary_pairs),
    <<BinaryPairs/binary, Suffix/binary>>;
key_value_append(RequestInfo, TextPairs)
    when is_binary(TextPairs) ->
    Suffix = key_value_new(RequestInfo, text_pairs),
    <<TextPairs/binary, Suffix/binary>>.

%%-------------------------------------------------------------------------
%% @doc
%% ===Parse RequestInfo key/value data.===
%% RequestInfo is meant to contain key/value pairs that is request
%% meta-data.
%% @end
%%-------------------------------------------------------------------------

-spec key_value_parse(RequestInfo :: binary() |
                                     cloudi_key_value:key_values()) ->
    Result :: #{cloudi_key_value:key() := cloudi_key_value:value()}.

key_value_parse(<<0:8, _/binary>> = RequestInfo) ->
    binary_pairs_binary_to_map(RequestInfo);
key_value_parse(RequestInfo)
    when is_binary(RequestInfo) ->
    text_pairs_binary_to_map(RequestInfo);
key_value_parse(RequestInfo) ->
    cloudi_key_value:to_map(RequestInfo).

%%-------------------------------------------------------------------------
%% @doc
%% ===Parse RequestInfo key/value data to the return type specified.===
%% RequestInfo is meant to contain key/value pairs that is request
%% meta-data.
%% @end
%%-------------------------------------------------------------------------

-spec key_value_parse(RequestInfo :: binary() |
                                     cloudi_key_value:key_values(),
                      ResultType :: map | list) ->
    Result :: #{cloudi_key_value:key() := cloudi_key_value:value()} |
              list({cloudi_key_value:key(), cloudi_key_value:value()}).

key_value_parse(RequestInfo, map) ->
    key_value_parse(RequestInfo);
key_value_parse(RequestInfo, list) ->
    key_value_parse_to_list(RequestInfo).

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

key_value_parse_to_list(<<0:8, _/binary>> = RequestInfo) ->
    binary_pairs_binary_to_list(RequestInfo);
key_value_parse_to_list(RequestInfo)
    when is_binary(RequestInfo) ->
    text_pairs_binary_to_list(RequestInfo);
key_value_parse_to_list(RequestInfo) ->
    cloudi_key_value:to_list(RequestInfo).

text_pairs_list_to_binary_element([] = L) ->
    L;
text_pairs_list_to_binary_element([{K, V} | L]) ->
    BinaryK = key_to_binary(K),
    % a text_pairs key must be of size 1 or greater
    <<TextPairs:8, _/binary>> = BinaryK,
    true = TextPairs /= 0,
    BinaryV = value_to_binary(V),
    [[BinaryK, 0, BinaryV, 0] | text_pairs_list_to_binary_element(L)].

text_pairs_list_to_binary(L) ->
    erlang:iolist_to_binary(text_pairs_list_to_binary_element(L)).

text_pairs_binary_to_map_element([<<>>], Lookup) ->
    Lookup;
text_pairs_binary_to_map_element([K, V | L], Lookup) ->
    case maps:find(K, Lookup) of
        {ok, [_ | _] = ListV} ->
            NewLookup = maps:put(K, ListV ++ [V], Lookup),
            text_pairs_binary_to_map_element(L, NewLookup);
        {ok, PreviousV} when is_binary(PreviousV) ->
            NewLookup = maps:put(K, [PreviousV, V], Lookup),
            text_pairs_binary_to_map_element(L, NewLookup);
        error ->
            text_pairs_binary_to_map_element(L, maps:put(K, V, Lookup))
    end.

text_pairs_binary_to_map(RequestInfo) ->
    L = binary:split(RequestInfo, <<0>>, [global]),
    text_pairs_binary_to_map_element(L, #{}).

text_pairs_binary_to_list_element([<<>>]) ->
    [];
text_pairs_binary_to_list_element([K, V | L]) ->
    [{K, V} | text_pairs_binary_to_list_element(L)].

text_pairs_binary_to_list(RequestInfo) ->
    L = binary:split(RequestInfo, <<0>>, [global]),
    text_pairs_binary_to_list_element(L).

binary_pairs_list_to_binary_element([] = L) ->
    L;
binary_pairs_list_to_binary_element([{K, V} | L]) ->
    BinaryK = key_to_binary(K),
    SizeK = byte_size(BinaryK),
    true = SizeK > 0,
    BinaryV = value_to_binary(V),
    SizeV = byte_size(BinaryV),
    [[<<SizeK:32/big-unsigned-integer>>, BinaryK,
      <<SizeV:32/big-unsigned-integer>>, BinaryV] |
     binary_pairs_list_to_binary_element(L)].

binary_pairs_list_to_binary(L) ->
    erlang:iolist_to_binary([0 | binary_pairs_list_to_binary_element(L)]).

binary_pairs_binary_to_map_element(<<>>, Lookup) ->
    Lookup;
binary_pairs_binary_to_map_element(B0, Lookup) ->
    <<SizeK:32/big-unsigned-integer, B1/binary>> = B0,
    <<K:SizeK/binary-unit:8, B2/binary>> = B1,
    <<SizeV:32/big-unsigned-integer, B3/binary>> = B2,
    <<V:SizeV/binary-unit:8, BN/binary>> = B3,
    case maps:find(K, Lookup) of
        {ok, [_ | _] = ListV} ->
            NewLookup = maps:put(K, ListV ++ [V], Lookup),
            binary_pairs_binary_to_map_element(BN, NewLookup);
        {ok, PreviousV} when is_binary(PreviousV) ->
            NewLookup = maps:put(K, [PreviousV, V], Lookup),
            binary_pairs_binary_to_map_element(BN, NewLookup);
        error ->
            binary_pairs_binary_to_map_element(BN, maps:put(K, V, Lookup))
    end.

binary_pairs_binary_to_map(<<0:8, BinaryPairs/binary>>) ->
    binary_pairs_binary_to_map_element(BinaryPairs, #{}).

binary_pairs_binary_to_list_element(<<>>, L) ->
    lists:reverse(L);
binary_pairs_binary_to_list_element(B0, L) ->
    <<SizeK:32/big-unsigned-integer, B1/binary>> = B0,
    <<K:SizeK/binary-unit:8, B2/binary>> = B1,
    <<SizeV:32/big-unsigned-integer, B3/binary>> = B2,
    <<V:SizeV/binary-unit:8, BN/binary>> = B3,
    binary_pairs_binary_to_list_element(BN, [{K, V} | L]).

binary_pairs_binary_to_list(<<0:8, BinaryPairs/binary>>) ->
    binary_pairs_binary_to_list_element(BinaryPairs, []).

key_to_binary(K)
    when is_binary(K) ->
    K;
key_to_binary([H | _] = K)
    when is_integer(H) ->
    erlang:list_to_binary(K);
key_to_binary(K)
    when is_atom(K) ->
    erlang:atom_to_binary(K, utf8).

value_to_binary(V)
    when is_binary(V) ->
    V;
value_to_binary(V)
    when is_list(V) ->
    erlang:iolist_to_binary(V);
value_to_binary(V)
    when is_atom(V) ->
    erlang:atom_to_binary(V, utf8);
value_to_binary(V) ->
    cloudi_string:term_to_binary_compact(V).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-include("cloudi_core_i_test.hrl").

module_test_() ->
    {timeout, ?TEST_TIMEOUT, [
        {"text_pairs tests", ?_assertOk(t_text_pairs())},
        {"binary_pairs tests", ?_assertOk(t_binary_pairs())}
    ]}.

t_text_pairs() ->
    <<>> = key_value_new([]),
    <<>> = key_value_new([], text_pairs),
    0 = maps:size(key_value_parse(<<>>)),
    KeyValues0 = [{atom_key, atom_value},
                  {atom_key, "list_value"},
                  {atom_key, <<"binary_value">>},
                  {"list_key", "list_value"},
                  {"list_key", <<"binary_value">>},
                  {<<"binary_key">>, <<"binary_value">>}],
    TextPairs0 = <<"atom_key", 0, "atom_value", 0,
                   "atom_key", 0, "list_value", 0,
                   "atom_key", 0, "binary_value", 0,
                   "list_key", 0, "list_value", 0,
                   "list_key", 0, "binary_value", 0,
                   "binary_key", 0, "binary_value", 0>>,
    TextPairs0 = key_value_new(KeyValues0, text_pairs),
    KeyValues1 = key_value_parse(TextPairs0),
    true = KeyValues1 == #{<<"atom_key">> => [<<"atom_value">>,
                                              <<"list_value">>,
                                              <<"binary_value">>],
                           <<"binary_key">> => <<"binary_value">>,
                           <<"list_key">> => [<<"list_value">>,
                                              <<"binary_value">>]},
    TextPairs1 = <<"binary_key", 0, "binary_value", 0>>,
    TextPairs2 = key_value_append(KeyValues0, TextPairs1),
    KeyValues2 = key_value_parse(TextPairs2),
    true = KeyValues2 == #{<<"atom_key">> => [<<"atom_value">>,
                                              <<"list_value">>,
                                              <<"binary_value">>],
                           <<"binary_key">> => [<<"binary_value">>,
                                                <<"binary_value">>],
                           <<"list_key">> => [<<"list_value">>,
                                              <<"binary_value">>]},
    KeyValues3 = key_value_parse(TextPairs0, list),
    true = KeyValues3 == [{<<"atom_key">>, <<"atom_value">>},
                          {<<"atom_key">>, <<"list_value">>},
                          {<<"atom_key">>, <<"binary_value">>},
                          {<<"list_key">>, <<"list_value">>},
                          {<<"list_key">>, <<"binary_value">>},
                          {<<"binary_key">>, <<"binary_value">>}],
    ok.

t_binary_pairs() ->
    <<0>> = key_value_new([], binary_pairs),
    0 = maps:size(key_value_parse(<<0>>)),
    KeyValues0 = [{atom_key, atom_value},
                  {atom_key, "list_value"},
                  {atom_key, <<"binary_value">>},
                  {"list_key", "list_value"},
                  {"list_key", <<"binary_value">>},
                  {<<"binary_key">>, <<"binary_value">>}],
    BinaryPairs0 = <<0,
                     8:32/big-unsigned-integer, "atom_key",
                     10:32/big-unsigned-integer, "atom_value",
                     8:32/big-unsigned-integer, "atom_key",
                     10:32/big-unsigned-integer, "list_value",
                     8:32/big-unsigned-integer, "atom_key",
                     12:32/big-unsigned-integer, "binary_value",
                     8:32/big-unsigned-integer, "list_key",
                     10:32/big-unsigned-integer, "list_value",
                     8:32/big-unsigned-integer, "list_key",
                     12:32/big-unsigned-integer, "binary_value",
                     10:32/big-unsigned-integer, "binary_key",
                     12:32/big-unsigned-integer, "binary_value">>,
    BinaryPairs0 = key_value_new(KeyValues0, binary_pairs),
    KeyValues1 = key_value_parse(BinaryPairs0),
    true = KeyValues1 == #{<<"atom_key">> => [<<"atom_value">>,
                                              <<"list_value">>,
                                              <<"binary_value">>],
                           <<"binary_key">> => <<"binary_value">>,
                           <<"list_key">> => [<<"list_value">>,
                                              <<"binary_value">>]},
    BinaryPairs1 = <<0,
                     10:32/big-unsigned-integer, "binary_key",
                     12:32/big-unsigned-integer, "binary_value">>,
    BinaryPairs2 = key_value_append(KeyValues0, BinaryPairs1),
    KeyValues2 = key_value_parse(BinaryPairs2),
    true = KeyValues2 == #{<<"atom_key">> => [<<"atom_value">>,
                                              <<"list_value">>,
                                              <<"binary_value">>],
                           <<"binary_key">> => [<<"binary_value">>,
                                                <<"binary_value">>],
                           <<"list_key">> => [<<"list_value">>,
                                              <<"binary_value">>]},
    KeyValues3 = key_value_parse(BinaryPairs0, list),
    true = KeyValues3 == [{<<"atom_key">>, <<"atom_value">>},
                          {<<"atom_key">>, <<"list_value">>},
                          {<<"atom_key">>, <<"binary_value">>},
                          {<<"list_key">>, <<"list_value">>},
                          {<<"list_key">>, <<"binary_value">>},
                          {<<"binary_key">>, <<"binary_value">>}],
    ok.

-endif.

