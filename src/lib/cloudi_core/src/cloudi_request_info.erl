%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI RequestInfo Creation and Parsing==
%%% RequestInfo is used for request meta-data which is normally
%%% key/value pairs that describe the context of the service request
%%% (e.g., HTTP header names and values for a HTTP request).  The encoding
%%% provided below is a basic format for textual key/value data
%%% (i.e., neither the key or value should contain a null character, '\0')
%%% which is easily parsed in any programming language and is referred to as
%%% the 'text_pairs' format.  It is valid to have multiple entries for the
%%% same key within the RequestInfo data.  A key must be of size 1 or greater
%%% (`<<>>' will never exist as a key in text_pairs data).
%%%
%%% The ResponseInfo data is normally service request response meta-data
%%% (providing the response equivalent of RequestInfo for a request)
%%% and can utilize the same functions below.
%%%
%%% These module functions provide Erlang serialization of the 'text_pairs'
%%% format for use with the cloudi_key_value module.
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2014-2017 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2014-2017 Michael Truog
%%% @version 1.7.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_request_info).
-author('mjtruog at protonmail dot com').

%% external interface
-export([key_value_new/1,
         key_value_append/2,
         key_value_parse/1]).

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
    text_pairs_list_to_binary(cloudi_key_value:to_list(RequestInfo)).

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
key_value_append(RequestInfo, <<TextPairs:8, _/binary>> = Existing)
    when TextPairs /= 0 ->
    Suffix = key_value_new(RequestInfo),
    <<Existing/binary, Suffix/binary>>.

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

key_value_parse(RequestInfo)
    when is_binary(RequestInfo) ->
    text_pairs_binary_to_map(RequestInfo);
key_value_parse(RequestInfo) ->
    cloudi_key_value:to_map(RequestInfo).

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

text_pairs_key_to_binary(K)
    when is_binary(K) ->
    K;
text_pairs_key_to_binary([H | _] = K)
    when is_integer(H) ->
    erlang:list_to_binary(K);
text_pairs_key_to_binary(K)
    when is_atom(K) ->
    erlang:atom_to_binary(K, utf8).

text_pairs_value_to_binary(V)
    when is_binary(V) ->
    V;
text_pairs_value_to_binary(V)
    when is_list(V) ->
    erlang:iolist_to_binary(V);
text_pairs_value_to_binary(V)
    when is_atom(V) ->
    erlang:atom_to_binary(V, utf8);
text_pairs_value_to_binary(V) ->
    cloudi_string:term_to_binary(V).

text_pairs_list_to_binary_element([] = L) ->
    L;
text_pairs_list_to_binary_element([{K, V} | L]) ->
    BinaryK = text_pairs_key_to_binary(K),
    % a text_pairs key must be of size 1 or greater
    <<TextPairs:8, _/binary>> = BinaryK,
    true = TextPairs /= 0,
    BinaryV = text_pairs_value_to_binary(V),
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

