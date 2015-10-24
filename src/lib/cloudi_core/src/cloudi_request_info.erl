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
%%% same key within the RequestInfo data.
%%%
%%% The ResponseInfo data is normally service request response meta-data
%%% (providing the response equivalent of RequestInfo for a request)
%%% and can utilize the same functions below.
%%%
%%% These module functions provide Erlang serialization of the 'text_pairs'
%%% format for use with the cloudi_key_value module.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2014-2015, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2014-2015 Michael Truog
%%% @version 1.5.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_request_info).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([key_value_new/1,
         key_value_append/2,
         key_value_parse/1]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===New RequestInfo key/value data.===
%% RequestInfo is meant to contain key/value pairs that is request
%% meta-data.  Create the binary RequestInfo data with a list of pairs or
%% a dict data structure.
%% @end
%%-------------------------------------------------------------------------

-spec key_value_new(RequestInfo :: cloudi_key_value:key_values()) ->
    Result :: binary().

key_value_new([]) ->
    <<>>;
key_value_new([{_, _} | _] = RequestInfo) ->
    text_pairs_new_list(RequestInfo, []);
key_value_new(RequestInfo) ->
    text_pairs_new_list(dict:to_list(RequestInfo), []).

%%-------------------------------------------------------------------------
%% @doc
%% ===Append RequestInfo key/value data.===
%% Use the same binary format.
%% @end
%%-------------------------------------------------------------------------

-spec key_value_append(RequestInfo :: cloudi_key_value:key_values(),
                       Existing :: binary()) ->
    Result :: binary().

key_value_append(RequestInfo, Existing)
    when is_binary(Existing) ->
    Suffix = key_value_new(RequestInfo),
    <<Existing/binary, Suffix/binary>>.

%%-------------------------------------------------------------------------
%% @doc
%% ===Parse RequestInfo key/value data.===
%% RequestInfo is meant to contain key/value pairs that is request
%% meta-data.
%% @end
%%-------------------------------------------------------------------------

-ifdef(ERLANG_OTP_VERSION_16).
-spec key_value_parse(RequestInfo :: binary() |
                                     list({any(), any()}) |
                                     dict()) ->
    Result :: dict().
-else.
-spec key_value_parse(RequestInfo :: binary() |
                                     list({any(), any()}) |
                                     dict:dict(any(), any())) ->
    Result :: dict:dict(any(), any()).
-endif.

key_value_parse(RequestInfo)
    when is_binary(RequestInfo) ->
    % key/values: binary() -> binary()
    text_pairs_parse_list(binary:split(RequestInfo, <<0>>, [global]),
                                dict:new());
key_value_parse(RequestInfo)
    when is_list(RequestInfo) ->
    % key/values: any() -> any()
    lists:foldl(fun({K, V}, D) ->
        dict:store(K, V, D)
    end, dict:new(), RequestInfo);
key_value_parse(RequestInfo) ->
    % key/values: any() -> any()
    RequestInfo.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

text_pairs_new_list([], Result) ->
    erlang:iolist_to_binary(lists:reverse(Result));
text_pairs_new_list([{K, V} | L], Result) ->
    NewK = if
        is_binary(K) ->
            K;
        is_list(K), is_integer(hd(K)) ->
            erlang:list_to_binary(K);
        is_atom(K) ->
            erlang:atom_to_binary(K, utf8)
    end,
    NewV = if
        is_binary(V) ->
            V;
        is_list(V), is_integer(hd(V)) ->
            erlang:list_to_binary(V);
        is_atom(V) ->
            erlang:atom_to_binary(V, utf8);
        true ->
            cloudi_string:term_to_binary(V)
    end,
    text_pairs_new_list(L, [[NewK, 0, NewV, 0] | Result]).

text_pairs_parse_list([<<>>], Lookup) ->
    Lookup;
text_pairs_parse_list([K, V | L], Lookup) ->
    case dict:find(K, Lookup) of
        {ok, [_ | _] = ListV} ->
            text_pairs_parse_list(L, dict:store(K, ListV ++ [V], Lookup));
        {ok, V0} ->
            text_pairs_parse_list(L, dict:store(K, [V0, V], Lookup));
        error ->
            text_pairs_parse_list(L, dict:store(K, V, Lookup))
    end.

