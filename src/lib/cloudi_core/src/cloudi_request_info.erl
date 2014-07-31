%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI RequestInfo Creation and Parsing==
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
%%% @version 1.3.3 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_request_info).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([key_value_new/1,
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
    binary_key_value_new_list(RequestInfo, []);
key_value_new(RequestInfo) ->
    binary_key_value_new_list(dict:to_list(RequestInfo), []).

%%-------------------------------------------------------------------------
%% @doc
%% ===Parse RequestInfo key/value data.===
%% RequestInfo is meant to contain key/value pairs that is request
%% meta-data.
%% @end
%%-------------------------------------------------------------------------

-ifdef(ERLANG_OTP_VER_16).
-spec key_value_parse(RequestInfo :: binary() |
                                     list({any(), any()})) ->
    Result :: dict().
-else.
-spec key_value_parse(RequestInfo :: binary() |
                                     list({any(), any()})) ->
    Result :: dict:dict(any(), any()).
-endif.

key_value_parse(RequestInfo)
    when is_list(RequestInfo) ->
    % any() -> any()
    lists:foldl(fun({K, V}, D) ->
        dict:store(K, V, D)
    end, dict:new(), RequestInfo);
key_value_parse(RequestInfo)
    when is_binary(RequestInfo) ->
    % binary() -> binary()
    binary_key_value_parse_list(binary:split(RequestInfo, <<0>>, [global]),
                                dict:new()).

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

binary_key_value_new_list([], Result) ->
    erlang:iolist_to_binary(lists:reverse(Result));
binary_key_value_new_list([{K, V} | L], Result) ->
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
    binary_key_value_new_list(L, [[NewK, 0, NewV, 0] | Result]).

binary_key_value_parse_list([<<>>], Lookup) ->
    Lookup;
binary_key_value_parse_list([K, V | L], Lookup) ->
    case dict:find(K, Lookup) of
        {ok, [_ | _] = ListV} ->
            binary_key_value_parse_list(L, dict:store(K, ListV ++ [V], Lookup));
        {ok, V0} ->
            binary_key_value_parse_list(L, dict:store(K, [V0, V], Lookup));
        error ->
            binary_key_value_parse_list(L, dict:store(K, V, Lookup))
    end.

