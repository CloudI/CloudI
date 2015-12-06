%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Key/Value Data Access for RequestInfo==
%%% Keys are unique in both the list and dict key_values data result.
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

-module(cloudi_key_value).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([erase/2,
         find/2,
         store/3,
         to_list/1,
         to_dict/1]).

-include("cloudi_core_i_constants.hrl").

% used for accessing RequestInfo data
-type key() :: binary() | string() | atom().
-type value() :: binary() | iolist() | any().
-type key_values(Key, Value) :: list({Key, Value}) |
                                dict_proxy(Key, Value).
-type key_values() :: key_values(key(), value()).
-export_type([key/0,
              value/0,
              key_values/2,
              key_values/0]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Generic key/value erase.===
%% RequestInfo's key/value result from request_info_key_value_parse/1
%% can be used here to erase request meta-data while encapsulating
%% the data structure used for the lookup.
%% @end
%%-------------------------------------------------------------------------

-spec erase(Key :: key(),
            KeyValues :: key_values()) ->
    NewKeyValues :: key_values().

erase(Key, KeyValues)
    when is_list(KeyValues) ->
    lists:keydelete(Key, 1, KeyValues);
erase(Key, KeyValues) ->
    dict:erase(Key, KeyValues).

%%-------------------------------------------------------------------------
%% @doc
%% ===Generic key/value find.===
%% RequestInfo's key/value result from request_info_key_value_parse/1
%% can be used here to access the request meta-data while encapsulating
%% the data structure used for the lookup.
%% @end
%%-------------------------------------------------------------------------

-spec find(Key :: key(),
           KeyValues :: key_values()) ->
    {ok, Value :: value()} |
    error.

find(Key, KeyValues)
    when is_list(KeyValues) ->
    case lists:keyfind(Key, 1, KeyValues) of
        {Key, Value} ->
            {ok, Value};
        false ->
            error
    end;
find(Key, KeyValues) ->
    dict:find(Key, KeyValues).

%%-------------------------------------------------------------------------
%% @doc
%% ===Generic key/value store.===
%% RequestInfo's key/value result from request_info_key_value_parse/1
%% can be used here to store request meta-data while encapsulating
%% the data structure used for the lookup.
%% @end
%%-------------------------------------------------------------------------

-spec store(Key :: key(),
            Value :: value(),
            KeyValues :: key_values()) ->
    NewKeyValues :: key_values().

store(Key, Value, KeyValues)
    when is_list(KeyValues) ->
    lists:keystore(Key, 1, KeyValues, {Key, Value});
store(Key, Value, KeyValues) ->
    dict:store(Key, Value, KeyValues).

%%-------------------------------------------------------------------------
%% @doc
%% ===Generic key/value to_list.===
%% RequestInfo's key/value result from request_info_key_value_parse/1
%% can be used here to erase request meta-data while encapsulating
%% the data structure used for the lookup.
%% @end
%%-------------------------------------------------------------------------

-spec to_list(KeyValues :: key_values()) ->
    list({key(), value()}).

to_list(KeyValues)
    when is_list(KeyValues) ->
    KeyValues;
to_list(KeyValues) ->
    dict:to_list(KeyValues).

%%-------------------------------------------------------------------------
%% @doc
%% ===Generic key/value to_dict.===
%% RequestInfo's key/value result from request_info_key_value_parse/1
%% can be used here to erase request meta-data while encapsulating
%% the data structure used for the lookup.
%% @end
%%-------------------------------------------------------------------------

-spec to_dict(KeyValues :: key_values()) ->
    dict_proxy(key(), value()).

to_dict(KeyValues)
    when is_list(KeyValues) ->
    lists:foldl(fun({K, V}, D) ->
        dict:store(K, V, D)
    end, dict:new(), KeyValues);
to_dict(KeyValues) ->
    KeyValues.

