%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Key/Value Data Access for RequestInfo==
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

-module(cloudi_key_value).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([erase/2,
         find/2,
         store/3]).

% used for accessing RequestInfo data
-ifdef(ERLANG_OTP_VERSION_16).
-type key_values(Key, Value) :: list({Key, Value}) |
                                dict().
-else.
-type key_values(Key, Value) :: list({Key, Value}) |
                                dict:dict(Key, Value).
-endif.
-type key_values() :: key_values(binary() | string() | atom(),
                                 binary() | string() | any()).
-export_type([key_values/2,
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

-spec erase(Key :: any(),
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

-spec find(Key :: any(),
           KeyValues :: key_values()) ->
    {ok, Value :: any()} |
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

-spec store(Key :: any(),
            Value :: any(),
            KeyValues :: key_values()) ->
    NewKeyValues :: key_values().

store(Key, Value, KeyValues)
    when is_list(KeyValues) ->
    lists:keystore(Key, 1, KeyValues, {Key, Value});
store(Key, Value, KeyValues) ->
    dict:store(Key, Value, KeyValues).

