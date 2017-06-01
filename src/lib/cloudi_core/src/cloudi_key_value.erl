%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Key/Value Data Access for RequestInfo==
%%% Keys are unique in any of the underlying representations.
%%% Used by the cloudi_request_info module.
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2014-2017 Michael Truog <mjtruog at gmail dot com>
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
%%% @author Michael Truog <mjtruog [at] gmail (dot) com>
%%% @copyright 2014-2017 Michael Truog
%%% @version 1.7.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_key_value).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([erase/2,
         find/2,
         store/3,
         to_list/1,
         to_dict/1,
         to_map/1]).

% used for accessing RequestInfo data
-type key() :: binary() | string() | atom().
-type value() :: binary() | iolist() | any().
-type key_values(Key, Value) :: #{Key := Value} |
                                list({Key, Value}) |
                                dict:dict(Key, Value).
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
%% @end
%%-------------------------------------------------------------------------

-spec erase(Key :: key(),
            KeyValues :: key_values()) ->
    NewKeyValues :: key_values().

erase(Key, KeyValues)
    when is_map(KeyValues) ->
    maps:remove(Key, KeyValues);
erase(Key, KeyValues)
    when is_list(KeyValues) ->
    lists:keydelete(Key, 1, KeyValues);
erase(Key, KeyValues) ->
    dict:erase(Key, KeyValues).

%%-------------------------------------------------------------------------
%% @doc
%% ===Generic key/value find.===
%% @end
%%-------------------------------------------------------------------------

-spec find(Key :: key(),
           KeyValues :: key_values()) ->
    {ok, Value :: value()} |
    error.

find(Key, KeyValues)
    when is_map(KeyValues) ->
    maps:find(Key, KeyValues);
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
%% @end
%%-------------------------------------------------------------------------

-spec store(Key :: key(),
            Value :: value(),
            KeyValues :: key_values()) ->
    NewKeyValues :: key_values().

store(Key, Value, KeyValues)
    when is_map(KeyValues) ->
    maps:put(Key, Value, KeyValues);
store(Key, Value, KeyValues)
    when is_list(KeyValues) ->
    lists:keystore(Key, 1, KeyValues, {Key, Value});
store(Key, Value, KeyValues) ->
    dict:store(Key, Value, KeyValues).

%%-------------------------------------------------------------------------
%% @doc
%% ===Generic key/value to_list.===
%% @end
%%-------------------------------------------------------------------------

-spec to_list(KeyValues :: key_values()) ->
    list({key(), value()}).

to_list(KeyValues)
    when is_map(KeyValues) ->
    maps:to_list(KeyValues);
to_list(KeyValues)
    when is_list(KeyValues) ->
    KeyValues;
to_list(KeyValues) ->
    dict:to_list(KeyValues).

%%-------------------------------------------------------------------------
%% @doc
%% ===Generic key/value to_dict.===
%% @end
%%-------------------------------------------------------------------------

-spec to_dict(KeyValues :: key_values()) ->
    dict:dict(key(), value()).

to_dict(KeyValues)
    when is_map(KeyValues) ->
    maps:fold(fun(K, V, D) ->
        dict:store(K, V, D)
    end, dict:new(), KeyValues);
to_dict(KeyValues)
    when is_list(KeyValues) ->
    lists:foldl(fun({K, V}, D) ->
        dict:store(K, V, D)
    end, dict:new(), KeyValues);
to_dict(KeyValues) ->
    KeyValues.

%%-------------------------------------------------------------------------
%% @doc
%% ===Generic key/value to_map.===
%% @end
%%-------------------------------------------------------------------------

-spec to_map(KeyValues :: key_values()) ->
    #{key() := value()}.

to_map(KeyValues)
    when is_map(KeyValues) ->
    KeyValues;
to_map(KeyValues)
    when is_list(KeyValues) ->
    lists:foldl(fun({K, V}, M) ->
        maps:put(K, V, M)
    end, #{}, KeyValues);
to_map(KeyValues) ->
    dict:fold(fun(K, V, M) ->
        maps:put(K, V, M)
    end, #{}, KeyValues).

