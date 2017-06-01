%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI JSON RPC==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2011-2017 Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2011-2017 Michael Truog
%%% @version 1.7.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_json_rpc).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([request_to_term/1,
         request_to_json/2,
         request_to_json/3,
         response_to_term/1,
         response_to_json/2,
         response_to_json/4]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

-spec request_to_term(Data :: binary() | string()) ->
    {binary(), list(binary()), integer()}.

request_to_term(Data) ->
    DataBin = if
        is_list(Data) ->
            erlang:list_to_binary(Data);
        is_binary(Data) ->
            Data
    end,
    RPC0 = cloudi_x_jsx:decode(DataBin),
    {value, {_, MethodBin}, RPC1} = lists:keytake(<<"method">>, 1, RPC0),
    {value, {_, Id}, RPC2} = lists:keytake(<<"id">>, 1, RPC1),
    case lists:keytake(<<"params">>, 1, RPC2) of
        {value, {_, ParamsBin}, _} ->
            {MethodBin, ParamsBin, Id};
        false ->
            {MethodBin, [], Id}
    end.

-spec request_to_json(Method :: atom() | string() | binary(),
                      Id :: integer()) ->
    binary().

request_to_json(Method, Id) ->
    request_to_json(Method, [], Id).

-spec request_to_json(Method :: atom() | string() | binary(),
                      Params :: list(),
                      Id :: integer()) ->
    binary().

request_to_json(Method, Params, Id)
    when is_list(Params), is_integer(Id) ->
    MethodBin = if
        is_atom(Method) ->
            erlang:list_to_binary(erlang:atom_to_list(Method));
        is_list(Method) ->
            erlang:list_to_binary(Method);
        is_binary(Method) ->
            Method
    end,
    if
        Params == [] ->
            cloudi_x_jsx:encode([{<<"method">>, MethodBin},
                                 {<<"id">>, Id},
                                 {<<"jsonrpc">>, <<"2.0">>}]);
        true ->
            ParamsBin = lists:map(fun(E) ->
                cloudi_string:term_to_binary(E)
            end, Params),
            cloudi_x_jsx:encode([{<<"method">>, MethodBin},
                                 {<<"params">>, ParamsBin},
                                 {<<"id">>, Id},
                                 {<<"jsonrpc">>, <<"2.0">>}])
    end.

-spec response_to_term(Data :: binary() | string()) ->
    {binary(), integer() | 'null', binary() | 'null', binary() | integer()}.

response_to_term(Data) ->
    DataBin = if
        is_list(Data) ->
            erlang:list_to_binary(Data);
        is_binary(Data) ->
            Data
    end,
    RPC0 = cloudi_x_jsx:decode(DataBin),
    {value, {_, Result}, RPC1} = lists:keytake(<<"result">>, 1, RPC0),
    {value, {_, Error}, RPC2} = lists:keytake(<<"error">>, 1, RPC1),
    {value, {_, Id}, _} = lists:keytake(<<"id">>, 1, RPC2),
    if
        is_list(Error) ->
            {value, {_, ErrorCode}, Error1} = lists:keytake(<<"code">>,
                                                            1, Error),
            {value, {_, ErrorMessage}, _} = lists:keytake(<<"message">>,
                                                          1, Error1),
            {Result, ErrorCode, ErrorMessage, Id};
        true ->
            {Result, null, null, Id}
    end.

-spec response_to_json(Result :: binary(),
                       Id :: integer()) ->
    binary().

response_to_json(Result, Id) ->
    response_to_json(Result, null, null, Id).

-spec response_to_json(Result :: binary() | 'null',
                       ErrorCode :: integer() | 'null',
                       ErrorMessage :: binary() | 'null',
                       Id :: binary() | integer()) ->
    binary().

response_to_json(Result, null, null, Id) ->
    Version = if
        is_integer(Id) ->
            <<"2.0">>;
        is_binary(Id) ->
            <<"1.1">>
    end,
    cloudi_x_jsx:encode([{<<"result">>, Result},
                         {<<"error">>, null},
                         {<<"id">>, Id},
                         {<<"jsonrpc">>, Version}]);

response_to_json(Result, ErrorCode, ErrorMessage, Id)
    when is_integer(ErrorCode), is_binary(ErrorMessage) ->
    Version = if
        is_integer(Id) ->
            <<"2.0">>;
        is_binary(Id) ->
            <<"1.1">>
    end,
    cloudi_x_jsx:encode([{<<"result">>, Result},
                         {<<"error">>,
                          [{<<"code">>, ErrorCode},
                           {<<"message">>, ErrorMessage}]},
                         {<<"id">>, Id},
                         {<<"jsonrpc">>, Version}]).

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

