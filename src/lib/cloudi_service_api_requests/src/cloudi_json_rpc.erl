%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI JSON RPC==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2011-2014, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2011-2014 Michael Truog
%%% @version 1.3.2 {@date} {@time}
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
    RPC0 = cloudi_x_jsx:json_to_term(DataBin),
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
            cloudi_x_jsx:term_to_json([{<<"method">>, MethodBin},
                                       {<<"id">>, Id},
                                       {<<"jsonrpc">>, <<"2.0">>}]);
        true ->
            ParamsBin = lists:map(fun(E) ->
                cloudi_string:term_to_binary(E)
            end, Params),
            cloudi_x_jsx:term_to_json([{<<"method">>, MethodBin},
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
    RPC0 = cloudi_x_jsx:json_to_term(DataBin),
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
    cloudi_x_jsx:term_to_json([{<<"result">>, Result},
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
    cloudi_x_jsx:term_to_json([{<<"result">>, Result},
                               {<<"error">>, [
                                 {<<"code">>, ErrorCode},
                                 {<<"message">>, ErrorMessage}
                                ]},
                               {<<"id">>, Id},
                               {<<"jsonrpc">>, Version}]).

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

