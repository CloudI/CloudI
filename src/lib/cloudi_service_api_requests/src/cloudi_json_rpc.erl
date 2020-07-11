%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI JSON RPC==
%%% Implementation based on the version 2.0 of the JSON-RPC specification.
%%% Named parameters are not supported to ensure parameters always have
%%% an implicit order.  Batch requests are not supported.
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2011-2020 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2011-2020 Michael Truog
%%% @version 2.0.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_json_rpc).
-author('mjtruog at protonmail dot com').

%% external interface
-export([error_parsing/0,
         error_invalid_request/0,
         error_method_not_found/1,
         error_invalid_params/1,
         error_internal_error/1,
         request_to_term/1,
         request_to_json/2,
         request_to_json/3,
         response_to_term/1,
         response_to_json/2,
         response_to_json/4]).

-type method() :: binary().
-type params() :: list().
-type result() :: any().
-type error_code() :: integer().
-type error_message() :: binary().
-type id() :: binary() | integer() | null.
-export_type([method/0,
              params/0,
              result/0,
              error_code/0,
              error_message/0,
              id/0]).

-define(JSONRPC_VERSION, <<"2.0">>).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Invalid JSON was received by the server.===
%% @end
%%-------------------------------------------------------------------------

-spec error_parsing() ->
    binary().

error_parsing() ->
    response_to_json(null, -32700, <<"Parse error">>, null).

%%-------------------------------------------------------------------------
%% @doc
%% ===The JSON sent is not a valid Request object.===
%% @end
%%-------------------------------------------------------------------------

-spec error_invalid_request() ->
    binary().

error_invalid_request() ->
    response_to_json(null, -32600, <<"Invalid Request">>, null).

%%-------------------------------------------------------------------------
%% @doc
%% ===The method does not exist / is not available.===
%% @end
%%-------------------------------------------------------------------------

-spec error_method_not_found(Id :: id()) ->
    binary().

error_method_not_found(Id) ->
    response_to_json(null, -32601, <<"Method not found">>, Id).

%%-------------------------------------------------------------------------
%% @doc
%% ===Invalid method parameter(s).===
%% @end
%%-------------------------------------------------------------------------

-spec error_invalid_params(Id :: id()) ->
    binary().

error_invalid_params(Id) ->
    response_to_json(null, -32602, <<"Invalid params">>, Id).

%%-------------------------------------------------------------------------
%% @doc
%% ===Internal JSON-RPC error.===
%% @end
%%-------------------------------------------------------------------------

-spec error_internal_error(Id :: id()) ->
    binary().

error_internal_error(Id) ->
    response_to_json(null, -32603, <<"Internal error">>, Id).

%%-------------------------------------------------------------------------
%% @doc
%% ===Parse a JSON-RPC request to create Erlang terms.===
%% @end
%%-------------------------------------------------------------------------

-spec request_to_term(Data :: binary() | string()) ->
    {method(), params(), id()}.

request_to_term(Data) ->
    DataBin = if
        is_list(Data) ->
            erlang:list_to_binary(Data);
        is_binary(Data) ->
            Data
    end,
    RPC0 = cloudi_x_jsx:decode(DataBin, [{return_maps, false}]),
    {value, {_, Method}, RPCN} = lists:keytake(<<"method">>, 1, RPC0),
    true = is_binary(Method),
    Id = case lists:keyfind(<<"id">>, 1, RPCN) of
        {_, IdValue} ->
            IdValue;
        false ->
            null
    end,
    true = is_integer(Id) orelse is_binary(Id) orelse (Id =:= null),
    case lists:keyfind(<<"params">>, 1, RPCN) of
        {_, Params} when is_list(Params) ->
            {Method, Params, Id};
        false ->
            {Method, [], Id}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a JSON-RPC request in a JSON binary.===
%% @end
%%-------------------------------------------------------------------------

-spec request_to_json(Method :: atom() | string() | binary(),
                      Id :: id()) ->
    binary().

request_to_json(Method, Id) ->
    request_to_json(Method, [], Id).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a JSON-RPC request with parameters in a JSON binary.===
%% @end
%%-------------------------------------------------------------------------

-spec request_to_json(Method :: atom() | string() | binary(),
                      Params :: params(),
                      Id :: id()) ->
    binary().

request_to_json(Method, Params, Id)
    when is_list(Params) ->
    MethodBin = if
        is_atom(Method) ->
            erlang:atom_to_binary(Method, utf8);
        is_list(Method) ->
            erlang:list_to_binary(Method);
        is_binary(Method) ->
            Method
    end,
    if
        Params == [] ->
            cloudi_x_jsx:encode([{<<"jsonrpc">>, ?JSONRPC_VERSION},
                                 {<<"method">>, MethodBin},
                                 {<<"id">>, Id}]);
        true ->
            cloudi_x_jsx:encode([{<<"jsonrpc">>, ?JSONRPC_VERSION},
                                 {<<"method">>, MethodBin},
                                 {<<"params">>, params_to_binary(Params)},
                                 {<<"id">>, Id}])
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Parse a JSON-RPC response to create Erlang terms.===
%% @end
%%-------------------------------------------------------------------------

-spec response_to_term(Data :: binary() | string()) ->
    {result(), error_code() | null, error_message() | null, id()}.

response_to_term(Data) ->
    DataBin = if
        is_list(Data) ->
            erlang:list_to_binary(Data);
        is_binary(Data) ->
            Data
    end,
    RPC0 = cloudi_x_jsx:decode(DataBin, [{return_maps, false}]),
    Id = case lists:keyfind(<<"id">>, 1, RPC0) of
        {_, IdValue} ->
            IdValue;
        false ->
            null
    end,
    true = is_integer(Id) orelse is_binary(Id) orelse (Id =:= null),
    case lists:keyfind(<<"error">>, 1, RPC0) of
        false ->
            case lists:keyfind(<<"result">>, 1, RPC0) of
                {_, Result} ->
                    {Result, null, null, Id};
                false ->
                    {null, null, null, Id}
            end;
        {_, Error0} when is_list(Error0) ->
            {value,
             {_, ErrorCode},
             ErrorN} = lists:keytake(<<"code">>, 1, Error0),
            {_, ErrorMessage} = lists:keyfind(<<"message">>, 1, ErrorN),
            true = is_integer(ErrorCode) andalso is_binary(ErrorMessage),
            {null, ErrorCode, ErrorMessage, Id}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a JSON-RPC response in a JSON binary.===
%% @end
%%-------------------------------------------------------------------------

-spec response_to_json(Result :: result(),
                       Id :: id()) ->
    binary().

response_to_json(Result, Id) ->
    response_to_json(Result, null, null, Id).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a JSON-RPC error response in a JSON binary.===
%% @end
%%-------------------------------------------------------------------------

-spec response_to_json(Result :: result() | null,
                       ErrorCode :: error_code() | null,
                       ErrorMessage :: error_message() | null,
                       Id :: id()) ->
    binary().

response_to_json(null, ErrorCode, ErrorMessage, Id)
    when is_integer(ErrorCode), is_binary(ErrorMessage) ->
    cloudi_x_jsx:encode([{<<"jsonrpc">>, ?JSONRPC_VERSION},
                         {<<"error">>,
                          [{<<"code">>, ErrorCode},
                           {<<"message">>, ErrorMessage}]},
                         {<<"id">>, Id}]);
response_to_json(Result, null, null, Id) ->
    cloudi_x_jsx:encode([{<<"jsonrpc">>, ?JSONRPC_VERSION},
                         {<<"result">>, Result},
                         {<<"id">>, Id}]).

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

params_to_binary([]) ->
    [];
params_to_binary([Param | Params]) ->
    ParamNew = if
        is_number(Param); is_binary(Param); Param =:= null ->
            Param;
        true ->
            cloudi_string:term_to_binary_compact(Param)
    end,
    [ParamNew | params_to_binary(Params)].

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-include("cloudi_service_api_requests_test.hrl").

module_test_() ->
    {timeout, ?TEST_TIMEOUT, [
        {"specification tests", ?_assertOk(t_specification())}
    ]}.

t_specification() ->
    % test data based on the JSON-RPC specification at
    % https://www.jsonrpc.org/specification
    Request1 = <<"{\"jsonrpc\":\"2.0\",\"method\":\"subtract\",\"params\":[42,23],\"id\":1}">>,
    Response1 = <<"{\"jsonrpc\":\"2.0\",\"result\":19,\"id\":1}">>,
    {<<"subtract">>, [Params1X, Params1Y], 1} = request_to_term(Request1),
    19 = Result1 = Params1X - Params1Y,
    {Result1, null, null, 1} = response_to_term(Response1),
    Request1 = request_to_json("subtract", [42,23], 1),
    Response1 = response_to_json(19, 1),
    Request2 = <<"{\"jsonrpc\":\"2.0\",\"method\":\"subtract\",\"params\":[23,42],\"id\":2}">>,
    Response2 = <<"{\"jsonrpc\":\"2.0\",\"result\":-19,\"id\":2}">>,
    {<<"subtract">>, [Params2X, Params2Y], 2} = request_to_term(Request2),
    -19 = Result2 = Params2X - Params2Y,
    {Result2, null, null, 2} = response_to_term(Response2),
    Request2 = request_to_json(<<"subtract">>, [23,42], 2),
    Response2 = response_to_json(-19, 2),

    Notification1 = <<"{\"jsonrpc\":\"2.0\",\"method\":\"update\",\"params\":[1,2,3,4,5]}">>,
    Notification2 = <<"{\"jsonrpc\":\"2.0\",\"method\":\"foobar\"}">>,
    {<<"update">>, [1,2,3,4,5], null} = request_to_term(Notification1),
    {<<"foobar">>, [], null} = request_to_term(Notification2),

    ResponseError1 = <<"{\"jsonrpc\":\"2.0\",\"error\":{\"code\":-32601,\"message\":\"Method not found\"},\"id\":\"1\"}">>,
    ResponseError2 = <<"{\"jsonrpc\":\"2.0\",\"error\":{\"code\":-32700,\"message\":\"Parse error\"},\"id\":null}">>,
    ResponseError3 = <<"{\"jsonrpc\":\"2.0\",\"error\":{\"code\":-32600,\"message\":\"Invalid Request\"},\"id\":null}">>,
    {null, -32601, <<"Method not found">>,
     <<"1">>} = response_to_term(ResponseError1),
    ResponseError1 = error_method_not_found(<<"1">>),
    {null, -32700, <<"Parse error">>,
     null} = response_to_term(ResponseError2),
    ResponseError2 = error_parsing(),
    {null, -32600, <<"Invalid Request">>,
     null} = response_to_term(ResponseError3),
    ResponseError3 = error_invalid_request(),
    ok.

-endif.

