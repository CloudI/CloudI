%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Service API Requests==
%%% A service that exposes dynamic configuration of CloudI.
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2011-2018 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2011-2018 Michael Truog
%%% @version 1.7.4 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_api_requests).
-author('mjtruog at protonmail dot com').

-behaviour(cloudi_service).

%% external interface

%% cloudi_service callbacks
-export([cloudi_service_init/4,
         cloudi_service_handle_request/11,
         cloudi_service_terminate/3]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").

-record(state,
    {
        functions :: cloudi_x_trie:cloudi_x_trie(), % name -> {method,arity}
        prefix :: cloudi:service_name_pattern()
    }).

-define(FORMAT_ERLANG, "erl").
 
%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init([], Prefix, _Timeout, Dispatcher) ->
    Exports = cloudi_x_reltool_util:module_exports(cloudi_service_api),
    CloudIServiceAPI = lists:foldl(fun({Method, Arity} = F, Functions) ->
        % service names are (prefix)rpc/(method)(format-extension)
        % (content-type hint as a file extension)
        FormatSuffix = if
            Arity == 1 ->
                "/get";
            Arity == 2 ->
                "/post"
        end,
        MethodName = erlang:atom_to_list(Method),
        Format = ?FORMAT_ERLANG,
        FormatName = "rpc/" ++ MethodName ++ "." ++ Format,
        cloudi_service:subscribe(Dispatcher, FormatName),
        cloudi_service:subscribe(Dispatcher, FormatName ++ FormatSuffix),
        cloudi_x_trie:store(MethodName, F, Functions)
    end, cloudi_x_trie:new(), Exports),
    % service names for JSON-RPC are:
    cloudi_service:subscribe(Dispatcher, "rpc.json"),
    cloudi_service:subscribe(Dispatcher, "rpc.json/post"),
    {ok, #state{functions = CloudIServiceAPI,
                prefix = Prefix}}.

cloudi_service_handle_request(_Type, Name, _Pattern, _RequestInfo, Request,
                              Timeout, _Priority, _TransId, _Pid,
                              #state{prefix = Prefix} = State, _Dispatcher) ->
    Response = case cloudi_service_name:suffix(Prefix, Name) of
        "rpc/" ++ MethodSuffix ->
            format_rpc(cloudi_string:beforel($/, MethodSuffix, input),
                       Request, Timeout, State);
        "rpc.json" ++ _ ->
            format_json_rpc(Request, Timeout, State)
    end,
    {reply, Response, State}.

cloudi_service_terminate(_Reason, _Timeout, _State) ->
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

format_rpc(MethodFormat, Request, Timeout,
           #state{functions = Functions}) ->
    {MethodName, Format} = cloudi_string:splitr($., MethodFormat),
    {Method, Arity} = cloudi_x_trie:fetch(MethodName, Functions),
    case Format of
        ?FORMAT_ERLANG ->
            format_erlang_call(Method, Arity, Request, Timeout)
    end.

format_erlang_call(_, _, Request, _)
    when not is_binary(Request) ->
    <<>>;
format_erlang_call(Method, 1, _, Timeout) ->
    try cloudi_service_api_call(Method, Timeout) of
        Result ->
            cloudi_service_api_result(Result, erlang_string)
    catch
        ErrorType:Error ->
            ?LOG_DEBUG("~p ~p", [ErrorType, Error]),
            <<>>
    end;
format_erlang_call(Method, 2, Request, Timeout) ->
    Arg1 = cloudi_string:binary_to_term(Request),
    try cloudi_service_api_call(Method, Arg1, Timeout) of
        Result ->
            cloudi_service_api_result(Result, erlang_string)
    catch
        ErrorType:Error ->
            ?LOG_DEBUG("~p ~p", [ErrorType, Error]),
            <<>>
    end.

-spec format_json_rpc(Request :: any(),
                      Timeout :: cloudi:timeout_value_milliseconds(),
                      State :: #state{}) -> binary().

format_json_rpc(Request, _, _)
    when not is_binary(Request) ->
    cloudi_json_rpc:response_to_json(<<"not_binary">>, 0);
format_json_rpc(Request, Timeout,
                #state{functions = Functions}) ->
    try cloudi_json_rpc:request_to_term(Request) of
        {MethodNameBin, Params, Id} ->
            MethodName = erlang:binary_to_list(MethodNameBin),
            case cloudi_x_trie:find(MethodName, Functions) of
                error ->
                    cloudi_json_rpc:response_to_json(null, 1,
                                                     <<"invalid_method">>, Id);
                {ok, {Method, 1}} when Params == [] ->
                    format_json_rpc_call(Method, 1, undefined, Timeout, Id);
                {ok, {Method, 2}} when length(Params) == 1 ->
                    format_json_rpc_call(Method, 2, hd(Params), Timeout, Id);
                {ok, _} ->
                    cloudi_json_rpc:response_to_json(null, 2,
                                                     <<"invalid_params">>, Id)
            end
    catch
        _:_ ->
            cloudi_json_rpc:response_to_json(null, 0,
                                             <<"invalid_json_rpc">>, 0)
    end.

format_json_rpc_call(Method, 1, _, Timeout, Id) ->
    try cloudi_service_api_call(Method, Timeout) of
        Result ->
            cloudi_json_rpc:response_to_json(
                cloudi_service_api_result(Result, erlang_string), Id)
    catch
        ErrorType:Error ->
            ?LOG_DEBUG("~p ~p", [ErrorType, Error]),
            <<>>
    end;
format_json_rpc_call(_, 2, Param, _, Id)
    when not is_binary(Param) ->
    cloudi_json_rpc:response_to_json(null, 3,
                                     <<"invalid_param">>, Id);
format_json_rpc_call(Method, 2, Param, Timeout, Id) ->
    Arg1 = cloudi_string:binary_to_term(Param),
    try cloudi_service_api_call(Method, Arg1, Timeout) of
        Result ->
            cloudi_json_rpc:response_to_json(
                cloudi_service_api_result(Result, erlang_string), Id)
    catch
        ErrorType:Error ->
            ?LOG_DEBUG("~p ~p", [ErrorType, Error]),
            <<>>
    end.

cloudi_service_api_call(services = Method, Timeout) ->
    case cloudi_service_api:Method(Timeout) of
        {ok, L} ->
            {ok, [{service_id(UUID), Data} ||
                  {UUID, Data} <- L]};
        {error, _} = Error ->
            Error
    end;
cloudi_service_api_call(Method, Timeout) ->
    cloudi_service_api:Method(Timeout).

cloudi_service_api_call(Method, Input, Timeout)
    when Method =:= services_search;
         Method =:= services_status ->
    case cloudi_service_api:Method(Input, Timeout) of
        {ok, L} ->
            {ok, [{service_id(UUID), Data} ||
                  {UUID, Data} <- L]};
        {error, _} = Error ->
            Error
    end;
cloudi_service_api_call(services_add = Method, Input, Timeout) ->
    case cloudi_service_api:Method(Input, Timeout) of
        {ok, L} ->
            {ok, [service_id(UUID) || UUID <- L]};
        {error, _} = Error ->
            Error
    end;
cloudi_service_api_call(services_update = Method, Input, Timeout) ->
    case cloudi_service_api:Method(Input, Timeout) of
        {ok, SuccessSet} ->
            {ok, [[service_id(UUID) || UUID <- L] || L <- SuccessSet]};
        {error, {ErrorL, Reason}, SuccessSet} ->
            {error,
             {[service_id(UUID) || UUID <- ErrorL], Reason},
             [[service_id(UUID) || UUID <- L] || L <- SuccessSet]};
        {error, _} = Error ->
            Error
    end;
cloudi_service_api_call(Method, Input, Timeout) ->
    cloudi_service_api:Method(Input, Timeout).

cloudi_service_api_result({ok, Result}, Format) ->
    response(Result, Format);
cloudi_service_api_result(Result, Format) ->
    response(Result, Format).

response(Result, erlang_string) ->
    cloudi_string:format_to_binary("~p", [Result]).

service_id(ID) ->
    cloudi_x_uuid:uuid_to_string(ID, list_nodash).
