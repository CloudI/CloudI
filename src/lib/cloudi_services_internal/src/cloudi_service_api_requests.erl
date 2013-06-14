%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Service API Requests==
%%% A service that exposes dynamic configuration of CloudI.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2011-2013, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2011-2013 Michael Truog
%%% @version 1.2.4 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_api_requests).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloudi_service).

%% external interface

%% cloudi_service callbacks
-export([cloudi_service_init/3,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/2]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").

-record(state,
    {
        functions = cloudi_x_trie:new([
            {"acl_add",
             fun cloudi_service_api:acl_add/2},
            {"acl_remove",
             fun cloudi_service_api:acl_remove/2},
            {"services_add",
             fun cloudi_service_api:services_add/2},
            {"services_remove",
             fun cloudi_service_api:services_remove/2},
            {"services_restart",
             fun cloudi_service_api:services_restart/2},
            {"services",
             fun cloudi_service_api:services/1},
            {"nodes_add",
             fun cloudi_service_api:nodes_add/2},
            {"nodes_remove",
             fun cloudi_service_api:nodes_remove/2},
            {"nodes_alive",
             fun cloudi_service_api:alive/1},
            {"nodes_dead",
             fun cloudi_service_api:dead/1},
            {"nodes",
             fun cloudi_service_api:nodes/1},
            {"loglevel_set",
             fun cloudi_service_api:loglevel_set/2},
            {"log_redirect",
             fun cloudi_service_api:log_redirect/2},
            {"code_path_add",
             fun cloudi_service_api:code_path_add/2},
            {"code_path_remove",
             fun cloudi_service_api:code_path_remove/2},
            {"code_path",
             fun cloudi_service_api:code_path/1}
        ]),
        formats = cloudi_x_trie:new([
            {"erlang",
             fun format_erlang/4},
            {"json_rpc",
             fun format_json_rpc/4}
        ]),
        suffix_index = undefined
    }).
 
%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(_Args, Prefix, Dispatcher) ->
    % names are [prefix]format/[method] (i.e., request format)
    cloudi_service:subscribe(Dispatcher, "erlang/acl_add"),
    cloudi_service:subscribe(Dispatcher, "erlang/acl_add/post"),
    cloudi_service:subscribe(Dispatcher, "erlang/acl_remove"),
    cloudi_service:subscribe(Dispatcher, "erlang/acl_remove/post"),
    cloudi_service:subscribe(Dispatcher, "erlang/services_add"),
    cloudi_service:subscribe(Dispatcher, "erlang/services_add/post"),
    cloudi_service:subscribe(Dispatcher, "erlang/services_remove"),
    cloudi_service:subscribe(Dispatcher, "erlang/services_remove/post"),
    cloudi_service:subscribe(Dispatcher, "erlang/services_restart"),
    cloudi_service:subscribe(Dispatcher, "erlang/services_restart/post"),
    cloudi_service:subscribe(Dispatcher, "erlang/services"),
    cloudi_service:subscribe(Dispatcher, "erlang/services/get"),
    cloudi_service:subscribe(Dispatcher, "erlang/nodes_add"),
    cloudi_service:subscribe(Dispatcher, "erlang/nodes_add/post"),
    cloudi_service:subscribe(Dispatcher, "erlang/nodes_remove"),
    cloudi_service:subscribe(Dispatcher, "erlang/nodes_remove/post"),
    cloudi_service:subscribe(Dispatcher, "erlang/nodes_alive"),
    cloudi_service:subscribe(Dispatcher, "erlang/nodes_alive/get"),
    cloudi_service:subscribe(Dispatcher, "erlang/nodes_dead"),
    cloudi_service:subscribe(Dispatcher, "erlang/nodes_dead/get"),
    cloudi_service:subscribe(Dispatcher, "erlang/nodes"),
    cloudi_service:subscribe(Dispatcher, "erlang/nodes/get"),
    cloudi_service:subscribe(Dispatcher, "erlang/loglevel_set"),
    cloudi_service:subscribe(Dispatcher, "erlang/loglevel_set/post"),
    cloudi_service:subscribe(Dispatcher, "erlang/log_redirect"),
    cloudi_service:subscribe(Dispatcher, "erlang/log_redirect/post"),
    cloudi_service:subscribe(Dispatcher, "erlang/code_path_add"),
    cloudi_service:subscribe(Dispatcher, "erlang/code_path_add/post"),
    cloudi_service:subscribe(Dispatcher, "erlang/code_path_remove"),
    cloudi_service:subscribe(Dispatcher, "erlang/code_path_remove/post"),
    cloudi_service:subscribe(Dispatcher, "erlang/code_path"),
    cloudi_service:subscribe(Dispatcher, "erlang/code_path/get"),
    cloudi_service:subscribe(Dispatcher, "json_rpc/"),
    cloudi_service:subscribe(Dispatcher, "json_rpc//post"),
    {ok, #state{suffix_index = erlang:length(Prefix) + 1}}.

cloudi_service_handle_request(_Type, _Name, Pattern, _RequestInfo, Request,
                              Timeout, _Priority, _TransId, _Pid,
                              #state{suffix_index = SuffixIndex,
                                     functions = Functions,
                                     formats = Formats} = State, _Dispatcher) ->
    {Format, Suffix} = cloudi_string:splitl(
        $/, string:sub_string(Pattern, SuffixIndex)
    ),
    FunctionArity = case cloudi_string:beforel($/, Suffix, input) of
        [] ->
            undefined;
        Method ->
            cloudi_x_trie:fetch(Method, Functions)
    end,
    FormatF = cloudi_x_trie:fetch(Format, Formats),
    Response = FormatF(FunctionArity, Request, Timeout, Functions),
    {reply, cloudi_response:new(Request, Response), State}.

cloudi_service_handle_info(Request, State, _) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_service_terminate(_, #state{}) ->
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

format_erlang(F, Input, Timeout, _)
    when is_function(F) ->
    Arity = erlang:fun_info(F, arity),
    format_erlang_f(F, Arity, Input, Timeout).

format_erlang_f(F, 2, Input, Timeout) ->
    if
        is_binary(Input) ->
            case F(cloudi_string:binary_to_term(Input), Timeout) of
                Result when is_binary(Result) ->
                    Result;
                Result ->
                    cloudi_string:term_to_binary(Result)
            end;
        is_list(Input) ->
            case F(cloudi_string:list_to_term(Input), Timeout) of
                Result when is_binary(Result) ->
                    erlang:binary_to_list(Result);
                Result ->
                    cloudi_string:term_to_list(Result)
            end
    end;

format_erlang_f(F, 1, Input, Timeout) ->
    if
        is_binary(Input) ->
            case F(Timeout) of
                Result when is_binary(Result) ->
                    Result;
                Result ->
                    cloudi_string:term_to_binary(Result)
            end;
        is_list(Input) ->
            case F(Timeout) of
                Result when is_binary(Result) ->
                    erlang:binary_to_list(Result);
                Result ->
                    cloudi_string:term_to_list(Result)
            end
    end.

-spec format_json_rpc('undefined',
                      Input :: binary() | string(),
                      Timeout :: integer(),
                      Functions :: any()) -> binary().

format_json_rpc(undefined, Input, Timeout, Functions) ->
    {Method, Params, Id} = cloudi_json_rpc:request_to_term(Input),
    try (case cloudi_x_trie:fetch(erlang:binary_to_list(Method), Functions) of
        F when Params == [], is_function(F, 1) ->
            F(Timeout);
        F when length(Params) == 1, is_function(F, 2) ->
            F(cloudi_string:binary_to_term(erlang:hd(Params)), Timeout)
         end) of
        Result when is_binary(Result) ->
            cloudi_json_rpc:response_to_json(
                Result, Id
            );
        Result ->
            cloudi_json_rpc:response_to_json(
                cloudi_string:term_to_binary(Result), Id
            )
    catch
        _:Error ->
            cloudi_json_rpc:response_to_json(
                null, 0, cloudi_string:term_to_binary(Error), Id
            )
    end.

