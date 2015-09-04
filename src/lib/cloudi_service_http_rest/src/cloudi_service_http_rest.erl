%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI HTTP REST Integration==
%%% Provide an easy way of connecting CloudI service requests to
%%% Erlang function calls for a HTTP REST API, with service configuration
%%% arguments.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2015, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2015 Michael Truog
%%% @version 1.5.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_http_rest).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloudi_service).

%% cloudi_service callbacks
-export([cloudi_service_init/4,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/3]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").

-type method() :: 'GET' | 'POST' | 'PUT' | 'DELETE' | 'HEAD' | 'OPTIONS' |
                  'PATCH' | 'TRACE' | 'CONNECT'.

-type handler_f_11() :: fun((Method :: method(),
                             Path :: cloudi:service_name_pattern(),
                             Parameters :: list(string()),
                             Format :: string(),
                             RequestInfo :: any(),
                             Request :: any(),
                             Timeout :: cloudi:timeout_value_milliseconds(),
                             Priority :: cloudi:priority_value(),
                             TransId :: cloudi:trans_id(),
                             State :: any(),
                             Dispatcher :: cloudi:dispatcher()) ->
    {reply, Response :: any(), NewState :: any()} |
    {reply, ResponseInfo :: any(), Response :: any(), NewState :: any()} |
    {forward, NextName :: cloudi:service_name(),
     NextRequestInfo :: any(), NextRequest :: any(), NewState :: any()} |
    {forward, NextName :: cloudi:service_name(),
     NextRequestInfo :: any(), NextRequest :: any(),
     NextTimeout :: cloudi:timeout_value_milliseconds(),
     NextPriority :: cloudi:priority_value(), NewState :: any()} |
    {noreply, NewState :: any()} |
    {stop, Reason :: any(), NewState :: any()}).

-record(api,
    {
        method :: method(),
        path :: cloudi:service_name_pattern(),
        parameters :: boolean(),
        format :: string(),
        handler :: handler_f_11(),
        arity :: 11
    }).

-record(state,
    {
        prefix :: cloudi:service_name_pattern(),
        lookup :: cloudi_x_trie:cloudi_x_trie()
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, Prefix, _Timeout, Dispatcher) ->
    Defaults = [
        {handlers,                 undefined},
        {formats,                  undefined}],
    [Handlers, Formats] =
        cloudi_proplists:take_values(Defaults, Args),
    true = is_list(Handlers),
    true = is_list(Formats),
    ContentTypes = cloudi_response_info:lookup_content_type(),
    lists:foreach(fun(Format) ->
        true = cloudi_x_trie:is_key("." ++ Format, ContentTypes)
    end, Formats),
    LookupN = lists:foldl(fun({Method, Path, Handler0}, Lookup0) ->
        {Handler1, Arity} = case Handler0 of
            {HandlerModule, HandlerFunction}
                when is_atom(HandlerModule),
                     is_atom(HandlerFunction) ->
                true = erlang:function_exported(HandlerModule,
                                                HandlerFunction, 11),
                {fun(HandlerArg1, HandlerArg2, HandlerArg3,
                     HandlerArg4, HandlerArg5, HandlerArg6,
                     HandlerArg7, HandlerArg8, HandlerArg9,
                     HandlerArg10, HandlerArg11) ->
                     HandlerModule:
                     HandlerFunction(HandlerArg1, HandlerArg2, HandlerArg3,
                                     HandlerArg4, HandlerArg5, HandlerArg6,
                                     HandlerArg7, HandlerArg8, HandlerArg9,
                                     HandlerArg10, HandlerArg11)
                 end, 11};
            _ when is_function(Handler0, 11) ->
                {Handler0, 11}
        end,
        API = #api{method = Method,
                   path = Path,
                   parameters = lists:member($*, Prefix ++ Path),
                   handler = Handler1,
                   arity = Arity},
        subscribe_paths(Method, Path, Formats, API, Lookup0, Dispatcher)
    end, cloudi_x_trie:new(), Handlers),
    {ok, #state{prefix = Prefix,
                lookup = LookupN}}.

cloudi_service_handle_request(_Type, Name, Pattern, RequestInfo, Request,
                              Timeout, Priority, TransId, _Pid,
                              #state{prefix = Prefix,
                                     lookup = Lookup} = State, Dispatcher) ->
    Suffix = cloudi_service_name:suffix(Prefix, Pattern),
    #api{method = Method,
         path = Path,
         parameters = Parameters,
         format = Format,
         handler = Handler,
         arity = Arity} = cloudi_x_trie:fetch(Suffix, Lookup),
    ParametersL = if
        Parameters =:= true ->
            cloudi_service_name:parse(Name, Pattern);
        Parameters =:= false ->
            []
    end,
    if
        Arity == 11 ->
            Handler(Method, Path, ParametersL, Format, RequestInfo, Request,
                    Timeout, Priority, TransId, State, Dispatcher)
    end.

cloudi_service_handle_info(Request, State, _Dispatcher) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_service_terminate(_Reason, _Timeout, #state{}) ->
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

subscribe_path([], _, _, _, Lookup, _) ->
    Lookup;
subscribe_path([Format | Formats], Path, MethodSuffix, API,
               Lookup, Dispatcher) ->
    Suffix = Path ++ "." ++ Format ++ MethodSuffix,
    ok = cloudi_service:subscribe(Dispatcher, Suffix),
    NewLookup = cloudi_x_trie:store(Suffix, API#api{format = Format}, Lookup),
    subscribe_path(Formats, Path, MethodSuffix, API,
                   NewLookup, Dispatcher).

subscribe_paths('GET', Path, Formats, API, Lookup, Dispatcher) ->
    subscribe_path(Formats, Path, "/get", API, Lookup, Dispatcher);
subscribe_paths('POST', Path, Formats, API, Lookup, Dispatcher) ->
    subscribe_path(Formats, Path, "/post", API, Lookup, Dispatcher);
subscribe_paths('PUT', Path, Formats, API, Lookup, Dispatcher) ->
    subscribe_path(Formats, Path, "/put", API, Lookup, Dispatcher);
subscribe_paths('DELETE', Path, Formats, API, Lookup, Dispatcher) ->
    subscribe_path(Formats, Path, "/delete", API, Lookup, Dispatcher);
subscribe_paths('HEAD', Path, Formats, API, Lookup, Dispatcher) ->
    subscribe_path(Formats, Path, "/head", API, Lookup, Dispatcher);
subscribe_paths('OPTIONS', Path, Formats, API, Lookup, Dispatcher) ->
    subscribe_path(Formats, Path, "/options", API, Lookup, Dispatcher);
subscribe_paths('PATCH', Path, Formats, API, Lookup, Dispatcher) ->
    subscribe_path(Formats, Path, "/patch", API, Lookup, Dispatcher);
subscribe_paths('TRACE', Path, Formats, API, Lookup, Dispatcher) ->
    subscribe_path(Formats, Path, "/trace", API, Lookup, Dispatcher);
subscribe_paths('CONNECT', Path, Formats, API, Lookup, Dispatcher) ->
    subscribe_path(Formats, Path, "/connect", API, Lookup, Dispatcher).

