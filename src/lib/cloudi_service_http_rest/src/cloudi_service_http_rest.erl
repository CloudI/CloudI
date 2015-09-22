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

-define(DEFAULT_DEBUG,                      false). % log output for debugging
-define(DEFAULT_DEBUG_LEVEL,                trace).

-type method() :: 'GET' | 'POST' | 'PUT' | 'DELETE' | 'HEAD' | 'OPTIONS' |
                  'PATCH' | 'TRACE' | 'CONNECT'.

-type initialize_f() :: fun((Args :: list(),
                             Timeout :: cloudi_service_api:
                                        timeout_milliseconds(),
                             Dispatcher :: cloudi:dispatcher()) ->
    {ok, State :: any()} |
    {stop, Reason :: any()} |
    {stop, Reason :: any(), State :: any()}).
-type handler_f_11() :: fun((Method :: method(),
                             Path :: cloudi:service_name_pattern(),
                             Parameters :: list(string()),
                             Format :: atom(),
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
-type terminate_f() :: fun((Reason :: any(),
                            Timeout :: cloudi:timeout_value_milliseconds(),
                            State :: any()) ->
                           ok).
-export_type([method/0,
              initialize_f/0,
              handler_f_11/0,
              terminate_f/0]).

-record(api,
    {
        method :: method(),
        path :: cloudi:service_name_pattern(),
        parameters :: boolean(),
        format :: atom(),
        handler_f :: handler_f_11(),
        arity :: 11
    }).

-record(state,
    {
        prefix :: cloudi:service_name_pattern(),
        lookup :: cloudi_x_trie:cloudi_x_trie(),
        terminate_f :: terminate_f() | undefined,
        debug_level :: off | trace | debug | info | warn | error | fatal,
        api_state :: any()
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, Prefix, Timeout, Dispatcher) ->
    Defaults = [
        {initialize,               undefined},
        {terminate,                undefined},
        {handlers,                 undefined},
        {formats,                  undefined},
        {debug,                    ?DEFAULT_DEBUG},
        {debug_level,              ?DEFAULT_DEBUG_LEVEL}],
    [Initialize, Terminate0, Handlers, Formats0,
     Debug, DebugLevel | ArgsAPI] =
        cloudi_proplists:take_values(Defaults, Args),
    TerminateN = case Terminate0 of
        {TerminateModule, TerminateFunction}
            when is_atom(TerminateModule),
                 is_atom(TerminateFunction) ->
            true = erlang:function_exported(TerminateModule,
                                            TerminateFunction, 3),
            fun(TerminateArg1, TerminateArg2, TerminateArg3) ->
                TerminateModule:
                TerminateFunction(TerminateArg1, TerminateArg2,
                                  TerminateArg3)
            end;
        _ when is_function(Terminate0, 3) ->
            Terminate0;
        undefined ->
            undefined
    end,
    true = is_list(Handlers),
    true = is_list(Formats0),
    true = ((Debug =:= true) orelse
            (Debug =:= false)),
    true = ((DebugLevel =:= trace) orelse
            (DebugLevel =:= debug) orelse
            (DebugLevel =:= info) orelse
            (DebugLevel =:= warn) orelse
            (DebugLevel =:= error) orelse
            (DebugLevel =:= fatal)),
    DebugLogLevel = if
        Debug =:= false ->
            off;
        Debug =:= true ->
            DebugLevel
    end,
    ContentTypes = cloudi_response_info:lookup_content_type(),
    FormatsN = lists:map(fun(Format0) ->
        FormatN = if
            is_list(Format0), is_integer(hd(Format0)) ->
                Format0;
            is_atom(Format0) ->
                erlang:atom_to_list(Format0)
        end,
        true = cloudi_x_trie:is_key("." ++ FormatN, ContentTypes),
        FormatN
    end, Formats0),
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
                   handler_f = Handler1,
                   arity = Arity},
        subscribe_paths(Method, Path, FormatsN, API, Lookup0, Dispatcher)
    end, cloudi_x_trie:new(), Handlers),
    State = #state{prefix = Prefix,
                   lookup = LookupN,
                   terminate_f = TerminateN,
                   debug_level = DebugLogLevel},
    ReturnAPI = case Initialize of
        {InitializeModule, InitializeFunction}
            when is_atom(InitializeModule),
                 is_atom(InitializeFunction) ->
            true = erlang:function_exported(InitializeModule,
                                            InitializeFunction, 3),
            InitializeModule:
            InitializeFunction(ArgsAPI, Timeout, Dispatcher);
        _ when is_function(Initialize, 3) ->
            Initialize(ArgsAPI, Timeout, Dispatcher);
        undefined ->
            true = (ArgsAPI == []),
            {ok, undefined}
    end,
    case ReturnAPI of
        {ok, StateAPI} ->
            {ok, State#state{api_state = StateAPI}};
        {stop, Reason} ->
            {stop, Reason, State};
        {stop, Reason, StateAPI} ->
            {stop, Reason, State#state{api_state = StateAPI}}
    end.

cloudi_service_handle_request(_Type, Name, Pattern, RequestInfo, Request,
                              Timeout, Priority, TransId, _Pid,
                              #state{prefix = Prefix,
                                     lookup = Lookup,
                                     debug_level = DebugLevel,
                                     api_state = StateAPI} = State,
                              Dispatcher) ->
    Suffix = cloudi_service_name:suffix(Prefix, Pattern),
    #api{method = Method,
         path = Path,
         parameters = Parameters,
         format = Format,
         handler_f = Handler,
         arity = Arity} = cloudi_x_trie:fetch(Suffix, Lookup),
    ParametersL = if
        Parameters =:= true ->
            cloudi_service_name:parse(Name, Pattern);
        Parameters =:= false ->
            []
    end,
    if
        DebugLevel =:= off ->
            ok;
        RequestInfo /= <<>> ->
            protocol_debug_log(DebugLevel, "request ~p ~p",
                               [Name, {RequestInfo, Request}]);
        true ->
            protocol_debug_log(DebugLevel, "request ~p ~p",
                               [Name, Request])
    end,
    true = is_list(ParametersL),
    ReturnAPI = if
        Arity == 11 ->
            Handler(Method, Path, ParametersL, Format, RequestInfo, Request,
                    Timeout, Priority, TransId, StateAPI, Dispatcher)
    end,
    case ReturnAPI of
        {reply, Response, NewStateAPI} ->
            if
                DebugLevel =:= off ->
                    ok;
                true ->
                    protocol_debug_log(DebugLevel, "response ~p ~p",
                                       [Name, Response])
            end,
            {reply, Response,
             State#state{api_state = NewStateAPI}};
        {reply, ResponseInfo, Response, NewStateAPI} ->
            if
                DebugLevel =:= off ->
                    ok;
                true ->
                    protocol_debug_log(DebugLevel, "response ~p ~p",
                                       [Name, {ResponseInfo, Response}])
            end,
            {reply, ResponseInfo, Response,
             State#state{api_state = NewStateAPI}};
        {forward, NextName, NextRequestInfo, NextRequest, NewStateAPI} ->
            if
                DebugLevel =:= off ->
                    ok;
                true ->
                    protocol_debug_log(DebugLevel, "forward ~p to ~p ~p",
                                       [Name, NextName,
                                        {NextRequestInfo, NextRequest}])
            end,
            {forward, NextName, NextRequestInfo, NextRequest,
             State#state{api_state = NewStateAPI}};
        {forward, NextName, NextRequestInfo, NextRequest,
         NextTimeout, NextPriority, NewStateAPI} ->
            if
                DebugLevel =:= off ->
                    ok;
                true ->
                    protocol_debug_log(DebugLevel, "forward ~p to ~p ~p",
                                       [Name, NextName,
                                        {NextRequestInfo, NextRequest}])
            end,
            {forward, NextName, NextRequestInfo, NextRequest,
             NextTimeout, NextPriority,
             State#state{api_state = NewStateAPI}};
        {noreply, NewStateAPI} ->
            {noreply, State#state{api_state = NewStateAPI}};
        {stop, Reason, NewStateAPI} ->
            {stop, Reason, State#state{api_state = NewStateAPI}}
    end.

cloudi_service_handle_info(Request, State, _Dispatcher) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_service_terminate(Reason, Timeout,
                         #state{terminate_f = TerminateF,
                                api_state = StateAPI}) ->
    if
        TerminateF =:= undefined ->
            ok;
        true ->
            (catch TerminateF(Reason, Timeout, StateAPI))
    end,
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
    NewAPI = API#api{format = erlang:list_to_atom(Format)},
    NewLookup = cloudi_x_trie:store(Suffix, NewAPI, Lookup),
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

protocol_debug_log(trace, Message, Args) ->
    ?LOG_TRACE(Message, Args);
protocol_debug_log(debug, Message, Args) ->
    ?LOG_DEBUG(Message, Args);
protocol_debug_log(info, Message, Args) ->
    ?LOG_INFO(Message, Args);
protocol_debug_log(warn, Message, Args) ->
    ?LOG_WARN(Message, Args);
protocol_debug_log(error, Message, Args) ->
    ?LOG_ERROR(Message, Args);
protocol_debug_log(fatal, Message, Args) ->
    ?LOG_FATAL(Message, Args).
