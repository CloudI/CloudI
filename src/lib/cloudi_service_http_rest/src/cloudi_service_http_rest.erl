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

-define(DEFAULT_INITIALIZE,             undefined). % see below:
        % If necessary, provide an initialization function to be called
        % for initializing REST handler state data.  The initialize
        % function can be specified as an anonymous function or a
        % {module(), FunctionName :: atom()} tuple.
-define(DEFAULT_TERMINATE,              undefined). % see below:
        % If necessary, provide a terminate function to be called
        % for terminating the REST handler's state data.  The terminate
        % function can be specified as an anonymous function or a
        % {module(), FunctionName :: atom()} tuple.
-define(DEFAULT_HANDLERS,               undefined). % see below:
        % Provide a list of handler functions to be used with the
        % service name prefix.  Each handler function entry in the list
        % takes the form:
        % {Method :: atom() | string(), Path :: string(), handler()}
        % (e.g., Method == 'GET', Path == "index.html")
        % The handler function can be specified as an anonymous function or a
        % {module(), FunctionName :: atom()} tuple.
-define(DEFAULT_INFO,                   undefined). % see below:
        % If necessary, provide an info function to be called
        % for Erlang messages received by the process. The info
        % function can be specified as an anonymous function or a
        % {module(), FunctionName :: atom()} tuple.
-define(DEFAULT_FORMATS,                undefined). % see below:
        % Provide a list of formats to handle that are added as file type
        % suffixes on the URL path which also determine the content-type used
        % for the request and response.
-define(DEFAULT_USE_OPTIONS_METHOD,         false).
        % Provide default handling of the OPTIONS method based on the
        % configured handlers.
-define(DEFAULT_USE_TRACE_METHOD,           false).
        % Provide default handling of the TRACE method based on the
        % configured handlers.
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
-type info_f() :: fun((Request :: any(),
                       State :: any(),
                       Dispatcher :: cloudi:dispatcher()) ->
                      {noreply, NewState :: any()} |
                      {stop, Reason :: any(), NewState :: any()}).
-type terminate_f() :: fun((Reason :: any(),
                            Timeout :: cloudi:timeout_value_milliseconds(),
                            State :: any()) ->
                           ok).
-export_type([method/0,
              initialize_f/0,
              handler_f_11/0,
              info_f/0,
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
        info_f :: info_f() | undefined,
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
        {initialize,               ?DEFAULT_INITIALIZE},
        {terminate,                ?DEFAULT_TERMINATE},
        {handlers,                 ?DEFAULT_HANDLERS},
        {info,                     ?DEFAULT_INFO},
        {formats,                  ?DEFAULT_FORMATS},
        {use_options_method,       ?DEFAULT_USE_OPTIONS_METHOD},
        {use_trace_method,         ?DEFAULT_USE_TRACE_METHOD},
        {debug,                    ?DEFAULT_DEBUG},
        {debug_level,              ?DEFAULT_DEBUG_LEVEL}],
    [Initialize, Terminate0, Handlers0, Info0, Formats0,
     UseOptionsMethod, UseTraceMethod, Debug, DebugLevel | ArgsAPI] =
        cloudi_proplists:take_values(Defaults, Args),
    TerminateN = case Terminate0 of
        {TerminateModule, TerminateFunction}
            when is_atom(TerminateModule),
                 is_atom(TerminateFunction) ->
            true = erlang:function_exported(TerminateModule,
                                            TerminateFunction, 3),
            fun(TerminateArg1, TerminateArg2, TerminateArg3) ->
                TerminateModule:
                TerminateFunction(TerminateArg1, TerminateArg2, TerminateArg3)
            end;
        _ when is_function(Terminate0, 3) ->
            Terminate0;
        undefined ->
            undefined
    end,
    true = is_list(Handlers0),
    lists:foreach(fun({Method, Path, _}) ->
        MethodString = if
            is_atom(Method) ->
                erlang:atom_to_list(Method);
            is_list(Method), is_integer(hd(Method)) ->
                Method
        end,
        MethodString = string:to_upper(MethodString),
        true = is_list(Path) andalso is_integer(hd(Path))
    end, Handlers0),
    InfoN = case Info0 of
        {InfoModule, InfoFunction}
            when is_atom(InfoModule),
                 is_atom(InfoFunction) ->
            true = erlang:function_exported(InfoModule,
                                            InfoFunction, 3),
            fun(InfoArg1, InfoArg2, InfoArg3) ->
                InfoModule:
                InfoFunction(InfoArg1, InfoArg2, InfoArg3)
            end;
        _ when is_function(Info0, 3) ->
            Info0;
        undefined ->
            undefined
    end,
    true = is_list(Formats0),
    true = is_boolean(UseOptionsMethod),
    true = is_boolean(UseTraceMethod),
    true = is_boolean(Debug),
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
    MethodListsN = cloudi_x_trie:to_list(lists:foldr(fun({Method, Path, _},
                                                         MethodLists0) ->
        MethodString = if
            is_atom(Method) ->
                erlang:atom_to_list(Method);
            is_list(Method) ->
                Method
        end,
        cloudi_x_trie:update(Path, fun(MethodList0) ->
            lists:umerge(MethodList0, [MethodString])
        end, [MethodString], MethodLists0)
    end, cloudi_x_trie:new(), Handlers0)),
    Handlers1 = if
        UseTraceMethod =:= true ->
            lists:map(fun({Path, _}) ->
                HandlerTrace = fun(_, _, _, _, _, _, _, _, _,
                                   TraceHandlerState, _) ->
                    {reply,
                     [{<<"via">>, <<"1.1 CloudI">>}], <<>>, TraceHandlerState}
                end,
                {'TRACE', Path, HandlerTrace}
            end, MethodListsN) ++ Handlers0;
        UseTraceMethod =:= false ->
            Handlers0
    end,
    HandlersN = if
        UseOptionsMethod =:= true ->
            lists:map(fun({Path, MethodList1}) ->
                MethodListN = lists:umerge(MethodList1, ["OPTIONS"]),
                Methods = erlang:list_to_binary(string:join(MethodListN,
                                                            ", ")),
                HandlerOptions = fun(_, _, _, _, _, _, _, _, _,
                                     OptionsHandlerState, _) ->
                    % content-type is set automatically by HTTP process
                    {reply,
                     [{<<"allow">>, Methods}], <<>>, OptionsHandlerState}
                end,
                {'OPTIONS', Path, HandlerOptions}
            end, MethodListsN) ++ Handlers1;
        UseOptionsMethod =:= false ->
            Handlers1
    end,
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
        HandlerMethod = if
            is_atom(Method) ->
                Method;
            is_list(Method) ->
                erlang:list_to_atom(Method)
        end,
        API = #api{method = HandlerMethod,
                   path = Path,
                   parameters = lists:member($*, Prefix ++ Path),
                   handler_f = Handler1,
                   arity = Arity},
        subscribe_paths(Method, Path, FormatsN, API, Lookup0, Dispatcher)
    end, cloudi_x_trie:new(), HandlersN),
    State = #state{prefix = Prefix,
                   lookup = LookupN,
                   info_f = InfoN,
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

cloudi_service_handle_info(Request,
                           #state{info_f = InfoF,
                                  api_state = StateAPI} = State,
                           Dispatcher) ->
    if
        InfoF =:= undefined ->
            ?LOG_WARN("Unknown info \"~p\"", [Request]),
            {noreply, State};
        true ->
            case InfoF(Request, StateAPI, Dispatcher) of
                {noreply, NewStateAPI} ->
                    {noreply, State#state{api_state = NewStateAPI}};
                {stop, Reason, NewStateAPI} ->
                    {stop, Reason, State#state{api_state = NewStateAPI}}
            end
    end.

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
    subscribe_path(Formats, Path, "/connect", API, Lookup, Dispatcher);
subscribe_paths(Method, Path, Formats, API, Lookup, Dispatcher)
    when is_list(Method) ->
    MethodSuffix = [$/ | string:to_lower(Method)],
    subscribe_path(Formats, Path, MethodSuffix, API, Lookup, Dispatcher).

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
