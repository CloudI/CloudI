%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Elli HTTP Integration==
%%% Uses the elli Erlang HTTP Server.
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2013-2018 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2013-2018 Michael Truog
%%% @version 1.7.5 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_http_elli).
-author('mjtruog at protonmail dot com').

-behaviour(cloudi_service).

%% external interface

%% cloudi_service callbacks
-export([cloudi_service_init/4,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/3]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").
-include_lib("cloudi_core/include/cloudi_service_children.hrl").
-include("cloudi_http_elli_handler.hrl").

-define(DEFAULT_IP,                         {127,0,0,1}). % interface ip address
-define(DEFAULT_PORT,                              8080).
-define(DEFAULT_ACCEPT_TIMEOUT,               10 * 1000). % milliseconds
-define(DEFAULT_RECV_TIMEOUT,                 30 * 1000). % milliseconds
-define(DEFAULT_HEADER_TIMEOUT,               10 * 1000). % milliseconds
-define(DEFAULT_BODY_TIMEOUT,                 30 * 1000). % milliseconds
-define(DEFAULT_MIN_ACCEPTORS,                       20).
-define(DEFAULT_SSL,                              false).
-define(DEFAULT_MAX_BODY_SIZE,                  1024000).
-define(DEFAULT_OUTPUT,                        external).
-define(DEFAULT_CONTENT_TYPE,                 undefined). % force a content type
-define(DEFAULT_CONTENT_TYPES_ACCEPTED,       undefined).
-define(DEFAULT_STATUS_CODE_TIMEOUT,                504). % "Gateway Timeout"
-define(DEFAULT_QUERY_GET_FORMAT,                   raw). % see below:
        % If set to 'text_pairs' any GET query string is parsed and
        % encoded based on the format defined in the cloudi_request_info
        % module (to create a single binary accessible in external services)
        % if the output is not internal, if the output is internal the
        % parsed query string is provided in an Erlang data structure used
        % by the cloudi_key_value module.
        % Due to making testing simpler without requiring extra dependencies,
        % the 'text_pairs' format is used when performing basic loadtests
        % of CloudI with the http_req integration test.
        % The default 'raw' setting will not parse GET query string data.
        % GET query string data is always provided in the service request
        % Request data (so using GET request body data will be ignored,
        % due to being bad practice).
-define(DEFAULT_SET_X_FORWARDED_FOR,              false). % if it is missing
-define(DEFAULT_USE_HOST_PREFIX,                  false). % for virtual hosts
-define(DEFAULT_USE_CLIENT_IP_PREFIX,             false).
-define(DEFAULT_USE_X_METHOD_OVERRIDE,            false).
-define(DEFAULT_USE_METHOD_SUFFIX,                 true). % see below:
        % Always append a suffix on the service name used to send the
        % HTTP request as a CloudI service request, to utilize the service
        % name routing when handling separate HTTP request methods.
        % For example, a GET HTTP request method would cause "/get" to be
        % added to the service name (the URL path) that is used when sending
        % the service request.
-define(DEFAULT_UPDATE_DELAY,                         5). % see below:
        % The number of seconds before applying service configuration
        % update (e.g., changes with cloudi_service_api:services_update/2)
        % for the value timeout_sync.

-record(state,
    {
        listener :: pid(),
        service :: pid(),
        handler_state :: #elli_state{}
    }).

-define(CALLBACK, cloudi_http_elli_handler).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, Prefix, _Timeout, Dispatcher) ->
    Defaults = [
        {ip,                            ?DEFAULT_IP},
        {port,                          ?DEFAULT_PORT},
        {accept_timeout,                ?DEFAULT_ACCEPT_TIMEOUT},
        {recv_timeout,                  ?DEFAULT_RECV_TIMEOUT},
        {header_timeout,                ?DEFAULT_HEADER_TIMEOUT},
        {body_timeout,                  ?DEFAULT_BODY_TIMEOUT},
        {min_acceptors,                 ?DEFAULT_MIN_ACCEPTORS},
        {ssl,                           ?DEFAULT_SSL},
        {max_body_size,                 ?DEFAULT_MAX_BODY_SIZE},
        {output,                        ?DEFAULT_OUTPUT},
        {content_type,                  ?DEFAULT_CONTENT_TYPE},
        {content_types_accepted,        ?DEFAULT_CONTENT_TYPES_ACCEPTED},
        {set_x_forwarded_for,           ?DEFAULT_SET_X_FORWARDED_FOR},
        {status_code_timeout,           ?DEFAULT_STATUS_CODE_TIMEOUT},
        {query_get_format,              ?DEFAULT_QUERY_GET_FORMAT},
        {use_host_prefix,               ?DEFAULT_USE_HOST_PREFIX},
        {use_client_ip_prefix,          ?DEFAULT_USE_CLIENT_IP_PREFIX},
        {use_x_method_override,         ?DEFAULT_USE_X_METHOD_OVERRIDE},
        {use_method_suffix,             ?DEFAULT_USE_METHOD_SUFFIX},
        {update_delay,                  ?DEFAULT_UPDATE_DELAY}],
    [Interface, Port, AcceptTimeout, RecvTimeout, HeaderTimeout, BodyTimeout,
     MinAcceptors, SSL, MaxBodySize,
     OutputType, ContentTypeForced0, ContentTypesAccepted0, SetXForwardedFor,
     StatusCodeTimeout, QueryGetFormat,
     UseHostPrefix, UseClientIpPrefix, UseXMethodOverride, UseMethodSuffix,
     UpdateDelaySeconds] =
        cloudi_proplists:take_values(Defaults, Args),
    1 = cloudi_service:process_count_max(Dispatcher),
    true = is_integer(Port),
    true = is_integer(AcceptTimeout),
    true = is_integer(RecvTimeout),
    true = is_integer(HeaderTimeout),
    true = is_integer(BodyTimeout),
    true = is_integer(MinAcceptors),
    SSLConfig = if
        SSL =:= false ->
            [];
        is_list(SSL) ->
            {value,
             {keyfile, KeyFile}, SSL0} = lists:keytake(keyfile, 1, SSL),
            {value,
             {certfile, CertFile}, []} = lists:keytake(certfile, 1, SSL0),
            Environment = cloudi_environment:lookup(),
            environment_transform_ssl_options([{ssl, true},
                                               {keyfile, KeyFile},
                                               {certfile, CertFile}],
                                              Environment)
    end,
    true = is_integer(MaxBodySize),
    true = OutputType =:= external orelse OutputType =:= internal orelse
           OutputType =:= list orelse OutputType =:= binary,
    ContentTypeForced1 = if
        ContentTypeForced0 =:= undefined ->
            undefined;
        is_list(ContentTypeForced0) ->
            erlang:list_to_binary(ContentTypeForced0);
        is_binary(ContentTypeForced0) ->
            ContentTypeForced0
    end,
    ContentTypesAccepted1 = if
        ContentTypesAccepted0 =:= undefined ->
            undefined;
        is_list(ContentTypesAccepted0) ->
            content_types_accepted_pattern(ContentTypesAccepted0)
    end,
    true = is_boolean(SetXForwardedFor),
    true = is_integer(StatusCodeTimeout) andalso
           (StatusCodeTimeout > 100) andalso
           (StatusCodeTimeout =< 599),
    true = (QueryGetFormat =:= raw) orelse
           (QueryGetFormat =:= text_pairs),
    true = is_boolean(UseHostPrefix),
    true = is_boolean(UseClientIpPrefix),
    true = ((UseXMethodOverride =:= true) andalso
            (UseMethodSuffix =:= true)) orelse
           (UseXMethodOverride =:= false),
    true = is_boolean(UseMethodSuffix),
    true = is_integer(UpdateDelaySeconds) andalso
           (UpdateDelaySeconds > 0) andalso (UpdateDelaySeconds =< 4294967),
    false = cloudi_service_name:pattern(Prefix),
    HandlerState = #elli_state{
        dispatcher = cloudi_service:dispatcher(Dispatcher),
        timeout_sync = cloudi_service:timeout_sync(Dispatcher),
        output_type = OutputType,
        content_type_forced = ContentTypeForced1,
        content_types_accepted = ContentTypesAccepted1,
        set_x_forwarded_for = SetXForwardedFor,
        status_code_timeout = StatusCodeTimeout,
        query_get_format = QueryGetFormat,
        use_host_prefix = UseHostPrefix,
        use_client_ip_prefix = UseClientIpPrefix,
        use_x_method_override = UseXMethodOverride,
        use_method_suffix = UseMethodSuffix},
    Service = cloudi_service:self(Dispatcher),
    erlang:send_after(UpdateDelaySeconds * 1000, Service,
                      {update, UpdateDelaySeconds}),
    {ok,
     ListenerPid} = cloudi_x_elli:
                    start_link([{name, undefined},
                                {callback, ?CALLBACK},
                                {callback_args, HandlerState},
                                {ip, Interface},
                                {port, Port},
                                {min_acceptors, MinAcceptors},
                                {max_body_size, MaxBodySize},
                                {accept_timeout, AcceptTimeout},
                                {request_timeout, RecvTimeout},
                                {header_timeout, HeaderTimeout},
                                {body_timeout, BodyTimeout} | SSLConfig]),
    {ok, #state{listener = ListenerPid,
                service = Service,
                handler_state = HandlerState}}.

cloudi_service_handle_request(_Type, _Name, _Pattern, _RequestInfo, _Request,
                              _Timeout, _Priority, _TransId, _Pid,
                              State, _Dispatcher) ->
    {reply, <<>>, State}.

cloudi_service_handle_info({update, UpdateDelaySeconds},
                           #state{listener = ListenerPid,
                                  service = Service,
                                  handler_state = HandlerState} = State,
                           Dispatcher) ->
    % The timeout_sync service configuration value needs to be updated
    % within the elli_state record used for new connection processes,
    % after an update has occurred using the
    % CloudI Service API function services_update
    #elli_state{timeout_sync = TimeoutSync} = HandlerState,
    ContextOptions = cloudi_service:context_options(Dispatcher),
    {_, TimeoutSyncCurrent} = lists:keyfind(timeout_sync, 1, ContextOptions),
    NewHandlerState = if
        TimeoutSync == TimeoutSyncCurrent ->
            HandlerState;
        true ->
            NextHandlerState = HandlerState#elli_state{
                timeout_sync = TimeoutSyncCurrent},
            ok = cloudi_x_elli:set_callback(ListenerPid,
                                            ?CALLBACK,
                                            NextHandlerState),
            NextHandlerState
    end,
    erlang:send_after(UpdateDelaySeconds * 1000, Service,
                      {update, UpdateDelaySeconds}),
    {noreply, State#state{handler_state = NewHandlerState}};
cloudi_service_handle_info(Request, State, _Dispatcher) ->
    {stop, cloudi_string:format("Unknown info \"~w\"", [Request]), State}.

cloudi_service_terminate(_Reason, _Timeout, undefined) ->
    ok;
cloudi_service_terminate(_Reason, _Timeout,
                         #state{listener = ListenerPid}) ->
    (catch cloudi_x_elli:stop(ListenerPid)),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

environment_transform_ssl_options(SSLOpts, Environment) ->
    environment_transform_ssl_options(SSLOpts, [], Environment).

environment_transform_ssl_options([], Output, _) ->
    lists:reverse(Output);
environment_transform_ssl_options([{K, FilePath} | SSLOpts],
                                  Output, Environment)
    when K =:= certfile; K =:= cacertfile; K =:= keyfile ->
    NewFilePath = cloudi_environment:transform(FilePath, Environment),
    environment_transform_ssl_options(SSLOpts, [{K, NewFilePath} | Output],
                                      Environment);
environment_transform_ssl_options([E | SSLOpts], Output, Environment) ->
    environment_transform_ssl_options(SSLOpts, [E | Output], Environment).

content_types_accepted_pattern(ContentTypesAccepted) ->
    content_types_accepted_pattern(ContentTypesAccepted, [<<"*/*">>]).

content_types_accepted_pattern([], L) ->
    binary:compile_pattern(L);
content_types_accepted_pattern([H | ContentTypesAccepted], L) ->
    ContentType = if
        is_list(H), is_integer(hd(H)) ->
            erlang:list_to_binary(H);
        is_binary(H) ->
            H
    end,
    NewL = case binary:split(ContentType, <<"/">>) of
        [<<"*">>, <<"*">>] ->
            lists:umerge(L, [ContentType]);
        [_, <<"*">>] ->
            lists:umerge(L, [ContentType]);
        [Type, _] ->
            lists:umerge(L, [<<Type/binary,<<"/*">>/binary>>, ContentType])
    end,
    content_types_accepted_pattern(ContentTypesAccepted, NewL).

