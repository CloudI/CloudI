%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Elli HTTP Integration==
%%% Uses the elli Erlang HTTP Server.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2013-2015, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2013-2015 Michael Truog
%%% @version 1.4.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_http_elli).
-author('mjtruog [at] gmail (dot) com').

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
-define(DEFAULT_SET_X_FORWARDED_FOR,              false). % if it is missing
-define(DEFAULT_USE_HOST_PREFIX,                  false). % for virtual hosts
-define(DEFAULT_USE_CLIENT_IP_PREFIX,             false).
-define(DEFAULT_USE_METHOD_SUFFIX,                 true). % get/post name suffix

-record(state,
    {
        listener
    }).

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
        {use_host_prefix,               ?DEFAULT_USE_HOST_PREFIX},
        {use_client_ip_prefix,          ?DEFAULT_USE_CLIENT_IP_PREFIX},
        {use_method_suffix,             ?DEFAULT_USE_METHOD_SUFFIX}],
    [Interface, Port, AcceptTimeout, RecvTimeout, HeaderTimeout, BodyTimeout,
     MinAcceptors, SSL, MaxBodySize,
     OutputType, ContentTypeForced0, ContentTypesAccepted0, SetXForwardedFor,
     StatusCodeTimeout, UseHostPrefix, UseClientIpPrefix, UseMethodSuffix] =
        cloudi_proplists:take_values(Defaults, Args),
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
            Environment = cloudi_service:environment_lookup(),
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
    true = is_boolean(UseHostPrefix),
    true = is_boolean(UseClientIpPrefix),
    true = is_boolean(UseMethodSuffix),
    false = lists:member($*, Prefix),
    CallbackArgs = #elli_state{
                       dispatcher = cloudi_service:dispatcher(Dispatcher),
                       context = create_context(Dispatcher),
                       output_type = OutputType,
                       content_type_forced = ContentTypeForced1,
                       content_types_accepted = ContentTypesAccepted1,
                       set_x_forwarded_for = SetXForwardedFor,
                       status_code_timeout = StatusCodeTimeout,
                       use_host_prefix = UseHostPrefix,
                       use_client_ip_prefix = UseClientIpPrefix,
                       use_method_suffix = UseMethodSuffix},
    {ok,
     ListenerPid} = cloudi_x_elli:
                    start_link([{name, undefined},
                                {callback, cloudi_http_elli_handler},
                                {callback_args, CallbackArgs},
                                {ip, Interface},
                                {port, Port},
                                {min_acceptors, MinAcceptors},
                                {max_body_size, MaxBodySize},
                                {accept_timeout, AcceptTimeout},
                                {request_timeout, RecvTimeout},
                                {header_timeout, HeaderTimeout},
                                {body_timeout, BodyTimeout} | SSLConfig]),
    {ok, #state{listener = ListenerPid}}.

cloudi_service_handle_request(_Type, _Name, _Pattern, _RequestInfo, _Request,
                              _Timeout, _Priority, _TransId, _Pid,
                              State, _Dispatcher) ->
    {reply, <<>>, State}.

cloudi_service_handle_info(Request, State, _Dispatcher) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

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
    NewFilePath = cloudi_service:environment_transform(FilePath, Environment),
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

