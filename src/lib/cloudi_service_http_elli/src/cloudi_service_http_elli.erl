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
%%% Copyright (c) 2013-2014, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2013-2014 Michael Truog
%%% @version 1.4.0 {@date} {@time}
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

-define(DEFAULT_INTERFACE,         {127,0,0,1}). % ip address
-define(DEFAULT_PORT,                     8080).
-define(DEFAULT_RECV_TIMEOUT,        30 * 1000). % milliseconds
-define(DEFAULT_MIN_ACCEPTORS,             100).
-define(DEFAULT_MAX_BODY_SIZE,         1024000).
-define(DEFAULT_OUTPUT,               external).
-define(DEFAULT_CONTENT_TYPE,        undefined). % force a content type
-define(DEFAULT_USE_HOST_PREFIX,         false). % for virtual hosts
-define(DEFAULT_USE_METHOD_SUFFIX,        true). % get/post/etc. name suffix

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
        {ip,                       ?DEFAULT_INTERFACE},
        {port,                     ?DEFAULT_PORT},
        {recv_timeout,             ?DEFAULT_RECV_TIMEOUT},
        {min_acceptors,            ?DEFAULT_MIN_ACCEPTORS},
        {max_body_size,            ?DEFAULT_MAX_BODY_SIZE},
        {output,                   ?DEFAULT_OUTPUT},
        {content_type,             ?DEFAULT_CONTENT_TYPE},
        {use_host_prefix,          ?DEFAULT_USE_HOST_PREFIX},
        {use_method_suffix,        ?DEFAULT_USE_METHOD_SUFFIX}],
    [Interface, Port, RecvTimeout, MinAcceptors, MaxBodySize,
     OutputType, DefaultContentType0, UseHostPrefix, UseMethodSuffix] =
        cloudi_proplists:take_values(Defaults, Args),
    true = is_integer(Port),
    true = is_integer(RecvTimeout),
    true = is_integer(MinAcceptors),
    true = is_integer(MaxBodySize),
    true = OutputType =:= external orelse OutputType =:= internal orelse
           OutputType =:= list orelse OutputType =:= binary,
    DefaultContentType1 = if
        DefaultContentType0 =:= undefined ->
            undefined;
        is_list(DefaultContentType0) ->
            erlang:list_to_binary(DefaultContentType0);
        is_binary(DefaultContentType0) ->
            DefaultContentType0
    end,
    true = is_boolean(UseHostPrefix),
    true = is_boolean(UseMethodSuffix),
    false = lists:member($*, Prefix),
    CallbackArgs = #elli_state{dispatcher =
                                   cloudi_service:dispatcher(Dispatcher),
                               context = create_context(Dispatcher),
                               output_type = OutputType,
                               default_content_type = DefaultContentType1,
                               use_host_prefix = UseHostPrefix,
                               use_method_suffix = UseMethodSuffix},
    {ok, ListenerPid} =
        cloudi_x_elli:start_link([{name, undefined},
                                  {callback, cloudi_http_elli_handler},
                                  {callback_args, CallbackArgs},
                                  {ip, Interface},
                                  {port, Port},
                                  {min_acceptors, MinAcceptors},
                                  {max_body_size, MaxBodySize},
                                  {accept_timeout, RecvTimeout},
                                  {request_timeout, RecvTimeout},
                                  {header_timeout, RecvTimeout},
                                  {body_timeout, RecvTimeout}]),
    {ok, #state{listener = ListenerPid}}.

cloudi_service_handle_request(_Type, _Name, _Pattern, _RequestInfo, _Request,
                              _Timeout, _Priority, _TransId, _Pid,
                              State, _Dispatcher) ->
    {reply, <<>>, State}.

cloudi_service_handle_info(Request, State, _Dispatcher) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_service_terminate(_Reason, _Timeout,
                         #state{listener = ListenerPid}) ->
    (catch cloudi_x_elli:stop(ListenerPid)),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

