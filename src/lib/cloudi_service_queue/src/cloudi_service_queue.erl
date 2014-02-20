%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Persistent Queue Service==
%%% Use Write Ahead Logging (WAL) to persist service requests.  This
%%% service provides a leaner alternative to persistent messaging queues
%%% (e.g., when compared to RabbitMQ, both topics/queues).
%%% The filesystem is used for keeping the queue service requests persistent
%%% and the queue file path is provided to this service's configuration
%%% arguments.  You must make sure the queue file path is unique.  If you
%%% use a process count higher than 1, make sure to have "${I}" within the
%%% file path, so the process index is used within the file path.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2014, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2014 Michael Truog
%%% @version 1.3.2 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_queue).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloudi_service).

%% cloudi_service callbacks
-export([cloudi_service_init/3,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/2]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").
-include_lib("cloudi_core/include/cloudi_service.hrl").

-define(DEFAULT_RETRY,                          0).

-type request() ::
    {cloudi_service:service_name(),
     cloudi_service:request_type(),
     cloudi_service:service_name(),
     cloudi_service:service_name_pattern(),
     cloudi_service:request_info(),
     cloudi_service:request(),
     cloudi_service:timeout_value_milliseconds(),
     cloudi_service:priority(),
     cloudi_service:trans_id(),
     cloudi_service:source()}.
-export_type([request/0]).

-record(state,
    {
        logging,
        retry :: non_neg_integer(),
        retry_f :: fun((request()) ->
                       {ok, cloudi_service:trans_id()} |
                       {error, any()})
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, Prefix, Dispatcher) ->
    Defaults = [
        {retry,               ?DEFAULT_RETRY},
        {file,                     undefined}],
    [Retry, FilePath] = cloudi_proplists:take_values(Defaults, Args),
    true = (is_integer(Retry) andalso (Retry >= 0)),
    true = (is_list(FilePath) andalso is_integer(hd(FilePath))),
    I = erlang:integer_to_list(cloudi_service:process_index(Dispatcher)),
    Environment = cloudi_x_trie:store("I", I,
                                      cloudi_service:environment_lookup()),
    QueueFilePath = cloudi_service:environment_transform(FilePath,
                                                         Environment),
    Logging = cloudi_write_ahead_logging:new(QueueFilePath,
                                             fun(T) ->
                                                retry(Dispatcher, T)
                                             end),
    false = cloudi_x_trie:is_pattern(Prefix),
    cloudi_service:subscribe(Dispatcher, "*"),
    DispatcherPid = cloudi_service:dispatcher(Dispatcher),
    {ok, #state{logging = Logging,
                retry = Retry,
                retry_f = fun(T) -> retry(DispatcherPid, T) end}}.

cloudi_service_handle_request(Type, Name, Pattern, RequestInfo, Request,
                              Timeout, Priority, TransId, Pid,
                              #state{logging = Logging} = State,
                              Dispatcher) ->
    [QueueName] = cloudi_service:service_name_parse(Name, Pattern),
    {Chunk,
     NextLogging} = cloudi_write_ahead_logging:store_start({QueueName, Type,
                                                            Name, Pattern,
                                                            RequestInfo,
                                                            Request,
                                                            Timeout, Priority,
                                                            TransId, Pid},
                                                           Logging),
    % PERSISTENCE START:
    % at this point the service request has been persisted and
    % can be restarted
    case cloudi_service:send_async_active(Dispatcher, QueueName,
                                          RequestInfo, Request,
                                          Timeout, Priority) of
        {ok, QueueTransId} ->
            NewLogging = cloudi_write_ahead_logging:store_end(QueueTransId,
                                                              Chunk,
                                                              NextLogging),
            {noreply, State#state{logging = NewLogging}};
        {error, timeout} ->
            NewLogging = cloudi_write_ahead_logging:store_fail(Chunk,
                                                               NextLogging),
            {reply, <<>>, State#state{logging = NewLogging}};
        {error, Reason} ->
            NewLogging = cloudi_write_ahead_logging:store_fail(Chunk,
                                                               NextLogging),
            ?LOG_ERROR("request to ~p failed: ~p", [QueueName, Reason]),
            {reply, <<>>, State#state{logging = NewLogging}}
    end.

cloudi_service_handle_info(#return_async_active{response_info = ResponseInfo,
                                                response = Response,
                                                timeout = Timeout,
                                                trans_id = QueueTransId},
                           #state{logging = Logging} = State,
                           Dispatcher) ->
    {{_, Type, Name, Pattern, _, _, _, _, TransId, Pid},
     NewLogging} = cloudi_write_ahead_logging:erase(QueueTransId, Logging),
    % PERSISTENCE END:
    % at this point the service request is no longer persisted
    cloudi_service:return_nothrow(Dispatcher, Type, Name, Pattern,
                                  ResponseInfo, Response,
                                  Timeout, TransId, Pid),
    {noreply, State#state{logging = NewLogging}};

cloudi_service_handle_info(#timeout_async_active{trans_id = QueueTransId},
                           #state{logging = Logging,
                                  retry = Retry,
                                  retry_f = RetryF} = State,
                           _Dispatcher) ->
    NewLogging = cloudi_write_ahead_logging:erase_retry(QueueTransId,
                                                        Retry, RetryF,
                                                        Logging),
    % PERSISTENCE END (if a retry didn't occur):
    % at this point the service request is no longer persisted
    % due to the service request's timeout
    {noreply, State#state{logging = NewLogging}};

cloudi_service_handle_info(Request, State, _) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_service_terminate(_, #state{}) ->
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

-spec retry(Dispatcher :: cloudi_service:dispatcher(),
            request()) ->
    {ok, cloudi_service:trans_id()} |
    {error, any()}.
-compile({inline, [{retry, 2}]}).

retry(Dispatcher,
      {QueueName, _Type, _Name, _Pattern, RequestInfo, Request,
       Timeout, Priority, TransId, Pid}) ->
    Age = (cloudi_x_uuid:get_v1_time(erlang) -
           cloudi_x_uuid:get_v1_time(TransId)) div 1000 + 100, % milliseconds
    case erlang:is_process_alive(Pid) of
        false ->
            {error, timeout};
        true when Age >= Timeout ->
            {error, timeout};
        true ->
            NewTimeout = Timeout - Age,
            cloudi_service:send_async_active(Dispatcher, QueueName,
                                             RequestInfo, Request,
                                             NewTimeout, Priority)
    end.

