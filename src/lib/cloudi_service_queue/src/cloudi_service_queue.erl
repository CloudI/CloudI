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
%%%
%%% The fault_isolation service argument determines the fault-tolerance
%%% guarantee for the request/response exchange.  When fault_tolerance is
%%% set to 'destination' (the default), the sender is isolated from
%%% destination instability only.  So, this means persistence to the
%%% filesystem begins and ends within this service and the source could
%%% fail to receive the response due to its own instability.
%%%
%%% When fault_tolerance is set to 'both', both the sender and the
%%% destination are isolated from instability.  Persistence of the request
%%% begins when this service receives the incoming service request, but
%%% persistence ends after the source receives a service request that contains
%%% the response.  So, 'both' does not assume the source Erlang pid
%%% remains alive during the request/response exchange.  If you need
%%% requests to survive an Erlang VM restart, this is the mode you should use.
%%% (This assumes the source is also meant to receive the response, which
%%%  doesn't need to be the case with 'both'.  Only a valid service name
%%%  needs to be specified for the destination of the response and it
%%%  doesn't need to be the source of the request).
%%%
%%% The retry service argument controls the number of retries during the
%%% lifetime of this service's instance.  So, this means that after an
%%% Erlang VM restart, the retry counter will start from 0
%%% after the initial retry that occurs when the WAL is read upon startup.
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
-define(DEFAULT_FAULT_ISOLATION,      destination). % | both

% fault_isolation: destination
-type request_destination_mode() ::
    {cloudi_service:request_type(),
     cloudi_service:service_name(),
     cloudi_service:service_name_pattern(),
     cloudi_service:request_info(),
     cloudi_service:request(),
     cloudi_service:timeout_value_milliseconds(),
     cloudi_service:priority(),
     cloudi_service:trans_id(),
     cloudi_service:source()}.
% fault_isolation: both
-type request_both_mode() ::
    {cloudi_service:service_name(),
     cloudi_service:request_info(),
     cloudi_service:request(),
     cloudi_service:timeout_value_milliseconds(),
     cloudi_service:priority(),
     cloudi_service:trans_id(),
     cloudi_service:service_name(),
     cloudi_service:trans_id()} |
    {cloudi_service:service_name(),
     cloudi_service:response_info(),
     cloudi_service:response(),
     cloudi_service:timeout_value_milliseconds(),
     cloudi_service:priority(),
     cloudi_service:trans_id()}.
-type request() :: request_destination_mode() |
                   request_both_mode().
-export_type([request/0]).

-record(state,
    {
        logging,
        mode :: destination | both,
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
        {retry,                       ?DEFAULT_RETRY},
        {fault_isolation,   ?DEFAULT_FAULT_ISOLATION},
        {file,                             undefined}],
    [Retry, Mode, FilePath] = cloudi_proplists:take_values(Defaults, Args),
    true = (is_integer(Retry) andalso (Retry >= 0)),
    true = ((Mode =:= destination) orelse (Mode =:= both)),
    true = (is_list(FilePath) andalso is_integer(hd(FilePath))),
    I = erlang:integer_to_list(cloudi_service:process_index(Dispatcher)),
    Environment = cloudi_x_trie:store("I", I,
                                      cloudi_service:environment_lookup()),
    QueueFilePath = cloudi_service:environment_transform(FilePath,
                                                         Environment),
    Logging = cloudi_write_ahead_logging:new(QueueFilePath,
                                             fun(T) ->
                                                retry(Mode, Dispatcher, T)
                                             end),
    false = cloudi_x_trie:is_pattern(Prefix),
    cloudi_service:subscribe(Dispatcher, "*"),
    DispatcherPid = cloudi_service:dispatcher(Dispatcher),
    {ok, #state{logging = Logging,
                mode = Mode,
                retry = Retry,
                retry_f = fun(T) -> retry(Mode, DispatcherPid, T) end}}.

cloudi_service_handle_request(Type, Name, Pattern, RequestInfo, Request,
                              Timeout, Priority, TransId, Pid,
                              #state{logging = Logging,
                                     mode = destination} = State,
                              Dispatcher) ->
    [QueueName] = cloudi_service:service_name_parse(Name, Pattern),
    ChunkRequest = {Type, Name, Pattern, RequestInfo, Request,
                    Timeout, Priority, TransId, Pid},
    {Chunk, NextLogging} = cloudi_write_ahead_logging:
                           store_start(ChunkRequest, Logging),
    % PERSISTENCE START:
    % at this point the service request has been persisted and
    % can be restarted
    case cloudi_service:send_async_active(Dispatcher, QueueName,
                                          RequestInfo, Request,
                                          Timeout, Priority) of
        {ok, QueueTransId} ->
            NewLogging = cloudi_write_ahead_logging:
                         store_end(QueueTransId, Chunk, NextLogging),
            {noreply, State#state{logging = NewLogging}};
        {error, timeout} ->
            NewLogging = cloudi_write_ahead_logging:
                         store_fail(Chunk, NextLogging),
            {reply, <<>>, State#state{logging = NewLogging}};
        {error, Reason} ->
            NewLogging = cloudi_write_ahead_logging:
                         store_fail(Chunk, NextLogging),
            ?LOG_ERROR("request to ~p failed: ~p",
                       [QueueName, Reason]),
            {reply, <<>>, State#state{logging = NewLogging}}
    end;
cloudi_service_handle_request(_Type, Name, Pattern, RequestInfo, Request,
                              Timeout, Priority, TransId, _Pid,
                              #state{logging = Logging,
                                     mode = both} = State,
                              Dispatcher) ->
    RequestMetaData = cloudi_service:request_info_key_value_parse(RequestInfo),
    case cloudi_service:key_value_find(<<"service_name">>, RequestMetaData) of
        {ok, NextName} ->
            [QueueName] = cloudi_service:service_name_parse(Name, Pattern),
            NextTransId = cloudi_service:trans_id(Dispatcher),
            ChunkRequest = {QueueName, RequestInfo, Request,
                            Timeout, Priority, TransId, NextName, NextTransId},
            {Chunk, NextLogging} = cloudi_write_ahead_logging:
                                   store_start(ChunkRequest, Logging),
            % PERSISTENCE START:
            % at this point the service request has been persisted and
            % can be restarted
            case cloudi_service:send_async_active(Dispatcher, QueueName,
                                                  RequestInfo, Request,
                                                  Timeout, Priority) of
                {ok, QueueTransId} ->
                    NewLogging = cloudi_write_ahead_logging:
                                 store_end(QueueTransId, Chunk, NextLogging),
                    {reply, NextTransId, State#state{logging = NewLogging}};
                {error, timeout} ->
                    NewLogging = cloudi_write_ahead_logging:
                                 store_fail(Chunk, NextLogging),
                    {reply, <<>>, State#state{logging = NewLogging}};
                {error, Reason} ->
                    NewLogging = cloudi_write_ahead_logging:
                                 store_fail(Chunk, NextLogging),
                    ?LOG_ERROR("request to ~p failed: ~p",
                               [QueueName, Reason]),
                    {reply, <<>>, State#state{logging = NewLogging}}
            end;
        error ->
            ?LOG_ERROR("service_name not found in RequestInfo for ~s",
                       [cloudi_x_uuid:uuid_to_string(TransId, nodash)]),
            {reply, <<>>, State}
    end.

cloudi_service_handle_info(#return_async_active{response_info = ResponseInfo,
                                                response = Response,
                                                timeout = Timeout,
                                                trans_id = QueueTransId},
                           #state{logging = Logging,
                                  mode = destination} = State,
                           Dispatcher) ->
    {{Type, Name, Pattern, _, _, _, _, TransId, Pid},
     NewLogging} = cloudi_write_ahead_logging:
                   erase(QueueTransId, Logging),
    % PERSISTENCE END:
    % at this point the service request is no longer persisted
    cloudi_service:return_nothrow(Dispatcher, Type, Name, Pattern,
                                  ResponseInfo, Response,
                                  Timeout, TransId, Pid),
    {noreply, State#state{logging = NewLogging}};

cloudi_service_handle_info(#return_async_active{response_info = ResponseInfo,
                                                response = Response,
                                                trans_id = QueueTransId},
                           #state{logging = Logging,
                                  mode = both} = State,
                           Dispatcher) ->
    UpdateF = fun
        ({_QueueName, _RequestInfo, _Request,
          Timeout, Priority, _TransId, NextName, NextTransId}) ->
            {NextTransId,
             {NextName, ResponseInfo, Response,
              Timeout, Priority, NextTransId}};
        ({_NextName, _ResponseInfo, _Response,
          _NextTimeout, _NextPriority, _NextTransId}) ->
            undefined
    end,
    {NewChunkRequest,
     NewLogging} = cloudi_write_ahead_logging:
                   update(QueueTransId, UpdateF, Logging),
    case NewChunkRequest of
        undefined ->
            % PERSISTENCE END:
            % at this point the service request is no longer persisted
            % because both the request and the response were delivered
            {noreply, State#state{logging = NewLogging}};
        {NextName, ResponseInfo, Response,
         NextTimeout, NextPriority, NextTransId} ->
            % deliver the response as a service request
            case cloudi_service:get_pid(Dispatcher, NextName, NextTimeout) of
                {ok, PatternPid} ->
                    cloudi_service:send_async_active(Dispatcher, NextName,
                                                     ResponseInfo, Response,
                                                     NextTimeout, NextPriority,
                                                     NextTransId, PatternPid);
                {error, _} ->
                    Self = cloudi_service:self(Dispatcher),
                    Self ! #timeout_async_active{trans_id = NextTransId}
            end,
            {noreply, State#state{logging = NewLogging}}
    end;

cloudi_service_handle_info(#timeout_async_active{trans_id = QueueTransId},
                           #state{logging = Logging,
                                  retry = Retry,
                                  retry_f = RetryF} = State,
                           _Dispatcher) ->
    NewLogging = cloudi_write_ahead_logging:
                 erase_retry(QueueTransId, Retry, RetryF, Logging),
    % PERSISTENCE END (if a retry didn't occur):
    % at this point the service request/response is no longer persisted
    % due to its timeout value
    {noreply, State#state{logging = NewLogging}};

cloudi_service_handle_info(Request, State, _) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_service_terminate(_, #state{}) ->
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

-spec retry(Mode :: destination | both,
            Dispatcher :: cloudi_service:dispatcher(),
            request()) ->
    {ok, cloudi_service:trans_id()} |
    {error, any()}.
-compile({inline, [{retry, 3}]}).

retry(destination, Dispatcher,
      {_Type, Name, Pattern, RequestInfo, Request,
       Timeout, Priority, TransId, Pid}) ->
    Age = (cloudi_x_uuid:get_v1_time(erlang) -
           cloudi_x_uuid:get_v1_time(TransId)) div 1000 + 100, % milliseconds
    case erlang:is_process_alive(Pid) of
        false ->
            {error, timeout};
        true when Age >= Timeout ->
            {error, timeout};
        true ->
            [QueueName] = cloudi_service:service_name_parse(Name, Pattern),
            NewTimeout = Timeout - Age,
            cloudi_service:send_async_active(Dispatcher, QueueName,
                                             RequestInfo, Request,
                                             NewTimeout, Priority)
    end;
retry(both, Dispatcher,
      {QueueName, RequestInfo, Request,
       Timeout, Priority, TransId, NextName, NextTransId}) ->
    Age = (cloudi_x_uuid:get_v1_time(erlang) -
           cloudi_x_uuid:get_v1_time(TransId)) div 1000 + 100, % milliseconds
    if
        Age >= Timeout ->
            case cloudi_service:get_pid(Dispatcher, NextName, Timeout) of
                {ok, PatternPid} ->
                    cloudi_service:send_async_active(Dispatcher, NextName,
                                                     <<>>, <<>>,
                                                     Timeout, Priority,
                                                     NextTransId,
                                                     PatternPid);
                {error, _} = Error ->
                    Error
            end;
        true ->
            NewTimeout = Timeout - Age,
            cloudi_service:send_async_active(Dispatcher, QueueName,
                                             RequestInfo, Request,
                                             NewTimeout, Priority)
    end;
retry(both, Dispatcher,
      {NextName, ResponseInfo, Response,
       NextTimeout, NextPriority, NextTransId}) ->
    case cloudi_service:get_pid(Dispatcher, NextName, NextTimeout) of
        {ok, PatternPid} ->
            cloudi_service:send_async_active(Dispatcher, NextName,
                                             ResponseInfo, Response,
                                             NextTimeout, NextPriority,
                                             NextTransId, PatternPid);
        {error, _} = Error ->
            Error
    end.

