%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
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
%%% guarantee for the request/response exchange.  When fault_isolation is
%%% set to 'destination' (the default), the sender is isolated from
%%% destination instability only.  So, this means persistence to the
%%% filesystem begins and ends within this service and the source could
%%% fail to receive the response due to its own instability.
%%%
%%% When fault_isolation is set to 'both', both the sender and the
%%% destination are isolated from instability.  Persistence of the request
%%% begins when this service receives the incoming service request, but
%%% persistence ends after the source receives a service request that contains
%%% the response.  So, 'both' does not assume the source Erlang pid
%%% remains alive during the request/response exchange.  If you need
%%% requests to survive an Erlang VM restart, this is the mode you should use.
%%% (This assumes the source is also meant to receive the response, which
%%%  doesn't need to be the case with 'both'.  Only a valid service name
%%%  needs to be specified for the destination of the response
%%%  (with a &lt;&lt;"service_name"&gt;&gt; key/value entry in the
%%%   RequestInfo of the initial service request) and it doesn't need to be
%%%  the source of the request).
%%%
%%% The retry service argument controls the number of retries during the
%%% lifetime of this service's instance.  So, this means that after an
%%% Erlang VM restart, the retry counter will start from 0
%%% after the initial retry that occurs when the WAL is read upon startup.
%%%
%%% The amount of time the service request is persisted is always limited by
%%% the timeout of the service request.  Tracking the time taken by a
%%% service request depends on the Erlang VM time-keeping being dependable
%%% which depends on the OS time-keeping not varying wildly, if
%%% fault_isolation is set to 'both' and an Erlang VM restart causes an old
%%% queue file to be used (the new Erlang VM OS process will use the
%%% new OS time to determine if service requests in the old queue file have
%%% timed-out based on each service request timeout value).
%%% If the fault_isolation service argument is set to 'both',
%%% the original service request timeout value will be used for the
%%% service request send to the destination and the service request
%%% send containing the response.
%%%
%%% If the retry service argument is set higher than 0, any retry attempts
%%% will occur during the time period defined by the timeout of the
%%% service request (i.e., a service request is only retried if its
%%% timeout has not expired).  Any usage of the retry_delay service argument
%%% will contribute to time elapsed during the time period defined by the
%%% timeout of the service request.
%%%
%%% To make sure cloudi_service_queue gets a service request timeout quickly
%%% (i.e., without depending on the timeout elapsing locally, despite the
%%%  timeout being a small value), it is common to set the service
%%% configuration options request_timeout_immediate_max and
%%% response_timeout_immediate_max to 'limit_min'.  If the retry_delay
%%% service argument is used, setting the request_name_lookup
%%% service configuration option to async is best if the service request
%%% destinations are expected to rarely be present
%%% (i.e., only appear when anticipating the receive of data).
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2014-2018 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2014-2018 Michael Truog
%%% @version 1.7.5 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_queue).
-author('mjtruog at protonmail dot com').

-behaviour(cloudi_service).

%% cloudi_service callbacks
-export([cloudi_service_init/4,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/3]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").
-include_lib("cloudi_core/include/cloudi_service.hrl").

-define(DEFAULT_FILE,                   undefined). % see below:
        % required argument, string
        % use "$I" or "${I}" for the process index within the string
        % so unique files are created when the configuration count_process > 1
-define(DEFAULT_FILE_SIZE_LIMIT,       128 * 1024). % limit in kB
        % if fault_isolation is both, the response size may cause
        % the file size to exceed the file_size_limit because the
        % response size isn't able to be anticipated accurately
        % (if the response size is less than or equal to the request size
        %  this is never a problem because the space is reused)
-define(DEFAULT_COMPRESSION,                    0). % zlib compression 0..9
-define(DEFAULT_CHECKSUM,               undefined).
        % Add a checksum to each data chunk in the queue file.
        % A checksum is not used on the whole queue file.
        % This is not necessary if your filesystem already
        % uses checksums (e.g., Btrfs (crc32 variation) and ZFS (sha256)).
        % Without a checksum it isn't possible to be sure the
        % data on disk isn't corrupt (once it is recovered after a crash).
        % All disk writes are done as atomic actions with rename use,
        % so corruption would be due to an OS failure or hardware failure.
        % Valid values are:
        % crc32, md5, ripemd160, sha, sha224, sha256, sha384, sha512
-define(DEFAULT_RETRY,                          0).
-define(DEFAULT_RETRY_DELAY,                    0). % milliseconds
-define(DEFAULT_FAULT_ISOLATION,      destination). % | both

% fault_isolation: destination
-record(destination_request,
    {
        type :: cloudi_service:request_type(),
        name :: cloudi_service:service_name(),
        pattern :: cloudi_service:service_name_pattern(),
        request_info :: cloudi_service:request_info(),
        request :: cloudi_service:request(),
        timeout :: cloudi_service:timeout_value_milliseconds(),
        priority :: cloudi_service:priority(),
        trans_id :: cloudi_service:trans_id(),
        pid :: cloudi_service:source()
    }).
-type request_destination_mode() :: #destination_request{}.
% fault_isolation: both
-record(both_request,
    {
        name :: cloudi_service:service_name(),
        request_info :: cloudi_service:request_info(),
        request :: cloudi_service:request(),
        timeout :: cloudi_service:timeout_value_milliseconds(),
        priority :: cloudi_service:priority(),
        trans_id :: cloudi_service:trans_id(),
        next_name :: cloudi_service:service_name(),
        next_trans_id :: cloudi_service:trans_id()
    }).
-record(both_response,
    {
        name :: cloudi_service:service_name(),
        response_info :: cloudi_service:response_info(),
        response :: cloudi_service:response(),
        timeout :: cloudi_service:timeout_value_milliseconds(),
        priority :: cloudi_service:priority(),
        trans_id :: cloudi_service:trans_id()
    }).
-type request_both_mode() :: #both_request{} | #both_response{}.
% cloudi_write_ahead_logging 
-type request() :: request_destination_mode() |
                   request_both_mode().
-export_type([request/0]).

-record(state,
    {
        service :: pid(),
        file_size_limit :: pos_integer(),
        logging :: cloudi_write_ahead_logging:state(),
        mode :: destination | both,
        retry :: non_neg_integer(),
        retry_delay :: non_neg_integer(),
        retry_f :: cloudi_write_ahead_logging:retry_function()
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, Prefix, _Timeout, Dispatcher) ->
    Defaults = [
        {file,                         ?DEFAULT_FILE},
        {file_size_limit,   ?DEFAULT_FILE_SIZE_LIMIT},
        {compression,           ?DEFAULT_COMPRESSION},
        {checksum,                 ?DEFAULT_CHECKSUM},
        {retry,                       ?DEFAULT_RETRY},
        {retry_delay,           ?DEFAULT_RETRY_DELAY},
        {fault_isolation,   ?DEFAULT_FAULT_ISOLATION}],
    [FilePath, FileSizeLimit, Compression, Checksum, Retry, RetryDelay,
     Mode] = cloudi_proplists:take_values(Defaults, Args),
    false = cloudi_service_name:pattern(Prefix),
    true = is_list(FilePath) andalso is_integer(hd(FilePath)),
    true = is_integer(FileSizeLimit) andalso
           (FileSizeLimit >= 1) andalso (FileSizeLimit =< 18014398509481983),
    true = is_integer(Compression) andalso
           (Compression >= 0) andalso (Compression =< 9),
    true = (Checksum =:= undefined) orelse
           (Checksum =:= crc32) orelse (Checksum =:= md5) orelse
           (Checksum =:= ripemd160) orelse (Checksum =:= sha) orelse
           (Checksum =:= sha224) orelse (Checksum =:= sha256) orelse
           (Checksum =:= sha384) orelse (Checksum =:= sha512),
    true = is_integer(Retry) andalso (Retry >= 0),
    true = is_integer(RetryDelay) andalso
           (RetryDelay >= 0) andalso (RetryDelay =< 4294967295),
    true = ((Retry == 0) andalso (RetryDelay == 0)) orelse
           ((Retry > 0) andalso (RetryDelay >= 0)),
    true = ((Mode =:= destination) orelse (Mode =:= both)),
    I = erlang:integer_to_list(cloudi_service:process_index(Dispatcher)),
    Environment = cloudi_x_trie:store("I", I,
                                      cloudi_environment:lookup()),
    QueueFilePath = cloudi_environment:transform(FilePath, Environment),
    Service = cloudi_service:self(Dispatcher),
    DispatcherPid = cloudi_service:dispatcher(Dispatcher),
    RetryF = fun(T, RetryT) ->
        retry(T, RetryT, Mode, DispatcherPid, Service)
    end,
    Logging = cloudi_write_ahead_logging:new(QueueFilePath,
                                             FileSizeLimit * 1024,
                                             Compression,
                                             Checksum,
                                             RetryF),
    cloudi_service:subscribe(Dispatcher, "*"),
    {ok, #state{service = Service,
                file_size_limit = FileSizeLimit,
                logging = Logging,
                mode = Mode,
                retry = Retry,
                retry_delay = RetryDelay,
                retry_f = RetryF}}.

cloudi_service_handle_request(Type, Name, Pattern, RequestInfo, Request,
                              Timeout, Priority, TransId, Pid,
                              #state{service = Service,
                                     file_size_limit = FileSizeLimit,
                                     logging = Logging,
                                     mode = destination} = State,
                              Dispatcher) ->
    [QueueName] = cloudi_service_name:parse(Name, Pattern),
    ChunkRequest = #destination_request{type = Type,
                                        name = Name,
                                        pattern = Pattern,
                                        request_info = RequestInfo,
                                        request = Request,
                                        timeout = Timeout,
                                        priority = Priority,
                                        trans_id = TransId,
                                        pid = Pid},
    case cloudi_write_ahead_logging:
         store_start(ChunkRequest, Logging) of
        {Chunk, NextLogging} ->
            % PERSISTENCE START:
            % at this point the service request has been persisted and
            % can be restarted
            {ok, QueueTransId} = send_async_active(QueueName,
                                                   RequestInfo, Request,
                                                   Timeout, Priority,
                                                   Dispatcher, Service),
            NewLogging = cloudi_write_ahead_logging:
                         store_end(QueueTransId, Chunk, NextLogging),
            {noreply, State#state{logging = NewLogging}};
        full ->
            ?LOG_WARN("file_size_limit of ~w KB has been reached!",
                      [FileSizeLimit]),
            {reply, <<>>, State}
    end;
cloudi_service_handle_request(_Type, Name, Pattern, RequestInfo, Request,
                              Timeout, Priority, TransId, _Pid,
                              #state{service = Service,
                                     file_size_limit = FileSizeLimit,
                                     logging = Logging,
                                     mode = both} = State,
                              Dispatcher) ->
    RequestMetaData = cloudi_request_info:key_value_parse(RequestInfo),
    case cloudi_key_value:find(<<"service_name">>, RequestMetaData) of
        {ok, NextNameAnyType} ->
            [QueueName] = cloudi_service_name:parse(Name, Pattern),
            NextName = if
                is_binary(NextNameAnyType) ->
                    erlang:binary_to_list(NextNameAnyType);
                is_list(NextNameAnyType), is_integer(hd(NextNameAnyType)) ->
                    NextNameAnyType
            end,
            NextTransId = cloudi_service:trans_id(Dispatcher),
            ChunkRequest = #both_request{name = QueueName,
                                         request_info = RequestInfo,
                                         request = Request,
                                         timeout = Timeout,
                                         priority = Priority,
                                         trans_id = TransId,
                                         next_name = NextName,
                                         next_trans_id = NextTransId},
            case cloudi_write_ahead_logging:
                 store_start(ChunkRequest, Logging) of
                {Chunk, NextLogging} ->
                    % PERSISTENCE START:
                    % at this point the service request has been persisted and
                    % can be restarted
                    {ok, QueueTransId} = send_async_active(QueueName,
                                                           RequestInfo, Request,
                                                           Timeout, Priority,
                                                           Dispatcher, Service),
                    NewLogging = cloudi_write_ahead_logging:
                                 store_end(QueueTransId, Chunk, NextLogging),
                    {reply, NextTransId, State#state{logging = NewLogging}};
                full ->
                    ?LOG_WARN("file_size_limit of ~w KB has been reached!",
                              [FileSizeLimit]),
                    {reply, <<>>, State}
            end;
        error ->
            ?LOG_ERROR("service_name not found in RequestInfo for ~s",
                       [cloudi_trans_id:to_string(TransId, nodash)]),
            {reply, <<>>, State}
    end.

cloudi_service_handle_info(#return_async_active{response_info = ResponseInfo,
                                                response = Response,
                                                timeout = Timeout,
                                                trans_id = QueueTransId},
                           #state{logging = Logging,
                                  mode = destination} = State,
                           Dispatcher) ->
    {#destination_request{type = Type,
                          name = Name,
                          pattern = Pattern,
                          trans_id = TransId,
                          pid = Pid},
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
                           #state{service = Service,
                                  logging = Logging,
                                  mode = both} = State,
                           Dispatcher) ->
    UpdateF = fun
        (#both_request{timeout = Timeout,
                       priority = Priority,
                       next_name = NextName,
                       next_trans_id = NextTransId}) ->
            {NextTransId,
             #both_response{name = NextName,
                            response_info = ResponseInfo,
                            response = Response,
                            timeout = Timeout,
                            priority = Priority,
                            trans_id = NextTransId}};
        (#both_response{}) ->
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
        #both_response{name = NextName,
                       response_info = ResponseInfo,
                       response = Response,
                       timeout = NextTimeout,
                       priority = NextPriority,
                       trans_id = NextTransId} ->
            % deliver the response as a service request
            {ok, _} = send_async_active(NextName, ResponseInfo, Response,
                                        NextTimeout, NextPriority, NextTransId,
                                        Dispatcher, Service),
            {noreply, State#state{logging = NewLogging}}
    end;
cloudi_service_handle_info(#timeout_async_active{trans_id = QueueTransId},
                           #state{logging = Logging,
                                  retry = Retry,
                                  retry_delay = 0,
                                  retry_f = RetryF} = State,
                           _Dispatcher) ->
    NewLogging = cloudi_write_ahead_logging:
                 erase_retry(QueueTransId, Retry, RetryF, Logging),
    % PERSISTENCE END (if a retry didn't occur):
    % at this point the service request/response is no longer persisted
    % due to its timeout value
    {noreply, State#state{logging = NewLogging}};
cloudi_service_handle_info(#timeout_async_active{trans_id = QueueTransId},
                           #state{service = Service,
                                  retry_delay = RetryDelay} = State,
                           _Dispatcher)
    when RetryDelay > 0 ->
    erlang:send_after(RetryDelay, Service, {retry_delay, QueueTransId}),
    {noreply, State};
cloudi_service_handle_info({retry_delay, QueueTransId},
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
cloudi_service_handle_info(Request, State, _Dispatcher) ->
    {stop, cloudi_string:format("Unknown info \"~w\"", [Request]), State}.

cloudi_service_terminate(_Reason, _Timeout, _State) ->
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

-compile({inline, [{retry, 5}]}).
-spec retry(request(),
            Retry :: boolean(),
            Mode :: destination | both,
            Dispatcher :: cloudi_service:dispatcher(),
            Service :: cloudi_service:source()) ->
    {ok, cloudi_service:trans_id()} |
    {error, timeout}.

retry(#destination_request{name = Name,
                           pattern = Pattern,
                           request_info = RequestInfo,
                           request = Request,
                           timeout = Timeout,
                           priority = Priority,
                           trans_id = TransId,
                           pid = Pid}, true, destination,
      Dispatcher, Service) ->
    Age = (cloudi_trans_id:microseconds() -
           cloudi_trans_id:microseconds(TransId)) div 1000 + 100, % milliseconds
    case erlang:is_process_alive(Pid) of
        false ->
            {error, timeout};
        true when Age >= Timeout ->
            {error, timeout};
        true ->
            [QueueName] = cloudi_service_name:parse(Name, Pattern),
            NewTimeout = Timeout - Age,
            send_async_active(QueueName, RequestInfo, Request,
                              NewTimeout, Priority,
                              Dispatcher, Service)
    end;
retry(#both_request{name = QueueName,
                    request_info = RequestInfo,
                    request = Request,
                    timeout = Timeout,
                    priority = Priority,
                    trans_id = TransId}, Retry, both,
      Dispatcher, Service) ->
    Age = (cloudi_trans_id:microseconds() -
           cloudi_trans_id:microseconds(TransId)) div 1000 + 100, % milliseconds
    if
        Age >= Timeout ->
            % an empty response will be handled due to the request timeout
            % (always occurs, even without using retries)
            Service ! #return_async_active{name = QueueName,
                                           pattern = QueueName,
                                           response_info = <<>>,
                                           response = <<>>,
                                           timeout = Timeout,
                                           trans_id = TransId},
            {ok, TransId};
        Retry =:= true ->
            NewTimeout = Timeout - Age,
            send_async_active(QueueName, RequestInfo, Request,
                              NewTimeout, Priority,
                              Dispatcher, Service);
        Retry =:= false ->
            {error, timeout}
    end;
retry(#both_response{name = NextName,
                     response_info = ResponseInfo,
                     response = Response,
                     timeout = NextTimeout,
                     priority = NextPriority,
                     trans_id = NextTransId}, true, both,
      Dispatcher, Service) ->
    send_async_active(NextName, ResponseInfo, Response,
                      NextTimeout, NextPriority, NextTransId,
                      Dispatcher, Service);
retry(_, false, _, _, _) ->
    {error, timeout}.

send_async_active(Name, ResponseInfo, Response, Timeout, Priority, TransId,
                  Dispatcher, Service) ->
    Result = case cloudi_service:get_pid(Dispatcher, Name, Timeout) of
        {ok, PatternPid} ->
            cloudi_service:send_async_active(Dispatcher, Name,
                                             ResponseInfo, Response,
                                             Timeout, Priority,
                                             TransId, PatternPid);
        {error, _} = Error ->
            Error
    end,
    case Result of
        {ok, _} = Success ->
            Success;
        {error, timeout} ->
            Service ! #timeout_async_active{trans_id = TransId},
            {ok, TransId}
    end.

send_async_active(Name, RequestInfo, Request, Timeout, Priority,
                  Dispatcher, Service) ->
    case cloudi_service:send_async_active(Dispatcher, Name,
                                          RequestInfo, Request,
                                          Timeout, Priority) of
        {ok, _} = Success ->
            Success;
        {error, timeout} ->
            TransId = cloudi_service:trans_id(Dispatcher),
            Service ! #timeout_async_active{trans_id = TransId},
            {ok, TransId}
    end.

