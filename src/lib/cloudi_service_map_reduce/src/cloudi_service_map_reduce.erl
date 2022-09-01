%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI (Abstract) Map-Reduce Service==
%%% This module provides an Erlang behaviour for fault-tolerant,
%%% database agnostic map-reduce.  See the hexpi test for example usage.
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2012-2022 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2012-2022 Michael Truog
%%% @version 2.0.5 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_map_reduce).
-author('mjtruog at protonmail dot com').

-behaviour(cloudi_service).

%% external interface
-export([aspect_suspend/1,
         aspect_resume/1,
         elapsed_seconds/0]).

%% cloudi_service callbacks
-export([cloudi_service_init/4,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/3]).

-include_lib("cloudi_core/include/cloudi_constants.hrl").
-include_lib("cloudi_core/include/cloudi_logger.hrl").
-include_lib("cloudi_core/include/cloudi_service.hrl").

-define(DEFAULT_MAP_REDUCE_MODULE,            undefined).
-define(DEFAULT_MAP_REDUCE_ARGUMENTS,                []).
-define(DEFAULT_NAME,                      "controller").
-define(DEFAULT_CONCURRENCY,                        1.0).
        % logical cpu count multiplier
        % (calculated the same way as count_process and count_thread in
        %  service configuration).
-define(DEFAULT_LOG_EXECUTION_TIME,                true).
-define(DEFAULT_RETRY,                                3).
        % Max retries with a Timeout value of ?TIMEOUT_MAX_ERLANG
        % before exiting with retry_max.
        % The Timeout value needs to be provided in the map_send_args()
        % and is typically increasing due to service request failures.
-define(DEFAULT_RETRY_DELAY,                          0). % milliseconds

-type map_send_args() :: nonempty_list().
-export_type([map_send_args/0]).

-record(map_send,
    {
        send_args
            :: map_send_args(),
        retry_count
            :: non_neg_integer()
    }).

-record(state,
    {
        service
            :: pid(),
        map_reduce_module
            :: module(),
        map_reduce_state
            :: any(),
        map_reduce_name
            :: string(),
        map_count
            :: pos_integer(),
        log_execution_time
            :: boolean(),
        retry
            :: non_neg_integer(),
        retry_delay
            :: non_neg_integer(),
        map_requests
            :: #{cloudi_service:trans_id() := #map_send{}},
        time_running
            :: cloudi_timestamp:seconds_monotonic(),
        suspended = false
            :: boolean(),
        elapsed_seconds = 0
            :: non_neg_integer()
    }).

-record(init_state,
    {
        service
            :: pid(),
        info_queued = []
            :: list()
    }).

-record(init_begin,
    {
        service
            :: pid(),
        prefix
            :: string(),
        timeout
            :: cloudi_service_api:timeout_initialize_value_milliseconds(),
        map_reduce_module
            :: module(),
        map_reduce_args
            :: list(),
        map_reduce_name
            :: string(),
        concurrency
            :: number(),
        log_execution_time
            :: boolean(),
        retry
            :: non_neg_integer(),
        retry_delay
            :: non_neg_integer(),
        time_start
            :: cloudi_timestamp:seconds_monotonic()
    }).

-record(init_end,
    {
        state
            :: undefined | #state{},
        error = undefined
            :: any()
    }).

-define(ELAPSED_SECONDS_PDICT_KEY, cloudi_service_map_reduce_elapsed_seconds).

%%%------------------------------------------------------------------------
%%% Callback functions from behavior
%%%------------------------------------------------------------------------

-callback cloudi_service_map_reduce_new(ModuleReduceArgs :: list(),
                                        Count :: pos_integer(),
                                        Prefix :: string(),
                                        Timeout ::
                                        cloudi_service_api:
                                        timeout_initialize_value_milliseconds(),
                                        Dispatcher :: pid()) ->
    {'ok', ModuleReduceState :: any()} |
    {'error', Reason :: any()}.

-callback cloudi_service_map_reduce_send(ModuleReduceState :: any(),
                                         Dispatcher :: pid()) ->
    {'ok', SendArgs :: map_send_args(), ModuleReduceStateNew :: any()} |
    {'done', ModuleReduceStateNew :: any()} |
    {'error', Reason :: any()}.

-callback cloudi_service_map_reduce_resend(SendArgs :: map_send_args(),
                                           ModuleReduceState :: any()) ->
    {'ok', SendArgsNew :: map_send_args(), ModuleReduceStateNew :: any()} |
    {'error', Reason :: any()}.

-callback cloudi_service_map_reduce_recv(SendArgs :: map_send_args(),
                                         ResponseInfo :: any(),
                                         Response :: any(),
                                         Timeout :: non_neg_integer(),
                                         TransId :: binary(),
                                         ModuleReduceState :: any(),
                                         Dispatcher :: pid()) ->
    {'ok', ModuleReduceStateNew :: any()} |
    {'done', ModuleReduceStateNew :: any()} |
    {'error', Reason :: any()}.

-callback cloudi_service_map_reduce_info(Request :: any(),
                                         ModuleReduceState :: any(),
                                         Dispatcher :: pid()) ->
    {'ok', ModuleReduceStateNew :: any()} |
    {'done', ModuleReduceStateNew :: any()} |
    {'error', Reason :: any()}.

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Function for aspects_suspend service configuration option.===
%% Add as {cloudi_service_map_reduce, aspect_suspend}.
%% @end
%%-------------------------------------------------------------------------

-spec aspect_suspend(State :: #state{}) ->
    {ok, #state{}}.

aspect_suspend(#state{map_reduce_module = MapReduceModule,
                      map_reduce_name = MapReduceName,
                      time_running = TimeRunningStart,
                      suspended = false,
                      elapsed_seconds = ElapsedSeconds} = State) ->
    TimeRunningEnd = cloudi_timestamp:seconds_monotonic(),
    ElapsedSecondsNew = ElapsedSeconds + (TimeRunningEnd - TimeRunningStart),
    ?LOG_INFO("~s ~ts suspended",
              [MapReduceModule, cloudi_service_name:utf8(MapReduceName)]),
    {ok,
     State#state{suspended = true,
                 elapsed_seconds = ElapsedSecondsNew}}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Function for aspects_resume service configuration option.===
%% Add as {cloudi_service_map_reduce, aspect_resume}.
%% @end
%%-------------------------------------------------------------------------

-spec aspect_resume(State :: #state{}) ->
    {ok, #state{}}.

aspect_resume(#state{map_reduce_module = MapReduceModule,
                     map_reduce_name = MapReduceName,
                     suspended = true} = State) ->
    TimeRunningStart = cloudi_timestamp:seconds_monotonic(),
    ?LOG_INFO("~s ~ts resumed",
              [MapReduceModule, cloudi_service_name:utf8(MapReduceName)]),
    {ok,
     State#state{time_running = TimeRunningStart,
                 suspended = false}}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the current elapsed seconds in a Map-Reduce callback function.===
%% @end
%%-------------------------------------------------------------------------

-spec elapsed_seconds() ->
    cloudi_service_api:seconds().

elapsed_seconds() ->
    {TimeRunningStart,
     ElapsedSeconds} = erlang:get(?ELAPSED_SECONDS_PDICT_KEY),
    ElapsedSeconds + (cloudi_timestamp:seconds_monotonic() - TimeRunningStart).

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, Prefix, Timeout, Dispatcher) ->
    Defaults = [
        {map_reduce,             ?DEFAULT_MAP_REDUCE_MODULE},
        {map_reduce_args,        ?DEFAULT_MAP_REDUCE_ARGUMENTS},
        {name,                   ?DEFAULT_NAME},
        {concurrency,            ?DEFAULT_CONCURRENCY},
        {log_execution_time,     ?DEFAULT_LOG_EXECUTION_TIME},
        {retry,                  ?DEFAULT_RETRY},
        {retry_delay,            ?DEFAULT_RETRY_DELAY}],
    [MapReduceModule, MapReduceArgs, Name, Concurrency, LogExecutionTime,
     Retry, RetryDelay0] =
        cloudi_proplists:take_values(Defaults, Args),
    TimeStart = cloudi_timestamp:seconds_monotonic(),
    true = is_atom(MapReduceModule) andalso (MapReduceModule /= undefined),
    true = is_list(MapReduceArgs),
    true = is_number(Concurrency) andalso (Concurrency > 0),
    true = is_boolean(LogExecutionTime),
    true = is_integer(Retry) andalso (Retry >= 0),
    RetryDelayN = cloudi_args_type:
                  period_to_milliseconds(RetryDelay0, 0, ?TIMEOUT_MAX_ERLANG),
    true = ((Retry == 0) andalso (RetryDelayN == 0)) orelse
           ((Retry > 0) andalso (RetryDelayN >= 0)),
    case application:load(MapReduceModule) of
        ok ->
            ok = cloudi_x_reltool_util:application_start(MapReduceModule,
                                                         [], Timeout);
        {error, {already_loaded, MapReduceModule}} ->
            ok = cloudi_x_reltool_util:application_start(MapReduceModule,
                                                         [], Timeout);
        {error, _} ->
            ok = cloudi_x_reltool_util:module_loaded(MapReduceModule)
    end,
    % cloudi_service_init/4 is always executed by the service process
    Service = self(),
    Service ! #init_begin{service = Service,
                          prefix = Prefix,
                          timeout = Timeout,
                          map_reduce_module = MapReduceModule,
                          map_reduce_args = MapReduceArgs,
                          map_reduce_name = Name,
                          concurrency = Concurrency,
                          log_execution_time = LogExecutionTime,
                          retry = Retry,
                          retry_delay = RetryDelayN,
                          time_start = TimeStart},
    ok = cloudi_service:subscribe(Dispatcher, Name),
    {ok, #init_state{service = Service}}.

cloudi_service_handle_info(#init_begin{service = Service} = InitBegin,
                           #init_state{} = InitState,
                           Dispatcher) ->
    % cloudi_service_map_reduce_new/5 execution occurs outside of
    % cloudi_service_init/4 to allow send_sync and recv_async function calls
    % (only algorithmic initialization should be occurring).
    % Initialization is done in a temporary process so there is
    % no blocking problem (with send_sync or recv_async)
    % if duo_mode == true.  No timeout is enforced on
    % cloudi_service_map_reduce_new/5 execution.
    _ = erlang:spawn(fun() ->
        true = erlang:link(Service),
        case init(InitBegin, Dispatcher) of
            {noreply, State} ->
                Service ! #init_end{state = State},
                ok;
            {stop, Reason, State} ->
                true = Reason /= undefined,
                Service ! #init_end{state = State,
                                    error = Reason},
                ok
        end,
        true = erlang:unlink(Service)
    end),
    {noreply, InitState};
cloudi_service_handle_info(#init_end{} = InitEnd,
                           #init_state{} = InitState,
                           _Dispatcher) ->
    init_end(InitEnd, InitState);
cloudi_service_handle_info(Request,
                           #init_state{info_queued = InfoQueued} = InitState,
                           _Dispatcher) ->
    {noreply, InitState#init_state{info_queued = [Request | InfoQueued]}};
cloudi_service_handle_info(#timeout_async_active{trans_id = TransId} = Request,
                           #state{map_requests = MapRequests} = State,
                           Dispatcher) ->
    case maps:take(TransId, MapRequests) of
        {MapRequest, MapRequestsNew} ->
            map_retry(MapRequest,
                      State#state{map_requests = MapRequestsNew},
                      Dispatcher);
        error ->
            map_info(Request, State, Dispatcher)
    end;
cloudi_service_handle_info({cloudi_service_map_reduce_retry, MapRequest},
                           State, Dispatcher) ->
    map_resend(MapRequest, State, Dispatcher);
cloudi_service_handle_info(#return_async_active{response_info = ResponseInfo,
                                                response = Response,
                                                timeout = Timeout,
                                                trans_id = TransId} = Request,
                           #state{map_reduce_module = MapReduceModule,
                                  map_reduce_state = MapReduceState,
                                  map_requests = MapRequests,
                                  time_running = TimeRunningStart,
                                  elapsed_seconds = ElapsedSeconds} = State,
                           Dispatcher) ->
    case maps:take(TransId, MapRequests) of
        {#map_send{send_args = [_ | SendArgs]},
         MapRequestsNew} ->
            ok = elapsed_seconds_set(TimeRunningStart, ElapsedSeconds),
            case MapReduceModule:
                 cloudi_service_map_reduce_recv([Dispatcher | SendArgs],
                                                ResponseInfo, Response,
                                                Timeout, TransId,
                                                MapReduceState, Dispatcher) of
                {ok, MapReduceStateNew} ->
                    StateNew = State#state{map_reduce_state = MapReduceStateNew,
                                           map_requests = MapRequestsNew},
                    map_check_continue(StateNew, Dispatcher);
                {done, MapReduceStateNew} ->
                    StateNew = State#state{map_reduce_state = MapReduceStateNew,
                                           map_requests = MapRequestsNew},
                    map_check_done(StateNew);
                {error, _} = Error ->
                    {stop, Error, State}
            end;
        error ->
            map_info(Request, State, Dispatcher)
    end;
cloudi_service_handle_info(Request, State, Dispatcher) ->
    map_info(Request, State, Dispatcher).

cloudi_service_terminate(shutdown, _Timeout,
                         #state{log_execution_time = true,
                                map_requests = #{},
                                time_running = TimeRunningStart,
                                suspended = Suspended,
                                elapsed_seconds = ElapsedSeconds}) ->
    ElapsedSecondsNew = if
        Suspended =:= true ->
            ElapsedSeconds;
        Suspended =:= false ->
            TimeRunningEnd = cloudi_timestamp:seconds_monotonic(),
            ElapsedSeconds + (TimeRunningEnd - TimeRunningStart)
    end,
    ?LOG_INFO("total time taken was ~p hours",
              [hours_elapsed(ElapsedSecondsNew)]),
    ok;
cloudi_service_terminate(_Reason, _Timeout, _State) ->
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

init(#init_begin{service = Service,
                 prefix = Prefix,
                 timeout = Timeout,
                 map_reduce_module = MapReduceModule,
                 map_reduce_args = MapReduceArgs,
                 map_reduce_name = MapReduceName,
                 concurrency = Concurrency,
                 log_execution_time = LogExecutionTime,
                 retry = Retry,
                 retry_delay = RetryDelay,
                 time_start = TimeStart},
     Dispatcher) ->
    MapCount = cloudi_concurrency:count(Concurrency),
    ok = elapsed_seconds_set(TimeStart),
    case MapReduceModule:
         cloudi_service_map_reduce_new(MapReduceArgs, MapCount,
                                       Prefix, Timeout, Dispatcher) of
        {ok, MapReduceState} ->
            case map_send(MapCount, #{},
                          MapReduceModule, MapReduceState, Dispatcher) of
                {ok, MapRequests, MapReduceStateNew} ->
                    {noreply,
                     #state{service = Service,
                            map_reduce_module = MapReduceModule,
                            map_reduce_state = MapReduceStateNew,
                            map_reduce_name = MapReduceName,
                            map_count = MapCount,
                            log_execution_time = LogExecutionTime,
                            retry = Retry,
                            retry_delay = RetryDelay,
                            map_requests = MapRequests,
                            time_running = TimeStart}};
                {error, _} = Error ->
                    {stop, Error, undefined}
            end;
        {error, _} = Error ->
            {stop, Error, undefined}
    end.

init_end(#init_end{state = State,
                   error = undefined},
         #init_state{service = Service,
                     info_queued = InfoQueued}) ->
    ok = init_end_send(lists:reverse(InfoQueued), Service),
    {noreply, State};
init_end(#init_end{state = State,
                   error = Error},
         #init_state{}) ->
    {stop, Error, State}.

init_end_send([], _) ->
    ok;
init_end_send([Request | InfoQueued], Service) ->
    Service ! Request,
    init_end_send(InfoQueued, Service).

map_retry(MapRequest, State, Dispatcher) ->
    case retry(MapRequest, State) of
        true ->
            {noreply, State};
        false ->
            map_resend(MapRequest, State, Dispatcher)
    end.

map_resend(#map_send{send_args = [_ | SendArgsTail],
                     retry_count = RetryCount},
           #state{map_reduce_module = MapReduceModule,
                  map_reduce_state = MapReduceState,
                  map_requests = MapRequests,
                  time_running = TimeRunningStart,
                  elapsed_seconds = ElapsedSeconds} = State, Dispatcher) ->
    SendArgs = [Dispatcher | SendArgsTail],
    ok = elapsed_seconds_set(TimeRunningStart, ElapsedSeconds),
    case MapReduceModule:
         cloudi_service_map_reduce_resend(SendArgs, MapReduceState) of
        {ok, SendArgsNew, MapReduceStateNew} ->
            case map_send_request(SendArgsNew, RetryCount, MapRequests) of
                {ok, MapRequestsNew} ->
                    {noreply,
                     State#state{map_reduce_state = MapReduceStateNew,
                                 map_requests = MapRequestsNew}};
                {error, _} = Error ->
                    {stop, Error, State}
            end;
        {error, _} = Error ->
            {stop, Error, State}
    end.

map_info(Request, State, Dispatcher) ->
    #state{map_reduce_module = MapReduceModule,
           map_reduce_state = MapReduceState,
           time_running = TimeRunningStart,
           elapsed_seconds = ElapsedSeconds} = State,
    ok = elapsed_seconds_set(TimeRunningStart, ElapsedSeconds),
    case MapReduceModule:
         cloudi_service_map_reduce_info(Request, MapReduceState, Dispatcher) of
        {ok, MapReduceStateNew} ->
            {noreply, State#state{map_reduce_state = MapReduceStateNew}};
        {done, MapReduceStateNew} ->
            map_check_done(State#state{map_reduce_state = MapReduceStateNew});
        {error, _} = Error ->
            {stop, Error, State}
    end.

map_send(MapRequests, MapReduceModule, MapReduceState, Dispatcher) ->
    map_send(1, MapRequests, MapReduceModule, MapReduceState, Dispatcher).

map_send(0, MapRequests, _MapReduceModule, MapReduceState, _Dispatcher) ->
    {ok, MapRequests, MapReduceState};
map_send(Count, MapRequests, MapReduceModule, MapReduceState, Dispatcher) ->
    case MapReduceModule:
         cloudi_service_map_reduce_send(MapReduceState, Dispatcher) of
        {ok, SendArgs, MapReduceStateNew} ->
            case map_send_request(SendArgs, MapRequests) of
                {ok, MapRequestsNew} ->
                    map_send(Count - 1, MapRequestsNew,
                             MapReduceModule, MapReduceStateNew, Dispatcher);
                {error, _} = Error ->
                    Error
            end;
        {done, MapReduceStateNew} ->
            {ok, MapRequests, MapReduceStateNew};
        {error, _} = Error ->
            Error
    end.

map_send_request(SendArgs, MapRequests) ->
    map_send_request(SendArgs, 0, MapRequests).

map_send_request(SendArgs, RetryCount, MapRequests) ->
    case erlang:apply(cloudi_service, send_async_active, SendArgs) of
        {ok, TransId} ->
            MapRequestsNew = maps:put(TransId,
                                      #map_send{send_args = SendArgs,
                                                retry_count = RetryCount},
                                      MapRequests),
            {ok, MapRequestsNew};
        {error, _} = Error ->
            Error
    end.

map_check_continue(#state{map_reduce_module = MapReduceModule,
                          map_reduce_state = MapReduceState,
                          map_requests = MapRequests} = State, Dispatcher) ->
    case map_send(MapRequests, MapReduceModule, MapReduceState, Dispatcher) of
        {ok, MapRequestsNew, MapReduceStateNew} ->
            {noreply,
             State#state{map_reduce_state = MapReduceStateNew,
                         map_requests = MapRequestsNew}};
        {error, _} = Error ->
            {stop, Error, State}
    end.

map_check_done(#state{map_requests = MapRequests} = State) ->
    case maps:size(MapRequests) of
        0 ->
            {stop, shutdown, State};
        _ ->
            {noreply, State}
    end.

retry(#map_send{send_args = [_Dispatcher, _Name, _Request,
                             ?TIMEOUT_MAX_ERLANG],
                retry_count = RetryCount} = MapRequest,
      #state{service = Service,
             retry = Retry,
             retry_delay = RetryDelay}) ->
    if
        RetryCount < Retry ->
            retry_delay(RetryDelay, Service, MapRequest);
        true ->
            erlang:exit(retry_max)
    end;
retry(#map_send{send_args = [_Dispatcher, _Name, _Request,
                             ?TIMEOUT_MAX_ERLANG, _PatternPid],
                retry_count = RetryCount} = MapRequest,
      #state{service = Service,
             retry = Retry,
             retry_delay = RetryDelay}) ->
    if
        RetryCount < Retry ->
            retry_delay(RetryDelay, Service, MapRequest);
        true ->
            erlang:exit(retry_max)
    end;
retry(#map_send{send_args = [_Dispatcher, _Name, _RequestInfo, _Request,
                             ?TIMEOUT_MAX_ERLANG, _Priority],
                retry_count = RetryCount} = MapRequest,
      #state{service = Service,
             retry = Retry,
             retry_delay = RetryDelay}) ->
    if
        RetryCount < Retry ->
            retry_delay(RetryDelay, Service, MapRequest);
        true ->
            erlang:exit(retry_max)
    end;
retry(#map_send{send_args = [_Dispatcher, _Name, _RequestInfo, _Request,
                             ?TIMEOUT_MAX_ERLANG, _Priority, _PatternPid],
                retry_count = RetryCount} = MapRequest,
      #state{service = Service,
             retry = Retry,
             retry_delay = RetryDelay}) ->
    if
        RetryCount < Retry ->
            retry_delay(RetryDelay, Service, MapRequest);
        true ->
            erlang:exit(retry_max)
    end;
retry(_, _) ->
    false.

retry_delay(RetryDelay, Service,
            #map_send{retry_count = RetryCount} = MapRequest) ->
    MapRequestNew = MapRequest#map_send{retry_count = RetryCount + 1},
    _ = erlang:send_after(RetryDelay, Service,
                          {cloudi_service_map_reduce_retry, MapRequestNew}),
    true.

elapsed_seconds_set(TimeRunningStart) ->
    elapsed_seconds_set(TimeRunningStart, 0).

elapsed_seconds_set(TimeRunningStart, ElapsedSeconds) ->
    _ = erlang:put(?ELAPSED_SECONDS_PDICT_KEY,
                   {TimeRunningStart, ElapsedSeconds}),
    ok.

hours_elapsed(ElapsedSeconds) ->
    erlang:round((ElapsedSeconds / (60 * 60)) * 10) / 10.

