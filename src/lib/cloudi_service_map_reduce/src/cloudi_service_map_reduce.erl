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
%%% Copyright (c) 2012-2021 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2012-2021 Michael Truog
%%% @version 2.0.2 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_map_reduce).
-author('mjtruog at protonmail dot com').

-behaviour(cloudi_service).

%% external interface
-export([resume/2,
         resume/3,
         suspend/2,
         suspend/3,
         aspect_suspend/1,
         aspect_resume/1]).

%% cloudi_service callbacks
-export([cloudi_service_init/4,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/3]).

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
        % Max retries with a Timeout value of timeout_max
        % before cloudi_service_map_reduce_resend is called.
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
        timeout_max
            :: cloudi_service:timeout_value_milliseconds(),
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
        timeout_max
            :: cloudi_service:timeout_value_milliseconds(),
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

-type agent() :: cloudi:agent().
-type service_name() :: cloudi:service_name().
-type timeout_milliseconds() :: cloudi:timeout_milliseconds().
-type module_response(Result) ::
    {{ok, Result}, AgentNew :: agent()} |
    {{error, cloudi:error_reason()}, AgentNew :: agent()}.

-spec resume(Agent :: agent(),
             Prefix :: service_name()) ->
    module_response(ok | {error, any()}).

resume(Agent, Prefix) ->
    cloudi:send_sync(Agent, Prefix, resume).

-spec resume(Agent :: agent(),
             Prefix :: service_name(),
             Timeout :: timeout_milliseconds()) ->
    module_response(ok | {error, any()}).

resume(Agent, Prefix, Timeout) ->
    cloudi:send_sync(Agent, Prefix, resume, Timeout).

-spec suspend(Agent :: agent(),
              Prefix :: service_name()) ->
    module_response(ok | {error, any()}).

suspend(Agent, Prefix) ->
    cloudi:send_sync(Agent, Prefix, suspend).

-spec suspend(Agent :: agent(),
              Prefix :: service_name(),
              Timeout :: timeout_milliseconds()) ->
    module_response(ok | {error, any()}).

suspend(Agent, Prefix, Timeout) ->
    cloudi:send_sync(Agent, Prefix, suspend, Timeout).

-spec aspect_suspend(State :: #state{}) ->
    {ok, #state{}}.

aspect_suspend(#state{map_reduce_module = MapReduceModule,
                      map_reduce_name = MapReduceName,
                      time_running = TimeRunningStart,
                      suspended = false,
                      elapsed_seconds = ElapsedSeconds} = State) ->
    TimeRunningEnd = cloudi_timestamp:seconds_monotonic(),
    ElapsedSecondsNew = ElapsedSeconds + (TimeRunningEnd - TimeRunningStart),
    ?LOG_INFO("~s ~ts suspended", [MapReduceModule, MapReduceName]),
    {ok,
     State#state{suspended = true,
                 elapsed_seconds = ElapsedSecondsNew}};
aspect_suspend(#state{suspended = true} = State) ->
    {ok, State}.

-spec aspect_resume(State :: #state{}) ->
    {ok, #state{}}.

aspect_resume(#state{service = Service,
                     map_reduce_module = MapReduceModule,
                     map_reduce_name = MapReduceName,
                     suspended = true} = State) ->
    TimeRunningStart = cloudi_timestamp:seconds_monotonic(),
    Service ! cloudi_service_map_reduce_resumed,
    ?LOG_INFO("~s ~ts resumed", [MapReduceModule, MapReduceName]),
    {ok,
     State#state{time_running = TimeRunningStart,
                 suspended = false}}.

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
     Retry, RetryDelay] =
        cloudi_proplists:take_values(Defaults, Args),
    TimeStart = cloudi_timestamp:seconds_monotonic(),
    true = is_atom(MapReduceModule) andalso (MapReduceModule /= undefined),
    true = is_list(MapReduceArgs),
    true = is_number(Concurrency) andalso (Concurrency > 0),
    true = is_boolean(LogExecutionTime),
    true = is_integer(Retry) andalso (Retry >= 0),
    true = is_integer(RetryDelay) andalso
           (RetryDelay >= 0) andalso (RetryDelay =< 4294967295),
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
    TimeoutMax = cloudi_service:timeout_max(Dispatcher),
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
                          retry_delay = RetryDelay,
                          timeout_max = TimeoutMax,
                          time_start = TimeStart},
    ok = cloudi_service:subscribe(Dispatcher, Name),
    {ok, #init_state{service = Service}}.

cloudi_service_handle_request(_RequestType, _Name, _Pattern,
                              _RequestInfo, Request,
                              _Timeout, _Priority, _TransId, _Pid,
                              State, _Dispatcher) ->
    request(Request, State).

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
        {#map_send{send_args = [_ | SendArgs],
                   retry_count = RetryCount},
         MapRequestsNew} ->
            map_resend([Dispatcher | SendArgs], RetryCount,
                       State#state{map_requests = MapRequestsNew});
        error ->
            map_info(Request, State, Dispatcher)
    end;
cloudi_service_handle_info(#return_async_active{response_info = ResponseInfo,
                                                response = Response,
                                                timeout = Timeout,
                                                trans_id = TransId} = Request,
                           #state{map_reduce_module = MapReduceModule,
                                  map_reduce_state = MapReduceState,
                                  map_requests = MapRequests} = State,
                           Dispatcher) ->
    case maps:take(TransId, MapRequests) of
        {#map_send{send_args = [_ | SendArgs]},
         MapRequestsNew} ->
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
cloudi_service_handle_info(cloudi_service_map_reduce_resumed,
                           #state{map_reduce_module = MapReduceModule,
                                  map_reduce_state = MapReduceState,
                                  map_count = MapCount,
                                  map_requests = MapRequests,
                                  suspended = Suspended} = State,
                           Dispatcher) ->
    if
        Suspended =:= true ->
            {noreply, State};
        Suspended =:= false ->
            case map_send(MapCount - maps:size(MapRequests), MapRequests,
                          MapReduceModule, MapReduceState, Dispatcher) of
                {ok, MapRequestsNew, MapReduceStateNew} ->
                    {noreply,
                     State#state{map_reduce_state = MapReduceStateNew,
                                 map_requests = MapRequestsNew}};
                {error, _} = Error ->
                    {stop, Error, State}
            end
    end;
cloudi_service_handle_info(Request, State, Dispatcher) ->
    map_info(Request, State, Dispatcher).

cloudi_service_terminate(shutdown, _Timeout,
                         #state{log_execution_time = true,
                                map_requests = #{},
                                time_running = TimeRunningStart,
                                elapsed_seconds = ElapsedSeconds}) ->
    TimeRunningEnd = cloudi_timestamp:seconds_monotonic(),
    ElapsedSecondsNew = ElapsedSeconds + (TimeRunningEnd - TimeRunningStart),
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
                 timeout_max = TimeoutMax,
                 time_start = TimeStart},
     Dispatcher) ->
    MapCount = cloudi_concurrency:count(Concurrency),
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
                            timeout_max = TimeoutMax,
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

request(_, #init_state{} = InitState) ->
    {reply, {error, init_pending}, InitState};
request(suspend,
        #state{suspended = Suspended} = State) ->
    if
        Suspended =:= true ->
            {reply, {error, already_suspended}, State};
        Suspended =:= false ->
            {ok, StateNew} = aspect_suspend(State),
            {reply, ok, StateNew}
    end;
request(resume,
        #state{suspended = Suspended} = State) ->
    if
        Suspended =:= true ->
            {ok, StateNew} = aspect_resume(State),
            {reply, ok, StateNew};
        Suspended =:= false ->
            {reply, {error, already_resumed}, State}
    end.

map_resend(SendArgs, RetryCount,
           #state{map_reduce_module = MapReduceModule,
                  map_reduce_state = MapReduceState,
                  map_requests = MapRequests} = State) ->
    RetryCountNew = retry(SendArgs, RetryCount, State),
    case MapReduceModule:
         cloudi_service_map_reduce_resend(SendArgs, MapReduceState) of
        {ok, SendArgsNew, MapReduceStateNew} ->
            case map_send_request(SendArgsNew, RetryCountNew, MapRequests) of
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
           map_reduce_state = MapReduceState} = State,
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

map_check_continue(#state{suspended = true} = State, _Dispatcher) ->
    {noreply, State};
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

retry([_Dispatcher, _Name, _Request,
       TimeoutMax], RetryCount,
      #state{retry = Retry,
             retry_delay = RetryDelay,
             timeout_max = TimeoutMax}) ->
    if
        RetryCount < Retry ->
            ok = retry_delay(RetryDelay),
            RetryCount + 1;
        true ->
            erlang:exit(retry_max)
    end;
retry([_Dispatcher, _Name, _Request,
       TimeoutMax, _PatternPid], RetryCount,
      #state{retry = Retry,
             retry_delay = RetryDelay,
             timeout_max = TimeoutMax}) ->
    if
        RetryCount < Retry ->
            ok = retry_delay(RetryDelay),
            RetryCount + 1;
        true ->
            erlang:exit(retry_max)
    end;
retry([_Dispatcher, _Name, _RequestInfo, _Request,
       TimeoutMax, _Priority], RetryCount,
      #state{retry = Retry,
             retry_delay = RetryDelay,
             timeout_max = TimeoutMax}) ->
    if
        RetryCount < Retry ->
            ok = retry_delay(RetryDelay),
            RetryCount + 1;
        true ->
            erlang:exit(retry_max)
    end;
retry([_Dispatcher, _Name, _RequestInfo, _Request,
       TimeoutMax, _Priority, _PatternPid], RetryCount,
      #state{retry = Retry,
             retry_delay = RetryDelay,
             timeout_max = TimeoutMax}) ->
    if
        RetryCount < Retry ->
            ok = retry_delay(RetryDelay),
            RetryCount + 1;
        true ->
            erlang:exit(retry_max)
    end;
retry(_, RetryCount, _) ->
    RetryCount.

retry_delay(0) ->
    ok;
retry_delay(RetryDelay) ->
    receive after RetryDelay -> ok end.

hours_elapsed(ElapsedSeconds) ->
    erlang:round((ElapsedSeconds / (60 * 60)) * 10) / 10.

