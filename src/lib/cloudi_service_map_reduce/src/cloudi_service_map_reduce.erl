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
%%% Copyright (c) 2012-2018 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2012-2018 Michael Truog
%%% @version 1.7.4 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_map_reduce).
-author('mjtruog at protonmail dot com').

-behaviour(cloudi_service).

%% cloudi_service callbacks
-export([cloudi_service_init/4,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/3]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").
-include_lib("cloudi_core/include/cloudi_service.hrl").

-define(DEFAULT_MAP_REDUCE_MODULE,     undefined).
-define(DEFAULT_MAP_REDUCE_ARGUMENTS,         []).
-define(DEFAULT_CONCURRENCY,                 1.0). % schedulers multiplier
-define(DEFAULT_LOG_EXECUTION_TIME,         true).

-type map_send_args() :: list().
-export_type([map_send_args/0]).

-record(state,
    {
        time_start :: cloudi_timestamp:seconds_monotonic(),
        log_execution_time :: boolean(),
        map_reduce_module :: module(),
        map_reduce_state :: any(),
        map_count :: pos_integer(),
        map_requests :: #{cloudi_service:trans_id() := map_send_args()}
    }).

-record(init_begin,
    {
        service :: pid(),
        prefix :: string(),
        timeout :: cloudi_service_api:timeout_initialize_value_milliseconds(),
        time_start :: cloudi_timestamp:seconds_monotonic(),
        log_execution_time :: boolean(),
        map_reduce_module :: module(),
        map_reduce_args :: list(),
        concurrency :: number()
    }).

-record(init_end,
    {
        state :: undefined | #state{},
        error = undefined :: any()
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

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
    {'ok', SendArgs :: map_send_args(), NewModuleReduceState :: any()} |
    {'done', NewModuleReduceState :: any()} |
    {'error', Reason :: any()}.

-callback cloudi_service_map_reduce_resend(SendArgs :: map_send_args(),
                                           ModuleReduceState :: any()) ->
    {'ok', NewSendArgs :: map_send_args(), NewModuleReduceState :: any()} |
    {'error', Reason :: any()}.

-callback cloudi_service_map_reduce_recv(SendArgs :: map_send_args(),
                                         ResponseInfo :: any(),
                                         Response :: any(),
                                         Timeout :: non_neg_integer(),
                                         TransId :: binary(),
                                         ModuleReduceState :: any(),
                                         Dispatcher :: pid()) ->
    {'ok', NewModuleReduceState :: any()} |
    {'done', NewModuleReduceState :: any()} |
    {'error', Reason :: any()}.

-callback cloudi_service_map_reduce_info(Request :: any(),
                                         ModuleReduceState :: any(),
                                         Dispatcher :: pid()) ->
    {'ok', NewModuleReduceState :: any()} |
    {'done', NewModuleReduceState :: any()} |
    {'error', Reason :: any()}.

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, Prefix, Timeout, _Dispatcher) ->
    Defaults = [
        {map_reduce,             ?DEFAULT_MAP_REDUCE_MODULE},
        {map_reduce_args,        ?DEFAULT_MAP_REDUCE_ARGUMENTS},
        {concurrency,            ?DEFAULT_CONCURRENCY},
        {log_execution_time,     ?DEFAULT_LOG_EXECUTION_TIME}],
    [MapReduceModule, MapReduceArgs, Concurrency, LogExecutionTime] =
        cloudi_proplists:take_values(Defaults, Args),
    TimeStart = cloudi_timestamp:seconds_monotonic(),
    true = is_atom(MapReduceModule) andalso (MapReduceModule /= undefined),
    true = is_list(MapReduceArgs),
    true = is_number(Concurrency) andalso (Concurrency > 0),
    true = is_boolean(LogExecutionTime),
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
    Service !  #init_begin{service = Service,
                           prefix = Prefix,
                           timeout = Timeout,
                           time_start = TimeStart,
                           log_execution_time = LogExecutionTime,
                           map_reduce_module = MapReduceModule,
                           map_reduce_args = MapReduceArgs,
                           concurrency = Concurrency},
    {ok, undefined}.

cloudi_service_handle_request(_Type, _Name, _Pattern, _RequestInfo, _Request,
                              _Timeout, _Priority, _TransId, _Pid,
                              State, _Dispatcher) ->
    {reply, <<>>, State}.

cloudi_service_handle_info(#init_begin{service = Service} = InitBegin,
                           undefined, Dispatcher) ->
    % cloudi_service_map_reduce_new/5 execution occurs outside of
    % cloudi_service_init/4 to allow send_sync and recv_async function calls
    % because no Erlang process linking/spawning/etc. should be occurring,
    % only algorithmic initialization.  Initialization is done in a temporary
    % process so there is no blocking problem (with send_sync or recv_async)
    % if duo_mode == true.  No timeout is enforced on
    % cloudi_service_map_reduce_new/5 execution.
    _ = erlang:spawn_link(fun() ->
        case init(InitBegin, Dispatcher) of
            {noreply, State} ->
                Service ! #init_end{state = State};
            {stop, Reason, State} when Reason /= undefined ->
                Service ! #init_end{state = State,
                                    error = Reason}
        end,
        true = erlang:unlink(Service)
    end),
    {noreply, undefined};

cloudi_service_handle_info(#init_end{state = State,
                                     error = Error},
                           undefined, _Dispatcher) ->
    if
        Error =:= undefined ->
            {noreply, State};
        true ->
            {stop, Error, State}
    end;

cloudi_service_handle_info(#timeout_async_active{trans_id = TransId} = Request,
                           #state{map_reduce_module = MapReduceModule,
                                  map_reduce_state = MapReduceState,
                                  map_requests = MapRequests} = State,
                           Dispatcher) ->
    case maps:find(TransId, MapRequests) of
        {ok, [_ | SendArgs]} ->
            NextMapRequests = maps:remove(TransId, MapRequests),
            case MapReduceModule:cloudi_service_map_reduce_resend(
                [Dispatcher | SendArgs], MapReduceState) of
                {ok, NewSendArgs, NewMapReduceState} ->
                    case erlang:apply(cloudi_service, send_async_active,
                                      NewSendArgs) of
                        {ok, NewTransId} ->
                            NewMapRequests = maps:put(NewTransId,
                                                      NewSendArgs,
                                                      NextMapRequests),
                            {noreply,
                             State#state{map_reduce_state = NewMapReduceState,
                                         map_requests = NewMapRequests}};
                        {error, _} = Error ->
                            {stop, Error, State}
                    end;
                {error, _} = Error ->
                    {stop, Error, State}
            end;
        error ->
            cloudi_service_map_reduce_info(Request, State, Dispatcher)
    end;

cloudi_service_handle_info(#return_async_active{response_info = ResponseInfo,
                                                response = Response,
                                                timeout = Timeout,
                                                trans_id = TransId} = Request,
                           #state{map_reduce_module = MapReduceModule,
                                  map_reduce_state = MapReduceState,
                                  map_requests = MapRequests} = State,
                           Dispatcher) ->
    case maps:find(TransId, MapRequests) of
        {ok, [_ | SendArgs]} ->
            case MapReduceModule:cloudi_service_map_reduce_recv(
                [Dispatcher | SendArgs], ResponseInfo, Response,
                Timeout, TransId, MapReduceState, Dispatcher) of
                {ok, NextMapReduceState} ->
                    case map_send(maps:remove(TransId, MapRequests),
                                  Dispatcher, MapReduceModule,
                                  NextMapReduceState) of
                        {ok, NewMapRequests, NewMapReduceState} ->
                            {noreply,
                             State#state{map_reduce_state = NewMapReduceState,
                                         map_requests = NewMapRequests}};
                        {error, _} = Error ->
                            {stop, Error, State}
                    end;
                {done, NewMapReduceState} ->
                    NewMapRequests = maps:remove(TransId, MapRequests),
                    NewState = State#state{map_reduce_state = NewMapReduceState,
                                           map_requests = NewMapRequests},
                    case maps:size(NewMapRequests) of
                        0 ->
                            {stop, shutdown, NewState};
                        _ ->
                            {noreply, NewState}
                    end;
                {error, _} = Error ->
                    {stop, Error, State}
            end;
        error ->
            cloudi_service_map_reduce_info(Request, State, Dispatcher)
    end;

cloudi_service_handle_info(Request, State, Dispatcher) ->
    cloudi_service_map_reduce_info(Request, State, Dispatcher).

cloudi_service_terminate(_Reason, _Timeout, undefined) ->
    ok;
cloudi_service_terminate(shutdown, _Timeout,
                         #state{time_start = TimeStart,
                                log_execution_time = true,
                                map_requests = #{}}) ->
    TimeEnd = cloudi_timestamp:seconds_monotonic(),
    ?LOG_INFO("total time taken was ~p hours",
              [hours_elapsed(TimeEnd, TimeStart)]),
    ok;
cloudi_service_terminate(_Reason, _Timeout, #state{}) ->
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

init(#init_begin{prefix = Prefix,
                 timeout = Timeout,
                 time_start = TimeStart,
                 log_execution_time = LogExecutionTime,
                 map_reduce_module = MapReduceModule,
                 map_reduce_args = MapReduceArgs,
                 concurrency = Concurrency},
     Dispatcher) ->
    MapCount = cloudi_concurrency:count(Concurrency),
    case MapReduceModule:cloudi_service_map_reduce_new(MapReduceArgs,
                                                       MapCount,
                                                       Prefix,
                                                       Timeout,
                                                       Dispatcher) of
        {ok, MapReduceState} ->
            case map_send(MapCount, #{}, Dispatcher,
                          MapReduceModule, MapReduceState) of
                {ok, MapRequests, NewMapReduceState} ->
                    {noreply, #state{time_start = TimeStart,
                                     log_execution_time = LogExecutionTime,
                                     map_reduce_module = MapReduceModule,
                                     map_reduce_state = NewMapReduceState,
                                     map_count = MapCount,
                                     map_requests = MapRequests}};
                {error, _} = Error ->
                    {stop, Error, undefined}
            end;
        {error, _} = Error ->
            {stop, Error, undefined}
    end.

map_send(MapRequests, Dispatcher, MapReduceModule, MapReduceState) ->
    map_send(1, MapRequests, Dispatcher, MapReduceModule, MapReduceState).

map_send(0, MapRequests, _Dispatcher, _MapReduceModule, MapReduceState) ->
    {ok, MapRequests, MapReduceState};

map_send(Count, MapRequests, Dispatcher, MapReduceModule, MapReduceState) ->
    case MapReduceModule:cloudi_service_map_reduce_send(MapReduceState,
                                                        Dispatcher) of
        {ok, SendArgs, NewMapReduceState} ->
            case erlang:apply(cloudi_service, send_async_active, SendArgs) of
                {ok, TransId} ->
                    map_send(Count - 1,
                             maps:put(TransId, SendArgs, MapRequests),
                             Dispatcher, MapReduceModule, NewMapReduceState);
                {error, _} = Error ->
                    Error
            end;
        {done, NewMapReduceState} ->
            {ok, MapRequests, NewMapReduceState};
        {error, _} = Error ->
            Error
    end.

cloudi_service_map_reduce_info(Request,
                               #state{map_reduce_module = MapReduceModule,
                                      map_reduce_state = MapReduceState,
                                      map_requests = MapRequests} = State,
                               Dispatcher) ->
    case MapReduceModule:cloudi_service_map_reduce_info(Request,
                                                        MapReduceState,
                                                        Dispatcher) of
        {ok, NewMapReduceState} ->
            {noreply, State#state{map_reduce_state = NewMapReduceState}};
        {done, NewMapReduceState} ->
            NewState = State#state{map_reduce_state = NewMapReduceState},
            case maps:size(MapRequests) of
                0 ->
                    {stop, shutdown, NewState};
                _ ->
                    {noreply, NewState}
            end;
        {error, _} = Error ->
            {stop, Error, State}
    end.

hours_elapsed(Seconds1, Seconds0)
    when Seconds1 >= Seconds0 ->
    erlang:round(((Seconds1 - Seconds0) / (60 * 60)) * 10) / 10.

