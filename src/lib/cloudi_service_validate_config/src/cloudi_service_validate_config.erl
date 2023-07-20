%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Validate Config Service==
%%% Examine the current CloudI services and their configuration to log
%%% any service configuration values that look like a mistake.
%%%
%%% This service's logic exists separate from the CloudI configuration
%%% logic because the dynamic nature of CloudI services requires
%%% flexibility.  Only CloudI service configuration that is clearly
%%% invalid creates an error when the service attempts to start.
%%% However, service configuration values may only become used
%%% after a CloudI service is updated.  A CloudI service could have
%%% configuration values for sending service requests without the logic
%%% for sending service requests until after the service is updated.
%%% A CloudI service could delay its subscription to service name patterns
%%% to delay its ability to receive CloudI service requests while
%%% still having service configuration values only valid for
%%% receiving service requests.
%%%
%%% Run this service to identify service configuration inconsistencies
%%% and use the CloudI log output to modify the service configuration
%%% if unexpected problems are discovered.
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2023 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2023 Michael Truog
%%% @version 2.0.7 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_validate_config).
-author('mjtruog at protonmail dot com').

-behaviour(cloudi_service).

%% external interface

%% cloudi_service callbacks
-export([cloudi_service_init/4,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/3]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").
-include_lib("cloudi_core/include/cloudi_service_api.hrl").

-define(DEFAULT_DEBUG,                      true). % log output for debugging
-define(DEFAULT_DEBUG_LEVEL,                info).
-define(DEFAULT_ERROR_LEVEL,                error).

-record(state,
    {
        debug_level :: off | trace | debug | info | warn | error | fatal,
        error_level :: trace | debug | info | warn | error | fatal
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, _Prefix, _Timeout, _Dispatcher) ->
    Defaults = [
        {debug,                    ?DEFAULT_DEBUG},
        {debug_level,              ?DEFAULT_DEBUG_LEVEL},
        {error_level,              ?DEFAULT_ERROR_LEVEL}],
    [Debug, DebugLevel,
     ErrorLevel] = cloudi_proplists:take_values(Defaults, Args),
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
    true = ((ErrorLevel =:= trace) orelse
            (ErrorLevel =:= debug) orelse
            (ErrorLevel =:= info) orelse
            (ErrorLevel =:= warn) orelse
            (ErrorLevel =:= error) orelse
            (ErrorLevel =:= fatal)),
    % cloudi_service_init/4 is always executed by the service process
    Service = self(),
    Service ! validate_config,
    {ok, #state{debug_level = DebugLogLevel,
                error_level = ErrorLevel}}.

cloudi_service_handle_info(validate_config,
                           #state{debug_level = DebugLogLevel,
                                  error_level = ErrorLogLevel} = State,
                           _Dispatcher) ->
    {ok, Services} = cloudi_service_api:services(infinity),
    StopReason = validate(Services, 0, DebugLogLevel, ErrorLogLevel),
    {stop, StopReason, State}.

cloudi_service_terminate(_Reason, _Timeout, _State) ->
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

validate([], Count, DebugLogLevel, _) ->
    stop_reason(Count, DebugLogLevel);
validate([{ServiceId,
           #internal{dest_refresh = DestRefresh,
                     dest_list_deny = DestListDeny,
                     dest_list_allow = DestListAllow,
                     options = Options}} | Services],
         Count, DebugLogLevel, ErrorLogLevel) ->
    case cloudi_service_api:service_subscriptions(ServiceId, infinity) of
        {ok, Subscriptions} ->
            AbleToSend = DestRefresh /= none,
            AbleToReceive = Subscriptions /= [],
            validate(Services,
                     validate_service(Count,
                                      AbleToSend, AbleToReceive,
                                      DestListDeny, DestListAllow,
                                      Options, ServiceId, ErrorLogLevel),
                     DebugLogLevel, ErrorLogLevel);
        {error, not_found} ->
            validate(Services, Count, DebugLogLevel, ErrorLogLevel);
        {error, shutdown} ->
            {validate_config_failed, interrupted}
    end;
validate([{ServiceId,
           #external{dest_refresh = DestRefresh,
                     dest_list_deny = DestListDeny,
                     dest_list_allow = DestListAllow,
                     options = Options}} | Services],
         Count, DebugLogLevel, ErrorLogLevel) ->
    case cloudi_service_api:service_subscriptions(ServiceId, infinity) of
        {ok, Subscriptions} ->
            AbleToSend = DestRefresh /= none,
            AbleToReceive = Subscriptions /= [],
            validate(Services,
                     validate_service(Count,
                                      AbleToSend, AbleToReceive,
                                      DestListDeny, DestListAllow,
                                      Options, ServiceId, ErrorLogLevel),
                     DebugLogLevel, ErrorLogLevel);
        {error, not_found} ->
            validate(Services, Count, DebugLogLevel, ErrorLogLevel);
        {error, shutdown} ->
            {validate_config_failed, interrupted}
    end.

validate_service(Count0, AbleToSend, AbleToReceive,
                 DestListDeny, DestListAllow, Options,
                 ServiceId, ErrorLogLevel) ->
    Count1 = if
        DestListDeny /= undefined, AbleToSend =:= false ->
            ?LOG(ErrorLogLevel,
                 "unable to send with dest_list_deny /= undefined~n ~p",
                 [service_id(ServiceId)]),
            Count0 + 1;
        true ->
            Count0
    end,
    CountN = if
        DestListAllow /= undefined, AbleToSend =:= false ->
            ?LOG(ErrorLogLevel,
                 "unable to send with dest_list_allow /= undefined~n ~p",
                 [service_id(ServiceId)]),
            Count1 + 1;
        true ->
            Count1
    end,
    validate_service_option(Options, CountN,
                            AbleToSend, AbleToReceive,
                            ServiceId, ErrorLogLevel).

validate_service_option([], Count0, _, _, _, _) ->
    Count0;
validate_service_option([{Name, _} | Options], Count0,
                        AbleToSend, AbleToReceive,
                        ServiceId, ErrorLogLevel) ->
    Count1 = if
        Name =:= priority_default orelse
        Name =:= dest_refresh_start orelse
        Name =:= dest_refresh_delay orelse
        Name =:= request_name_lookup orelse
        Name =:= request_timeout_immediate_max orelse
        Name =:= response_timeout_adjustment,
        AbleToSend =:= false ->
            ?LOG(ErrorLogLevel,
                 "unable to send with option ~p set~n ~p",
                 [Name, service_id(ServiceId)]),
            Count0 + 1;
        true ->
            Count0
    end,
    CountN = if
        Name =:= queue_limit orelse
        Name =:= queue_size orelse
        Name =:= rate_request_max orelse
        Name =:= request_timeout_adjustment orelse
        Name =:= response_timeout_immediate_max orelse
        Name =:= count_process_dynamic orelse
        Name =:= fatal_exceptions orelse
        Name =:= fatal_timeout orelse
        Name =:= fatal_timeout_delay orelse
        Name =:= aspects_request_before orelse
        Name =:= aspects_request_after,
        AbleToReceive =:= false ->
            ?LOG(ErrorLogLevel,
                 "unable to receive with option ~p set~n ~p",
                 [Name, service_id(ServiceId)]),
            Count1 + 1;
        true ->
            Count1
    end,
    validate_service_option(Options, CountN,
                            AbleToSend, AbleToReceive,
                            ServiceId, ErrorLogLevel).

stop_reason(0, DebugLogLevel) ->
    ?LOG(DebugLogLevel,
         "no configuration problems found", []),
    shutdown;
stop_reason(Count, _)
    when Count > 0 ->
    {validate_config_failed, Count}.

service_id(ID) ->
    cloudi_x_uuid:uuid_to_string(ID, list_nodash).
