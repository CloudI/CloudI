%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Cron Service==
%%% Cron expression events are supported within the limitation defined by
%%% erlang:system_info(end_time) (events during a 250 year period).
%%% The cron expression string may use the 5 required fields
%%% (minutes, hours, day_of_month, month, day_of_week),
%%% 6 fields with year as the last field,
%%% 7 fields with seconds as the first field or a supported macro
%%% (@yearly, @annually, @monthly, @weekly, @daily, @midnight, @hourly).
%%%
%%% If the system clock is adjusted backwards, the same sequence of
%%% events (described by the cron expression) occurs without redoing
%%% events that have already occurred before the time change.  That means
%%% the adjustment backwards only delays the next event's occurrence
%%% by the adjustment amount.
%%%
%%% If the system clock is adjusted forwards, an event may occur immediately
%%% (when the system clock change is detected) due to the event time becoming
%%% a time in the past.  Each cron expression's next event will be a
%%% a time in the future based on the updated current time
%%% (events may have been skipped by adjusting the system clock forwards).
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2019-2021 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2019-2021 Michael Truog
%%% @version 2.0.3 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_cron).
-author('mjtruog at protonmail dot com').

-behaviour(cloudi_service).

%% cloudi_service callbacks
-export([cloudi_service_init/4,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/3]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").
-include_lib("cloudi_core/include/cloudi_service.hrl").

-define(DEFAULT_SEND_ARGS_INFO,             false).
        % Set the default send_args_info value for all expressions.
-define(DEFAULT_SEND_MCAST,                 false).
        % Set the default send_mcast value for all expressions.
-define(DEFAULT_REQUEST_INFO_DEFAULT,
        [{"cron_description", "\"$DESCRIPTION\""},
         {"cron_datetime", "${DATETIME}"},
         {"cron_datetime_next", "${DATETIME_NEXT}"}]).
        % If a cron expression's send_args doesn't provide a
        % RequestInfo argument or the RequestInfo argument is set to default,
        % use the provided data when send_args_info (for the expression)
        % is set to true.
-define(DEFAULT_EXPRESSIONS,                   []).
        % cron expression arguments:
        %     description
        %         A descriptive string for the cron expression.
        %     send_args
        %         Arguments to a cloudi_service:send_async_active function
        %         call.  If the Timeout parameter is provided as undefined,
        %         the number of milliseconds until the next event is used.
        %         If RequestInfo is provided as a list of string
        %         key/value tuples and send_args_info is set to true,
        %         substitution of event data can occur in the string values
        %         with (environment variable syntax):
        %             DESCRIPTION - description provided above
        %             DATETIME - event datetime in UTC that caused send
        %             DATETIME_NEXT - next event datetime in UTC
        %     send_args_info
        %         Should the RequestInfo value in send_args be modified
        %         with event data related to the cron expression.
        %         (override the default send_args_info value)
        %     send_mcast
        %         Should the send occur as a call to the function
        %         cloudi_service:mcast_async_active instead of
        %         cloudi_service:send_async_active.
        %         (override the default send_mcast value)
        %
        % e.g. [{"0 9 * * mon-fri",
        %        [{description, "hello"},
        %         {send_args, ["/shell", "echo \"hello world\""]}]}]
-define(DEFAULT_VALIDATE_RESPONSE,
        {cloudi_service_shell, validate_response}).
-define(DEFAULT_USE_UTC,                    false).
-define(DEFAULT_DEBUG,                       true).
-define(DEFAULT_DEBUG_LEVEL,                trace).

-type expression_id() :: pos_integer().

-record(expression,
    {
        description :: nonempty_string() | undefined,
        definition :: cloudi_cron:state(),
        send_args :: nonempty_list(),
        send_args_info :: boolean(),
        send_mcast :: boolean(),
        datetime_utc = undefined :: calendar:datetime() | undefined,
        timer = undefined :: reference() | undefined
    }).

-record(send_data,
    {
        id :: expression_id(),
        send_start :: cloudi_timestamp:milliseconds_monotonic(),
        trans_ids = [] :: list(cloudi_service:trans_id())
    }).

-record(state,
    {
        service :: cloudi_service:source(),
        utc :: boolean(),
        debug_level :: off | trace | debug | info | warn | error | fatal,
        time_offset_milliseconds :: integer(),
        time_offset_monitor :: reference(),
        id_next :: expression_id(),
        expressions :: #{expression_id() := #expression{}},
        validate_response :: fun((any(), any()) -> boolean()),
        sends = #{} :: #{cloudi_service:trans_id() := #send_data{}}
    }).

% All cron expression events will occur a small number of milliseconds after
% the datetime specified to anticipate any Erlang timer jitter
% (ensuring that the event datetime is always reached by the timer)
-define(MILLISECONDS_OFFSET, 10).

-ifdef(ERLANG_OTP_VERSION_19).
-else.
-define(ERLANG_OTP_VERSION_20_FEATURES, true).
-endif.

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, _Prefix, _Timeout, Dispatcher) ->
    Defaults = [
        {send_args_info,               ?DEFAULT_SEND_ARGS_INFO},
        {send_mcast,                   ?DEFAULT_SEND_MCAST},
        {request_info_default,         ?DEFAULT_REQUEST_INFO_DEFAULT},
        {expressions,                  ?DEFAULT_EXPRESSIONS},
        {validate_response,            ?DEFAULT_VALIDATE_RESPONSE},
        {use_utc,                      ?DEFAULT_USE_UTC},
        {debug,                        ?DEFAULT_DEBUG},
        {debug_level,                  ?DEFAULT_DEBUG_LEVEL}],
    [SendArgsInfo, SendMcast, RequestInfoDefault, Expressions,
     ValidateResponse0, UseUTC,
     Debug, DebugLevel] = cloudi_proplists:take_values(Defaults, Args),
    true = is_boolean(SendArgsInfo),
    true = is_boolean(SendMcast),
    true = lists:all(fun({Key, Value}) ->
        is_integer(hd(Key)) andalso is_integer(hd(Value))
    end, RequestInfoDefault),
    true = is_boolean(UseUTC),
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
    ProcessIndex = cloudi_service:process_index(Dispatcher),
    ProcessCount = cloudi_service:process_count(Dispatcher),
    {IdNext,
     ExpressionsLoaded} = expressions_load(Expressions,
                                           ProcessIndex, ProcessCount,
                                           SendArgsInfo, SendMcast,
                                           RequestInfoDefault),
    ValidateResponseN = cloudi_args_type:
                        function_required(ValidateResponse0, 2),
    Service = cloudi_service:self(Dispatcher),
    TimeOffsetMilliseconds = time_offset_milliseconds(),
    TimeOffsetMonitor = erlang:monitor(time_offset, clock_service),
    ExpressionsStarted = expressions_start(ExpressionsLoaded, UseUTC, Service),
    {ok, #state{service = Service,
                utc = UseUTC,
                debug_level = DebugLogLevel,
                time_offset_milliseconds = TimeOffsetMilliseconds,
                time_offset_monitor = TimeOffsetMonitor,
                id_next = IdNext,
                expressions = ExpressionsStarted,
                validate_response = ValidateResponseN}}.

cloudi_service_handle_info(#timeout_async_active{trans_id = TransId},
                           #state{debug_level = DebugLogLevel,
                                  expressions = Expressions,
                                  sends = Sends} = State,
                           _Dispatcher) ->
    SendsNew = event_recv(Sends, Expressions, timeout, TransId, DebugLogLevel),
    {noreply, State#state{sends = SendsNew}};
cloudi_service_handle_info(#return_async_active{response_info = ResponseInfo,
                                                response = Response,
                                                trans_id = TransId},
                           #state{debug_level = DebugLogLevel,
                                  expressions = Expressions,
                                  validate_response = ValidateResponse,
                                  sends = Sends} = State,
                           _Dispatcher) ->
    Result = case ValidateResponse(ResponseInfo, Response) of
        true ->
            ok;
        false ->
            error
    end,
    SendsNew = event_recv(Sends, Expressions, Result, TransId, DebugLogLevel),
    {noreply, State#state{sends = SendsNew}};
cloudi_service_handle_info({expression, Id},
                           #state{service = Service,
                                  utc = UseUTC,
                                  debug_level = DebugLogLevel,
                                  expressions = Expressions,
                                  sends = Sends} = State,
                           Dispatcher) ->
    {SendsNew,
     ExpressionsNew} = expression_event(Sends, Expressions, Id,
                                        DebugLogLevel, UseUTC,
                                        Service, Dispatcher),
    {noreply, State#state{expressions = ExpressionsNew,
                          sends = SendsNew}};
cloudi_service_handle_info({'CHANGE', Monitor, time_offset,
                            clock_service, TimeOffset},
                           #state{service = Service,
                                  utc = UseUTC,
                                  time_offset_milliseconds = ValueOld,
                                  time_offset_monitor = Monitor,
                                  expressions = Expressions} = State,
                           _Dispatcher) ->
    ValueNew = time_offset_to_milliseconds(TimeOffset),
    ExpressionsNew = expressions_time_offset(Expressions,
                                             ValueOld - ValueNew,
                                             UseUTC, Service),
    {noreply, State#state{time_offset_milliseconds = ValueNew,
                          expressions = ExpressionsNew}}.

cloudi_service_terminate(_Reason, _Timeout, _State) ->
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

expressions_load([], Id, Expressions, _, _, _, _, _, _) ->
    {Id, Expressions};
expressions_load([{ExpressionStr, Args} | L], Id, Expressions,
                 ProcessIndex, ProcessIndex, ProcessCount,
                 SendArgsInfoDefault, SendMcastDefault, RequestInfoDefault) ->
    Defaults = [
        {description,                  undefined},
        {send_args,                    undefined},
        {send_args_info,               SendArgsInfoDefault},
        {send_mcast,                   SendMcastDefault}],
    [Description, SendArgs, SendArgsInfo,
     SendMcast] = cloudi_proplists:take_values(Defaults, Args),
    [_ | _] = ExpressionStr,
    case Description of
        [_ | _] ->
            ok;
        undefined ->
            ok
    end,
    true = is_boolean(SendArgsInfo),
    true = is_boolean(SendMcast),
    SendArgsNew = send_args_valid(SendArgs, SendArgsInfo, RequestInfoDefault),
    Cron = cloudi_cron:new(ExpressionStr),
    Expression = #expression{description = Description,
                             definition = Cron,
                             send_args = SendArgsNew,
                             send_args_info = SendArgsInfo,
                             send_mcast = SendMcast},
    expressions_load(L, Id + 1, maps:put(Id, Expression, Expressions),
                     (ProcessIndex + 1) rem ProcessCount,
                     ProcessIndex, ProcessCount,
                     SendArgsInfoDefault, SendMcastDefault, RequestInfoDefault);
expressions_load([{_, _} | L], Id, Expressions,
                 Index, ProcessIndex, ProcessCount,
                 SendArgsInfoDefault, SendMcastDefault, RequestInfoDefault) ->
    expressions_load(L, Id, Expressions,
                     (Index + 1) rem ProcessCount,
                     ProcessIndex, ProcessCount,
                     SendArgsInfoDefault, SendMcastDefault, RequestInfoDefault).

expressions_load(L, ProcessIndex, ProcessCount,
                 SendArgsInfoDefault, SendMcastDefault, RequestInfoDefault) ->
    expressions_load(L, 1, #{}, 0, ProcessIndex, ProcessCount,
                     SendArgsInfoDefault, SendMcastDefault, RequestInfoDefault).

expression_next(#expression{definition = Cron,
                            datetime_utc = DateTimeOldEventUTC} = Expression,
                Id, UseUTC, Service) ->
    case expression_next_event(DateTimeOldEventUTC, Cron, UseUTC) of
        {DateTimeNewEventUTC, MilliSecondsEvent} ->
            Timer = erlang:send_after(MilliSecondsEvent, Service,
                                      {expression, Id}),
            Expression#expression{datetime_utc = DateTimeNewEventUTC,
                                  timer = Timer};
        undefined ->
            Expression#expression{timer = undefined}
    end.

expression_next_event(DateTimeOldEventUTC, Cron, UseUTC) ->
    {DateTimeNowUTC, PastMilliSeconds} = datetime_now_utc(),
    DateTimeNextEventUTC = if
        DateTimeOldEventUTC =:= undefined ->
            DateTimeNowUTC;
        DateTimeOldEventUTC > DateTimeNowUTC ->
            % if the time_offset adjustment occurred, ensure the same
            % cron sequence occurs to prevent cloudi_service_cron from
            % sending more service requests than the cron expression defines
            DateTimeOldEventUTC;
        true ->
            DateTimeNowUTC
    end,
    DateTimeNextEvent = if
        UseUTC =:= true ->
            DateTimeNextEventUTC;
        UseUTC =:= false ->
            calendar:universal_time_to_local_time(DateTimeNextEventUTC)
    end,
    DateTimeNewEventUTC = datetime_event_utc(DateTimeNextEvent, Cron, UseUTC),
    if
        DateTimeNewEventUTC =:= undefined ->
            undefined;
        is_tuple(DateTimeNewEventUTC) ->
            MilliSecondsEvent = event_milliseconds(DateTimeNewEventUTC,
                                                   DateTimeNowUTC,
                                                   PastMilliSeconds),
            {DateTimeNewEventUTC,
             MilliSecondsEvent}
    end.

expression_update(#expression{timer = undefined} = Expression,
                  Id, _, UseUTC, Service) ->
    expression_next(Expression, Id, UseUTC, Service);
expression_update(#expression{datetime_utc = DateTimeEventUTC,
                              timer = TimerOld} = Expression,
                  Id, MilliSecondsOffset, _, Service) ->
    TimerNew = case erlang:cancel_timer(TimerOld) of
        false ->
            TimerOld;
        MilliSecondsOld ->
            {DateTimeNowUTC, PastMilliSeconds} = datetime_now_utc(),
            if
                MilliSecondsOld + MilliSecondsOffset =< 0;
                DateTimeNowUTC >= DateTimeEventUTC ->
                    Service ! {expression, Id},
                    TimerOld;
                true ->
                    % determining MilliSecondsNew from
                    % DateTimeEventUTC is more accurate than using
                    % (MilliSecondsOld + MilliSecondsOffset)
                    % because multiple time_offsets may occur
                    MilliSecondsNew = event_milliseconds(DateTimeEventUTC,
                                                         DateTimeNowUTC,
                                                         PastMilliSeconds),
                    erlang:send_after(MilliSecondsNew, Service,
                                      {expression, Id})
            end
    end,
    Expression#expression{timer = TimerNew}.

expressions_start(Expressions, UseUTC, Service) ->
    maps:map(fun(Id, Expression) ->
        expression_next(Expression, Id, UseUTC, Service)
    end, Expressions).

expressions_time_offset(Expressions, MilliSecondsOffset, UseUTC, Service) ->
    maps:map(fun(Id, Expression) ->
        expression_update(Expression, Id, MilliSecondsOffset, UseUTC, Service)
    end, Expressions).

expression_event(Sends, Expressions, Id,
                 DebugLogLevel, UseUTC, Service, Dispatcher) ->
    Expression = maps:get(Id, Expressions),
    #expression{send_args = [ServiceName | _] = SendArgs,
                send_args_info = SendArgsInfo,
                send_mcast = SendMcast,
                datetime_utc = DateTimeEventUTC} = Expression,
    ExpressionNew = expression_next(Expression, Id, UseUTC, Service),
    #expression{datetime_utc = DateTimeNextEventUTC} = ExpressionNew,
    Description = description(Expression),
    SendArgsCall = send_args_call(SendArgs, SendArgsInfo,
                                  DateTimeEventUTC, DateTimeNextEventUTC,
                                  Description, Dispatcher),
    ?LOG(DebugLogLevel,
         "\"~ts\" event sent to ~ts",
         [Description, ServiceName]),
    SendsNew = event_send(SendMcast, Sends, SendArgsCall, Id,
                          Service, Dispatcher),
    {SendsNew,
     maps:put(Id, ExpressionNew, Expressions)}.

event_send(false, Sends, SendArgsCall, Id, Service, Dispatcher) ->
    SendStart = cloudi_timestamp:milliseconds_monotonic(),
    TransId = case erlang:apply(cloudi_service, send_async_active,
                                SendArgsCall) of
        {ok, TransIdValue} ->
            TransIdValue;
        {error, timeout} ->
            TransIdValue = cloudi_service:trans_id(Dispatcher),
            Service ! #timeout_async_active{trans_id = TransIdValue},
            TransIdValue
    end,
    maps:put(TransId,
             #send_data{id = Id,
                        send_start = SendStart}, Sends);
event_send(true, Sends, SendArgsCall, Id, Service, Dispatcher) ->
    SendStart = cloudi_timestamp:milliseconds_monotonic(),
    case erlang:apply(cloudi_service, mcast_async_active,
                      SendArgsCall) of
        {ok, TransIdList} ->
            lists:foldl(fun(TransId, SendsNew) ->
                TransIdListNew = lists:delete(TransId, TransIdList),
                maps:put(TransId,
                         #send_data{id = Id,
                                    send_start = SendStart,
                                    trans_ids = TransIdListNew}, SendsNew)
            end, Sends, TransIdList);
        {error, timeout} ->
            TransId = cloudi_service:trans_id(Dispatcher),
            Service ! #timeout_async_active{trans_id = TransId},
            maps:put(TransId,
                     #send_data{id = Id,
                                send_start = SendStart}, Sends)
    end.

event_recv(Sends, Expressions, Result, TransId, DebugLogLevel) ->
    case maps:find(TransId, Sends) of
        {ok, #send_data{id = Id,
                        send_start = SendStart,
                        trans_ids = TransIdList}} ->
            SendEnd = cloudi_timestamp:milliseconds_monotonic(),
            Expression = maps:get(Id, Expressions),
            SecondsElapsed = (SendEnd - SendStart) / 1000.0,
            if
                Result =:= ok ->
                    ?LOG(DebugLogLevel,
                         "\"~ts\" event completed after ~p seconds",
                         [description(Expression), SecondsElapsed]);
                Result =:= timeout ->
                    ?LOG_ERROR("\"~ts\" event timeout after ~p seconds",
                               [description(Expression), SecondsElapsed]);
                Result =:= error ->
                    ?LOG_ERROR("\"~ts\" event failed after ~p seconds",
                               [description(Expression), SecondsElapsed])
            end,
            lists:foldl(fun(TransIdOld, SendsNew) ->
                maps:remove(TransIdOld, SendsNew)
            end, maps:remove(TransId, Sends), TransIdList);
        error ->
            Sends
    end.

event_milliseconds(DateTimeEventUTC, DateTimeNowUTC, PastMilliSeconds) ->
    % subtraction must occur as gregorian seconds in UTC
    % to avoid DST offsets causing the difference to be invalid
    Seconds = calendar:datetime_to_gregorian_seconds(DateTimeEventUTC) -
              calendar:datetime_to_gregorian_seconds(DateTimeNowUTC),
    Seconds * 1000 - PastMilliSeconds.

datetime_event_utc(DateTime, Cron, UseUTC) ->
    DateTimeEvent = cloudi_cron:next_datetime(DateTime, Cron),
    if
        DateTimeEvent =:= undefined ->
            undefined;
        UseUTC =:= true ->
            DateTimeEvent;
        UseUTC =:= false ->
            case calendar:local_time_to_universal_time_dst(DateTimeEvent) of
                [] ->
                    % when DST sets clocks ahead,
                    % pick the next valid datetime after the DST change
                    datetime_event_utc(DateTimeEvent, Cron, UseUTC);
                [DateTimeEventUTC] ->
                    DateTimeEventUTC;
                [_, DateTimeEventUTC] ->
                    % when DST sets clocks back,
                    % pick the UTC datetime after the DST change
                    DateTimeEventUTC
            end
    end.

datetime_now_utc() ->
    {_, _, MicroSeconds} = Now = erlang:timestamp(),
    {calendar:now_to_universal_time(Now),
     MicroSeconds div 1000 - ?MILLISECONDS_OFFSET}.

description(#expression{description = [_ | _] = Description}) ->
    Description;
description(#expression{definition = Cron}) ->
    cloudi_cron:expression(Cron).

send_args_valid([Name, Request] = SendArgs,
                SendArgsInfo, RequestInfoDefault) ->
    true = cloudi_args_type:service_name(Name),
    if
        SendArgsInfo =:= true ->
            [Name, RequestInfoDefault, Request, undefined, undefined];
        SendArgsInfo =:= false ->
            SendArgs
    end;
send_args_valid([Name, Request, Timeout] = SendArgs,
                SendArgsInfo, RequestInfoDefault) ->
    true = cloudi_args_type:service_name(Name),
    true = cloudi_args_type:timeout_milliseconds(Timeout),
    if
        SendArgsInfo =:= true ->
            [Name, RequestInfoDefault, Request, Timeout, undefined];
        SendArgsInfo =:= false ->
            SendArgs
    end;
send_args_valid([Name, RequestInfo, Request, Timeout, Priority],
                SendArgsInfo, RequestInfoDefault) ->
    true = cloudi_args_type:service_name(Name),
    true = cloudi_args_type:timeout_milliseconds(Timeout),
    true = cloudi_args_type:priority(Priority),
    RequestInfoNew = if
        RequestInfo =:= default ->
            RequestInfoDefault;
        true ->
            RequestInfo
    end,
    if
        SendArgsInfo =:= true ->
            [_ | _] = RequestInfoNew,
            true = lists:all(fun({Key, Value}) ->
                is_integer(hd(Key)) andalso is_integer(hd(Value))
            end, RequestInfoNew),
            ok;
        SendArgsInfo =:= false ->
            ok
    end,
    [Name, RequestInfoNew, Request, Timeout, Priority].

send_args_call([Name, [_ | _] = RequestInfo, Request, Timeout, Priority],
               true, DateTimeEventUTC, DateTimeNextEventUTC, Description,
               Dispatcher) ->
    Environment0 = [{"DATETIME",
                     cloudi_timestamp:datetime_utc_to_string(DateTimeEventUTC)},
                    {"DESCRIPTION", Description}],
    EnvironmentN = if
        DateTimeNextEventUTC =:= undefined ->
            Environment0;
        is_tuple(DateTimeNextEventUTC) ->
            [{"DATETIME_NEXT",
              cloudi_timestamp:datetime_utc_to_string(DateTimeNextEventUTC)} |
             Environment0]
    end,
    Lookup = cloudi_x_trie:new(EnvironmentN),
    RequestInfoNew = [{Key, cloudi_environment:transform(Value, Lookup)}
                      || {Key, Value} <- RequestInfo],
    TimeoutNew = send_args_call_timeout(Timeout,
                                        DateTimeEventUTC,
                                        DateTimeNextEventUTC,
                                        Description),
    [Dispatcher, Name,
     cloudi_request_info:key_value_new(RequestInfoNew, text_pairs),
     Request, TimeoutNew, Priority];
send_args_call([Name, Request, Timeout],
               _, DateTimeEventUTC, DateTimeNextEventUTC, Description,
               Dispatcher) ->
    TimeoutNew = send_args_call_timeout(Timeout,
                                        DateTimeEventUTC,
                                        DateTimeNextEventUTC,
                                        Description),
    [Dispatcher, Name, Request, TimeoutNew];
send_args_call(SendArgs, _, _, _, _, Dispatcher) ->
    [Dispatcher | SendArgs].

send_args_call_timeout(Timeout, _, undefined, _) ->
    Timeout;
send_args_call_timeout(Timeout,
                       DateTimeEventUTC, DateTimeNextEventUTC, Description) ->
    TimeoutMax = (calendar:datetime_to_gregorian_seconds(DateTimeNextEventUTC) -
                  calendar:datetime_to_gregorian_seconds(DateTimeEventUTC)) *
                 1000,
    send_args_call_timeout(Timeout, TimeoutMax, Description).

send_args_call_timeout(Timeout, TimeoutMax, _)
    when is_atom(Timeout) ->
    if
        Timeout =:= undefined ->
            TimeoutMax;
        true ->
            Timeout
    end;
send_args_call_timeout(Timeout, TimeoutMax, Description)
    when Timeout > TimeoutMax ->
    ?LOG_WARN("\"~ts\" event timeout ~w > ~w overlaps next event",
              [Description, Timeout, TimeoutMax]),
    Timeout;
send_args_call_timeout(Timeout, _, _) ->
    Timeout.

-ifdef(ERLANG_OTP_VERSION_20_FEATURES).
time_offset_milliseconds() ->
    erlang:time_offset(millisecond).
time_offset_to_milliseconds(TimeOffset) ->
    erlang:convert_time_unit(TimeOffset, native, millisecond).
-else.
time_offset_milliseconds() ->
    erlang:time_offset(milli_seconds).
time_offset_to_milliseconds(TimeOffset) ->
    erlang:convert_time_unit(TimeOffset, native, milli_seconds).
-endif.
