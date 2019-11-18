%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Cron Service==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2019 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2019 Michael Truog
%%% @version 1.8.0 {@date} {@time}
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

-define(DEFAULT_EXPRESSIONS,                   []).
        % e.g. [{"0 9 * * mon-fri",
        %        [{description, "hello"},
        %         {send_args, ["/shell", "echo \"hello world\""]}]}]
-define(DEFAULT_USE_UTC,                    false).
-define(DEFAULT_DEBUG,                       true).
-define(DEFAULT_DEBUG_LEVEL,                trace).

-type expression_id() :: pos_integer().

-record(expression,
    {
        description :: nonempty_string() | undefined,
        definition :: cloudi_cron:state(),
        send_args :: nonempty_list(),
        datetime_utc = undefined :: calendar:datetime() | undefined,
        timer = undefined :: reference() | undefined
    }).

-record(send_data,
    {
        id :: expression_id(),
        send_start :: cloudi_timestamp:milliseconds_monotonic()
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
        {expressions,                  ?DEFAULT_EXPRESSIONS},
        {use_utc,                      ?DEFAULT_USE_UTC},
        {debug,                        ?DEFAULT_DEBUG},
        {debug_level,                  ?DEFAULT_DEBUG_LEVEL}],
    [Expressions, UseUTC,
     Debug, DebugLevel] = cloudi_proplists:take_values(Defaults, Args),
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
                                           ProcessIndex, ProcessCount),
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
                expressions = ExpressionsStarted}}.

cloudi_service_handle_info(#timeout_async_active{trans_id = TransId},
                           #state{debug_level = DebugLogLevel,
                                  expressions = Expressions,
                                  sends = Sends} = State,
                           _Dispatcher) ->
    SendsNew = event_recv(Sends, Expressions, timeout, TransId, DebugLogLevel),
    {noreply, State#state{sends = SendsNew}};
cloudi_service_handle_info(#return_async_active{response = Response,
                                                trans_id = TransId},
                           #state{debug_level = DebugLogLevel,
                                  expressions = Expressions,
                                  sends = Sends} = State,
                           _Dispatcher) ->
    Result = try erlang:binary_to_integer(erlang:iolist_to_binary(Response)) of
        0 ->
            ok;
        _ ->
            error
    catch
        _ ->
            % response is not understood as a shell status code response
            ok
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
                                  time_offset_milliseconds = ValueOld,
                                  time_offset_monitor = Monitor,
                                  expressions = Expressions} = State,
                           _Dispatcher) ->
    ValueNew = time_offset_to_milliseconds(TimeOffset),
    ExpressionsNew = expressions_time_offset(Expressions,
                                             ValueOld - ValueNew, Service),
    {noreply, State#state{time_offset_milliseconds = ValueNew,
                          expressions = ExpressionsNew}}.

cloudi_service_terminate(_Reason, _Timeout, _State) ->
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

expressions_load([], Id, Expressions, _, _, _) ->
    {Id, Expressions};
expressions_load([{ExpressionStr, Args} | L], Id, Expressions,
                 ProcessIndex, ProcessIndex, ProcessCount) ->
    Defaults = [
        {description,                  undefined},
        {send_args,                    undefined}],
    [Description, SendArgs] = cloudi_proplists:take_values(Defaults, Args),
    [_ | _] = ExpressionStr,
    case Description of
        [_ | _] ->
            ok;
        undefined ->
            ok
    end,
    [_ | _] = SendArgs,
    Cron = cloudi_cron:new(ExpressionStr),
    Expression = #expression{description = Description,
                             definition = Cron,
                             send_args = SendArgs},
    expressions_load(L, Id + 1, maps:put(Id, Expression, Expressions),
                     (ProcessIndex + 1) rem ProcessCount,
                     ProcessIndex, ProcessCount);
expressions_load([{_, _} | L], Id, Expressions,
                 Index, ProcessIndex, ProcessCount) ->
    expressions_load(L, Id, Expressions,
                     (Index + 1) rem ProcessCount,
                     ProcessIndex, ProcessCount).

expressions_load(L, ProcessIndex, ProcessCount) ->
    expressions_load(L, 1, #{}, 0, ProcessIndex, ProcessCount).

expression_next(#expression{definition = Cron,
                            datetime_utc = DateTimeOldEventUTC} = Expression,
                Id, UseUTC, Service) ->
    {DateTimeNewEventUTC,
     MilliSecondsEvent} = expression_next_event(DateTimeOldEventUTC,
                                                Cron, UseUTC),
    Timer = erlang:send_after(MilliSecondsEvent, Service, {expression, Id}),
    Expression#expression{datetime_utc = DateTimeNewEventUTC,
                          timer = Timer}.

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
    MilliSecondsEvent = event_milliseconds(DateTimeNewEventUTC,
                                           DateTimeNowUTC, PastMilliSeconds),
    {DateTimeNewEventUTC,
     MilliSecondsEvent}.

expressions_start(Expressions, UseUTC, Service) ->
    maps:map(fun(Id, Expression) ->
        expression_next(Expression, Id, UseUTC, Service)
    end, Expressions).

expressions_time_offset(Expressions, MilliSecondsOffset, Service) ->
    maps:map(fun(Id, #expression{datetime_utc = DateTimeEventUTC,
                                 timer = TimerOld} = Expression) ->
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
                        erlang:send_after(MilliSecondsNew,
                                          Service, {expression, Id})
                end
        end,
        Expression#expression{timer = TimerNew}
    end, Expressions).

expression_event(Sends, Expressions, Id,
                 DebugLogLevel, UseUTC, Service, Dispatcher) ->
    Expression = maps:get(Id, Expressions),
    #expression{send_args = [ServiceName | _] = SendArgs} = Expression,
    ?LOG(DebugLogLevel,
         "\"~ts\" event sent to ~ts",
         [description(Expression), ServiceName]),
    SendsNew = event_send(Sends, SendArgs, Id, Service, Dispatcher),
    ExpressionNew = expression_next(Expression, Id, UseUTC, Service),
    {SendsNew,
     maps:put(Id, ExpressionNew, Expressions)}.

event_send(Sends, SendArgs, Id, Service, Dispatcher) ->
    SendStart = cloudi_timestamp:milliseconds_monotonic(),
    TransId = case erlang:apply(cloudi_service, send_async_active,
                                [Dispatcher | SendArgs]) of
        {ok, TransIdValue} ->
            TransIdValue;
        {error, timeout} ->
            TransIdValue = cloudi_service:trans_id(Dispatcher),
            Service ! #timeout_async_active{trans_id = TransIdValue},
            TransIdValue
    end,
    SendData = #send_data{id = Id,
                          send_start = SendStart},
    maps:put(TransId, SendData, Sends).

event_recv(Sends, Expressions, Result, TransId, DebugLogLevel) ->
    SendEnd = cloudi_timestamp:milliseconds_monotonic(),
    {SendData, SendsNew} = maps:take(TransId, Sends),
    #send_data{id = Id,
               send_start = SendStart} = SendData,
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
    SendsNew.

event_milliseconds(DateTimeEventUTC, DateTimeNowUTC, PastMilliSeconds) ->
    % subtraction must occur as gregorian seconds in UTC
    % to avoid DST offsets causing the difference to be invalid
    Seconds = calendar:datetime_to_gregorian_seconds(DateTimeEventUTC) -
              calendar:datetime_to_gregorian_seconds(DateTimeNowUTC),
    Seconds * 1000 - PastMilliSeconds.

datetime_event_utc(DateTime, Cron, UseUTC) ->
    DateTimeEvent = cloudi_cron:next_datetime(DateTime, Cron),
    if
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
