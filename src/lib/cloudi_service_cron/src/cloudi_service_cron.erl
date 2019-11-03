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
        timer = undefined :: reference() | undefined
    }).

-record(state,
    {
        service :: cloudi_service:source(),
        utc :: boolean(),
        debug_level :: off | trace | debug | info | warn | error | fatal,
        id_next :: expression_id(),
        expressions :: #{expression_id() := #expression{}},
        sends = #{} :: #{cloudi_service:trans_id() := expression_id()}
    }).

% All cron expression events will occur a small number of milliseconds after
% the datetime specified to anticipate any Erlang timer jitter
% (ensuring that the event datetime is always reached by the timer)
-define(MILLISECONDS_OFFSET, 10).

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
    ExpressionsStarted = expressions_start(ExpressionsLoaded, Service, UseUTC),
    {ok, #state{service = Service,
                utc = UseUTC,
                debug_level = DebugLogLevel,
                id_next = IdNext,
                expressions = ExpressionsStarted}}.

cloudi_service_handle_info(#timeout_async_active{trans_id = TransId},
                           #state{debug_level = DebugLogLevel,
                                  expressions = Expressions,
                                  sends = Sends} = State,
                           _Dispatcher) ->
    SendsNew = event_recv(timeout, TransId, Expressions, Sends, DebugLogLevel),
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
    SendsNew = event_recv(Result, TransId, Expressions, Sends, DebugLogLevel),
    {noreply, State#state{sends = SendsNew}};
cloudi_service_handle_info({expression, Id},
                           #state{service = Service,
                                  utc = UseUTC,
                                  debug_level = DebugLogLevel,
                                  expressions = Expressions,
                                  sends = Sends} = State,
                           Dispatcher) ->
    {ExpressionsNew,
     SendsNew} = expression_event(Expressions, Sends, Id,
                                  Service, UseUTC, DebugLogLevel, Dispatcher),
    {noreply, State#state{expressions = ExpressionsNew,
                          sends = SendsNew}}.

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

expression_next_event(Cron, UseUTC) ->
    {DateTimeNowUTC, PastMilliSeconds} = datetime_now_utc(),
    DateTimeNow = if
        UseUTC =:= true ->
            DateTimeNowUTC;
        UseUTC =:= false ->
            calendar:universal_time_to_local_time(DateTimeNowUTC)
    end,
    DateTimeEvent = cloudi_cron:next_datetime(DateTimeNow, Cron),
    DateTimeEventUTC = if
        UseUTC =:= true ->
            DateTimeEvent;
        UseUTC =:= false ->
            datetime_event_utc(DateTimeEvent)
    end,
    % subtraction must occur as gregorian seconds in UTC
    % to avoid DST offsets causing the difference to be invalid
    Seconds = calendar:datetime_to_gregorian_seconds(DateTimeEventUTC) -
              calendar:datetime_to_gregorian_seconds(DateTimeNowUTC),
    Seconds * 1000 - PastMilliSeconds.

expression_next(Id, Cron, Service, UseUTC) ->
    AbsoluteNow = cloudi_timestamp:milliseconds_monotonic(),
    MilliSecondsEvent = expression_next_event(Cron, UseUTC),
    erlang:send_after(AbsoluteNow + MilliSecondsEvent,
                      Service, {expression, Id}, [{abs, true}]).

expressions_start(Expressions, Service, UseUTC) ->
    maps:map(fun(Id, #expression{definition = Cron} = Expression) ->
        Timer = expression_next(Id, Cron, Service, UseUTC),
        Expression#expression{timer = Timer}
    end, Expressions).

expression_event(Expressions, Sends, Id,
                 Service, UseUTC, DebugLogLevel, Dispatcher) ->
    Expression = maps:get(Id, Expressions),
    #expression{definition = Cron,
                send_args = [ServiceName | _] = SendArgs} = Expression,
    ?LOG(DebugLogLevel,
         "\"~s\" event sent to ~s",
         [description(Expression), ServiceName]),
    TransId = event_send(SendArgs, Service, Dispatcher),
    Timer = expression_next(Id, Cron, Service, UseUTC),
    {maps:put(Id, Expression#expression{timer = Timer}, Expressions),
     maps:put(TransId, Id, Sends)}.

event_send(SendArgs, Service, Dispatcher) ->
    case erlang:apply(cloudi_service, send_async_active,
                      [Dispatcher | SendArgs]) of
        {ok, TransId} ->
            TransId;
        {error, timeout} ->
            TransId = cloudi_service:trans_id(Dispatcher),
            Service ! #timeout_async_active{trans_id = TransId},
            TransId
    end.

event_recv(Result, TransId, Expressions, Sends, DebugLogLevel) ->
    {Id, SendsNew} = maps:take(TransId, Sends),
    Expression = maps:get(Id, Expressions),
    #expression{send_args = [ServiceName | _]} = Expression,
    if
        Result =:= ok ->
            ?LOG(DebugLogLevel,
                 "\"~s\" event completed at ~s",
                 [description(Expression), ServiceName]);
        Result =:= timeout ->
            ?LOG_ERROR("\"~s\" event timeout at ~s",
                       [description(Expression), ServiceName]);
        Result =:= error ->
            ?LOG_ERROR("\"~s\" event failed at ~s",
                       [description(Expression), ServiceName])
    end,
    SendsNew.

datetime_now_utc() ->
    {_, _, MicroSeconds} = Now = erlang:timestamp(),
    {calendar:now_to_universal_time(Now),
     MicroSeconds div 1000 - ?MILLISECONDS_OFFSET}.

datetime_event_utc(DateTime) ->
    case calendar:local_time_to_universal_time_dst(DateTime) of
        [] ->
            % when DST sets clocks ahead,
            % pick the next valid datetime after the DST change
            datetime_event_utc(datetime_add(DateTime, 1));
        [DateTimeUTC] ->
            DateTimeUTC;
        [_, DateTimeUTC] ->
            % when DST sets clocks back,
            % pick the UTC datetime after the DST change
            DateTimeUTC
    end.

datetime_add(DateTime, SecondsIncr) ->
    Seconds = calendar:datetime_to_gregorian_seconds(DateTime),
    calendar:gregorian_seconds_to_datetime(Seconds + SecondsIncr).

description(#expression{description = [_ | _] = Description}) ->
    Description;
description(#expression{definition = Cron}) ->
    cloudi_cron:expression(Cron).

