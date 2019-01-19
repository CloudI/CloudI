%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Service API Batch==
%%% A service that provides batch execution of CloudI services.
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
%%% @version 1.7.6 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_api_batch).
-author('mjtruog at protonmail dot com').

-behaviour(cloudi_service).

%% external interface
-export([services_add/4,
         services_add/5,
         services_remove/3,
         services_remove/4,
         services_restart/3,
         services_restart/4]).

%% cloudi_service callbacks
-export([cloudi_service_init/4,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/3]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").
-include_lib("cloudi_core/include/cloudi_service_api.hrl").

-define(DEFAULT_PURGE_ON_ERROR,              true).
        % Should the batch queue contents be purged when
        % a queue's currently running service fails
        % with an error and is unable to restart
        % (n.b., if a service configuration is unable
        %  to start (initialization fails) the queue is
        %  always purged with the error logged).
-define(DEFAULT_QUEUES,                        []).
        % List of {QueueName, Configs} to be used for services_add
-define(DEFAULT_QUEUES_STATIC,              false).
        % Disable dynamic modifications to the queues
        % (disables the external interface).
        % If queues_static is set to true,
        % stop_when_done must also be set to true.
-define(DEFAULT_STOP_WHEN_DONE,             false).
        % If queues contains entries, cause the
        % cloudi_service_api_batch service to stop successfully
        % once all queued service configurations have finished executing.

-record(queue,
    {
        count :: non_neg_integer(),
        data :: queue:queue(),
        service_id :: cloudi_service_api:service_id(),
        timeout_init = undefined
          :: undefined |
             cloudi_service_api:timeout_initialize_value_milliseconds(),
        terminate = false :: boolean(),
        terminate_timer = undefined :: undefined | reference()
    }).

-record(state,
    {
        purge_on_error :: boolean(),
        stop_when_done :: boolean(),
        queue_count = 0 :: non_neg_integer(),
        queues = cloudi_x_trie:new()
            :: cloudi_x_trie:cloudi_x_trie(), % queue name -> queue
        service :: cloudi_service:source()
    }).

-define(NAME_BATCH, "batch").
-define(FORMAT_ERLANG, "erl").
-define(FORMAT_JSON, "json").
-define(TIMEOUT_DELTA, 100). % milliseconds
-define(TERMINATE_INTERVAL, 500). % milliseconds

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

-type agent() :: cloudi:agent().
-type service_name() :: cloudi:service_name().
-type service_configurations() ::
    nonempty_list(cloudi_service_api:service_internal() |
                  cloudi_service_api:service_external() |
                  cloudi_service_api:service_proplist()).
-type timeout_milliseconds() :: cloudi:timeout_milliseconds().
-type module_response(Result) ::
    {{ok, Result}, NewAgent :: agent()} |
    {{error, cloudi:error_reason_sync()}, NewAgent :: agent()}.

-spec services_add(Agent :: agent(),
                   Prefix :: service_name(),
                   QueueName :: nonempty_string(),
                   Configs :: service_configurations()) ->
    module_response(CountQueued :: non_neg_integer() | {error, purged}).

services_add(Agent, Prefix, [I | _] = QueueName, [_ | _] = Configs)
    when is_integer(I) ->
    cloudi:send_sync(Agent, Prefix ++ ?NAME_BATCH,
                     {services_add, QueueName, Configs}).

-spec services_add(Agent :: agent(),
                   Prefix :: service_name(),
                   QueueName :: nonempty_string(),
                   Configs :: service_configurations(),
                   Timeout :: timeout_milliseconds()) ->
    module_response(CountQueued :: non_neg_integer() | {error, purged}).

services_add(Agent, Prefix, [I | _] = QueueName, [_ | _] = Configs, Timeout)
    when is_integer(I) ->
    cloudi:send_sync(Agent, Prefix ++ ?NAME_BATCH,
                     {services_add, QueueName, Configs}, Timeout).

-spec services_remove(Agent :: agent(),
                      Prefix :: service_name(),
                      QueueName :: nonempty_string()) ->
    module_response(ok | {error, not_found}).

services_remove(Agent, Prefix, [I | _] = QueueName)
    when is_integer(I) ->
    cloudi:send_sync(Agent, Prefix ++ ?NAME_BATCH,
                     {services_remove, QueueName}).

-spec services_remove(Agent :: agent(),
                      Prefix :: service_name(),
                      QueueName :: nonempty_string(),
                      Timeout :: timeout_milliseconds()) ->
    module_response(ok | {error, not_found}).

services_remove(Agent, Prefix, [I | _] = QueueName, Timeout)
    when is_integer(I) ->
    cloudi:send_sync(Agent, Prefix ++ ?NAME_BATCH,
                     {services_remove, QueueName}, Timeout).

-spec services_restart(Agent :: agent(),
                       Prefix :: service_name(),
                       QueueName :: nonempty_string()) ->
    module_response(ok | {error, not_found}).

services_restart(Agent, Prefix, [I | _] = QueueName)
    when is_integer(I) ->
    cloudi:send_sync(Agent, Prefix ++ ?NAME_BATCH,
                     {services_restart, QueueName}).

-spec services_restart(Agent :: agent(),
                       Prefix :: service_name(),
                       QueueName :: nonempty_string(),
                       Timeout :: timeout_milliseconds()) ->
    module_response(ok | {error, not_found}).

services_restart(Agent, Prefix, [I | _] = QueueName, Timeout)
    when is_integer(I) ->
    cloudi:send_sync(Agent, Prefix ++ ?NAME_BATCH,
                     {services_restart, QueueName}, Timeout).

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, Prefix, _Timeout, Dispatcher) ->
    Defaults = [
        {purge_on_error,               ?DEFAULT_PURGE_ON_ERROR},
        {queues,                       ?DEFAULT_QUEUES},
        {queues_static,                ?DEFAULT_QUEUES_STATIC},
        {stop_when_done,               ?DEFAULT_STOP_WHEN_DONE}],
    [PurgeOnError, QueuesList, QueuesStatic,
     StopWhenDone] = cloudi_proplists:take_values(Defaults, Args),
    true = is_boolean(PurgeOnError),
    true = is_boolean(QueuesStatic),
    true = is_boolean(StopWhenDone),
    false = cloudi_service_name:pattern(Prefix),
    1 = cloudi_service:process_count_max(Dispatcher),
    Service = cloudi_service:self(Dispatcher),
    case QueuesList of
        [] ->
            false = QueuesStatic,
            false = StopWhenDone,
            ok;
        [_ | _] ->
            Service ! {init, QueuesList}
    end,
    if
        QueuesStatic =:= true ->
            true = StopWhenDone,
            ok;
        QueuesStatic =:= false ->
            cloudi_service:subscribe(Dispatcher, ?NAME_BATCH),
            Interface = [{"services_add", "/post"},
                         {"services_remove", "/get"},
                         {"services_restart", "/get"}],
            Suffixes = lists:flatmap(fun({MethodName, FormatSuffix}) ->
                lists:flatmap(fun(Format) ->
                    FormatName = "/?/" ++ MethodName ++ "." ++ Format,
                    [FormatName,
                     FormatName ++ FormatSuffix]
                end, [?FORMAT_ERLANG, ?FORMAT_JSON])
            end, Interface),
            lists:foreach(fun(Suffix) ->
                cloudi_service:subscribe(Dispatcher, ?NAME_BATCH ++ Suffix)
            end, Suffixes)
    end,
    {ok, #state{purge_on_error = PurgeOnError,
                stop_when_done = StopWhenDone,
                service = Service}}.

cloudi_service_handle_request(_Type, Name, Pattern, _RequestInfo, Request,
                              _Timeout, _Priority, _TransId, _Pid,
                              State, _Dispatcher) ->
    {Response,
     StateNew} = case cloudi_service_name:parse_with_suffix(Name, Pattern) of
        {[], _} ->
            case Request of
                {services_add, QueueName, Configs} ->
                    queue_add(QueueName, Configs, State);
                {services_remove, QueueName} ->
                    queue_remove(QueueName, State);
                {services_restart, QueueName} ->
                    queue_restart(QueueName, State)
            end;
        {[QueueName], "/" ++ MethodSuffix} ->
            MethodFormat = cloudi_string:beforel($/, MethodSuffix, input),
            {MethodName, Format} = cloudi_string:splitr($., MethodFormat),
            case MethodName of
                "services_add" ->
                    Configs = external_format_from(Format, services_add,
                                                   Request),
                    external_format_to(Format, services_add,
                                       queue_add(QueueName, Configs, State));
                "services_remove" ->
                    external_format_to(Format, services_remove,
                                       queue_remove(QueueName, State));
                "services_restart" ->
                    external_format_to(Format, services_restart,
                                       queue_restart(QueueName, State))
            end
    end,
    {reply, Response, StateNew}.

cloudi_service_handle_info({init, QueuesList},
                           State, _Dispatcher) ->
    {noreply, queue_adds(QueuesList, State)};
cloudi_service_handle_info({aspects_init_after, QueueName, TimeoutInit},
                           State, _Dispatcher) ->
    {noreply, running_init(QueueName, TimeoutInit, State)};
cloudi_service_handle_info({aspects_terminate_before,
                            QueueName, Reason, TimeoutTerminate},
                           State, _Dispatcher) ->
    {noreply, running_terminate(QueueName, Reason, TimeoutTerminate, State)};
cloudi_service_handle_info({terminate, QueueName, Reason, Timeout},
                           State, _Dispatcher) ->
    {noreply, running_stopping(QueueName, Reason, Timeout, State)};
cloudi_service_handle_info({terminated, QueueName, Reason},
                           State, _Dispatcher) ->
    PurgeQueue = purge_queue(Reason, State),
    case running_stopped(PurgeQueue, QueueName, State) of
        #state{stop_when_done = true,
               queue_count = 0} = StateNew ->
            {stop, shutdown, StateNew};
        StateNew ->
            {noreply, StateNew}
    end.

cloudi_service_terminate(_Reason, _Timeout, _State) ->
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

queue_add(QueueName, [Config | ConfigsTail] = Configs,
          #state{queue_count = QueueCount,
                 queues = Queues,
                 service = Service} = State) ->
    {QueueCountNew,
     QueueNew} = case cloudi_x_trie:find(QueueName, Queues) of
        {ok, #queue{count = Count,
                    data = Data} = Queue} ->
            DataNew = queue:join(Data, queue:from_list(Configs)),
            {QueueCount,
             Queue#queue{count = Count + length(Configs),
                         data = DataNew}};
        error ->
            case service_add(Config, QueueName, Service) of
                {ok, ServiceId} ->
                    Data = queue:from_list(ConfigsTail),
                    {QueueCount + 1,
                     #queue{count = length(ConfigsTail),
                            data = Data,
                            service_id = ServiceId}};
                {error, _} ->
                    {QueueCount, undefined}
            end
    end,
    {Response,
     QueuesNew} = case QueueNew of
        #queue{count = CountNew} ->
            {CountNew,
             cloudi_x_trie:store(QueueName, QueueNew, Queues)};
        undefined ->
            {{error, purged},
             Queues}
    end,
    {Response,
     State#state{queue_count = QueueCountNew,
                 queues = QueuesNew}}.

queue_remove(QueueName,
             #state{queue_count = QueueCount,
                    queues = Queues} = State) ->
    case cloudi_x_trie:find(QueueName, Queues) of
        {ok, #queue{service_id = ServiceId}} ->
            cloudi_service_api:services_remove([ServiceId], infinity),
            {ok,
             State#state{queue_count = QueueCount - 1,
                         queues = cloudi_x_trie:erase(QueueName, Queues)}};
        error ->
            {{error, not_found}, State}
    end.

queue_restart(QueueName,
              #state{queues = Queues} = State) ->
    Result = case cloudi_x_trie:find(QueueName, Queues) of
        {ok, #queue{service_id = ServiceId}} ->
            case cloudi_service_api:services_restart([ServiceId], infinity) of
                ok ->
                    ok;
                {error, {service_not_found, ServiceId}} ->
                    {error, not_found}
            end;
        error ->
            {error, not_found}
    end,
    {Result, State}.

queue_adds([], State) ->
    State;
queue_adds([{QueueName, Configs} | QueuesList], State) ->
    {_, StateNew} = queue_add(QueueName, Configs, State),
    queue_adds(QueuesList, StateNew).

external_format_from(?FORMAT_ERLANG, Method, Request) ->
    cloudi_service_api_requests:from_erl(Method, Request);
external_format_from(?FORMAT_JSON, Method, Request) ->
    cloudi_service_api_requests:from_json(Method, Request).

external_format_to(?FORMAT_ERLANG, _Method, {Response, State}) ->
    {convert_term_to_erlang(Response), State};
external_format_to(?FORMAT_JSON, Method, {Response, State}) ->
    {convert_term_to_json(Response, Method), State}.

running_init(QueueName, TimeoutInit,
             #state{queues = Queues} = State) ->
    Queue = cloudi_x_trie:fetch(QueueName, Queues),
    #queue{terminate_timer = TerminateTimer} = Queue,
    if
        is_reference(TerminateTimer) ->
            erlang:cancel_timer(TerminateTimer);
        TerminateTimer =:= undefined ->
            ok
    end,
    QueueNew = Queue#queue{timeout_init = TimeoutInit,
                           terminate = false,
                           terminate_timer = undefined},
    State#state{queues = cloudi_x_trie:store(QueueName, QueueNew, Queues)}.

running_terminate(QueueName, Reason, TimeoutTerminate,
                  #state{queues = Queues,
                         service = Service} = State) ->
    case cloudi_x_trie:find(QueueName, Queues) of
        {ok, #queue{terminate = true}} ->
            State;
        {ok, #queue{timeout_init = TimeoutInit,
                    terminate = false} = Queue} ->
            Timeout = TimeoutTerminate + TimeoutInit + ?TIMEOUT_DELTA,
            TimeoutNew = Timeout - ?TERMINATE_INTERVAL,
            TerminateTimer = if
                TimeoutNew > 0 ->
                    erlang:send_after(?TERMINATE_INTERVAL, Service,
                                      {terminate, QueueName, Reason,
                                       TimeoutNew});
                true ->
                    erlang:send_after(Timeout, Service,
                                      {terminated, QueueName, Reason})
            end,
            QueueNew = Queue#queue{terminate = true,
                                   terminate_timer = TerminateTimer},
            State#state{queues = cloudi_x_trie:store(QueueName,
                                                     QueueNew, Queues)};
        error ->
            State
    end.

running_stopping(QueueName, Reason, Timeout,
                 #state{queues = Queues,
                        service = Service} = State) ->
    Queue = cloudi_x_trie:fetch(QueueName, Queues),
    #queue{service_id = ServiceId,
           terminate = Terminate} = Queue,
    case cloudi_service_api:service_subscriptions(ServiceId, infinity) of
        {error, not_found} ->
            PurgeQueue = purge_queue(Reason, State),
            QueueNew = Queue#queue{terminate = true,
                                   terminate_timer = undefined},
            QueuesNew = cloudi_x_trie:store(QueueName, QueueNew, Queues),
            running_stopped(PurgeQueue, QueueName,
                            State#state{queues = QueuesNew});
        _ when Terminate =:= true ->
            TimeoutNew = Timeout - ?TERMINATE_INTERVAL,
            TerminateTimer = if
                TimeoutNew > 0 ->
                    erlang:send_after(?TERMINATE_INTERVAL, Service,
                                      {terminate, QueueName, Reason,
                                       TimeoutNew});
                true ->
                    erlang:send_after(Timeout, Service,
                                      {terminated, QueueName, Reason})
            end,
            QueueNew = Queue#queue{terminate_timer = TerminateTimer},
            State#state{queues = cloudi_x_trie:store(QueueName,
                                                     QueueNew, Queues)};
        _ ->
            % terminate_timer was cancelled after the message was queued
            State
    end.

running_stopped(true, QueueName,
                #state{queue_count = QueueCount,
                       queues = Queues} = State) ->
    #queue{terminate = Terminate} = cloudi_x_trie:fetch(QueueName, Queues),
    {QueueCountNew,
     QueuesNew} = if
        Terminate =:= true ->
            {QueueCount - 1,
             cloudi_x_trie:erase(QueueName, Queues)};
        Terminate =:= false ->
            % terminate_timer was cancelled after the message was queued
            {QueueCount,
             Queues}
    end,
    State#state{queue_count = QueueCountNew,
                queues = QueuesNew};
running_stopped(false, QueueName,
                #state{queue_count = QueueCount,
                       queues = Queues,
                       service = Service} = State) ->
    {QueueCountNew,
     QueuesNew} = case cloudi_x_trie:fetch(QueueName, Queues) of
        #queue{terminate = false} ->
            % terminate_timer was cancelled after the message was queued
            {QueueCount,
             Queues};
        #queue{count = 0} ->
            {QueueCount - 1,
             cloudi_x_trie:erase(QueueName, Queues)};
        #queue{count = Count,
               data = Data} = Queue ->
            {{value, Config}, DataNew} = queue:out(Data),
            case service_add(Config, QueueName, Service) of
                {ok, ServiceId} ->
                    QueueNew = Queue#queue{count = Count - 1,
                                           data = DataNew,
                                           service_id = ServiceId,
                                           terminate = false,
                                           terminate_timer = undefined},
                    {QueueCount,
                     cloudi_x_trie:store(QueueName, QueueNew, Queues)};
                {error, _} ->
                    {QueueCount - 1,
                     cloudi_x_trie:erase(QueueName, Queues)}
            end
    end,
    State#state{queue_count = QueueCountNew,
                queues = QueuesNew}.

purge_queue(Reason, #state{purge_on_error = true}) ->
    case Reason of
        {shutdown, _} ->
            false;
        shutdown ->
            false;
        _ ->
            true
    end;
purge_queue(_, #state{purge_on_error = false}) ->
    false.

service_add(Config, QueueName, Service) ->
    ConfigBatch = service_add_config(Config, QueueName, Service),
    case cloudi_service_api:services_add([ConfigBatch], infinity) of
        {ok, [ServiceId]} ->
            {ok, ServiceId};
        {error, Reason} = Error ->
            ?LOG_ERROR("config ~p~nfailure ~p purged \"~s\" queue",
                       [ConfigBatch, Reason, QueueName]),
            Error
    end.

service_add_config(#internal{options = Options} = ServiceConfig,
                   QueueName, Service) ->
    ServiceConfig#internal{options = add_options(internal, Options,
                                                 QueueName, Service)};
service_add_config(#external{options = Options} = ServiceConfig,
                   QueueName, Service) ->
    ServiceConfig#external{options = add_options(external, Options,
                                                 QueueName, Service)};
service_add_config([_ | _] = ServiceConfig, QueueName, Service) ->
    Type = case lists:keyfind(module, 1, ServiceConfig) of
        {_, _} ->
            internal;
        false ->
            external
    end,
    case lists:keytake(options, 1, ServiceConfig) of
        {value, {_, Options}, NextServiceConfig} ->
            [{options, add_options(Type, Options, QueueName, Service)} |
             NextServiceConfig];
        false ->
            [{options, add_options(Type, [], QueueName, Service)} |
             ServiceConfig]
    end.

add_options(Type, Options0, QueueName, Service) ->
    OptionsN = add_option(Type, aspects_init_after,
                          Options0, QueueName, Service),
    add_option(Type, aspects_terminate_before,
               OptionsN, QueueName, Service).

add_option(Type, Option, Options, QueueName, Service) ->
    Aspect = if
        Option =:= aspects_init_after ->
            if
                Type =:= internal ->
                    fun(_Args, _Prefix, Timeout, State, _Dispatcher) ->
                        Service ! {Option, QueueName, Timeout},
                        {ok, State}
                    end;
                Type =:= external ->
                    fun(_CommandLine, _Prefix, Timeout, State) ->
                        Service ! {Option, QueueName, Timeout},
                        {ok, State}
                    end
            end;
        Option =:= aspects_terminate_before ->
            fun(Reason, Timeout, State) ->
                Service ! {Option, QueueName, Reason, Timeout},
                {ok, State}
            end
    end,
    case lists:keytake(Option, 1, Options) of
        {value, {_, OptionL}, NextOptions} ->
            if
                Option =:= aspects_init_after ->
                    [{Option, [Aspect | OptionL]} | NextOptions];
                Option =:= aspects_terminate_before ->
                    [{Option, OptionL ++ [Aspect]} | NextOptions]
            end;
        false ->
            [{Option, [Aspect]} | Options]
    end.

%convert_term_to_erlang({ok, Result}) ->
%    convert_term_to_erlang_string(Result);
convert_term_to_erlang(Result) ->
    convert_term_to_erlang_string(Result).

convert_term_to_erlang_string(Result) ->
    cloudi_string:format_to_binary("~p", [Result]).

convert_term_to_json(ok, _) ->
    json_encode([{<<"success">>, true}]);
convert_term_to_json({error, Reason}, _) ->
    ReasonBinary = cloudi_string:term_to_binary_compact(Reason),
    json_encode([{<<"success">>, false},
                 {<<"error">>, ReasonBinary}]);
convert_term_to_json(Term, Method) ->
    json_encode([{<<"success">>, true},
                 {erlang:atom_to_binary(Method, utf8), Term}]).

json_encode(Term) ->
    cloudi_x_jsx:encode(Term, [{indent, 1}]).

