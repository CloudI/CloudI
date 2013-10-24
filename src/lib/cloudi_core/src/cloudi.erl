%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Erlang Interface==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2013, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2013 Michael Truog
%%% @version 1.3.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([new/0,
         new/1,
         destinations_refresh/2,
         get_pid/2,
         get_pid/3,
         get_pids/2,
         get_pids/3,
         send_async/3,
         send_async/4,
         send_async/5,
         send_async/6,
         send_async/7,
         send_async_passive/3,
         send_async_passive/4,
         send_async_passive/5,
         send_async_passive/6,
         send_async_passive/7,
         send_sync/3,
         send_sync/4,
         send_sync/5,
         send_sync/6,
         send_sync/7,
         mcast_async/3,
         mcast_async/4,
         mcast_async/6,
         mcast_async_passive/3,
         mcast_async_passive/4,
         mcast_async_passive/6,
         recv_async/1,
         recv_async/2,
         recv_async/3,
         timeout_async/1,
         timeout_sync/1]).

-include("cloudi_logger.hrl").
-include("cloudi_constants.hrl").
-include("cloudi_configuration_defaults.hrl").

-type service_name() :: string().
-type service_name_pattern() :: string().
-type request_info() :: any().
-type request() :: any().
-type response_info() :: any().
-type response() :: any().
-type timeout_milliseconds() :: 0..4294967295 | undefined.
-type priority() :: ?PRIORITY_HIGH..?PRIORITY_LOW | undefined. % (high)..(low)
-type trans_id() :: <<_:128>>. % version 1 UUID
-type pattern_pid() :: {service_name_pattern(), pid()}.
-export_type([service_name/0,
              service_name_pattern/0,
              request_info/0, request/0,
              response_info/0, response/0,
              timeout_milliseconds/0,
              priority/0,
              trans_id/0,
              pattern_pid/0]).

-record(cloudi_context,
        {
            dest_refresh :: cloudi_service_api:dest_refresh(),
            dest_refresh_delay
                :: cloudi_service_api:dest_refresh_delay_milliseconds(),
            timeout_async :: cloudi_service_api:timeout_milliseconds(),
            timeout_sync :: cloudi_service_api:timeout_milliseconds(),
            priority_default :: priority(),
            scope :: atom(),
            receiver :: pid(),
            uuid_generator :: cloudi_x_uuid:state(),
            cpg_data = cloudi_x_cpg_data:get_empty_groups() :: any()
        }).

-type context() :: #cloudi_context{}.
-export_type([context/0]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a CloudI context.===
%% @end
%%-------------------------------------------------------------------------
-spec new() ->
    #cloudi_context{}.

new() ->
    new([]).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a CloudI context.===
%% If a lazy destination refresh method is used, make sure to receive the
%% {cloudi_cpg_data, Groups} message and pass it to the
%% destinations_refresh/2 function.
%% @end
%%-------------------------------------------------------------------------
-spec new(Settings :: list({atom(), any()})) ->
    #cloudi_context{}.

new(Settings)
    when is_list(Settings) ->
    Defaults = [
        {dest_refresh,                     ?DEFAULT_DEST_REFRESH},
        {dest_refresh_start,         ?DEFAULT_DEST_REFRESH_START},
        {dest_refresh_delay,         ?DEFAULT_DEST_REFRESH_DELAY},
        {timeout_async,                   ?DEFAULT_TIMEOUT_ASYNC},
        {timeout_sync,                     ?DEFAULT_TIMEOUT_SYNC},
        {priority_default,                     ?DEFAULT_PRIORITY},
        {scope,                                   ?DEFAULT_SCOPE}
        ],
    [DestRefresh, DestRefreshStart, DestRefreshDelay,
     DefaultTimeoutAsync, DefaultTimeoutSync,
     PriorityDefault, Scope] =
        cloudi_proplists:take_values(Defaults, Settings),
    true = (DestRefresh =:= immediate_closest) orelse
           (DestRefresh =:= lazy_closest) orelse
           (DestRefresh =:= immediate_furthest) orelse
           (DestRefresh =:= lazy_furthest) orelse
           (DestRefresh =:= immediate_random) orelse
           (DestRefresh =:= lazy_random) orelse
           (DestRefresh =:= immediate_local) orelse
           (DestRefresh =:= lazy_local) orelse
           (DestRefresh =:= immediate_remote) orelse
           (DestRefresh =:= lazy_remote) orelse
           (DestRefresh =:= immediate_newest) orelse
           (DestRefresh =:= lazy_newest) orelse
           (DestRefresh =:= immediate_oldest) orelse
           (DestRefresh =:= lazy_oldest),
    true = is_integer(DestRefreshStart) and
           (DestRefreshStart > ?TIMEOUT_DELTA),
    true = is_integer(DestRefreshDelay) and
           (DestRefreshDelay > ?TIMEOUT_DELTA),
    true = is_integer(DefaultTimeoutAsync) and
           (DefaultTimeoutAsync > ?TIMEOUT_DELTA),
    true = is_integer(DefaultTimeoutSync) and
           (DefaultTimeoutSync > ?TIMEOUT_DELTA),
    true = (PriorityDefault >= ?PRIORITY_HIGH) and
           (PriorityDefault =< ?PRIORITY_LOW),
    true = is_atom(Scope),
    ConfiguredScope = ?SCOPE_ASSIGN(Scope),
    ok = cloudi_x_cpg:scope_exists(ConfiguredScope),
    Self = self(),
    {ok, MacAddress} = application:get_env(cloudi_core, mac_address),
    UUID = cloudi_x_uuid:new(Self, [{timestamp_type, erlang},
                                    {mac_address, MacAddress}]),
    ok = destination_refresh_first(DestRefresh, DestRefreshStart, Scope),
    #cloudi_context{
        dest_refresh = DestRefresh,
        dest_refresh_delay = DestRefreshDelay,
        timeout_async = DefaultTimeoutAsync,
        timeout_sync = DefaultTimeoutSync,
        priority_default = PriorityDefault,
        scope = ConfiguredScope,
        receiver = Self,
        uuid_generator = UUID
    }.

%%-------------------------------------------------------------------------
%% @doc
%% ===Refresh destination lookup data.===
%% Must be called if using a lazy destination refresh method.
%% The {cloudi_cpg_data, Groups} message must be received and processed
%% by this function.
%% @end
%%-------------------------------------------------------------------------

-spec destinations_refresh(Context :: context(),
                           Message :: {cloudi_cpg_data, any()}) ->
    context().

destinations_refresh(#cloudi_context{
                         dest_refresh = DestRefresh,
                         dest_refresh_delay = DestRefreshDelay,
                         scope = Scope} = Context,
                     {cloudi_cpg_data, Groups}) ->
    ok = destination_refresh_start(DestRefresh, DestRefreshDelay, Scope),
    Context#cloudi_context{cpg_data = Groups}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a service destination based on a service name.===
%% @end
%%-------------------------------------------------------------------------

-spec get_pid(Context :: context() | cloudi_service:dispatcher(),
              Name :: service_name()) ->
    {'ok', PatternPid :: pattern_pid()} |
    {'error', Reason :: any()}.

get_pid(Dispatcher, Name)
    when is_pid(Dispatcher) ->
    cloudi_service:get_pid(Dispatcher, Name);

get_pid(#cloudi_context{timeout_sync = DefaultTimeoutSync} = Context, Name) ->
    get_pid(Context, Name, DefaultTimeoutSync).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a service destination based on a service name.===
%% @end
%%-------------------------------------------------------------------------

-spec get_pid(Context :: context() | cloudi_service:dispatcher(),
              Name :: service_name(),
              Timeout :: timeout_milliseconds()) ->
    {'ok', PatternPid :: pattern_pid()} |
    {'error', Reason :: any()}.

get_pid(Dispatcher, Name, Timeout)
    when is_pid(Dispatcher) ->
    cloudi_service:get_pid(Dispatcher, Name, Timeout);

get_pid(#cloudi_context{dest_refresh = DestRefresh,
                        scope = Scope,
                        cpg_data = Groups} = Context, Name, Timeout)
    when is_list(Name), is_integer(Timeout),
         Timeout >= 0 ->
    case destination_get(DestRefresh, Scope, Name, Groups,
                         Timeout + ?TIMEOUT_DELTA) of
        {error, {Reason, Name}}
        when (Reason =:= no_process orelse Reason =:= no_such_group),
             Timeout >= ?SEND_SYNC_INTERVAL ->
            receive after ?SEND_SYNC_INTERVAL -> ok end,
            get_pid(Context, Name,
                    Timeout - ?SEND_SYNC_INTERVAL);
        {error, _} = Error ->
            Error;
        {ok, Pattern, Pid} ->
            {ok, {Pattern, Pid}}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get all service destinations based on a service name.===
%% @end
%%-------------------------------------------------------------------------

-spec get_pids(Context :: context() | cloudi_service:dispatcher(),
               Name :: service_name()) ->
    {'ok', PatternPids :: list(pattern_pid())} |
    {'error', Reason :: any()}.

get_pids(Dispatcher, Name)
    when is_pid(Dispatcher) ->
    cloudi_service:get_pids(Dispatcher, Name);

get_pids(#cloudi_context{timeout_sync = DefaultTimeoutSync} = Context, Name) ->
    get_pids(Context, Name, DefaultTimeoutSync).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get all service destinations based on a service name.===
%% @end
%%-------------------------------------------------------------------------

-spec get_pids(Context :: context() | cloudi_service:dispatcher(),
               Name :: service_name(),
               Timeout :: timeout_milliseconds()) ->
    {'ok', PatternPids :: list(pattern_pid())} |
    {'error', Reason :: any()}.

get_pids(Dispatcher, Name, Timeout)
    when is_pid(Dispatcher) ->
    cloudi_service:get_pids(Dispatcher, Name, Timeout);

get_pids(#cloudi_context{dest_refresh = DestRefresh,
                         scope = Scope,
                         cpg_data = Groups} = Context, Name, Timeout)
    when is_list(Name), is_integer(Timeout),
         Timeout >= 0 ->
    case destination_all(DestRefresh, Scope, Name, Groups,
                         Timeout + ?TIMEOUT_DELTA) of
        {error, {no_such_group, Name}}
        when Timeout >= ?SEND_SYNC_INTERVAL ->
            receive after ?SEND_SYNC_INTERVAL -> ok end,
            get_pids(Context, Name,
                     Timeout - ?SEND_SYNC_INTERVAL);
        {error, _} = Error ->
            Error;
        {ok, Pattern, Pids} ->
            {ok, [{Pattern, Pid} || Pid <- Pids]}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_async(Context :: context() | cloudi_service:dispatcher(),
                 Name :: service_name(),
                 Request :: request()) ->
    {'ok', TransId :: trans_id()} |
    {'error', Reason :: any()}.

send_async(Dispatcher, Name, Request)
    when is_pid(Dispatcher) ->
    cloudi_service:send_async(Dispatcher, Name, Request);

send_async(Context, Name, Request) ->
    send_async(Context, Name, <<>>, Request,
               undefined, undefined, undefined).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_async(Context :: context() | cloudi_service:dispatcher(),
                 Name :: service_name(),
                 Request :: request(),
                 Timeout :: timeout_milliseconds() | 'undefined') ->
    {'ok', TransId :: trans_id()} |
    {'error', Reason :: any()}.

send_async(Dispatcher, Name, Request, Timeout)
    when is_pid(Dispatcher) ->
    cloudi_service:send_async(Dispatcher, Name, Request, Timeout);

send_async(Context, Name, Request, Timeout) ->
    send_async(Context, Name, <<>>, Request,
               Timeout, undefined, undefined).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_async(Context :: context() | cloudi_service:dispatcher(),
                 Name :: service_name(),
                 Request :: request(),
                 Timeout :: timeout_milliseconds() | 'undefined',
                 PatternPid :: pattern_pid()) ->
    {'ok', TransId :: trans_id()} |
    {'error', Reason :: any()}.

send_async(Dispatcher, Name, Request, Timeout, PatternPid)
    when is_pid(Dispatcher) ->
    cloudi_service:send_async(Dispatcher, Name, Request, Timeout, PatternPid);

send_async(Context, Name, Request, Timeout, PatternPid) ->
    send_async(Context, Name, <<>>, Request,
               Timeout, undefined, PatternPid).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_async(Context :: context() | cloudi_service:dispatcher(),
                 Name :: service_name(),
                 RequestInfo :: request_info(),
                 Request :: request(),
                 Timeout :: timeout_milliseconds() | 'undefined',
                 Priority :: priority() | 'undefined') ->
    {'ok', TransId :: trans_id()} |
    {'error', Reason :: any()}.

send_async(Dispatcher, Name, RequestInfo, Request,
           Timeout, Priority)
    when is_pid(Dispatcher) ->
    cloudi_service:send_async(Dispatcher, Name, RequestInfo, Request,
                              Timeout, Priority);

send_async(Context, Name, RequestInfo, Request,
           Timeout, Priority) ->
    send_async(Context, Name, RequestInfo, Request,
               Timeout, Priority, undefined).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_async(Context :: context() | cloudi_service:dispatcher(),
                 Name :: service_name(),
                 RequestInfo :: request_info(),
                 Request :: request(),
                 Timeout :: timeout_milliseconds() | 'undefined',
                 Priority :: priority() | 'undefined',
                 PatternPid :: pattern_pid() | 'undefined') ->
    {'ok', TransId :: trans_id()} |
    {'error', Reason :: any()}.

send_async(Dispatcher, Name, RequestInfo, Request,
           Timeout, Priority, PatternPid)
    when is_pid(Dispatcher) ->
    cloudi_service:send_async(Dispatcher, Name, RequestInfo, Request,
                              Timeout, Priority, PatternPid);

send_async(#cloudi_context{timeout_async = DefaultTimeoutAsync} = Context,
           Name, RequestInfo, Request,
           undefined, Priority, PatternPid) ->
    send_async(Context, Name, RequestInfo, Request,
               DefaultTimeoutAsync, Priority, PatternPid);

send_async(#cloudi_context{priority_default = PriorityDefault} = Context,
           Name, RequestInfo, Request,
           Timeout, undefined, PatternPid) ->
    send_async(Context, Name, RequestInfo, Request,
               Timeout, PriorityDefault, PatternPid);

send_async(#cloudi_context{dest_refresh = DestRefresh,
                           scope = Scope,
                           cpg_data = Groups} = Context,
           Name, RequestInfo, Request,
           Timeout, Priority, undefined)
    when is_list(Name), is_integer(Timeout),
         Timeout >= 0 ->
    case destination_get(DestRefresh, Scope, Name, Groups,
                         Timeout + ?TIMEOUT_DELTA) of
        {error, {Reason, Name}}
        when (Reason =:= no_process orelse Reason =:= no_such_group),
             Timeout >= ?SEND_ASYNC_INTERVAL ->
            receive after ?SEND_ASYNC_INTERVAL -> ok end,
            send_async(Context, Name, RequestInfo, Request,
                       Timeout - ?SEND_ASYNC_INTERVAL,
                       Priority, undefined);
        {error, _} = Error ->
            Error;
        {ok, Pattern, Pid} ->
            send_async(Context, Name, RequestInfo, Request,
                       Timeout, Priority, {Pattern, Pid})
    end;

send_async(#cloudi_context{receiver = Receiver,
                           uuid_generator = UUID},
           Name, RequestInfo, Request,
           Timeout, Priority, {Pattern, Pid})
    when is_list(Name), is_integer(Timeout),
         Timeout >= 0, is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    TransId = cloudi_x_uuid:get_v1(UUID),
    Pid ! {'cloudi_service_send_async',
           Name, Pattern, RequestInfo, Request,
           Timeout, Priority, TransId, Receiver},
    {ok, TransId}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% An alias for send_async.  The asynchronous service request is returned
%% and handled the same way as within external services.
%% @end
%%-------------------------------------------------------------------------

-spec send_async_passive(Context :: context() | cloudi_service:dispatcher(),
                         Name :: service_name(),
                         Request :: request()) ->
    {'ok', TransId :: trans_id()} |
    {'error', Reason :: any()}.

send_async_passive(Context, Name, Request) ->
    send_async(Context, Name, Request).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% An alias for send_async.  The asynchronous service request is returned
%% and handled the same way as within external services.
%% @end
%%-------------------------------------------------------------------------

-spec send_async_passive(Context :: context() | cloudi_service:dispatcher(),
                         Name :: service_name(),
                         Request :: request(),
                         Timeout :: timeout_milliseconds() | 'undefined') ->
    {'ok', TransId :: trans_id()} |
    {'error', Reason :: any()}.

send_async_passive(Context, Name, Request, Timeout) ->
    send_async(Context, Name, Request, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% An alias for send_async.  The asynchronous service request is returned
%% and handled the same way as within external services.
%% @end
%%-------------------------------------------------------------------------

-spec send_async_passive(Context :: context() | cloudi_service:dispatcher(),
                         Name :: service_name(),
                         Request :: request(),
                         Timeout :: timeout_milliseconds() | 'undefined',
                         PatternPid :: pattern_pid()) ->
    {'ok', TransId :: trans_id()} |
    {'error', Reason :: any()}.

send_async_passive(Context, Name, Request, Timeout, PatternPid) ->
    send_async(Context, Name, Request, Timeout, PatternPid).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% An alias for send_async.  The asynchronous service request is returned
%% and handled the same way as within external services.
%% @end
%%-------------------------------------------------------------------------

-spec send_async_passive(Context :: context() | cloudi_service:dispatcher(),
                         Name :: service_name(),
                         RequestInfo :: request_info(),
                         Request :: request(),
                         Timeout :: timeout_milliseconds() | 'undefined',
                         Priority :: priority() | 'undefined') ->
    {'ok', TransId :: trans_id()} |
    {'error', Reason :: any()}.

send_async_passive(Context, Name, RequestInfo, Request,
                   Timeout, Priority) ->
    send_async(Context, Name, RequestInfo, Request,
               Timeout, Priority).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% An alias for send_async.  The asynchronous service request is returned
%% and handled the same way as within external services.
%% @end
%%-------------------------------------------------------------------------

-spec send_async_passive(Context :: context() | cloudi_service:dispatcher(),
                         Name :: service_name(),
                         RequestInfo :: request_info(),
                         Request :: request(),
                         Timeout :: timeout_milliseconds() | 'undefined',
                         Priority :: priority() | 'undefined',
                         PatternPid :: pattern_pid()) ->
    {'ok', TransId :: trans_id()} |
    {'error', Reason :: any()}.

send_async_passive(Context, Name, RequestInfo, Request,
                   Timeout, Priority, PatternPid) ->
    send_async(Context, Name, RequestInfo, Request,
               Timeout, Priority, PatternPid).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a synchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_sync(Context :: context() | cloudi_service:dispatcher(),
                Name :: service_name(),
                Request :: request()) ->
    {'ok', ResponseInfo :: response_info(), Response :: response()} |
    {'ok', Response :: response()} |
    {'error', Reason :: any()}.

send_sync(Dispatcher, Name, Request)
    when is_pid(Dispatcher) ->
    cloudi_service:send_sync(Dispatcher, Name, Request);

send_sync(Context, Name, Request) ->
    send_sync(Context, Name, <<>>, Request,
              undefined, undefined, undefined).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a synchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_sync(Context :: context() | cloudi_service:dispatcher(),
                Name :: service_name(),
                Request :: request(),
                Timeout :: timeout_milliseconds() | 'undefined') ->
    {'ok', ResponseInfo :: response_info(), Response :: response()} |
    {'ok', Response :: response()} |
    {'error', Reason :: any()}.

send_sync(Dispatcher, Name, Request, Timeout)
    when is_pid(Dispatcher) ->
    cloudi_service:send_sync(Dispatcher, Name, Request, Timeout);

send_sync(Context, Name, Request, Timeout) ->
    send_sync(Context, Name, <<>>, Request,
              Timeout, undefined, undefined).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a synchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_sync(Context :: context() | cloudi_service:dispatcher(),
                Name :: service_name(),
                Request :: request(),
                Timeout :: timeout_milliseconds() | 'undefined',
                PatternPid :: pattern_pid()) ->
    {'ok', ResponseInfo :: response_info(), Response :: response()} |
    {'ok', Response :: response()} |
    {'error', Reason :: any()}.

send_sync(Dispatcher, Name, Request, Timeout, PatternPid)
    when is_pid(Dispatcher) ->
    cloudi_service:send_sync(Dispatcher, Name, Request, Timeout, PatternPid);

send_sync(Context, Name, Request, Timeout, PatternPid) ->
    send_sync(Context, Name, <<>>, Request,
              Timeout, undefined, PatternPid).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a synchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_sync(Context :: context() | cloudi_service:dispatcher(),
                Name :: service_name(),
                RequestInfo :: request_info(),
                Request :: request(),
                Timeout :: timeout_milliseconds() | 'undefined',
                Priority :: priority() | 'undefined') ->
    {'ok', ResponseInfo :: response_info(), Response :: response()} |
    {'ok', Response :: response()} |
    {'error', Reason :: any()}.

send_sync(Dispatcher, Name, RequestInfo, Request,
          Timeout, Priority)
    when is_pid(Dispatcher) ->
    cloudi_service:send_sync(Dispatcher, Name, RequestInfo, Request,
                             Timeout, Priority);

send_sync(Context, Name, RequestInfo, Request,
          Timeout, Priority) ->
    send_sync(Context, Name, RequestInfo, Request,
              Timeout, Priority, undefined).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a synchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_sync(Context :: context() | cloudi_service:dispatcher(),
                Name :: service_name(),
                RequestInfo :: request_info(),
                Request :: request(),
                Timeout :: timeout_milliseconds() | 'undefined',
                Priority :: priority() | 'undefined',
                PatternPid :: pattern_pid() | 'undefined') ->
    {'ok', ResponseInfo :: response_info(), Response :: response()} |
    {'ok', Response :: response()} |
    {'error', Reason :: any()}.

send_sync(Dispatcher, Name, RequestInfo, Request,
          Timeout, Priority, PatternPid)
    when is_pid(Dispatcher) ->
    cloudi_service:send_sync(Dispatcher, Name, RequestInfo, Request,
                             Timeout, Priority, PatternPid);

send_sync(#cloudi_context{timeout_sync = DefaultTimeoutSync} = Context,
          Name, RequestInfo, Request,
          undefined, Priority, PatternPid) ->
    send_sync(Context, Name, RequestInfo, Request,
              DefaultTimeoutSync, Priority, PatternPid);

send_sync(#cloudi_context{priority_default = PriorityDefault} = Context,
          Name, RequestInfo, Request,
          Timeout, undefined, PatternPid) ->
    send_sync(Context, Name, RequestInfo, Request,
              Timeout, PriorityDefault, PatternPid);

send_sync(#cloudi_context{dest_refresh = DestRefresh,
                          scope = Scope,
                          cpg_data = Groups} = Context,
          Name, RequestInfo, Request,
          Timeout, Priority, undefined)
    when is_list(Name), is_integer(Timeout),
         Timeout >= 0 ->
    case destination_get(DestRefresh, Scope, Name, Groups,
                         Timeout + ?TIMEOUT_DELTA) of
        {error, {Reason, Name}}
        when (Reason =:= no_process orelse Reason =:= no_such_group),
             Timeout >= ?SEND_SYNC_INTERVAL ->
            receive after ?SEND_SYNC_INTERVAL -> ok end,
            send_sync(Context, Name, RequestInfo, Request,
                      Timeout - ?SEND_SYNC_INTERVAL,
                      Priority, undefined);
        {error, _} = Error ->
            Error;
        {ok, Pattern, Pid} ->
            send_sync(Context, Name, RequestInfo, Request,
                      Timeout, Priority, {Pattern, Pid})
    end;

send_sync(#cloudi_context{receiver = Receiver,
                          uuid_generator = UUID},
          Name, RequestInfo, Request,
          Timeout, Priority, {Pattern, Pid})
    when is_list(Name), is_integer(Timeout),
         Timeout >= 0, is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    if
        self() /= Receiver ->
            ?LOG_ERROR("send_sync called outside of context", []),
            erlang:exit(badarg);
        true ->
            ok
    end,
    TransId = cloudi_x_uuid:get_v1(UUID),
    Pid ! {'cloudi_service_send_sync',
           Name, Pattern, RequestInfo, Request,
           Timeout, Priority, TransId, Receiver},
    receive
        {'cloudi_service_return_sync',
         _, _, <<>>, <<>>, _, TransId, Receiver} ->
            {error, timeout};
        {'cloudi_service_return_sync',
         _, _, <<>>, Response, _, TransId, Receiver} ->
            {ok, Response};
        {'cloudi_service_return_sync',
         _, _, ResponseInfo, Response, _, TransId, Receiver} ->
            {ok, ResponseInfo, Response}
    after
        Timeout ->
            {error, timeout}
    end.
    

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a multicast asynchronous service request.===
%% Asynchronous service requests are sent to all services that have
%% subscribed to the service name pattern that matches the destination.
%% @end
%%-------------------------------------------------------------------------

-spec mcast_async(Context :: context() | cloudi_service:dispatcher(),
                  Name :: service_name(),
                  Request :: request()) ->
    {'ok', TransIdList :: list(trans_id())} |
    {'error', Reason :: any()}.

mcast_async(Dispatcher, Name, Request)
    when is_pid(Dispatcher) ->
    cloudi_service:mcast_async(Dispatcher, Name, Request);

mcast_async(Context, Name, Request) ->
    mcast_async(Context, Name, <<>>, Request,
                undefined, undefined).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a multicast asynchronous service request.===
%% Asynchronous service requests are sent to all services that have
%% subscribed to the service name pattern that matches the destination.
%% @end
%%-------------------------------------------------------------------------

-spec mcast_async(Context :: context() | cloudi_service:dispatcher(),
                  Name :: service_name(),
                  Request :: request(),
                  Timeout :: timeout_milliseconds() | 'undefined') ->
    {'ok', TransIdList :: list(trans_id())} |
    {'error', Reason :: any()}.

mcast_async(Dispatcher, Name, Request, Timeout)
    when is_pid(Dispatcher) ->
    cloudi_service:mcast_async(Dispatcher, Name, Request, Timeout);

mcast_async(Context, Name, Request, Timeout) ->
    mcast_async(Context, Name, <<>>, Request,
                Timeout, undefined).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a multicast asynchronous service request.===
%% Asynchronous service requests are sent to all services that have
%% subscribed to the service name pattern that matches the destination.
%% @end
%%-------------------------------------------------------------------------

-spec mcast_async(Context :: context() | cloudi_service:dispatcher(),
                  Name :: service_name(),
                  RequestInfo :: request_info(),
                  Request :: request(),
                  Timeout :: timeout_milliseconds() | 'undefined',
                  Priority :: priority() | 'undefined') ->
    {'ok', TransIdList :: list(trans_id())} |
    {'error', Reason :: any()}.

mcast_async(Dispatcher, Name, RequestInfo, Request,
            Timeout, Priority)
    when is_pid(Dispatcher) ->
    cloudi_service:mcast_async(Dispatcher, Name, RequestInfo, Request,
                               Timeout, Priority);

mcast_async(#cloudi_context{timeout_async = DefaultTimeoutAsync} = Context,
            Name, RequestInfo, Request, undefined, Priority) ->
    mcast_async(Context, Name, RequestInfo, Request,
                DefaultTimeoutAsync, Priority);

mcast_async(#cloudi_context{priority_default = PriorityDefault} = Context,
            Name, RequestInfo, Request, Timeout, undefined) ->
    mcast_async(Context, Name, RequestInfo, Request,
                Timeout, PriorityDefault);

mcast_async(#cloudi_context{dest_refresh = DestRefresh,
                            scope = Scope,
                            receiver = Receiver,
                            uuid_generator = UUID,
                            cpg_data = Groups} = Context,
            Name, RequestInfo, Request, Timeout, Priority)
    when is_list(Name), is_integer(Timeout),
         Timeout >= 0, is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    case destination_all(DestRefresh, Scope, Name, Groups,
                         Timeout + ?TIMEOUT_DELTA) of
        {error, {no_such_group, Name}}
        when Timeout >= ?MCAST_ASYNC_INTERVAL ->
            receive after ?MCAST_ASYNC_INTERVAL -> ok end,
            mcast_async(Context, Name, RequestInfo, Request,
                        Timeout - ?MCAST_ASYNC_INTERVAL,
                        Priority);
        {error, _} = Error ->
            Error;
        {ok, Pattern, PidList} ->
            TransIdList = lists:map(fun(Pid) ->
                TransId = cloudi_x_uuid:get_v1(UUID),
                Pid ! {'cloudi_service_send_async',
                       Name, Pattern, RequestInfo, Request,
                       Timeout, Priority, TransId, Receiver},
                TransId
            end, PidList),
            {ok, TransIdList}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a multicast asynchronous service request.===
%% An alias for mcast_async.  The asynchronous service requests are returned
%% and handled the same way as within external services.
%% @end
%%-------------------------------------------------------------------------

-spec mcast_async_passive(Context :: context() | cloudi_service:dispatcher(),
                          Name :: service_name(),
                          Request :: request()) ->
    {'ok', TransIdList :: list(trans_id())} |
    {'error', Reason :: any()}.

mcast_async_passive(Context, Name, Request) ->
    mcast_async(Context, Name, Request).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a multicast asynchronous service request.===
%% An alias for mcast_async.  The asynchronous service requests are returned
%% and handled the same way as within external services.
%% @end
%%-------------------------------------------------------------------------

-spec mcast_async_passive(Context :: context() | cloudi_service:dispatcher(),
                          Name :: service_name(),
                          Request :: request(),
                          Timeout :: timeout_milliseconds() | 'undefined') ->
    {'ok', TransIdList :: list(trans_id())} |
    {'error', Reason :: any()}.

mcast_async_passive(Context, Name, Request, Timeout) ->
    mcast_async(Context, Name, Request, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a multicast asynchronous service request.===
%% An alias for mcast_async.  The asynchronous service requests are returned
%% and handled the same way as within external services.
%% @end
%%-------------------------------------------------------------------------

-spec mcast_async_passive(Context :: context() | cloudi_service:dispatcher(),
                          Name :: service_name(),
                          RequestInfo :: request_info(),
                          Request :: request(),
                          Timeout :: timeout_milliseconds() | 'undefined',
                          Priority :: priority() | 'undefined') ->
    {'ok', TransIdList :: list(trans_id())} |
    {'error', Reason :: any()}.

mcast_async_passive(Context, Name, RequestInfo, Request,
                    Timeout, Priority) ->
    mcast_async(Context, Name, RequestInfo, Request,
                Timeout, Priority).

%%-------------------------------------------------------------------------
%% @doc
%% ===Receive an asynchronous service request.===
%% Use a null TransId to receive the oldest service request.  Consume is
%% implicitly true.
%% @end
%%-------------------------------------------------------------------------

-spec recv_async(Context :: context() | cloudi_service:dispatcher()) ->
    {'ok', ResponseInfo :: response_info(), Response :: response(),
     TransId :: trans_id()} |
    {'error', Reason :: atom()}.

recv_async(Dispatcher)
    when is_pid(Dispatcher) ->
    cloudi_service:recv_async(Dispatcher);

recv_async(Context) ->
    recv_async(Context, undefined, <<0:128>>).

%%-------------------------------------------------------------------------
%% @doc
%% ===Receive an asynchronous service request.===
%% Either use the supplied TransId to receive the specific service request
%% or use a null TransId to receive the oldest service request.  Consume is
%% implicitly true.
%% @end
%%-------------------------------------------------------------------------

-spec recv_async(Context :: context() | cloudi_service:dispatcher(),
                 trans_id() | timeout_milliseconds()) ->
    {'ok', ResponseInfo :: response_info(), Response :: response(),
     TransId :: trans_id()} |
    {'error', Reason :: atom()}.

recv_async(Dispatcher, TransId_Timeout)
    when is_pid(Dispatcher) ->
    cloudi_service:recv_async(Dispatcher, TransId_Timeout);

recv_async(Context, TransId)
    when is_binary(TransId) ->
    recv_async(Context, undefined, TransId);

recv_async(Context, Timeout)
    when is_integer(Timeout), Timeout >= 0 ->
    recv_async(Context, Timeout, <<0:128>>).

%%-------------------------------------------------------------------------
%% @doc
%% ===Receive an asynchronous service request.===
%% Either use the supplied TransId to receive the specific service request
%% or use a null TransId to receive the oldest service request.  Consume is
%% implicitly true.
%% @end
%%-------------------------------------------------------------------------

-spec recv_async(Context :: context() | cloudi_service:dispatcher(),
                 Timeout :: timeout_milliseconds() | 'undefined',
                 TransId :: trans_id()) ->
    {'ok', ResponseInfo :: response_info(), Response :: response(),
     TransId :: trans_id()} |
    {'error', Reason :: atom()}.

recv_async(Dispatcher, Timeout, TransId)
    when is_pid(Dispatcher) ->
    cloudi_service:recv_async(Dispatcher, Timeout, TransId);

recv_async(#cloudi_context{timeout_async = DefaultTimeoutAsync} = Context,
           undefined, TransId) ->
    recv_async(Context, DefaultTimeoutAsync, TransId);

recv_async(#cloudi_context{receiver = Receiver},
           Timeout, <<0:128>>)
    when is_integer(Timeout), Timeout >= 0 ->
    if
        self() /= Receiver ->
            ?LOG_ERROR("recv_async called outside of context", []),
            erlang:exit(badarg);
        true ->
            ok
    end,
    receive
        {'cloudi_service_return_async',
         _, _, _, <<>>, _, _, Receiver} ->
            {error, timeout};
        {'cloudi_service_return_async',
         _, _, ResponseInfo, Response, _, TransId, Receiver} ->
            {ok, ResponseInfo, Response, TransId}
    after
        Timeout ->
            {error, timeout}
    end;

recv_async(#cloudi_context{receiver = Receiver},
           Timeout, TransId)
    when is_integer(Timeout), is_binary(TransId), Timeout >= 0 ->
    if
        self() /= Receiver ->
            ?LOG_ERROR("recv_async called outside of context", []),
            erlang:exit(badarg);
        true ->
            ok
    end,
    receive
        {'cloudi_service_return_async',
         _, _, _, <<>>, _, TransId, Receiver} ->
            {error, timeout};
        {'cloudi_service_return_async',
         _, _, ResponseInfo, Response, _, TransId, Receiver} ->
            {ok, ResponseInfo, Response, TransId}
    after
        Timeout ->
            {error, timeout}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Configured service default asynchronous timeout (in milliseconds).===
%% @end
%%-------------------------------------------------------------------------

-spec timeout_async(Context :: #cloudi_context{}) ->
    TimeoutAsync :: cloudi_service_api:timeout_milliseconds().

timeout_async(#cloudi_context{timeout_async = DefaultTimeoutAsync}) ->
    DefaultTimeoutAsync.

%%-------------------------------------------------------------------------
%% @doc
%% ===Configured service default synchronous timeout (in milliseconds).===
%% @end
%%-------------------------------------------------------------------------

-spec timeout_sync(Context :: #cloudi_context{}) ->
    TimeoutSync :: cloudi_service_api:timeout_milliseconds().

timeout_sync(#cloudi_context{timeout_sync = DefaultTimeoutSync}) ->
    DefaultTimeoutSync.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

destination_refresh_first(DestRefresh, Delay, Scope)
    when (DestRefresh =:= lazy_closest orelse
          DestRefresh =:= lazy_furthest orelse
          DestRefresh =:= lazy_random orelse
          DestRefresh =:= lazy_local orelse
          DestRefresh =:= lazy_remote orelse
          DestRefresh =:= lazy_newest orelse
          DestRefresh =:= lazy_oldest) ->
    cloudi_x_cpg_data:get_groups(Scope, Delay);

destination_refresh_first(DestRefresh, _, _)
    when (DestRefresh =:= immediate_closest orelse
          DestRefresh =:= immediate_furthest orelse
          DestRefresh =:= immediate_random orelse
          DestRefresh =:= immediate_local orelse
          DestRefresh =:= immediate_remote orelse
          DestRefresh =:= immediate_newest orelse
          DestRefresh =:= immediate_oldest) ->
    ok.

destination_refresh_start(DestRefresh, Delay, Scope)
    when (DestRefresh =:= lazy_closest orelse
          DestRefresh =:= lazy_furthest orelse
          DestRefresh =:= lazy_random orelse
          DestRefresh =:= lazy_local orelse
          DestRefresh =:= lazy_remote orelse
          DestRefresh =:= lazy_newest orelse
          DestRefresh =:= lazy_oldest) ->
    cloudi_x_cpg_data:get_groups(Scope, Delay);

destination_refresh_start(DestRefresh, _, _)
    when (DestRefresh =:= immediate_closest orelse
          DestRefresh =:= immediate_furthest orelse
          DestRefresh =:= immediate_random orelse
          DestRefresh =:= immediate_local orelse
          DestRefresh =:= immediate_remote orelse
          DestRefresh =:= immediate_newest orelse
          DestRefresh =:= immediate_oldest) ->
    ok.

-define(CATCH_EXIT(F),
        try F catch exit:{Reason, _} -> {error, Reason} end).

destination_get(lazy_closest, _, Name, Groups, _)
    when is_list(Name) ->
    cloudi_x_cpg_data:get_closest_pid(Name, Groups);

destination_get(lazy_furthest, _, Name, Groups, _)
    when is_list(Name) ->
    cloudi_x_cpg_data:get_furthest_pid(Name, Groups);

destination_get(lazy_random, _, Name, Groups, _)
    when is_list(Name) ->
    cloudi_x_cpg_data:get_random_pid(Name, Groups);

destination_get(lazy_local, _, Name, Groups, _)
    when is_list(Name) ->
    cloudi_x_cpg_data:get_local_pid(Name, Groups);

destination_get(lazy_remote, _, Name, Groups, _)
    when is_list(Name) ->
    cloudi_x_cpg_data:get_remote_pid(Name, Groups);

destination_get(lazy_newest, _, Name, Groups, _)
    when is_list(Name) ->
    cloudi_x_cpg_data:get_newest_pid(Name, Groups);

destination_get(lazy_oldest, _, Name, Groups, _)
    when is_list(Name) ->
    cloudi_x_cpg_data:get_oldest_pid(Name, Groups);

destination_get(immediate_closest, Scope, Name, _, Timeout)
    when is_list(Name) ->
    ?CATCH_EXIT(cloudi_x_cpg:get_closest_pid(Scope, Name, Timeout));

destination_get(immediate_furthest, Scope, Name, _, Timeout)
    when is_list(Name) ->
    ?CATCH_EXIT(cloudi_x_cpg:get_furthest_pid(Scope, Name, Timeout));

destination_get(immediate_random, Scope, Name, _, Timeout)
    when is_list(Name) ->
    ?CATCH_EXIT(cloudi_x_cpg:get_random_pid(Scope, Name, Timeout));

destination_get(immediate_local, Scope, Name, _, Timeout)
    when is_list(Name) ->
    ?CATCH_EXIT(cloudi_x_cpg:get_local_pid(Scope, Name, Timeout));

destination_get(immediate_remote, Scope, Name, _, Timeout)
    when is_list(Name) ->
    ?CATCH_EXIT(cloudi_x_cpg:get_remote_pid(Scope, Name, Timeout));

destination_get(immediate_newest, Scope, Name, _, Timeout)
    when is_list(Name) ->
    ?CATCH_EXIT(cloudi_x_cpg:get_newest_pid(Scope, Name, Timeout));

destination_get(immediate_oldest, Scope, Name, _, Timeout)
    when is_list(Name) ->
    ?CATCH_EXIT(cloudi_x_cpg:get_oldest_pid(Scope, Name, Timeout));

destination_get(DestRefresh, _, _, _, _) ->
    ?LOG_ERROR("unable to send with invalid destination refresh: ~p",
               [DestRefresh]),
    erlang:exit(badarg).

destination_all(DestRefresh, _, Name, Groups, _)
    when is_list(Name),
         (DestRefresh =:= lazy_closest orelse
          DestRefresh =:= lazy_furthest orelse
          DestRefresh =:= lazy_random orelse
          DestRefresh =:= lazy_newest orelse
          DestRefresh =:= lazy_oldest) ->
    cloudi_x_cpg_data:get_members(Name, Groups);

destination_all(lazy_local, _, Name, Groups, _)
    when is_list(Name) ->
    cloudi_x_cpg_data:get_local_members(Name, Groups);

destination_all(lazy_remote, _, Name, Groups, _)
    when is_list(Name) ->
    cloudi_x_cpg_data:get_remote_members(Name, Groups);

destination_all(DestRefresh, Scope, Name, _, Timeout)
    when (DestRefresh =:= immediate_closest orelse
          DestRefresh =:= immediate_furthest orelse
          DestRefresh =:= immediate_random orelse
          DestRefresh =:= immediate_newest orelse
          DestRefresh =:= immediate_oldest),
         is_list(Name) ->
    ?CATCH_EXIT(cloudi_x_cpg:get_members(Scope, Name, Timeout));

destination_all(immediate_local, Scope, Name, _, Timeout)
    when is_list(Name) ->
    ?CATCH_EXIT(cloudi_x_cpg:get_local_members(Scope, Name, Timeout));

destination_all(immediate_remote, Scope, Name, _, Timeout)
    when is_list(Name) ->
    ?CATCH_EXIT(cloudi_x_cpg:get_remote_members(Scope, Name, Timeout));

destination_all(DestRefresh, _, _, _, _) ->
    ?LOG_ERROR("unable to send with invalid destination refresh: ~p",
               [DestRefresh]),
    erlang:exit(badarg).

