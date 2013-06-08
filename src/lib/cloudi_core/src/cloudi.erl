%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
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
%%% @version 1.2.2 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([new/0,
         new/1,
         get_pid/2,
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
         recv_async/1,
         recv_async/2,
         recv_async/3,
         timeout_async/1,
         timeout_sync/1]).

-include("cloudi_logger.hrl").
-include("cloudi_constants.hrl").

-define(DEFAULT_PRIORITY,                           0).
-define(DEFAULT_DEST_REFRESH,       immediate_closest).
-define(DEFAULT_TIMEOUT_ASYNC,                   5000). % milliseconds
-define(DEFAULT_TIMEOUT_SYNC,                    5000). % milliseconds

-record(cloudi_context,
        {
            dest_refresh,
            timeout_async,
            timeout_sync,
            priority_default,
            receiver,
            uuid_generator
        }).

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
%% @end
%%-------------------------------------------------------------------------
-spec new(Settings :: list({atom(), any()})) ->
    #cloudi_context{}.

new(Settings)
    when is_list(Settings) ->
    Defaults = [
        {dest_refresh,          ?DEFAULT_DEST_REFRESH},
        {timeout_async,        ?DEFAULT_TIMEOUT_ASYNC},
        {timeout_sync,          ?DEFAULT_TIMEOUT_SYNC},
        {priority_default,          ?DEFAULT_PRIORITY}
        ],
    [DestRefresh, DefaultTimeoutAsync, DefaultTimeoutSync,
     PriorityDefault] =
        cloudi_proplists:take_values(Defaults, Settings),
    % the distinction between "immediate" and "lazy" is ignored
    % (all usage of the cloudi module is immediate)
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
           (DestRefresh =:= lazy_oldest) orelse
           (DestRefresh =:= none),
    true = is_integer(DefaultTimeoutAsync) and
           (DefaultTimeoutAsync > 0),
    true = is_integer(DefaultTimeoutSync) and
           (DefaultTimeoutSync > 0),
    true = (PriorityDefault >= ?PRIORITY_HIGH) and
           (PriorityDefault =< ?PRIORITY_LOW),
    Self = self(),
    #cloudi_context{
        dest_refresh = DestRefresh,
        timeout_async = DefaultTimeoutAsync,
        timeout_sync = DefaultTimeoutSync,
        priority_default = PriorityDefault,
        receiver = Self,
        uuid_generator = cloudi_x_uuid:new(Self)
    }.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a service destination based on a service name.===
%% @end
%%-------------------------------------------------------------------------

-spec get_pid(Context :: #cloudi_context{},
              Name :: string()) ->
    {'ok', PatternPid :: {string(), pid()}} |
    {'error', Reason :: any()}.

get_pid(#cloudi_context{dest_refresh = DestRefresh}, Name)
    when is_list(Name) ->
    case destination_get(DestRefresh, Name) of
        {error, _} = Error ->
            Error;
        {ok, Pattern, Pid} ->
            {ok, {Pattern, Pid}}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_async(Context :: #cloudi_context{},
                 Name :: string(),
                 Request :: any()) ->
    {'ok', TransId :: <<_:128>>} |
    {'error', Reason :: any()}.

send_async(Context, Name, Request) ->
    send_async(Context, Name, <<>>, Request,
               undefined, undefined, undefined).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_async(Context :: #cloudi_context{},
                 Name :: string(),
                 Request :: any(),
                 Timeout :: non_neg_integer() | 'undefined') ->
    {'ok', TransId :: <<_:128>>} |
    {'error', Reason :: any()}.

send_async(Context, Name, Request, Timeout) ->
    send_async(Context, Name, <<>>, Request,
               Timeout, undefined, undefined).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_async(Context :: #cloudi_context{},
                 Name :: string(),
                 Request :: any(),
                 Timeout :: non_neg_integer() | 'undefined',
                 PatternPid :: {string(), pid()}) ->
    {'ok', TransId :: <<_:128>>} |
    {'error', Reason :: any()}.

send_async(Context, Name, Request, Timeout, PatternPid) ->
    send_async(Context, Name, <<>>, Request,
               Timeout, undefined, PatternPid).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_async(Context :: #cloudi_context{},
                 Name :: string(),
                 RequestInfo :: any(),
                 Request :: any(),
                 Timeout :: non_neg_integer() | 'undefined',
                 Priority :: integer() | 'undefined') ->
    {'ok', TransId :: <<_:128>>} |
    {'error', Reason :: any()}.

send_async(Context, Name, RequestInfo, Request,
           Timeout, Priority) ->
    send_async(Context, Name, RequestInfo, Request,
               Timeout, Priority, undefined).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_async(Context :: #cloudi_context{},
                 Name :: string(),
                 RequestInfo :: any(),
                 Request :: any(),
                 Timeout :: non_neg_integer() | 'undefined',
                 Priority :: integer() | 'undefined',
                 PatternPid :: {string(), pid()} | 'undefined') ->
    {'ok', TransId :: <<_:128>>} |
    {'error', Reason :: any()}.

send_async(#cloudi_context{dest_refresh = DestRefresh} = Context,
           Name, RequestInfo, Request,
           Timeout, Priority, undefined)
    when is_list(Name) ->
    case destination_get(DestRefresh, Name) of
        {error, _} = Error ->
            Error;
        {ok, Pattern, Pid} ->
            send_async(Context, Name, RequestInfo, Request,
                       Timeout, Priority, {Pattern, Pid})
    end;

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

-spec send_async_passive(Context :: #cloudi_context{},
                         Name :: string(),
                         Request :: any()) ->
    {'ok', TransId :: <<_:128>>} |
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

-spec send_async_passive(Context :: #cloudi_context{},
                         Name :: string(),
                         Request :: any(),
                         Timeout :: non_neg_integer() | 'undefined') ->
    {'ok', TransId :: <<_:128>>} |
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

-spec send_async_passive(Context :: #cloudi_context{},
                         Name :: string(),
                         Request :: any(),
                         Timeout :: non_neg_integer() | 'undefined',
                         PatternPid :: {string(), pid()}) ->
    {'ok', TransId :: <<_:128>>} |
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

-spec send_async_passive(Context :: #cloudi_context{},
                         Name :: string(),
                         RequestInfo :: any(),
                         Request :: any(),
                         Timeout :: non_neg_integer() | 'undefined',
                         Priority :: integer() | 'undefined') ->
    {'ok', TransId :: <<_:128>>} |
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

-spec send_async_passive(Context :: #cloudi_context{},
                         Name :: string(),
                         RequestInfo :: any(),
                         Request :: any(),
                         Timeout :: non_neg_integer() | 'undefined',
                         Priority :: integer() | 'undefined',
                         PatternPid :: {string(), pid()}) ->
    {'ok', TransId :: <<_:128>>} |
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

-spec send_sync(Context :: #cloudi_context{},
                Name :: string(),
                Request :: any()) ->
    {'ok', ResponseInfo :: any(), Response :: any()} |
    {'ok', Response :: any()} |
    {'error', Reason :: any()}.

send_sync(Context, Name, Request) ->
    send_sync(Context, Name, <<>>, Request,
              undefined, undefined, undefined).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a synchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_sync(Context :: #cloudi_context{},
                Name :: string(),
                Request :: any(),
                Timeout :: non_neg_integer() | 'undefined') ->
    {'ok', ResponseInfo :: any(), Response :: any()} |
    {'ok', Response :: any()} |
    {'error', Reason :: any()}.

send_sync(Context, Name, Request, Timeout) ->
    send_sync(Context, Name, <<>>, Request,
              Timeout, undefined, undefined).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a synchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_sync(Context :: #cloudi_context{},
                Name :: string(),
                Request :: any(),
                Timeout :: non_neg_integer() | 'undefined',
                PatternPid :: {string(), pid()}) ->
    {'ok', ResponseInfo :: any(), Response :: any()} |
    {'ok', Response :: any()} |
    {'error', Reason :: any()}.

send_sync(Context, Name, Request, Timeout, PatternPid) ->
    send_sync(Context, Name, <<>>, Request,
              Timeout, undefined, PatternPid).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a synchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_sync(Context :: #cloudi_context{},
                Name :: string(),
                RequestInfo :: any(),
                Request :: any(),
                Timeout :: non_neg_integer() | 'undefined',
                Priority :: integer() | 'undefined') ->
    {'ok', ResponseInfo :: any(), Response :: any()} |
    {'ok', Response :: any()} |
    {'error', Reason :: any()}.

send_sync(Context, Name, RequestInfo, Request,
          Timeout, Priority) ->
    send_sync(Context, Name, RequestInfo, Request,
              Timeout, Priority, undefined).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a synchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_sync(Context :: #cloudi_context{},
                Name :: string(),
                RequestInfo :: any(),
                Request :: any(),
                Timeout :: non_neg_integer() | 'undefined',
                Priority :: integer() | 'undefined',
                PatternPid :: {string(), pid()} | 'undefined') ->
    {'ok', ResponseInfo :: any(), Response :: any()} |
    {'ok', Response :: any()} |
    {'error', Reason :: any()}.

send_sync(#cloudi_context{dest_refresh = DestRefresh} = Context,
          Name, RequestInfo, Request,
          Timeout, Priority, undefined)
    when is_list(Name) ->
    case destination_get(DestRefresh, Name) of
        {error, _} = Error ->
            Error;
        {ok, Pattern, Pid} ->
            send_sync(Context, Name, RequestInfo, Request,
                      Timeout, Priority, {Pattern, Pid})
    end;

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

send_sync(#cloudi_context{receiver = Receiver,
                          uuid_generator = UUID},
          Name, RequestInfo, Request,
          Timeout, Priority, {Pattern, Pid})
    when is_list(Name), is_integer(Timeout),
         Timeout >= 0, is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    TransId = cloudi_x_uuid:get_v1(UUID),
    Pid ! {'cloudi_service_send_sync',
           Name, Pattern, RequestInfo, Request,
           Timeout, Priority, TransId, Receiver},
    receive
        {'cloudi_service_return_sync',
         _, _, _, <<>>, _, TransId, Receiver} ->
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

-spec mcast_async(Context :: #cloudi_context{},
                  Name :: string(),
                  Request :: any()) ->
    {'ok', TransIdList :: list(<<_:128>>)} |
    {'error', Reason :: any()}.

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

-spec mcast_async(Context :: #cloudi_context{},
                  Name :: string(),
                  Request :: any(),
                  Timeout :: non_neg_integer() | 'undefined') ->
    {'ok', TransIdList :: list(<<_:128>>)} |
    {'error', Reason :: any()}.

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

-spec mcast_async(Context :: #cloudi_context{},
                  Name :: string(),
                  RequestInfo :: any(),
                  Request :: any(),
                  Timeout :: non_neg_integer() | 'undefined',
                  Priority :: integer() | 'undefined') ->
    {'ok', TransIdList :: list(<<_:128>>)} |
    {'error', Reason :: any()}.

mcast_async(#cloudi_context{timeout_async = DefaultTimeoutAsync} = Context,
            Name, RequestInfo, Request, undefined, Priority) ->
    mcast_async(Context, Name, RequestInfo, Request,
                DefaultTimeoutAsync, Priority);

mcast_async(#cloudi_context{priority_default = PriorityDefault} = Context,
            Name, RequestInfo, Request, Timeout, undefined) ->
    mcast_async(Context, Name, RequestInfo, Request,
                Timeout, PriorityDefault);

mcast_async(#cloudi_context{dest_refresh = DestRefresh,
                            receiver = Receiver,
                            uuid_generator = UUID},
            Name, RequestInfo, Request, Timeout, Priority)
    when is_list(Name), is_integer(Timeout),
         Timeout >= 0, is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    case destination_all(DestRefresh, Name) of
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
%% ===Receive an asynchronous service request.===
%% Use a null TransId to receive the oldest service request.  Consume is
%% implicitly true.
%% @end
%%-------------------------------------------------------------------------

-spec recv_async(Context :: #cloudi_context{}) ->
    {'ok', ResponseInfo :: any(), Response :: any(), TransId :: <<_:128>>} |
    {'error', Reason :: atom()}.

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

-spec recv_async(Context :: #cloudi_context{},
                 non_neg_integer() | binary()) ->
    {'ok', ResponseInfo :: any(), Response :: any(), TransId :: <<_:128>>} |
    {'error', Reason :: atom()}.

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

-spec recv_async(Context :: #cloudi_context{},
                 Timeout :: non_neg_integer() | 'undefined',
                 TransId :: binary()) ->
    {'ok', ResponseInfo :: any(), Response :: any(), TransId :: <<_:128>>} |
    {'error', Reason :: atom()}.

recv_async(#cloudi_context{timeout_async = DefaultTimeoutAsync} = Context,
           undefined, TransId) ->
    recv_async(Context, DefaultTimeoutAsync, TransId);

recv_async(#cloudi_context{receiver = Receiver},
           Timeout, <<0:128>>)
    when is_integer(Timeout), Timeout >= 0 ->
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
    TimeoutAsync :: pos_integer().

timeout_async(#cloudi_context{timeout_async = DefaultTimeoutAsync}) ->
    DefaultTimeoutAsync.

%%-------------------------------------------------------------------------
%% @doc
%% ===Configured service default synchronous timeout (in milliseconds).===
%% @end
%%-------------------------------------------------------------------------

-spec timeout_sync(Context :: #cloudi_context{}) ->
    TimeoutSync :: pos_integer().

timeout_sync(#cloudi_context{timeout_sync = DefaultTimeoutSync}) ->
    DefaultTimeoutSync.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

destination_get(DestRefresh, Name)
    when is_list(Name),
         (DestRefresh =:= lazy_closest orelse
          DestRefresh =:= immediate_closest) ->
    cloudi_x_cpg:get_closest_pid(Name);

destination_get(DestRefresh, Name)
    when is_list(Name),
         (DestRefresh =:= lazy_furthest orelse
          DestRefresh =:= immediate_furthest) ->
    cloudi_x_cpg:get_furthest_pid(Name);

destination_get(DestRefresh, Name)
    when is_list(Name),
         (DestRefresh =:= lazy_random orelse
          DestRefresh =:= immediate_random) ->
    cloudi_x_cpg:get_random_pid(Name);

destination_get(DestRefresh, Name)
    when is_list(Name),
         (DestRefresh =:= lazy_local orelse
          DestRefresh =:= immediate_local) ->
    cloudi_x_cpg:get_local_pid(Name);

destination_get(DestRefresh, Name)
    when is_list(Name),
         (DestRefresh =:= lazy_remote orelse
          DestRefresh =:= immediate_remote) ->
    cloudi_x_cpg:get_remote_pid(Name);

destination_get(DestRefresh, Name)
    when is_list(Name),
         (DestRefresh =:= lazy_newest orelse
          DestRefresh =:= immediate_newest) ->
    cloudi_x_cpg:get_newest_pid(Name);

destination_get(DestRefresh, Name)
    when is_list(Name),
         (DestRefresh =:= lazy_oldest orelse
          DestRefresh =:= immediate_oldest) ->
    cloudi_x_cpg:get_oldest_pid(Name);

destination_get(DestRefresh, _) ->
    ?LOG_ERROR("unable to send with invalid destination refresh: ~p",
               [DestRefresh]),
    erlang:exit(badarg).

destination_all(DestRefresh, Name)
    when is_list(Name),
         (DestRefresh =:= lazy_closest orelse
          DestRefresh =:= immediate_closest orelse
          DestRefresh =:= lazy_furthest orelse
          DestRefresh =:= immediate_furthest orelse
          DestRefresh =:= lazy_random orelse
          DestRefresh =:= immediate_random orelse
          DestRefresh =:= lazy_newest orelse
          DestRefresh =:= immediate_newest orelse
          DestRefresh =:= lazy_oldest orelse
          DestRefresh =:= immediate_oldest) ->
    cloudi_x_cpg:get_members(Name);

destination_all(DestRefresh, Name)
    when is_list(Name),
         (DestRefresh =:= lazy_local orelse
          DestRefresh =:= immediate_local) ->
    cloudi_x_cpg:get_local_members(Name);

destination_all(DestRefresh, Name)
    when is_list(Name),
         (DestRefresh =:= lazy_remote orelse
          DestRefresh =:= immediate_remote) ->
    cloudi_x_cpg:get_remote_members(Name);

destination_all(DestRefresh, _) ->
    ?LOG_ERROR("unable to send with invalid destination refresh: ~p",
               [DestRefresh]),
    erlang:exit(badarg).
