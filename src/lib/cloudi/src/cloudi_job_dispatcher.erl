%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Job Dispatcher==
%%% Parent process for each cloudi_job behaviour process.
%%% The dispatcher handles all cloudi_job process outgoing requests to 
%%% prevent undesirable synchronous function call paths that are prone to
%%% deadlocks.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2011, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2011 Michael Truog
%%% @version 0.1.9 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_job_dispatcher).
-author('mjtruog [at] gmail (dot) com').

-behaviour(gen_server).

%% external interface
-export([start_link/9]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("cloudi_logger.hrl").
-include("cloudi_constants.hrl").

-record(state,
    {
        job,             % job pid
        prefix,          % subscribe/unsubscribe name prefix
        timeout_async,   % default timeout for send_async
        timeout_sync,    % default timeout for send_sync
        send_timeouts = dict:new(),    % tracking for timeouts
        async_responses = dict:new(),  % tracking for async messages
        uuid_generator,  % transaction id generator
        dest_refresh,    % immediate_closest |
                         % lazy_closest |
                         % immediate_random |
                         % lazy_random, destination pid refresh
        list_pg_data = list_pg_data:get_empty_groups(), % dest_refresh lazy
        dest_deny,       % is the socket denied from sending to a destination
        dest_allow       % is the socket allowed to send to a destination
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

start_link(Module, Args, Timeout, Prefix,
           TimeoutAsync, TimeoutSync, DestRefresh, DestDeny, DestAllow)
    when is_atom(Module), is_list(Args), is_integer(Timeout), is_list(Prefix),
         is_integer(TimeoutAsync), is_integer(TimeoutSync) ->
    true = (DestRefresh == immediate_closest) or
           (DestRefresh == lazy_closest) or
           (DestRefresh == immediate_random) or
           (DestRefresh == lazy_random) or
           (DestRefresh == none),
    gen_server:start_link(?MODULE, [Module, Args, Timeout, Prefix, TimeoutAsync,
                                    TimeoutSync, DestRefresh,
                                    DestDeny, DestAllow], []).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([Module, Args, Timeout, Prefix, TimeoutAsync, TimeoutSync,
      DestRefresh, DestDeny, DestAllow]) ->
    case cloudi_job:start_link(Module, Args, Prefix, Timeout) of
        {ok, Job} ->
            destination_refresh_first(DestRefresh),
            destination_refresh_start(DestRefresh),
            {ok, #state{job = Job,
                        prefix = Prefix,
                        timeout_async = TimeoutAsync,
                        timeout_sync = TimeoutSync,
                        uuid_generator = uuid:new(Job),
                        dest_refresh = DestRefresh,
                        dest_deny = DestDeny,
                        dest_allow = DestAllow}};
        ignore ->
            ignore;
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call(timeout_async, _, #state{timeout_async = TimeoutAsync} = State) ->
    {reply, TimeoutAsync, State};

handle_call(timeout_sync, _, #state{timeout_sync = TimeoutSync} = State) ->
    {reply, TimeoutSync, State};

handle_call({'get_pid', Name}, Client,
            #state{timeout_sync = TimeoutSync} = State) ->
    handle_call({'get_pid', Name, TimeoutSync}, Client, State);

handle_call({'get_pid', Name, Timeout}, {Exclude, _} = Client,
            #state{dest_refresh = DestRefresh,
                   list_pg_data = Groups,
                   dest_deny = DestDeny,
                   dest_allow = DestAllow} = State) ->
    case destination_allowed(Name, DestDeny, DestAllow) of
        true ->
            case destination_get(DestRefresh, Name, Exclude, Groups) of
                {error, _} when Timeout >= ?SEND_SYNC_INTERVAL ->
                    erlang:send_after(?SEND_SYNC_INTERVAL, self(),
                                      {'get_pid', Name,
                                       Timeout - ?SEND_SYNC_INTERVAL,
                                       Client}),
                    {noreply, State};
                {error, _} ->
                    {reply, {error, timeout}, State};
                Pid ->
                    {reply, {ok, Pid}, State}
            end;
        false ->
            {reply, {error, timeout}, State}
    end;

handle_call({'send_async', Name, RequestInfo, Request,
             undefined, Priority}, Client,
            #state{timeout_async = TimeoutAsync} = State) ->
    handle_call({'send_async', Name, RequestInfo, Request,
                 TimeoutAsync, Priority}, Client, State);

handle_call({'send_async', Name, RequestInfo, Request,
             Timeout, Priority}, Client,
            #state{dest_deny = DestDeny,
                   dest_allow = DestAllow} = State) ->
    case destination_allowed(Name, DestDeny, DestAllow) of
        true ->
            handle_send_async(Name, RequestInfo, Request,
                              Timeout, Priority, Client, State);
        false ->
            {reply, {error, timeout}, State}
    end;

handle_call({'send_async', Name, RequestInfo, Request,
             undefined, Priority, Pid}, Client,
            #state{timeout_async = TimeoutAsync} = State) ->
    handle_call({'send_async', Name, RequestInfo, Request,
                 TimeoutAsync, Priority, Pid}, Client, State);

handle_call({'send_async', Name, RequestInfo, Request,
             Timeout, Priority, Pid}, _,
            #state{uuid_generator = UUID} = State) ->
    TransId = uuid:get_v1(UUID),
    Pid ! {'send_async', Name, RequestInfo, Request,
           Timeout, Priority, TransId, self()},
    {reply, {ok, TransId}, send_async_timeout_start(Timeout, TransId, State)};

handle_call({'send_async_active', Name, RequestInfo, Request,
             undefined, Priority}, Client,
            #state{timeout_async = TimeoutAsync} = State) ->
    handle_call({'send_async_active', Name, RequestInfo, Request,
                 TimeoutAsync, Priority}, Client, State);

handle_call({'send_async_active', Name, RequestInfo, Request,
             Timeout, Priority}, Client,
            #state{dest_deny = DestDeny,
                   dest_allow = DestAllow} = State) ->
    case destination_allowed(Name, DestDeny, DestAllow) of
        true ->
            handle_send_async_active(Name, RequestInfo, Request,
                                     Timeout, Priority, Client, State);
        false ->
            {reply, {error, timeout}, State}
    end;

handle_call({'send_async_active', Name, RequestInfo, Request,
             undefined, Priority, Pid}, Client,
            #state{timeout_async = TimeoutAsync} = State) ->
    handle_call({'send_async_active', Name, RequestInfo, Request,
                 TimeoutAsync, Priority, Pid}, Client, State);

handle_call({'send_async_active', Name, RequestInfo, Request,
             Timeout, Priority, Pid}, _,
            #state{uuid_generator = UUID} = State) ->
    TransId = uuid:get_v1(UUID),
    Pid ! {'send_async', Name, RequestInfo, Request,
           Timeout, Priority, TransId, self()},
    {reply, {ok, TransId}, send_async_active_timeout_start(Timeout,
                                                           TransId, State)};

handle_call({'send_sync', Name, RequestInfo, Request,
             undefined, Priority}, Client,
            #state{timeout_sync = TimeoutSync} = State) ->
    handle_call({'send_sync', Name, RequestInfo, Request,
                 TimeoutSync, Priority}, Client, State);

handle_call({'send_sync', Name, RequestInfo, Request,
             Timeout, Priority}, Client,
            #state{dest_deny = DestDeny,
                   dest_allow = DestAllow} = State) ->
    case destination_allowed(Name, DestDeny, DestAllow) of
        true ->
            handle_send_sync(Name, RequestInfo, Request,
                             Timeout, Priority, Client, State);
        false ->
            {reply, {error, timeout}, State}
    end;

handle_call({'send_sync', Name, RequestInfo, Request,
             undefined, Priority, Pid}, Client,
            #state{timeout_sync = TimeoutSync} = State) ->
    handle_call({'send_sync', Name, RequestInfo, Request,
                 TimeoutSync, Priority, Pid}, Client, State);

handle_call({'send_sync', Name, RequestInfo, Request,
             Timeout, Priority, Pid}, _,
            #state{uuid_generator = UUID} = State) ->
    TransId = uuid:get_v1(UUID),
    Self = self(),
    Pid ! {'send_sync', Name, RequestInfo, Request,
           Timeout - ?TIMEOUT_DELTA, Priority, TransId, Self},
    receive
        {'return_sync', _, ResponseInfo, Response, _, TransId, Self} ->
            if
                Response == <<>> ->
                    {reply, {error, timeout}, State};
                ResponseInfo == <<>> ->
                    {reply, {ok, Response}, State};
                true ->
                    {reply, {ok, ResponseInfo, Response}, State}
            end
    after
        Timeout ->
            {reply, {error, timeout}, State}
    end;

handle_call({'mcast_async', Name, RequestInfo, Request,
             undefined, Priority}, Client,
            #state{timeout_async = TimeoutAsync} = State) ->
    handle_call({'mcast_async', Name, RequestInfo, Request,
                 TimeoutAsync, Priority}, Client, State);

handle_call({'mcast_async', Name, RequestInfo, Request,
             Timeout, Priority}, Client,
            #state{dest_deny = DestDeny,
                   dest_allow = DestAllow} = State) ->
    case destination_allowed(Name, DestDeny, DestAllow) of
        true ->
            handle_mcast_async(Name, RequestInfo, Request,
                               Timeout, Priority, Client, State);
        false ->
            {reply, {error, timeout}, State}
    end;

handle_call({'recv_async', Timeout, TransId}, Client,
            #state{async_responses = AsyncResponses} = State) ->
    if
        TransId == <<0:128>> ->
            case dict:to_list(AsyncResponses) of
                [] when Timeout >= ?RECV_ASYNC_INTERVAL ->
                    erlang:send_after(?RECV_ASYNC_INTERVAL, self(),
                                      {'recv_async',
                                       Timeout - ?RECV_ASYNC_INTERVAL,
                                       TransId, Client}),
                    {noreply, State};
                [] ->
                    {reply, {error, timeout}, State};
                [{TransIdUsed, {<<>>, Response}} | _] ->
                    NewAsyncResponses = dict:erase(TransIdUsed, AsyncResponses),
                    {reply, {ok, Response},
                     State#state{async_responses = NewAsyncResponses}};
                [{TransIdUsed, {ResponseInfo, Response}} | _] ->
                    NewAsyncResponses = dict:erase(TransIdUsed, AsyncResponses),
                    {reply, {ok, ResponseInfo, Response},
                     State#state{async_responses = NewAsyncResponses}}
            end;
        true ->
            case dict:find(TransId, AsyncResponses) of
                error when Timeout >= ?RECV_ASYNC_INTERVAL ->
                    erlang:send_after(?RECV_ASYNC_INTERVAL, self(),
                                      {'recv_async',
                                       Timeout - ?RECV_ASYNC_INTERVAL,
                                       TransId, Client}),
                    {noreply, State};
                error ->
                    {reply, {error, timeout}, State};
                {ok, {<<>>, Response}} ->
                    NewAsyncResponses = dict:erase(TransId, AsyncResponses),
                    {reply, {ok, Response},
                     State#state{async_responses = NewAsyncResponses}};
                {ok, {ResponseInfo, Response}} ->
                    NewAsyncResponses = dict:erase(TransId, AsyncResponses),
                    {reply, {ok, ResponseInfo, Response},
                     State#state{async_responses = NewAsyncResponses}}
            end
    end;

handle_call(Request, _, State) ->
    ?LOG_WARN("Unknown call \"~p\"", [Request]),
    {stop, string2:format("Unknown call \"~p\"", [Request]), error, State}.

handle_cast({'subscribe', Name},
            #state{job = Job,
                   prefix = Prefix} = State) ->
    list_pg:join(Prefix ++ Name, Job),
    {noreply, State};

handle_cast({'unsubscribe', Name},
            #state{job = Job,
                   prefix = Prefix} = State) ->
    list_pg:leave(Prefix ++ Name, Job),
    {noreply, State};

handle_cast(Request, State) ->
    ?LOG_WARN("Unknown cast \"~p\"", [Request]),
    {noreply, State}.

handle_info({list_pg_data, Groups},
            #state{dest_refresh = DestRefresh} = State) ->
    destination_refresh_start(DestRefresh),
    {noreply, State#state{list_pg_data = Groups}};

handle_info({'get_pid', Name, Timeout, {Exclude, _} = Client},
            #state{dest_refresh = DestRefresh,
                   list_pg_data = Groups} = State) ->
    case destination_get(DestRefresh, Name, Exclude, Groups) of
        {error, _} when Timeout >= ?SEND_SYNC_INTERVAL ->
            erlang:send_after(?SEND_SYNC_INTERVAL, self(),
                              {'get_pid', Name,
                               Timeout - ?SEND_SYNC_INTERVAL,
                               Client}),
            {noreply, State};
        {error, _} ->
            gen_server:reply(Client, {error, timeout}),
            {noreply, State};
        Pid ->
            gen_server:reply(Client, {ok, Pid}),
            {noreply, State}
    end;

handle_info({'send_async', Name, RequestInfo, Request,
             Timeout, Priority, Client}, State) ->
    handle_send_async(Name, RequestInfo, Request,
                      Timeout, Priority, Client, State);

handle_info({'send_async_active', Name, RequestInfo, Request,
             Timeout, Priority, Client}, State) ->
    handle_send_async_active(Name, RequestInfo, Request,
                             Timeout, Priority, Client, State);

handle_info({'send_sync', Name, RequestInfo, Request,
             Timeout, Priority, Client}, State) ->
    handle_send_sync(Name, RequestInfo, Request,
                     Timeout, Priority, Client, State);

handle_info({'mcast_async', Name, RequestInfo, Request,
             Timeout, Priority, Client}, State) ->
    handle_mcast_async(Name, RequestInfo, Request,
                       Timeout, Priority, Client, State);

handle_info({'forward_async', Name, RequestInfo, Request,
             Timeout, Priority, TransId, Pid},
            #state{dest_refresh = DestRefresh,
                   list_pg_data = Groups,
                   dest_deny = DestDeny,
                   dest_allow = DestAllow} = State) ->
    case destination_allowed(Name, DestDeny, DestAllow) of
        true ->
            case destination_get(DestRefresh, Name, Pid, Groups) of
                {error, _} when Timeout >= ?FORWARD_ASYNC_INTERVAL ->
                    erlang:send_after(?FORWARD_ASYNC_INTERVAL, self(),
                                      {'forward_async', Name,
                                       RequestInfo, Request,
                                       Timeout - ?FORWARD_ASYNC_INTERVAL,
                                       Priority, TransId, Pid}),
                    ok;
                {error, _} ->
                    ok;
                NextPid when Timeout >= ?FORWARD_DELTA ->
                    NextPid ! {'send_async', Name, RequestInfo, Request,
                               Timeout - ?FORWARD_DELTA,
                               Priority, TransId, Pid};
                _ ->
                    ok
            end;
        false ->
            ok
    end,
    {noreply, State};

handle_info({'forward_sync', Name, RequestInfo, Request,
             Timeout, Priority, TransId, Pid},
            #state{dest_refresh = DestRefresh,
                   list_pg_data = Groups,
                   dest_deny = DestDeny,
                   dest_allow = DestAllow} = State) ->
    case destination_allowed(Name, DestDeny, DestAllow) of
        true ->
            case destination_get(DestRefresh, Name, Pid, Groups) of
                {error, _} when Timeout >= ?FORWARD_SYNC_INTERVAL ->
                    erlang:send_after(?FORWARD_SYNC_INTERVAL, self(),
                                      {'forward_sync', Name,
                                       RequestInfo, Request,
                                       Timeout - ?FORWARD_SYNC_INTERVAL,
                                       Priority, TransId, Pid}),
                    ok;
                {error, _} ->
                    ok;
                NextPid when Timeout >= ?FORWARD_DELTA ->
                    NextPid ! {'send_sync', Name, RequestInfo, Request,
                               Timeout - ?FORWARD_DELTA,
                               Priority, TransId, Pid};
                _ ->
                    ok
            end;
        false ->
            ok
    end,
    {noreply, State};

handle_info({'recv_async', Timeout, TransId, Client},
            #state{async_responses = AsyncResponses} = State) ->
    if
        TransId == <<0:128>> ->
            case dict:to_list(AsyncResponses) of
                [] when Timeout >= ?RECV_ASYNC_INTERVAL ->
                    erlang:send_after(?RECV_ASYNC_INTERVAL, self(),
                                      {'recv_async',
                                       Timeout - ?RECV_ASYNC_INTERVAL,
                                       TransId, Client}),
                    {noreply, State};
                [] ->
                    gen_server:reply(Client, {error, timeout}),
                    {noreply, State};
                [{TransIdUsed, {<<>>, Response}} | _] ->
                    NewAsyncResponses = dict:erase(TransIdUsed, AsyncResponses),
                    gen_server:reply(Client, {ok, Response}),
                    {noreply,
                     State#state{async_responses = NewAsyncResponses}};
                [{TransIdUsed, {ResponseInfo, Response}} | _] ->
                    NewAsyncResponses = dict:erase(TransIdUsed, AsyncResponses),
                    gen_server:reply(Client, {ok, ResponseInfo, Response}),
                    {noreply,
                     State#state{async_responses = NewAsyncResponses}}
            end;
        true ->
            case dict:find(TransId, AsyncResponses) of
                error when Timeout >= ?RECV_ASYNC_INTERVAL ->
                    erlang:send_after(?RECV_ASYNC_INTERVAL, self(),
                                      {'recv_async',
                                       Timeout - ?RECV_ASYNC_INTERVAL,
                                       TransId, Client}),
                    {noreply, State};
                error ->
                    gen_server:reply(Client, {error, timeout}),
                    {noreply, State};
                {ok, {<<>>, Response}} ->
                    NewAsyncResponses = dict:erase(TransId, AsyncResponses),
                    gen_server:reply(Client, {ok, Response}),
                    {noreply, State#state{async_responses = NewAsyncResponses}};
                {ok, {ResponseInfo, Response}} ->
                    NewAsyncResponses = dict:erase(TransId, AsyncResponses),
                    gen_server:reply(Client, {ok, ResponseInfo, Response}),
                    {noreply, State#state{async_responses = NewAsyncResponses}}
            end
    end;

handle_info({'return_async', Name, ResponseInfo, Response,
             Timeout, TransId, Pid},
            #state{job = Job} = State) ->
    true = Pid == self(),
    case send_timeout_check(TransId, State) of
        error ->
            % send_async timeout already occurred
            {noreply, State};
        {ok, {active, Tref}} when Response == <<>> ->
            erlang:cancel_timer(Tref),
            Job ! {'timeout_async_active', TransId},
            {noreply, send_timeout_end(TransId, State)};
        {ok, {active, Tref}} ->
            erlang:cancel_timer(Tref),
            Job ! {'return_async_active', Name, ResponseInfo, Response,
                   Timeout, TransId},
            {noreply, send_timeout_end(TransId, State)};
        {ok, {passive, Tref}} when Response == <<>> ->
            erlang:cancel_timer(Tref),
            {noreply, send_timeout_end(TransId, State)};
        {ok, {passive, Tref}} ->
            erlang:cancel_timer(Tref),
            {noreply,
             recv_async_timeout_start(ResponseInfo, Response, Timeout, TransId,
                                      send_timeout_end(TransId, State))}
    end;

handle_info({'return_sync', _Name, _ResponseInfo, _Response,
             _Timeout, _TransId, _Pid},
            State) ->
    % a response after a timeout is discarded
    {noreply, State};

handle_info({'send_async_timeout', TransId},
            #state{job = Job} = State) ->
    case send_timeout_check(TransId, State) of
        error ->
            % should never happen, timer should have been cancelled
            % if the send_async already returned
            %XXX
            {noreply, State};
        {ok, {active, _}} ->
            Job ! {'timeout_async_active', TransId},
            {noreply, send_timeout_end(TransId, State)};
        {ok, _} ->
            {noreply, send_timeout_end(TransId, State)}
    end;

handle_info({'recv_async_timeout', TransId},
            State) ->
    {noreply, recv_async_timeout_end(TransId, State)};

handle_info(Request, State) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

handle_send_async(Name, RequestInfo, Request,
                  Timeout, Priority, {Exclude, _} = Client,
                  #state{uuid_generator = UUID,
                         dest_refresh = DestRefresh,
                         list_pg_data = Groups} = State) ->
    case destination_get(DestRefresh, Name, Exclude, Groups) of
        {error, _} when Timeout >= ?SEND_ASYNC_INTERVAL ->
            erlang:send_after(?SEND_ASYNC_INTERVAL, self(),
                              {'send_async', Name,
                               RequestInfo, Request,
                               Timeout - ?SEND_ASYNC_INTERVAL,
                               Priority, Client}),
            {noreply, State};
        {error, _} ->
            gen_server:reply(Client, {error, timeout}),
            {noreply, State};
        Pid ->
            TransId = uuid:get_v1(UUID),
            Pid ! {'send_async', Name, RequestInfo, Request,
                   Timeout, Priority, TransId, self()},
            gen_server:reply(Client, {ok, TransId}),
            {noreply, send_async_timeout_start(Timeout, TransId, State)}
    end.

handle_send_async_active(Name, RequestInfo, Request,
                         Timeout, Priority, {Exclude, _} = Client,
                         #state{uuid_generator = UUID,
                                dest_refresh = DestRefresh,
                                list_pg_data = Groups} = State) ->
    case destination_get(DestRefresh, Name, Exclude, Groups) of
        {error, _} when Timeout >= ?SEND_ASYNC_INTERVAL ->
            erlang:send_after(?SEND_ASYNC_INTERVAL, self(),
                              {'send_async_active', Name,
                               RequestInfo, Request,
                               Timeout - ?SEND_ASYNC_INTERVAL,
                               Priority, Client}),
            {noreply, State};
        {error, _} ->
            gen_server:reply(Client, {error, timeout}),
            {noreply, State};
        Pid ->
            TransId = uuid:get_v1(UUID),
            Pid ! {'send_async', Name, RequestInfo, Request,
                   Timeout, Priority, TransId, self()},
            gen_server:reply(Client, {ok, TransId}),
            {noreply, send_async_active_timeout_start(Timeout, TransId, State)}
    end.

handle_send_sync(Name, RequestInfo, Request,
                 Timeout, Priority, {Exclude, _} = Client,
                 #state{uuid_generator = UUID,
                        dest_refresh = DestRefresh,
                        list_pg_data = Groups} = State) ->
    case destination_get(DestRefresh, Name, Exclude, Groups) of
        {error, _} when Timeout >= ?SEND_SYNC_INTERVAL ->
            erlang:send_after(?SEND_SYNC_INTERVAL, self(),
                              {'send_sync', Name,
                               RequestInfo, Request,
                               Timeout - ?SEND_SYNC_INTERVAL,
                               Priority, Client}),
            {noreply, State};
        {error, _} ->
            gen_server:reply(Client, {error, timeout}),
            {noreply, State};
        Pid ->
            TransId = uuid:get_v1(UUID),
            Self = self(),
            Pid ! {'send_sync', Name, RequestInfo, Request,
                   Timeout, Priority, TransId, Self},
            receive
                {'return_sync', _, ResponseInfo, Response, _, TransId, Self} ->
                    if
                        Response == <<>> ->
                            gen_server:reply(Client, {error, timeout});
                        ResponseInfo == <<>> ->
                            gen_server:reply(Client, {ok, Response});
                        true ->
                            gen_server:reply(Client, {ok, ResponseInfo,
                                                      Response})
                    end
            after
                Timeout ->
                    gen_server:reply(Client, {error, timeout})
            end,
            {noreply, State}
    end.

handle_mcast_async(Name, RequestInfo, Request,
                   Timeout, Priority, {Exclude, _} = Client,
                   #state{uuid_generator = UUID,
                          dest_refresh = DestRefresh,
                          list_pg_data = Groups} = State) ->
    Self = self(),
    case destination_all(DestRefresh, Name, Exclude, Groups) of
        {error, _} when Timeout >= ?MCAST_ASYNC_INTERVAL ->
            erlang:send_after(?MCAST_ASYNC_INTERVAL, Self,
                              {'mcast_async', Name,
                               RequestInfo, Request,
                               Timeout - ?MCAST_ASYNC_INTERVAL,
                               Priority, Client}),
            {noreply, State};
        {error, _} ->
            gen_server:reply(Client, {error, timeout}),
            {noreply, State};
        PidList ->
            TransIdList = lists:map(fun(Pid) ->
                TransId = uuid:get_v1(UUID),
                Pid ! {'send_async', Name, RequestInfo, Request,
                       Timeout, Priority, TransId, Self},
                TransId
            end, PidList),
            gen_server:reply(Client, {ok, TransIdList}),
            NewState = lists:foldl(fun(Id, S) ->
                send_async_timeout_start(Timeout, Id, S)
            end, State, TransIdList),
            {noreply, NewState}
            
    end.

destination_allowed([], _, _) ->
    false;

destination_allowed(_, undefined, undefined) ->
    true;

destination_allowed(Name, undefined, DestAllow) ->
    trie:is_prefixed(Name, "/", DestAllow);

destination_allowed(Name, DestDeny, undefined) ->
    not trie:is_prefixed(Name, "/", DestDeny);

destination_allowed(Name, DestDeny, DestAllow) ->
    case trie:is_prefixed(Name, "/", DestDeny) of
        true -> 
            false;  
        false ->
            trie:is_prefixed(Name, "/", DestAllow)
    end.    

destination_refresh_first(lazy_closest) ->
    list_pg_data:get_groups(?DEST_REFRESH_FIRST);

destination_refresh_first(lazy_random) ->
    list_pg_data:get_groups(?DEST_REFRESH_FIRST);

destination_refresh_first(immediate_closest) ->
    ok;

destination_refresh_first(immediate_random) ->
    ok;

destination_refresh_first(none) ->
    ok.

destination_refresh_start(lazy_closest) ->
    list_pg_data:get_groups(?DEST_REFRESH_SLOW);

destination_refresh_start(lazy_random) ->
    list_pg_data:get_groups(?DEST_REFRESH_SLOW);

destination_refresh_start(immediate_closest) ->
    ok;

destination_refresh_start(immediate_random) ->
    ok;

destination_refresh_start(none) ->
    ok.

destination_get(lazy_closest, Name, Pid, Groups)
    when is_list(Name) ->
    list_pg_data:get_closest_pid(Name, Pid, Groups);

destination_get(lazy_random, Name, Pid, Groups)
    when is_list(Name) ->
    list_pg_data:get_random_pid(Name, Pid, Groups);

destination_get(immediate_closest, Name, Pid, _)
    when is_list(Name) ->
    list_pg:get_closest_pid(Name, Pid);

destination_get(immediate_random, Name, Pid, _)
    when is_list(Name) ->
    list_pg:get_random_pid(Name, Pid);

destination_get(DestRefresh, _, _, _) ->
    ?LOG_ERROR("unable to send with invalid destination refresh: ~p",
               [DestRefresh]),
    throw(badarg).

destination_all(lazy_closest, Name, Pid, Groups)
    when is_list(Name) ->
    list_pg_data:get_members(Name, Pid, Groups);

destination_all(lazy_random, Name, Pid, Groups)
    when is_list(Name) ->
    list_pg_data:get_members(Name, Pid, Groups);

destination_all(immediate_closest, Name, Pid, _)
    when is_list(Name) ->
    list_pg:get_members(Name, Pid);

destination_all(immediate_random, Name, Pid, _)
    when is_list(Name) ->
    list_pg:get_members(Name, Pid);

destination_all(DestRefresh, _, _, _) ->
    ?LOG_ERROR("unable to send with invalid destination refresh: ~p",
               [DestRefresh]),
    throw(badarg).

send_async_timeout_start(Timeout, TransId,
                         #state{send_timeouts = Ids} = State)
    when is_integer(Timeout), is_binary(TransId) ->
    Tref = erlang:send_after(Timeout, self(), {'send_async_timeout', TransId}),
    State#state{send_timeouts = dict:store(TransId, {passive, Tref}, Ids)}.

send_async_active_timeout_start(Timeout, TransId,
                                #state{send_timeouts = Ids} = State)
    when is_integer(Timeout), is_binary(TransId) ->
    Tref = erlang:send_after(Timeout, self(), {'send_async_timeout', TransId}),
    State#state{send_timeouts = dict:store(TransId, {active, Tref}, Ids)}.

send_timeout_check(TransId, #state{send_timeouts = Ids})
    when is_binary(TransId) ->
    dict:find(TransId, Ids).

send_timeout_end(TransId, #state{send_timeouts = Ids} = State)
    when is_binary(TransId) ->
    State#state{send_timeouts = dict:erase(TransId, Ids)}.

recv_async_timeout_start(ResponseInfo, Response, Timeout, TransId,
                         #state{async_responses = Ids} = State)
    when is_binary(Response), is_integer(Timeout), is_binary(TransId) ->
    erlang:send_after(Timeout, self(), {'recv_async_timeout', TransId}),
    State#state{async_responses = dict:store(TransId,
                                             {ResponseInfo, Response}, Ids)}.

recv_async_timeout_end(TransId,
                       #state{async_responses = Ids} = State)
    when is_binary(TransId) ->
    State#state{async_responses = dict:erase(TransId, Ids)}.

