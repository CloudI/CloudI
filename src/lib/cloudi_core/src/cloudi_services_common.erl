%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Services Fuctions Common to Both Internal and External Services==
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
%%% @version 1.2.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_services_common).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([destination_allowed/3,
         destination_refresh_first/2,
         destination_refresh_start/2,
         destination_get/4,
         destination_all/4,
         send_async_timeout_start/4,
         send_sync_timeout_start/5,
         send_timeout_check/2,
         send_timeout_end/2,
         recv_timeout_start/7,
         async_response_timeout_start/6,
         async_response_timeout_end/2,
         recv_async_select_random/1,
         recv_async_select_oldest/1]).

-include("cloudi_configuration.hrl").
-include("cloudi_logger.hrl").

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

destination_allowed([], _, _) ->
    false;

destination_allowed(_, undefined, undefined) ->
    true;

destination_allowed(Name, undefined, DestAllow) ->
    case trie:find_match(Name, DestAllow) of
        {ok, _, _} ->
            true;
        error ->
            false
    end;

destination_allowed(Name, DestDeny, undefined) ->
    case trie:find_match(Name, DestDeny) of
        {ok, _, _} ->
            false;
        error ->
            true
    end;

destination_allowed(Name, DestDeny, DestAllow) ->
    case trie:find_match(Name, DestDeny) of
        {ok, _, _} ->
            false;
        error ->
            case trie:find_match(Name, DestAllow) of
                {ok, _, _} ->
                    true;
                error ->
                    false
            end
    end.

destination_refresh_first(DestRefresh,
                          #config_service_options{dest_refresh_start = Delay})
    when (DestRefresh =:= lazy_closest orelse
          DestRefresh =:= lazy_furthest orelse
          DestRefresh =:= lazy_random orelse
          DestRefresh =:= lazy_local orelse
          DestRefresh =:= lazy_remote) ->
    cpg_data:get_groups(Delay);

destination_refresh_first(DestRefresh, _)
    when (DestRefresh =:= immediate_closest orelse
          DestRefresh =:= immediate_furthest orelse
          DestRefresh =:= immediate_random orelse
          DestRefresh =:= immediate_local orelse
          DestRefresh =:= immediate_remote) ->
    ok;

destination_refresh_first(none, _) ->
    ok.

destination_refresh_start(DestRefresh,
                          #config_service_options{dest_refresh_delay = Delay})
    when (DestRefresh =:= lazy_closest orelse
          DestRefresh =:= lazy_furthest orelse
          DestRefresh =:= lazy_random orelse
          DestRefresh =:= lazy_local orelse
          DestRefresh =:= lazy_remote) ->
    cpg_data:get_groups(Delay);

destination_refresh_start(DestRefresh, _)
    when (DestRefresh =:= immediate_closest orelse
          DestRefresh =:= immediate_furthest orelse
          DestRefresh =:= immediate_random orelse
          DestRefresh =:= immediate_local orelse
          DestRefresh =:= immediate_remote) ->
    ok;

destination_refresh_start(none, _) ->
    ok.

destination_get(lazy_closest, Name, Pid, Groups)
    when is_list(Name) ->
    cpg_data:get_closest_pid(Name, Pid, Groups);

destination_get(lazy_furthest, Name, Pid, Groups)
    when is_list(Name) ->
    cpg_data:get_furthest_pid(Name, Pid, Groups);

destination_get(lazy_random, Name, Pid, Groups)
    when is_list(Name) ->
    cpg_data:get_random_pid(Name, Pid, Groups);

destination_get(lazy_local, Name, Pid, Groups)
    when is_list(Name) ->
    cpg_data:get_local_pid(Name, Pid, Groups);

destination_get(lazy_remote, Name, Pid, Groups)
    when is_list(Name) ->
    cpg_data:get_remote_pid(Name, Pid, Groups);

destination_get(immediate_closest, Name, Pid, _)
    when is_list(Name) ->
    cpg:get_closest_pid(Name, Pid);

destination_get(immediate_furthest, Name, Pid, _)
    when is_list(Name) ->
    cpg:get_furthest_pid(Name, Pid);

destination_get(immediate_random, Name, Pid, _)
    when is_list(Name) ->
    cpg:get_random_pid(Name, Pid);

destination_get(immediate_local, Name, Pid, _)
    when is_list(Name) ->
    cpg:get_local_pid(Name, Pid);

destination_get(immediate_remote, Name, Pid, _)
    when is_list(Name) ->
    cpg:get_remote_pid(Name, Pid);

destination_get(DestRefresh, _, _, _) ->
    ?LOG_ERROR("unable to send with invalid destination refresh: ~p",
               [DestRefresh]),
    throw(badarg).

destination_all(DestRefresh, Name, Pid, Groups)
    when is_list(Name),
         (DestRefresh =:= lazy_closest orelse
          DestRefresh =:= lazy_furthest orelse
          DestRefresh =:= lazy_random orelse
          DestRefresh =:= lazy_local orelse
          DestRefresh =:= lazy_remote) ->
    cpg_data:get_members(Name, Pid, Groups);

destination_all(DestRefresh, Name, Pid, _)
    when is_list(Name),
         (DestRefresh =:= immediate_closest orelse
          DestRefresh =:= immediate_furthest orelse
          DestRefresh =:= immediate_random orelse
          DestRefresh =:= immediate_local orelse
          DestRefresh =:= immediate_remote) ->
    cpg:get_members(Name, Pid);

destination_all(DestRefresh, _, _, _) ->
    ?LOG_ERROR("unable to send with invalid destination refresh: ~p",
               [DestRefresh]),
    throw(badarg).

send_async_timeout_start(Timeout, TransId, SendTimeouts, Dispatcher)
    when is_integer(Timeout), is_binary(TransId) ->
    Tref = erlang:send_after(Timeout, Dispatcher,
                             {'cloudi_service_send_async_timeout', TransId}),
    dict:store(TransId, {passive, Tref}, SendTimeouts).

send_sync_timeout_start(Timeout, TransId, Client, SendTimeouts, Dispatcher)
    when is_integer(Timeout), is_binary(TransId) ->
    Tref = erlang:send_after(Timeout, Dispatcher,
                             {'cloudi_service_send_sync_timeout', TransId}),
    dict:store(TransId, {Client, Tref}, SendTimeouts).

send_timeout_check(TransId, SendTimeouts)
    when is_binary(TransId) ->
    dict:find(TransId, SendTimeouts).

send_timeout_end(TransId, SendTimeouts)
    when is_binary(TransId) ->
    dict:erase(TransId, SendTimeouts).

recv_timeout_start(Timeout, Priority, TransId, T,
                   RecvTimeouts, Queue, Dispatcher)
    when is_integer(Timeout), is_integer(Priority), is_binary(TransId) ->
    Tref = erlang:send_after(Timeout, Dispatcher,
                             {'cloudi_service_recv_timeout',
                              Priority, TransId}),
    {dict:store(TransId, Tref, RecvTimeouts),
     pqueue4:in(T, Priority, Queue)}.

async_response_timeout_start(ResponseInfo, Response, Timeout, TransId,
                             AsyncResponses, Dispatcher)
    when is_integer(Timeout), is_binary(TransId) ->
    erlang:send_after(Timeout, Dispatcher,
                      {'cloudi_service_recv_async_timeout', TransId}),
    dict:store(TransId, {ResponseInfo, Response}, AsyncResponses).

async_response_timeout_end(TransId, AsyncResponses)
    when is_binary(TransId) ->
    dict:erase(TransId, AsyncResponses).

recv_async_select_random([{TransId, _} | _]) ->
    TransId.

recv_async_select_oldest([{TransId, _} | L]) ->
    recv_async_select_oldest(L, uuid:get_v1_time(TransId), TransId).

recv_async_select_oldest([], _, TransIdCurrent) ->
    TransIdCurrent;

recv_async_select_oldest([{TransId, _} | L], Time0, TransIdCurrent) ->
    Time1 = uuid:get_v1_time(TransId),
    if
        Time1 < Time0 ->
            recv_async_select_oldest(L, Time1, TransId);
        true ->
            recv_async_select_oldest(L, Time0, TransIdCurrent)
    end.

