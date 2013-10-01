%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Work Manager==
%%% Queues up results that need to be sent to data modules, to avoid
%%% overloading the data repositories. This CloudI service can store a
%%% specific destination that is used for all binary traffic.  Many
%%% destinations are handled in one work manager if any internal
%%% services (i.e., Erlang services, using the cloudi_service behavior) send
%%% "{Name, Data}" tuples to this service.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2009-2013, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2009-2013 Michael Truog
%%% @version 1.2.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_work_manager).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloudi_service).

%% external interface

%% cloudi_service callbacks
-export([cloudi_service_init/3,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/2]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").

-define(DEFAULT_DELAY,                  1000). % milliseconds
-define(DEFAULT_DESTINATION,       undefined). % a Name, e.g. "/db/pgsql"

-record(state,
    {
        delay,
        destination,
        delay_timer,
        queue = cloudi_x_trie:new()
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, _Prefix, Dispatcher) ->
    Defaults = [
        {delay,           ?DEFAULT_DELAY},
        {destination,     ?DEFAULT_DESTINATION},
        {name,            undefined}],
    [Delay, Destination, Name] =
        cloudi_proplists:take_values(Defaults, Args),
    true = is_integer(Delay),
    true = is_list(Destination),
    true = is_list(Name),
    cloudi_service:subscribe(Dispatcher, Name),
    {ok, #state{delay = Delay,
                destination = Destination,
                delay_timer = erlang:send_after(Delay, self(), empty)}}.

cloudi_service_handle_request(_Type, _Name, _Pattern, _RequestInfo, Request,
                              _Timeout, _Priority, _TransId, _Pid,
                              #state{destination = Destination,
                                     queue = Queue} = State,
                              _Dispatcher) ->
    case Request of
        {DestinationName, Data} when is_list(DestinationName) ->
            {reply, ok,
             State#state{queue = cloudi_x_trie:prefix(DestinationName, Data, Queue)}};
        Data when is_binary(Data) ->
            {reply, cloudi_response:new(Data, ok),
             State#state{queue = cloudi_x_trie:prefix(Destination, Data, Queue)}}
    end.

cloudi_service_handle_info(empty,
                           #state{delay = Delay,
                                  queue = Queue} = State, Dispatcher) ->
    
    NewQueue = cloudi_x_trie:map(fun(Name, DataList) ->
        send_data(lists:reverse(DataList), Name, Dispatcher)
    end, Queue),
    {noreply,
     State#state{queue = NewQueue,
                 delay_timer = erlang:send_after(Delay, self(), empty)}};

cloudi_service_handle_info(Request, State, _) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_service_terminate(_, #state{}) ->
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

send_data([], _, _) ->
    [];

send_data(L, Name, Dispatcher) ->
    send_data([], 0, L, Name, Dispatcher).

send_data([], 0, [], _, _) ->
    [];

send_data(Failed, FailedCount, [], Name, _) ->
    ?LOG_WARN("~s failed ~w requests", [Name, FailedCount]),
    lists:reverse(Failed);

send_data(Failed, FailedCount, [Data | L], Name, Dispatcher) ->
    case cloudi_service:send_sync(Dispatcher, Name, Data) of
        {ok, _} ->
            send_data(Failed, FailedCount, L, Name, Dispatcher);
        {error, Reason} ->
            if
                Reason =/= timeout ->
                    ?LOG_ERROR("~s error ~p", [Name, Reason]);
                true ->
                    ok
            end,
            send_data([Data | Failed], FailedCount + 1, L, Name, Dispatcher)
    end.

