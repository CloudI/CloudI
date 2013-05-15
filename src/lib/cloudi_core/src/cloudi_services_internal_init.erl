%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Internal Service Init Process==
%%% A separate Erlang process that exists as a Dispatcher proxy.  Using this
%%% Erlang process as a type of bootstrap process prevents a deadlock on the
%%% Dispatcher process while still allowing the internal service init function
%%% call to occur within the Dispatcher process.
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

-module(cloudi_services_internal_init).
-author('mjtruog [at] gmail (dot) com').

-behaviour(gen_server).

%% external interface
-export([start_link/2,
         stop_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,
    {
        service_state,
        init_timeout
    }).

-include("cloudi_logger.hrl").
-include("cloudi_constants.hrl").

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

start_link(Timeout, InternalState) ->
    gen_server:start_link(?MODULE,
                          [Timeout, erlang:get(), InternalState],
                          [{timeout, Timeout}]).
stop_link(Pid) ->
    {ProcessDictionary, InternalState} = gen_server:call(Pid, stop, infinity),
    erlang:erase(),
    lists:foreach(fun({K, V}) ->
        erlang:put(K, V)
    end, ProcessDictionary),
    InternalState.

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([Timeout, ProcessDictionary, InternalState]) ->
    InitTimeout = erlang:send_after(Timeout, self(),
                                    'cloudi_service_init_timeout'),
    lists:foreach(fun({K, V}) ->
        erlang:put(K, V)
    end, ProcessDictionary),
    {ok, #state{service_state = InternalState,
                init_timeout = InitTimeout}}.

handle_call(stop, {Pid, _}, #state{service_state = InternalState,
                                   init_timeout = InitTimeout} = State) ->
    erlang:cancel_timer(InitTimeout),
    Result = {erlang:get(), InternalState},
    NewState = State#state{service_state = undefined,
                           init_timeout = undefined},
    erlang:unlink(Pid),
    {stop, normal, Result, NewState};

handle_call(Request, From, #state{service_state = InternalState} = State) ->
    case cloudi_services_internal:handle_call(Request, From, InternalState) of
        {reply, Reply, NewInternalState} ->
            {reply, Reply,
             State#state{service_state = NewInternalState}};
        {reply, Reply, NewInternalState, Timeout} ->
            {reply, Reply,
             State#state{service_state = NewInternalState}, Timeout};
        {noreply, NewInternalState} ->
            {noreply,
             State#state{service_state = NewInternalState}};
        {noreply, NewInternalState, Timeout} ->
            {noreply,
             State#state{service_state = NewInternalState}, Timeout};
        {stop, Reason, Reply, NewInternalState} ->
            {stop, Reason, Reply,
             State#state{service_state = NewInternalState}};
        {stop, Reason, NewInternalState} ->
            {stop, Reason,
             State#state{service_state = NewInternalState}}
    end.

handle_cast(Request, #state{service_state = InternalState} = State) ->
    case cloudi_services_internal:handle_cast(Request, InternalState) of
        {noreply, NewInternalState} ->
            {noreply,
             State#state{service_state = NewInternalState}};
        {noreply, NewInternalState, Timeout} ->
            {noreply,
             State#state{service_state = NewInternalState}, Timeout};
        {stop, Reason, NewInternalState} ->
            {stop, Reason,
             State#state{service_state = NewInternalState}}
    end.

handle_info('cloudi_service_init_timeout', State) ->
    {stop, timeout, State#state{init_timeout = undefined}};

handle_info(Request, #state{service_state = InternalState} = State) ->
    case cloudi_services_internal:handle_info(Request, InternalState) of
        {noreply, NewInternalState} ->
            {noreply,
             State#state{service_state = NewInternalState}};
        {noreply, NewInternalState, Timeout} ->
            {noreply,
             State#state{service_state = NewInternalState}, Timeout};
        {stop, Reason, NewInternalState} ->
            {stop, Reason,
             State#state{service_state = NewInternalState}}
    end.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

