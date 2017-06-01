%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
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
%%% MIT License
%%%
%%% Copyright (c) 2013-2017 Michael Truog <mjtruog at gmail dot com>
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
%%% @author Michael Truog <mjtruog [at] gmail (dot) com>
%%% @copyright 2013-2017 Michael Truog
%%% @version 1.7.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_core_i_services_internal_init).
-author('mjtruog [at] gmail (dot) com').

-behaviour(gen_server).

%% external interface
-export([start_link/4,
         stop_link/1,
         process_dictionary_get/0,
         process_dictionary_set/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,
    {
        service_state :: any(),
        init_timeout :: undefined | reference()
    }).

-include("cloudi_logger.hrl").
-include("cloudi_core_i_constants.hrl").
-include("cloudi_core_i_services_common_init.hrl").

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

start_link(Timeout, PidOptions, ProcessDictionary, InternalState) ->
    gen_server:start_link(?MODULE,
                          [Timeout, PidOptions,
                           ProcessDictionary, InternalState],
                          [{timeout, Timeout},
                           {spawn_opt,
                            spawn_opt_options_before(PidOptions)}]).
stop_link(Pid) ->
    gen_server:call(Pid, stop, infinity).

process_dictionary_get() ->
    erlang:get().

process_dictionary_set(ProcessDictionary) ->
    erlang:erase(),
    lists:foreach(fun({K, V}) ->
        erlang:put(K, V)
    end, ProcessDictionary),
    ok.

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([Timeout, PidOptions, ProcessDictionary, InternalState]) ->
    ok = spawn_opt_options_after(PidOptions),
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

handle_call(Request, _, State)
    when is_tuple(Request),
         (element(1, Request) =:= 'send_sync' orelse
          element(1, Request) =:= 'recv_async' orelse
          element(1, Request) =:= 'recv_asyncs') ->
    % service request responses are always handled after initialization
    % is successful though it is possible for the
    % service request response to be received before initialization
    % is complete (the response remains queued)
    {stop, {error, invalid_state}, State};

handle_call(Request, From, #state{service_state = InternalState} = State) ->
    case cloudi_core_i_services_internal:
         handle_call(Request, From, InternalState) of
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
             State#state{service_state = NewInternalState}}%;
        %{stop, Reason, NewInternalState} ->
        %    {stop, Reason,
        %     State#state{service_state = NewInternalState}}
    end.

handle_cast(Request, #state{service_state = InternalState} = State) ->
    case cloudi_core_i_services_internal:
         handle_cast(Request, InternalState) of
        {noreply, NewInternalState} ->
            {noreply,
             State#state{service_state = NewInternalState}};
        {noreply, NewInternalState, Timeout} ->
            {noreply,
             State#state{service_state = NewInternalState}, Timeout}%;
        %{stop, Reason, NewInternalState} ->
        %    {stop, Reason,
        %     State#state{service_state = NewInternalState}}
    end.

handle_info('cloudi_service_init_timeout', State) ->
    {stop, timeout, State#state{init_timeout = undefined}};

handle_info(Request, #state{service_state = InternalState} = State) ->
    case cloudi_core_i_services_internal:
         handle_info(Request, InternalState) of
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

