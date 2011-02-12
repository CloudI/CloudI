%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Cloudi Job Behavior==
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
%%% @version 0.1.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_job).
-author('mjtruog [at] gmail (dot) com').

-behaviour(gen_server).

%% dispatcher interface
-export([start_link/3]).

%% behavior interface
-export([subscribe/2,
         unsubscribe/2,
         send_async/3,
         send_async/4,
         send_sync/3,
         send_sync/4,
         forward/7,
         forward_async/6,
         forward_sync/6,
         recv_async/3,
         return/7,
         return_async/6,
         return_sync/6]).

%% behavior callbacks
-export([behaviour_info/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("cloudi_logger.hrl").
-include("cloudi_constants.hrl").

-record(state,
    {
        module,          % job module
        dispatcher,      % dispatcher pid
        job_state        % job state
    }).

%%%------------------------------------------------------------------------
%%% Callback functions from behavior
%%%------------------------------------------------------------------------

-spec behaviour_info(atom()) -> 'undefined' | [{atom(), byte()}].

behaviour_info(callbacks) ->
    [
        {cloudi_job_init, 2},
        {cloudi_job_handle_request, 8},
        {cloudi_job_handle_info, 3},
        {cloudi_job_terminate, 2}
    ];
behaviour_info(_) ->
    undefined.

%%%------------------------------------------------------------------------
%%% Dispatcher interface functions
%%%------------------------------------------------------------------------

start_link(Module, Args, Timeout)
    when is_atom(Module), is_list(Args), is_integer(Timeout) ->
    gen_server:start_link(?MODULE, [Module, Args, self()],
                          [{timeout, Timeout}]).

%%%------------------------------------------------------------------------
%%% Behavior interface functions
%%%------------------------------------------------------------------------

subscribe(Dispatcher, Name)
    when is_pid(Dispatcher), is_list(Name) ->
    gen_server:cast(Dispatcher, {'subscribe', Name}).

unsubscribe(Dispatcher, Name)
    when is_pid(Dispatcher), is_list(Name) ->
    gen_server:cast(Dispatcher, {'unsubscribe', Name}).

send_async(Dispatcher, Name, Request)
    when is_pid(Dispatcher), is_list(Name) ->
    gen_server:call(Dispatcher, {'send_async', Name, Request}, infinity).

send_async(Dispatcher, Name, Request, Timeout)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout), Timeout > 0 ->
    gen_server:call(Dispatcher, {'send_async', Name, Request,
                                 Timeout - ?TIMEOUT_DELTA}, Timeout).

send_sync(Dispatcher, Name, Request)
    when is_pid(Dispatcher), is_list(Name) ->
    gen_server:call(Dispatcher, {'send_sync', Name, Request}, infinity).

send_sync(Dispatcher, Name, Request, Timeout)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout), Timeout > 0 ->
    gen_server:call(Dispatcher, {'send_sync', Name, Request,
                                 Timeout - ?TIMEOUT_DELTA}, Timeout).

forward(Dispatcher, 'send_async', Name, Request, Timeout, TransId, Pid) ->
    forward_async(Dispatcher, Name, Request, Timeout, TransId, Pid);

forward(Dispatcher, 'send_sync', Name, Request, Timeout, TransId, Pid) ->
    forward_sync(Dispatcher, Name, Request, Timeout, TransId, Pid).

forward_async(Dispatcher, Name, Request, Timeout, TransId, Pid)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         is_binary(TransId), is_pid(Pid), Timeout > 0 ->
    Dispatcher ! {'forward_async', Name, Request, Timeout, TransId, Pid},
    erlang:throw(forward).

forward_sync(Dispatcher, Name, Request, Timeout, TransId, Pid)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         is_binary(TransId), is_pid(Pid), Timeout > 0 ->
    Dispatcher ! {'forward_sync', Name, Request, Timeout, TransId, Pid},
    erlang:throw(forward).

recv_async(Dispatcher, Timeout, TransId)
    when is_pid(Dispatcher), is_integer(Timeout), is_binary(TransId),
         Timeout > 0 ->
    gen_server:call(Dispatcher, {'recv_async', Timeout - ?TIMEOUT_DELTA,
                                 TransId}, Timeout).

return(Dispatcher, 'send_async', Name, Response, Timeout, TransId, Pid) ->
    return_async(Dispatcher, Name, Response, Timeout, TransId, Pid);

return(Dispatcher, 'send_sync', Name, Response, Timeout, TransId, Pid) ->
    return_sync(Dispatcher, Name, Response, Timeout, TransId, Pid).

return_async(Dispatcher, Name, Response, Timeout, TransId, Pid)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         is_binary(TransId), is_pid(Pid), Timeout > 0 ->
    Dispatcher ! {'return_async', Name, Response, Timeout, TransId, Pid},
    erlang:throw(return).

return_sync(Dispatcher, Name, Response, Timeout, TransId, Pid)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         is_binary(TransId), is_pid(Pid), Timeout > 0 ->
    Dispatcher ! {'return_sync', Name, Response, Timeout, TransId, Pid},
    erlang:throw(return).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([Module, Args, Dispatcher]) ->
    case Module:cloudi_job_init(Args, Dispatcher) of
        {ok, JobState} ->
            {ok, #state{module = Module,
                        dispatcher = Dispatcher,
                        job_state = JobState}};
        {stop, _} = Stop ->
            Stop
    end.

handle_call(Request, _, State) ->
    ?LOG_WARNING("Unknown call \"~p\"", [Request]),
    {stop, string2:format("Unknown call \"~p\"", [Request]), error, State}.

handle_cast(Request, State) ->
    ?LOG_WARNING("Unknown cast \"~p\"", [Request]),
    {noreply, State}.

handle_info({'send_async', Name, Request, Timeout, TransId, Pid},
            #state{module = Module,
                   dispatcher = Dispatcher,
                   job_state = JobState} = State) ->
    try Module:cloudi_job_handle_request('send_async', Name, Request, Timeout,
                                         TransId, Pid, JobState, Dispatcher) of
        {reply, Response, NewJobState} ->
            Pid ! {'return_async', Name, Response, Timeout, TransId, Pid},
            {noreply, State#state{job_state = NewJobState}};
        {noreply, NewJobState} ->
            {noreply, State#state{job_state = NewJobState}}
    catch
        throw:return ->
            {noreply, State};
        throw:forward ->
            {noreply, State}
    end;

handle_info({'send_sync', Name, Request, Timeout, TransId, Pid},
            #state{module = Module,
                   dispatcher = Dispatcher,
                   job_state = JobState} = State) ->
    try Module:cloudi_job_handle_request('send_sync', Name, Request, Timeout,
                                         TransId, Pid, JobState, Dispatcher) of
        {reply, Response, NewJobState} ->
            Pid ! {'return_sync', Name, Response, Timeout, TransId, Pid},
            {noreply, State#state{job_state = NewJobState}};
        {noreply, NewJobState} ->
            {noreply, State#state{job_state = NewJobState}}
    catch
        throw:return ->
            {noreply, State};
        throw:forward ->
            {noreply, State}
    end;

handle_info(Request,
            #state{module = Module,
                   dispatcher = Dispatcher,
                   job_state = JobState} = State) ->
    {noreply, NewJobState} =  Module:cloudi_job_handle_info(Request, 
                                                            JobState,
                                                            Dispatcher),
    {noreply, State#state{job_state = NewJobState}}.

terminate(Reason,
          #state{module = Module,
                 job_state = JobState}) ->
    Module:cloudi_job_terminate(Reason, JobState),
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

