%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Job Behavior==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2011-2012, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2011-2012 Michael Truog
%%% @version 0.2.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_job).
-author('mjtruog [at] gmail (dot) com').

-behaviour(gen_server).

%% dispatcher interface
-export([start_link/4]).

%% behavior interface
-export([subscribe/2,
         unsubscribe/2,
         timeout_async/1,
         timeout_sync/1,
         get_pid/2,
         get_pid/3,
         send_async/3,
         send_async/4,
         send_async/5,
         send_async/6,
         send_async/7,
         send_async_active/3,
         send_async_active/4,
         send_async_active/5,
         send_async_active/6,
         send_async_active/7,
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
         mcast_async/5,
         mcast_async/6,
         forward/9,
         forward_async/8,
         forward_sync/8,
         recv_async/3,
         return/8,
         return_async/7,
         return_sync/7,
         return_nothrow/8,
         request_http_qs_parse/1,
         request_info_key_value_parse/1]).

%% behavior callbacks
-export([behaviour_info/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% internal interface
-export([handle_request/12]).

-include("cloudi_logger.hrl").
-include("cloudi_constants.hrl").

-record(state,
    {
        module,                        % job module
        dispatcher,                    % dispatcher pid
        job_state,                     % job state
        recv_timeouts = dict:new(),    % tracking for recv timeouts
        queue_messages = false,        % is the request pid busy?
        queued = pqueue4:new(),        % queued incoming messages
        queued_info = queue:new(),     % queue process messages for job
        request = undefined            % request pid
    }).

-define(CATCH_TIMEOUT(F),
        try F catch exit:{timeout, _} -> {error, timeout} end).

%%%------------------------------------------------------------------------
%%% Callback functions from behavior
%%%------------------------------------------------------------------------

-spec behaviour_info(atom()) -> 'undefined' | [{atom(), byte()}].

behaviour_info(callbacks) ->
    [
        {cloudi_job_init, 3},
        {cloudi_job_handle_request, 10},
        {cloudi_job_handle_info, 3},
        {cloudi_job_terminate, 2}
    ];
behaviour_info(_) ->
    undefined.

%%%------------------------------------------------------------------------
%%% Dispatcher interface functions
%%%------------------------------------------------------------------------

start_link(Module, Args, Prefix, Timeout)
    when is_atom(Module), is_list(Args), is_integer(Timeout) ->
    gen_server:start_link(?MODULE, [Module, Args, Prefix, self()],
                          [{timeout, Timeout}]).

%%%------------------------------------------------------------------------
%%% Behavior interface functions
%%%------------------------------------------------------------------------

-spec subscribe(Dispatcher :: pid(),
                Name :: string()) -> ok.

subscribe(Dispatcher, Name)
    when is_pid(Dispatcher), is_list(Name) ->
    gen_server:cast(Dispatcher, {'subscribe', Name}).

-spec unsubscribe(Dispatcher :: pid(),
                  Name :: string()) -> ok.

unsubscribe(Dispatcher, Name)
    when is_pid(Dispatcher), is_list(Name) ->
    gen_server:cast(Dispatcher, {'unsubscribe', Name}).

-spec timeout_async(Dispatcher :: pid()) -> pos_integer().

timeout_async(Dispatcher) ->
    gen_server:call(Dispatcher, timeout_async, infinity).

-spec timeout_sync(Dispatcher :: pid()) -> pos_integer().

timeout_sync(Dispatcher) ->
    gen_server:call(Dispatcher, timeout_sync, infinity).

-spec get_pid(Dispatcher :: pid(),
              Name :: string()) ->
    {'ok', pid()} |
    {'error', atom()}.

get_pid(Dispatcher, Name)
    when is_pid(Dispatcher), is_list(Name) ->
    gen_server:call(Dispatcher, {'get_pid', Name}, infinity).

-spec get_pid(Dispatcher :: pid(),
              Name :: string(),
              Timeout :: pos_integer()) ->
    {'ok', pid()} |
    {'error', atom()}.

get_pid(Dispatcher, Name, Timeout)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         Timeout > ?TIMEOUT_DELTA ->
    ?CATCH_TIMEOUT(gen_server:call(Dispatcher,
                                   {'get_pid', Name,
                                    Timeout - ?TIMEOUT_DELTA}, Timeout)).

-spec send_async(Dispatcher :: pid(),
                 Name :: string(),
                 Request :: any()) ->
    {'ok', binary()} |
    {'error', atom()}.

send_async(Dispatcher, Name, Request)
    when is_pid(Dispatcher), is_list(Name) ->
    gen_server:call(Dispatcher, {'send_async', Name, <<>>, Request,
                                 undefined,
                                 ?PRIORITY_DEFAULT}, infinity).

-spec send_async(Dispatcher :: pid(),
                 Name :: string(),
                 Request :: any(),
                 Timeout :: pos_integer() | 'undefined') ->
    {'ok', binary()} |
    {'error', atom()}.

send_async(Dispatcher, Name, Request, undefined)
    when is_pid(Dispatcher), is_list(Name) ->
    gen_server:call(Dispatcher, {'send_async', Name, <<>>, Request,
                                 undefined,
                                 ?PRIORITY_DEFAULT}, infinity);

send_async(Dispatcher, Name, Request, Timeout)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         Timeout > ?TIMEOUT_DELTA ->
    ?CATCH_TIMEOUT(gen_server:call(Dispatcher,
                                   {'send_async', Name, <<>>, Request,
                                    Timeout - ?TIMEOUT_DELTA,
                                    ?PRIORITY_DEFAULT}, Timeout)).

-spec send_async(Dispatcher :: pid(),
                 Name :: string(),
                 Request :: any(),
                 Timeout :: pos_integer() | 'undefined',
                 Pid :: pid()) ->
    {'ok', binary()} |
    {'error', atom()}.

send_async(Dispatcher, Name, Request, undefined, Pid)
    when is_pid(Dispatcher), is_list(Name), is_pid(Pid) ->
    gen_server:call(Dispatcher, {'send_async', Name, <<>>, Request,
                                 undefined,
                                 ?PRIORITY_DEFAULT, Pid}, infinity);

send_async(Dispatcher, Name, Request, Timeout, Pid)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         Timeout > ?TIMEOUT_DELTA, is_pid(Pid) ->
    ?CATCH_TIMEOUT(gen_server:call(Dispatcher,
                                   {'send_async', Name, <<>>, Request,
                                    Timeout - ?TIMEOUT_DELTA,
                                    ?PRIORITY_DEFAULT, Pid}, Timeout)).

-spec send_async(Dispatcher :: pid(),
                 Name :: string(),
                 RequestInfo :: any(),
                 Request :: any(),
                 Timeout :: pos_integer() | 'undefined',
                 Priority :: integer()) ->
    {'ok', binary()} |
    {'error', atom()}.

send_async(Dispatcher, Name, RequestInfo, Request,
           undefined, Priority)
    when is_pid(Dispatcher), is_list(Name), is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    gen_server:call(Dispatcher, {'send_async', Name,
                                 RequestInfo, Request,
                                 undefined,
                                 Priority}, infinity);

send_async(Dispatcher, Name, RequestInfo, Request,
           Timeout, Priority)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         Timeout > ?TIMEOUT_DELTA, is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    ?CATCH_TIMEOUT(gen_server:call(Dispatcher,
                                   {'send_async', Name,
                                    RequestInfo, Request,
                                    Timeout - ?TIMEOUT_DELTA,
                                    Priority}, Timeout)).

-spec send_async(Dispatcher :: pid(),
                 Name :: string(),
                 RequestInfo :: any(),
                 Request :: any(),
                 Timeout :: pos_integer() | 'undefined',
                 Priority :: integer(),
                 Pid :: pid()) ->
    {'ok', binary()} |
    {'error', atom()}.

send_async(Dispatcher, Name, RequestInfo, Request,
           undefined, Priority, Pid)
    when is_pid(Dispatcher), is_list(Name), is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW, is_pid(Pid) ->
    gen_server:call(Dispatcher, {'send_async', Name,
                                 RequestInfo, Request,
                                 undefined,
                                 Priority, Pid}, infinity);

send_async(Dispatcher, Name, RequestInfo, Request,
           Timeout, Priority, Pid)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         Timeout > ?TIMEOUT_DELTA, is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW, is_pid(Pid) ->
    ?CATCH_TIMEOUT(gen_server:call(Dispatcher,
                                   {'send_async', Name,
                                    RequestInfo, Request,
                                    Timeout - ?TIMEOUT_DELTA,
                                    Priority, Pid}, Timeout)).

-spec send_async_active(Dispatcher :: pid(),
                        Name :: string(),
                        Request :: any()) ->
    {'ok', binary()} |
    {'error', atom()}.

send_async_active(Dispatcher, Name, Request)
    when is_pid(Dispatcher), is_list(Name) ->
    gen_server:call(Dispatcher, {'send_async_active', Name, <<>>, Request,
                                 undefined,
                                 ?PRIORITY_DEFAULT}, infinity).

-spec send_async_active(Dispatcher :: pid(),
                        Name :: string(),
                        Request :: any(),
                        Timeout :: pos_integer() | 'undefined') ->
    {'ok', binary()} |
    {'error', atom()}.

send_async_active(Dispatcher, Name, Request, undefined)
    when is_pid(Dispatcher), is_list(Name) ->
    gen_server:call(Dispatcher, {'send_async_active', Name, <<>>, Request,
                                 undefined,
                                 ?PRIORITY_DEFAULT}, infinity);

send_async_active(Dispatcher, Name, Request, Timeout)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         Timeout > ?TIMEOUT_DELTA ->
    ?CATCH_TIMEOUT(gen_server:call(Dispatcher,
                                   {'send_async_active', Name, <<>>, Request,
                                    Timeout - ?TIMEOUT_DELTA,
                                    ?PRIORITY_DEFAULT}, Timeout)).

-spec send_async_active(Dispatcher :: pid(),
                        Name :: string(),
                        Request :: any(),
                        Timeout :: pos_integer() | 'undefined',
                        Pid :: pid()) ->
    {'ok', binary()} |
    {'error', atom()}.

send_async_active(Dispatcher, Name, Request, undefined, Pid)
    when is_pid(Dispatcher), is_list(Name), is_pid(Pid) ->
    gen_server:call(Dispatcher, {'send_async_active', Name, <<>>, Request,
                                 undefined,
                                 ?PRIORITY_DEFAULT, Pid}, infinity);

send_async_active(Dispatcher, Name, Request, Timeout, Pid)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         Timeout > ?TIMEOUT_DELTA, is_pid(Pid) ->
    ?CATCH_TIMEOUT(gen_server:call(Dispatcher,
                                   {'send_async_active', Name, <<>>, Request,
                                    Timeout - ?TIMEOUT_DELTA,
                                    ?PRIORITY_DEFAULT, Pid}, Timeout)).

-spec send_async_active(Dispatcher :: pid(),
                        Name :: string(),
                        RequestInfo :: any(),
                        Request :: any(),
                        Timeout :: pos_integer() | 'undefined',
                        Priority :: integer()) ->
    {'ok', binary()} |
    {'error', atom()}.

send_async_active(Dispatcher, Name, RequestInfo, Request,
                  undefined, Priority)
    when is_pid(Dispatcher), is_list(Name), is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    gen_server:call(Dispatcher, {'send_async_active', Name,
                                 RequestInfo, Request,
                                 undefined,
                                 Priority}, infinity);

send_async_active(Dispatcher, Name, RequestInfo, Request,
                  Timeout, Priority)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         Timeout > ?TIMEOUT_DELTA, is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    ?CATCH_TIMEOUT(gen_server:call(Dispatcher,
                                   {'send_async_active', Name,
                                    RequestInfo, Request,
                                    Timeout - ?TIMEOUT_DELTA,
                                    Priority}, Timeout)).

-spec send_async_active(Dispatcher :: pid(),
                        Name :: string(),
                        RequestInfo :: any(),
                        Request :: any(),
                        Timeout :: pos_integer() | 'undefined',
                        Priority :: integer(),
                        Pid :: pid()) ->
    {'ok', binary()} |
    {'error', atom()}.

send_async_active(Dispatcher, Name, RequestInfo, Request,
                  undefined, Priority, Pid)
    when is_pid(Dispatcher), is_list(Name), is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW, is_pid(Pid) ->
    gen_server:call(Dispatcher, {'send_async_active', Name,
                                 RequestInfo, Request,
                                 undefined,
                                 Priority, Pid}, infinity);

send_async_active(Dispatcher, Name, RequestInfo, Request,
                  Timeout, Priority, Pid)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         Timeout > ?TIMEOUT_DELTA, is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW, is_pid(Pid) ->
    ?CATCH_TIMEOUT(gen_server:call(Dispatcher,
                                   {'send_async_active', Name,
                                    RequestInfo, Request,
                                    Timeout - ?TIMEOUT_DELTA,
                                    Priority, Pid}, Timeout)).

-spec send_async_passive(Dispatcher :: pid(),
                         Name :: string(),
                         Request :: any()) ->
    {'ok', binary()} |
    {'error', atom()}.

send_async_passive(Dispatcher, Name, Request) ->
    send_async(Dispatcher, Name, Request).

-spec send_async_passive(Dispatcher :: pid(),
                         Name :: string(),
                         Request :: any(),
                         Timeout :: pos_integer() | 'undefined') ->
    {'ok', binary()} |
    {'error', atom()}.

send_async_passive(Dispatcher, Name, Request, Timeout) ->
    send_async(Dispatcher, Name, Request, Timeout).

-spec send_async_passive(Dispatcher :: pid(),
                         Name :: string(),
                         Request :: any(),
                         Timeout :: pos_integer() | 'undefined',
                         Pid :: pid()) ->
    {'ok', binary()} |
    {'error', atom()}.

send_async_passive(Dispatcher, Name, Request, Timeout, Pid) ->
    send_async(Dispatcher, Name, Request, Timeout, Pid).

-spec send_async_passive(Dispatcher :: pid(),
                         Name :: string(),
                         RequestInfo :: any(),
                         Request :: any(),
                         Timeout :: pos_integer() | 'undefined',
                         Priority :: integer()) ->
    {'ok', binary()} |
    {'error', atom()}.

send_async_passive(Dispatcher, Name, RequestInfo, Request,
                   Timeout, Priority) ->
    send_async(Dispatcher, Name, RequestInfo, Request,
               Timeout, Priority).

-spec send_async_passive(Dispatcher :: pid(),
                         Name :: string(),
                         RequestInfo :: any(),
                         Request :: any(),
                         Timeout :: pos_integer() | 'undefined',
                         Priority :: integer(),
                         Pid :: pid()) ->
    {'ok', binary()} |
    {'error', atom()}.

send_async_passive(Dispatcher, Name, RequestInfo, Request,
                   Timeout, Priority, Pid) ->
    send_async(Dispatcher, Name, RequestInfo, Request,
               Timeout, Priority, Pid).

-spec send_sync(Dispatcher :: pid(),
                Name :: string(),
                Request :: any()) ->
    {'ok', any(), any()} |
    {'ok', any()} |
    {'error', atom()}.

send_sync(Dispatcher, Name, Request)
    when is_pid(Dispatcher), is_list(Name) ->
    gen_server:call(Dispatcher, {'send_sync', Name, <<>>, Request,
                                 undefined,
                                 ?PRIORITY_DEFAULT}, infinity).

-spec send_sync(Dispatcher :: pid(),
                Name :: string(),
                Request :: any(),
                Timeout :: pos_integer() | 'undefined') ->
    {'ok', any(), any()} |
    {'ok', any()} |
    {'error', atom()}.

send_sync(Dispatcher, Name, Request, undefined)
    when is_pid(Dispatcher), is_list(Name) ->
    gen_server:call(Dispatcher, {'send_sync', Name, <<>>, Request,
                                 undefined,
                                 ?PRIORITY_DEFAULT}, infinity);

send_sync(Dispatcher, Name, Request, Timeout)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         Timeout > ?TIMEOUT_DELTA ->
    ?CATCH_TIMEOUT(gen_server:call(Dispatcher,
                                   {'send_sync', Name, <<>>, Request,
                                    Timeout - ?TIMEOUT_DELTA,
                                    ?PRIORITY_DEFAULT}, Timeout)).

-spec send_sync(Dispatcher :: pid(),
                Name :: string(),
                Request :: any(),
                Timeout :: pos_integer() | 'undefined',
                Pid :: pid()) ->
    {'ok', any(), any()} |
    {'ok', any()} |
    {'error', atom()}.

send_sync(Dispatcher, Name, Request, undefined, Pid)
    when is_pid(Dispatcher), is_list(Name), is_pid(Pid) ->
    gen_server:call(Dispatcher, {'send_sync', Name, <<>>, Request,
                                 undefined,
                                 ?PRIORITY_DEFAULT, Pid}, infinity);

send_sync(Dispatcher, Name, Request, Timeout, Pid)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         Timeout > ?TIMEOUT_DELTA, is_pid(Pid) ->
    ?CATCH_TIMEOUT(gen_server:call(Dispatcher,
                                   {'send_sync', Name, <<>>, Request,
                                    Timeout - ?TIMEOUT_DELTA,
                                    ?PRIORITY_DEFAULT, Pid}, Timeout)).

-spec send_sync(Dispatcher :: pid(),
                Name :: string(),
                RequestInfo :: any(),
                Request :: any(),
                Timeout :: pos_integer() | 'undefined', 
                Priority :: integer()) ->
    {'ok', any(), any()} |
    {'ok', any()} |
    {'error', atom()}.

send_sync(Dispatcher, Name, RequestInfo, Request,
          undefined, Priority)
    when is_pid(Dispatcher), is_list(Name), is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    gen_server:call(Dispatcher, {'send_sync', Name,
                                 RequestInfo, Request,
                                 undefined,
                                 Priority}, infinity);

send_sync(Dispatcher, Name, RequestInfo, Request,
          Timeout, Priority)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         Timeout > ?TIMEOUT_DELTA, is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    ?CATCH_TIMEOUT(gen_server:call(Dispatcher,
                                   {'send_sync', Name,
                                    RequestInfo, Request,
                                    Timeout - ?TIMEOUT_DELTA,
                                    Priority}, Timeout)).

-spec send_sync(Dispatcher :: pid(),
                Name :: string(),
                RequestInfo :: any(),
                Request :: any(),
                Timeout :: pos_integer() | 'undefined',
                Priority :: integer(),
                Pid :: pid()) ->
    {'ok', any(), any()} |
    {'ok', any()} |
    {'error', atom()}.

send_sync(Dispatcher, Name, RequestInfo, Request,
          undefined, Priority, Pid)
    when is_pid(Dispatcher), is_list(Name), is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW, is_pid(Pid) ->
    gen_server:call(Dispatcher, {'send_sync', Name,
                                 RequestInfo, Request,
                                 undefined,
                                 Priority, Pid}, infinity);

send_sync(Dispatcher, Name, RequestInfo, Request,
          Timeout, Priority, Pid)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         Timeout > ?TIMEOUT_DELTA, is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW, is_pid(Pid) ->
    ?CATCH_TIMEOUT(gen_server:call(Dispatcher,
                                   {'send_sync', Name,
                                    RequestInfo, Request,
                                    Timeout - ?TIMEOUT_DELTA,
                                    Priority, Pid}, Timeout)).

-spec mcast_async(Dispatcher :: pid(),
                  Name :: string(),
                  Request :: any()) ->
    {'ok', list(binary())} |
    {'error', atom()}.

mcast_async(Dispatcher, Name, Request)
    when is_pid(Dispatcher), is_list(Name) ->
    gen_server:call(Dispatcher, {'mcast_async', Name, <<>>, Request,
                                 undefined,
                                 ?PRIORITY_DEFAULT}, infinity).

-spec mcast_async(Dispatcher :: pid(),
                  Name :: string(),
                  Request :: any(),
                  Timeout :: pos_integer() | 'undefined') ->
    {'ok', list(binary())} |
    {'error', atom()}.

mcast_async(Dispatcher, Name, Request, undefined)
    when is_pid(Dispatcher), is_list(Name) ->
    gen_server:call(Dispatcher, {'mcast_async', Name, <<>>, Request,
                                 undefined,
                                 ?PRIORITY_DEFAULT}, infinity);

mcast_async(Dispatcher, Name, Request, Timeout)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         Timeout > ?TIMEOUT_DELTA ->
    ?CATCH_TIMEOUT(gen_server:call(Dispatcher,
                                   {'mcast_async', Name, <<>>, Request,
                                    Timeout - ?TIMEOUT_DELTA,
                                    ?PRIORITY_DEFAULT}, Timeout)).

-spec mcast_async(Dispatcher :: pid(),
                  Name :: string(),
                  Request :: any(),
                  Timeout :: pos_integer() | 'undefined',
                  Pid :: pid()) ->
    {'ok', list(binary())} |
    {'error', atom()}.

mcast_async(Dispatcher, Name, Request, undefined, Pid)
    when is_pid(Dispatcher), is_list(Name), is_pid(Pid) ->
    gen_server:call(Dispatcher, {'mcast_async', Name, <<>>, Request,
                                 undefined,
                                 ?PRIORITY_DEFAULT, Pid}, infinity);

mcast_async(Dispatcher, Name, Request, Timeout, Pid)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         Timeout > ?TIMEOUT_DELTA, is_pid(Pid) ->
    ?CATCH_TIMEOUT(gen_server:call(Dispatcher,
                                   {'mcast_async', Name, <<>>, Request,
                                    Timeout - ?TIMEOUT_DELTA,
                                    ?PRIORITY_DEFAULT, Pid}, Timeout)).

-spec mcast_async(Dispatcher :: pid(),
                  Name :: string(),
                  RequestInfo :: any(),
                  Request :: any(),
                  Timeout :: pos_integer() | 'undefined',
                  Priority :: integer()) ->
    {'ok', list(binary())} |
    {'error', atom()}.

mcast_async(Dispatcher, Name, RequestInfo, Request, undefined, Priority)
    when is_pid(Dispatcher), is_list(Name), is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    gen_server:call(Dispatcher, {'mcast_async', Name,
                                 RequestInfo, Request,
                                 undefined,
                                 Priority}, infinity);

mcast_async(Dispatcher, Name, RequestInfo, Request, Timeout, Priority)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         Timeout > ?TIMEOUT_DELTA, is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    ?CATCH_TIMEOUT(gen_server:call(Dispatcher,
                                   {'mcast_async', Name,
                                    RequestInfo, Request,
                                    Timeout - ?TIMEOUT_DELTA,
                                    Priority}, Timeout)).

-spec forward(Dispatcher :: pid(),
              'send_async' | 'send_sync',
              Name :: string(),
              RequestInfo :: any(),
              Request :: any(),
              Timeout :: pos_integer(),
              Priority :: integer(),
              TransId :: binary(),
              Pid :: pid()) -> none().

forward(Dispatcher, 'send_async', Name, RequestInfo, Request,
        Timeout, Priority, TransId, Pid) ->
    forward_async(Dispatcher, Name, RequestInfo, Request,
                  Timeout, Priority, TransId, Pid),
    erlang:throw(forward);

forward(Dispatcher, 'send_sync', Name, RequestInfo, Request,
        Timeout, Priority, TransId, Pid) ->
    forward_sync(Dispatcher, Name, RequestInfo, Request,
                 Timeout, Priority, TransId, Pid),
    erlang:throw(forward).

-spec forward_async(Dispatcher :: pid(),
                    Name :: string(),
                    RequestInfo :: any(),
                    Request :: any(),
                    Timeout :: pos_integer(),
                    Priority :: integer(),
                    TransId :: binary(),
                    Pid :: pid()) -> none().

forward_async(Dispatcher, Name, RequestInfo, Request,
              Timeout, Priority, TransId, Pid)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         is_binary(TransId), is_pid(Pid), Timeout > 0, is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    Dispatcher ! {'forward_async', Name, RequestInfo, Request,
                  Timeout, Priority, TransId, Pid},
    erlang:throw(forward).

-spec forward_sync(Dispatcher :: pid(),
                   Name :: string(),
                   RequestInfo :: any(),
                   Request :: any(),
                   Timeout :: pos_integer(),
                   Priority :: integer(),
                   TransId :: binary(),
                   Pid :: pid()) -> none().

forward_sync(Dispatcher, Name, RequestInfo, Request,
             Timeout, Priority, TransId, Pid)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         is_binary(TransId), is_pid(Pid), Timeout > 0, is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    Dispatcher ! {'forward_sync', Name, RequestInfo, Request,
                  Timeout, Priority, TransId, Pid},
    erlang:throw(forward).

-spec recv_async(Dispatcher :: pid(),
                 Timeout :: pos_integer(),
                 TransId :: binary()) ->
    {'ok', any(), any()} |
    {'ok', any()} |
    {'error', atom()}.

recv_async(Dispatcher, Timeout, TransId)
    when is_pid(Dispatcher), is_integer(Timeout), is_binary(TransId),
         Timeout > ?TIMEOUT_DELTA ->
    ?CATCH_TIMEOUT(gen_server:call(Dispatcher,
                                   {'recv_async', Timeout - ?TIMEOUT_DELTA,
                                    TransId}, Timeout)).

-spec return(Dispatcher :: pid(),
             'send_async' | 'send_sync',
             Name :: string(),
             ResponseInfo :: any(),
             Response :: any(),
             Timeout :: pos_integer(),
             TransId :: binary(),
             Pid :: pid()) -> none().

return(Dispatcher, Type, Name, ResponseInfo, Response,
       Timeout, TransId, Pid) ->
    return_nothrow(Dispatcher, Type, Name, ResponseInfo, Response,
                   Timeout, TransId, Pid),
    erlang:throw(return).

-spec return_async(Dispatcher :: pid(),
                   Name :: string(),
                   ResponseInfo :: any(),
                   Response :: any(),
                   Timeout :: pos_integer(),
                   TransId :: binary(),
                   Pid :: pid()) -> none().

return_async(Dispatcher, Name, ResponseInfo, Response,
             Timeout, TransId, Pid)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         is_binary(TransId), is_pid(Pid), Timeout > 0 ->
    Pid ! {'return_async', Name, ResponseInfo, Response,
           Timeout, TransId, Pid},
    erlang:throw(return).

-spec return_sync(Dispatcher :: pid(),
                  Name :: string(),
                  ResponseInfo :: any(),
                  Response :: any(),
                  Timeout :: pos_integer(),
                  TransId :: binary(),
                  Pid :: pid()) -> none().

return_sync(Dispatcher, Name, ResponseInfo, Response,
            Timeout, TransId, Pid)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         is_binary(TransId), is_pid(Pid), Timeout > 0 ->
    Pid ! {'return_sync', Name, ResponseInfo, Response,
           Timeout, TransId, Pid},
    erlang:throw(return).

-spec return_nothrow(Dispatcher :: pid(),
                     'send_async' | 'send_sync',
                     Name :: string(),
                     ResponseInfo :: any(),
                     Response :: any(),
                     Timeout :: pos_integer(),
                     TransId :: binary(),
                     Pid :: pid()) -> 'ok'.

return_nothrow(_, 'send_async', Name, ResponseInfo, Response,
               Timeout, TransId, Pid) ->
    Pid ! {'return_async', Name, ResponseInfo, Response,
           Timeout, TransId, Pid},
    ok;

return_nothrow(_, 'send_sync', Name, ResponseInfo, Response,
               Timeout, TransId, Pid) ->
    Pid ! {'return_sync', Name, ResponseInfo, Response,
           Timeout, TransId, Pid},
    ok.

-spec request_http_qs_parse(Request :: binary()) ->
    dict().

request_http_qs_parse(Request)
    when is_binary(Request) ->
    binary_key_value_parse_list(dict:new(),
                                binary:split(Request, <<0>>, [global])).

-spec request_info_key_value_parse(RequestInfo :: binary() | list()) ->
    dict().

request_info_key_value_parse(RequestInfo)
    when is_list(RequestInfo) ->
    % atom() -> string()
    lists:foldl(fun({K, V}, D) ->
        dict:store(K, V, D)
    end, dict:new(), RequestInfo);
request_info_key_value_parse(RequestInfo)
    when is_binary(RequestInfo) ->
    % binary() -> binary()
    binary_key_value_parse_list(dict:new(),
                                binary:split(RequestInfo, <<0>>, [global])).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([Module, Args, Prefix, Dispatcher]) ->
    case Module:cloudi_job_init(Args, Prefix, Dispatcher) of
        {ok, JobState} ->
            erlang:process_flag(trap_exit, true),
            {ok, #state{module = Module,
                        dispatcher = Dispatcher,
                        job_state = JobState}};
        {stop, _} = Stop ->
            Stop
    end.

handle_call(Request, _, State) ->
    ?LOG_WARN("Unknown call \"~p\"", [Request]),
    {stop, string2:format("Unknown call \"~p\"", [Request]), error, State}.

handle_cast(Request, State) ->
    ?LOG_WARN("Unknown cast \"~p\"", [Request]),
    {noreply, State}.

handle_info({'send_async', Name, RequestInfo, Request,
             Timeout, Priority, TransId, Pid},
            #state{module = Module,
                   dispatcher = Dispatcher,
                   job_state = JobState,
                   queue_messages = false} = State) ->
    RequestPid = erlang:spawn_link(?MODULE, handle_request,
                                   ['send_async', Name, RequestInfo, Request,
                                    Timeout, Priority, TransId, Pid,
                                    Module, Dispatcher, JobState, self()]),
    {noreply, State#state{queue_messages = true,
                          request = RequestPid}};

handle_info({'send_sync', Name, RequestInfo, Request,
             Timeout, Priority, TransId, Pid},
            #state{module = Module,
                   dispatcher = Dispatcher,
                   job_state = JobState,
                   queue_messages = false} = State) ->
    RequestPid = erlang:spawn_link(?MODULE, handle_request,
                                   ['send_sync', Name, RequestInfo, Request,
                                    Timeout, Priority, TransId, Pid,
                                    Module, Dispatcher, JobState, self()]),
    {noreply, State#state{queue_messages = true,
                          request = RequestPid}};

handle_info({_, _, _, _, Timeout, Priority, TransId, _} = T,
            #state{queue_messages = true} = State) ->
    {noreply, recv_timeout_start(Timeout, Priority, TransId, T, State)};

handle_info({cloudi_recv_timeout, Priority, TransId},
            #state{recv_timeouts = Ids,
                   queue_messages = QueueMessages,
                   queued = Queue} = State) ->
    NewQueue = if
        QueueMessages =:= true ->
            pqueue4:filter(fun({_, _, _, _, _, _, Id, _}) ->
                Id /= TransId
            end, Priority, Queue);
        true ->
            Queue
    end,
    {noreply, State#state{recv_timeouts = dict:erase(TransId, Ids),
                          queued = NewQueue}};

handle_info({'EXIT', _, cloudi_request_done}, State) ->
    {noreply, State};

handle_info({'EXIT', Pid, Reason}, State) ->
    % make sure the terminate function is called if the dispatcher dies
    % or any job related exits
    ?LOG_ERROR("~p exited: ~p", [Pid, Reason]),
    {stop, Reason, State};
            
handle_info({cloudi_request_success, Request, NewJobState},
            #state{dispatcher = Dispatcher} = State) ->
    case Request of
        undefined ->
            ok;
        {'return_async', _, _, _, _, _, Pid} = T ->
            Pid ! T;
        {'return_sync', _, _, _, _, _, Pid} = T ->
            Pid ! T;
        {'forward_async', _, _, _, _, _, _, _} = T ->
            Dispatcher ! T;
        {'forward_sync', _, _, _, _, _, _, _} = T ->
            Dispatcher ! T
    end,
    process_queue_info(process_queue(NewJobState, State));

handle_info({cloudi_request_failure, Type, Error, Stack}, State) ->
    ?LOG_ERROR("~p ~p~n~p", [Type, Error, Stack]),
    {stop, {Type, {Error, Stack}}, State};

handle_info(Request,
            #state{queue_messages = true,
                   queued_info = QueueInfo} = State) ->
    {noreply, State#state{queued_info = queue:in(Request, QueueInfo)}};

handle_info(Request,
            #state{module = Module,
                   dispatcher = Dispatcher,
                   job_state = JobState} = State) ->
    case Module:cloudi_job_handle_info(Request, JobState, Dispatcher) of
        {noreply, NewJobState} ->
            {noreply, State#state{job_state = NewJobState}};
        {stop, Reason, NewJobState} ->
            {stop, Reason, State#state{job_state = NewJobState}}
    end.

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

handle_request('send_async', Name, RequestInfo, Request,
               Timeout, Priority, TransId, Pid,
               Module, Dispatcher, JobState, Parent) ->
    try Module:cloudi_job_handle_request('send_async', Name,
                                         RequestInfo, Request,
                                         Timeout, Priority,
                                         TransId, Pid, JobState, Dispatcher) of
        {reply, Response, NewJobState} ->
            Parent ! {cloudi_request_success,
                      {'return_async', Name, <<>>, Response,
                       Timeout, TransId, Pid},
                      NewJobState};
        {reply, ResponseInfo, Response, NewJobState} ->
            Parent ! {cloudi_request_success,
                      {'return_async', Name, ResponseInfo, Response,
                       Timeout, TransId, Pid},
                      NewJobState};
        {forward, NextName, NextRequestInfo, NextRequest,
                  NextTimeout, NextPriority, NewJobState} ->
            Parent ! {cloudi_request_success,
                      {'forward_async', NextName,
                       NextRequestInfo, NextRequest,
                       NextTimeout, NextPriority, TransId, Pid},
                      NewJobState};
        {forward, NextName, NextRequestInfo, NextRequest,
                  NewJobState} ->
            Parent ! {cloudi_request_success,
                      {'forward_async', NextName,
                       NextRequestInfo, NextRequest,
                       Timeout, Priority, TransId, Pid},
                      NewJobState};
        {noreply, NewJobState} ->
            Parent ! {cloudi_request_success, undefined, NewJobState}
    catch
        throw:return ->
            Parent ! {cloudi_request_success, undefined, JobState};
        throw:forward ->
            Parent ! {cloudi_request_success, undefined, JobState};
        Type:Error ->
            Stack = erlang:get_stacktrace(),
            Parent ! {cloudi_request_failure, Type, Error, Stack}
    end,
    erlang:exit(self(), cloudi_request_done);

handle_request('send_sync', Name, RequestInfo, Request,
               Timeout, Priority, TransId, Pid,
               Module, Dispatcher, JobState, Parent) ->
    try Module:cloudi_job_handle_request('send_sync', Name,
                                         RequestInfo, Request,
                                         Timeout, Priority,
                                         TransId, Pid, JobState, Dispatcher) of
        {reply, Response, NewJobState} ->
            Parent ! {cloudi_request_success,
                      {'return_sync', Name, <<>>, Response,
                       Timeout, TransId, Pid},
                      NewJobState};
        {reply, ResponseInfo, Response, NewJobState} ->
            Parent ! {cloudi_request_success,
                      {'return_sync', Name, ResponseInfo, Response,
                       Timeout, TransId, Pid},
                      NewJobState};
        {forward, NextName, NextRequestInfo, NextRequest,
                  NextTimeout, NextPriority, NewJobState} ->
            Parent ! {cloudi_request_success,
                      {'forward_sync', NextName,
                       NextRequestInfo, NextRequest,
                       NextTimeout, NextPriority, TransId, Pid},
                      NewJobState};
        {forward, NextName, NextRequestInfo, NextRequest,
                  NewJobState} ->
            Parent ! {cloudi_request_success,
                      {'forward_sync', NextName,
                       NextRequestInfo, NextRequest,
                       Timeout, Priority, TransId, Pid},
                      NewJobState};
        {noreply, NewJobState} ->
            Parent ! {cloudi_request_success, undefined, NewJobState}
    catch
        throw:return ->
            Parent ! {cloudi_request_success, undefined, JobState};
        throw:forward ->
            Parent ! {cloudi_request_success, undefined, JobState};
        Type:Error ->
            Stack = erlang:get_stacktrace(),
            Parent ! {cloudi_request_failure, Type, Error, Stack}
    end,
    erlang:exit(self(), cloudi_request_done).

recv_timeout_start(Timeout, Priority, TransId, T,
                   #state{recv_timeouts = Ids,
                          queued = Queue} = State)
    when is_integer(Timeout), is_integer(Priority), is_binary(TransId) ->
    Tref = erlang:send_after(Timeout, self(),
                             {cloudi_recv_timeout, Priority, TransId}),
    State#state{recv_timeouts = dict:store(TransId, Tref, Ids),
                queued = pqueue4:in(T, Priority, Queue)}.

process_queue(NewJobState,
              #state{module = Module,
                     dispatcher = Dispatcher,
                     recv_timeouts = Ids,
                     queue_messages = true,
                     queued = Queue} = State) ->
    case pqueue4:out(Queue) of
        {empty, NewQueue} ->
            State#state{job_state = NewJobState,
                        queue_messages = false,
                        queued = NewQueue,
                        request = undefined};
        {{value, {'send_async', Name, RequestInfo, Request,
                  _, Priority, TransId, Pid}}, NewQueue} ->
            Tref = dict:fetch(TransId, Ids),
            Timeout = case erlang:cancel_timer(Tref) of
                false ->
                    1;
                V ->
                    V
            end,
            RequestPid = erlang:spawn_link(?MODULE, handle_request,
                                           ['send_async', Name,
                                            RequestInfo, Request,
                                            Timeout, Priority, TransId, Pid,
                                            Module, Dispatcher,
                                            NewJobState, self()]),
            State#state{job_state = NewJobState,
                        recv_timeouts = dict:erase(TransId, Ids),
                        queued = NewQueue,
                        request = RequestPid};
        {{value, {'send_sync', Name, RequestInfo, Request,
                  _, Priority, TransId, Pid}}, NewQueue} ->
            Tref = dict:fetch(TransId, Ids),
            Timeout = case erlang:cancel_timer(Tref) of
                false ->
                    1;
                V ->
                    V
            end,
            RequestPid = erlang:spawn_link(?MODULE, handle_request,
                                           ['send_sync', Name,
                                            RequestInfo, Request,
                                            Timeout, Priority, TransId, Pid,
                                            Module, Dispatcher,
                                            NewJobState, self()]),
            State#state{job_state = NewJobState,
                        recv_timeouts = dict:erase(TransId, Ids),
                        queued = NewQueue,
                        request = RequestPid}
    end.

process_queue_info(#state{module = Module,
                          dispatcher = Dispatcher,
                          job_state = JobState,
                          queued_info = QueueInfo} = State) ->
    process_queue_info(Module, Dispatcher, JobState, QueueInfo, State).

process_queue_info(Module, Dispatcher, JobState, QueueInfo, State) ->
    case queue:out(QueueInfo) of
        {empty, NewQueueInfo} ->
            {noreply, State#state{job_state = JobState,
                                  queued_info = NewQueueInfo}};
        {{value, Request}, NewQueueInfo} ->
            case Module:cloudi_job_handle_info(Request, JobState, Dispatcher) of
                {noreply, NewJobState} ->
                    process_queue_info(Module, Dispatcher,
                                       NewJobState, NewQueueInfo, State);
                {stop, Reason, NewJobState} ->
                    {stop, Reason,
                     State#state{job_state = NewJobState,
                                 queued_info = NewQueueInfo}}
            end
    end.

binary_key_value_parse_list(Lookup, [<<>>]) ->
    Lookup;
binary_key_value_parse_list(Lookup, [K, V | L]) ->
    case dict:find(K, Lookup) of
        {ok, [_ | _] = ListV} ->
            binary_key_value_parse_list(dict:store(K, ListV ++ [V], Lookup), L);
        {ok, V0} ->
            binary_key_value_parse_list(dict:store(K, [V0, V], Lookup), L);
        error ->
            binary_key_value_parse_list(dict:store(K, V, Lookup), L)
    end.

