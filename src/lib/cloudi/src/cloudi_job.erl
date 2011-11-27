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
         request_info_http_headers_parse/1]).

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
    gen_server:call(Dispatcher, {'get_pid', Name,
                                 Timeout - ?TIMEOUT_DELTA}, Timeout).

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
    gen_server:call(Dispatcher, {'send_async', Name, <<>>, Request,
                                 Timeout - ?TIMEOUT_DELTA,
                                 ?PRIORITY_DEFAULT}, Timeout).

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
    gen_server:call(Dispatcher, {'send_async', Name, <<>>, Request,
                                 Timeout - ?TIMEOUT_DELTA,
                                 ?PRIORITY_DEFAULT, Pid}, Timeout).

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
    gen_server:call(Dispatcher, {'send_async', Name,
                                 RequestInfo, Request,
                                 Timeout - ?TIMEOUT_DELTA,
                                 Priority}, Timeout).

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
    gen_server:call(Dispatcher, {'send_async', Name,
                                 RequestInfo, Request,
                                 Timeout - ?TIMEOUT_DELTA,
                                 Priority, Pid}, Timeout).

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
    gen_server:call(Dispatcher, {'send_async_active', Name, <<>>, Request,
                                 Timeout - ?TIMEOUT_DELTA,
                                 ?PRIORITY_DEFAULT}, Timeout).

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
    gen_server:call(Dispatcher, {'send_async_active', Name, <<>>, Request,
                                 Timeout - ?TIMEOUT_DELTA,
                                 ?PRIORITY_DEFAULT, Pid}, Timeout).

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
    gen_server:call(Dispatcher, {'send_async_active', Name,
                                 RequestInfo, Request,
                                 Timeout - ?TIMEOUT_DELTA,
                                 Priority}, Timeout).

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
    gen_server:call(Dispatcher, {'send_async_active', Name,
                                 RequestInfo, Request,
                                 Timeout - ?TIMEOUT_DELTA,
                                 Priority, Pid}, Timeout).

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
    gen_server:call(Dispatcher, {'send_sync', Name, <<>>, Request,
                                 Timeout - ?TIMEOUT_DELTA,
                                 ?PRIORITY_DEFAULT}, Timeout).

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
    gen_server:call(Dispatcher, {'send_sync', Name, <<>>, Request,
                                 Timeout - ?TIMEOUT_DELTA,
                                 ?PRIORITY_DEFAULT, Pid}, Timeout).

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
    gen_server:call(Dispatcher, {'send_sync', Name,
                                 RequestInfo, Request,
                                 Timeout - ?TIMEOUT_DELTA,
                                 Priority}, Timeout).

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
    gen_server:call(Dispatcher, {'send_sync', Name,
                                 RequestInfo, Request,
                                 Timeout - ?TIMEOUT_DELTA,
                                 Priority, Pid}, Timeout).

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
    gen_server:call(Dispatcher, {'mcast_async', Name, <<>>, Request,
                                 Timeout - ?TIMEOUT_DELTA,
                                 ?PRIORITY_DEFAULT}, Timeout).

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
    gen_server:call(Dispatcher, {'mcast_async', Name, <<>>, Request,
                                 Timeout - ?TIMEOUT_DELTA,
                                 ?PRIORITY_DEFAULT, Pid}, Timeout).

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
    gen_server:call(Dispatcher, {'mcast_async', Name,
                                 RequestInfo, Request,
                                 Timeout - ?TIMEOUT_DELTA,
                                 Priority}, Timeout).

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
    gen_server:call(Dispatcher, {'recv_async', Timeout - ?TIMEOUT_DELTA,
                                 TransId}, Timeout).

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
    request_http_qs_parse_list(dict:new(),
                               binary:split(Request, <<0>>, [global])).

-spec request_info_http_headers_parse(RequestInfo :: binary() | list()) ->
    dict().

request_info_http_headers_parse(RequestInfo)
    when is_list(RequestInfo) ->
    % atom() -> string()
    lists:foldl(fun({K, V}, D) ->
        dict:store(K, V, D)
    end, dict:new(), RequestInfo);
request_info_http_headers_parse(RequestInfo)
    when is_binary(RequestInfo) ->
    % binary() -> binary()
    request_info_http_headers_parse_list(dict:new(),
                                         binary:split(RequestInfo, <<0>>,
                                                      [global])).

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
                   job_state = JobState} = State) ->
    try Module:cloudi_job_handle_request('send_async', Name,
                                         RequestInfo, Request,
                                         Timeout, Priority,
                                         TransId, Pid, JobState, Dispatcher) of
        {reply, Response, NewJobState} ->
            Pid ! {'return_async', Name, <<>>, Response,
                   Timeout, TransId, Pid},
            {noreply, State#state{job_state = NewJobState}};
        {reply, ResponseInfo, Response, NewJobState} ->
            Pid ! {'return_async', Name, ResponseInfo, Response,
                   Timeout, TransId, Pid},
            {noreply, State#state{job_state = NewJobState}};
        {noreply, NewJobState} ->
            {noreply, State#state{job_state = NewJobState}}
    catch
        throw:return ->
            {noreply, State};
        throw:forward ->
            {noreply, State};
        Type:Error ->
            Stack = erlang:get_stacktrace(),
            ?LOG_ERROR("~p ~p~n~p", [Type, Error, Stack]),
            {stop, {Type, {Error, Stack}}, State}
    end;

handle_info({'send_sync', Name, RequestInfo, Request,
             Timeout, Priority, TransId, Pid},
            #state{module = Module,
                   dispatcher = Dispatcher,
                   job_state = JobState} = State) ->
    try Module:cloudi_job_handle_request('send_sync', Name,
                                         RequestInfo, Request,
                                         Timeout, Priority,
                                         TransId, Pid, JobState, Dispatcher) of
        {reply, Response, NewJobState} ->
            Pid ! {'return_sync', Name, <<>>, Response,
                   Timeout, TransId, Pid},
            {noreply, State#state{job_state = NewJobState}};
        {reply, ResponseInfo, Response, NewJobState} ->
            Pid ! {'return_sync', Name, ResponseInfo, Response,
                   Timeout, TransId, Pid},
            {noreply, State#state{job_state = NewJobState}};
        {noreply, NewJobState} ->
            {noreply, State#state{job_state = NewJobState}}
    catch
        throw:return ->
            {noreply, State};
        throw:forward ->
            {noreply, State};
        Type:Error ->
            Stack = erlang:get_stacktrace(),
            ?LOG_ERROR("~p ~p~n~p", [Type, Error, Stack]),
            {stop, {Type, {Error, Stack}}, State}
    end;

handle_info({'EXIT', _, Reason}, State) ->
    % make sure the terminate function is called if the dispatcher dies
    {stop, Reason, State};

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

request_http_qs_parse_list(Lookup, [<<>>]) ->
    Lookup;
request_http_qs_parse_list(Lookup, [K, V | L]) ->
    request_http_qs_parse_list(dict:store(K, V, Lookup), L).

request_info_http_headers_parse_list(Lookup, [<<>>]) ->
    Lookup;
request_info_http_headers_parse_list(Lookup, [K, V | L]) ->
    request_info_http_headers_parse_list(dict:store(K, V, Lookup), L).

