%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI (Internal) Service Behavior==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2011-2013, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2011-2013 Michael Truog
%%% @version 1.2.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service).
-author('mjtruog [at] gmail (dot) com').

-behaviour(gen_server).

%% behavior interface
-export([process_index/1,
         self/1,
         subscribe/2,
         unsubscribe/2,
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
         return/9,
         return_async/8,
         return_sync/8,
         return_nothrow/9,
         recv_async/1,
         recv_async/2,
         recv_async/3,
         recv_async/4,
         prefix/1,
         timeout_async/1,
         timeout_sync/1,
         request_http_qs_parse/1,
         request_info_key_value_parse/1]).

-ifdef(ERLANG_OTP_VER_R14).
%% behavior callbacks
-export([behaviour_info/1]).
-endif.

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% internal interface
-export([handle_request/13]).

-include("cloudi_configuration.hrl").
-include("cloudi_logger.hrl").
-include("cloudi_constants.hrl").

-record(state,
    {
        module,                      % service module
        dispatcher,                  % dispatcher pid
        service_state,               % service state
        recv_timeouts = dict:new(),  % tracking for recv timeouts
        queue_messages = true,       % is the request pid busy?
        queued = pqueue4:new(),      % queued incoming messages
        queued_info = queue:new(),   % queue process messages for service
        request = undefined,         % request pid
        options                      % #config_service_options{} configuration
    }).

-define(CATCH_TIMEOUT(F),
        try F catch exit:{timeout, _} -> {error, timeout} end).

%%%------------------------------------------------------------------------
%%% Callback functions from behavior
%%%------------------------------------------------------------------------

-ifdef(ERLANG_OTP_VER_R14).

-spec behaviour_info(atom()) -> 'undefined' | [{atom(), byte()}].

behaviour_info(callbacks) ->
    [
        {cloudi_service_init, 3},
        {cloudi_service_handle_request, 11},
        {cloudi_service_handle_info, 3},
        {cloudi_service_terminate, 2}
    ];
behaviour_info(_) ->
    undefined.

-else. % Erlang version must be >= R15

-callback cloudi_service_init(Args :: list(),
                              Prefix :: string(),
                              Dispatcher :: pid()) ->
    {'ok', State :: any()} |
    {'stop', Reason :: any()}.

-callback cloudi_service_handle_request(Type :: 'send_async' | 'send_sync',
                                        Name :: string(),
                                        Pattern :: string(),
                                        RequestInfo :: any(),
                                        Request :: any(),
                                        Timeout :: non_neg_integer(),
                                        Priority :: integer(),
                                        TransId :: binary(),
                                        Pid :: pid(),
                                        State :: any(),
                                        Dispatcher :: pid()) ->
    {'reply', Response :: any(), NewState :: any()} |
    {'reply', ResponseInfo :: any(), Response :: any(), NewState :: any()} |
    {'forward', NextName :: string(),
     NextRequestInfo :: any(), NextRequest :: any(), NewState :: any()} |
    {'forward', NextName :: string(),
     NextRequestInfo :: any(), NextRequest :: any(),
     NextTimeout :: non_neg_integer(), NextPriority :: integer(),
     NewState :: any()} |
    {'noreply', NewState :: any()}.

-callback cloudi_service_handle_info(Request :: any(),
                                     State :: any(),
                                     Dispatcher :: pid()) ->
    {'noreply', NewState :: any()} |
    {'stop', Reason :: any(), NewState :: any()}.

-callback cloudi_service_terminate(Reason :: any(),
                                   State :: any()) ->
    'ok'.

-endif.

%%%------------------------------------------------------------------------
%%% Behavior interface functions
%%%------------------------------------------------------------------------

-spec process_index(Dispatcher :: pid()) -> pos_integer().

process_index(Dispatcher) ->
    gen_server:call(Dispatcher, process_index, infinity).

-spec self(Dispatcher :: pid()) -> pid().

self(Dispatcher) ->
    gen_server:call(Dispatcher, {self, self()}, infinity).

-spec subscribe(Dispatcher :: pid(),
                Pattern :: string()) -> ok.

subscribe(Dispatcher, Pattern)
    when is_pid(Dispatcher), is_list(Pattern) ->
    gen_server:call(Dispatcher, {'subscribe', Pattern}, infinity).

-spec unsubscribe(Dispatcher :: pid(),
                  Pattern :: string()) -> ok.

unsubscribe(Dispatcher, Pattern)
    when is_pid(Dispatcher), is_list(Pattern) ->
    gen_server:call(Dispatcher, {'unsubscribe', Pattern}, infinity).

-spec get_pid(Dispatcher :: pid(),
              Name :: string()) ->
    {'ok', {string(), pid()}} |
    {'error', atom()}.

get_pid(Dispatcher, Name)
    when is_pid(Dispatcher), is_list(Name) ->
    gen_server:call(Dispatcher, {'get_pid', Name}, infinity).

-spec get_pid(Dispatcher :: pid(),
              Name :: string(),
              Timeout :: pos_integer()) ->
    {'ok', {string(), pid()}} |
    {'error', atom()}.

get_pid(Dispatcher, Name, Timeout)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         Timeout >= 0 ->
    ?CATCH_TIMEOUT(gen_server:call(Dispatcher,
                                   {'get_pid', Name,
                                    Timeout}, Timeout + ?TIMEOUT_DELTA)).

-spec send_async(Dispatcher :: pid(),
                 Name :: string(),
                 Request :: any()) ->
    {'ok', binary()} |
    {'error', atom()}.

send_async(Dispatcher, Name, Request)
    when is_pid(Dispatcher), is_list(Name) ->
    gen_server:call(Dispatcher, {'send_async', Name, <<>>, Request,
                                 undefined, undefined}, infinity).

-spec send_async(Dispatcher :: pid(),
                 Name :: string(),
                 Request :: any(),
                 Timeout :: pos_integer() | 'undefined') ->
    {'ok', binary()} |
    {'error', atom()}.

send_async(Dispatcher, Name, Request, undefined)
    when is_pid(Dispatcher), is_list(Name) ->
    gen_server:call(Dispatcher, {'send_async', Name, <<>>, Request,
                                 undefined, undefined}, infinity);

send_async(Dispatcher, Name, Request, Timeout)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         Timeout >= 0 ->
    ?CATCH_TIMEOUT(gen_server:call(Dispatcher,
                                   {'send_async', Name, <<>>, Request,
                                    Timeout, undefined},
                                   Timeout + ?TIMEOUT_DELTA)).

-spec send_async(Dispatcher :: pid(),
                 Name :: string(),
                 Request :: any(),
                 Timeout :: pos_integer() | 'undefined',
                 PatternPid :: {string(), pid()}) ->
    {'ok', binary()} |
    {'error', atom()}.

send_async(Dispatcher, Name, Request, undefined, PatternPid)
    when is_pid(Dispatcher), is_list(Name), is_tuple(PatternPid) ->
    gen_server:call(Dispatcher, {'send_async', Name, <<>>, Request,
                                 undefined, undefined, PatternPid}, infinity);

send_async(Dispatcher, Name, Request, Timeout, PatternPid)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         Timeout >= 0, is_tuple(PatternPid) ->
    ?CATCH_TIMEOUT(gen_server:call(Dispatcher,
                                   {'send_async', Name, <<>>, Request,
                                    Timeout, undefined, PatternPid},
                                   Timeout + ?TIMEOUT_DELTA)).

-spec send_async(Dispatcher :: pid(),
                 Name :: string(),
                 RequestInfo :: any(),
                 Request :: any(),
                 Timeout :: pos_integer() | 'undefined',
                 Priority :: integer() | 'undefined') ->
    {'ok', binary()} |
    {'error', atom()}.

send_async(Dispatcher, Name, RequestInfo, Request,
           undefined, undefined)
    when is_pid(Dispatcher), is_list(Name) ->
    gen_server:call(Dispatcher, {'send_async', Name,
                                 RequestInfo, Request,
                                 undefined, undefined}, infinity);

send_async(Dispatcher, Name, RequestInfo, Request,
           undefined, Priority)
    when is_pid(Dispatcher), is_list(Name), is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    gen_server:call(Dispatcher, {'send_async', Name,
                                 RequestInfo, Request,
                                 undefined, Priority}, infinity);

send_async(Dispatcher, Name, RequestInfo, Request,
           Timeout, undefined)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         Timeout >= 0 ->
    ?CATCH_TIMEOUT(gen_server:call(Dispatcher,
                                   {'send_async', Name,
                                    RequestInfo, Request,
                                    Timeout, undefined},
                                   Timeout + ?TIMEOUT_DELTA));

send_async(Dispatcher, Name, RequestInfo, Request,
           Timeout, Priority)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         Timeout >= 0, is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    ?CATCH_TIMEOUT(gen_server:call(Dispatcher,
                                   {'send_async', Name,
                                    RequestInfo, Request,
                                    Timeout, Priority},
                                   Timeout + ?TIMEOUT_DELTA)).

-spec send_async(Dispatcher :: pid(),
                 Name :: string(),
                 RequestInfo :: any(),
                 Request :: any(),
                 Timeout :: pos_integer() | 'undefined',
                 Priority :: integer() | 'undefined',
                 PatternPid :: {string(), pid()}) ->
    {'ok', binary()} |
    {'error', atom()}.

send_async(Dispatcher, Name, RequestInfo, Request,
           undefined, undefined, PatternPid)
    when is_pid(Dispatcher), is_list(Name), is_tuple(PatternPid) ->
    gen_server:call(Dispatcher, {'send_async', Name,
                                 RequestInfo, Request,
                                 undefined,
                                 undefined, PatternPid}, infinity);

send_async(Dispatcher, Name, RequestInfo, Request,
           undefined, Priority, PatternPid)
    when is_pid(Dispatcher), is_list(Name), is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW,
         is_tuple(PatternPid) ->
    gen_server:call(Dispatcher, {'send_async', Name,
                                 RequestInfo, Request,
                                 undefined,
                                 Priority, PatternPid}, infinity);

send_async(Dispatcher, Name, RequestInfo, Request,
           Timeout, undefined, PatternPid)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         Timeout >= 0, is_tuple(PatternPid) ->
    ?CATCH_TIMEOUT(gen_server:call(Dispatcher,
                                   {'send_async', Name,
                                    RequestInfo, Request,
                                    Timeout, undefined, PatternPid},
                                   Timeout + ?TIMEOUT_DELTA));

send_async(Dispatcher, Name, RequestInfo, Request,
           Timeout, Priority, PatternPid)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         Timeout >= 0, is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW,
         is_tuple(PatternPid) ->
    ?CATCH_TIMEOUT(gen_server:call(Dispatcher,
                                   {'send_async', Name,
                                    RequestInfo, Request,
                                    Timeout, Priority, PatternPid},
                                   Timeout + ?TIMEOUT_DELTA)).

-spec send_async_active(Dispatcher :: pid(),
                        Name :: string(),
                        Request :: any()) ->
    {'ok', binary()} |
    {'error', atom()}.

send_async_active(Dispatcher, Name, Request)
    when is_pid(Dispatcher), is_list(Name) ->
    gen_server:call(Dispatcher, {'send_async_active', Name, <<>>, Request,
                                 undefined, undefined}, infinity).

-spec send_async_active(Dispatcher :: pid(),
                        Name :: string(),
                        Request :: any(),
                        Timeout :: pos_integer() | 'undefined') ->
    {'ok', binary()} |
    {'error', atom()}.

send_async_active(Dispatcher, Name, Request, undefined)
    when is_pid(Dispatcher), is_list(Name) ->
    gen_server:call(Dispatcher, {'send_async_active', Name, <<>>, Request,
                                 undefined, undefined}, infinity);

send_async_active(Dispatcher, Name, Request, Timeout)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         Timeout >= 0 ->
    ?CATCH_TIMEOUT(gen_server:call(Dispatcher,
                                   {'send_async_active', Name, <<>>, Request,
                                    Timeout, undefined},
                                   Timeout + ?TIMEOUT_DELTA)).

-spec send_async_active(Dispatcher :: pid(),
                        Name :: string(),
                        Request :: any(),
                        Timeout :: pos_integer() | 'undefined',
                        PatternPid :: {string(), pid()}) ->
    {'ok', binary()} |
    {'error', atom()}.

send_async_active(Dispatcher, Name, Request, undefined, PatternPid)
    when is_pid(Dispatcher), is_list(Name), is_tuple(PatternPid) ->
    gen_server:call(Dispatcher, {'send_async_active', Name, <<>>, Request,
                                 undefined, undefined, PatternPid}, infinity);

send_async_active(Dispatcher, Name, Request, Timeout, PatternPid)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         Timeout >= 0, is_tuple(PatternPid) ->
    ?CATCH_TIMEOUT(gen_server:call(Dispatcher,
                                   {'send_async_active', Name, <<>>, Request,
                                    Timeout, undefined, PatternPid},
                                   Timeout + ?TIMEOUT_DELTA)).

-spec send_async_active(Dispatcher :: pid(),
                        Name :: string(),
                        RequestInfo :: any(),
                        Request :: any(),
                        Timeout :: pos_integer() | 'undefined',
                        Priority :: integer() | 'undefined') ->
    {'ok', binary()} |
    {'error', atom()}.

send_async_active(Dispatcher, Name, RequestInfo, Request,
                  undefined, undefined)
    when is_pid(Dispatcher), is_list(Name) ->
    gen_server:call(Dispatcher, {'send_async_active', Name,
                                 RequestInfo, Request,
                                 undefined,
                                 undefined}, infinity);

send_async_active(Dispatcher, Name, RequestInfo, Request,
                  undefined, Priority)
    when is_pid(Dispatcher), is_list(Name), is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    gen_server:call(Dispatcher, {'send_async_active', Name,
                                 RequestInfo, Request,
                                 undefined,
                                 Priority}, infinity);

send_async_active(Dispatcher, Name, RequestInfo, Request,
                  Timeout, undefined)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         Timeout >= 0 ->
    ?CATCH_TIMEOUT(gen_server:call(Dispatcher,
                                   {'send_async_active', Name,
                                    RequestInfo, Request,
                                    Timeout, undefined},
                                   Timeout + ?TIMEOUT_DELTA));

send_async_active(Dispatcher, Name, RequestInfo, Request,
                  Timeout, Priority)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         Timeout >= 0, is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    ?CATCH_TIMEOUT(gen_server:call(Dispatcher,
                                   {'send_async_active', Name,
                                    RequestInfo, Request,
                                    Timeout, Priority},
                                   Timeout + ?TIMEOUT_DELTA)).

-spec send_async_active(Dispatcher :: pid(),
                        Name :: string(),
                        RequestInfo :: any(),
                        Request :: any(),
                        Timeout :: pos_integer() | 'undefined',
                        Priority :: integer() | 'undefined',
                        PatternPid :: {string(), pid()}) ->
    {'ok', binary()} |
    {'error', atom()}.

send_async_active(Dispatcher, Name, RequestInfo, Request,
                  undefined, undefined, PatternPid)
    when is_pid(Dispatcher), is_list(Name), is_tuple(PatternPid) ->
    gen_server:call(Dispatcher, {'send_async_active', Name,
                                 RequestInfo, Request,
                                 undefined,
                                 undefined, PatternPid}, infinity);

send_async_active(Dispatcher, Name, RequestInfo, Request,
                  undefined, Priority, PatternPid)
    when is_pid(Dispatcher), is_list(Name), is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW,
         is_tuple(PatternPid) ->
    gen_server:call(Dispatcher, {'send_async_active', Name,
                                 RequestInfo, Request,
                                 undefined,
                                 Priority, PatternPid}, infinity);

send_async_active(Dispatcher, Name, RequestInfo, Request,
                  Timeout, undefined, PatternPid)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         Timeout >= 0, is_tuple(PatternPid) ->
    ?CATCH_TIMEOUT(gen_server:call(Dispatcher,
                                   {'send_async_active', Name,
                                    RequestInfo, Request,
                                    Timeout, undefined, PatternPid},
                                   Timeout + ?TIMEOUT_DELTA));

send_async_active(Dispatcher, Name, RequestInfo, Request,
                  Timeout, Priority, PatternPid)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         Timeout >= 0, is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW,
         is_tuple(PatternPid) ->
    ?CATCH_TIMEOUT(gen_server:call(Dispatcher,
                                   {'send_async_active', Name,
                                    RequestInfo, Request,
                                    Timeout, Priority, PatternPid},
                                   Timeout + ?TIMEOUT_DELTA)).

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
                         PatternPid :: {string(), pid()}) ->
    {'ok', binary()} |
    {'error', atom()}.

send_async_passive(Dispatcher, Name, Request, Timeout, PatternPid) ->
    send_async(Dispatcher, Name, Request, Timeout, PatternPid).

-spec send_async_passive(Dispatcher :: pid(),
                         Name :: string(),
                         RequestInfo :: any(),
                         Request :: any(),
                         Timeout :: pos_integer() | 'undefined',
                         Priority :: integer() | 'undefined') ->
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
                         Priority :: integer() | 'undefined',
                         PatternPid :: {string(), pid()}) ->
    {'ok', binary()} |
    {'error', atom()}.

send_async_passive(Dispatcher, Name, RequestInfo, Request,
                   Timeout, Priority, PatternPid) ->
    send_async(Dispatcher, Name, RequestInfo, Request,
               Timeout, Priority, PatternPid).

-spec send_sync(Dispatcher :: pid(),
                Name :: string(),
                Request :: any()) ->
    {'ok', any(), any()} |
    {'ok', any()} |
    {'error', atom()}.

send_sync(Dispatcher, Name, Request)
    when is_pid(Dispatcher), is_list(Name) ->
    gen_server:call(Dispatcher, {'send_sync', Name, <<>>, Request,
                                 undefined, undefined}, infinity).

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
                                 undefined, undefined}, infinity);

send_sync(Dispatcher, Name, Request, Timeout)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         Timeout >= 0 ->
    ?CATCH_TIMEOUT(gen_server:call(Dispatcher,
                                   {'send_sync', Name, <<>>, Request,
                                    Timeout, undefined},
                                   Timeout + ?TIMEOUT_DELTA)).

-spec send_sync(Dispatcher :: pid(),
                Name :: string(),
                Request :: any(),
                Timeout :: pos_integer() | 'undefined',
                PatternPid :: {string(), pid()}) ->
    {'ok', any(), any()} |
    {'ok', any()} |
    {'error', atom()}.

send_sync(Dispatcher, Name, Request, undefined, PatternPid)
    when is_pid(Dispatcher), is_list(Name), is_tuple(PatternPid) ->
    gen_server:call(Dispatcher, {'send_sync', Name, <<>>, Request,
                                 undefined, undefined, PatternPid}, infinity);

send_sync(Dispatcher, Name, Request, Timeout, PatternPid)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         Timeout >= 0, is_tuple(PatternPid) ->
    ?CATCH_TIMEOUT(gen_server:call(Dispatcher,
                                   {'send_sync', Name, <<>>, Request,
                                    Timeout, undefined, PatternPid},
                                   Timeout + ?TIMEOUT_DELTA)).
-spec send_sync(Dispatcher :: pid(),
                Name :: string(),
                RequestInfo :: any(),
                Request :: any(),
                Timeout :: pos_integer() | 'undefined', 
                Priority :: integer() | 'undefined') ->
    {'ok', any(), any()} |
    {'ok', any()} |
    {'error', atom()}.

send_sync(Dispatcher, Name, RequestInfo, Request,
          undefined, undefined)
    when is_pid(Dispatcher), is_list(Name) ->
    gen_server:call(Dispatcher, {'send_sync', Name,
                                 RequestInfo, Request,
                                 undefined, undefined}, infinity);

send_sync(Dispatcher, Name, RequestInfo, Request,
          undefined, Priority)
    when is_pid(Dispatcher), is_list(Name), is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    gen_server:call(Dispatcher, {'send_sync', Name,
                                 RequestInfo, Request,
                                 undefined, Priority}, infinity);

send_sync(Dispatcher, Name, RequestInfo, Request,
          Timeout, undefined)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         Timeout >= 0 ->
    ?CATCH_TIMEOUT(gen_server:call(Dispatcher,
                                   {'send_sync', Name,
                                    RequestInfo, Request,
                                    Timeout, undefined},
                                   Timeout + ?TIMEOUT_DELTA));

send_sync(Dispatcher, Name, RequestInfo, Request,
          Timeout, Priority)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         Timeout >= 0, is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    ?CATCH_TIMEOUT(gen_server:call(Dispatcher,
                                   {'send_sync', Name,
                                    RequestInfo, Request,
                                    Timeout, Priority},
                                   Timeout + ?TIMEOUT_DELTA)).

-spec send_sync(Dispatcher :: pid(),
                Name :: string(),
                RequestInfo :: any(),
                Request :: any(),
                Timeout :: pos_integer() | 'undefined',
                Priority :: integer() | 'undefined',
                PatternPid :: {string(), pid()}) ->
    {'ok', any(), any()} |
    {'ok', any()} |
    {'error', atom()}.

send_sync(Dispatcher, Name, RequestInfo, Request,
          undefined, undefined, PatternPid)
    when is_pid(Dispatcher), is_list(Name), is_tuple(PatternPid) ->
    gen_server:call(Dispatcher, {'send_sync', Name,
                                 RequestInfo, Request,
                                 undefined,
                                 undefined, PatternPid}, infinity);

send_sync(Dispatcher, Name, RequestInfo, Request,
          undefined, Priority, PatternPid)
    when is_pid(Dispatcher), is_list(Name), is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW,
         is_tuple(PatternPid) ->
    gen_server:call(Dispatcher, {'send_sync', Name,
                                 RequestInfo, Request,
                                 undefined,
                                 Priority, PatternPid}, infinity);

send_sync(Dispatcher, Name, RequestInfo, Request,
          Timeout, undefined, PatternPid)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         Timeout >= 0, is_tuple(PatternPid) ->
    ?CATCH_TIMEOUT(gen_server:call(Dispatcher,
                                   {'send_sync', Name,
                                    RequestInfo, Request,
                                    Timeout, undefined, PatternPid},
                                   Timeout + ?TIMEOUT_DELTA));

send_sync(Dispatcher, Name, RequestInfo, Request,
          Timeout, Priority, PatternPid)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         Timeout >= 0, is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW,
         is_tuple(PatternPid) ->
    ?CATCH_TIMEOUT(gen_server:call(Dispatcher,
                                   {'send_sync', Name,
                                    RequestInfo, Request,
                                    Timeout, Priority, PatternPid},
                                   Timeout + ?TIMEOUT_DELTA)).

-spec mcast_async(Dispatcher :: pid(),
                  Name :: string(),
                  Request :: any()) ->
    {'ok', list(binary())} |
    {'error', atom()}.

mcast_async(Dispatcher, Name, Request)
    when is_pid(Dispatcher), is_list(Name) ->
    gen_server:call(Dispatcher, {'mcast_async', Name, <<>>, Request,
                                 undefined, undefined}, infinity).

-spec mcast_async(Dispatcher :: pid(),
                  Name :: string(),
                  Request :: any(),
                  Timeout :: pos_integer() | 'undefined') ->
    {'ok', list(binary())} |
    {'error', atom()}.

mcast_async(Dispatcher, Name, Request, undefined)
    when is_pid(Dispatcher), is_list(Name) ->
    gen_server:call(Dispatcher, {'mcast_async', Name, <<>>, Request,
                                 undefined, undefined}, infinity);

mcast_async(Dispatcher, Name, Request, Timeout)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         Timeout >= 0 ->
    ?CATCH_TIMEOUT(gen_server:call(Dispatcher,
                                   {'mcast_async', Name, <<>>, Request,
                                    Timeout, undefined},
                                   Timeout + ?TIMEOUT_DELTA)).

-spec mcast_async(Dispatcher :: pid(),
                  Name :: string(),
                  Request :: any(),
                  Timeout :: pos_integer() | 'undefined',
                  PatternPid :: {string(), pid()}) ->
    {'ok', list(binary())} |
    {'error', atom()}.

mcast_async(Dispatcher, Name, Request, undefined, PatternPid)
    when is_pid(Dispatcher), is_list(Name), is_tuple(PatternPid) ->
    gen_server:call(Dispatcher, {'mcast_async', Name, <<>>, Request,
                                 undefined, undefined, PatternPid}, infinity);

mcast_async(Dispatcher, Name, Request, Timeout, PatternPid)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         Timeout >= 0, is_tuple(PatternPid) ->
    ?CATCH_TIMEOUT(gen_server:call(Dispatcher,
                                   {'mcast_async', Name, <<>>, Request,
                                    Timeout, undefined, PatternPid},
                                   Timeout + ?TIMEOUT_DELTA)).

-spec mcast_async(Dispatcher :: pid(),
                  Name :: string(),
                  RequestInfo :: any(),
                  Request :: any(),
                  Timeout :: pos_integer() | 'undefined',
                  Priority :: integer() | 'undefined') ->
    {'ok', list(binary())} |
    {'error', atom()}.

mcast_async(Dispatcher, Name, RequestInfo, Request, undefined, undefined)
    when is_pid(Dispatcher), is_list(Name) ->
    gen_server:call(Dispatcher, {'mcast_async', Name,
                                 RequestInfo, Request,
                                 undefined,
                                 undefined}, infinity);

mcast_async(Dispatcher, Name, RequestInfo, Request, undefined, Priority)
    when is_pid(Dispatcher), is_list(Name), is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    gen_server:call(Dispatcher, {'mcast_async', Name,
                                 RequestInfo, Request,
                                 undefined,
                                 Priority}, infinity);

mcast_async(Dispatcher, Name, RequestInfo, Request, Timeout, undefined)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         Timeout >= 0 ->
    ?CATCH_TIMEOUT(gen_server:call(Dispatcher,
                                   {'mcast_async', Name,
                                    RequestInfo, Request,
                                    Timeout, undefined},
                                   Timeout + ?TIMEOUT_DELTA));

mcast_async(Dispatcher, Name, RequestInfo, Request, Timeout, Priority)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         Timeout >= 0, is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    ?CATCH_TIMEOUT(gen_server:call(Dispatcher,
                                   {'mcast_async', Name,
                                    RequestInfo, Request,
                                    Timeout, Priority},
                                   Timeout + ?TIMEOUT_DELTA)).

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

-spec return(Dispatcher :: pid(),
             'send_async' | 'send_sync',
             Name :: string(),
             Pattern :: string(),
             ResponseInfo :: any(),
             Response :: any(),
             Timeout :: pos_integer(),
             TransId :: binary(),
             Pid :: pid()) -> none().

return(Dispatcher, Type, Name, Pattern, ResponseInfo, Response,
       Timeout, TransId, Pid) ->
    return_nothrow(Dispatcher, Type, Name, Pattern, ResponseInfo, Response,
                   Timeout, TransId, Pid),
    erlang:throw(return).

-spec return_async(Dispatcher :: pid(),
                   Name :: string(),
                   Pattern :: string(),
                   ResponseInfo :: any(),
                   Response :: any(),
                   Timeout :: pos_integer(),
                   TransId :: binary(),
                   Pid :: pid()) -> none().

return_async(Dispatcher, Name, Pattern, ResponseInfo, Response,
             Timeout, TransId, Pid)
    when is_pid(Dispatcher), is_list(Name), is_list(Pattern),
         is_integer(Timeout), is_binary(TransId), is_pid(Pid), Timeout > 0 ->
    Pid ! {'return_async', Name, Pattern, ResponseInfo, Response,
           Timeout, TransId, Pid},
    erlang:throw(return).

-spec return_sync(Dispatcher :: pid(),
                  Name :: string(),
                  Pattern :: string(),
                  ResponseInfo :: any(),
                  Response :: any(),
                  Timeout :: pos_integer(),
                  TransId :: binary(),
                  Pid :: pid()) -> none().

return_sync(Dispatcher, Name, Pattern, ResponseInfo, Response,
            Timeout, TransId, Pid)
    when is_pid(Dispatcher), is_list(Name), is_list(Pattern),
         is_integer(Timeout), is_binary(TransId), is_pid(Pid), Timeout > 0 ->
    Pid ! {'return_sync', Name, Pattern, ResponseInfo, Response,
           Timeout, TransId, Pid},
    erlang:throw(return).

-spec return_nothrow(Dispatcher :: pid(),
                     'send_async' | 'send_sync',
                     Name :: string(),
                     Pattern :: string(),
                     ResponseInfo :: any(),
                     Response :: any(),
                     Timeout :: pos_integer(),
                     TransId :: binary(),
                     Pid :: pid()) -> 'ok'.

return_nothrow(_, 'send_async', Name, Pattern, ResponseInfo, Response,
               Timeout, TransId, Pid) ->
    Pid ! {'return_async', Name, Pattern, ResponseInfo, Response,
           Timeout, TransId, Pid},
    ok;

return_nothrow(_, 'send_sync', Name, Pattern, ResponseInfo, Response,
               Timeout, TransId, Pid) ->
    Pid ! {'return_sync', Name, Pattern, ResponseInfo, Response,
           Timeout, TransId, Pid},
    ok.

-spec recv_async(Dispatcher :: pid()) ->
    {'ok', any(), any(), binary()} |
    {'error', atom()}.

recv_async(Dispatcher)
    when is_pid(Dispatcher) ->
    gen_server:call(Dispatcher,
                    {'recv_async', <<0:128>>, true}, infinity).

-spec recv_async(Dispatcher :: pid(),
                 pos_integer() | binary()) ->
    {'ok', any(), any(), binary()} |
    {'error', atom()}.

recv_async(Dispatcher, TransId)
    when is_pid(Dispatcher), is_binary(TransId) ->
    gen_server:call(Dispatcher,
                    {'recv_async', TransId, true}, infinity);

recv_async(Dispatcher, Timeout)
    when is_pid(Dispatcher), is_integer(Timeout),
         Timeout >= 0 ->
    ?CATCH_TIMEOUT(gen_server:call(Dispatcher,
                                   {'recv_async', Timeout, <<0:128>>, true},
                                   Timeout + ?TIMEOUT_DELTA)).

-spec recv_async(Dispatcher :: pid(),
                 pos_integer() | binary(),
                 binary() | boolean()) ->
    {'ok', any(), any(), binary()} |
    {'error', atom()}.

recv_async(Dispatcher, Timeout, TransId)
    when is_pid(Dispatcher), is_integer(Timeout), is_binary(TransId),
         Timeout >= 0 ->
    ?CATCH_TIMEOUT(gen_server:call(Dispatcher,
                                   {'recv_async', Timeout, TransId, true},
                                   Timeout + ?TIMEOUT_DELTA));

recv_async(Dispatcher, Timeout, Consume)
    when is_pid(Dispatcher), is_integer(Timeout), is_boolean(Consume),
         Timeout >= 0 ->
    ?CATCH_TIMEOUT(gen_server:call(Dispatcher,
                                   {'recv_async', Timeout, <<0:128>>, Consume},
                                   Timeout + ?TIMEOUT_DELTA));

recv_async(Dispatcher, TransId, Consume)
    when is_pid(Dispatcher), is_binary(TransId), is_boolean(Consume) ->
    gen_server:call(Dispatcher,
                    {'recv_async', TransId, Consume}, infinity).

-spec recv_async(Dispatcher :: pid(),
                 Timeout :: pos_integer(),
                 TransId :: binary(),
                 Consume :: boolean()) ->
    {'ok', any(), any(), binary()} |
    {'error', atom()}.

recv_async(Dispatcher, Timeout, TransId, Consume)
    when is_pid(Dispatcher), is_integer(Timeout), is_binary(TransId),
         is_boolean(Consume), Timeout >= 0 ->
    ?CATCH_TIMEOUT(gen_server:call(Dispatcher,
                                   {'recv_async', Timeout, TransId, Consume},
                                   Timeout + ?TIMEOUT_DELTA)).

-spec prefix(Dispatcher :: pid()) -> pos_integer().

prefix(Dispatcher) ->
    gen_server:call(Dispatcher, prefix, infinity).

-spec timeout_async(Dispatcher :: pid()) -> pos_integer().

timeout_async(Dispatcher) ->
    gen_server:call(Dispatcher, timeout_async, infinity).

-spec timeout_sync(Dispatcher :: pid()) -> pos_integer().

timeout_sync(Dispatcher) ->
    gen_server:call(Dispatcher, timeout_sync, infinity).

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

init([Module, Args, Prefix, ConfigOptions, Dispatcher]) ->
    case Module:cloudi_service_init(Args, Prefix, Dispatcher) of
        {ok, ServiceState} ->
            {ok, #state{module = Module,
                        dispatcher = Dispatcher,
                        service_state = ServiceState,
                        options = ConfigOptions}};
        {stop, _} = Stop ->
            Stop
    end.

handle_call(Request, _, State) ->
    ?LOG_WARN("Unknown call \"~p\"", [Request]),
    {stop, cloudi_string:format("Unknown call \"~p\"", [Request]),
     error, State}.

handle_cast(polling,
            #state{service_state = ServiceState,
                   queue_messages = true} = State) ->
    erlang:process_flag(trap_exit, true),
    process_queue_info(process_queue(ServiceState, State));

handle_cast(Request, State) ->
    ?LOG_WARN("Unknown cast \"~p\"", [Request]),
    {noreply, State}.

handle_info({'send_async', Name, Pattern, RequestInfo, Request,
             Timeout, Priority, TransId, Pid},
            #state{module = Module,
                   dispatcher = Dispatcher,
                   service_state = ServiceState,
                   queue_messages = false} = State) ->
    RequestPid = erlang:spawn_link(?MODULE, handle_request,
                                   ['send_async', Name, Pattern,
                                    RequestInfo, Request,
                                    Timeout, Priority, TransId, Pid,
                                    Module, Dispatcher, ServiceState, self()]),
    {noreply, State#state{queue_messages = true,
                          request = RequestPid}};

handle_info({'send_sync', Name, Pattern, RequestInfo, Request,
             Timeout, Priority, TransId, Pid},
            #state{module = Module,
                   dispatcher = Dispatcher,
                   service_state = ServiceState,
                   queue_messages = false} = State) ->
    RequestPid = erlang:spawn_link(?MODULE, handle_request,
                                   ['send_sync', Name, Pattern,
                                    RequestInfo, Request,
                                    Timeout, Priority, TransId, Pid,
                                    Module, Dispatcher, ServiceState, self()]),
    {noreply, State#state{queue_messages = true,
                          request = RequestPid}};

handle_info({'return_async', _, _, _, _, _, _, Pid} = T,
            #state{dispatcher = Dispatcher} = State) ->
    true = Pid == self(),
    Dispatcher ! T,
    {noreply, State};

handle_info({'return_sync', _, _, _, _, _, _, Pid} = T,
            #state{dispatcher = Dispatcher} = State) ->
    true = Pid == self(),
    Dispatcher ! T,
    {noreply, State};

handle_info({_, _, _, _, _, Timeout, Priority, TransId, _} = T,
            #state{queue_messages = true,
                   queued = Queue,
                   options = ConfigOptions} = State) ->
    QueueLimit = ConfigOptions#config_service_options.queue_limit,
    QueueLimitOk = if
        QueueLimit /= undefined ->
            pqueue4:len(Queue) < QueueLimit;
        true ->
            true
    end,
    if
        QueueLimitOk ->
            {noreply, recv_timeout_start(Timeout, Priority, TransId, T, State)};
        true ->
            % message is discarded since too many messages have been queued
            {noreply, State}
    end;

handle_info({cloudi_recv_timeout, Priority, TransId},
            #state{recv_timeouts = Ids,
                   queue_messages = QueueMessages,
                   queued = Queue} = State) ->
    NewQueue = if
        QueueMessages =:= true ->
            pqueue4:filter(fun({_, _, _, _, _, _, _, Id, _}) ->
                Id /= TransId
            end, Priority, Queue);
        true ->
            Queue
    end,
    {noreply, State#state{recv_timeouts = dict:erase(TransId, Ids),
                          queued = NewQueue}};

handle_info({'EXIT', _, cloudi_request_done}, State) ->
    {noreply, State};

handle_info({'EXIT', _, shutdown}, State) ->
    {stop, shutdown, State};

handle_info({'EXIT', Pid, Reason}, State) ->
    % make sure the terminate function is called if the dispatcher dies
    % or any service related exits
    ?LOG_ERROR("~p exited: ~p", [Pid, Reason]),
    {stop, Reason, State};
            
handle_info({cloudi_request_success, Request, NewServiceState},
            #state{dispatcher = Dispatcher} = State) ->
    case Request of
        undefined ->
            ok;
        {'return_async', _, _, _, _, _, _, Pid} = T ->
            Pid ! T;
        {'return_sync', _, _, _, _, _, _, Pid} = T ->
            Pid ! T;
        {'forward_async', _, _, _, _, _, _, _} = T ->
            Dispatcher ! T;
        {'forward_sync', _, _, _, _, _, _, _} = T ->
            Dispatcher ! T
    end,
    process_queue_info(process_queue(NewServiceState, State));

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
                   service_state = ServiceState} = State) ->
    case Module:cloudi_service_handle_info(Request,
                                           ServiceState,
                                           Dispatcher) of
        {noreply, NewServiceState} ->
            {noreply, State#state{service_state = NewServiceState}};
        {stop, Reason, NewServiceState} ->
            {stop, Reason, State#state{service_state = NewServiceState}}
    end.

terminate(Reason,
          #state{module = Module,
                 service_state = ServiceState}) ->
    Module:cloudi_service_terminate(Reason, ServiceState),
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

handle_request('send_async', Name, Pattern, RequestInfo, Request,
               Timeout, Priority, TransId, Pid,
               Module, Dispatcher, ServiceState, Parent) ->
    try Module:cloudi_service_handle_request('send_async', Name, Pattern,
                                             RequestInfo, Request,
                                             Timeout, Priority,
                                             TransId, Pid, ServiceState,
                                             Dispatcher) of
        {reply, Response, NewServiceState} ->
            Parent ! {cloudi_request_success,
                      {'return_async', Name, Pattern, <<>>, Response,
                       Timeout, TransId, Pid},
                      NewServiceState};
        {reply, ResponseInfo, Response, NewServiceState} ->
            Parent ! {cloudi_request_success,
                      {'return_async', Name, Pattern, ResponseInfo, Response,
                       Timeout, TransId, Pid},
                      NewServiceState};
        {forward, NextName, NextRequestInfo, NextRequest,
                  NextTimeout, NextPriority, NewServiceState} ->
            Parent ! {cloudi_request_success,
                      {'forward_async', NextName,
                       NextRequestInfo, NextRequest,
                       NextTimeout, NextPriority, TransId, Pid},
                      NewServiceState};
        {forward, NextName, NextRequestInfo, NextRequest,
                  NewServiceState} ->
            Parent ! {cloudi_request_success,
                      {'forward_async', NextName,
                       NextRequestInfo, NextRequest,
                       Timeout, Priority, TransId, Pid},
                      NewServiceState};
        {noreply, NewServiceState} ->
            Parent ! {cloudi_request_success, undefined, NewServiceState}
    catch
        throw:return ->
            Parent ! {cloudi_request_success, undefined, ServiceState};
        throw:forward ->
            Parent ! {cloudi_request_success, undefined, ServiceState};
        Type:Error ->
            Stack = erlang:get_stacktrace(),
            Parent ! {cloudi_request_failure, Type, Error, Stack}
    end,
    erlang:exit(self(), cloudi_request_done);

handle_request('send_sync', Name, Pattern, RequestInfo, Request,
               Timeout, Priority, TransId, Pid,
               Module, Dispatcher, ServiceState, Parent) ->
    try Module:cloudi_service_handle_request('send_sync', Name, Pattern,
                                             RequestInfo, Request,
                                             Timeout, Priority,
                                             TransId, Pid, ServiceState,
                                             Dispatcher) of
        {reply, Response, NewServiceState} ->
            Parent ! {cloudi_request_success,
                      {'return_sync', Name, Pattern, <<>>, Response,
                       Timeout, TransId, Pid},
                      NewServiceState};
        {reply, ResponseInfo, Response, NewServiceState} ->
            Parent ! {cloudi_request_success,
                      {'return_sync', Name, Pattern, ResponseInfo, Response,
                       Timeout, TransId, Pid},
                      NewServiceState};
        {forward, NextName, NextRequestInfo, NextRequest,
                  NextTimeout, NextPriority, NewServiceState} ->
            Parent ! {cloudi_request_success,
                      {'forward_sync', NextName,
                       NextRequestInfo, NextRequest,
                       NextTimeout, NextPriority, TransId, Pid},
                      NewServiceState};
        {forward, NextName, NextRequestInfo, NextRequest,
                  NewServiceState} ->
            Parent ! {cloudi_request_success,
                      {'forward_sync', NextName,
                       NextRequestInfo, NextRequest,
                       Timeout, Priority, TransId, Pid},
                      NewServiceState};
        {noreply, NewServiceState} ->
            Parent ! {cloudi_request_success, undefined, NewServiceState}
    catch
        throw:return ->
            Parent ! {cloudi_request_success, undefined, ServiceState};
        throw:forward ->
            Parent ! {cloudi_request_success, undefined, ServiceState};
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

process_queue(NewServiceState,
              #state{module = Module,
                     dispatcher = Dispatcher,
                     recv_timeouts = Ids,
                     queue_messages = true,
                     queued = Queue} = State) ->
    case pqueue4:out(Queue) of
        {empty, NewQueue} ->
            State#state{service_state = NewServiceState,
                        queue_messages = false,
                        queued = NewQueue,
                        request = undefined};
        {{value, {'send_async', Name, Pattern, RequestInfo, Request,
                  _, Priority, TransId, Pid}}, NewQueue} ->
            Tref = dict:fetch(TransId, Ids),
            Timeout = case erlang:cancel_timer(Tref) of
                false ->
                    1;
                V ->
                    V
            end,
            RequestPid = erlang:spawn_link(?MODULE, handle_request,
                                           ['send_async', Name, Pattern,
                                            RequestInfo, Request,
                                            Timeout, Priority, TransId, Pid,
                                            Module, Dispatcher,
                                            NewServiceState, self()]),
            State#state{service_state = NewServiceState,
                        recv_timeouts = dict:erase(TransId, Ids),
                        queued = NewQueue,
                        request = RequestPid};
        {{value, {'send_sync', Name, Pattern, RequestInfo, Request,
                  _, Priority, TransId, Pid}}, NewQueue} ->
            Tref = dict:fetch(TransId, Ids),
            Timeout = case erlang:cancel_timer(Tref) of
                false ->
                    1;
                V ->
                    V
            end,
            RequestPid = erlang:spawn_link(?MODULE, handle_request,
                                           ['send_sync', Name, Pattern,
                                            RequestInfo, Request,
                                            Timeout, Priority, TransId, Pid,
                                            Module, Dispatcher,
                                            NewServiceState, self()]),
            State#state{service_state = NewServiceState,
                        recv_timeouts = dict:erase(TransId, Ids),
                        queued = NewQueue,
                        request = RequestPid}
    end.

process_queue_info(#state{module = Module,
                          dispatcher = Dispatcher,
                          service_state = ServiceState,
                          queued_info = QueueInfo} = State) ->
    process_queue_info(Module, Dispatcher, ServiceState, QueueInfo, State).

process_queue_info(Module, Dispatcher, ServiceState, QueueInfo, State) ->
    case queue:out(QueueInfo) of
        {empty, NewQueueInfo} ->
            {noreply, State#state{service_state = ServiceState,
                                  queued_info = NewQueueInfo}};
        {{value, Request}, NewQueueInfo} ->
            case Module:cloudi_service_handle_info(Request,
                                                   ServiceState,
                                                   Dispatcher) of
                {noreply, NewServiceState} ->
                    process_queue_info(Module, Dispatcher,
                                       NewServiceState, NewQueueInfo, State);
                {stop, Reason, NewServiceState} ->
                    {stop, Reason,
                     State#state{service_state = NewServiceState,
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

