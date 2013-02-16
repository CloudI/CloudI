%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Internal Service Behavior==
%%% The interface which all internal services must implement.
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

-include("cloudi_constants.hrl").

-define(CATCH_TIMEOUT(F),
        try F catch exit:{timeout, _} -> {error, timeout} end).

%%%------------------------------------------------------------------------
%%% Callback functions from behavior
%%%------------------------------------------------------------------------

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

%%%------------------------------------------------------------------------
%%% Behavior interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the index of this instance of the service.===
%% The configuration of the service defined how many instances should exist.
%% @end
%%-------------------------------------------------------------------------

-spec process_index(Dispatcher :: pid()) ->
    ProcessIndex :: non_neg_integer().

process_index(Dispatcher) ->
    gen_server:call(Dispatcher, process_index, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the Erlang pid representing the service.===
%% Makes the service source code simpler, though the function may
%% seem unnecessary.
%% @end
%%-------------------------------------------------------------------------

-spec self(Dispatcher :: pid()) ->
    Self :: pid().

self(Dispatcher) ->
    Dispatcher.

%%-------------------------------------------------------------------------
%% @doc
%% ===Subscribe to a service name pattern.===
%% @end
%%-------------------------------------------------------------------------

-spec subscribe(Dispatcher :: pid(),
                Pattern :: string()) -> ok.

subscribe(Dispatcher, Pattern)
    when is_pid(Dispatcher), is_list(Pattern) ->
    gen_server:call(Dispatcher, {'subscribe', Pattern}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Unsubscribe from a service name pattern.===
%% @end
%%-------------------------------------------------------------------------

-spec unsubscribe(Dispatcher :: pid(),
                  Pattern :: string()) -> ok.

unsubscribe(Dispatcher, Pattern)
    when is_pid(Dispatcher), is_list(Pattern) ->
    gen_server:call(Dispatcher, {'unsubscribe', Pattern}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a service destination based on a service name.===
%% @end
%%-------------------------------------------------------------------------

-spec get_pid(Dispatcher :: pid(),
              Name :: string()) ->
    {'ok', PatternPid :: {string(), pid()}} |
    {'error', Reason :: atom()}.

get_pid(Dispatcher, Name)
    when is_pid(Dispatcher), is_list(Name) ->
    gen_server:call(Dispatcher, {'get_pid', Name}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a service destination based on a service name.===
%% @end
%%-------------------------------------------------------------------------

-spec get_pid(Dispatcher :: pid(),
              Name :: string(),
              Timeout :: non_neg_integer()) ->
    {'ok', PatternPid :: {string(), pid()}} |
    {'error', Reason :: atom()}.

get_pid(Dispatcher, Name, Timeout)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         Timeout >= 0 ->
    ?CATCH_TIMEOUT(gen_server:call(Dispatcher,
                                   {'get_pid', Name,
                                    Timeout}, Timeout + ?TIMEOUT_DELTA)).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_async(Dispatcher :: pid(),
                 Name :: string(),
                 Request :: any()) ->
    {'ok', TransId :: binary()} |
    {'error', Reason :: atom()}.

send_async(Dispatcher, Name, Request)
    when is_pid(Dispatcher), is_list(Name) ->
    gen_server:call(Dispatcher, {'send_async', Name, <<>>, Request,
                                 undefined, undefined}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_async(Dispatcher :: pid(),
                 Name :: string(),
                 Request :: any(),
                 Timeout :: non_neg_integer() | 'undefined') ->
    {'ok', TransId :: binary()} |
    {'error', Reason :: atom()}.

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

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_async(Dispatcher :: pid(),
                 Name :: string(),
                 Request :: any(),
                 Timeout :: non_neg_integer() | 'undefined',
                 PatternPid :: {string(), pid()}) ->
    {'ok', TransId :: binary()} |
    {'error', Reason :: atom()}.

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

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_async(Dispatcher :: pid(),
                 Name :: string(),
                 RequestInfo :: any(),
                 Request :: any(),
                 Timeout :: non_neg_integer() | 'undefined',
                 Priority :: integer() | 'undefined') ->
    {'ok', TransId :: binary()} |
    {'error', Reason :: atom()}.

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

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_async(Dispatcher :: pid(),
                 Name :: string(),
                 RequestInfo :: any(),
                 Request :: any(),
                 Timeout :: non_neg_integer() | 'undefined',
                 Priority :: integer() | 'undefined',
                 PatternPid :: {string(), pid()}) ->
    {'ok', TransId :: binary()} |
    {'error', Reason :: atom()}.

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

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% The response is sent to the service as an Erlang message which is either:
%% `{return_async_active, Name, Pattern, ResponseInfo, Response, Timeout, TransId}'
%% (or)
%% `{timeout_async_active, TransId}'
%% @end
%%-------------------------------------------------------------------------

-spec send_async_active(Dispatcher :: pid(),
                        Name :: string(),
                        Request :: any()) ->
    {'ok', TransId :: binary()} |
    {'error', Reason :: atom()}.

send_async_active(Dispatcher, Name, Request)
    when is_pid(Dispatcher), is_list(Name) ->
    gen_server:call(Dispatcher, {'send_async_active', Name, <<>>, Request,
                                 undefined, undefined}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% The response is sent to the service as an Erlang message which is either:
%% `{return_async_active, Name, Pattern, ResponseInfo, Response, Timeout, TransId}'
%% (or)
%% `{timeout_async_active, TransId}'
%% @end
%%-------------------------------------------------------------------------

-spec send_async_active(Dispatcher :: pid(),
                        Name :: string(),
                        Request :: any(),
                        Timeout :: non_neg_integer() | 'undefined') ->
    {'ok', TransId :: binary()} |
    {'error', Reason :: atom()}.

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

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% The response is sent to the service as an Erlang message which is either:
%% `{return_async_active, Name, Pattern, ResponseInfo, Response, Timeout, TransId}'
%% (or)
%% `{timeout_async_active, TransId}'
%% @end
%%-------------------------------------------------------------------------

-spec send_async_active(Dispatcher :: pid(),
                        Name :: string(),
                        Request :: any(),
                        Timeout :: non_neg_integer() | 'undefined',
                        PatternPid :: {string(), pid()}) ->
    {'ok', TransId :: binary()} |
    {'error', Reason :: atom()}.

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

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% The response is sent to the service as an Erlang message which is either:
%% `{return_async_active, Name, Pattern, ResponseInfo, Response, Timeout, TransId}'
%% (or)
%% `{timeout_async_active, TransId}'
%% @end
%%-------------------------------------------------------------------------

-spec send_async_active(Dispatcher :: pid(),
                        Name :: string(),
                        RequestInfo :: any(),
                        Request :: any(),
                        Timeout :: non_neg_integer() | 'undefined',
                        Priority :: integer() | 'undefined') ->
    {'ok', TransId :: binary()} |
    {'error', Reason :: atom()}.

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

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% The response is sent to the service as an Erlang message which is either:
%% `{return_async_active, Name, Pattern, ResponseInfo, Response, Timeout, TransId}'
%% (or)
%% `{timeout_async_active, TransId}'
%% @end
%%-------------------------------------------------------------------------

-spec send_async_active(Dispatcher :: pid(),
                        Name :: string(),
                        RequestInfo :: any(),
                        Request :: any(),
                        Timeout :: non_neg_integer() | 'undefined',
                        Priority :: integer() | 'undefined',
                        PatternPid :: {string(), pid()}) ->
    {'ok', TransId :: binary()} |
    {'error', Reason :: atom()}.

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

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% An alias for send_async.  The asynchronous service request is returned
%% and handled the same way as within external services.
%% @end
%%-------------------------------------------------------------------------

-spec send_async_passive(Dispatcher :: pid(),
                         Name :: string(),
                         Request :: any()) ->
    {'ok', TransId :: binary()} |
    {'error', Reason :: atom()}.

send_async_passive(Dispatcher, Name, Request) ->
    send_async(Dispatcher, Name, Request).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% An alias for send_async.  The asynchronous service request is returned
%% and handled the same way as within external services.
%% @end
%%-------------------------------------------------------------------------

-spec send_async_passive(Dispatcher :: pid(),
                         Name :: string(),
                         Request :: any(),
                         Timeout :: non_neg_integer() | 'undefined') ->
    {'ok', TransId :: binary()} |
    {'error', Reason :: atom()}.

send_async_passive(Dispatcher, Name, Request, Timeout) ->
    send_async(Dispatcher, Name, Request, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% An alias for send_async.  The asynchronous service request is returned
%% and handled the same way as within external services.
%% @end
%%-------------------------------------------------------------------------

-spec send_async_passive(Dispatcher :: pid(),
                         Name :: string(),
                         Request :: any(),
                         Timeout :: non_neg_integer() | 'undefined',
                         PatternPid :: {string(), pid()}) ->
    {'ok', TransId :: binary()} |
    {'error', Reason :: atom()}.

send_async_passive(Dispatcher, Name, Request, Timeout, PatternPid) ->
    send_async(Dispatcher, Name, Request, Timeout, PatternPid).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% An alias for send_async.  The asynchronous service request is returned
%% and handled the same way as within external services.
%% @end
%%-------------------------------------------------------------------------

-spec send_async_passive(Dispatcher :: pid(),
                         Name :: string(),
                         RequestInfo :: any(),
                         Request :: any(),
                         Timeout :: non_neg_integer() | 'undefined',
                         Priority :: integer() | 'undefined') ->
    {'ok', TransId :: binary()} |
    {'error', Reason :: atom()}.

send_async_passive(Dispatcher, Name, RequestInfo, Request,
                   Timeout, Priority) ->
    send_async(Dispatcher, Name, RequestInfo, Request,
               Timeout, Priority).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% An alias for send_async.  The asynchronous service request is returned
%% and handled the same way as within external services.
%% @end
%%-------------------------------------------------------------------------

-spec send_async_passive(Dispatcher :: pid(),
                         Name :: string(),
                         RequestInfo :: any(),
                         Request :: any(),
                         Timeout :: non_neg_integer() | 'undefined',
                         Priority :: integer() | 'undefined',
                         PatternPid :: {string(), pid()}) ->
    {'ok', TransId :: binary()} |
    {'error', Reason :: atom()}.

send_async_passive(Dispatcher, Name, RequestInfo, Request,
                   Timeout, Priority, PatternPid) ->
    send_async(Dispatcher, Name, RequestInfo, Request,
               Timeout, Priority, PatternPid).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a synchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_sync(Dispatcher :: pid(),
                Name :: string(),
                Request :: any()) ->
    {'ok', ResponseInfo :: any(), Response :: any()} |
    {'ok', Response :: any()} |
    {'error', Reason :: atom()}.

send_sync(Dispatcher, Name, Request)
    when is_pid(Dispatcher), is_list(Name) ->
    gen_server:call(Dispatcher, {'send_sync', Name, <<>>, Request,
                                 undefined, undefined}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a synchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_sync(Dispatcher :: pid(),
                Name :: string(),
                Request :: any(),
                Timeout :: non_neg_integer() | 'undefined') ->
    {'ok', ResponseInfo :: any(), Response :: any()} |
    {'ok', Response :: any()} |
    {'error', Reason :: atom()}.

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

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a synchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_sync(Dispatcher :: pid(),
                Name :: string(),
                Request :: any(),
                Timeout :: non_neg_integer() | 'undefined',
                PatternPid :: {string(), pid()}) ->
    {'ok', ResponseInfo :: any(), Response :: any()} |
    {'ok', Response :: any()} |
    {'error', Reason :: atom()}.

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

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a synchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_sync(Dispatcher :: pid(),
                Name :: string(),
                RequestInfo :: any(),
                Request :: any(),
                Timeout :: non_neg_integer() | 'undefined', 
                Priority :: integer() | 'undefined') ->
    {'ok', ResponseInfo :: any(), Response :: any()} |
    {'ok', Response :: any()} |
    {'error', Reason :: atom()}.

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

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a synchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_sync(Dispatcher :: pid(),
                Name :: string(),
                RequestInfo :: any(),
                Request :: any(),
                Timeout :: non_neg_integer() | 'undefined',
                Priority :: integer() | 'undefined',
                PatternPid :: {string(), pid()}) ->
    {'ok', ResponseInfo :: any(), Response :: any()} |
    {'ok', Response :: any()} |
    {'error', Reason :: atom()}.

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

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a multicast asynchronous service request.===
%% Asynchronous service requests are sent to all services that have
%% subscribed to the service name pattern that matches the destination.
%% @end
%%-------------------------------------------------------------------------

-spec mcast_async(Dispatcher :: pid(),
                  Name :: string(),
                  Request :: any()) ->
    {'ok', TransIdList :: list(binary())} |
    {'error', Reason :: atom()}.

mcast_async(Dispatcher, Name, Request)
    when is_pid(Dispatcher), is_list(Name) ->
    gen_server:call(Dispatcher, {'mcast_async', Name, <<>>, Request,
                                 undefined, undefined}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a multicast asynchronous service request.===
%% Asynchronous service requests are sent to all services that have
%% subscribed to the service name pattern that matches the destination.
%% @end
%%-------------------------------------------------------------------------

-spec mcast_async(Dispatcher :: pid(),
                  Name :: string(),
                  Request :: any(),
                  Timeout :: non_neg_integer() | 'undefined') ->
    {'ok', TransIdList :: list(binary())} |
    {'error', Reason :: atom()}.

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

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a multicast asynchronous service request.===
%% Asynchronous service requests are sent to all services that have
%% subscribed to the service name pattern that matches the destination.
%% @end
%%-------------------------------------------------------------------------

-spec mcast_async(Dispatcher :: pid(),
                  Name :: string(),
                  Request :: any(),
                  Timeout :: non_neg_integer() | 'undefined',
                  PatternPid :: {string(), pid()}) ->
    {'ok', TransIdList :: list(binary())} |
    {'error', Reason :: atom()}.

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

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a multicast asynchronous service request.===
%% Asynchronous service requests are sent to all services that have
%% subscribed to the service name pattern that matches the destination.
%% @end
%%-------------------------------------------------------------------------

-spec mcast_async(Dispatcher :: pid(),
                  Name :: string(),
                  RequestInfo :: any(),
                  Request :: any(),
                  Timeout :: non_neg_integer() | 'undefined',
                  Priority :: integer() | 'undefined') ->
    {'ok', TransIdList :: list(binary())} |
    {'error', Reason :: atom()}.

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

%%-------------------------------------------------------------------------
%% @doc
%% ===Forward a service request.===
%% @end
%%-------------------------------------------------------------------------

-spec forward(Dispatcher :: pid(),
              'send_async' | 'send_sync',
              Name :: string(),
              RequestInfo :: any(),
              Request :: any(),
              Timeout :: non_neg_integer(),
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

%%-------------------------------------------------------------------------
%% @doc
%% ===Forward an asynchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec forward_async(Dispatcher :: pid(),
                    Name :: string(),
                    RequestInfo :: any(),
                    Request :: any(),
                    Timeout :: non_neg_integer(),
                    Priority :: integer(),
                    TransId :: binary(),
                    Pid :: pid()) -> none().

forward_async(Dispatcher, Name, RequestInfo, Request,
              Timeout, Priority, TransId, Pid)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         is_binary(TransId), is_pid(Pid), Timeout > 0, is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    Dispatcher ! {'cloudi_service_forward_async', Name,
                  RequestInfo, Request,
                  Timeout, Priority, TransId, Pid},
    erlang:throw(forward).

%%-------------------------------------------------------------------------
%% @doc
%% ===Forward a synchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec forward_sync(Dispatcher :: pid(),
                   Name :: string(),
                   RequestInfo :: any(),
                   Request :: any(),
                   Timeout :: non_neg_integer(),
                   Priority :: integer(),
                   TransId :: binary(),
                   Pid :: pid()) -> none().

forward_sync(Dispatcher, Name, RequestInfo, Request,
             Timeout, Priority, TransId, Pid)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout),
         is_binary(TransId), is_pid(Pid), Timeout > 0, is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    Dispatcher ! {'cloudi_service_forward_sync', Name,
                  RequestInfo, Request,
                  Timeout, Priority, TransId, Pid},
    erlang:throw(forward).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return a service response.===
%% @end
%%-------------------------------------------------------------------------

-spec return(Dispatcher :: pid(),
             'send_async' | 'send_sync',
             Name :: string(),
             Pattern :: string(),
             ResponseInfo :: any(),
             Response :: any(),
             Timeout :: non_neg_integer(),
             TransId :: binary(),
             Pid :: pid()) -> none().

return(Dispatcher, Type, Name, Pattern, ResponseInfo, Response,
       Timeout, TransId, Pid) ->
    return_nothrow(Dispatcher, Type, Name, Pattern, ResponseInfo, Response,
                   Timeout, TransId, Pid),
    erlang:throw(return).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return an asynchronous service response.===
%% @end
%%-------------------------------------------------------------------------

-spec return_async(Dispatcher :: pid(),
                   Name :: string(),
                   Pattern :: string(),
                   ResponseInfo :: any(),
                   Response :: any(),
                   Timeout :: non_neg_integer(),
                   TransId :: binary(),
                   Pid :: pid()) -> none().

return_async(Dispatcher, Name, Pattern, ResponseInfo, Response,
             Timeout, TransId, Pid)
    when is_pid(Dispatcher), is_list(Name), is_list(Pattern),
         is_integer(Timeout), is_binary(TransId), is_pid(Pid), Timeout > 0 ->
    Pid ! {'cloudi_service_return_async', Name, Pattern,
           ResponseInfo, Response,
           Timeout, TransId, Pid},
    erlang:throw(return).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return a synchronous service response.===
%% @end
%%-------------------------------------------------------------------------

-spec return_sync(Dispatcher :: pid(),
                  Name :: string(),
                  Pattern :: string(),
                  ResponseInfo :: any(),
                  Response :: any(),
                  Timeout :: non_neg_integer(),
                  TransId :: binary(),
                  Pid :: pid()) -> none().

return_sync(Dispatcher, Name, Pattern, ResponseInfo, Response,
            Timeout, TransId, Pid)
    when is_pid(Dispatcher), is_list(Name), is_list(Pattern),
         is_integer(Timeout), is_binary(TransId), is_pid(Pid), Timeout > 0 ->
    Pid ! {'cloudi_service_return_sync', Name, Pattern,
           ResponseInfo, Response,
           Timeout, TransId, Pid},
    erlang:throw(return).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return a service response without exiting the request handler.===
%% @end
%%-------------------------------------------------------------------------

-spec return_nothrow(Dispatcher :: pid(),
                     'send_async' | 'send_sync',
                     Name :: string(),
                     Pattern :: string(),
                     ResponseInfo :: any(),
                     Response :: any(),
                     Timeout :: non_neg_integer(),
                     TransId :: binary(),
                     Pid :: pid()) -> 'ok'.

return_nothrow(_, 'send_async', Name, Pattern, ResponseInfo, Response,
               Timeout, TransId, Pid) ->
    Pid ! {'cloudi_service_return_async', Name, Pattern,
           ResponseInfo, Response,
           Timeout, TransId, Pid},
    ok;

return_nothrow(_, 'send_sync', Name, Pattern, ResponseInfo, Response,
               Timeout, TransId, Pid) ->
    Pid ! {'cloudi_service_return_sync', Name, Pattern,
           ResponseInfo, Response,
           Timeout, TransId, Pid},
    ok.

%%-------------------------------------------------------------------------
%% @doc
%% ===Receive an asynchronous service request.===
%% Use a null TransId to receive the oldest service request.
%% @end
%%-------------------------------------------------------------------------

-spec recv_async(Dispatcher :: pid()) ->
    {'ok', ResponseInfo :: any(), Response :: any(), TransId :: binary()} |
    {'error', Reason :: atom()}.

recv_async(Dispatcher)
    when is_pid(Dispatcher) ->
    gen_server:call(Dispatcher,
                    {'recv_async', <<0:128>>, true}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Receive an asynchronous service request.===
%% Either use the supplied TransId to receive the specific service request
%% or use a null TransId to receive the oldest service request.
%% @end
%%-------------------------------------------------------------------------

-spec recv_async(Dispatcher :: pid(),
                 non_neg_integer() | binary()) ->
    {'ok', ResponseInfo :: any(), Response :: any(), TransId :: binary()} |
    {'error', Reason :: atom()}.

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

%%-------------------------------------------------------------------------
%% @doc
%% ===Receive an asynchronous service request.===
%% Either use the supplied TransId to receive the specific service request
%% or use a null TransId to receive the oldest service request.
%% @end
%%-------------------------------------------------------------------------

-spec recv_async(Dispatcher :: pid(),
                 non_neg_integer() | binary(),
                 binary() | boolean()) ->
    {'ok', ResponseInfo :: any(), Response :: any(), TransId :: binary()} |
    {'error', Reason :: atom()}.

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

%%-------------------------------------------------------------------------
%% @doc
%% ===Receive an asynchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec recv_async(Dispatcher :: pid(),
                 Timeout :: non_neg_integer(),
                 TransId :: binary(),
                 Consume :: boolean()) ->
    {'ok', ResponseInfo :: any(), Response :: any(), TransId :: binary()} |
    {'error', Reason :: atom()}.

recv_async(Dispatcher, Timeout, TransId, Consume)
    when is_pid(Dispatcher), is_integer(Timeout), is_binary(TransId),
         is_boolean(Consume), Timeout >= 0 ->
    ?CATCH_TIMEOUT(gen_server:call(Dispatcher,
                                   {'recv_async', Timeout, TransId, Consume},
                                   Timeout + ?TIMEOUT_DELTA)).

%%-------------------------------------------------------------------------
%% @doc
%% ===Configured service default prefix.===
%% All subscribed/unsubscribed service names use this prefix.  The prefix
%% defines the scope of the service.
%% @end
%%-------------------------------------------------------------------------

-spec prefix(Dispatcher :: pid()) ->
    Prefix :: string().

prefix(Dispatcher) ->
    gen_server:call(Dispatcher, prefix, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Configured service default asynchronous timeout (in milliseconds).===
%% @end
%%-------------------------------------------------------------------------

-spec timeout_async(Dispatcher :: pid()) ->
    TimeoutAsync :: pos_integer().

timeout_async(Dispatcher) ->
    gen_server:call(Dispatcher, timeout_async, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Configured service default synchronous timeout (in milliseconds).===
%% @end
%%-------------------------------------------------------------------------

-spec timeout_sync(Dispatcher :: pid()) ->
    TimeoutSync :: pos_integer().

timeout_sync(Dispatcher) ->
    gen_server:call(Dispatcher, timeout_sync, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Parse HTTP Request query string data.===
%% @end
%%-------------------------------------------------------------------------

-spec request_http_qs_parse(Request :: binary()) ->
    Result :: dict().

request_http_qs_parse(Request)
    when is_binary(Request) ->
    binary_key_value_parse_list(dict:new(),
                                binary:split(Request, <<0>>, [global])).

%%-------------------------------------------------------------------------
%% @doc
%% ===Parse RequestInfo key/value data.===
%% @end
%% RequestInfo is meant to contain key/value pairs that is request
%% meta-data.
%%-------------------------------------------------------------------------

-spec request_info_key_value_parse(RequestInfo :: binary() | list()) ->
    Result :: dict().

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
%%% Private functions
%%%------------------------------------------------------------------------

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

