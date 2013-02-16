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

-ifdef(ERLANG_OTP_VER_R14).
%% behavior callbacks
-export([behaviour_info/1]).
-endif.

-include("cloudi_constants.hrl").

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

-spec process_index(Dispatcher :: pid()) ->
    non_neg_integer().

process_index(Dispatcher) ->
    gen_server:call(Dispatcher, process_index, infinity).

-spec self(Dispatcher :: pid()) -> pid().

self(Dispatcher) ->
    Dispatcher.

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
              Timeout :: non_neg_integer()) ->
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
                 Timeout :: non_neg_integer() | 'undefined') ->
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
                 Timeout :: non_neg_integer() | 'undefined',
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
                 Timeout :: non_neg_integer() | 'undefined',
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
                 Timeout :: non_neg_integer() | 'undefined',
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
                        Timeout :: non_neg_integer() | 'undefined') ->
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
                        Timeout :: non_neg_integer() | 'undefined',
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
                        Timeout :: non_neg_integer() | 'undefined',
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
                        Timeout :: non_neg_integer() | 'undefined',
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
                         Timeout :: non_neg_integer() | 'undefined') ->
    {'ok', binary()} |
    {'error', atom()}.

send_async_passive(Dispatcher, Name, Request, Timeout) ->
    send_async(Dispatcher, Name, Request, Timeout).

-spec send_async_passive(Dispatcher :: pid(),
                         Name :: string(),
                         Request :: any(),
                         Timeout :: non_neg_integer() | 'undefined',
                         PatternPid :: {string(), pid()}) ->
    {'ok', binary()} |
    {'error', atom()}.

send_async_passive(Dispatcher, Name, Request, Timeout, PatternPid) ->
    send_async(Dispatcher, Name, Request, Timeout, PatternPid).

-spec send_async_passive(Dispatcher :: pid(),
                         Name :: string(),
                         RequestInfo :: any(),
                         Request :: any(),
                         Timeout :: non_neg_integer() | 'undefined',
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
                         Timeout :: non_neg_integer() | 'undefined',
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
                Timeout :: non_neg_integer() | 'undefined') ->
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
                Timeout :: non_neg_integer() | 'undefined',
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
                Timeout :: non_neg_integer() | 'undefined', 
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
                Timeout :: non_neg_integer() | 'undefined',
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
                  Timeout :: non_neg_integer() | 'undefined') ->
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
                  Timeout :: non_neg_integer() | 'undefined',
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
                  Timeout :: non_neg_integer() | 'undefined',
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

-spec recv_async(Dispatcher :: pid()) ->
    {'ok', ResponseInfo :: any(), Response :: any(), TransId :: binary()} |
    {'error', Reason :: atom()}.

recv_async(Dispatcher)
    when is_pid(Dispatcher) ->
    gen_server:call(Dispatcher,
                    {'recv_async', <<0:128>>, true}, infinity).

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

-spec prefix(Dispatcher :: pid()) ->
    Prefix :: string().

prefix(Dispatcher) ->
    gen_server:call(Dispatcher, prefix, infinity).

-spec timeout_async(Dispatcher :: pid()) ->
    TimeoutAsync :: pos_integer().

timeout_async(Dispatcher) ->
    gen_server:call(Dispatcher, timeout_async, infinity).

-spec timeout_sync(Dispatcher :: pid()) ->
    TimeoutSync :: pos_integer().

timeout_sync(Dispatcher) ->
    gen_server:call(Dispatcher, timeout_sync, infinity).

-spec request_http_qs_parse(Request :: binary()) ->
    Result :: dict().

request_http_qs_parse(Request)
    when is_binary(Request) ->
    binary_key_value_parse_list(dict:new(),
                                binary:split(Request, <<0>>, [global])).

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

