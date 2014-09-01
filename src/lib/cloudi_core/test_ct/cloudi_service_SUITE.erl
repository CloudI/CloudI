%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Core Basic Tests==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2014, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2014 Michael Truog
%%% @version 1.3.3 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_SUITE).
-behaviour(cloudi_service).

%% cloudi_service callbacks
-export([cloudi_service_init/3,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/2]).

%% CT callbacks
-export([all/0,
         groups/0,
         suite/0,
         init_per_suite/1,
         end_per_suite/1,
         group/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

%% test callbacks
-export([t_service_internal_sync_1/1,
         t_service_internal_sync_2/1,
         t_service_internal_sync_3/1,
         t_service_internal_async_1/1,
         t_service_internal_async_2/1,
         t_service_internal_async_3/1,
         t_service_internal_aspects_1/1,
         t_service_internal_terminate_1/1,
         t_service_internal_terminate_2/1,
         t_service_internal_terminate_3/1,
         t_service_internal_terminate_4/1]).

%% test helpers
-export([service_increment_aspect_init/4,
         service_increment_aspect_request_before/11,
         service_increment_aspect_request_after/12,
         service_increment_aspect_info/3,
         service_increment_aspect_terminate/2]).

-record(state,
    {
        mode :: atom(),
        requests = [] :: list({cloudi_service:request_type(),
                               cloudi_service:service_name(),
                               cloudi_service:service_name_pattern(),
                               cloudi_service:request_info(),
                               cloudi_service:request(),
                               cloudi_service:timeout_value_milliseconds(),
                               cloudi_service:priority(),
                               cloudi_service:trans_id(),
                               cloudi_service:source()}),
        count = 0 :: non_neg_integer()
    }).

-include_lib("common_test/include/ct.hrl").
-include_lib("cloudi_core/include/cloudi_logger.hrl").

-define(SERVICE_PREFIX1, "/").
-define(SERVICE_SUFFIX1, "service_name").
-define(RESPONSE_INFO1, <<"response_info">>).
-define(RESPONSE1, <<"response">>).
-define(REQUEST_INFO1, <<"request_info">>).
-define(REQUEST1, <<"request">>).
-define(REQUEST_INFO2, <<>>).
-define(REQUEST2, <<"requests">>).
-define(REQUEST_INFO3, <<>>).
-define(REQUEST3, <<"mcast">>).
-define(REQUEST_INFO4, <<>>).
-define(REQUEST4, <<"request_stateless">>).
-define(REQUEST_INFO5, <<>>).
-define(REQUEST5, <<"increment">>).
-define(REQUEST_INFO6, <<>>).
-define(REQUEST6, <<"count">>).
-define(REQUEST_INFO7, <<>>).
-define(REQUEST7, <<"crash">>).

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, ?SERVICE_PREFIX1, Dispatcher) ->
    Defaults = [
        {mode,                             undefined}],
    [Mode] = cloudi_proplists:take_values(Defaults, Args),
    NewMode = if
        Mode =:= reply ->
            cloudi_service:subscribe(Dispatcher,
                                     ?SERVICE_SUFFIX1),
            reply;
        Mode =:= reply_x4 ->
            cloudi_service:subscribe(Dispatcher,
                                     ?SERVICE_SUFFIX1),
            cloudi_service:subscribe(Dispatcher,
                                     ?SERVICE_SUFFIX1),
            cloudi_service:subscribe(Dispatcher,
                                     ?SERVICE_SUFFIX1),
            cloudi_service:subscribe(Dispatcher,
                                     ?SERVICE_SUFFIX1),
            reply;
        Mode =:= init_send_sync ->
            {ok, _, _} = cloudi_service:send_sync(Dispatcher,
                                                  ?SERVICE_PREFIX1 ++
                                                  ?SERVICE_SUFFIX1,
                                                  ?REQUEST_INFO4, ?REQUEST4,
                                                  undefined, undefined),
            undefined;
        Mode =:= init_send_async_recv ->
            {ok, TransId} = cloudi_service:send_async(Dispatcher,
                                                      ?SERVICE_PREFIX1 ++
                                                      ?SERVICE_SUFFIX1,
                                                      ?REQUEST_INFO4, ?REQUEST4,
                                                      undefined, undefined),
            {ok, _, _} = cloudi_service:recv_async(Dispatcher, TransId),
            undefined;
        Mode =:= terminate_sleep ->
            cloudi_service:subscribe(Dispatcher,
                                     ?SERVICE_SUFFIX1),
            terminate_sleep
    end,
    {ok, #state{mode = NewMode}}.

cloudi_service_handle_request(Type, Name, Pattern,
                              ?REQUEST_INFO1 = RequestInfo, ?REQUEST1 = Request,
                              Timeout, Priority, TransId, Pid,
                              #state{mode = reply,
                                     requests = Requests} = State,
                              _Dispatcher) ->
    NewRequests = [{Type, Name, Pattern, RequestInfo, Request,
                    Timeout, Priority, TransId, Pid} | Requests],
    {reply, ?RESPONSE_INFO1, ?RESPONSE1,
     State#state{requests = NewRequests}};
cloudi_service_handle_request(_Type, _Name, _Pattern,
                              ?REQUEST_INFO2, ?REQUEST2,
                              _Timeout, _Priority, _TransId, _Pid,
                              #state{requests = Requests} = State,
                              _Dispatcher) ->
    {reply, <<>>, lists:reverse(Requests), State#state{requests = []}};
cloudi_service_handle_request(_Type, Name, _Pattern,
                              ?REQUEST_INFO3, ?REQUEST3,
                              _Timeout, _Priority, _TransId, _Pid,
                              #state{mode = reply} = State,
                              Dispatcher) ->
    {ok, TransIds} = cloudi_service:mcast_async(Dispatcher, Name,
                                                ?REQUEST_INFO1, ?REQUEST1,
                                                undefined, undefined),
    {ok, Responses} = cloudi_service:recv_asyncs(Dispatcher, TransIds),
    {reply, Responses, State};
cloudi_service_handle_request(_Type, _Name, _Pattern,
                              ?REQUEST_INFO4, ?REQUEST4,
                              _Timeout, _Priority, _TransId, _Pid,
                              #state{} = State,
                              _Dispatcher) ->
    {reply, ?RESPONSE_INFO1, ?RESPONSE1, State};
cloudi_service_handle_request(_Type, _Name, _Pattern,
                              ?REQUEST_INFO5, ?REQUEST5,
                              _Timeout, _Priority, _TransId, _Pid,
                              #state{mode = reply,
                                     count = Count} = State,
                              _Dispatcher) ->
    {reply, ?RESPONSE_INFO1, ?RESPONSE1,
     State#state{count = Count + 1}};
cloudi_service_handle_request(_Type, _Name, _Pattern,
                              ?REQUEST_INFO6, ?REQUEST6,
                              _Timeout, _Priority, _TransId, _Pid,
                              #state{count = Count} = State,
                              _Dispatcher) ->
    {reply, Count, State};
cloudi_service_handle_request(_Type, _Name, _Pattern,
                              ?REQUEST_INFO7, ?REQUEST7,
                              _Timeout, _Priority, _TransId, _Pid,
                              #state{} = State,
                              _Dispatcher) ->
    erlang:exit(crash),
    {noreply, State}.

cloudi_service_handle_info(increment, #state{count = Count} = State, _) ->
    {noreply, State#state{count = Count + 2}};
cloudi_service_handle_info(Request, State, _) ->
    {stop, {unexpected_info, Request}, State}.

cloudi_service_terminate(_, undefined) ->
    % cloudi_service_init/3 caused an exception
    ok;
cloudi_service_terminate(terminate_sleep_test_1,
                         #state{mode = terminate_sleep}) ->
    % t_service_internal_terminate_1/1 and
    % t_service_internal_terminate_2/1 result
    ?LOG_INFO("terminate_sleep_test_1 requires brutal_kill", []),
    receive after 2000 -> ok end,
    ?LOG_FATAL("execution should never get to this point", []),
    ok;
cloudi_service_terminate(terminate_sleep_test_2,
                         #state{mode = terminate_sleep}) ->
    % t_service_internal_terminate_3/1 and
    % t_service_internal_terminate_4/1 result
    ?LOG_INFO("terminate_sleep_test_2 does not requires brutal_kill", []),
    receive after 3000 -> ok end,
    ?LOG_INFO("terminate_sleep_test_2 finished the terminate function", []),
    ok;
cloudi_service_terminate(_, #state{count = 0}) ->
    ok;
cloudi_service_terminate(_, #state{count = 60}) ->
    % t_service_internal_aspects_1/1 result
    ok.

%%%------------------------------------------------------------------------
%%% Callback functions from CT
%%%------------------------------------------------------------------------

all() ->
    [{group, service_internal_1}].

groups() ->
    [{service_internal_1, [],
      [t_service_internal_sync_1,
       t_service_internal_sync_2,
       t_service_internal_sync_3,
       t_service_internal_async_1,
       t_service_internal_async_2,
       t_service_internal_async_3,
       t_service_internal_aspects_1,
       t_service_internal_terminate_1,
       t_service_internal_terminate_2,
       t_service_internal_terminate_3,
       t_service_internal_terminate_4]}].

suite() ->
    [{ct_hooks, [cth_surefire]},
     {timetrap, 10100}].

init_per_suite(Config) ->
    ok = cloudi_x_reltool_util:
             application_start(sasl, [{sasl_error_logger, false}], infinity),
    ok = cloudi_x_reltool_util:application_start(cloudi_core, [], infinity),
    Config.

end_per_suite(_Config) ->
    ok = cloudi_x_reltool_util:application_stop(cloudi_core),
    ok.

group(_GroupName) ->
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(TestCase) ->
    error_logger:info_msg("~p init~n", [TestCase]),
    error_logger:tty(false),
    ok.

end_per_testcase(TestCase) ->
    error_logger:tty(true),
    error_logger:info_msg("~p end~n", [TestCase]),
    ok.

init_per_testcase(TestCase, Config)
    when (TestCase =:= t_service_internal_sync_1) orelse
         (TestCase =:= t_service_internal_sync_2) orelse
         (TestCase =:= t_service_internal_async_1) ->
    init_per_testcase(TestCase),
    {ok, ServiceIds} = cloudi_service_api:services_add([
        % using proplist configuration format, not the tuple/record format
        [{prefix, ?SERVICE_PREFIX1},
         {module, ?MODULE},
         {args, [{mode, reply}]},
         {options,
          [{request_timeout_adjustment, true},
           {response_timeout_adjustment, true},
           {automatic_loading, false},
           {hibernate, false}]}]
        ], infinity),
    [{service_ids, ServiceIds} | Config];
init_per_testcase(TestCase, Config)
    when (TestCase =:= t_service_internal_sync_3) ->
    init_per_testcase(TestCase),
    {ok, ServiceIds} = cloudi_service_api:services_add([
        % using proplist configuration format, not the tuple/record format
        [{prefix, ?SERVICE_PREFIX1},
         {module, ?MODULE},
         {args, [{mode, reply}]},
         {options,
          [{request_timeout_adjustment, true},
           {response_timeout_adjustment, true},
           {automatic_loading, false},
           {hibernate, true}]}]
        ], infinity),
    [{service_ids, ServiceIds} | Config];
init_per_testcase(TestCase, Config)
    when (TestCase =:= t_service_internal_async_2) ->
    init_per_testcase(TestCase),
    {ok, ServiceIds} = cloudi_service_api:services_add([
        % using proplist configuration format, not the tuple/record format
        [{prefix, ?SERVICE_PREFIX1},
         {module, ?MODULE},
         {args, [{mode, reply_x4}]},
         {options,
          [{request_timeout_adjustment, true},
           {response_timeout_adjustment, true},
           {automatic_loading, false}]}]
        ], infinity),
    [{service_ids, ServiceIds} | Config];
init_per_testcase(TestCase, Config)
    when (TestCase =:= t_service_internal_async_3) ->
    init_per_testcase(TestCase),
    {ok, ServiceIds} = cloudi_service_api:services_add([
        % using proplist configuration format, not the tuple/record format
        [{prefix, ?SERVICE_PREFIX1},
         {module, ?MODULE},
         {args, [{mode, reply_x4}]},
         {count_process, 2},
         {options,
          [{request_timeout_adjustment, true},
           {response_timeout_adjustment, true},
           {automatic_loading, false}]}]
        ], infinity),
    [{service_ids, ServiceIds} | Config];
init_per_testcase(TestCase, Config)
    when (TestCase =:= t_service_internal_aspects_1) ->
    init_per_testcase(TestCase),
    InitAfter1 = fun(_, _, #state{count = Count} = State, _) ->
        {ok, State#state{count = Count + 3}}
    end,
    InitAfter2 = {?MODULE, service_increment_aspect_init},
    RequestBefore = {?MODULE, service_increment_aspect_request_before},
    RequestAfter = {?MODULE, service_increment_aspect_request_after},
    InfoBefore = {?MODULE, service_increment_aspect_info},
    InfoAfter = {?MODULE, service_increment_aspect_info},
    TerminateBefore = {?MODULE, service_increment_aspect_terminate},
    {ok, ServiceIds} = cloudi_service_api:services_add([
        % using proplist configuration format, not the tuple/record format
        [{prefix, ?SERVICE_PREFIX1},
         {module, ?MODULE},
         {args, [{mode, reply}]},
         {options,
          [{request_timeout_adjustment, true},
           {response_timeout_adjustment, true},
           {automatic_loading, false},
           {aspects_init_after, [InitAfter1, InitAfter2]},
           {aspects_request_before, [RequestBefore]},
           {aspects_request_after, [RequestAfter]},
           {aspects_info_before, [InfoBefore]},
           {aspects_info_after, [InfoAfter]},
           {aspects_terminate_before, [TerminateBefore]}]}]
        ], infinity),
    [{service_ids, ServiceIds} | Config];
init_per_testcase(TestCase, Config)
    when (TestCase =:= t_service_internal_terminate_1) ->
    init_per_testcase(TestCase),
    {ok, ServiceIds} = cloudi_service_api:services_add([
        % using proplist configuration format, not the tuple/record format
        [{prefix, ?SERVICE_PREFIX1},
         {module, ?MODULE},
         {args, [{mode, terminate_sleep}]},
         % make max terminate execution time 1.5 second
         {max_r, 200},
         {max_t, 300},
         {options,
          [{automatic_loading, false}]}]
        ], infinity),
    [{service_ids, ServiceIds} | Config];
init_per_testcase(TestCase, Config)
    when (TestCase =:= t_service_internal_terminate_2) ->
    init_per_testcase(TestCase),
    {ok, ServiceIds} = cloudi_service_api:services_add([
        % using proplist configuration format, not the tuple/record format
        [{prefix, ?SERVICE_PREFIX1},
         {module, ?MODULE},
         {args, [{mode, terminate_sleep}]},
         % make max terminate execution time 1.5 second
         {max_r, 200},
         {max_t, 300},
         {options,
          [{automatic_loading, false},
           {duo_mode, true}]}]
        ], infinity),
    [{service_ids, ServiceIds} | Config];
init_per_testcase(TestCase, Config)
    when (TestCase =:= t_service_internal_terminate_3) ->
    init_per_testcase(TestCase),
    {ok, ServiceIds} = cloudi_service_api:services_add([
        % using proplist configuration format, not the tuple/record format
        [{prefix, ?SERVICE_PREFIX1},
         {module, ?MODULE},
         {args, [{mode, terminate_sleep}]},
         % make max terminate execution time 3.191 seconds
         {max_r, 94},
         {max_t, 300},
         {options,
          [{automatic_loading, false}]}]
        ], infinity),
    [{service_ids, ServiceIds} | Config];
init_per_testcase(TestCase, Config)
    when (TestCase =:= t_service_internal_terminate_4) ->
    init_per_testcase(TestCase),
    {ok, ServiceIds} = cloudi_service_api:services_add([
        % using proplist configuration format, not the tuple/record format
        [{prefix, ?SERVICE_PREFIX1},
         {module, ?MODULE},
         {args, [{mode, terminate_sleep}]},
         % make max terminate execution time 3.191 seconds
         {max_r, 94},
         {max_t, 300},
         {options,
          [{automatic_loading, false},
           {duo_mode, true}]}]
        ], infinity),
    [{service_ids, ServiceIds} | Config].

end_per_testcase(TestCase, Config) ->
    end_per_testcase(TestCase),
    {value, {_, ServiceIds}, NewConfig} = lists:keytake(service_ids, 1, Config),
    ok = cloudi_service_api:services_remove(ServiceIds, infinity),
    NewConfig.

%%%------------------------------------------------------------------------
%%% test cases
%%%------------------------------------------------------------------------

t_service_internal_sync_1(_Config) ->
    % make sure synchronous sends work normally,
    % fail within the cloudi_service_init/3 callback with invalid_state,
    % and that an exception within cloudi_service_init/3 doesn't affect
    % other services
    Context = cloudi:new(),
    ServiceName = ?SERVICE_PREFIX1 ++ ?SERVICE_SUFFIX1,
    Self = self(),
    {ok,
     ?RESPONSE_INFO1,
     ?RESPONSE1} = cloudi:send_sync(Context,
                                    ServiceName,
                                    ?REQUEST_INFO1, ?REQUEST1,
                                    undefined, undefined),
    {ok,
     [{'send_sync', ServiceName, ServiceName, ?REQUEST_INFO1, ?REQUEST1,
       _Timeout1, 0, TransId1, Self}]} = cloudi:send_sync(Context,
                                                          ServiceName,
                                                          ?REQUEST2),
    true = cloudi_x_uuid:is_v1(TransId1),
    {ok, _} = cloudi_service_api:services_add([
        [{prefix, ?SERVICE_PREFIX1},
         {module, ?MODULE},
         {args, [{mode, init_send_sync}]},
         {options, [{automatic_loading, false}]}]
        ], infinity),
    {ok,
     ?RESPONSE_INFO1,
     ?RESPONSE1} = cloudi:send_sync(Context,
                                    ServiceName,
                                    ?REQUEST_INFO1, ?REQUEST1,
                                    undefined, undefined),
    {ok,
     [{'send_sync', ServiceName, ServiceName, ?REQUEST_INFO1, ?REQUEST1,
       _Timeout2, 0, TransId2, Self}]} = cloudi:send_sync(Context,
                                                          ServiceName,
                                                          ?REQUEST2),
    true = cloudi_x_uuid:is_v1(TransId2),
    true = (TransId2 > TransId1),
    {ok, _} = cloudi_service_api:services_add([
        [{prefix, ?SERVICE_PREFIX1},
         {module, ?MODULE},
         {args, [{mode, init_send_sync}]},
         {options, [{automatic_loading, false}]}]
        ], infinity),
    ok.

t_service_internal_sync_2(_Config) ->
    % check sync service requests that cause exceptions
    Context = cloudi:new(),
    ServiceName = ?SERVICE_PREFIX1 ++ ?SERVICE_SUFFIX1,
    {ok,
     ?RESPONSE_INFO1,
     ?RESPONSE1} = cloudi:send_sync(Context,
                                    ServiceName,
                                    ?REQUEST_INFO4, ?REQUEST4,
                                    undefined, undefined),
    {ok,
     {_, Service}} = cloudi:get_pid(Context,
                                    ServiceName,
                                    immediate),
    true = erlang:is_process_alive(Service),
    {error,
     timeout} = cloudi:send_sync(Context,
                                 ServiceName,
                                 ?REQUEST_INFO7, ?REQUEST7,
                                 immediate, undefined),
    receive after 100 -> ok end,
    false = erlang:is_process_alive(Service),
    ok.

t_service_internal_sync_3(Config) ->
    t_service_internal_sync_2(Config).

t_service_internal_async_1(_Config) ->
    % make sure asynchronous sends work normally, that recv_async
    % fails within the cloudi_service_init/3 callback with invalid_state,
    % and that an exception within cloudi_service_init/3 doesn't affect
    % other services
    Context = cloudi:new(),
    ServiceName = ?SERVICE_PREFIX1 ++ ?SERVICE_SUFFIX1,
    Self = self(),
    TimeoutMax = cloudi:timeout_async(Context),
    {ok, TransId1} = cloudi:send_async(Context,
                                       ServiceName,
                                       ?REQUEST_INFO1, ?REQUEST1,
                                       undefined, undefined),
    {ok,
     ?RESPONSE_INFO1,
     ?RESPONSE1,
     TransId1} = cloudi:recv_async(Context, TransId1),
    {ok,
     [{'send_async', ServiceName, ServiceName, ?REQUEST_INFO1, ?REQUEST1,
       Timeout1, 0, TransId1, Self}]} = cloudi:send_sync(Context,
                                                         ServiceName,
                                                         ?REQUEST2),
    true = (Timeout1 > (TimeoutMax - 1000)) andalso (Timeout1 =< TimeoutMax),
    true = cloudi_x_uuid:is_v1(TransId1),
    {ok, _} = cloudi_service_api:services_add([
        [{prefix, ?SERVICE_PREFIX1},
         {module, ?MODULE},
         {args, [{mode, init_send_async_recv}]},
         {options, [{automatic_loading, false}]}]
        ], infinity),
    {ok, TransId2} = cloudi:send_async(Context,
                                       ServiceName,
                                       ?REQUEST_INFO1, ?REQUEST1,
                                       undefined, undefined),
    {ok,
     ?RESPONSE_INFO1,
     ?RESPONSE1,
     TransId2} = cloudi:recv_async(Context, TransId2),
    {ok,
     [{'send_async', ServiceName, ServiceName, ?REQUEST_INFO1, ?REQUEST1,
       Timeout2, 0, TransId2, Self}]} = cloudi:send_sync(Context,
                                                         ServiceName,
                                                         ?REQUEST2),
    true = (Timeout2 > (TimeoutMax - 1000)) andalso (Timeout2 =< TimeoutMax),
    true = cloudi_x_uuid:is_v1(TransId2),
    true = (TransId2 > TransId1),
    {ok, _} = cloudi_service_api:services_add([
        [{prefix, ?SERVICE_PREFIX1},
         {module, ?MODULE},
         {args, [{mode, init_send_async_recv}]},
         {options, [{automatic_loading, false}]}]
        ], infinity),
    ok.

t_service_internal_async_2(_Config) ->
    % make sure mcast_async works normally and remains ordered when
    % sending to a service that has a single process
    % (including cloudi:recv_asyncs functionality)
    receive after 1000 -> ok end,
    Context = cloudi:new(),
    ServiceName = ?SERVICE_PREFIX1 ++ ?SERVICE_SUFFIX1,
    Self = self(),
    TimeoutMax = cloudi:timeout_async(Context),
    {ok,
     [TransId1,
      TransId2,
      TransId3,
      TransId4]} = cloudi:mcast_async(Context,
                                      ServiceName,
                                      ?REQUEST_INFO1, ?REQUEST1,
                                      undefined, undefined),
    {ok,
     ?RESPONSE_INFO1,
     ?RESPONSE1,
     TransId1} = cloudi:recv_async(Context, <<0:128>>),
    {ok,
     ?RESPONSE_INFO1,
     ?RESPONSE1,
     TransId2} = cloudi:recv_async(Context, <<0:128>>),
    {ok,
     ?RESPONSE_INFO1,
     ?RESPONSE1,
     TransId3} = cloudi:recv_async(Context, <<0:128>>),
    {ok,
     ?RESPONSE_INFO1,
     ?RESPONSE1,
     TransId4} = cloudi:recv_async(Context, <<0:128>>),
    true = (TransId1 < TransId2) andalso
           (TransId2 < TransId3) andalso
           (TransId3 < TransId4),
    {ok,
     [{'send_async', ServiceName, ServiceName, ?REQUEST_INFO1, ?REQUEST1,
       Timeout1, 0, TransId1, Self},
      {'send_async', ServiceName, ServiceName, ?REQUEST_INFO1, ?REQUEST1,
       Timeout2, 0, TransId2, Self},
      {'send_async', ServiceName, ServiceName, ?REQUEST_INFO1, ?REQUEST1,
       Timeout3, 0, TransId3, Self},
      {'send_async', ServiceName, ServiceName, ?REQUEST_INFO1, ?REQUEST1,
       Timeout4, 0, TransId4, Self}]} = cloudi:send_sync(Context,
                                                         ServiceName,
                                                         ?REQUEST2),
    true = (Timeout1 > (TimeoutMax - 1000)) andalso (Timeout1 =< TimeoutMax),
    true = (Timeout2 > (TimeoutMax - 1000)) andalso (Timeout2 =< TimeoutMax),
    true = (Timeout3 > (TimeoutMax - 1000)) andalso (Timeout3 =< TimeoutMax),
    true = (Timeout4 > (TimeoutMax - 1000)) andalso (Timeout4 =< TimeoutMax),
    true = cloudi_x_uuid:is_v1(TransId1),
    true = cloudi_x_uuid:is_v1(TransId2),
    true = cloudi_x_uuid:is_v1(TransId3),
    true = cloudi_x_uuid:is_v1(TransId4),

    {ok,
     [TransId5,
      TransId6,
      TransId7,
      TransId8] = TransIds} = cloudi:mcast_async(Context,
                                                 ServiceName,
                                                 ?REQUEST_INFO1, ?REQUEST1,
                                                 undefined, undefined),
    {ok,
     [{?RESPONSE_INFO1, ?RESPONSE1, TransId5},
      {?RESPONSE_INFO1, ?RESPONSE1, TransId6},
      {?RESPONSE_INFO1, ?RESPONSE1, TransId7},
      {?RESPONSE_INFO1, ?RESPONSE1, TransId8}]
     } = cloudi:recv_asyncs(Context, TransIds),
    true = (TransId5 < TransId6) andalso
           (TransId6 < TransId7) andalso
           (TransId7 < TransId8),
    {ok,
     [{'send_async', ServiceName, ServiceName, ?REQUEST_INFO1, ?REQUEST1,
       Timeout5, 0, TransId5, Self},
      {'send_async', ServiceName, ServiceName, ?REQUEST_INFO1, ?REQUEST1,
       Timeout6, 0, TransId6, Self},
      {'send_async', ServiceName, ServiceName, ?REQUEST_INFO1, ?REQUEST1,
       Timeout7, 0, TransId7, Self},
      {'send_async', ServiceName, ServiceName, ?REQUEST_INFO1, ?REQUEST1,
       Timeout8, 0, TransId8, Self}]} = cloudi:send_sync(Context,
                                                         ServiceName,
                                                         ?REQUEST2),
    true = (Timeout5 > (TimeoutMax - 1000)) andalso (Timeout5 =< TimeoutMax),
    true = (Timeout6 > (TimeoutMax - 1000)) andalso (Timeout6 =< TimeoutMax),
    true = (Timeout7 > (TimeoutMax - 1000)) andalso (Timeout7 =< TimeoutMax),
    true = (Timeout8 > (TimeoutMax - 1000)) andalso (Timeout8 =< TimeoutMax),
    true = cloudi_x_uuid:is_v1(TransId5),
    true = cloudi_x_uuid:is_v1(TransId6),
    true = cloudi_x_uuid:is_v1(TransId7),
    true = cloudi_x_uuid:is_v1(TransId8),
    ok.

t_service_internal_async_3(_Config) ->
    % make sure mcast_async works normally and remains ordered when
    % sending to a single service process (i.e., when only 2 service processes
    % exist, a service mcast_async to the service name will only be able to
    % send to a single service process)
    % (including cloudi_service:recv_asyncs functionality)
    receive after 1000 -> ok end,
    Context = cloudi:new(),
    ServiceName = ?SERVICE_PREFIX1 ++ ?SERVICE_SUFFIX1,
    {ok,
     [{?RESPONSE_INFO1, ?RESPONSE1, TransId1},
      {?RESPONSE_INFO1, ?RESPONSE1, TransId2},
      {?RESPONSE_INFO1, ?RESPONSE1, TransId3},
      {?RESPONSE_INFO1, ?RESPONSE1, TransId4}]
     } = cloudi:send_sync(Context, ServiceName, ?REQUEST3),
    true = (TransId1 < TransId2) andalso
           (TransId2 < TransId3) andalso
           (TransId3 < TransId4),
    true = cloudi_x_uuid:is_v1(TransId1),
    true = cloudi_x_uuid:is_v1(TransId2),
    true = cloudi_x_uuid:is_v1(TransId3),
    true = cloudi_x_uuid:is_v1(TransId4),
    ok.

t_service_internal_aspects_1(_Config) ->
    % make sure aspects occur as expected

    % count == 3 (0 + 3), InitAfter1
    % count == 7 (3 + 4), InitAfter2
    Context = cloudi:new(),
    ServiceName = ?SERVICE_PREFIX1 ++ ?SERVICE_SUFFIX1,
    % count == 12 (7 + 5), RequestBefore
    {ok,
     ?RESPONSE_INFO1,
     ?RESPONSE1} = cloudi:send_sync(Context,
                                    ServiceName, % count == 13 (12 + 1)
                                    ?REQUEST_INFO5, ?REQUEST5,
                                    undefined, undefined),
    % count == 18 (13 + 5), RequestAfter
    % count == 23 (18 + 5), RequestBefore
    {ok,
     ?RESPONSE_INFO1,
     ?RESPONSE1} = cloudi:send_sync(Context,
                                    ServiceName, % count == 24 (23 + 1)
                                    ?REQUEST_INFO5, ?REQUEST5,
                                    undefined, undefined),
    % count == 29 (24 + 5), RequestAfter
    {ok,
     {_, Service}} = cloudi:get_pid(Context,
                                    ServiceName,
                                    immediate),
    % count == 35 (29 + 6), InfoBefore
    Service ! increment, % count == 37 (35 + 2)
    % count == 43 (37 + 6), InfoAfter
    % count == 48 (43 + 5), RequestBefore
    {ok,
     Count} = cloudi:send_sync(Context,
                               ServiceName,
                               ?REQUEST_INFO6, ?REQUEST6,
                               undefined, undefined),
    48 = Count,
    % count == 53 (48 + 5), RequestAfter
    % count == 60 (53 + 7), TerminateBefore
    ok.

service_increment_aspect_init(_, _, #state{count = Count} = State, _) ->
    {ok, State#state{count = Count + 4}}.

service_increment_aspect_request_before(_, _, _, _, _, _, _, _, _,
                                        #state{count = Count} = State, _) ->
    {ok, State#state{count = Count + 5}}.

service_increment_aspect_request_after(_, _, _, _, _, _, _, _, _, _,
                                       #state{count = Count} = State, _) ->
    {ok, State#state{count = Count + 5}}.

service_increment_aspect_info(_, #state{count = Count} = State, _) ->
    {ok, State#state{count = Count + 6}}.

service_increment_aspect_terminate(_, #state{count = Count} = State) ->
    {ok, State#state{count = Count + 7}}.

t_service_internal_terminate_1(_Config) ->
    % verify the cloudi_service_terminate/2 function execution
    % limitation is enforced
    Context = cloudi:new(),
    ServiceName = ?SERVICE_PREFIX1 ++ ?SERVICE_SUFFIX1,
    {ok,
     ?RESPONSE_INFO1,
     ?RESPONSE1} = cloudi:send_sync(Context,
                                    ServiceName,
                                    ?REQUEST_INFO4, ?REQUEST4,
                                    undefined, undefined),
    {ok,
     {_, Service}} = cloudi:get_pid(Context,
                                    ServiceName,
                                    immediate),
    erlang:exit(Service, terminate_sleep_test_1),
    % wait for termination
    receive after 4000 -> ok end,
    ok.

t_service_internal_terminate_2(Config) ->
    t_service_internal_terminate_1(Config).

t_service_internal_terminate_3(_Config) ->
    % verify the cloudi_service_terminate/2 function execution
    % takes the appropriate amount of time
    Context = cloudi:new(),
    ServiceName = ?SERVICE_PREFIX1 ++ ?SERVICE_SUFFIX1,
    {ok,
     ?RESPONSE_INFO1,
     ?RESPONSE1} = cloudi:send_sync(Context,
                                    ServiceName,
                                    ?REQUEST_INFO4, ?REQUEST4,
                                    undefined, undefined),
    {ok,
     {_, Service}} = cloudi:get_pid(Context,
                                    ServiceName,
                                    immediate),
    erlang:exit(Service, terminate_sleep_test_2),
    % wait for termination
    receive after 4000 -> ok end,
    ok.

t_service_internal_terminate_4(Config) ->
    t_service_internal_terminate_3(Config).

