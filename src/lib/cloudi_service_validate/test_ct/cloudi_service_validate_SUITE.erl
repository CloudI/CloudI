%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Service Validate Tests==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2015, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2015 Michael Truog
%%% @version 1.5.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_validate_SUITE).
-behaviour(cloudi_service).

%% cloudi_service callbacks
-export([cloudi_service_init/4,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/3]).

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
-export([t_integer_even_success_1/1,
         t_integer_even_failure_1/1,
         t_integer_even_failure_2/1,
         t_integer_even_failure_3/1]).

-record(state,
    {
        mode :: atom()
    }).

-include_lib("common_test/include/ct.hrl").
-include_lib("cloudi_core/include/cloudi_logger.hrl").

-define(SERVICE_PREFIX1, "/validate").
-define(SERVICE_PREFIX2, "/test/").
-define(SERVICE_SUFFIX1, "incr_source").
-define(SERVICE_SUFFIX2, "incr_dest").

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, ?SERVICE_PREFIX2, _Timeout, Dispatcher) ->
    Defaults = [
        {mode,                             undefined}],
    [Mode] = cloudi_proplists:take_values(Defaults, Args),
    if
        Mode =:= incr_source ->
            cloudi_service:subscribe(Dispatcher, ?SERVICE_SUFFIX1),
            ok;
        Mode =:= incr_dest ->
            cloudi_service:subscribe(Dispatcher, ?SERVICE_SUFFIX2),
            ok
    end,
    {ok, #state{mode = Mode}}.

cloudi_service_handle_request(_Type,
                              ?SERVICE_PREFIX2 ++ ?SERVICE_SUFFIX1,
                              ?SERVICE_PREFIX2 ++ ?SERVICE_SUFFIX1,
                              RequestInfo, Request,
                              Timeout, Priority, _TransId, _Pid,
                              #state{mode = incr_source} = State,
                              Dispatcher) ->
    NextName = ?SERVICE_PREFIX1 ++ ?SERVICE_PREFIX2 ++ ?SERVICE_SUFFIX2,
    case cloudi_service:send_sync(Dispatcher, NextName,
                                  RequestInfo, Request,
                                  Timeout, Priority) of
        {ok,
         ResponseInfo,
         Response} ->
            {reply, ResponseInfo, Response, State};
        {error, timeout} ->
            {reply, -1, -1, State}
    end;
cloudi_service_handle_request(_Type,
                              ?SERVICE_PREFIX2 ++ ?SERVICE_SUFFIX2,
                              ?SERVICE_PREFIX2 ++ ?SERVICE_SUFFIX2,
                              RequestInfo, Request,
                              _Timeout, _Priority, _TransId, _Pid,
                              #state{mode = incr_dest} = State,
                              _Dispatcher) ->
    {reply, RequestInfo, Request + 1, State}.

cloudi_service_handle_info(Request, State, _Dispatcher) ->
    {stop, {unexpected_info, Request}, State}.

cloudi_service_terminate(_Reason, _Timeout, _State) ->
    ok.

%%%------------------------------------------------------------------------
%%% Callback functions from CT
%%%------------------------------------------------------------------------

all() ->
    [{group, integer_1}].

groups() ->
    [{integer_1, [],
      [t_integer_even_success_1,
       t_integer_even_failure_1,
       t_integer_even_failure_2,
       t_integer_even_failure_3]}].

suite() ->
    [{ct_hooks, [cth_surefire]},
     {timetrap, 5100}].

init_per_suite(Config) ->
    ok = cloudi_x_reltool_util:application_start(sasl,
                                                 [{sasl_error_logger, false}],
                                                 infinity),
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
    ok.

end_per_testcase(TestCase) ->
    error_logger:info_msg("~p end~n", [TestCase]),
    ok.

init_per_testcase(TestCase, Config)
    when (TestCase =:= t_integer_even_success_1) ->
    init_per_testcase(TestCase),
    {ok, ServiceIds} = cloudi_service_api:services_add([
        % using proplist configuration format, not the tuple/record format
        [{prefix, ?SERVICE_PREFIX2},
         {module, ?MODULE},
         {args, [{mode, incr_source}]}],
        [{prefix, ?SERVICE_PREFIX2},
         {module, ?MODULE},
         {args, [{mode, incr_dest}]}],
        [{prefix, ?SERVICE_PREFIX1},
         {module, cloudi_service_validate},
         {args,
          [{validate_request_info, fun is_zero/1},
           {validate_request, fun is_even/2},
           {validate_response_info, fun is_zero/1},
           {validate_response, fun is_odd/2},
           {failures_source_die, true},
           {failures_source_max_count, 1},
           {failures_source_max_period, infinity},
           {failures_dest_die, true},
           {failures_dest_max_count, 1},
           {failures_dest_max_period, infinity}]}]
        ], infinity),
    [{service_ids, ServiceIds} | Config];
init_per_testcase(TestCase, Config)
    when (TestCase =:= t_integer_even_failure_1) ->
    init_per_testcase(TestCase),
    {ok, ServiceIds} = cloudi_service_api:services_add([
        % using proplist configuration format, not the tuple/record format
        [{prefix, ?SERVICE_PREFIX2},
         {module, ?MODULE},
         {args, [{mode, incr_source}]}],
        [{prefix, ?SERVICE_PREFIX2},
         {module, ?MODULE},
         {args, [{mode, incr_dest}]}],
        [{prefix, ?SERVICE_PREFIX1},
         {module, cloudi_service_validate},
         {args,
          [{validate_request_info, fun is_zero/1},
           {validate_request, fun is_odd/2},
           {validate_response_info, fun is_zero/1},
           {validate_response, fun is_even/2},
           {failures_source_die, true},
           {failures_source_max_count, 1},
           {failures_source_max_period, infinity},
           {failures_dest_die, true},
           {failures_dest_max_count, 1},
           {failures_dest_max_period, infinity}]}]
        ], infinity),
    [{service_ids, ServiceIds} | Config];
init_per_testcase(TestCase, Config)
    when (TestCase =:= t_integer_even_failure_2) ->
    init_per_testcase(TestCase),
    {ok, ServiceIds} = cloudi_service_api:services_add([
        % using proplist configuration format, not the tuple/record format
        [{prefix, ?SERVICE_PREFIX2},
         {module, ?MODULE},
         {args, [{mode, incr_source}]}],
        [{prefix, ?SERVICE_PREFIX2},
         {module, ?MODULE},
         {args, [{mode, incr_dest}]}],
        [{prefix, ?SERVICE_PREFIX1},
         {module, cloudi_service_validate},
         {args,
          [{validate_request_info, fun is_zero/1},
           {validate_request, fun is_even/2},
           {validate_response_info, fun is_zero/1},
           {validate_response, fun is_even/2},
           {failures_source_die, true},
           {failures_source_max_count, 1},
           {failures_source_max_period, infinity},
           {failures_dest_die, true},
           {failures_dest_max_count, 1},
           {failures_dest_max_period, infinity}]}]
        ], infinity),
    [{service_ids, ServiceIds} | Config];
init_per_testcase(TestCase, Config)
    when (TestCase =:= t_integer_even_failure_3) ->
    init_per_testcase(TestCase),
    {ok, ServiceIds} = cloudi_service_api:services_add([
        % using proplist configuration format, not the tuple/record format
        [{prefix, ?SERVICE_PREFIX2},
         {module, ?MODULE},
         {args, [{mode, incr_source}]}],
        [{prefix, ?SERVICE_PREFIX2},
         {module, ?MODULE},
         {args, [{mode, incr_dest}]}],
        [{prefix, ?SERVICE_PREFIX1},
         {module, cloudi_service_validate},
         {args,
          [{validate_request_info, fun is_zero/1},
           {validate_request, fun is_odd/2},
           {validate_response_info, fun is_zero/1},
           {validate_response, fun is_even/2},
           {failures_source_die, true},
           {failures_source_max_count, 2},
           {failures_source_max_period, 1}]},
         {options,
          [{response_timeout_immediate_max, 0}]}]
        ], infinity),
    [{service_ids, ServiceIds} | Config].

end_per_testcase(TestCase, Config) ->
    end_per_testcase(TestCase),
    case lists:keytake(service_ids, 1, Config) of
        {value, {_, ServiceIds}, NewConfig} ->
            ok = cloudi_service_api:services_remove(ServiceIds, infinity),
            NewConfig;
        false ->
            Config
    end.

%%%------------------------------------------------------------------------
%%% test cases
%%%------------------------------------------------------------------------

t_integer_even_success_1(_Config) ->
    Context0 = cloudi:new(),
    ServiceNameSrc = ?SERVICE_PREFIX2 ++ ?SERVICE_SUFFIX1,
    ServiceNameDst = ?SERVICE_PREFIX2 ++ ?SERVICE_SUFFIX2,
    {{ok,
      {_, SrcPid} = PatternPidSrc},
     Context1} = cloudi:get_pid(Context0,
                                ServiceNameSrc,
                                immediate),
    {{ok,
      {_, DstPid}},
     Context2} = cloudi:get_pid(Context1,
                                ServiceNameDst,
                                immediate),
    {{ok, 0, 3},
     Context3} = cloudi:send_sync(Context2, ServiceNameSrc, 0, 2,
                                  undefined, undefined, PatternPidSrc),
    {{ok, 0, 11},
     Context4} = cloudi:send_sync(Context3, ServiceNameSrc, 0, 10,
                                   undefined, undefined, PatternPidSrc),
    {{ok, 0, 5},
     _} = cloudi:send_sync(Context4, ServiceNameSrc, 0, 4,
                           undefined, undefined, PatternPidSrc),
    true = erlang:is_process_alive(SrcPid),
    true = erlang:is_process_alive(DstPid),
    ok.

t_integer_even_failure_1(_Config) ->
    Context0 = cloudi:new(),
    ServiceNameSrc = ?SERVICE_PREFIX2 ++ ?SERVICE_SUFFIX1,
    ServiceNameDst = ?SERVICE_PREFIX2 ++ ?SERVICE_SUFFIX2,
    {{ok,
      {_, SrcPid} = PatternPidSrc},
     Context1} = cloudi:get_pid(Context0,
                                ServiceNameSrc,
                                immediate),
    {{ok,
      {_, DstPid}},
     Context2} = cloudi:get_pid(Context1,
                                ServiceNameDst,
                                immediate),
    MonitorRefSrc = erlang:monitor(process, SrcPid),
    {{ok, 0, 4},
     Context3} = cloudi:send_sync(Context2, ServiceNameSrc, 0, 3,
                                  undefined, undefined, PatternPidSrc),
    {{ok, 0, 12},
     Context4} = cloudi:send_sync(Context3, ServiceNameSrc, 0, 11,
                                  undefined, undefined, PatternPidSrc),
    {{ok, 0, 6},
     Context5} = cloudi:send_sync(Context4, ServiceNameSrc, 0, 5,
                                  undefined, undefined, PatternPidSrc),
    true = erlang:is_process_alive(SrcPid),
    true = erlang:is_process_alive(DstPid),
    {{ok, _},
     _} = cloudi:send_async(Context5, ServiceNameSrc, 0, 2,
                            undefined, undefined, PatternPidSrc),
    % only the source pid is killed due to the request validation
    receive
        {'DOWN', MonitorRefSrc, process, SrcPid, cloudi_service_validate} ->
            ok
    end,
    true = erlang:is_process_alive(DstPid),
    ok.

t_integer_even_failure_2(_Config) ->
    Context0 = cloudi:new(),
    ServiceNameSrc = ?SERVICE_PREFIX2 ++ ?SERVICE_SUFFIX1,
    ServiceNameDst = ?SERVICE_PREFIX2 ++ ?SERVICE_SUFFIX2,
    {{ok,
      {_, SrcPid} = PatternPidSrc},
     Context1} = cloudi:get_pid(Context0,
                                ServiceNameSrc,
                                immediate),
    {{ok,
      {_, DstPid}},
     Context2} = cloudi:get_pid(Context1,
                                ServiceNameDst,
                                immediate),
    MonitorRefSrc = erlang:monitor(process, SrcPid),
    MonitorRefDst = erlang:monitor(process, DstPid),
    {{ok, _},
     _} = cloudi:send_async(Context2, ServiceNameSrc, 0, 2,
                            undefined, undefined, PatternPidSrc),
    % both the source pid and destination pid
    % are killed due to the response validation
    receive
        {'DOWN', MonitorRefSrc, process, SrcPid, cloudi_service_validate} ->
            ok
    end,
    receive
        {'DOWN', MonitorRefDst, process, DstPid, cloudi_service_validate} ->
            ok
    end,
    ok.

t_integer_even_failure_3(_Config) ->
    Context0 = cloudi:new(),
    ServiceNameSrc = ?SERVICE_PREFIX2 ++ ?SERVICE_SUFFIX1,
    ServiceNameDst = ?SERVICE_PREFIX2 ++ ?SERVICE_SUFFIX2,
    {{ok,
      {_, SrcPid} = PatternPidSrc},
     Context1} = cloudi:get_pid(Context0,
                                ServiceNameSrc,
                                immediate),
    {{ok,
      {_, DstPid}},
     Context2} = cloudi:get_pid(Context1,
                                ServiceNameDst,
                                immediate),
    MonitorRefSrc = erlang:monitor(process, SrcPid),
    {{ok, -1, -1},
     Context3} = cloudi:send_sync(Context2, ServiceNameSrc, 0, 2,
                                  undefined, undefined, PatternPidSrc),
    receive after 2000 -> ok end, % avoid a source failure
    true = erlang:is_process_alive(SrcPid),
    true = erlang:is_process_alive(DstPid),
    {{ok, -1, -1},
     Context4} = cloudi:send_sync(Context3, ServiceNameSrc, 0, 4,
                                  undefined, undefined, PatternPidSrc),
    receive after 2000 -> ok end, % avoid a source failure
    true = erlang:is_process_alive(SrcPid),
    true = erlang:is_process_alive(DstPid),
    {{ok, -1, -1},
     Context5} = cloudi:send_sync(Context4, ServiceNameSrc, 0, 6,
                                  undefined, undefined, PatternPidSrc),
    {{ok, _},
     _} = cloudi:send_async(Context5, ServiceNameSrc, 0, 8,
                            undefined, undefined, PatternPidSrc),
    receive
        {'DOWN', MonitorRefSrc, process, SrcPid, cloudi_service_validate} ->
            ok
    end,
    true = erlang:is_process_alive(DstPid),
    ok.

is_zero(I) ->
    is_integer(I) andalso (I == 0).

is_even(_, I) ->
    is_integer(I) andalso (I rem 2 == 0).

is_odd(_, I) ->
    is_integer(I) andalso (I rem 2 == 1).
