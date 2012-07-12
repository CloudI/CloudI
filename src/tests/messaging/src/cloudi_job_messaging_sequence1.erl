%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Work Module For messaging Test (sequence1)==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2012, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2012 Michael Truog
%%% @version 0.2.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_job_messaging_sequence1).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloudi_job).

%% cloudi_job callbacks
-export([cloudi_job_init/3,
         cloudi_job_handle_request/11,
         cloudi_job_handle_info/3,
         cloudi_job_terminate/2]).

-include("cloudi_logger.hrl").

-record(state, {
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------


%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_job
%%%------------------------------------------------------------------------

cloudi_job_init(_Args, Prefix, Dispatcher) ->
    case cloudi_job:process_index(Dispatcher) of
        0 ->
            cloudi_job:send_async(Dispatcher, Prefix ++ "sequence1", "start");
        _ ->
            ok
    end,
    cloudi_job:subscribe(Dispatcher, "a/b/c/d"),
    cloudi_job:subscribe(Dispatcher, "a/b/c/*"),
    cloudi_job:subscribe(Dispatcher, "a/b/*/d"),
    cloudi_job:subscribe(Dispatcher, "a/*/c/d"),
    cloudi_job:subscribe(Dispatcher, "*/b/c/d"),
    cloudi_job:subscribe(Dispatcher, "a/b/*"),
    cloudi_job:subscribe(Dispatcher, "a/*/d"),
    cloudi_job:subscribe(Dispatcher, "*/c/d"),
    cloudi_job:subscribe(Dispatcher, "a/*"),
    cloudi_job:subscribe(Dispatcher, "*/d"),
    cloudi_job:subscribe(Dispatcher, "*"),
    cloudi_job:subscribe(Dispatcher, "sequence1"),
    {ok, #state{}}.

cloudi_job_handle_request(_Type, _Name, Pattern, _RequestInfo, Request,
                          _Timeout, _Priority, _TransId, _Pid,
                          #state{} = State,
                          Dispatcher) ->
    Prefix = cloudi_job:prefix(Dispatcher),
    case Request of
        "start" ->
            ?LOG_INFO("messaging sequence1 start erlang", []),
            sequence1(Dispatcher, Prefix),
            ?LOG_INFO("messaging sequence1 end erlang", []),
            cloudi_job:send_async(Dispatcher, Prefix ++ "sequence2", "start"),
            {reply, "end", State};
        <<"test1">> ->
            true = Pattern == (Prefix ++ "a/b/c/d"),
            {reply, Request, State};
        <<"test2">> ->
            true = Pattern == (Prefix ++ "a/b/c/*"),
            {reply, Request, State};
        <<"test3">> ->
            true = Pattern == (Prefix ++ "a/b/c/*"),
            {reply, Request, State};
        <<"test4">> ->
            true = Pattern == (Prefix ++ "a/b/*/d"),
            {reply, Request, State};
        <<"test5">> ->
            true = Pattern == (Prefix ++ "a/b/*/d"),
            {reply, Request, State};
        <<"test6">> ->
            true = Pattern == (Prefix ++ "a/*/c/d"),
            {reply, Request, State};
        <<"test7">> ->
            true = Pattern == (Prefix ++ "a/*/c/d"),
            {reply, Request, State};
        <<"test8">> ->
            true = Pattern == (Prefix ++ "*/b/c/d"),
            {reply, Request, State};
        <<"test9">> ->
            true = Pattern == (Prefix ++ "*/b/c/d"),
            {reply, Request, State};
        <<"test10">> ->
            true = Pattern == (Prefix ++ "a/b/*"),
            {reply, Request, State};
        <<"test11">> ->
            true = Pattern == (Prefix ++ "a/*/d"),
            {reply, Request, State};
        <<"test12">> ->
            true = Pattern == (Prefix ++ "*/c/d"),
            {reply, Request, State};
        <<"test13">> ->
            true = Pattern == (Prefix ++ "a/*"),
            {reply, Request, State};
        <<"test14">> ->
            true = Pattern == (Prefix ++ "*/d"),
            {reply, Request, State};
        <<"test15">> ->
            true = Pattern == (Prefix ++ "*"),
            {reply, Request, State}
    end.

cloudi_job_handle_info(Request, State, _) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_job_terminate(_, #state{}) ->
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

sequence1(Dispatcher, Prefix) ->
    % n.b., depends on cloudi_constants.hrl having
    % GROUP_NAME_PATTERN_MATCHING defined
    cloudi_job:send_async(Dispatcher, Prefix ++ "a/b/c/d", <<"test1">>),
    cloudi_job:send_async(Dispatcher, Prefix ++ "a/b/c/z", <<"test2">>),
    cloudi_job:send_async(Dispatcher, Prefix ++ "a/b/c/dd", <<"test3">>),
    cloudi_job:send_async(Dispatcher, Prefix ++ "a/b/z/d", <<"test4">>),
    cloudi_job:send_async(Dispatcher, Prefix ++ "a/b/cc/d", <<"test5">>),
    cloudi_job:send_async(Dispatcher, Prefix ++ "a/z/c/d", <<"test6">>),
    cloudi_job:send_async(Dispatcher, Prefix ++ "a/bb/c/d", <<"test7">>),
    cloudi_job:send_async(Dispatcher, Prefix ++ "z/b/c/d", <<"test8">>),
    cloudi_job:send_async(Dispatcher, Prefix ++ "aa/b/c/d", <<"test9">>),
    cloudi_job:send_async(Dispatcher, Prefix ++ "a/b/czd", <<"test10">>),
    cloudi_job:send_async(Dispatcher, Prefix ++ "a/bzc/d", <<"test11">>),
    cloudi_job:send_async(Dispatcher, Prefix ++ "azb/c/d", <<"test12">>),
    cloudi_job:send_async(Dispatcher, Prefix ++ "a/bzczd", <<"test13">>),
    cloudi_job:send_async(Dispatcher, Prefix ++ "azbzc/d", <<"test14">>),
    cloudi_job:send_async(Dispatcher, Prefix ++ "azbzczd", <<"test15">>),
    receive after 5000 -> ok end,
    % n.b., depends on cloudi_constants.hrl having
    % RECV_ASYNC_STRATEGY == recv_async_select_oldest
    {ok, Test1Response} = cloudi_job:recv_async(Dispatcher),
    true = Test1Response == <<"test1">>,
    {ok, Test2Response} = cloudi_job:recv_async(Dispatcher),
    true = Test2Response == <<"test2">>,
    {ok, Test3Response} = cloudi_job:recv_async(Dispatcher),
    true = Test3Response == <<"test3">>,
    {ok, Test4Response} = cloudi_job:recv_async(Dispatcher),
    true = Test4Response == <<"test4">>,
    {ok, Test5Response} = cloudi_job:recv_async(Dispatcher),
    true = Test5Response == <<"test5">>,
    {ok, Test6Response} = cloudi_job:recv_async(Dispatcher),
    true = Test6Response == <<"test6">>,
    {ok, Test7Response} = cloudi_job:recv_async(Dispatcher),
    true = Test7Response == <<"test7">>,
    {ok, Test8Response} = cloudi_job:recv_async(Dispatcher),
    true = Test8Response == <<"test8">>,
    {ok, Test9Response} = cloudi_job:recv_async(Dispatcher),
    true = Test9Response == <<"test9">>,
    {ok, Test10Response} = cloudi_job:recv_async(Dispatcher),
    true = Test10Response == <<"test10">>,
    {ok, Test11Response} = cloudi_job:recv_async(Dispatcher),
    true = Test11Response == <<"test11">>,
    {ok, Test12Response} = cloudi_job:recv_async(Dispatcher),
    true = Test12Response == <<"test12">>,
    {ok, Test13Response} = cloudi_job:recv_async(Dispatcher),
    true = Test13Response == <<"test13">>,
    {ok, Test14Response} = cloudi_job:recv_async(Dispatcher),
    true = Test14Response == <<"test14">>,
    {ok, Test15Response} = cloudi_job:recv_async(Dispatcher),
    true = Test15Response == <<"test15">>,
    ok.
