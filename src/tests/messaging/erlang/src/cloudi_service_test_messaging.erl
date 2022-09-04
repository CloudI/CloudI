%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Service for the messaging Test==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2012-2022 Michael Truog <mjtruog at protonmail dot com>
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
%%% @author Michael Truog <mjtruog at protonmail dot com>
%%% @copyright 2012-2022 Michael Truog
%%% @version 2.0.5 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_test_messaging).
-author('mjtruog at protonmail dot com').
-vsn("2.0.5").

-behaviour(cloudi_service).

%% cloudi_service callbacks
-export([cloudi_service_init/4,
         cloudi_service_handle_request/11,
         cloudi_service_terminate/3]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").

-type sequence2_states() :: state1 | state2 | state3 | state4 |
                            state5 | state6 | state7 | state8.
-record(state,
    {
        prefix :: cloudi_service:service_name_pattern(),
        variation :: nonempty_string(),
        sequence2_state = state1 :: sequence2_states(),
        sequence2_recv = recv_async :: recv_async | recv_asyncs
    }).

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(_Args, Prefix, _Timeout, Dispatcher) ->
    [[], Variation | _] = lists:reverse(cloudi_string:split("/", Prefix)),
    cloudi_service:subscribe(Dispatcher, "a/b/c/d"),
    cloudi_service:subscribe(Dispatcher, "a/b/c/*"),
    cloudi_service:subscribe(Dispatcher, "a/b/*/d"),
    cloudi_service:subscribe(Dispatcher, "a/*/c/d"),
    cloudi_service:subscribe(Dispatcher, "*/b/c/d"),
    cloudi_service:subscribe(Dispatcher, "a/b/*"),
    cloudi_service:subscribe(Dispatcher, "a/*/d"),
    cloudi_service:subscribe(Dispatcher, "*/c/d"),
    cloudi_service:subscribe(Dispatcher, "a/*"),
    cloudi_service:subscribe(Dispatcher, "*/d"),
    cloudi_service:subscribe(Dispatcher, "*"),
    cloudi_service:subscribe(Dispatcher, "sequence1"),
    cloudi_service:subscribe(Dispatcher, "e"),
    cloudi_service:subscribe(Dispatcher, "e"),
    cloudi_service:subscribe(Dispatcher, "e"),
    cloudi_service:subscribe(Dispatcher, "e"),
    cloudi_service:subscribe(Dispatcher, "e"),
    cloudi_service:subscribe(Dispatcher, "e"),
    cloudi_service:subscribe(Dispatcher, "e"),
    cloudi_service:subscribe(Dispatcher, "e"),
    8 = cloudi_service:subscribe_count(Dispatcher, "e"),
    cloudi_service:subscribe(Dispatcher, "sequence2"),
    cloudi_service:subscribe(Dispatcher, "f1"),
    cloudi_service:subscribe(Dispatcher, "f2"),
    cloudi_service:subscribe(Dispatcher, "g1"),
    cloudi_service:subscribe(Dispatcher, "sequence3"),
    case cloudi_service:process_index(Dispatcher) of
        0 ->
            ?LOG_TRACE("~p is sending", [?FUNCTION_NAME]),
            {ok, _} = cloudi_service:send_async(Dispatcher,
                                                Prefix ++ "sequence1", "1"),
            ok;
        _ ->
            ok
    end,
    {ok, #state{prefix = Prefix,
                variation = Variation}}.

cloudi_service_handle_request(_RequestType, _Name, Pattern,
                              _RequestInfo, Request,
                              _Timeout, _Priority, _TransId, _Source,
                              #state{prefix = Prefix,
                                     variation = Variation,
                                     sequence2_state = Sequence2State,
                                     sequence2_recv = Sequence2Recv} = State,
                              Dispatcher) ->
    case cloudi_service_name:suffix(Prefix, Pattern) of
        "sequence1" ->
            consume_end_and_sleep(Dispatcher),
            ?LOG_INFO("messaging sequence1 start erlang"
                      " ~s (~s)", [Variation, Request]),
            sequence1(Dispatcher, Prefix),
            ?LOG_INFO("messaging sequence1 end erlang"
                      " ~s (~s)", [Variation, Request]),
            {ok, _} = cloudi_service:send_async(Dispatcher,
                                                Prefix ++ "sequence2",
                                                Request),
            {reply, "end", State};
        "a/b/c/d" ->
            true = Pattern == (Prefix ++ "a/b/c/d"),
            true = Request == <<"test1">>,
            {reply, Request, State};
        "a/b/c/*" ->
            true = Pattern == (Prefix ++ "a/b/c/*"),
            true = (Request == <<"test2">>) orelse (Request == <<"test3">>),
            {reply, Request, State};
        "a/b/*/d" ->
            true = Pattern == (Prefix ++ "a/b/*/d"),
            true = (Request == <<"test4">>) orelse (Request == <<"test5">>),
            {reply, Request, State};
        "a/*/c/d" ->
            true = Pattern == (Prefix ++ "a/*/c/d"),
            true = (Request == <<"test6">>) orelse (Request == <<"test7">>),
            {reply, Request, State};
        "*/b/c/d" ->
            true = Pattern == (Prefix ++ "*/b/c/d"),
            true = (Request == <<"test8">>) orelse (Request == <<"test9">>),
            {reply, Request, State};
        "a/b/*" ->
            true = Pattern == (Prefix ++ "a/b/*"),
            true = Request == <<"test10">>,
            {reply, Request, State};
        "a/*/d" ->
            true = Pattern == (Prefix ++ "a/*/d"),
            true = Request == <<"test11">>,
            {reply, Request, State};
        "*/c/d" ->
            true = Pattern == (Prefix ++ "*/c/d"),
            true = Request == <<"test12">>,
            {reply, Request, State};
        "a/*" ->
            true = Pattern == (Prefix ++ "a/*"),
            true = Request == <<"test13">>,
            {reply, Request, State};
        "*/d" ->
            true = Pattern == (Prefix ++ "*/d"),
            true = Request == <<"test14">>,
            {reply, Request, State};
        "*" ->
            true = Pattern == (Prefix ++ "*"),
            true = Request == <<"test15">>,
            {reply, Request, State};
        "sequence2" ->
            ?LOG_INFO("messaging sequence2 start erlang"
                      " ~s (~s)", [Variation, Request]),
            Sequence2RecvNew = sequence2(Dispatcher, Prefix, Sequence2Recv),
            ?LOG_INFO("messaging sequence2 end erlang"
                      " ~s (~s)", [Variation, Request]),
            {ok, _} = cloudi_service:send_async(Dispatcher,
                                                Prefix ++ "sequence3",
                                                Request),
            {reply, "end", State#state{sequence2_recv = Sequence2RecvNew}};
        "e" ->
            if
                Sequence2State =:= state1 ->
                    {reply, <<"1">>, State#state{sequence2_state = state2}};
                Sequence2State =:= state2 ->
                    {reply, <<"2">>, State#state{sequence2_state = state3}};
                Sequence2State =:= state3 ->
                    {reply, <<"3">>, State#state{sequence2_state = state4}};
                Sequence2State =:= state4 ->
                    {reply, <<"4">>, State#state{sequence2_state = state5}};
                Sequence2State =:= state5 ->
                    {reply, <<"5">>, State#state{sequence2_state = state6}};
                Sequence2State =:= state6 ->
                    {reply, <<"6">>, State#state{sequence2_state = state7}};
                Sequence2State =:= state7 ->
                    {reply, <<"7">>, State#state{sequence2_state = state8}};
                Sequence2State =:= state8 ->
                    {reply, <<"8">>, State#state{sequence2_state = state1}}
            end;
        "sequence3" ->
            ?LOG_INFO("messaging sequence3 start erlang"
                      " ~s (~s)", [Variation, Request]),
            sequence3(Dispatcher, Prefix),
            ?LOG_INFO("messaging sequence3 end erlang"
                      " ~s (~s)", [Variation, Request]),
            Iteration = erlang:list_to_integer(Request) + 1,
            RequestNew = erlang:integer_to_list(Iteration),
            {ok, _} = cloudi_service:send_async(Dispatcher,
                                                Prefix ++ "sequence1",
                                                RequestNew),
            {reply, "end", State};
        "f1" ->
            RequestI = erlang:list_to_integer(Request),
            if
                RequestI == 4 ->
                    {reply, "done", State};
                true ->
                    RequestNew = RequestI + 2, % two steps forward
                    {forward, Prefix ++ "f2", <<>>,
                     cloudi_string:term_to_list(RequestNew), State}
            end;
        "f2" ->
            RequestI = erlang:list_to_integer(Request),
            RequestNew = RequestI - 1, % one step back
            {forward, Prefix ++ "f1", <<>>,
             cloudi_string:term_to_list(RequestNew), State};
        "g1" ->
            {reply, Request ++ "suffix", State}
    end.

cloudi_service_terminate(_Reason, _Timeout,
                         #state{variation = Variation}) ->
    ?LOG_INFO("terminate messaging erlang ~s", [Variation]),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

sequence1(Dispatcher, Prefix) ->
    {ok, Test1Id} = cloudi_service:send_async(Dispatcher, Prefix ++
                                              "a/b/c/d", <<"test1">>),
    {ok, Test2Id} = cloudi_service:send_async(Dispatcher, Prefix ++
                                              "a/b/c/z", <<"test2">>),
    {ok, Test3Id} = cloudi_service:send_async(Dispatcher, Prefix ++
                                              "a/b/c/dd", <<"test3">>),
    {ok, Test4Id} = cloudi_service:send_async(Dispatcher, Prefix ++
                                              "a/b/z/d", <<"test4">>),
    {ok, Test5Id} = cloudi_service:send_async(Dispatcher, Prefix ++
                                              "a/b/cc/d", <<"test5">>),
    {ok, Test6Id} = cloudi_service:send_async(Dispatcher, Prefix ++
                                              "a/z/c/d", <<"test6">>),
    {ok, Test7Id} = cloudi_service:send_async(Dispatcher, Prefix ++
                                              "a/bb/c/d", <<"test7">>),
    {ok, Test8Id} = cloudi_service:send_async(Dispatcher, Prefix ++
                                              "z/b/c/d", <<"test8">>),
    {ok, Test9Id} = cloudi_service:send_async(Dispatcher, Prefix ++
                                              "aa/b/c/d", <<"test9">>),
    {ok, Test10Id} = cloudi_service:send_async(Dispatcher, Prefix ++
                                               "a/b/czd", <<"test10">>),
    {ok, Test11Id} = cloudi_service:send_async(Dispatcher, Prefix ++
                                               "a/bzc/d", <<"test11">>),
    {ok, Test12Id} = cloudi_service:send_async(Dispatcher, Prefix ++
                                               "azb/c/d", <<"test12">>),
    {ok, Test13Id} = cloudi_service:send_async(Dispatcher, Prefix ++
                                               "a/bzczd", <<"test13">>),
    {ok, Test14Id} = cloudi_service:send_async(Dispatcher, Prefix ++
                                               "azbzc/d", <<"test14">>),
    {ok, Test15Id} = cloudi_service:send_async(Dispatcher, Prefix ++
                                               "azbzczd", <<"test15">>),
    % n.b., depends on cloudi_core_i_constants.hrl having
    % RECV_ASYNC_STRATEGY == recv_async_select_oldest
    _ = cloudi_service:recv_async(Dispatcher, Test1Id, false),
    {ok, <<>>, Test1Response, Test1Id} =
        cloudi_service:recv_async(Dispatcher),
    <<"test1">> = Test1Response,
    _ = cloudi_service:recv_async(Dispatcher, Test2Id, false),
    {ok, <<>>, Test2Response, Test2Id} =
        cloudi_service:recv_async(Dispatcher),
    <<"test2">> = Test2Response,
    _ = cloudi_service:recv_async(Dispatcher, Test3Id, false),
    {ok, <<>>, Test3Response, Test3Id} =
        cloudi_service:recv_async(Dispatcher),
    <<"test3">> = Test3Response,
    _ = cloudi_service:recv_async(Dispatcher, Test4Id, false),
    {ok, <<>>, Test4Response, Test4Id} =
        cloudi_service:recv_async(Dispatcher),
    <<"test4">> = Test4Response,
    _ = cloudi_service:recv_async(Dispatcher, Test5Id, false),
    {ok, <<>>, Test5Response, Test5Id} =
        cloudi_service:recv_async(Dispatcher),
    <<"test5">> = Test5Response,
    _ = cloudi_service:recv_async(Dispatcher, Test6Id, false),
    {ok, <<>>, Test6Response, Test6Id} =
        cloudi_service:recv_async(Dispatcher),
    <<"test6">> = Test6Response,
    _ = cloudi_service:recv_async(Dispatcher, Test7Id, false),
    {ok, <<>>, Test7Response, Test7Id} =
        cloudi_service:recv_async(Dispatcher),
    <<"test7">> = Test7Response,
    _ = cloudi_service:recv_async(Dispatcher, Test8Id, false),
    {ok, <<>>, Test8Response, Test8Id} =
        cloudi_service:recv_async(Dispatcher),
    <<"test8">> = Test8Response,
    _ = cloudi_service:recv_async(Dispatcher, Test9Id, false),
    {ok, <<>>, Test9Response, Test9Id} =
        cloudi_service:recv_async(Dispatcher),
    <<"test9">> = Test9Response,
    _ = cloudi_service:recv_async(Dispatcher, Test10Id, false),
    {ok, <<>>, Test10Response, Test10Id} =
        cloudi_service:recv_async(Dispatcher),
    <<"test10">> = Test10Response,
    _ = cloudi_service:recv_async(Dispatcher, Test11Id, false),
    {ok, <<>>, Test11Response, Test11Id} =
        cloudi_service:recv_async(Dispatcher),
    <<"test11">> = Test11Response,
    _ = cloudi_service:recv_async(Dispatcher, Test12Id, false),
    {ok, <<>>, Test12Response, Test12Id} =
        cloudi_service:recv_async(Dispatcher),
    <<"test12">> = Test12Response,
    _ = cloudi_service:recv_async(Dispatcher, Test13Id, false),
    {ok, <<>>, Test13Response, Test13Id} =
        cloudi_service:recv_async(Dispatcher),
    <<"test13">> = Test13Response,
    _ = cloudi_service:recv_async(Dispatcher, Test14Id, false),
    {ok, <<>>, Test14Response, Test14Id} =
        cloudi_service:recv_async(Dispatcher),
    <<"test14">> = Test14Response,
    _ = cloudi_service:recv_async(Dispatcher, Test15Id, false),
    {ok, <<>>, Test15Response, Test15Id} =
        cloudi_service:recv_async(Dispatcher),
    <<"test15">> = Test15Response,
    ok.

sequence2(Dispatcher, Prefix, Sequence2Recv) ->
    % the sending process is excluded from the services that receive
    % the asynchronous message, so in this case, the receiving process
    % will not be called, despite the fact it has subscribed to 'e',
    % to prevent a process from deadlocking with itself.
    {ok, TransIds} = cloudi_service:mcast_async(Dispatcher, Prefix ++ "e", " "),
    % 4 * 8 == 32, but only 3 out of 4 CloudI services can receive messages,
    % since 1 CloudI service is sending the mcast_async, so 3 * 8 == 24
    if
        erlang:length(TransIds) == 24 ->
            % all service processes have finished initialization
            {Sequence2RecvNew,
             L} = sequence2_recv(Sequence2Recv, TransIds, Dispatcher),
            true = L == [<<"1">>, <<"1">>, <<"1">>,
                         <<"2">>, <<"2">>, <<"2">>,
                         <<"3">>, <<"3">>, <<"3">>,
                         <<"4">>, <<"4">>, <<"4">>,
                         <<"5">>, <<"5">>, <<"5">>,
                         <<"6">>, <<"6">>, <<"6">>,
                         <<"7">>, <<"7">>, <<"7">>,
                         <<"8">>, <<"8">>, <<"8">>],
            Sequence2RecvNew;
        true ->
            % service processes have not finished initialization
            ?LOG_WARN("Waiting for ~p services to initialize",
                      [4 - (erlang:length(TransIds) / 8)]),
            {Sequence2RecvNew,
             _} = sequence2_recv(Sequence2Recv, TransIds, Dispatcher),
            % sleep
            {error, timeout} = cloudi_service:recv_async(Dispatcher, 1000),
            % retry
            sequence2(Dispatcher, Prefix, Sequence2RecvNew)
    end.

sequence2_recv(recv_async, TransIds, Dispatcher) ->
    L = lists:foldl(fun(TransId, Results) ->
        {ok, <<>>, Result, _} =
            cloudi_service:recv_async(Dispatcher, TransId),
        lists:merge(Results, [Result])
    end, [], TransIds),
    {recv_asyncs, L};
sequence2_recv(recv_asyncs, TransIds, Dispatcher) ->
    {ok, Recvs} = cloudi_service:recv_asyncs(Dispatcher, TransIds),
    L = lists:foldl(fun({<<>>, Result, _TransId}, Results) ->
        lists:merge(Results, [Result])
    end, [], Recvs),
    {recv_async, L}.

sequence3(Dispatcher, Prefix) ->
    {ok, Test1Id} = cloudi_service:send_async(Dispatcher, Prefix ++ "f1", "0"),
    {ok, <<>>,
     Test1Check, Test1Id} = cloudi_service:recv_async(Dispatcher, Test1Id),
    true = Test1Check == "done",
    {ok, Test2Check} = cloudi_service:send_sync(Dispatcher,
                                                Prefix ++ "g1", "prefix_"),
    true = Test2Check == "prefix_suffix",
    ok.

consume_end_and_sleep(Dispatcher) ->
    case cloudi_service:recv_async(Dispatcher, 1000) of
        {ok, <<>>, "end", _} ->
            consume_end_and_sleep(Dispatcher);
        {error, timeout} ->
            ok
    end.

