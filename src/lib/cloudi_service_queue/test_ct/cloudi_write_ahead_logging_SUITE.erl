%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
-module(cloudi_write_ahead_logging_SUITE).

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
-export([t_wal_sequence0/1]).

-include_lib("common_test/include/ct.hrl").

% cloudi_core isn't started, so using error_logger
-define(LOG_TRACE, error_logger:info_msg).

%%%------------------------------------------------------------------------
%%% Callback functions from CT
%%%------------------------------------------------------------------------

all() ->
    [{group, wal_sequence}].

groups() ->
    [{wal_sequence, [],
      [t_wal_sequence0]}].

suite() ->
    [{ct_hooks, [cth_surefire]},
     {timetrap, 5100}].

init_per_suite(Config) ->
    ok = cloudi_x_reltool_util:application_start(cloudi_x_uuid, [], infinity),
    Config.

end_per_suite(_Config) ->
    ok = cloudi_x_reltool_util:application_stop(cloudi_x_uuid),
    ok.

group(_GroupName) ->
    [].

init_per_group(_GroupName, Config) ->
    [{file, "./queue.log"} | Config].

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

%%%------------------------------------------------------------------------
%%% test cases
%%%------------------------------------------------------------------------

t_wal_sequence0(Config) ->
    UUID = cloudi_x_uuid:new(self()),
    SendF0 = fun({_, _, _, _, _, _, _, _, _, _}) ->
        erlang:exit(queue_file_not_empty)
    end,
    State0 = cloudi_write_ahead_logging:new(?config(file, Config), SendF0),
    % request0
    ChunkRequest0 = {"request0",
                     undefined, undefined, undefined, undefined, undefined,
                     5000, undefined, cloudi_x_uuid:get_v1(UUID), undefined},
    {Chunk0, State1} = cloudi_write_ahead_logging:store_start(ChunkRequest0,
                                                              State0),
    ChunkRequestId0 = cloudi_x_uuid:get_v1(UUID),
    State2 = cloudi_write_ahead_logging:store_end(ChunkRequestId0,
                                                  Chunk0, State1),
    % request1
    ChunkRequest1 = {"request1",
                     undefined, undefined, undefined, undefined, undefined,
                     5000, undefined, cloudi_x_uuid:get_v1(UUID), undefined},
    {Chunk1, State3} = cloudi_write_ahead_logging:store_start(ChunkRequest1,
                                                              State2),
    ChunkRequestId1 = cloudi_x_uuid:get_v1(UUID),
    State4 = cloudi_write_ahead_logging:store_end(ChunkRequestId1,
                                                  Chunk1, State3),
    % request2
    ChunkRequest2 = {"request2",
                     undefined, undefined, undefined, undefined, undefined,
                     5000, undefined, cloudi_x_uuid:get_v1(UUID), undefined},
    {Chunk2, State5} = cloudi_write_ahead_logging:store_start(ChunkRequest2,
                                                              State4),
    ChunkRequestId2 = cloudi_x_uuid:get_v1(UUID),
    State6 = cloudi_write_ahead_logging:store_end(ChunkRequestId2,
                                                  Chunk2, State5),
    % request1 completes
    {{"request1", _, _, _, _, _, _, _, _, _},
     State7} = cloudi_write_ahead_logging:erase(ChunkRequestId1, State6),
    % request3
    ChunkRequest3 = {"request3",
                     undefined, undefined, undefined, undefined, <<0:512>>,
                     5000, undefined, cloudi_x_uuid:get_v1(UUID), undefined},
    {Chunk3, State8} = cloudi_write_ahead_logging:store_start(ChunkRequest3,
                                                              State7),
    ChunkRequestId3 = cloudi_x_uuid:get_v1(UUID),
    State9 = cloudi_write_ahead_logging:store_end(ChunkRequestId3,
                                                  Chunk3, State8),
    % request0 completes
    {{"request0", _, _, _, _, _, _, _, _, _},
     State10} = cloudi_write_ahead_logging:erase(ChunkRequestId0, State9),
    % request4 fails
    ChunkRequest4 = {"request4",
                     undefined, undefined, undefined, undefined, undefined,
                     5000, undefined, cloudi_x_uuid:get_v1(UUID), undefined},
    {Chunk4, State11} = cloudi_write_ahead_logging:store_start(ChunkRequest4,
                                                               State10),
    State12 = cloudi_write_ahead_logging:store_fail(Chunk4, State11),
    % request5
    ChunkRequest5 = {"request5",
                     undefined, undefined, undefined, undefined, undefined,
                     5000, undefined, cloudi_x_uuid:get_v1(UUID), undefined},
    {Chunk5, State13} = cloudi_write_ahead_logging:store_start(ChunkRequest5,
                                                               State12),
    ChunkRequestId5 = cloudi_x_uuid:get_v1(UUID),
    State14 = cloudi_write_ahead_logging:store_end(ChunkRequestId5,
                                                   Chunk5, State13),
    % request6
    ChunkRequest6 = {"request6",
                     undefined, undefined, undefined, undefined, undefined,
                     5000, undefined, cloudi_x_uuid:get_v1(UUID), undefined},
    {Chunk6, State15} = cloudi_write_ahead_logging:store_start(ChunkRequest6,
                                                               State14),
    ChunkRequestId6 = cloudi_x_uuid:get_v1(UUID),
    _State16 = cloudi_write_ahead_logging:store_end(ChunkRequestId6,
                                                    Chunk6, State15),
    % at this point, the requests (oldest to newest) are:
    % request2, request3, request5, request6

    % restart1:
    SendF1 = fun({_QueueName, _Type, _Name, _Pattern, _RequestInfo, _Request,
                  Timeout, _Priority, TransId, _Pid} = T) ->
        Age = (cloudi_x_uuid:get_v1_time(erlang) -
               cloudi_x_uuid:get_v1_time(TransId)) div 1000, % milliseconds
        if
            Age >= Timeout ->
                ?LOG_TRACE("new1: ~p (age ~p ms): timeout", [T, Age]),
                {error, timeout};
            true ->
                NewTransId = cloudi_x_uuid:get_v1(UUID),
                ?LOG_TRACE("new1: ~p (age ~p ms):~n"
                           "    resend ~p", [T, Age, NewTransId]),
                {ok, NewTransId}
        end
    end,
    _State17 = cloudi_write_ahead_logging:new(?config(file, Config), SendF1),

    % restart2:
    SendF2 = fun({_QueueName, _Type, _Name, _Pattern, _RequestInfo, _Request,
                  Timeout, _Priority, TransId, _Pid} = T) ->
        Age = (cloudi_x_uuid:get_v1_time(erlang) -
               cloudi_x_uuid:get_v1_time(TransId)) div 1000, % milliseconds
        if
            Age >= Timeout ->
                ?LOG_TRACE("new2: ~p (age ~p ms): timeout", [T, Age]),
                {error, timeout};
            true ->
                NewTransId = cloudi_x_uuid:get_v1(UUID),
                ?LOG_TRACE("new2: ~p (age ~p ms):~n"
                           "    resend ~p", [T, Age, NewTransId]),
                {ok, NewTransId}
        end
    end,
    State18 = cloudi_write_ahead_logging:new(?config(file, Config), SendF2),
    % request7
    ChunkRequest7 = {"request7",
                     undefined, undefined, undefined, undefined, undefined,
                     5000, undefined, cloudi_x_uuid:get_v1(UUID), undefined},
    {Chunk7, State19} = cloudi_write_ahead_logging:store_start(ChunkRequest7,
                                                               State18),
    ChunkRequestId7 = cloudi_x_uuid:get_v1(UUID),
    State20 = cloudi_write_ahead_logging:store_end(ChunkRequestId7,
                                                   Chunk7, State19),
    % order is based on file order
    [NewChunkRequestId5,
     NewChunkRequestId6,
     NewChunkRequestId2,
     NewChunkRequestId3,
     NewChunkRequestId7] = cloudi_write_ahead_logging:fetch_keys(State20),
    % request2 completes
    {{"request2", _, _, _, _, _, _, _, _, _},
     State21} = cloudi_write_ahead_logging:erase(NewChunkRequestId2, State20),
    true = (cloudi_write_ahead_logging:size(State21) == 4),
    true = (cloudi_write_ahead_logging:size_free(State21) == 1),
    % request5 completes
    {{"request5", _, _, _, _, _, _, _, _, _},
     State22} = cloudi_write_ahead_logging:erase(NewChunkRequestId5, State21),
    true = (cloudi_write_ahead_logging:size(State22) == 3),
    true = (cloudi_write_ahead_logging:size_free(State22) == 2),
    % request7 completes
    {{"request7", _, _, _, _, _, _, _, _, _},
     State23} = cloudi_write_ahead_logging:erase(NewChunkRequestId7, State22),
    true = (cloudi_write_ahead_logging:size(State23) == 2),
    true = (cloudi_write_ahead_logging:size_free(State23) == 2),
    % request3 completes
    {{"request3", _, _, _, _, _, _, _, _, _},
     State24} = cloudi_write_ahead_logging:erase(NewChunkRequestId3, State23),
    true = (cloudi_write_ahead_logging:size(State24) == 1),
    true = (cloudi_write_ahead_logging:size_free(State24) == 2),
    % request6 completes
    {{"request6", _, _, _, _, _, _, _, _, _},
     State25} = cloudi_write_ahead_logging:erase(NewChunkRequestId6, State24),
    true = (cloudi_write_ahead_logging:size(State25) == 0),
    true = (cloudi_write_ahead_logging:size_free(State25) == 3),

    % restart3
    SendF3 = fun({_, _, _, _, _, _, _, _, _, _}) ->
        erlang:exit(queue_file_not_empty)
    end,
    State26 = cloudi_write_ahead_logging:new(?config(file, Config), SendF3),
    true = (cloudi_write_ahead_logging:fetch_keys(State26) == []),
    true = (cloudi_write_ahead_logging:size_free(State26) == 3),

    % finish
    ok = file:delete(?config(file, Config)),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

