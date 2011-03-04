#!/usr/bin/env escript

-export([main/1]).

main(_Any) ->
  gruff:start(),
  MemoryWriter = stat_writer:start(<<"Memory Info">>, fun() -> erlang:memory() end),
  ProcessWriter = stat_writer:start(<<"Process Info">>,
    fun() ->
      {_, QueueLength} = erlang:process_info(erlang:whereis(gruff), message_queue_len),
      [{processes, erlang:system_info(process_count)},
       {gruff_queue_length, QueueLength}]
    end
  ),
  receive
  after 20000 ->
    MemoryWriter ! stop,
    ProcessWriter ! stop,
    elang:halt()
  end.