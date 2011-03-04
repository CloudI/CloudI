#!/usr/bin/env escript
main(_) ->
  Cmd = "ruby rerl.rb",
  Port = open_port({spawn, Cmd}, [{packet, 4}, nouse_stdio, exit_status, binary]),
  loop(Port).

send(Port, Message) ->
  io:format("[erlang]  sending: ~p~n", [Message]),
  % can also use ! instead of port_command
  % Port ! { self(), { command, term_to_binary(Message) } }.
  port_command(Port, term_to_binary(Message)).

loop(Port) ->
  receive
    {Port, {data, In}} ->
      Data = binary_to_term(In),
      process(Port, Data);
    Any ->
      io:format("[erlang]    other: ~p~n", [Any])
  end,
  loop(Port).

process(Port, i_am_alive) ->
  io:format("[erlang] ruby is alive~n"),

  send(Port, test),
  send(Port, {atom, symbol}),
  send(Port, {bool, true}),
  send(Port, {number, 1}),
  send(Port, {string, <<"reverse">>}),
  send(Port, {array, [1,2,3]}),
  send(Port, {array, [<<"abc">>, <<"cde">>]}),
  send(Port, {hash, [{key,val}]}),
  send(Port, {object, {1,{2},3,<<"four">>}});

process(Port, Data) ->
  io:format("[erlang] received: ~p~n", [Data]).