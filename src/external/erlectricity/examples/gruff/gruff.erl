-module(gruff).
-export([start/0, stop/0, plot/4]).

start() ->
  spawn(fun() ->
    register(gruff, self()),
    process_flag(trap_exit, true),
    Cmd = "ruby ./gruff_provider.rb",
    Port = open_port({spawn, Cmd}, [{packet, 4}, nouse_stdio, exit_status, binary]),
    port_loop(Port)
  end).

stop() -> gruff ! stop.

plot(Name, Font, Data, Labels) ->
  gruff ! {plot, self(), Name, Font, Data, Labels},
  receive
    {result, Bin} -> Bin
  end.

send_data(_Port, []) -> ok;
send_data(Port, [{Name, Points}|Rest]) ->
  Data = {data, Name, Points},
  Port ! {self(), {command, term_to_binary(Data)}},
  send_data(Port, Rest).

send_labels(Port, Labels) ->
  Data = {labels, Labels},
  Port ! {self(), {command, term_to_binary(Data)}}.

port_loop(Port) ->
  receive
    {plot, Caller, Name, Font, Data, Labels} ->
      PlotData = term_to_binary({plot, Name, 'Line', Font}),
      Port ! {self(), {command, PlotData}},

      send_data(Port, Data),
      send_labels(Port, Labels),

      EndData = term_to_binary('end'),
      Port ! {self(), {command, EndData}},
      Result = get_result(Port),
      Caller ! {result, Result },

      port_loop(Port);

    stop ->
      Port ! {self(), close},
      receive
        {Port, closed} -> exit(normal)
      end
  end.

get_result(Port) ->
  receive
    {Port, {data, Data}} ->
      {result, Bin} = binary_to_term(Data),
      Bin;
    {'EXIT', Port, Reason} ->
      exit({port_terminated,Reason})
    end.