-module(tinderl).
-export([start/4, stop/0, speak/1, paste/1]).

start(Domain, Email, Password, Room) ->
  spawn(fun() ->
    register(tinderl, self()),
    process_flag(trap_exit, true),
    Cmd = lists:flatten(io_lib:format("ruby ./tinderl.rb ~s ~s ~s ~s", [Domain, Email, Password, Room])),
    Port = open_port({spawn, Cmd}, [{packet, 4}, nouse_stdio, exit_status, binary]),
    port_loop(Port)
  end).

stop() -> tinderl ! stop.

speak(String) when is_list(String) -> speak(list_to_binary(String));
speak(String) when is_binary(String) -> tinderl ! {speak, self(), String}.

paste(String) when is_list(String) -> speak(list_to_binary(String));
paste(String) when is_binary(String) -> tinderl ! {paste, self(), String}.

port_loop(Port) ->
  receive
    {speak, _Caller, String} ->
      Data = term_to_binary({speak, String}),
      Port ! {self(), {command, Data}},

      port_loop(Port);

    {paste, _Caller, String} ->
        Data = term_to_binary({paste, String}),
        Port ! {self(), {command, Data}},

        port_loop(Port);

    stop ->
      Port ! {self(), close},
      receive
        {Port, closed} -> exit(normal)
      end;

    {'EXIT', Port, Reason} ->
      exit({port_terminated,Reason})
  end.