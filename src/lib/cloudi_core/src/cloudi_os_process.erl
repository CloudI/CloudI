%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI OS Process Functionality==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2020 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2020 Michael Truog
%%% @version 2.0.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_os_process).
-author('mjtruog at protonmail dot com').

%% external interface
-export([kill/2,
         shell/1,
         shell/2,
         signal_to_integer/1]).

-type signal() ::
    sighup | sigint | sigquit | sigill | sigtrap | sigabrt | sigbus |
    sigfpe | sigkill | sigusr1 | sigsegv | sigusr2 | sigpipe | sigalrm |
    sigterm | sigstkflt | sigchld | sigcont | sigstop | sigtstp |
    sigttin | sigttou | sigurg | sigxcpu | sigxfsz | sigvtalrm | sigprof |
    sigwinch | sigio | sigpwr | sigsys | pos_integer().
-export_type([signal/0]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

-spec kill(Signal :: signal(),
           OSPids :: pos_integer() | list(pos_integer())) ->
    ok |
    {error, list(binary())}.

kill(Signal, OSPids) ->
    SignalInteger = signal_to_integer(Signal),
    OSPidsString = ospids_to_iolist(OSPids),
    Exec = ["kill -", erlang:integer_to_list(SignalInteger) | OSPidsString],
    case shell(Exec) of
        {0, _} ->
            ok;
        {_, Output} ->
            {error, Output}
    end.

-spec shell(Exec :: iodata()) ->
    {non_neg_integer(), list(binary())}.

shell(Exec) ->
    Shell = erlang:open_port({spawn_executable, "/bin/sh"},
                             [{args, ["-"]}, {cd, "/"},
                              stream, binary, stderr_to_stdout, exit_status]),
    true = erlang:port_command(Shell, [Exec, "\nexit $?\n"]),
    shell_output(Shell, []).

-spec shell(Command :: string(),
            Arguments :: list()) ->
    {non_neg_integer(), list(binary())}.

shell(Command, Arguments) ->
    shell(io_lib:format(Command, Arguments)).

-spec signal_to_integer(Signal :: signal()) ->
    pos_integer().

signal_to_integer(sighup   ) -> 1;
signal_to_integer(sigint   ) -> 2;
signal_to_integer(sigquit  ) -> 3;
signal_to_integer(sigill   ) -> 4;
signal_to_integer(sigtrap  ) -> 5;
signal_to_integer(sigabrt  ) -> 6;
signal_to_integer(sigbus   ) -> 7;
signal_to_integer(sigfpe   ) -> 8;
signal_to_integer(sigkill  ) -> 9;
signal_to_integer(sigusr1  ) -> 10;
signal_to_integer(sigsegv  ) -> 11;
signal_to_integer(sigusr2  ) -> 12;
signal_to_integer(sigpipe  ) -> 13;
signal_to_integer(sigalrm  ) -> 14;
signal_to_integer(sigterm  ) -> 15;
signal_to_integer(sigstkflt) -> 16;
signal_to_integer(sigchld  ) -> 17;
signal_to_integer(sigcont  ) -> 18;
signal_to_integer(sigstop  ) -> 19;
signal_to_integer(sigtstp  ) -> 20;
signal_to_integer(sigttin  ) -> 21;
signal_to_integer(sigttou  ) -> 22;
signal_to_integer(sigurg   ) -> 23;
signal_to_integer(sigxcpu  ) -> 24;
signal_to_integer(sigxfsz  ) -> 25;
signal_to_integer(sigvtalrm) -> 26;
signal_to_integer(sigprof  ) -> 27;
signal_to_integer(sigwinch ) -> 28;
signal_to_integer(sigio    ) -> 29;
signal_to_integer(sigpwr   ) -> 30;
signal_to_integer(sigsys   ) -> 31;
signal_to_integer(Signal) when is_integer(Signal), Signal > 0 -> Signal.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

shell_output(Shell, Output) ->
    receive
        {Shell, {data, Data}} ->
            shell_output(Shell, [Data | Output]);
        {Shell, {exit_status, Status}} ->
            {Status, lists:reverse(Output)}
    end.

ospids_to_iolist(OSPids)
    when is_list(OSPids) ->
    [ospid_to_list(OSPid) || OSPid <- OSPids];
ospids_to_iolist(OSPids) ->
    ospid_to_list(OSPids).

ospid_to_list(OSPid)
    when is_integer(OSPid), OSPid > 0 ->
    [$  | erlang:integer_to_list(OSPid)].
