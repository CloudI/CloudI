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
         kill_group/2,
         shell/1,
         shell/2,
         signal_to_integer/1,
         signal_to_string/1]).

-type signal() ::
    sighup | sigint | sigquit | sigill | sigtrap | sigabrt | sigbus |
    sigfpe | sigkill | sigusr1 | sigsegv | sigusr2 | sigpipe | sigalrm |
    sigterm | sigstkflt | sigchld | sigcont | sigstop | sigtstp |
    sigttin | sigttou | sigurg | sigxcpu | sigxfsz | sigvtalrm | sigprof |
    sigwinch | sigio | sigpwr | sigsys | pos_integer().
-export_type([signal/0]).

-include("cloudi_core_i_constants.hrl").
-ifdef(CLOUDI_CORE_STANDALONE).
-compile({nowarn_unused_function,
          [{os_spawn_kill_pids, 3}]}).
-else.
-compile({nowarn_unused_function,
          [{shell_ok, 1},
           {ospids_to_iolist, 1},
           {ospid_to_list, 1},
           {ospgids_to_iolist, 1},
           {ospgid_to_list, 1}]}).
-endif.

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Kill OS processes.===
%% @end
%%-------------------------------------------------------------------------

-spec kill(Signal :: signal(),
           OSPids :: pos_integer() | list(pos_integer())) ->
    ok |
    {error, any()}.

-ifdef(CLOUDI_CORE_STANDALONE).
kill(Signal, OSPids) ->
    shell_ok(["kill -",
              erlang:integer_to_list(signal_to_integer(Signal)) |
              ospids_to_iolist(OSPids)]).
-else.
kill(Signal, OSPids) ->
    os_spawn_kill_pids(signal_to_integer(Signal), false, OSPids).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Kill OS process groups.===
%% @end
%%-------------------------------------------------------------------------

-spec kill_group(Signal :: signal(),
                 OSPids :: pos_integer() | list(pos_integer())) ->
    ok |
    {error, any()}.

-ifdef(CLOUDI_CORE_STANDALONE).
kill_group(Signal, OSPids) ->
    shell_ok(["kill -",
              erlang:integer_to_list(signal_to_integer(Signal)) |
              ospgids_to_iolist(OSPids)]).
-else.
kill_group(Signal, OSPids) ->
    os_spawn_kill_pids(signal_to_integer(Signal), true, OSPids).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Execute a shell command.===
%% @end
%%-------------------------------------------------------------------------

-spec shell(Exec :: iodata()) ->
    {non_neg_integer(), list(binary())}.

shell(Exec) ->
    Shell = erlang:open_port({spawn_executable, "/bin/sh"},
                             [{args, ["-"]}, {cd, "/"},
                              stream, binary, stderr_to_stdout, exit_status]),
    true = erlang:port_command(Shell, [Exec, "\nexit $?\n"]),
    shell_output(Shell, []).

%%-------------------------------------------------------------------------
%% @doc
%% ===Execute a shell command with a string format.===
%% @end
%%-------------------------------------------------------------------------

-spec shell(Command :: string(),
            Arguments :: list()) ->
    {non_neg_integer(), list(binary())}.

shell(Command, Arguments) ->
    shell(io_lib:format(Command, Arguments)).

%%-------------------------------------------------------------------------
%% @doc
%% ===Convert a signal to an integer.===
%% @end
%%-------------------------------------------------------------------------

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
signal_to_integer(Signal) when is_integer(Signal), Signal > 0 ->
    Signal.

%%-------------------------------------------------------------------------
%% @doc
%% ===Convert a signal integer to a string.===
%% Only signal integers that are consistent among all platforms use a
%% specific string.
%% @end
%%-------------------------------------------------------------------------

-spec signal_to_string(Signal :: pos_integer()) ->
    string().

signal_to_string( 1) -> "SIGHUP";
signal_to_string( 2) -> "SIGINT";
signal_to_string( 3) -> "SIGQUIT";
signal_to_string( 4) -> "SIGILL";
signal_to_string( 5) -> "SIGTRAP";
signal_to_string( 6) -> "SIGABRT";
signal_to_string( 8) -> "SIGFPE";
signal_to_string( 9) -> "SIGKILL";
signal_to_string(11) -> "SIGSEGV";
signal_to_string(13) -> "SIGPIPE";
signal_to_string(14) -> "SIGALRM";
signal_to_string(15) -> "SIGTERM";
signal_to_string(Signal) when is_integer(Signal), Signal > 0 ->
    "SIG#" ++ erlang:integer_to_list(Signal).

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

shell_ok(Exec) ->
    case shell(Exec) of
        {0, _} ->
            ok;
        {_, Output} ->
            {error, Output}
    end.

os_spawn_kill_pids(SignalInteger, Group, OSPids)
    when is_integer(OSPids) ->
    os_spawn_kill_pids(SignalInteger, Group, [OSPids]);
os_spawn_kill_pids(SignalInteger, Group, OSPids)
    when is_integer(SignalInteger), is_boolean(Group), is_list(OSPids) ->
    SpawnProcess = cloudi_x_supool:get(?OS_SPAWN_POOL2),
    case cloudi_core_i_os_spawn:kill_pids(SpawnProcess,
                                          SignalInteger, Group, OSPids) of
        {ok, ErrorString} ->
            if
                ErrorString == "" ->
                    ok;
                is_list(ErrorString) ->
                    {error, ErrorString}
            end;
        {error, _} = Error ->
            Error
    end.

ospids_to_iolist(OSPids)
    when is_list(OSPids) ->
    [ospid_to_list(OSPid) || OSPid <- OSPids];
ospids_to_iolist(OSPids) ->
    ospid_to_list(OSPids).

ospid_to_list(OSPid)
    when is_integer(OSPid), OSPid > 0 ->
    [$  | erlang:integer_to_list(OSPid)].

ospgids_to_iolist(OSPgids)
    when is_list(OSPgids) ->
    [ospgid_to_list(OSPgid) || OSPgid <- OSPgids];
ospgids_to_iolist(OSPgids) ->
    ospgid_to_list(OSPgids).

ospgid_to_list(OSPgid)
    when is_integer(OSPgid), OSPgid > 1 ->
    [$ , $- | erlang:integer_to_list(OSPgid)].
