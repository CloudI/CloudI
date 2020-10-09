%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==OS Process Port==
%%% Common source code for Erlang port processes used by CloudI.
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2011-2020 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2011-2020 Michael Truog
%%% @version 2.0.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_core_i_os_port).
-author('mjtruog at protonmail dot com').

%% external interface
-export([sync/3,
         init/4,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("cloudi_core_i_constants.hrl").

-type output_handler() ::
    fun((OSPid :: pos_integer(), Output :: string()) -> ok).
-type stdout_handler() :: output_handler().
-type stderr_handler() :: output_handler().
-export_type([stdout_handler/0,
              stderr_handler/0]).

-type client() :: {pid(),any()}.
-record(state,
    {
        file_name :: string(),
        port :: port(),
        output_newline :: boolean(),
        stdout_handler :: stdout_handler(),
        stderr_handler :: stderr_handler(),
        replies = [] :: list({non_neg_integer(), client()})
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

-spec sync(Process :: pid(),
           Command :: pos_integer(),
           Message :: iolist()) ->
    {ok, any()} | {error, any()}.

sync(Process, Command, Message) ->
    true = is_integer(Command) andalso (Command > 0),
    true = is_list(Message),
    try gen_server:call(Process, {call, Command, Message}, infinity)
    catch
        _:Reason ->
            {error, Reason}
    end.

init(undefined, _, _, _) ->
    ignore;
init(FileName, OutputNewline, StdOutHandler, StdErrHandler) ->
    {ok, FilePath} = load_path(FileName),
    Port = erlang:open_port({spawn, FilePath},
                            [{packet, 4}, binary,
                             exit_status, nouse_stdio]),
    true = is_port(Port),
    true = is_boolean(OutputNewline),
    true = is_function(StdOutHandler, 2),
    true = is_function(StdErrHandler, 2),
    false = erlang:process_flag(trap_exit, true),
    {ok, #state{file_name = FileName,
                port = Port,
                output_newline = OutputNewline,
                stdout_handler = StdOutHandler,
                stderr_handler = StdErrHandler}}.

handle_call({call, Command, Message}, Client,
            #state{port = Port,
                   replies = Replies} = State) ->
    case call_port(Port, Message) of
        ok ->
            {noreply, State#state{replies = Replies ++ [{Command, Client}]}};
        {error, badarg} ->
            {stop, "invalid request", State}
    end;

handle_call(Request, _, State) ->
    {stop, cloudi_string:format("Unknown call \"~w\"", [Request]),
     error, State}.

handle_cast(Request, State) ->
    {stop, cloudi_string:format("Unknown cast \"~w\"", [Request]), State}.

handle_info({Port, {data, Data}},
            #state{port = Port,
                   output_newline = OutputNewline,
                   stdout_handler = StdOutHandler,
                   stderr_handler = StdErrHandler} = State) ->
    case erlang:binary_to_term(Data, [safe]) of
        {Command, Success} ->
            call_port_reply(Command, {ok, Success}, State);
        {error, 0, Reason} ->
            {stop, Reason, State};
        {error, Command, Reason} ->
            call_port_reply(Command, {error, Reason}, State);
        {stdout, OSPid, Output} ->
            ok = StdOutHandler(OSPid, trim_output(OutputNewline, Output)),
            {noreply, State};
        {stderr, OSPid, Output} ->
            ok = StdErrHandler(OSPid, trim_output(OutputNewline, Output)),
            {noreply, State}
    end;

handle_info({Port, {exit_status, Status}},
            #state{port = Port} = State) ->
    {stop, "port exited with " ++ exit_status_to_string(Status), State};

handle_info({'EXIT', Port, PosixCode},
            #state{port = Port} = State) ->
    {stop, port_exit_to_string(PosixCode), State};

handle_info(Request, State) ->
    {stop, cloudi_string:format("Unknown info \"~w\"", [Request]), State}.

terminate(Reason, #state{port = Port} = State) ->
    ok = terminate_now(Reason, State),
    _ = (catch erlang:port_close(Port)),
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

load_path(FileName)
    when is_list(FileName) ->
    case cloudi_core_i_app:test() of
        true ->
            CloudICorePath = [_ | _] = code:lib_dir(cloudi_core),
            Path = filename:join(CloudICorePath, "cxx_src"),
            load_path_check(Path, FileName);
        false ->
            case code:priv_dir(cloudi_core) of
                {error, _} ->
                    {error, enotdir};
                Path ->
                    load_path_check(Path, FileName)
            end
    end.

load_path_check(Path, FileName) ->
    FilePath = filename:join(Path, FileName),
    case file:read_file_info(FilePath, [raw]) of
        {ok, _} ->
            {ok, FilePath};
        _ ->
            {error, enoent}
    end.

call_port(Port, Message)
    when is_port(Port), is_list(Message) ->
    try erlang:port_command(Port, Message) of
        true ->
            ok
    catch
        error:_ ->
            {error, badarg}
    end.

call_port_reply(Command, Reply,
                #state{replies = Replies} = State) ->
    case lists:keytake(Command, 1, Replies) of
        false ->
            {stop, "invalid reply", State};
        {value, {Command, Client}, RepliesNew} ->
            _ = gen_server:reply(Client, Reply),
            {noreply, State#state{replies = RepliesNew}}
    end.

trim_output(true, Output) ->
    Output;
trim_output(false, Output) ->
    % just consume the last newline character, if one exists
    case lists:reverse(Output) of
        [10 | OutputReversed] ->
            lists:reverse(OutputReversed);
        _ ->
            Output
    end.

terminate_now(Reason, State)
    when Reason =:= shutdown;
         element(1, Reason) =:= shutdown ->
    % ensure the Erlang port is ready to terminate with the terminate_now
    % function that is always the Command == 1 for CloudI Erlang port processes
    TerminateTimeMax = cloudi_timestamp:milliseconds_monotonic() +
                       ?TIMEOUT_TERMINATE_MAX,
    terminate_now_wait(TerminateTimeMax, State);
terminate_now(_, _) ->
    ok.

terminate_now_wait(TerminateTimeMax,
                   #state{port = Port} = State) ->
    case call_port(Port, [<<1:16/unsigned-integer-native>>]) of
        ok ->
            terminate_now_wait_response(TerminateTimeMax, State);
        {error, badarg} ->
            ok
    end.

terminate_now_wait_response(TerminateTimeMax,
                            #state{port = Port,
                                   output_newline = OutputNewline,
                                   stdout_handler = StdOutHandler,
                                   stderr_handler = StdErrHandler} = State) ->
    receive
        {Port, {data, Data}} ->
            case erlang:binary_to_term(Data, [safe]) of
                {1, TerminateNow} ->
                    terminate_now_wait_check(TerminateNow,
                                             TerminateTimeMax, State);
                {_, _} ->
                    terminate_now_wait_response(TerminateTimeMax, State);
                {error, _, _} ->
                    ok;
                {stdout, OSPid, Output} ->
                    ok = StdOutHandler(OSPid,
                                       trim_output(OutputNewline, Output)),
                    terminate_now_wait_response(TerminateTimeMax, State);
                {stderr, OSPid, Output} ->
                    ok = StdErrHandler(OSPid,
                                       trim_output(OutputNewline, Output)),
                    terminate_now_wait_response(TerminateTimeMax, State)
            end;
        {Port, {exit_status, _}} ->
            ok;
        {'EXIT', Port, _} ->
            ok
    end.

terminate_now_wait_check(true, _, _) ->
    ok;
terminate_now_wait_check(false, TerminateTimeMax, State) ->
    RemainingMilliSeconds = TerminateTimeMax -
                            cloudi_timestamp:milliseconds_monotonic(),
    if
        RemainingMilliSeconds > 1500 ->
            receive after 1000 -> ok end,
            terminate_now_wait(TerminateTimeMax, State);
        RemainingMilliSeconds > 0 ->
            receive after RemainingMilliSeconds -> ok end;
        true ->
            ok
    end.

port_exit_to_string(eacces) ->
    "permission denied";
port_exit_to_string(eagain) ->
    "resource temporarily unavailable";
port_exit_to_string(ebadf) ->
    "bad file number";
port_exit_to_string(ebusy) ->
    "file busy";
port_exit_to_string(edquot) ->
    "disk quota exceeded";
port_exit_to_string(eexist) ->
    "file already exists";
port_exit_to_string(efault) ->
    "bad address in system call argument";
port_exit_to_string(efbig) ->
    "file too large";
port_exit_to_string(eintr) ->
    "interrupted system call";
port_exit_to_string(einval) ->
    "invalid argument";
port_exit_to_string(eio) ->
    "IO error";
port_exit_to_string(eisdir) ->
    "illegal operation on a directory";
port_exit_to_string(eloop) ->
    "too many levels of symbolic links";
port_exit_to_string(emfile) ->
    "too many open files";
port_exit_to_string(emlink) ->
    "too many links";
port_exit_to_string(enametoolong) ->
    "file name too long";
port_exit_to_string(enfile) ->
    "file table overflow";
port_exit_to_string(enodev) ->
    "no such device";
port_exit_to_string(enoent) ->
    "no such file or directory";
port_exit_to_string(enomem) ->
    "not enough memory";
port_exit_to_string(enospc) ->
    "no space left on device";
port_exit_to_string(enotblk) ->
    "block device required";
port_exit_to_string(enotdir) ->
    "not a directory";
port_exit_to_string(enotsup) ->
    "operation not supported";
port_exit_to_string(enxio) ->
    "no such device or address";
port_exit_to_string(eperm) ->
    "not owner";
port_exit_to_string(epipe) ->
    "broken pipe";
port_exit_to_string(erofs) ->
    "read-only file system";
port_exit_to_string(espipe) ->
    "invalid seek";
port_exit_to_string(esrch) ->
    "no such process";
port_exit_to_string(estale) ->
    "stale remote file handle";
port_exit_to_string(exdev) ->
    "cross-domain link";
port_exit_to_string(Other) when is_atom(Other) ->
    erlang:atom_to_list(Other).

exit_status_to_string(  0) -> "no error occurred";
%% GEPD exit status values (from InternalExitStatus in port.cpp)
%% exit_status >= GEPD::ExitStatus::errors_min (from port.hpp)
%% (GEPD::ExitStatus::errors_min == 80)
exit_status_to_string( 80) -> "erlang exited";
exit_status_to_string( 81) -> "erlang port read EAGAIN";
exit_status_to_string( 82) -> "erlang port read EBADF";
exit_status_to_string( 83) -> "erlang port read EFAULT";
exit_status_to_string( 84) -> "erlang port read EINTR";
exit_status_to_string( 85) -> "erlang port read EINVAL";
exit_status_to_string( 86) -> "erlang port read EIO";
exit_status_to_string( 87) -> "erlang port read EISDIR";
exit_status_to_string( 88) -> "erlang port read null";
exit_status_to_string( 89) -> "erlang port read overflow";
exit_status_to_string( 90) -> "erlang port read unknown";
exit_status_to_string( 91) -> "erlang port write EAGAIN";
exit_status_to_string( 92) -> "erlang port write EBADF";
exit_status_to_string( 93) -> "erlang port write EFAULT";
exit_status_to_string( 94) -> "erlang port write EFBIG";
exit_status_to_string( 95) -> "erlang port write EINTR";
exit_status_to_string( 96) -> "erlang port write EINVAL";
exit_status_to_string( 97) -> "erlang port write EIO";
exit_status_to_string( 98) -> "erlang port write ENOSPC";
exit_status_to_string( 99) -> "erlang port write EPIPE";
exit_status_to_string(100) -> "erlang port write null";
exit_status_to_string(101) -> "erlang port write overflow";
exit_status_to_string(102) -> "erlang port write unknown";
exit_status_to_string(103) -> "erlang port ei_encode_error";
exit_status_to_string(104) -> "erlang port poll EBADF";
exit_status_to_string(105) -> "erlang port poll EFAULT";
exit_status_to_string(106) -> "erlang port poll EINTR";
exit_status_to_string(107) -> "erlang port poll EINVAL";
exit_status_to_string(108) -> "erlang port poll ENOMEM";
exit_status_to_string(109) -> "erlang port poll ERR";
exit_status_to_string(110) -> "erlang port poll HUP";
exit_status_to_string(111) -> "erlang port poll NVAL";
exit_status_to_string(112) -> "erlang port poll unknown";
exit_status_to_string(113) -> "erlang port pipe EFAULT";
exit_status_to_string(114) -> "erlang port pipe EINVAL";
exit_status_to_string(115) -> "erlang port pipe EMFILE";
exit_status_to_string(116) -> "erlang port pipe ENFILE";
exit_status_to_string(117) -> "erlang port pipe unknown";
exit_status_to_string(118) -> "erlang port dup EBADF";
exit_status_to_string(119) -> "erlang port dup EBUSY";
exit_status_to_string(120) -> "erlang port dup EINTR";
exit_status_to_string(121) -> "erlang port dup EINVAL";
exit_status_to_string(122) -> "erlang port dup EMFILE";
exit_status_to_string(123) -> "erlang port dup unknown";
exit_status_to_string(124) -> "erlang port close EBADF";
exit_status_to_string(125) -> "erlang port close EINTR";
exit_status_to_string(126) -> "erlang port close EIO";
exit_status_to_string(127) -> "erlang port close unknown";
exit_status_to_string(Status) when is_integer(Status) ->
    if
        Status > 128 ->
            %% exit status values due to signals
            cloudi_os_process:signal_to_string(Status - 128);
        true ->
            erlang:integer_to_list(Status)
    end.

