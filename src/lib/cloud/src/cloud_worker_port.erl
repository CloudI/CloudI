%%% -*- Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Cloudi Worker Process Server==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2009, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2009 Michael Truog
%%% @version 0.0.4 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloud_worker_port).
-author('mjtruog [at] gmail (dot) com').

-behaviour(gen_server).

%% external interface
-export([worker_start_proxy/4,
         worker_stop_proxy/1,
         has_work_proxy/4,
         add_work_proxy/4,
         remove_work_proxy/2]).
-export([start_link/2,
         acquire_as_new_process/2,
         acquire_as_old_process/2]).

%% gen_server callbacks
-export([init/1, 
         handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-include("cloud_worker_port.hrl").
-include("cloud_logger.hrl").

% checks to make sure the port is still responding
-define(WORKER_KEEP_ALIVE_INTERVAL, 60000). % 60 seconds

-record(state,
    {
    process_name = undefined,
    master = {undefined, undefined},
    keep_alive_timer = undefined,
    port_file_name = "",
    port = undefined}).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Worker start configuration to configure the worker as an Erlang cnode.===
%% @end
%%-------------------------------------------------------------------------

-spec worker_start_proxy(Process :: {atom(), atom()},
                         CNodeName :: string(),
                         Port :: pos_integer(),
                         Instance :: non_neg_integer()) -> bool().

worker_start_proxy({Name, Node} = Process, CNodeName, Port, Instance)
    when is_list(CNodeName), is_integer(Port), is_integer(Instance) ->
    % shortname start function, doesn't include domain XXX
    HostName = string_extensions:after_character($@, atom_to_list(Node)),
    Started = try worker_start(Process,
        CNodeName, atom_to_list(erlang:get_cookie()), Port, Instance) of
        {ok, Running} ->
            Running;
        {error, {exit_status, Status}} ->
            ?LOG_ERROR("port exited with ~s when "
                       "unable to start worker ~p on ~p",
                       [exit_status_to_list(Status), Name, Node]),
            false;
        {error, Reason} ->
            ?LOG_ERROR("unable to start worker ~p on ~p: ~p",
                       [Name, Node, Reason]),
            false
    catch
        exit:{timeout, _} ->
            ?LOG_ERROR("start worker ~p on ~p timeout",
                       [Name, Node]),
            false;
        _:Reason ->
            ?LOG_ERROR("unable to start worker ~p on ~p: ~P",
                       [Name, Node, Reason, 3]),
            false
    end,
    if
        Started ->
            CNode = list_to_atom(CNodeName ++ "@" ++ HostName),
            case net_kernel:connect_node(CNode) of
                ignored ->
                    false;
                false ->
                    ?LOG_ERROR("unable to connect to started ~p", [CNode]),
                    false;
                true ->
                    true
            end;
        true ->
            false
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Stop running Erlang cnode worker.===
%% @end
%%-------------------------------------------------------------------------

-spec worker_stop_proxy(Process :: {atom(), atom()}) ->
    {'ok', bool()} |
    {'error', any()}.

worker_stop_proxy({Name, Node} = Process) ->
    try worker_stop(Process) of
        {ok, _} = Result ->
            Result;
        {error, {exit_status, Status}} = Result ->
            ?LOG_ERROR("port exited with ~s when "
                       "unable to stop worker ~p on ~p",
                       [exit_status_to_list(Status), Name, Node]),
            Result;
        {error, Reason} = Result ->
            ?LOG_ERROR("unable to stop worker ~p on ~p: ~p",
                       [Name, Node, Reason]),
            Result
    catch
        exit:{timeout, _} ->
            ?LOG_ERROR("stop worker ~p on ~p timeout",
                       [Name, Node]),
            {error, timeout};
        _:Reason ->
            ?LOG_ERROR("unable to stop worker ~p on ~p: ~P",
                       [Name, Node, Reason, 3]),
            {error, Reason}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Verify a work assignment on the running worker.===
%% @end
%%-------------------------------------------------------------------------

-spec has_work_proxy(Process :: {atom(), atom()},
                     WorkTitle :: string(),
                     TaskIdOffset :: non_neg_integer(),
                     ConcurrentTasks :: pos_integer()) ->
    {'ok', bool()} |
    {'error', any()}.

has_work_proxy({Name, Node} = Process, WorkTitle, TaskIdOffset, ConcurrentTasks)
    when is_list(WorkTitle), is_integer(TaskIdOffset),
         is_integer(ConcurrentTasks) ->
    try has_work(Process, WorkTitle, TaskIdOffset, ConcurrentTasks) of
        {ok, _} = Result ->
            Result;
        {error, {exit_status, Status}} = Result ->
            ?LOG_ERROR("port exited with ~s when "
                       "has_work on worker ~p on ~p failed",
                       [exit_status_to_list(Status), Name, Node]),
            Result;
        {error, Reason} = Result ->
            ?LOG_ERROR("has_work on worker ~p on ~p failed: ~p",
                       [Name, Node, Reason]),
            Result
    catch
        exit:{timeout, _} ->
            ?LOG_ERROR("has_work on worker ~p on ~p timeout",
                       [Name, Node]),
            {error, timeout};
        _:Reason ->
            ?LOG_ERROR("has_work on worker ~p on ~p failed: ~P",
                       [Name, Node, Reason, 3]),
            {error, Reason}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Add a work assignment to the running worker.===
%% @end
%%-------------------------------------------------------------------------

-spec add_work_proxy(Process :: {atom(), atom()},
                     WorkTitle :: string(),
                     TaskIdOffset :: non_neg_integer(),
                     ConcurrentTasks :: pos_integer()) ->
    {'ok', bool()} |
    {'error', any()}.

add_work_proxy({Name, Node} = Process, WorkTitle, TaskIdOffset, ConcurrentTasks)
    when is_list(WorkTitle), is_integer(TaskIdOffset),
         is_integer(ConcurrentTasks) ->
    try add_work(Process, WorkTitle, TaskIdOffset, ConcurrentTasks) of
        {ok, _} = Result ->
            Result;
        {error, {exit_status, Status}} = Result ->
            ?LOG_ERROR("port exited with ~s when "
                       "add_work on worker ~p on ~p failed",
                       [exit_status_to_list(Status), Name, Node]),
            Result;
        {error, Reason} = Result ->
            ?LOG_ERROR("add_work on worker ~p on ~p failed: ~p",
                       [Name, Node, Reason]),
            Result
    catch
        exit:{timeout, _} ->
            ?LOG_ERROR("add_work on worker ~p on ~p timeout",
                       [Name, Node]),
            {error, timeout};
        _:Reason ->
            ?LOG_ERROR("add_work on worker ~p on ~p failed: ~P",
                       [Name, Node, Reason, 3]),
            {error, Reason}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Remove a work assignment from the running worker.===
%% @end
%%-------------------------------------------------------------------------

-spec remove_work_proxy(Process :: {atom(), atom()},
                        WorkTitle :: string()) ->
    {'ok', bool()} |
    {'error', any()}.

remove_work_proxy({Name, Node} = Process, WorkTitle)
    when is_list(WorkTitle) ->
    try remove_work(Process, WorkTitle) of
        {ok, _} = Result ->
            Result;
        {error, {exit_status, Status}} = Result ->
            ?LOG_ERROR("port exited with ~s when "
                       "remove_work on worker ~p on ~p failed",
                       [exit_status_to_list(Status), Name, Node]),
            Result;
        {error, Reason} = Result ->
            ?LOG_ERROR("remove_work on worker ~p on ~p failed: ~p",
                       [Name, Node, Reason]),
            Result
    catch
        exit:{timeout, _} ->
            ?LOG_ERROR("remove_work on worker ~p on ~p timeout",
                       [Name, Node]),
            {error, timeout};
        _:Reason ->
            ?LOG_ERROR("remove_work on worker ~p on ~p failed: ~P",
                       [Name, Node, Reason, 3]),
            {error, Reason}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Start the worker process server.===
%% @end
%%-------------------------------------------------------------------------

-spec start_link(ProcessName :: atom(), MasterNode :: atom()) ->
    {'ok', pid()} |
    {'error', any()}.

start_link(ProcessName, MasterNode)
    when is_atom(ProcessName), is_atom(MasterNode) ->
    gen_server:start_link({local, ProcessName},
        ?MODULE, [ProcessName, MasterNode], []).

%%-------------------------------------------------------------------------
%% @doc
%% ===Acquire the worker process as if it was a new process.===
%% This will cause the worker process to be reconfigured.
%% @end
%%-------------------------------------------------------------------------

-spec acquire_as_new_process(Process :: {atom(), atom()},
                             Master :: {atom(), atom()}) ->
    'true' |
    'false'.

acquire_as_new_process({Name, Node} = Process, Master) ->
    try gen_server:call(Process, {ready, Process, Master}) of
        ok ->
            true
    catch
        exit:{timeout, _} ->
            ?LOG_ERROR("timeout of process ~p on ~p",
                       [Name, Node]),
            false;
        _:Reason ->
            ?LOG_ERROR("unable to acquire process ~p on ~p: ~P",
                       [Name, Node, Reason, 3]),
            false
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Acquire the worker process as if it was an old process.===
%% This will cause the worker process to have all of
%% its work assignments verified.
%% @end
%%-------------------------------------------------------------------------

-spec acquire_as_old_process(Process :: {atom(), atom()},
                             Master :: {atom(), atom()}) ->
    'true' |
    'false'.

acquire_as_old_process({Name, Node} = Process, Master) ->
    try gen_server:call(Process, {reset_ready, Process, Master}) of
        ok ->
            true
    catch
        exit:{timeout, _} ->
            ?LOG_ERROR("timeout of process ~p on ~p",
                       [Name, Node]),
            false;
        _:Reason ->
            ?LOG_ERROR("unable to acquire process ~p on ~p: ~P",
                       [Name, Node, Reason, 3]),
            false
    end.

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([ProcessName, MasterNode]) ->
    PortFileName = local_port_name(),
    case load_local_port(PortFileName) of
        {ok, Port} when is_port(Port) ->
            Timer = erlang:send_after(?WORKER_KEEP_ALIVE_INTERVAL,
                self(), keep_alive),
            % message to start the process of making the master process
            % configure the worker and store state after doing so
            gen_server:cast(self(), {ready, {ProcessName, node()}}),
            {ok, #state{process_name = ProcessName,
                        master = {cloud_leader, MasterNode},
                        keep_alive_timer = Timer,
                        port_file_name = PortFileName,
                        port = Port}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call({call, Msg}, _, #state{port = Port} = State)
    when is_list(Msg), is_port(Port), is_record(State, state) ->
    case call_port(Port, Msg) of
        ok ->
            receive
                {Port, {exit_status, Status}} ->
                    self() ! {Port, {exit_status, Status}},
                    {reply, {error, {exit_status, Status}}, State};
                {Port, {data, Data}} ->
                    case erlang:binary_to_term(Data) of
                        {error, Reason} ->
                            {reply, {error, Reason}, State};
                        Result ->
                            {reply, {ok, Result}, State}
                    end
            after
                4500 ->
                    {reply, {error, timeout}, State}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({call, Msg}, _, State)
    when is_list(Msg), is_record(State, state) ->
    {stop, "invalid port state", error, State};

%% reset the master process assignment for an old cloud_worker_port process
%% with an unconfigured worker
handle_call({ready, {Name, Node}, Master}, _, State)
    when is_atom(Name), is_tuple(Master), is_record(State, state) ->
    ok = gen_server:cast(self(), {ready, {Name, Node}}),
    {reply, ok, State#state{master = Master}};

%% reset the master process assignment for an old cloud_worker_port process
%% with a configured worker
handle_call({reset_ready, {Name, Node}, Master}, _, State)
    when is_atom(Name), is_tuple(Master), is_record(State, state) ->
    ok = gen_server:cast(self(), {reset_ready, {Name, Node}}),
    {reply, ok, State#state{master = Master}};

handle_call(Request, _, State) ->
    ?LOG_WARNING("Unknown call \"~p\"", [Request]),
    {stop, string_extensions:format("Unknown call \"~p\"", [Request]),
     error, State}.

%% notify the master process with a new cloud_worker_port process
%% that is ready to be initialized
handle_cast({ready, {Name, Node}} = Message,
            #state{master = Master} = State)
    when is_atom(Name), is_atom(Node), is_tuple(Master) ->
    ok = gen_server:cast(Master, Message),
    {noreply, State};

%% notify the master process with an old cloud_worker_port process
%% that is ready to be verified
handle_cast({reset_ready, {Name, Node}} = Message,
            #state{master = Master} = State)
    when is_atom(Name), is_atom(Node), is_tuple(Master) ->
    ok = gen_server:cast(Master, Message),
    {noreply, State};

handle_cast(Request, State) ->
    ?LOG_WARNING("Unknown cast \"~p\"", [Request]),
    {noreply, State}.

%% port exited with a fatal error/signal
handle_info({Port, {exit_status, Status}}, State)
    when is_port(Port), is_integer(Status), is_record(State, state) ->
    catch erlang:port_close(Port),
    Reason = "port exited with " ++ exit_status_to_list(Status),
    {stop, Reason, State#state{port = undefined}};

%% a timeout occurred and data was returned too late
handle_info({Port, {data, _}}, State)
    when is_port(Port), is_record(State, state) ->
    {noreply, State};

handle_info({stderr, Source, Error}, State) when is_list(Error) ->
    FormattedError = lists:flatmap(fun(Line) ->
        io_lib:format("    ~s~n", [Line])
    end, string:tokens(Error, "\n")),
    ?LOG_ERROR("~p stderr:~n~s", [node(Source), FormattedError]),
    {noreply, State};

handle_info(keep_alive, #state{process_name = ProcessName} = State) ->
    Parent = self(),
    spawn(fun() ->
        case catch keep_alive(Parent) of
            {ok, true} ->
                ok;
            _ ->
                Node = node(Parent),
                ?LOG_ERROR("keep_alive failed for ~p on ~p",
                    [ProcessName, Node]),
                cloud_worker_port_sup:restart_port({ProcessName, Node})
        end
    end),
    Timer = erlang:send_after(?WORKER_KEEP_ALIVE_INTERVAL, Parent, keep_alive),
    {noreply, State#state{keep_alive_timer = Timer}};

%% something unexpected happened when sending to the port
handle_info({'EXIT', Port, PosixCode}, #state{port = Port} = State) ->
    catch erlang:port_close(Port),
    Reason = case PosixCode of
        eacces ->
            "permission denied";
        eagain ->
            "resource temporarily unavailable";
        ebadf ->
            "bad file number";
        ebusy ->
            "file busy";
        edquot ->
            "disk quota exceeded";
        eexist ->
            "file already exists";
        efault ->
            "bad address in system call argument";
        efbig ->
            "file too large";
        eintr ->
            "interrupted system call";
        einval ->
            "invalid argument";
        eio ->
            "IO error";
        eisdir ->
            "illegal operation on a directory";
        eloop ->
            "too many levels of symbolic links";
        emfile ->
            "too many open files";
        emlink ->
            "too many links";
        enametoolong ->
            "file name too long";
        enfile ->
            "file table overflow";
        enodev ->
            "no such device";
        enoent ->
            "no such file or directory";
        enomem ->
            "not enough memory";
        enospc ->
            "no space left on device";
        enotblk ->
            "block device required";
        enotdir ->
            "not a directory";
        enotsup ->
            "operation not supported";
        enxio ->
            "no such device or address";
        eperm ->
            "not owner";
        epipe ->
            "broken pipe";
        erofs ->
            "read-only file system";
        espipe ->
            "invalid seek";
        esrch ->
            "no such process";
        estale ->
            "stale remote file handle";
        exdev ->
            "cross-domain link";
        Other when is_atom(Other) ->
            atom_to_list(Other)
    end,
    {stop, Reason, State#state{port = undefined}};

handle_info(Request, State) ->
    ?LOG_WARNING("Unknown info \"~p\"", [Request]),
    {noreply, State}.

terminate(_, #state{keep_alive_timer = Timer,
                    port = Port}) when is_port(Port) ->
    if
        Port /= undefined ->
            catch erlang:port_close(Port);
        true ->
            ok
    end,
    erlang:cancel_timer(Timer),
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

local_port_name() ->
    ?ERL_PORT_NAME.

load_local_port(Name) when is_list(Name) ->
    {ok, Path} = load_path(Name),
    try erlang:open_port({spawn, filename:join([Path, Name])},
                         [{packet, 2}, binary, exit_status, use_stdio]) of
        P ->
            {ok, P}
    catch
        _:Reason ->
            {error, Reason}
    end.

call_port_sync(Process, Msg) when is_list(Msg) ->
    gen_server:call(Process, {call, Msg}).

call_port(Port, Msg) when is_port(Port), is_list(Msg) ->
    try erlang:port_command(Port, Msg) of
        true -> ok
    catch
        error:badarg ->
            try erlang:iolist_size(Msg) of
                _ -> {error, einval}
            catch
                error:_ -> {error, badarg}
            end;
        error:Reason -> {error, Reason}
    end.

load_path(File) when is_list(File) ->
    Paths = lists:filter(fun(D) ->
        case file:read_file_info(filename:join([D, File])) of
            {ok, _} -> true;
            _ -> false
        end
    end, code:get_path()),
    case Paths of
        [Dir|_] ->
            {ok, Dir};
        [] ->
            {error, enoent}
    end.


%% exit status messages

%% GEPD exit status values (from ExitStatus in lib/gepd/port.cpp)
%% exit_status >= GEPD::ExitStatus::errors_min (from lib/gepd/port_main.h)
%% (GEPD::ExitStatus::errors_min == 10)
exit_status_to_list(10) -> "erlang port read EAGAIN";
exit_status_to_list(11) -> "erlang port read EBADF";
exit_status_to_list(12) -> "erlang port read EFAULT";
exit_status_to_list(13) -> "erlang port read EINTR";
exit_status_to_list(14) -> "erlang port read EINVAL";
exit_status_to_list(15) -> "erlang port read EIO";
exit_status_to_list(16) -> "erlang port read EISDIR";
exit_status_to_list(17) -> "erlang port read null";
exit_status_to_list(18) -> "erlang port read overflow";
exit_status_to_list(19) -> "erlang port read unknown";
exit_status_to_list(20) -> "erlang port write EAGAIN";
exit_status_to_list(21) -> "erlang port write EBADF";
exit_status_to_list(22) -> "erlang port write EFAULT";
exit_status_to_list(23) -> "erlang port write EFBIG";
exit_status_to_list(24) -> "erlang port write EINTR";
exit_status_to_list(25) -> "erlang port write EINVAL";
exit_status_to_list(26) -> "erlang port write EIO";
exit_status_to_list(27) -> "erlang port write ENOSPC";
exit_status_to_list(28) -> "erlang port write EPIPE";
exit_status_to_list(29) -> "erlang port write null";
exit_status_to_list(30) -> "erlang port write overflow";
exit_status_to_list(31) -> "erlang port write unknown";
exit_status_to_list(32) -> "erlang port ei_encode_error";
exit_status_to_list(33) -> "erlang port poll EBADF";
exit_status_to_list(34) -> "erlang port poll EFAULT";
exit_status_to_list(35) -> "erlang port poll EINTR";
exit_status_to_list(36) -> "erlang port poll EINVAL";
exit_status_to_list(37) -> "erlang port poll ENOMEM";
exit_status_to_list(38) -> "erlang port poll ERR";
exit_status_to_list(39) -> "erlang port poll NVAL";
exit_status_to_list(40) -> "erlang port poll unknown";
%% cloud_worker cnode server exit status values
%% (from lib/cloud_worker/src/worker_controller.hpp)
%% where exit_status >= GEPD::ExitStatus::errors_max
exit_status_to_list(64) -> "server socket ERR";
exit_status_to_list(65) -> "server socket HUP";
exit_status_to_list(66) -> "server socket NVAL";
exit_status_to_list(67) -> "server socket accept";
exit_status_to_list(68) -> "epmd socket ERR";
exit_status_to_list(69) -> "epmd socket HUP";
exit_status_to_list(70) -> "epmd socket NVAL";
exit_status_to_list(71) -> "event pipe ERR";
exit_status_to_list(72) -> "event pipe HUP";
exit_status_to_list(73) -> "event pipe NVAL";
exit_status_to_list(74) -> "stderr ERR";
exit_status_to_list(75) -> "stderr HUP";
exit_status_to_list(76) -> "stderr NVAL";
exit_status_to_list(77) -> "controller alloc";
exit_status_to_list(78) -> "node receive EIO";
exit_status_to_list(79) -> "node receive ETIMEDOUT";
exit_status_to_list(80) -> "node receive unknown";
exit_status_to_list(81) -> "node send EIO";
exit_status_to_list(82) -> "node send unknown";
exit_status_to_list(83) -> "protocol deserialize erlang failed";
exit_status_to_list(84) -> "protocol deserialize asn1 failed";
exit_status_to_list(85) -> "protocol deserialize asn1 incomplete";
exit_status_to_list(86) -> "protocol serialize erlang failed";
exit_status_to_list(87) -> "protocol serialize asn1 copy";
exit_status_to_list(88) -> "protocol serialize asn1 failed";
exit_status_to_list(89) -> "protocol unknown message";
%% exit status values due to signals
exit_status_to_list(129) -> "SIGHUP";
exit_status_to_list(130) -> "SIGINT";
exit_status_to_list(131) -> "SIGQUIT";
exit_status_to_list(132) -> "SIGILL";
exit_status_to_list(133) -> "SIGTRAP";
exit_status_to_list(134) -> "SIGABRT";
exit_status_to_list(135) -> "SIGBUS/SIGEMT";
exit_status_to_list(136) -> "SIGFPE";
exit_status_to_list(137) -> "SIGKILL";
exit_status_to_list(138) -> "SIGUSR1/SIGBUS";
exit_status_to_list(139) -> "SIGSEGV";
exit_status_to_list(140) -> "SIGUSR2/SIGSYS";
exit_status_to_list(141) -> "SIGPIPE";
exit_status_to_list(142) -> "SIGALRM";
exit_status_to_list(143) -> "SIGTERM";
exit_status_to_list(144) -> "SIGUSR1/SIGURG";
exit_status_to_list(145) -> "SIGUSR2/SIGCHLD/SIGSTOP";
exit_status_to_list(146) -> "SIGCHLD/SIGCONT/SIGTSTP";
exit_status_to_list(147) -> "SIGCONT/SIGSTOP/SIGPWR";
exit_status_to_list(148) -> "SIGCHLD/SIGTSTP/SIGWINCH";
exit_status_to_list(149) -> "SIGTTIN/SIGURG";
exit_status_to_list(150) -> "SIGTTOU/SIGIO";
exit_status_to_list(151) -> "SIGSTOP/SIGURG/SIGIO";
exit_status_to_list(152) -> "SIGTSTP/SIGXCPU";
exit_status_to_list(153) -> "SIGCONT/SIGXFSZ";
exit_status_to_list(154) -> "SIGTTIN/SIGVTALRM";
exit_status_to_list(155) -> "SIGTTOU/SIGPROF";
exit_status_to_list(156) -> "SIGVTALRM/SIGWINCH";
exit_status_to_list(157) -> "SIGPROF/SIGIO/SIGPWR";
exit_status_to_list(158) -> "SIGUSR1/SIGXCPU/SIGPWR";
exit_status_to_list(159) -> "SIGUSR2/SIGXFSZ/SIGSYS";
exit_status_to_list(0) -> "no error occurred(?), value 0";
exit_status_to_list(Status) when is_integer(Status), Status > 128 ->
    "SIG#" ++ integer_to_list(Status - 128);
exit_status_to_list(Status) when is_integer(Status) ->
    integer_to_list(Status).

