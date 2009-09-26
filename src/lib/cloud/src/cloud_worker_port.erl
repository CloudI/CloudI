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
%%% @version 0.0.2 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloud_worker_port).
-author('mjtruog [at] gmail (dot) com').

-behaviour(gen_server).

%% external interface
-export([start_link/3]).
-export([worker_start_proxy/4]).

%% gen_server callbacks
-export([init/1, 
         handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-include("cloud_worker_port.hrl").
-include("cloud_logger.hrl").

-record(state,
    {
    process_name = undefined,
    cnode_name = "",
    host_name = "",
    master = {undefined, undefined},
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

worker_start_proxy(Process, CNodeName, Port, Instance)
    when is_tuple(Process), is_list(CNodeName), is_integer(Port),
         is_integer(Instance) ->
    HostName = case Process of
        %P when is_pid(P) ->
        %    string_extensions:after_character($@, atom_to_list(node(P)));
        {_, Node} ->
            string_extensions:after_character($@, atom_to_list(Node))
    end,
    % shortname start function, doesn't include domain XXX
    Started = case worker_start(Process,
        CNodeName, atom_to_list(erlang:get_cookie()), Port, Instance) of
        {ok, Running} ->
            Running;
        {error, Reason} ->
            ?LOG_ERROR("unable to start worker ~s@~s: ~p", [
                       CNodeName, HostName, Reason]),
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
%% ===Start the worker process server.===
%% @end
%%-------------------------------------------------------------------------

-spec start_link(ProcessName :: atom(),
                 CNodeName :: string(),
                 MasterNode :: atom()) ->
    {'ok', pid()} |
    {'error', any()}.

start_link(ProcessName, CNodeName, MasterNode)
    when is_atom(ProcessName), is_list(CNodeName), is_atom(MasterNode) ->
    gen_server:start_link({local, ProcessName},
        ?MODULE, [ProcessName, CNodeName, MasterNode], []).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([ProcessName, CNodeName, MasterNode]) ->
    PortFileName = local_port_name(),
    case load_local_port(PortFileName) of
        {ok, Port} when is_port(Port) ->
            % message to start the process of making the master process
            % configure the worker and store state after doing so
            gen_server:cast(self(), {ready, {ProcessName, node()}}),
            {ok, HostName} = inet:gethostname(),
            {ok, #state{process_name = ProcessName,
                        cnode_name = CNodeName,
                        host_name = HostName,
                        master = {cloud_leader, MasterNode},
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
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({call, Msg}, _, State)
    when is_list(Msg), is_record(State, state) ->
    {stop, "invalid port state", error, State};

handle_call({reset_ready, {Name, Node}, Master}, _, State)
    when is_atom(Name), is_tuple(Master), is_record(State, state) ->
    ok = gen_server:cast(self(), {reset_ready, {Name, Node}}),
    {reply, ok, State#state{master = Master}};

handle_call(Request, _, State) ->
    ?LOG_WARNING("Unknown call \"~p\"", [Request]),
    {stop, string_extensions:format("Unknown call \"~p\"", [Request]),
     error, State}.

%% notify the master process that cloud_worker_port is ready to be started
handle_cast({ready, {Name, Node}} = Message,
            #state{master = Master} = State)
    when is_atom(Name), is_atom(Node), is_tuple(Master) ->
    ok = gen_server:cast(Master, Message),
    {noreply, State};

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

handle_info({stderr, Source, Error}, #state{cnode_name = CNodeName,
                                            host_name = HostName} = State)
    when is_list(Error), is_list(CNodeName) ->
    FormattedError = lists:flatmap(fun(Line) ->
        io_lib:format("    ~s~n", [Line])
    end, string:tokens(Error, "\n")),
    ?LOG_ERROR("~p stderr:~n~s", [node(Source), FormattedError]),
    {noreply, State};

handle_info(Request, State) ->
    ?LOG_WARNING("Unknown info \"~p\"", [Request]),
    {noreply, State}.

terminate(_, #state{port = Port}) when is_port(Port) ->
    catch erlang:port_close(Port),
    ok;

terminate(_, _) ->
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

