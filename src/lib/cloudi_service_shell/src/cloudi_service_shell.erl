%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Shell Service==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2019-2022 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2019-2022 Michael Truog
%%% @version 2.0.5 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_shell).
-author('mjtruog at protonmail dot com').

-behaviour(cloudi_service).

%% external interface
-export([exec/3,
         exec/4,
         validate_response/2]).

%% cloudi_service callbacks
-export([cloudi_service_init/4,
         cloudi_service_handle_request/11,
         cloudi_service_terminate/3]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").

-define(DEFAULT_FILE_PATH,                    "/bin/sh").
-define(DEFAULT_DIRECTORY,                          "/").
-define(DEFAULT_ENV,                                 []).
-define(DEFAULT_USER,                         undefined).
-define(DEFAULT_SU_PATH,                      "/bin/su").
-define(DEFAULT_LOGIN,                             true).
-define(DEFAULT_TIMEOUT_KILLS_PROCESS,            false).
-define(DEFAULT_TIMEOUT_KILLS_PROCESS_SIGNAL,   sigterm).
-define(DEFAULT_DEBUG,                             true).
-define(DEFAULT_DEBUG_LEVEL,                      trace).

-record(state,
    {
        file_path :: nonempty_string(),
        directory :: nonempty_string(),
        env :: list({nonempty_string(), string()}),
        env_port :: boolean(),
        user :: nonempty_string() | undefined,
        su :: nonempty_string(),
        login :: boolean(),
        kill_signal :: pos_integer() | undefined,
        debug_level :: off | trace | debug | info | warn | error | fatal
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

-type agent() :: cloudi:agent().
-type service_name() :: cloudi:service_name().
-type timeout_period() :: cloudi:timeout_period().
-type module_response(Result) ::
    {{ok, Result}, AgentNew :: agent()} |
    {{error, cloudi:error_reason()}, AgentNew :: agent()}.

-spec exec(Agent :: agent(),
           Prefix :: service_name(),
           Command :: nonempty_string() | binary()) ->
    module_response(binary()).

exec(Agent, Prefix, Command) ->
    cloudi:send_sync(Agent, Prefix, Command).

-spec exec(Agent :: agent(),
           Prefix :: service_name(),
           Command :: nonempty_string() | binary(),
           Timeout :: timeout_period()) ->
    module_response(binary()).

exec(Agent, Prefix, Command, Timeout) ->
    cloudi:send_sync(Agent, Prefix, Command, Timeout).

-spec validate_response(cloudi_service:response_info(),
                        Response :: cloudi_service:response()) ->
    boolean().

validate_response(_, Response) ->
    erlang:binary_to_integer(Response) == 0.

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, _Prefix, _Timeout, Dispatcher) ->
    Defaults = [
        {file_path,                     ?DEFAULT_FILE_PATH},
        {directory,                     ?DEFAULT_DIRECTORY},
        {env,                           ?DEFAULT_ENV},
        {user,                          ?DEFAULT_USER},
        {su_path,                       ?DEFAULT_SU_PATH},
        {login,                         ?DEFAULT_LOGIN},
        {timeout_kills_process,         ?DEFAULT_TIMEOUT_KILLS_PROCESS},
        {timeout_kills_process_signal,  ?DEFAULT_TIMEOUT_KILLS_PROCESS_SIGNAL},
        {debug,                         ?DEFAULT_DEBUG},
        {debug_level,                   ?DEFAULT_DEBUG_LEVEL}],
    [FilePath, Directory, Env, User, SUPath, Login,
     TimeoutKill, TimeoutKillSignal,
     Debug, DebugLevel] = cloudi_proplists:take_values(Defaults, Args),
    [_ | _] = FilePath,
    true = filelib:is_regular(FilePath),
    [_ | _] = Directory,
    true = filelib:is_dir(Directory),
    EnvExpanded = env_expand(Env, cloudi_environment:lookup()),
    case User of
        undefined ->
            ok;
        [_ | _] ->
            true = filelib:is_regular(SUPath)
    end,
    EnvPort = if
        Login =:= true, is_list(User) ->
            false;
        is_boolean(Login) ->
            true
    end,
    EnvExpanded = env_expand(Env, cloudi_environment:lookup()),
    EnvShell = if
        EnvPort =:= true ->
            env_reset(EnvExpanded);
        EnvPort =:= false ->
            EnvExpanded
    end,
    KillSignal = if
        TimeoutKill =:= true ->
            cloudi_os_process:signal_to_integer(TimeoutKillSignal);
        TimeoutKill =:= false ->
            undefined
    end,
    true = is_boolean(Debug),
    true = ((DebugLevel =:= trace) orelse
            (DebugLevel =:= debug) orelse
            (DebugLevel =:= info) orelse
            (DebugLevel =:= warn) orelse
            (DebugLevel =:= error) orelse
            (DebugLevel =:= fatal)),
    DebugLogLevel = if
        Debug =:= false ->
            off;
        Debug =:= true ->
            DebugLevel
    end,
    cloudi_service:subscribe(Dispatcher, ""),
    {ok, #state{file_path = FilePath,
                directory = Directory,
                env = EnvShell,
                env_port = EnvPort,
                user = User,
                su = SUPath,
                login = Login,
                kill_signal = KillSignal,
                debug_level = DebugLogLevel}}.

cloudi_service_handle_request(_RequestType, _Name, _Pattern,
                              RequestInfo, Request,
                              Timeout, _Priority, _TransId, _Pid,
                              #state{debug_level = DebugLogLevel} = State,
                              _Dispatcher) ->
    {Status, Output} = request(Request, Timeout, State),
    log_output(Status, Output, RequestInfo, Request, DebugLogLevel),
    {reply, erlang:integer_to_binary(Status), State}.

cloudi_service_terminate(_Reason, _Timeout, _State) ->
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

env_expand([] = L, _) ->
    L;
env_expand([{[_ | _] = Key, Value} | L], Lookup)
    when is_list(Value) ->
    [_ | _] = KeyExpanded = cloudi_environment:transform(Key, Lookup),
    [{KeyExpanded, cloudi_environment:transform(Value, Lookup)} |
     env_expand(L, Lookup)].

env_reset(L) ->
    % remove environment variables set by CloudI execution
    [{"BINDIR", false},
     {"EMU", false},
     {"ERL_AFLAGS", false},
     {"ERL_COMPILER_OPTIONS", false},
     {"ERL_CRASH_DUMP", false},
     {"ERL_CRASH_DUMP_SECONDS", false},
     {"ERL_EPMD_ADDRESS", false},
     {"ERL_EPMD_PORT", false},
     {"ERL_FLAGS", false},
     {"ERL_LIBS", false},
     {"ERL_ZFLAGS", false},
     {"ESCRIPT_NAME", false},
     {"HEART_BEAT_TIMEOUT", false},
     {"HEART_COMMAND", false},
     {"HEART_KILL_SIGNAL", false},
     {"HEART_NO_KILL", false},
     {"PROGNAME", false},
     {"ROOTDIR", false},
     {"RUN_ERL_DISABLE_FLOWCNTRL", false},
     {"RUN_ERL_LOG_ACTIVITY_MINUTES", false},
     {"RUN_ERL_LOG_ALIVE_FORMAT", false},
     {"RUN_ERL_LOG_ALIVE_IN_UTC", false},
     {"RUN_ERL_LOG_ALIVE_MINUTES", false},
     {"RUN_ERL_LOG_GENERATIONS", false},
     {"RUN_ERL_LOG_MAXSIZE", false},
     {"TMPDIR", false} | L].

env_set([], Directory, ShellInput) ->
    [["cd ", Directory, $\n] | ShellInput];
env_set([{[_ | _] = Key, Value} | L], Directory, ShellInput) ->
    [[Key, $=, Value, "; export ", Key, $\n] |
     env_set(L, Directory, ShellInput)].

request(Exec, Timeout,
        #state{file_path = FilePath,
               directory = Directory,
               env = Env,
               env_port = EnvPort,
               user = User,
               su = SUPath,
               login = Login,
               kill_signal = KillSignal}) ->
    ShellInput0 = [unicode:characters_to_binary(Exec, utf8), "\n"
                   "exit $?\n"],
    PortOptions0 = [stream, binary, stderr_to_stdout, exit_status],
    {ShellInputN, PortOptionsN} = if
        EnvPort =:= true ->
            {ShellInput0,
             [{env, Env}, {cd, Directory} | PortOptions0]};
        EnvPort =:= false ->
            {env_set(Env, Directory, ShellInput0),
             PortOptions0}
    end,
    {ShellExecutable, ShellArgs} = if
        User =:= undefined ->
            {FilePath,
             if
                 Login =:= true ->
                     ["-"];
                 Login =:= false ->
                     []
             end};
        is_list(User) ->
            {SUPath,
             if
                 Login =:= true ->
                     ["-s", FilePath, "-", User];
                 Login =:= false ->
                     ["-s", FilePath, User]
             end}
    end,
    Shell = erlang:open_port({spawn_executable, ShellExecutable},
                             [{args, ShellArgs} | PortOptionsN]),
    KillTimer = kill_timer_start(KillSignal, Shell, Timeout),
    true = erlang:port_command(Shell, ShellInputN),
    ShellOutput = request_output(Shell, []),
    ok = kill_timer_stop(KillTimer, Shell),
    ShellOutput.

request_output(Shell, Output) ->
    receive
        {Shell, {data, Data}} ->
            request_output(Shell, [Data | Output]);
        {Shell, {kill, KillSignal}} ->
            ok = kill_shell(KillSignal, Shell),
            request_output(Shell, Output);
        {Shell, {exit_status, Status}} ->
            {Status, lists:reverse(Output)}
    end.

kill_shell(KillSignal, Shell) ->
    case erlang:port_info(Shell, os_pid) of
        undefined ->
            ok;
        {os_pid, OSPid} ->
            _ = cloudi_os_process:kill_group(KillSignal, OSPid),
            ok
    end.

kill_timer_start(undefined, _, _) ->
    undefined;
kill_timer_start(KillSignal, Shell, Timeout)
    when is_integer(KillSignal) ->
    erlang:send_after(Timeout, self(), {Shell, {kill, KillSignal}}).

kill_timer_stop(undefined, _) ->
    ok;
kill_timer_stop(KillTimer, Shell) ->
    case erlang:cancel_timer(KillTimer) of
        false ->
            receive
                {Shell, {kill, _}} ->
                    ok
            after
                0 ->
                    ok
            end;
        _ ->
            ok
    end.

log_output(Status, Output, RequestInfo, Request, DebugLogLevel) ->
    Level = if
        Status == 0 ->
            DebugLogLevel;
        true ->
            error
    end,
    StatusStr = if
        Status > 128 ->
            cloudi_os_process:signal_to_string(Status - 128);
        true ->
            erlang:integer_to_list(Status)
    end,
    Info = log_output_info(RequestInfo),
    if
        Output == [] ->
            ?LOG(Level, "~ts~ts = ~s",
                 [Info, Request, StatusStr]);
        true ->
            ?LOG(Level, "~ts~ts = ~s (stdout/stderr below)~n~ts",
                 [Info, Request, StatusStr, erlang:iolist_to_binary(Output)])
    end.

log_output_info(RequestInfo)
    when is_binary(RequestInfo), RequestInfo /= <<>> ->
    InfoTextPairs = cloudi_request_info:key_value_parse(RequestInfo, list),
    erlang:iolist_to_binary(log_output_info_format(InfoTextPairs));
log_output_info(_) ->
    <<"">>.

log_output_info_format([]) ->
    [];
log_output_info_format([{Key, Value} | InfoTextPairs]) ->
    ["# ", Key, ": ", Value, $\n | log_output_info_format(InfoTextPairs)].

