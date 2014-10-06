%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Logger==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2009-2014, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2009-2014 Michael Truog
%%% @version 1.4.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_core_i_logger).
-author('mjtruog [at] gmail (dot) com').

-behaviour(gen_server).

%% external interface
-export([start_link/1,
         current_function/0,
         file_set/1,
         level_set/1,
         syslog_set/1,
         formatters_set/2,
         redirect_set/1,
         fatal/6, error/6, warn/6, info/6, debug/6, trace/6,
         metadata_get/0, metadata_set/1,
         format/2, format/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("cloudi_core_i_configuration.hrl").
-include("cloudi_core_i_constants.hrl").
-include_lib("kernel/include/file.hrl").

%% logging macros used only within this module
-define(LOG_INFO_T0(Format, Args, State),
    log_message_internal_t0(info, ?LINE, Format, Args, State)).
-define(LOG_INFO_T1(Format, Args, State),
    log_message_internal_t1(info, ?LINE, Format, Args, State)).

-record(state,
    {
        file_path = undefined
            :: undefined | string(),
        file_level = undefined
            :: undefined | cloudi_service_api:loglevel(),
        interface_module
            :: binary(),
        fd = undefined
            :: undefined | file:fd(),
        inode = undefined
            :: undefined | non_neg_integer(),
        level
            :: cloudi_service_api:loglevel(),
        mode = async
            :: async | sync,
        destination
            :: ?MODULE | {?MODULE, node()},
        syslog = undefined
            :: undefined | port(),
        syslog_level = undefined
            :: undefined | cloudi_service_api:loglevel(),
        formatters
            :: undefined | #config_logging_formatters{},
        formatters_level
            :: undefined | cloudi_service_api:loglevel()
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Start the logging server with the supplied configuration.===
%% @end
%%-------------------------------------------------------------------------

-spec start_link(#config{}) -> {'ok', pid()} | {'error', any()}.

start_link(#config{logging = LoggingConfig}) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [LoggingConfig], []).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the current function name being executed.===
%% @end
%%-------------------------------------------------------------------------

-spec current_function() ->
    atom().

current_function() ->
    catch throw(x), [_, {_, F, _, _} | _] = erlang:get_stacktrace(),
    F.

%%-------------------------------------------------------------------------
%% @doc
%% ===Change the file output path.===
%% @end
%%-------------------------------------------------------------------------

-spec file_set(FilePath :: string() | undefined) ->
    'ok' | {'error', file:posix() | badarg | system_limit}.

file_set(undefined) ->
    gen_server:cast(?MODULE, {file_set, undefined});
file_set(FilePath)
    when is_list(FilePath) andalso is_integer(hd(FilePath)) ->
    case file:open(FilePath, [append, raw]) of
        {ok, Fd} ->
            file:close(Fd),
            gen_server:cast(?MODULE, {file_set, FilePath});
        {error, _} = Error ->
            Error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Change the file output log level.===
%% @end
%%-------------------------------------------------------------------------

-spec level_set(Level :: cloudi_service_api:loglevel() | undefined) ->
    'ok'.

level_set(undefined) ->
    gen_server:cast(?MODULE, {level_set, off});
level_set(Level)
    when Level =:= fatal; Level =:= error; Level =:= warn;
         Level =:= info; Level =:= debug; Level =:= trace;
         Level =:= off ->
    gen_server:cast(?MODULE, {level_set, Level}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Change the syslog configuration.===
%% @end
%%-------------------------------------------------------------------------

-spec syslog_set(SyslogConfig :: #config_logging_syslog{} | undefined) ->
    'ok'.
                 
syslog_set(SyslogConfig)
    when SyslogConfig =:= undefined;
         is_record(SyslogConfig, config_logging_syslog) ->
    gen_server:cast(?MODULE, {syslog_set, SyslogConfig}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Change the logging formatters.===
%% @end
%%-------------------------------------------------------------------------

-spec formatters_set(FormattersConfig :: #config_logging_formatters{} |
                                         undefined,
                     Timeout :: cloudi_service_api:
                                api_timeout_milliseconds()) ->
    'ok'.

formatters_set(FormattersConfig, Timeout)
    when FormattersConfig =:= undefined;
         is_record(FormattersConfig, config_logging_formatters) ->
    gen_server:call(?MODULE, {formatters_set, FormattersConfig}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Redirect this node's logging to a different node.===
%% @end
%%-------------------------------------------------------------------------

-spec redirect_set(Node :: atom()) ->
    'ok'.

redirect_set(Node) ->
    gen_server:cast(?MODULE, {redirect_set, Node}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Critical log message.===
%% which indicates the system has failed and can not continue.
%% Called with ?LOG_CRITICAL(Format, []).
%% @end
%%-------------------------------------------------------------------------

-spec fatal(Mode :: async | sync,
            Process :: atom() | {atom(), node()},
            Module :: atom(),
            Line :: integer(),
            Format :: string(),
            Args :: list()) ->
    'ok'.

fatal(Mode, Process, Module, Line, Format, Args) ->
    log_message_external(Mode, Process, fatal, Module, Line, Format, Args).

%%-------------------------------------------------------------------------
%% @doc
%% ===Error log message.===
%% which indicates a subsystem has failed but the failure is not fatal.
%% Called with ?LOG_ERROR(Format, []).
%% @end
%%-------------------------------------------------------------------------

-spec error(Mode :: async | sync,
            Process :: atom() | {atom(), node()},
            Module :: atom(),
            Line :: integer(),
            Format :: string(),
            Args :: list()) ->
    'ok'.

error(Mode, Process, Module, Line, Format, Args) ->
    log_message_external(Mode, Process, error, Module, Line, Format, Args).

%%-------------------------------------------------------------------------
%% @doc
%% ===Warning log message.===
%% which indicates an unexpected occurance was found in a subsystem.
%% Called with ?LOG_WARNING(Format, []).
%% @end
%%-------------------------------------------------------------------------

-spec warn(Mode :: async | sync,
           Process :: atom() | {atom(), node()},
           Module :: atom(),
           Line :: integer(),
           Format :: string(),
           Args :: list()) ->
    'ok'.

warn(Mode, Process, Module, Line, Format, Args) ->
    log_message_external(Mode, Process, warn, Module, Line, Format, Args).

%%-------------------------------------------------------------------------
%% @doc
%% ===Info log message.===
%% which indicates a subsystem has changed state.
%% Called with ?LOG_INFO(Format, []).
%% @end
%%-------------------------------------------------------------------------

-spec info(Mode :: async | sync,
           Process :: atom() | {atom(), node()},
           Module :: atom(),
           Line :: integer(),
           Format :: string(),
           Args :: list()) ->
    'ok'.

info(Mode, Process, Module, Line, Format, Args) ->
    log_message_external(Mode, Process, info, Module, Line, Format, Args).

%%-------------------------------------------------------------------------
%% @doc
%% ===Debug log message.===
%% which reports subsystem data that should be useful for debugging.
%% Called with ?LOG_DEBUG(Format, []).
%% @end
%%-------------------------------------------------------------------------

-spec debug(Mode :: async | sync,
            Process :: atom() | {atom(), node()},
            Module :: atom(),
            Line :: integer(),
            Format :: string(),
            Args :: list()) ->
    'ok'.

debug(Mode, Process, Module, Line, Format, Args) ->
    log_message_external(Mode, Process, debug, Module, Line, Format, Args).

%%-------------------------------------------------------------------------
%% @doc
%% ===Trace log message.===
%% which reports subsystem data that is only for tracing execution.
%% Called with ?LOG_TRACE(Format, []).
%% @end
%%-------------------------------------------------------------------------

-spec trace(Mode :: async | sync,
            Process :: atom() | {atom(), node()},
            Module :: atom(),
            Line :: integer(),
            Format :: string(),
            Args :: list()) ->
    'ok'.

trace(Mode, Process, Module, Line, Format, Args) ->
    log_message_external(Mode, Process, trace, Module, Line, Format, Args).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get lager-compatible metadata.===
%% @end
%%-------------------------------------------------------------------------

-spec metadata_get() ->
    list({atom(), any()}).

metadata_get() ->
    lager_metadata_get().

%%-------------------------------------------------------------------------
%% @doc
%% ===Set lager-compatible metadata.===
%% @end
%%-------------------------------------------------------------------------

-spec metadata_set(list({atom(), any()})) ->
    ok.

metadata_set(L) ->
    lager_metadata_set(L).

%%-------------------------------------------------------------------------
%% @doc
%% ===Lager formatter for legacy output.===
%% Provides legacy CloudI logger output formatting in a
%% lager formatter function.
%% @end
%%-------------------------------------------------------------------------

format(Msg, Config) ->
    format(Msg, Config, []).

%%-------------------------------------------------------------------------
%% @doc
%% ===Lager formatter for legacy output.===
%% Provides legacy CloudI logger output formatting in a
%% lager formatter function.
%% Use ``{formatters, [{any, [{formatter, cloudi_core_i_logger}, {formatter_config, [{mode, legacy}]}]}, {['STDOUT'], [{formatter, cloudi_core_i_logger}, {formatter_config, [{mode, legacy_stdout}]}]}, {['STDERR'], [{formatter, cloudi_core_i_logger}, {formatter_config, [{mode, legacy_stderr}]}]}]}'' for 
%% legacy log output with this formatter function.
%% @end
%%-------------------------------------------------------------------------

format(Msg, Config, _) ->
    Mode = case lists:keyfind(mode, 1, Config) of
        {_, M} ->
            M;
        false ->
            legacy
    end,
    {lager_msg,
     _Destinations,
     MetaData,
     Severity,
     _DateTime,
     Timestamp,
     Message} = Msg,
    Level = lager_severity_input(Severity),
    Defaults = [
        {node, undefined},
        {pid, undefined},
        {module, undefined},
        {line, undefined}],
    [Node, Pid, Module, Line |
     _] = cloudi_proplists:take_values(Defaults, MetaData),
    LogMessage = Message,
    if
        Mode =:= legacy ->
            format_line(Level, Timestamp, Node, Pid,
                        Module, Line, [],
                        indent_space_1(LogMessage));
        Mode =:= legacy_stdout ->
            format_line(Level, Timestamp, Node, Pid,
                        Module, Line, [],
                        cloudi_string:format(" stdout (pid ~w):~n", [Line]) ++
                        indent_space_2(LogMessage));
        Mode =:= legacy_stderr ->
            format_line(Level, Timestamp, Node, Pid,
                        Module, Line, [],
                        cloudi_string:format(" stderr (pid ~w):~n", [Line]) ++
                        indent_space_2(LogMessage))
    end.

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([#config_logging{file = FilePath,
                      level = FileLevel,
                      redirect = NodeLogger,
                      syslog = SyslogConfig,
                      formatters = FormattersConfig}]) ->
    FormattersLevel = case FormattersConfig of
        undefined ->
            undefined;
        #config_logging_formatters{level = FormattersLevel0} ->
            FormattersLevel0
    end,
    #state{mode = Mode} = State = #state{file_level = FileLevel,
                                         formatters = FormattersConfig,
                                         formatters_level = FormattersLevel},
    #state{syslog_level = SyslogLevel} = StateNext = case SyslogConfig of
        undefined ->
            State;
        #config_logging_syslog{identity = SyslogIdentity,
                               facility = SyslogFacility,
                               level = SyslogLevel0} ->
            ok = syslog:load(),
            State#state{syslog = syslog_open(SyslogIdentity, SyslogFacility),
                        syslog_level = SyslogLevel0}
    end,
    Level = log_level([FileLevel, SyslogLevel, FormattersLevel]),
    Destination = if
        NodeLogger == node(); NodeLogger =:= undefined ->
            ?MODULE;
        true ->
            {?MODULE, NodeLogger}
    end,
    case load_interface_module(Level, Mode, Destination) of
        {ok, Binary} when Destination == ?MODULE ->
            case log_file_open(FilePath,
                               StateNext#state{interface_module = Binary,
                                               level = Level,
                                               destination = Destination}) of
                {ok, _} = Success ->
                    Success;
                {error, Reason} ->
                    {stop, Reason}
            end;
        {ok, Binary} ->
            case ?LOG_INFO_T0("redirecting log output to ~p",
                              [NodeLogger],
                              StateNext#state{file_path = FilePath,
                                              interface_module = Binary,
                                              level = Level,
                                              destination = ?MODULE}) of
                {ok, StateNew} ->
                    {ok, StateNew#state{destination = Destination}};
                {{error, Reason}, _} ->
                    {stop, Reason}
            end;
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call({Level, Timestamp, Node, Pid,
             Module, Line, MetaData, LogMessage}, _, State) ->
    case log_message_internal(Level, Timestamp, Node, Pid,
                              Module, Line, MetaData, LogMessage, State) of
        {ok, StateNext} ->
            case log_mode_check(StateNext) of
                {ok, StateNew} ->
                    {reply, ok, StateNew};
                {error, Reason} ->
                    {stop, Reason, StateNext}
            end;
        {{error, Reason}, StateNext} ->
            {stop, Reason, StateNext}
    end;
handle_call({formatters_set, FormattersConfigNew}, _,
            #state{file_level = FileLevel,
                   level = LevelOld,
                   mode = Mode,
                   destination = Destination,
                   syslog_level = SyslogLevel,
                   formatters_level = FormattersLevelOld} = State) ->
    FormattersLevelNew = case FormattersConfigNew of
        undefined ->
            undefined;
        #config_logging_formatters{level = FormattersLevelNew0} ->
            FormattersLevelNew0
    end,
    SwitchF = fun(StateSwitch) ->
        ok = cloudi_core_i_logger_sup:reconfigure(FormattersConfigNew),
        StateSwitch#state{formatters = FormattersConfigNew,
                          formatters_level = FormattersLevelNew}
    end,
    if
        FormattersLevelNew /= FormattersLevelOld ->
            case ?LOG_INFO_T0("changing formatters loglevel from ~p to ~p",
                              [FormattersLevelOld, FormattersLevelNew],
                              State) of
                {ok, StateNext} ->
                    StateUpdated = SwitchF(StateNext),
                    case log_level([FileLevel, SyslogLevel,
                                    FormattersLevelNew]) of
                        LevelOld ->
                            {reply, ok, StateUpdated};
                        LevelNew ->
                            case load_interface_module(LevelNew, Mode,
                                                       Destination) of
                                {ok, Binary} ->
                                    StateNew = StateUpdated#state{
                                        interface_module = Binary,
                                        level = LevelNew},
                                    ?LOG_INFO_T1("changed formatters loglevel "
                                                 "from ~p to ~p",
                                                 [LevelOld, LevelNew],
                                                 StateNew),
                                    {reply, ok, StateNew};
                                {error, Reason} ->
                                    {stop, Reason, StateUpdated}
                            end
                    end;
                {{error, Reason}, StateNext} ->
                    {stop, Reason, {error, Reason}, StateNext}
            end;
        true ->
            {reply, ok, SwitchF(State)}
    end;
handle_call(Request, _, State) ->
    {stop, cloudi_string:format("Unknown call \"~p\"~n", [Request]),
     error, State}.

handle_cast({file_set, FilePath},
            #state{file_path = FilePath} = State) ->
    {noreply, State};
handle_cast({file_set, FilePathNew},
            #state{file_path = FilePathOld,
                   file_level = FileLevelOld,
                   level = LevelOld,
                   mode = Mode,
                   destination = Destination,
                   syslog_level = SyslogLevel,
                   formatters_level = FormattersLevel} = State)
    when FilePathNew =:= undefined; FilePathOld =:= undefined ->
    FileLevelNew = if
        FilePathNew =:= undefined ->
            undefined;
        FilePathOld =:= undefined ->
            (#config_logging{})#config_logging.level
    end,
    case ?LOG_INFO_T0("changing file loglevel from ~p to ~p",
                      [FileLevelOld, FileLevelNew], State) of
        {ok, #state{fd = FdOld} = StateNext} ->
            file:close(FdOld),
            case log_level([FileLevelNew, SyslogLevel, FormattersLevel]) of
                LevelOld ->
                    {noreply, StateNext#state{file_path = FilePathNew,
                                              file_level = FileLevelNew,
                                              fd = undefined,
                                              inode = undefined}};
                LevelNew ->
                    case load_interface_module(LevelNew, Mode, Destination) of
                        {ok, Binary} ->
                            StateNew = StateNext#state{
                                file_path = FilePathNew,
                                file_level = FileLevelNew,
                                interface_module = Binary,
                                fd = undefined,
                                inode = undefined,
                                level = LevelNew},
                            ?LOG_INFO_T1("changed file loglevel from ~p to ~p",
                                         [LevelOld, LevelNew], StateNew),
                            {noreply, StateNew};
                        {error, Reason} ->
                            {stop, Reason,
                             StateNext#state{fd = undefined,
                                             inode = undefined}}
                    end
            end;
        {{error, Reason}, StateNext} ->
            {stop, Reason, StateNext}
    end;
handle_cast({file_set, FilePathNew},
            #state{file_path = FilePathOld} = State) ->
    case ?LOG_INFO_T0("changing file path from ~p to ~p",
                      [FilePathOld, FilePathNew], State) of
        {ok, #state{fd = FdOld} = StateNext} ->
            file:close(FdOld),
            {noreply, StateNext#state{file_path = FilePathNew,
                                      fd = undefined,
                                      inode = undefined}};
        {{error, Reason}, StateNext} ->
            {stop, Reason, StateNext}
    end;
handle_cast({level_set, _},
            #state{file_path = undefined} = State) ->
    {noreply, State};
handle_cast({level_set, FileLevel},
            #state{file_level = FileLevel} = State) ->
    {noreply, State};
handle_cast({level_set, FileLevelNew},
            #state{file_level = FileLevelOld,
                   level = LevelOld,
                   mode = Mode,
                   destination = Destination,
                   syslog_level = SyslogLevel,
                   formatters_level = FormattersLevel} = State) ->
    case ?LOG_INFO_T0("changing file loglevel from ~p to ~p",
                      [FileLevelOld, FileLevelNew], State) of
        {ok, StateNext} ->
            case log_level([FileLevelNew, SyslogLevel, FormattersLevel]) of
                LevelOld ->
                    {noreply, StateNext#state{file_level = FileLevelNew}};
                LevelNew ->
                    case load_interface_module(LevelNew, Mode, Destination) of
                        {ok, Binary} ->
                            StateNew = StateNext#state{
                                file_level = FileLevelNew,
                                interface_module = Binary,
                                level = LevelNew},
                            ?LOG_INFO_T1("changed file loglevel from ~p to ~p",
                                         [LevelOld, LevelNew], StateNew),
                            {noreply, StateNew};
                        {error, Reason} ->
                            {stop, Reason, StateNext}
                    end
            end;
        {{error, Reason}, StateNext} ->
            {stop, Reason, StateNext}
    end;
handle_cast({syslog_set, SyslogConfig},
            #state{file_level = FileLevel,
                   level = LevelOld,
                   mode = Mode,
                   destination = Destination,
                   syslog = SyslogOld,
                   syslog_level = SyslogLevelOld,
                   formatters_level = FormattersLevel} = State) ->
    SyslogLevelNew = case SyslogConfig of
        undefined ->
            undefined;
        #config_logging_syslog{level = SyslogLevelNew0} ->
            SyslogLevelNew0
    end,
    SwitchF = fun(StateSwitch) ->
        if
            SyslogOld =:= undefined ->
                if
                    SyslogConfig =:= undefined ->
                        ok;
                    true ->
                        ok = syslog:load()
                end;
            is_port(SyslogOld) ->
                ok = syslog:close(SyslogOld),
                if
                    SyslogConfig =:= undefined ->
                        ok = syslog:unload();
                    true ->
                        ok
                end
        end,
        case SyslogConfig of
            undefined ->
                StateSwitch#state{syslog = undefined,
                                  syslog_level = undefined};
            #config_logging_syslog{identity = SyslogIdentity,
                                   facility = SyslogFacility} ->
                StateSwitch#state{syslog = syslog_open(SyslogIdentity,
                                                       SyslogFacility),
                                  syslog_level = SyslogLevelNew}
        end
    end,
    if
        SyslogLevelNew /= SyslogLevelOld ->
            case ?LOG_INFO_T0("changing syslog loglevel from ~p to ~p",
                              [SyslogLevelOld, SyslogLevelNew], State) of
                {ok, StateNext} ->
                    StateUpdated = SwitchF(StateNext),
                    case log_level([FileLevel, SyslogLevelNew,
                                    FormattersLevel]) of
                        LevelOld ->
                            {noreply, StateUpdated};
                        LevelNew ->
                            case load_interface_module(LevelNew, Mode,
                                                       Destination) of
                                {ok, Binary} ->
                                    StateNew = StateUpdated#state{
                                        interface_module = Binary,
                                        level = LevelNew},
                                    ?LOG_INFO_T1("changed syslog loglevel "
                                                 "from ~p to ~p",
                                                 [LevelOld, LevelNew],
                                                 StateNew),
                                    {noreply, StateNew};
                                {error, Reason} ->
                                    {stop, Reason, StateUpdated}
                            end
                    end;
                {{error, Reason}, StateNext} ->
                    {stop, Reason, StateNext}
            end;
        true ->
            {noreply, SwitchF(State)}
    end;
handle_cast({redirect_set, Node}, State) ->
    Destination = if
        Node == node(); Node =:= undefined ->
            ?MODULE;
        true ->
            {?MODULE, Node}
    end,
    case log_redirect(Node, Destination, State) of
        {ok, StateNew} ->
            {noreply, StateNew};
        {error, Reason, StateNew} ->
            {stop, Reason, StateNew}
    end;
handle_cast({Level, Timestamp, Node, Pid,
             Module, Line, MetaData, LogMessage}, State) ->
    case log_message_internal(Level, Timestamp, Node, Pid,
                              Module, Line, MetaData, LogMessage, State) of
        {ok, StateNext} ->
            case log_mode_check(StateNext) of
                {ok, StateNew} ->
                    {noreply, StateNew};
                {error, Reason} ->
                    {stop, Reason, StateNext}
            end;
        {{error, Reason}, StateNext} ->
            {stop, Reason, StateNext}
    end;
handle_cast(Request, State) ->
    {stop, cloudi_string:format("Unknown cast \"~p\"~n", [Request]), State}.

handle_info(Request, State) ->
    {stop, cloudi_string:format("Unknown info \"~p\"~n", [Request]), State}.

terminate(_, #state{fd = Fd,
                    syslog = Syslog}) ->
    file:close(Fd),
    if
        is_port(Syslog) ->
            (catch syslog:close(Syslog)),
            syslog:unload();
        true ->
            ok
    end,
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

format_line(Level, {_, _, MicroSeconds} = Timestamp, Node, Pid,
            Module, Line, MetaData, LogMessage) ->
    {{DateYYYY, DateMM, DateDD},
     {TimeHH, TimeMM, TimeSS}} = calendar:now_to_universal_time(Timestamp),
    MetaDataStr = if
        MetaData == [] ->
            "";
        true ->
            io_lib:format("~p", [MetaData]) ++ "\n"
    end,
    % ISO 8601 for date/time http://www.w3.org/TR/NOTE-datetime
    cloudi_string:format("~4..0w-~2..0w-~2..0wT"
                         "~2..0w:~2..0w:~2..0w.~6..0wZ ~s "
                         "(~p:~p:~p:~p)~n~s~s~n",
                         [DateYYYY, DateMM, DateDD,
                          TimeHH, TimeMM, TimeSS, MicroSeconds,
                          log_level_to_string(Level),
                          Module, Line, Pid, Node,
                          MetaDataStr, LogMessage]).

indent_space_1(Output) ->
    indent_space_1_lines([32 | Output], []).
indent_space_1_lines([], Output) ->
    lists:reverse(Output);
indent_space_1_lines([10], Output) ->
    lists:reverse([10 | Output]);
indent_space_1_lines([10 | L], Output) ->
    indent_space_1_lines(L, [32, 10 | Output]);
indent_space_1_lines([C | L], Output) ->
    indent_space_1_lines(L, [C | Output]).

indent_space_2(Output) ->
    indent_space_2_lines([32, 32 | Output], []).
indent_space_2_lines([], Output) ->
    lists:reverse(Output);
indent_space_2_lines([10], Output) ->
    lists:reverse([10 | Output]);
indent_space_2_lines([10 | L], Output) ->
    indent_space_2_lines(L, [32, 32, 10 | Output]);
indent_space_2_lines([C | L], Output) ->
    indent_space_2_lines(L, [C | Output]).

log_message_formatter_call(Level, Timestamp, Node, Pid,
                           Module, Line, MetaData, LogMessage,
                           #config_logging_formatter{
                               output = undefined,
                               formatter = Formatter,
                               formatter_config = FormatterConfig}) ->
    % A formatter module has:
    % required: format(Msg, Config)
    % optional: format(Msg, Config, Colors)
    Msg = lager_msg(Level, Timestamp, Node, Pid,
                    Module, Line, MetaData, LogMessage),
    try Formatter:format(Msg, FormatterConfig)
    catch
        ErrorType:Error ->
            ErrorMessage = cloudi_string:format("formatter(~p) ~p ~p~n~p",
                                                [Formatter, ErrorType, Error,
                                                 erlang:get_stacktrace()]),
            format_line(Level, Timestamp, Node, Pid,
                        Module, Line, MetaData, LogMessage) ++
            format_line(error, timestamp_increment(Timestamp), node(), self(),
                        ?MODULE, ?LINE, [], ErrorMessage)
    end;
log_message_formatter_call(Level, Timestamp, Node, Pid,
                           Module, Line, MetaData, LogMessage,
                           #config_logging_formatter{
                               output = Output,
                               output_name = OutputName}) ->
    Msg = lager_msg(Level, Timestamp, Node, Pid,
                    Module, Line, MetaData, LogMessage),
    try gen_event:notify(OutputName, {log, Msg}) of
        ok ->
            format_line(Level, Timestamp, Node, Pid,
                        Module, Line, MetaData, LogMessage)
    catch
        error:badarg ->
            % output module is not currently running,
            % it likely exceeded the maximum restart intensity
            % (which is logged elsewhere via sasl)
            format_line(Level, Timestamp, Node, Pid,
                        Module, Line, MetaData, LogMessage);
        ErrorType:Error ->
            ErrorMessage = cloudi_string:format("output(~p) ~p ~p~n~p",
                                                [Output, ErrorType, Error,
                                                 erlang:get_stacktrace()]),
            format_line(Level, Timestamp, Node, Pid,
                        Module, Line, MetaData, LogMessage) ++
            format_line(error, timestamp_increment(Timestamp), node(), self(),
                        ?MODULE, ?LINE, [], ErrorMessage)
    end.

log_message_formatter(Level, Timestamp, Node, Pid,
                      Module, Line, MetaData, LogMessage,
                      #config_logging_formatter{
                          level = FormatterLevel} = FormatterConfig) ->
    case log_level_allowed(FormatterLevel, Level) of
        true ->
            log_message_formatter_call(Level, Timestamp, Node, Pid,
                                       Module, Line, MetaData, LogMessage,
                                       FormatterConfig);
        false ->
            format_line(Level, Timestamp, Node, Pid,
                        Module, Line, MetaData, LogMessage)
    end.

log_message_formatters(Level, Timestamp, Node, Pid,
                       Module, Line, MetaData, LogMessage,
                       undefined) ->
    format_line(Level, Timestamp, Node, Pid,
                Module, Line, MetaData, LogMessage);
log_message_formatters(Level, Timestamp, Node, Pid,
                       Module, Line, MetaData, LogMessage,
                       #config_logging_formatters{
                           default = Default,
                           lookup = Lookup}) ->
    case cloudi_x_keys1value:find(Module, Lookup) of
        {ok, FormatterConfig} ->
            log_message_formatter(Level, Timestamp, Node, Pid,
                                  Module, Line, MetaData, LogMessage,
                                  FormatterConfig);
        error ->
            if
                Default =:= undefined ->
                    format_line(Level, Timestamp, Node, Pid,
                                Module, Line, MetaData, LogMessage);
                true ->
                    log_message_formatter(Level, Timestamp, Node, Pid,
                                          Module, Line, MetaData, LogMessage,
                                          Default)
            end
    end.

log_message_external(Mode, Process, Level, Module, Line, Format, Args)
    when is_atom(Level), is_atom(Module), is_integer(Line) ->
    Timestamp = erlang:now(),
    case flooding_logger(Timestamp) of
        true ->
            ok;
        false ->
            LogMessage = try cloudi_string:format(Format, Args)
            catch
                error:badarg ->
                    cloudi_string:format("INVALID LOG INPUT: ~p ~p",
                                         [Format, Args])
            end,
            MetaData = metadata_get(),
            if
                Mode =:= async ->
                    gen_server:cast(Process,
                                    {Level, Timestamp, node(), self(),
                                     Module, Line, MetaData, LogMessage});
                Mode =:= sync ->
                    gen_server:call(Process,
                                    {Level, Timestamp, node(), self(),
                                     Module, Line, MetaData, LogMessage},
                                    infinity)
            end
    end.

%% every 10 seconds, determine if the process has sent too many logging messages
flooding_logger(Timestamp2) ->
    case erlang:get(cloudi_logger) of
        undefined ->
            erlang:put(cloudi_logger, {Timestamp2, 1}),
            false;
        {Timestamp1, Count1} ->
            Count2 = Count1 + 1,
            MicroSecondsElapsed = timer:now_diff(Timestamp2, Timestamp1),
            if
                MicroSecondsElapsed > 10000000 ->
                    erlang:put(cloudi_logger, {Timestamp2, 1}),
                    false;
                (MicroSecondsElapsed / Count2) < ?LOGGER_FLOODING_DELTA ->
                    erlang:put(cloudi_logger, {Timestamp1, Count2}),
                    true;
                true ->
                    erlang:put(cloudi_logger, {Timestamp1, Count2}),
                    false
            end
    end.

log_message_internal_t0(LevelCheck, Line, Format, Args,
                        #state{level = Level} = State)
    when LevelCheck =:= fatal; LevelCheck =:= error; LevelCheck =:= warn;
         LevelCheck =:= info; LevelCheck =:= debug; LevelCheck =:= trace ->
    case log_level_allowed(Level, LevelCheck) of
        true ->
            LogMessage = cloudi_string:format(Format, Args),
            log_message_internal(LevelCheck, erlang:now(), node(), self(),
                                 ?MODULE, Line, [], LogMessage, State);
        false ->
            {ok, State}
    end.

log_message_internal_t1(LevelCheck, Line, Format, Args,
                        #state{level = Level,
                               destination = Destination})
    when LevelCheck =:= fatal; LevelCheck =:= error; LevelCheck =:= warn;
         LevelCheck =:= info; LevelCheck =:= debug; LevelCheck =:= trace ->
    case log_level_allowed(Level, LevelCheck) of
        true ->
            LogMessage = cloudi_string:format(Format, Args),
            gen_server:cast(Destination,
                            {LevelCheck, erlang:now(), node(), self(),
                             ?MODULE, Line, [], LogMessage});
        false ->
            ok
    end.

log_message_internal(Level, Timestamp, Node, Pid,
                     Module, Line, MetaData, LogMessage,
                     #state{file_level = FileLevel,
                            syslog_level = SyslogLevel,
                            formatters = FormattersConfig} = State0)
    when Level =:= fatal; Level =:= error; Level =:= warn;
         Level =:= info; Level =:= debug; Level =:= trace ->
    Message = log_message_formatters(Level, Timestamp, Node, Pid,
                                     Module, Line, MetaData, LogMessage,
                                     FormattersConfig),
    {FileResult, State1} = case log_level_allowed(FileLevel, Level) of
        true ->
            log_file(Message, State0);
        false ->
            {ok, State0}
    end,
    {SyslogResult, StateN} = case log_level_allowed(SyslogLevel, Level) of
        true ->
            log_syslog(Level, Message, State1);
        false ->
            {ok, State1}
    end,
    log_message_internal_result(FileResult, SyslogResult, StateN).

log_message_internal_result(ok, ok, State) ->
    {ok, State};
log_message_internal_result({error, _} = Error, _, State) ->
    {Error, State}.

log_level([_ | _] = L) ->
    cloudi_core_i_configuration:logging_level_highest([off | L]).

log_file(Message,
         #state{file_path = FilePath,
                fd = FdOld,
                inode = InodeOld} = State) ->
    case file:read_file_info(FilePath) of
        {ok, #file_info{inode = CurrentInode}} ->
            if
                CurrentInode == InodeOld ->
                    file:write(FdOld, Message),
                    file:datasync(FdOld),
                    {ok, State};
                true ->
                    file:close(FdOld),
                    case log_file_reopen(FilePath,
                                         CurrentInode, State) of
                        {ok, #state{fd = FdNew} = StateNew} ->
                            file:write(FdNew, Message),
                            file:datasync(FdNew),
                            {ok, StateNew};
                        {error, _} = Error ->
                            {Error, State#state{fd = undefined,
                                                inode = undefined}}
                    end
            end;
        {error, enoent} ->
            file:close(FdOld),
            case log_file_open(FilePath, State) of
                {ok, #state{fd = FdNew} = StateNew} ->
                    file:write(FdNew, Message),
                    file:datasync(FdNew),
                    {ok, StateNew};
                {error, _} = Error ->
                    {Error, State#state{fd = undefined,
                                        inode = undefined}}
            end;
        {error, _} = Error ->
            {Error, State}
    end.

log_file_open(undefined, State) ->
    {ok, State};
log_file_open(FilePath, State) ->
    case file:open(FilePath, [append, raw]) of
        {ok, Fd} ->
            case file:read_file_info(FilePath) of
                {ok, #file_info{inode = Inode}} ->
                    {ok, State#state{file_path = FilePath,
                                     fd = Fd,
                                     inode = Inode}};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

log_file_reopen(FilePath, Inode, State) ->
    case file:open(FilePath, [append, raw]) of
        {ok, Fd} ->
            {ok, State#state{file_path = FilePath,
                             fd = Fd,
                             inode = Inode}};
        {error, _} = Error ->
            Error
    end.

log_syslog(Level, Message,
           #state{syslog = Syslog} = State) ->
    SyslogPriority = log_level_to_syslog_priority(Level),
    syslog:log(Syslog, SyslogPriority, Message),
    {ok, State}.

log_redirect(_, Destination,
             #state{destination = Destination} = State) ->
    {ok, State};
log_redirect(Node, DestinationNew,
             #state{level = Level,
                    mode = Mode} = State) ->
    NodeLogger = if
        DestinationNew =:= ?MODULE ->
            node();
        true ->
            Node
    end,
    case ?LOG_INFO_T0("redirecting log output to ~p",
                      [NodeLogger], State) of
        {ok, StateNext} ->
            case load_interface_module(Level, Mode, DestinationNew) of
                {ok, Binary} ->
                    ?LOG_INFO_T1("redirected log output from ~p to ~p",
                                 [node(), NodeLogger], StateNext),
                    {ok, StateNext#state{interface_module = Binary,
                                         destination = DestinationNew}};
                {error, Reason} ->
                    {error, Reason, StateNext}
            end;
        {{error, _}, _} = Error ->
            Error
    end.

log_mode_check(#state{level = Level,
                      mode = ModeOld,
                      destination = Destination} = State) ->
    {message_queue_len,
     MessageQueueLength} = erlang:process_info(self(), message_queue_len),
    ModeNew = if
        ModeOld =:= async,
        MessageQueueLength >= ?LOGGER_MSG_QUEUE_SYNC ->
            sync;
        ModeOld =:= sync,
        MessageQueueLength =< ?LOGGER_MSG_QUEUE_ASYNC ->
            async;
        true ->
            ModeOld
    end,
    if
        ModeNew /= ModeOld ->
            case load_interface_module(Level, ModeNew, Destination) of
                {ok, Binary} ->
                    {ok, State#state{interface_module = Binary,
                                     mode = ModeNew}};
                {error, _} = Error ->
                    Error
            end;
        true ->
            {ok, State}
    end.

log_level_to_string(fatal) ->
    "FATAL";
log_level_to_string(error) ->
    "ERROR";
log_level_to_string(warn) ->
    "WARN ";
log_level_to_string(info) ->
    "INFO ";
log_level_to_string(debug) ->
    "DEBUG";
log_level_to_string(trace) ->
    "TRACE".

log_level_to_syslog_priority(fatal) ->
    emerg;
log_level_to_syslog_priority(error) ->
    err;
log_level_to_syslog_priority(warn) ->
    warning;
log_level_to_syslog_priority(info) ->
    info;
log_level_to_syslog_priority(debug) ->
    debug;
log_level_to_syslog_priority(trace) ->
    8.

log_level_allowed(trace, fatal) ->
    true;
log_level_allowed(trace, error) ->
    true;
log_level_allowed(trace, warn) ->
    true;
log_level_allowed(trace, info) ->
    true;
log_level_allowed(trace, debug) ->
    true;
log_level_allowed(trace, trace) ->
    true;
log_level_allowed(debug, fatal) ->
    true;
log_level_allowed(debug, error) ->
    true;
log_level_allowed(debug, warn) ->
    true;
log_level_allowed(debug, info) ->
    true;
log_level_allowed(debug, debug) ->
    true;
log_level_allowed(info, fatal) ->
    true;
log_level_allowed(info, error) ->
    true;
log_level_allowed(info, warn) ->
    true;
log_level_allowed(info, info) ->
    true;
log_level_allowed(warn, fatal) ->
    true;
log_level_allowed(warn, error) ->
    true;
log_level_allowed(warn, warn) ->
    true;
log_level_allowed(error, fatal) ->
    true;
log_level_allowed(error, error) ->
    true;
log_level_allowed(fatal, fatal) ->
    true;
log_level_allowed(_, _) ->
    false.

timestamp_increment({MegaSecs, Secs, MicroSecs}) ->
    MicroSecsNew = MicroSecs + 1,
    SecsNew = Secs + (MicroSecsNew div 1000000),
    {MegaSecs + (SecsNew div 1000000),
     SecsNew rem 1000000,
     MicroSecsNew rem 1000000}.

-define(INTERFACE_MODULE_HEADER,
    "
    -module(cloudi_core_i_logger_interface).
    -author('mjtruog [at] gmail (dot) com').
    -export([fatal/4, error/4, warn/4, info/4, debug/4, trace/4,
             fatal_apply/2, error_apply/2, warn_apply/2,
             info_apply/2, debug_apply/2, trace_apply/2,
             fatal_apply/3, error_apply/3, warn_apply/3,
             info_apply/3, debug_apply/3, trace_apply/3]).").
interface(off, _, _) ->
    ?INTERFACE_MODULE_HEADER
    "
    fatal(_, _, _, _) -> ok.
    error(_, _, _, _) -> ok.
    warn(_, _, _, _) -> ok.
    info(_, _, _, _) -> ok.
    debug(_, _, _, _) -> ok.
    trace(_, _, _, _) -> ok.
    fatal_apply(_, _) -> undefined.
    error_apply(_, _) -> undefined.
    warn_apply(_, _) -> undefined.
    info_apply(_, _) -> undefined.
    debug_apply(_, _) -> undefined.
    trace_apply(_, _) -> undefined.
    fatal_apply(_, _, _) -> undefined.
    error_apply(_, _, _) -> undefined.
    warn_apply(_, _, _) -> undefined.
    info_apply(_, _, _) -> undefined.
    debug_apply(_, _, _) -> undefined.
    trace_apply(_, _, _) -> undefined.
    ";
interface(fatal, Mode, Process) ->
    cloudi_string:format(
    ?INTERFACE_MODULE_HEADER
    "
    fatal(Module, Line, Format, Arguments) ->
        cloudi_core_i_logger:fatal(~p, ~p, Module, Line, Format, Arguments).
    error(_, _, _, _) -> ok.
    warn(_, _, _, _) -> ok.
    info(_, _, _, _) -> ok.
    debug(_, _, _, _) -> ok.
    trace(_, _, _, _) -> ok.
    fatal_apply(F, A) ->
        erlang:apply(F, A).
    error_apply(_, _) -> undefined.
    warn_apply(_, _) -> undefined.
    info_apply(_, _) -> undefined.
    debug_apply(_, _) -> undefined.
    trace_apply(_, _) -> undefined.
    fatal_apply(M, F, A) ->
        erlang:apply(M, F, A).
    error_apply(_, _, _) -> undefined.
    warn_apply(_, _, _) -> undefined.
    info_apply(_, _, _) -> undefined.
    debug_apply(_, _, _) -> undefined.
    trace_apply(_, _, _) -> undefined.
    ", [Mode, Process]);
interface(error, Mode, Process) ->
    cloudi_string:format(
    ?INTERFACE_MODULE_HEADER
    "
    fatal(Module, Line, Format, Arguments) ->
        cloudi_core_i_logger:fatal(~p, ~p, Module, Line, Format, Arguments).
    error(Module, Line, Format, Arguments) ->
        cloudi_core_i_logger:error(~p, ~p, Module, Line, Format, Arguments).
    warn(_, _, _, _) -> ok.
    info(_, _, _, _) -> ok.
    debug(_, _, _, _) -> ok.
    trace(_, _, _, _) -> ok.
    fatal_apply(F, A) ->
        erlang:apply(F, A).
    error_apply(F, A) ->
        erlang:apply(F, A).
    warn_apply(_, _) -> undefined.
    info_apply(_, _) -> undefined.
    debug_apply(_, _) -> undefined.
    trace_apply(_, _) -> undefined.
    fatal_apply(M, F, A) ->
        erlang:apply(M, F, A).
    error_apply(M, F, A) ->
        erlang:apply(M, F, A).
    warn_apply(_, _, _) -> undefined.
    info_apply(_, _, _) -> undefined.
    debug_apply(_, _, _) -> undefined.
    trace_apply(_, _, _) -> undefined.
    ", [Mode, Process, Mode, Process]);
interface(warn, Mode, Process) ->
    cloudi_string:format(
    ?INTERFACE_MODULE_HEADER
    "
    fatal(Module, Line, Format, Arguments) ->
        cloudi_core_i_logger:fatal(~p, ~p, Module, Line, Format, Arguments).
    error(Module, Line, Format, Arguments) ->
        cloudi_core_i_logger:error(~p, ~p, Module, Line, Format, Arguments).
    warn(Module, Line, Format, Arguments) ->
        cloudi_core_i_logger:warn(~p, ~p, Module, Line, Format, Arguments).
    info(_, _, _, _) -> ok.
    debug(_, _, _, _) -> ok.
    trace(_, _, _, _) -> ok.
    fatal_apply(F, A) ->
        erlang:apply(F, A).
    error_apply(F, A) ->
        erlang:apply(F, A).
    warn_apply(F, A) ->
        erlang:apply(F, A).
    info_apply(_, _) -> undefined.
    debug_apply(_, _) -> undefined.
    trace_apply(_, _) -> undefined.
    fatal_apply(M, F, A) ->
        erlang:apply(M, F, A).
    error_apply(M, F, A) ->
        erlang:apply(M, F, A).
    warn_apply(M, F, A) ->
        erlang:apply(M, F, A).
    info_apply(_, _, _) -> undefined.
    debug_apply(_, _, _) -> undefined.
    trace_apply(_, _, _) -> undefined.
    ", [Mode, Process, Mode, Process, Mode, Process]);
interface(info, Mode, Process) ->
    cloudi_string:format(
    ?INTERFACE_MODULE_HEADER
    "
    fatal(Module, Line, Format, Arguments) ->
        cloudi_core_i_logger:fatal(~p, ~p, Module, Line, Format, Arguments).
    error(Module, Line, Format, Arguments) ->
        cloudi_core_i_logger:error(~p, ~p, Module, Line, Format, Arguments).
    warn(Module, Line, Format, Arguments) ->
        cloudi_core_i_logger:warn(~p, ~p, Module, Line, Format, Arguments).
    info(Module, Line, Format, Arguments) ->
        cloudi_core_i_logger:info(~p, ~p, Module, Line, Format, Arguments).
    debug(_, _, _, _) -> ok.
    trace(_, _, _, _) -> ok.
    fatal_apply(F, A) ->
        erlang:apply(F, A).
    error_apply(F, A) ->
        erlang:apply(F, A).
    warn_apply(F, A) ->
        erlang:apply(F, A).
    info_apply(F, A) ->
        erlang:apply(F, A).
    debug_apply(_, _) -> undefined.
    trace_apply(_, _) -> undefined.
    fatal_apply(M, F, A) ->
        erlang:apply(M, F, A).
    error_apply(M, F, A) ->
        erlang:apply(M, F, A).
    warn_apply(M, F, A) ->
        erlang:apply(M, F, A).
    info_apply(M, F, A) ->
        erlang:apply(M, F, A).
    debug_apply(_, _, _) -> undefined.
    trace_apply(_, _, _) -> undefined.
    ", [Mode, Process, Mode, Process, Mode, Process,
        Mode, Process]);
interface(debug, Mode, Process) ->
    cloudi_string:format(
    ?INTERFACE_MODULE_HEADER
    "
    fatal(Module, Line, Format, Arguments) ->
        cloudi_core_i_logger:fatal(~p, ~p, Module, Line, Format, Arguments).
    error(Module, Line, Format, Arguments) ->
        cloudi_core_i_logger:error(~p, ~p, Module, Line, Format, Arguments).
    warn(Module, Line, Format, Arguments) ->
        cloudi_core_i_logger:warn(~p, ~p, Module, Line, Format, Arguments).
    info(Module, Line, Format, Arguments) ->
        cloudi_core_i_logger:info(~p, ~p, Module, Line, Format, Arguments).
    debug(Module, Line, Format, Arguments) ->
        cloudi_core_i_logger:debug(~p, ~p, Module, Line, Format, Arguments).
    trace(_, _, _, _) -> ok.
    fatal_apply(F, A) ->
        erlang:apply(F, A).
    error_apply(F, A) ->
        erlang:apply(F, A).
    warn_apply(F, A) ->
        erlang:apply(F, A).
    info_apply(F, A) ->
        erlang:apply(F, A).
    debug_apply(F, A) ->
        erlang:apply(F, A).
    trace_apply(_, _) -> undefined.
    fatal_apply(M, F, A) ->
        erlang:apply(M, F, A).
    error_apply(M, F, A) ->
        erlang:apply(M, F, A).
    warn_apply(M, F, A) ->
        erlang:apply(M, F, A).
    info_apply(M, F, A) ->
        erlang:apply(M, F, A).
    debug_apply(M, F, A) ->
        erlang:apply(M, F, A).
    trace_apply(_, _, _) -> undefined.
    ", [Mode, Process, Mode, Process, Mode, Process,
        Mode, Process, Mode, Process]);
interface(trace, Mode, Process) ->
    cloudi_string:format(
    ?INTERFACE_MODULE_HEADER
    "
    fatal(Module, Line, Format, Arguments) ->
        cloudi_core_i_logger:fatal(~p, ~p, Module, Line, Format, Arguments).
    error(Module, Line, Format, Arguments) ->
        cloudi_core_i_logger:error(~p, ~p, Module, Line, Format, Arguments).
    warn(Module, Line, Format, Arguments) ->
        cloudi_core_i_logger:warn(~p, ~p, Module, Line, Format, Arguments).
    info(Module, Line, Format, Arguments) ->
        cloudi_core_i_logger:info(~p, ~p, Module, Line, Format, Arguments).
    debug(Module, Line, Format, Arguments) ->
        cloudi_core_i_logger:debug(~p, ~p, Module, Line, Format, Arguments).
    trace(Module, Line, Format, Arguments) ->
        cloudi_core_i_logger:trace(~p, ~p, Module, Line, Format, Arguments).
    fatal_apply(F, A) ->
        erlang:apply(F, A).
    error_apply(F, A) ->
        erlang:apply(F, A).
    warn_apply(F, A) ->
        erlang:apply(F, A).
    info_apply(F, A) ->
        erlang:apply(F, A).
    debug_apply(F, A) ->
        erlang:apply(F, A).
    trace_apply(F, A) ->
        erlang:apply(F, A).
    fatal_apply(M, F, A) ->
        erlang:apply(M, F, A).
    error_apply(M, F, A) ->
        erlang:apply(M, F, A).
    warn_apply(M, F, A) ->
        erlang:apply(M, F, A).
    info_apply(M, F, A) ->
        erlang:apply(M, F, A).
    debug_apply(M, F, A) ->
        erlang:apply(M, F, A).
    trace_apply(M, F, A) ->
        erlang:apply(M, F, A).
    ", [Mode, Process, Mode, Process, Mode, Process,
        Mode, Process, Mode, Process, Mode, Process]).

load_interface_module(undefined, _, _) ->
    {error, logging_level_undefined};
load_interface_module(Level, Mode, Process) when is_atom(Level) ->
    {Module, Binary} = cloudi_x_dynamic_compile:from_string(interface(Level,
                                                                      Mode,
                                                                      Process)),
    % make sure no old code exists
    code:purge(cloudi_core_i_logger_interface),
    % load the new current code
    case code:load_binary(Module,
                          "cloudi_core_i_logger_interface.erl",
                          Binary) of
        {module, Module} ->
            % remove the old code
            code:soft_purge(cloudi_core_i_logger_interface),
            {ok, Binary};
        {error, _} = Error ->
            Error
    end.

syslog_open(SyslogIdentity, SyslogFacility) ->
    {ok, Syslog} = syslog:open(SyslogIdentity, [ndelay, pid], SyslogFacility),
    Syslog.

%%%------------------------------------------------------------------------
%%% lager integration based on lager source code
%%% (lager in under the Apache version 2.0 license and
%%%  was developed by Basho Technologies)
%%%------------------------------------------------------------------------

% from lager module
-define(LAGER_MD_KEY, '__lager_metadata').

% from lager:md/0
-spec lager_metadata_get() -> list({atom(), any()}).
lager_metadata_get() ->
    case erlang:get(?LAGER_MD_KEY) of
        undefined -> [];
        L -> L
    end.

% from lager:md/1
-spec lager_metadata_set(list({atom(), any()})) -> ok.
lager_metadata_set(L) when is_list(L) ->
    case lists:all(fun({Key, _Value}) when is_atom(Key) -> true;
                      (_) -> false
                   end, L) of
        true ->
            erlang:put(?LAGER_MD_KEY, L),
            ok;
        false ->
            erlang:error(badarg)
    end;
lager_metadata_set(_) ->
    erlang:error(badarg).

% based on lager_util:maybe_utc/1
lager_datetime({_, _, MicroSeconds} = Timestamp) ->
    UTC = case application:get_env(sasl, utc_log) of
        {ok, Val} ->
            Val;
        undefined ->
            case application:get_env(stdlib, utc_log) of
                {ok, Val} ->
                    Val;
                undefined ->
                    false
            end
    end,
    TimeMS = MicroSeconds div 1000 rem 1000,
    if
        UTC =:= true ->
            {Date,
             {TimeHH,
              TimeMM,
              TimeSS}} = calendar:now_to_universal_time(Timestamp),
            {utc, {Date, {TimeHH, TimeMM, TimeSS, TimeMS}}};
        true ->
            {Date,
             {TimeHH,
              TimeMM,
              TimeSS}} = calendar:now_to_local_time(Timestamp),
            {Date, {TimeHH, TimeMM, TimeSS, TimeMS}}
    end.

% from lager_util:i2l/1
lager_i2l(I) when I < 10 -> [$0, $0 + I];
lager_i2l(I) -> erlang:integer_to_list(I).
% from lager_util:i3l/1
lager_i3l(I) when I < 100 -> [$0 | lager_i2l(I)];
lager_i3l(I) -> erlang:integer_to_list(I).

% based on lager_util:format_time/1
lager_datetime_format({utc, {{Y, M, D}, {H, Mi, S, Ms}}}) ->
    {[erlang:integer_to_list(Y), $-, lager_i2l(M), $-, lager_i2l(D)],
     [lager_i2l(H), $:, lager_i2l(Mi), $:,
      lager_i2l(S), $., lager_i3l(Ms), $ , $U, $T, $C]};
lager_datetime_format({{Y, M, D}, {H, Mi, S, Ms}}) ->
    {[erlang:integer_to_list(Y), $-, lager_i2l(M), $-, lager_i2l(D)],
     [lager_i2l(H), $:, lager_i2l(Mi), $:, lager_i2l(S), $., lager_i3l(Ms)]}.

% CloudI levels mapped to lager severity for formatters
lager_severity_output(fatal) -> emergency;
lager_severity_output(error) -> error;
lager_severity_output(warn) -> warning;
lager_severity_output(info) -> info;
lager_severity_output(debug) -> debug;
lager_severity_output(trace) -> debug.
lager_severity_input(emergency) -> fatal;
lager_severity_input(error) -> error;
lager_severity_input(warning) -> warn;
lager_severity_input(info) -> info;
lager_severity_input(debug) -> debug.

-record(lager_msg,
    {
        destinations :: list(),
        metadata :: list({atom(), any()}),
        severity :: debug | emergency | error | info | warning,
        datetime :: {string(), string()},
        timestamp :: erlang:timestamp(),
        message :: list()
    }).

-spec lager_msg(Level :: debug | error | fatal | info | trace | warn,
                Timestamp :: erlang:timestamp(),
                Node :: node(),
                Pid :: atom(),
                Module :: module(),
                Line :: pos_integer(),
                MetaData :: list({atom(), any()}),
                LogMessage :: string()) ->
    #lager_msg{}.

% based on lager_msg:new/5
lager_msg(Level, Timestamp, Node, Pid,
          Module, Line, MetaData, LogMessage) ->
    Destinations = [], % not using TraceFilters
    NewMetaData = [{node, Node},
                   {pid, Pid},
                   {module, Module},
                   % providing the function requires a preprocessor macro
                   % reasons:
                   % 1. parse transforms are not ok
                   % 2. the stack trace is slow
                   %    http://erlang.org/pipermail/erlang-questions
                   %          /2013-November/075928.html
                   % 3. avoids process_info overhead
                   %    (similar to stack trace overhead)
                   {line, Line} | MetaData],
    Severity = lager_severity_output(Level),
    DateTime = lager_datetime_format(lager_datetime(Timestamp)),
    Message = LogMessage,
    % create lager_msg record manually
    {lager_msg,
     Destinations,
     NewMetaData,
     Severity,
     DateTime,
     Timestamp,
     Message}.

