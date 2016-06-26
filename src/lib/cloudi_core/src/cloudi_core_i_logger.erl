%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Logger==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2009-2016, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2009-2016 Michael Truog
%%% @version 1.5.2 {@date} {@time}
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
         fatal/8, error/8, warn/8, info/8, debug/8, trace/8,
         metadata_get/0, metadata_set/1,
         format/2, format/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("cloudi_core_i_configuration.hrl").
-include("cloudi_core_i_constants.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("syntax_tools/include/merl.hrl").

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
        interface_module = undefined
            :: undefined | binary(),
        fd = undefined
            :: undefined | file:fd(),
        inode = undefined
            :: undefined | non_neg_integer(),
        level = undefined
            :: undefined | cloudi_service_api:loglevel(),
        mode = async
            :: async | sync,
        destination = undefined
            :: undefined | ?MODULE | {?MODULE, node()},
        syslog = undefined
            :: undefined | port(),
        syslog_level = undefined
            :: undefined | cloudi_service_api:loglevel(),
        formatters
            :: undefined | #config_logging_formatters{},
        formatters_level
            :: undefined | cloudi_service_api:loglevel(),
        aspects_log_before
            :: list(cloudi_service_api:aspect_log_before()),
        aspects_log_after
            :: list(cloudi_service_api:aspect_log_after())
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
%% Called with ?LOG_FATAL(Format, []).
%% @end
%%-------------------------------------------------------------------------

-spec fatal(Mode :: async | sync,
            Process :: atom() | {atom(), node()},
            Module :: atom(),
            Line :: integer(),
            Function :: atom(),
            Arity :: non_neg_integer() | undefined,
            Format :: string(),
            Args :: list() | undefined) ->
    'ok'.

fatal(Mode, Process, Module, Line, Function, Arity, Format, Args) ->
    log_message_external(Mode, Process, fatal, Module, Line,
                         Function, Arity, Format, Args).

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
            Function :: atom(),
            Arity :: non_neg_integer() | undefined,
            Format :: string(),
            Args :: list() | undefined) ->
    'ok'.

error(Mode, Process, Module, Line, Function, Arity, Format, Args) ->
    log_message_external(Mode, Process, error, Module, Line,
                         Function, Arity, Format, Args).

%%-------------------------------------------------------------------------
%% @doc
%% ===Warning log message.===
%% which indicates an unexpected occurance was found in a subsystem.
%% Called with ?LOG_WARN(Format, []).
%% @end
%%-------------------------------------------------------------------------

-spec warn(Mode :: async | sync,
           Process :: atom() | {atom(), node()},
           Module :: atom(),
           Line :: integer(),
           Function :: atom(),
           Arity :: non_neg_integer() | undefined,
           Format :: string(),
           Args :: list() | undefined) ->
    'ok'.

warn(Mode, Process, Module, Line, Function, Arity, Format, Args) ->
    log_message_external(Mode, Process, warn, Module, Line,
                         Function, Arity, Format, Args).

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
           Function :: atom(),
           Arity :: non_neg_integer() | undefined,
           Format :: string(),
           Args :: list() | undefined) ->
    'ok'.

info(Mode, Process, Module, Line, Function, Arity, Format, Args) ->
    log_message_external(Mode, Process, info, Module, Line,
                         Function, Arity, Format, Args).

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
            Function :: atom(),
            Arity :: non_neg_integer() | undefined,
            Format :: string(),
            Args :: list() | undefined) ->
    'ok'.

debug(Mode, Process, Module, Line, Function, Arity, Format, Args) ->
    log_message_external(Mode, Process, debug, Module, Line,
                         Function, Arity, Format, Args).

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
            Function :: atom(),
            Arity :: non_neg_integer() | undefined,
            Format :: string(),
            Args :: list() | undefined) ->
    'ok'.

trace(Mode, Process, Module, Line, Function, Arity, Format, Args) ->
    log_message_external(Mode, Process, trace, Module, Line,
                         Function, Arity, Format, Args).

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
    Defaults = [{function, undefined},
                {module, undefined},
                {line, undefined},
                {node, undefined},
                {pid, undefined}],
    [Function, Module, Line, Node, PidStr |
     ExtraMetaData] = cloudi_proplists:take_values(Defaults, MetaData),
    Pid = if
        PidStr =:= undefined ->
            undefined;
        is_list(PidStr) ->
            erlang:list_to_pid(PidStr)
    end,
    LogMessage = Message,
    if
        Mode =:= legacy ->
            format_line(Level, Timestamp, Node, Pid,
                        Module, Line, Function, undefined,
                        ExtraMetaData,
                        indent_space_1(LogMessage));
        Mode =:= legacy_stdout ->
            format_line(Level, Timestamp, Node, Pid,
                        Module, Line, Function, undefined,
                        ExtraMetaData,
                        cloudi_string:format(" stdout (pid ~w):~n", [Line]) ++
                        indent_space_2(LogMessage));
        Mode =:= legacy_stderr ->
            format_line(Level, Timestamp, Node, Pid,
                        Module, Line, Function, undefined,
                        ExtraMetaData,
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
                      formatters = FormattersConfig,
                      aspects_log_before = AspectsLogBefore,
                      aspects_log_after = AspectsLogAfter}]) ->
    FormattersLevel = case FormattersConfig of
        undefined ->
            undefined;
        #config_logging_formatters{level = FormattersLevel0} ->
            FormattersLevel0
    end,
    #state{mode = Mode} = State = #state{file_level = FileLevel,
                                         formatters = FormattersConfig,
                                         formatters_level = FormattersLevel,
                                         aspects_log_before = AspectsLogBefore,
                                         aspects_log_after = AspectsLogAfter},
    #state{syslog_level = SyslogLevel} = StateNext = case SyslogConfig of
        undefined ->
            State;
        #config_logging_syslog{identity = SyslogIdentity,
                               facility = SyslogFacility,
                               level = SyslogLevel0} ->
            ok = cloudi_x_syslog:load(),
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
             Module, Line, Function, Arity,
             MetaData, LogMessage}, _, State) ->
    case log_message_internal(Level, Timestamp, Node, Pid,
                              Module, Line, Function, Arity,
                              MetaData, LogMessage, State) of
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
                        ok = cloudi_x_syslog:load()
                end;
            is_port(SyslogOld) ->
                ok = cloudi_x_syslog:close(SyslogOld),
                if
                    SyslogConfig =:= undefined ->
                        ok = cloudi_x_syslog:unload();
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
             Module, Line, Function, Arity,
             MetaData, LogMessage}, State) ->
    case log_message_internal(Level, Timestamp, Node, Pid,
                              Module, Line, Function, Arity,
                              MetaData, LogMessage, State) of
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
            (catch cloudi_x_syslog:close(Syslog)),
            cloudi_x_syslog:unload();
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
            Module, Line, Function, Arity, MetaData, LogMessage) ->
    {{DateYYYY, DateMM, DateDD},
     {TimeHH, TimeMM, TimeSS}} = calendar:now_to_universal_time(Timestamp),
    MetaDataStr = if
        MetaData == [] ->
            "";
        true ->
            io_lib:format("~p~n", [MetaData])
    end,
    FunctionArity = if
        Function =:= undefined ->
            "";
        Arity =:= undefined ->
            erlang:atom_to_list(Function);
        true ->
            erlang:atom_to_list(Function) ++
            [$/ | erlang:integer_to_list(Arity)]
    end,
    % ISO 8601 for date/time http://www.w3.org/TR/NOTE-datetime
    cloudi_string:format("~4..0w-~2..0w-~2..0wT"
                         "~2..0w:~2..0w:~2..0w.~6..0wZ ~s "
                         "(~w:~w:~s:~w:~w)~n~s~s~n",
                         [DateYYYY, DateMM, DateDD,
                          TimeHH, TimeMM, TimeSS, MicroSeconds,
                          log_level_to_string(Level),
                          Module, Line, FunctionArity, Pid, Node,
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
                           Module, Line, Function, Arity,
                           MetaData, LogMessage,
                           #config_logging_formatter{
                               output = undefined,
                               formatter = Formatter,
                               formatter_config = FormatterConfig}) ->
    % A formatter module has:
    % required: format(Msg, Config)
    % optional: format(Msg, Config, Colors)
    Msg = lager_msg(Level, Timestamp, Node, Pid,
                    Module, Line, Function, Arity,
                    MetaData, LogMessage),
    try Formatter:format(Msg, FormatterConfig)
    catch
        ErrorType:Error ->
            ErrorMessage = cloudi_string:format("formatter(~p) ~p ~p~n~p",
                                                [Formatter, ErrorType, Error,
                                                 erlang:get_stacktrace()]),
            format_line(Level, Timestamp, Node, Pid,
                        Module, Line, Function, Arity,
                        MetaData, LogMessage) ++
            format_line(error, timestamp_increment(Timestamp), node(), self(),
                        ?MODULE, ?LINE, undefined, undefined,
                        [], ErrorMessage)
    end;
log_message_formatter_call(Level, Timestamp, Node, Pid,
                           Module, Line, Function, Arity,
                           MetaData, LogMessage,
                           #config_logging_formatter{
                               output = Output,
                               output_name = OutputName}) ->
    Msg = lager_msg(Level, Timestamp, Node, Pid,
                    Module, Line, Function, Arity,
                    MetaData, LogMessage),
    try gen_event:notify(OutputName, {log, Msg}) of
        ok ->
            format_line(Level, Timestamp, Node, Pid,
                        Module, Line, Function, Arity,
                        MetaData, LogMessage)
    catch
        error:badarg ->
            % output module is not currently running,
            % it likely exceeded the maximum restart intensity
            % (which is logged elsewhere via sasl)
            format_line(Level, Timestamp, Node, Pid,
                        Module, Line, Function, Arity,
                        MetaData, LogMessage);
        ErrorType:Error ->
            ErrorMessage = cloudi_string:format("output(~p) ~p ~p~n~p",
                                                [Output, ErrorType, Error,
                                                 erlang:get_stacktrace()]),
            format_line(Level, Timestamp, Node, Pid,
                        Module, Line, Function, Arity,
                        MetaData, LogMessage) ++
            format_line(error, timestamp_increment(Timestamp), node(), self(),
                        ?MODULE, ?LINE, undefined, undefined,
                        [], ErrorMessage)
    end.

log_message_formatter(Level, Timestamp, Node, Pid,
                      Module, Line, Function, Arity,
                      MetaData, LogMessage,
                      #config_logging_formatter{
                          level = FormatterLevel} = FormatterConfig) ->
    case log_level_allowed(FormatterLevel, Level) of
        true ->
            log_message_formatter_call(Level, Timestamp, Node, Pid,
                                       Module, Line, Function, Arity,
                                       MetaData, LogMessage,
                                       FormatterConfig);
        false ->
            format_line(Level, Timestamp, Node, Pid,
                        Module, Line, Function, Arity,
                        MetaData, LogMessage)
    end.

log_message_formatters(Level, Timestamp, Node, Pid,
                       Module, Line, Function, Arity,
                       MetaData, LogMessage,
                       undefined) ->
    format_line(Level, Timestamp, Node, Pid,
                Module, Line, Function, Arity,
                MetaData, LogMessage);
log_message_formatters(Level, Timestamp, Node, Pid,
                       Module, Line, Function, Arity,
                       MetaData, LogMessage,
                       #config_logging_formatters{
                           default = Default,
                           lookup = Lookup}) ->
    case cloudi_x_keys1value:find(Module, Lookup) of
        {ok, FormatterConfig} ->
            log_message_formatter(Level, Timestamp, Node, Pid,
                                  Module, Line, Function, Arity,
                                  MetaData, LogMessage,
                                  FormatterConfig);
        error ->
            if
                Default =:= undefined ->
                    format_line(Level, Timestamp, Node, Pid,
                                Module, Line, Function, Arity,
                                MetaData, LogMessage);
                true ->
                    log_message_formatter(Level, Timestamp, Node, Pid,
                                          Module, Line, Function, Arity,
                                          MetaData, LogMessage,
                                          Default)
            end
    end.

log_message_external(Mode, Process, Level, Module, Line, Function, Arity,
                     Format, Args)
    when is_atom(Level), is_atom(Module), is_integer(Line),
         is_atom(Function),
         (Arity =:= undefined) orelse
         (is_integer(Arity) andalso (Arity >= 0)) ->
    Timestamp = cloudi_timestamp:timestamp(),
    case flooding_logger(Timestamp) of
        {true, _} ->
            ok;
        {false, FloodingWarning} ->
            LogMessage0 = if
                is_list(Format), Args =:= undefined ->
                    Format;
                true ->
                    try cloudi_string:format(Format, Args)
                    catch
                        error:badarg ->
                            cloudi_string:format("INVALID LOG INPUT: ~p ~p",
                                                 [Format, Args])
                    end
            end,
            LogMessageN = if
                FloodingWarning =:= undefined ->
                    LogMessage0;
                is_list(FloodingWarning) ->
                    LogMessage0 ++ FloodingWarning
            end,
            MetaData = metadata_get(),
            if
                Mode =:= async ->
                    gen_server:cast(Process,
                                    {Level, Timestamp, node(), self(),
                                     Module, Line, Function, Arity,
                                     MetaData, LogMessageN});
                Mode =:= sync ->
                    gen_server:call(Process,
                                    {Level, Timestamp, node(), self(),
                                     Module, Line, Function, Arity,
                                     MetaData, LogMessageN},
                                    infinity)
            end
    end.

flooding_logger_warning(SecondsRemaining) ->
    cloudi_string:format("~n"
        "... (~w logged/second async stopped process logging for ~.2f seconds)",
        [1000000 div ?LOGGER_FLOODING_DELTA, SecondsRemaining]).

%% determine if a single process has sent too many logging messages
flooding_logger(Timestamp1) ->
    case erlang:get(?LOGGER_FLOODING_PDICT_KEY) of
        undefined ->
            erlang:put(?LOGGER_FLOODING_PDICT_KEY,
                       {Timestamp1, 1, false}),
            {false, undefined};
        {Timestamp0, Count0, Flooding} ->
            Count1 = Count0 + 1,
            MicroSecondsElapsed = timer:now_diff(Timestamp1, Timestamp0),
            if
                (MicroSecondsElapsed >
                 ?LOGGER_FLOODING_INTERVAL_MAX * 1000) orelse
                (MicroSecondsElapsed < 0) ->
                    erlang:put(?LOGGER_FLOODING_PDICT_KEY,
                               {Timestamp1, 1, false}),
                    {false, undefined};
                (MicroSecondsElapsed >
                 ?LOGGER_FLOODING_INTERVAL_MIN * 1000) andalso
                (MicroSecondsElapsed div Count1 < ?LOGGER_FLOODING_DELTA) ->
                    erlang:put(?LOGGER_FLOODING_PDICT_KEY,
                               {Timestamp0, Count1, true}),
                    SecondsRemaining = ?LOGGER_FLOODING_INTERVAL_MAX * 1000.0 -
                                       MicroSecondsElapsed / 1000000,
                    {false, flooding_logger_warning(SecondsRemaining)};
                true ->
                    erlang:put(?LOGGER_FLOODING_PDICT_KEY,
                               {Timestamp0, Count1, Flooding}),
                    {Flooding, undefined}
            end
    end.

log_message_internal_t0(LevelCheck, Line, Format, Args,
                        #state{level = Level} = State)
    when LevelCheck =:= fatal; LevelCheck =:= error; LevelCheck =:= warn;
         LevelCheck =:= info; LevelCheck =:= debug; LevelCheck =:= trace ->
    case log_level_allowed(Level, LevelCheck) of
        true ->
            LogMessage = cloudi_string:format(Format, Args),
            log_message_internal(LevelCheck,
                                 cloudi_timestamp:timestamp(), node(), self(),
                                 ?MODULE, Line, undefined, undefined,
                                 [], LogMessage, State);
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
                            {LevelCheck,
                             cloudi_timestamp:timestamp(), node(), self(),
                             ?MODULE, Line, undefined, undefined,
                             [], LogMessage});
        false ->
            ok
    end.

log_message_internal(Level, Timestamp, Node, Pid,
                     Module, Line, Function, Arity,
                     MetaData, LogMessage,
                     #state{file_level = FileLevel,
                            syslog_level = SyslogLevel,
                            formatters = FormattersConfig,
                            aspects_log_before = AspectsLogBefore,
                            aspects_log_after = AspectsLogAfter} = State0)
    when Level =:= fatal; Level =:= error; Level =:= warn;
         Level =:= info; Level =:= debug; Level =:= trace ->
    Message = log_message_formatters(Level, Timestamp, Node, Pid,
                                     Module, Line, Function, Arity,
                                     MetaData, LogMessage,
                                     FormattersConfig),
    ok = aspects_log(AspectsLogBefore,
                     Level, Timestamp, Node, Pid,
                     Module, Line, Function, Arity,
                     MetaData, LogMessage),
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
    ok = aspects_log(AspectsLogAfter,
                     Level, Timestamp, Node, Pid,
                     Module, Line, Function, Arity,
                     MetaData, LogMessage),
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
    cloudi_x_syslog:log(Syslog, SyslogPriority, Message),
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
    -export([fatal/6, error/6, warn/6, info/6, debug/6, trace/6,
             fatal_sync/6, error_sync/6, warn_sync/6,
             info_sync/6, debug_sync/6, trace_sync/6,
             fatal_apply/2, error_apply/2, warn_apply/2,
             info_apply/2, debug_apply/2, trace_apply/2,
             fatal_apply/3, error_apply/3, warn_apply/3,
             info_apply/3, debug_apply/3, trace_apply/3]).").
interface(off, _, _) ->
    ?INTERFACE_MODULE_HEADER
    "
    fatal(_, _, _, _, _, _) -> ok.
    error(_, _, _, _, _, _) -> ok.
    warn(_, _, _, _, _, _) -> ok.
    info(_, _, _, _, _, _) -> ok.
    debug(_, _, _, _, _, _) -> ok.
    trace(_, _, _, _, _, _) -> ok.
    fatal_sync(_, _, _, _, _, _) -> ok.
    error_sync(_, _, _, _, _, _) -> ok.
    warn_sync(_, _, _, _, _, _) -> ok.
    info_sync(_, _, _, _, _, _) -> ok.
    debug_sync(_, _, _, _, _, _) -> ok.
    trace_sync(_, _, _, _, _, _) -> ok.
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
    fatal(Module, Line, Function, Arity, Format, Arguments) ->
        cloudi_core_i_logger:fatal(~w, ~w, Module, Line, Function, Arity,
                                   Format, Arguments).
    error(_, _, _, _, _, _) -> ok.
    warn(_, _, _, _, _, _) -> ok.
    info(_, _, _, _, _, _) -> ok.
    debug(_, _, _, _, _, _) -> ok.
    trace(_, _, _, _, _, _) -> ok.
    fatal_sync(Module, Line, Function, Arity, Format, Arguments) ->
        cloudi_core_i_logger:fatal(sync, ~w, Module, Line, Function, Arity,
                                   Format, Arguments).
    error_sync(_, _, _, _, _, _) -> ok.
    warn_sync(_, _, _, _, _, _) -> ok.
    info_sync(_, _, _, _, _, _) -> ok.
    debug_sync(_, _, _, _, _, _) -> ok.
    trace_sync(_, _, _, _, _, _) -> ok.
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
    ", [Mode, Process,
        Process]);
interface(error, Mode, Process) ->
    cloudi_string:format(
    ?INTERFACE_MODULE_HEADER
    "
    fatal(Module, Line, Function, Arity, Format, Arguments) ->
        cloudi_core_i_logger:fatal(~w, ~w, Module, Line, Function, Arity,
                                   Format, Arguments).
    error(Module, Line, Function, Arity, Format, Arguments) ->
        cloudi_core_i_logger:error(~w, ~w, Module, Line, Function, Arity,
                                   Format, Arguments).
    warn(_, _, _, _, _, _) -> ok.
    info(_, _, _, _, _, _) -> ok.
    debug(_, _, _, _, _, _) -> ok.
    trace(_, _, _, _, _, _) -> ok.
    fatal_sync(Module, Line, Function, Arity, Format, Arguments) ->
        cloudi_core_i_logger:fatal(sync, ~w, Module, Line, Function, Arity,
                                   Format, Arguments).
    error_sync(Module, Line, Function, Arity, Format, Arguments) ->
        cloudi_core_i_logger:error(sync, ~w, Module, Line, Function, Arity,
                                   Format, Arguments).
    warn_sync(_, _, _, _, _, _) -> ok.
    info_sync(_, _, _, _, _, _) -> ok.
    debug_sync(_, _, _, _, _, _) -> ok.
    trace_sync(_, _, _, _, _, _) -> ok.
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
    ", [Mode, Process, Mode, Process,
        Process, Process]);
interface(warn, Mode, Process) ->
    cloudi_string:format(
    ?INTERFACE_MODULE_HEADER
    "
    fatal(Module, Line, Function, Arity, Format, Arguments) ->
        cloudi_core_i_logger:fatal(~w, ~w, Module, Line, Function, Arity,
                                   Format, Arguments).
    error(Module, Line, Function, Arity, Format, Arguments) ->
        cloudi_core_i_logger:error(~w, ~w, Module, Line, Function, Arity,
                                   Format, Arguments).
    warn(Module, Line, Function, Arity, Format, Arguments) ->
        cloudi_core_i_logger:warn(~w, ~w, Module, Line, Function, Arity,
                                  Format, Arguments).
    info(_, _, _, _, _, _) -> ok.
    debug(_, _, _, _, _, _) -> ok.
    trace(_, _, _, _, _, _) -> ok.
    fatal_sync(Module, Line, Function, Arity, Format, Arguments) ->
        cloudi_core_i_logger:fatal(sync, ~w, Module, Line, Function, Arity,
                                   Format, Arguments).
    error_sync(Module, Line, Function, Arity, Format, Arguments) ->
        cloudi_core_i_logger:error(sync, ~w, Module, Line, Function, Arity,
                                   Format, Arguments).
    warn_sync(Module, Line, Function, Arity, Format, Arguments) ->
        cloudi_core_i_logger:warn(sync, ~w, Module, Line, Function, Arity,
                                  Format, Arguments).
    info_sync(_, _, _, _, _, _) -> ok.
    debug_sync(_, _, _, _, _, _) -> ok.
    trace_sync(_, _, _, _, _, _) -> ok.
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
    ", [Mode, Process, Mode, Process, Mode, Process,
        Process, Process, Process]);
interface(info, Mode, Process) ->
    cloudi_string:format(
    ?INTERFACE_MODULE_HEADER
    "
    fatal(Module, Line, Function, Arity, Format, Arguments) ->
        cloudi_core_i_logger:fatal(~w, ~w, Module, Line, Function, Arity,
                                   Format, Arguments).
    error(Module, Line, Function, Arity, Format, Arguments) ->
        cloudi_core_i_logger:error(~w, ~w, Module, Line, Function, Arity,
                                   Format, Arguments).
    warn(Module, Line, Function, Arity, Format, Arguments) ->
        cloudi_core_i_logger:warn(~w, ~w, Module, Line, Function, Arity,
                                  Format, Arguments).
    info(Module, Line, Function, Arity, Format, Arguments) ->
        cloudi_core_i_logger:info(~w, ~w, Module, Line, Function, Arity,
                                  Format, Arguments).
    debug(_, _, _, _, _, _) -> ok.
    trace(_, _, _, _, _, _) -> ok.
    fatal_sync(Module, Line, Function, Arity, Format, Arguments) ->
        cloudi_core_i_logger:fatal(sync, ~w, Module, Line, Function, Arity,
                                   Format, Arguments).
    error_sync(Module, Line, Function, Arity, Format, Arguments) ->
        cloudi_core_i_logger:error(sync, ~w, Module, Line, Function, Arity,
                                   Format, Arguments).
    warn_sync(Module, Line, Function, Arity, Format, Arguments) ->
        cloudi_core_i_logger:warn(sync, ~w, Module, Line, Function, Arity,
                                  Format, Arguments).
    info_sync(Module, Line, Function, Arity, Format, Arguments) ->
        cloudi_core_i_logger:info(sync, ~w, Module, Line, Function, Arity,
                                  Format, Arguments).
    debug_sync(_, _, _, _, _, _) -> ok.
    trace_sync(_, _, _, _, _, _) -> ok.
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
        Mode, Process,
        Process, Process, Process, Process]);
interface(debug, Mode, Process) ->
    cloudi_string:format(
    ?INTERFACE_MODULE_HEADER
    "
    fatal(Module, Line, Function, Arity, Format, Arguments) ->
        cloudi_core_i_logger:fatal(~w, ~w, Module, Line, Function, Arity,
                                   Format, Arguments).
    error(Module, Line, Function, Arity, Format, Arguments) ->
        cloudi_core_i_logger:error(~w, ~w, Module, Line, Function, Arity,
                                   Format, Arguments).
    warn(Module, Line, Function, Arity, Format, Arguments) ->
        cloudi_core_i_logger:warn(~w, ~w, Module, Line, Function, Arity,
                                  Format, Arguments).
    info(Module, Line, Function, Arity, Format, Arguments) ->
        cloudi_core_i_logger:info(~w, ~w, Module, Line, Function, Arity,
                                  Format, Arguments).
    debug(Module, Line, Function, Arity, Format, Arguments) ->
        cloudi_core_i_logger:debug(~w, ~w, Module, Line, Function, Arity,
                                   Format, Arguments).
    trace(_, _, _, _, _, _) -> ok.
    fatal_sync(Module, Line, Function, Arity, Format, Arguments) ->
        cloudi_core_i_logger:fatal(sync, ~w, Module, Line, Function, Arity,
                                   Format, Arguments).
    error_sync(Module, Line, Function, Arity, Format, Arguments) ->
        cloudi_core_i_logger:error(sync, ~w, Module, Line, Function, Arity,
                                   Format, Arguments).
    warn_sync(Module, Line, Function, Arity, Format, Arguments) ->
        cloudi_core_i_logger:warn(sync, ~w, Module, Line, Function, Arity,
                                  Format, Arguments).
    info_sync(Module, Line, Function, Arity, Format, Arguments) ->
        cloudi_core_i_logger:info(sync, ~w, Module, Line, Function, Arity,
                                  Format, Arguments).
    debug_sync(Module, Line, Function, Arity, Format, Arguments) ->
        cloudi_core_i_logger:debug(sync, ~w, Module, Line, Function, Arity,
                                   Format, Arguments).
    trace_sync(_, _, _, _, _, _) -> ok.
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
        Mode, Process, Mode, Process,
        Process, Process, Process, Process, Process]);
interface(trace, Mode, Process) ->
    cloudi_string:format(
    ?INTERFACE_MODULE_HEADER
    "
    fatal(Module, Line, Function, Arity, Format, Arguments) ->
        cloudi_core_i_logger:fatal(~w, ~w, Module, Line, Function, Arity,
                                   Format, Arguments).
    error(Module, Line, Function, Arity, Format, Arguments) ->
        cloudi_core_i_logger:error(~w, ~w, Module, Line, Function, Arity,
                                   Format, Arguments).
    warn(Module, Line, Function, Arity, Format, Arguments) ->
        cloudi_core_i_logger:warn(~w, ~w, Module, Line, Function, Arity,
                                  Format, Arguments).
    info(Module, Line, Function, Arity, Format, Arguments) ->
        cloudi_core_i_logger:info(~w, ~w, Module, Line, Function, Arity,
                                  Format, Arguments).
    debug(Module, Line, Function, Arity, Format, Arguments) ->
        cloudi_core_i_logger:debug(~w, ~w, Module, Line, Function, Arity,
                                   Format, Arguments).
    trace(Module, Line, Function, Arity, Format, Arguments) ->
        cloudi_core_i_logger:trace(~w, ~w, Module, Line, Function, Arity,
                                   Format, Arguments).
    fatal_sync(Module, Line, Function, Arity, Format, Arguments) ->
        cloudi_core_i_logger:fatal(sync, ~w, Module, Line, Function, Arity,
                                   Format, Arguments).
    error_sync(Module, Line, Function, Arity, Format, Arguments) ->
        cloudi_core_i_logger:error(sync, ~w, Module, Line, Function, Arity,
                                   Format, Arguments).
    warn_sync(Module, Line, Function, Arity, Format, Arguments) ->
        cloudi_core_i_logger:warn(sync, ~w, Module, Line, Function, Arity,
                                  Format, Arguments).
    info_sync(Module, Line, Function, Arity, Format, Arguments) ->
        cloudi_core_i_logger:info(sync, ~w, Module, Line, Function, Arity,
                                  Format, Arguments).
    debug_sync(Module, Line, Function, Arity, Format, Arguments) ->
        cloudi_core_i_logger:debug(sync, ~w, Module, Line, Function, Arity,
                                   Format, Arguments).
    trace_sync(Module, Line, Function, Arity, Format, Arguments) ->
        cloudi_core_i_logger:trace(sync, ~w, Module, Line, Function, Arity,
                                   Format, Arguments).
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
        Mode, Process, Mode, Process, Mode, Process,
        Process, Process, Process, Process, Process, Process]).

load_interface_module(undefined, _, _) ->
    {error, logging_level_undefined};
load_interface_module(Level, Mode, Process) when is_atom(Level) ->
    {ok, Module,
     Binary} = merl:compile(?Q(interface(Level, Mode, Process))),
    cloudi_core_i_logger_interface = Module,
    % make sure no old code exists
    code:purge(Module),
    % load the new current code
    case code:load_binary(Module,
                          erlang:atom_to_list(Module) ++ ".erl",
                          Binary) of
        {module, Module} ->
            % remove the old code
            code:soft_purge(Module),
            {ok, Binary};
        {error, _} = Error ->
            Error
    end.

syslog_open(SyslogIdentity, SyslogFacility) ->
    {ok, Syslog} = cloudi_x_syslog:open(SyslogIdentity,
                                        [ndelay, pid], SyslogFacility),
    Syslog.

aspects_log([], _, _, _, _, _, _, _, _, _, _) ->
    ok;
aspects_log([{M, F} | L], Level, Timestamp, Node, Pid,
            Module, Line, Function, Arity, MetaData, LogMessage) ->
    try M:F(Level, Timestamp, Node, Pid,
            Module, Line, Function, Arity, MetaData, LogMessage) of
        _ ->
            aspects_log(L, Level, Timestamp, Node, Pid,
                        Module, Line, Function, Arity, MetaData, LogMessage)
    catch
        _:_ ->
            aspects_log(L, Level, Timestamp, Node, Pid,
                        Module, Line, Function, Arity, MetaData, LogMessage)
    end;
aspects_log([F | L], Level, Timestamp, Node, Pid,
            Module, Line, Function, Arity, MetaData, LogMessage) ->
    try F(Level, Timestamp, Node, Pid,
          Module, Line, Function, Arity, MetaData, LogMessage) of
        _ ->
            aspects_log(L, Level, Timestamp, Node, Pid,
                        Module, Line, Function, Arity, MetaData, LogMessage)
    catch
        _:_ ->
            aspects_log(L, Level, Timestamp, Node, Pid,
                        Module, Line, Function, Arity, MetaData, LogMessage)
    end.

%%%------------------------------------------------------------------------
%%% lager integration based on lager source code
%%% (lager is under the Apache version 2.0 license and
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
                Pid :: pid(),
                Module :: module(),
                Line :: pos_integer(),
                Function :: atom(),
                Arity :: non_neg_integer() | undefined,
                MetaData :: list({atom(), any()}),
                LogMessage :: string()) ->
    #lager_msg{}.

% based on lager_msg:new/5
lager_msg(Level, Timestamp, Node, Pid,
          Module, Line, Function, _Arity,
          MetaData0, LogMessage) ->
    Destinations = [], % not using TraceFilters
    MetaData1 = if
        Function =:= undefined ->
            MetaData0;
        true ->
            [{function, Function} | MetaData0]
    end,
    MetaDataN = [{module, Module},
                 {line, Line},
                 {node, Node},
                 {pid, erlang:pid_to_list(Pid)} | MetaData1],
    Severity = lager_severity_output(Level),
    DateTime = lager_datetime_format(lager_datetime(Timestamp)),
    Message = LogMessage,
    % create lager_msg record manually
    {lager_msg,
     Destinations,
     MetaDataN,
     Severity,
     DateTime,
     Timestamp,
     Message}.

