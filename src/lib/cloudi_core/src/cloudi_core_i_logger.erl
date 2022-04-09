%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Logger==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2009-2022 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2009-2022 Michael Truog
%%% @version 2.0.5 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_core_i_logger).
-author('mjtruog at protonmail dot com').

-behaviour(gen_server).

%% external interface
-export([start_link/1,
         set/1,
         file_set/1,
         stdout_set/1,
         level_set/1,
         syslog_set/1,
         formatters_set/1,
         redirect_set/1,
         redirect_update/1,
         fatal/8, error/8, warn/8, info/8, debug/8, trace/8,
         status/1,
         status_reset/1,
         metadata_get/0, metadata_set/1,
         format/2, format/3,
         microseconds_to_string/1,
         milliseconds_to_string/1,
         seconds_to_string/1,
         datetime_to_string/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("cloudi_core_i_configuration.hrl").
-include("cloudi_core_i_constants.hrl").
-include("cloudi_core_i_logger.hrl").
-include_lib("kernel/include/file.hrl").

%% logging macros used only within this module
-define(LOG_AT_T0(Level, T, Format, Args, State),
    log_message_internal_t0(Level, T, ?LINE, ?FUNCTION_NAME, ?FUNCTION_ARITY,
                            Format, Args, State)).
-define(LOG_T0(Level, Format, Args, State),
    log_message_internal_t0(Level, ?LINE, ?FUNCTION_NAME, ?FUNCTION_ARITY,
                            Format, Args, State)).
-define(LOG_T1(Level, Format, Args, State),
    log_message_internal_t1(Level, ?LINE, ?FUNCTION_NAME, ?FUNCTION_ARITY,
                            Format, Args, State)).
-define(LOG_T0_ERROR(Format, Args, State),
    ?LOG_T0(error, Format, Args, State)).
-define(LOG_T0_INFO(Format, Args, State),
    ?LOG_T0(info, Format, Args, State)).
-define(LOG_T1_INFO(Format, Args, State),
    ?LOG_T1(info, Format, Args, State)).

-define(TERMINATE_DELAY, 1000). % milliseconds

-type mode_process() :: async | sync | overload.
-type mode_interface() :: async | sync.

-record(state,
    {
        interface_module = undefined
            :: undefined | binary(),
        file_path = undefined
            :: undefined | string(),
        file_fd = undefined
            :: undefined | file:fd(),
        file_inode = undefined
            :: undefined | non_neg_integer(),
        file_sync
            :: cloudi_service_api:logging_file_sync_value_milliseconds(),
        file_sync_timer = undefined
            :: undefined | reference(),
        stdout = undefined
            :: undefined | port(),
        main_level = undefined % both file and stdout level
            :: undefined | cloudi_service_api:loglevel(),
        level = undefined
            :: undefined | cloudi_service_api:loglevel(),
        queue_pending = 0
            :: non_neg_integer(),
        queue_mode_async
            :: pos_integer(),
        queue_mode_sync
            :: pos_integer(),
        queue_mode_overload
            :: pos_integer(),
        mode = async
            :: mode_process(),
        destination = undefined
            :: undefined | ?MODULE | {?MODULE, node()},
        syslog = undefined
            :: undefined | pid(),
        syslog_level = undefined
            :: undefined | cloudi_service_api:loglevel(),
        formatters
            :: undefined | #config_logging_formatters{},
        formatters_level
            :: undefined | cloudi_service_api:loglevel(),
        log_time_offset
            :: cloudi_service_api:loglevel(),
        log_time_offset_nanoseconds
            :: integer(),
        log_time_offset_monitor
            :: reference(),
        aspects_log_before
            :: list(cloudi_service_api:aspect_log_before()),
        aspects_log_after
            :: list(cloudi_service_api:aspect_log_after()),
        logger_node
            :: node(),
        logger_self
            :: pid(),
        mode_sync_start = undefined
            :: undefined | cloudi_timestamp:native_monotonic(),
        mode_sync_start_event = undefined
            :: undefined | cloudi_timestamp:iso8601(),
        mode_sync_end = undefined
            :: undefined | cloudi_timestamp:native_monotonic(),
        mode_sync_end_event = undefined
            :: undefined | cloudi_timestamp:iso8601(),
        mode_sync_total = undefined
            :: undefined | cloudi_service_api:nanoseconds_string(),
        mode_overload_start = undefined
            :: undefined | cloudi_timestamp:native_monotonic(),
        mode_overload_start_event = undefined
            :: undefined | cloudi_timestamp:iso8601(),
        mode_overload_end = undefined
            :: undefined | cloudi_timestamp:native_monotonic(),
        mode_overload_end_event = undefined
            :: undefined | cloudi_timestamp:iso8601(),
        mode_overload_total = undefined
            :: undefined | cloudi_service_api:nanoseconds_string(),
        time_offset_change = undefined
            :: undefined | cloudi_service_api:seconds_change_string(),
        time_offset_event = undefined
            :: undefined | cloudi_timestamp:iso8601(),
        file_counts = #{}
            :: #{cloudi_service_api:loglevel() := pos_integer()},
        error_read_count = 0
            :: non_neg_integer(),
        error_read_types = []
            :: list(file:posix() | badarg | terminated),
        error_write_count = 0
            :: non_neg_integer(),
        error_write_types = []
            :: list(file:posix() | badarg | terminated),
        error_sync_count = 0
            :: non_neg_integer(),
        error_sync_types = []
            :: list(file:posix() | badarg | terminated)
    }).

-define(LAGER_MD_KEY, '__lager_metadata'). % from lager module

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
    % The logger process can have a large message queue with
    % message_queue_len checked gradually, so it is best to keep the
    % message queue data off of the heap to avoid extra garbage collections.
    gen_server:start_link({local, ?MODULE}, ?MODULE, [LoggingConfig],
                          [{spawn_opt,
                            [{message_queue_data, off_heap}]}]).

%%-------------------------------------------------------------------------
%% @doc
%% ===Change the logging configuration.===
%% @end
%%-------------------------------------------------------------------------

-spec set(LoggingConfig :: #config_logging{}) ->
    'ok' | {'error', file:posix() | badarg | system_limit}.

set(#config_logging{file = FilePath} = LoggingConfig) ->
    if
        FilePath =:= undefined ->
            gen_server:cast(?MODULE, {set, LoggingConfig});
        is_list(FilePath) ->
            case filepath_exists(FilePath) of
                ok ->
                    gen_server:cast(?MODULE, {set, LoggingConfig});
                {error, _} = Error ->
                    Error
            end
    end.

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
    case filepath_exists(FilePath) of
        ok ->
            gen_server:cast(?MODULE, {file_set, FilePath});
        {error, _} = Error ->
            Error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Change the stdout output state.===
%% @end
%%-------------------------------------------------------------------------

-spec stdout_set(Stdout :: boolean()) ->
    'ok'.

stdout_set(Stdout) when is_boolean(Stdout) ->
    gen_server:cast(?MODULE, {stdout_set, Stdout}).

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
                                         undefined) ->
    'ok'.

formatters_set(FormattersConfig)
    when FormattersConfig =:= undefined;
         is_record(FormattersConfig, config_logging_formatters) ->
    gen_server:cast(?MODULE, {formatters_set, FormattersConfig}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Redirect this node's logging to a different node.===
%% @end
%%-------------------------------------------------------------------------

-spec redirect_set(Node :: atom()) ->
    'ok'.

redirect_set(Node) ->
    cloudi_core_i_nodes:logging_redirect_set(Node).

%%-------------------------------------------------------------------------
%% @doc
%% ===Update the destination of logging output.===
%% @end
%%-------------------------------------------------------------------------

-spec redirect_update(Node :: atom()) ->
    'ok'.

redirect_update(Node) ->
    gen_server:cast(?MODULE, {redirect_update, Node}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Critical log message.===
%% which indicates the system has failed and can not continue.
%% Called with ?LOG_FATAL(Format, []).
%% @end
%%-------------------------------------------------------------------------

-spec fatal(ModeInterface :: mode_interface(),
            Process :: atom() | {atom(), node()},
            FileName :: nonempty_string(),
            Line :: non_neg_integer(),
            Function :: atom(),
            Arity :: arity() | undefined,
            Format :: string(),
            Args :: list() | undefined) ->
    'ok'.

fatal(ModeInterface, Process, FileName, Line, Function, Arity, Format, Args) ->
    log_message_external(ModeInterface, Process, fatal, FileName, Line,
                         Function, Arity, Format, Args).

%%-------------------------------------------------------------------------
%% @doc
%% ===Error log message.===
%% which indicates a subsystem has failed but the failure is not fatal.
%% Called with ?LOG_ERROR(Format, []).
%% @end
%%-------------------------------------------------------------------------

-spec error(ModeInterface :: mode_interface(),
            Process :: atom() | {atom(), node()},
            FileName :: nonempty_string(),
            Line :: non_neg_integer(),
            Function :: atom(),
            Arity :: arity() | undefined,
            Format :: string(),
            Args :: list() | undefined) ->
    'ok'.

error(ModeInterface, Process, FileName, Line, Function, Arity, Format, Args) ->
    log_message_external(ModeInterface, Process, error, FileName, Line,
                         Function, Arity, Format, Args).

%%-------------------------------------------------------------------------
%% @doc
%% ===Warning log message.===
%% which indicates an unexpected occurance was found in a subsystem.
%% Called with ?LOG_WARN(Format, []).
%% @end
%%-------------------------------------------------------------------------

-spec warn(ModeInterface :: mode_interface(),
           Process :: atom() | {atom(), node()},
           FileName :: nonempty_string(),
           Line :: non_neg_integer(),
           Function :: atom(),
           Arity :: arity() | undefined,
           Format :: string(),
           Args :: list() | undefined) ->
    'ok'.

warn(ModeInterface, Process, FileName, Line, Function, Arity, Format, Args) ->
    log_message_external(ModeInterface, Process, warn, FileName, Line,
                         Function, Arity, Format, Args).

%%-------------------------------------------------------------------------
%% @doc
%% ===Info log message.===
%% which indicates a subsystem has changed state.
%% Called with ?LOG_INFO(Format, []).
%% @end
%%-------------------------------------------------------------------------

-spec info(ModeInterface :: mode_interface(),
           Process :: atom() | {atom(), node()},
           FileName :: nonempty_string(),
           Line :: non_neg_integer(),
           Function :: atom(),
           Arity :: arity() | undefined,
           Format :: string(),
           Args :: list() | undefined) ->
    'ok'.

info(ModeInterface, Process, FileName, Line, Function, Arity, Format, Args) ->
    log_message_external(ModeInterface, Process, info, FileName, Line,
                         Function, Arity, Format, Args).

%%-------------------------------------------------------------------------
%% @doc
%% ===Debug log message.===
%% which reports subsystem data that should be useful for debugging.
%% Called with ?LOG_DEBUG(Format, []).
%% @end
%%-------------------------------------------------------------------------

-spec debug(ModeInterface :: mode_interface(),
            Process :: atom() | {atom(), node()},
            FileName :: nonempty_string(),
            Line :: non_neg_integer(),
            Function :: atom(),
            Arity :: arity() | undefined,
            Format :: string(),
            Args :: list() | undefined) ->
    'ok'.

debug(ModeInterface, Process, FileName, Line, Function, Arity, Format, Args) ->
    log_message_external(ModeInterface, Process, debug, FileName, Line,
                         Function, Arity, Format, Args).

%%-------------------------------------------------------------------------
%% @doc
%% ===Trace log message.===
%% which reports subsystem data that is only for tracing execution.
%% Called with ?LOG_TRACE(Format, []).
%% @end
%%-------------------------------------------------------------------------

-spec trace(ModeInterface :: mode_interface(),
            Process :: atom() | {atom(), node()},
            FileName :: nonempty_string(),
            Line :: non_neg_integer(),
            Function :: atom(),
            Arity :: arity() | undefined,
            Format :: string(),
            Args :: list() | undefined) ->
    'ok'.

trace(ModeInterface, Process, FileName, Line, Function, Arity, Format, Args) ->
    log_message_external(ModeInterface, Process, trace, FileName, Line,
                         Function, Arity, Format, Args).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get logging status.===
%% @end
%%-------------------------------------------------------------------------

-spec status(Timeout :: pos_integer() | infinity) ->
    {ok, cloudi_service_api:logging_status()} | {error, timeout | noproc}.

status(Timeout) ->
    ?CATCH_EXIT(gen_server:call(?MODULE, status, Timeout)).

%%-------------------------------------------------------------------------
%% @doc
%% ===Reset logging status.===
%% @end
%%-------------------------------------------------------------------------

-spec status_reset(Timeout :: pos_integer() | infinity) ->
    ok | {error, timeout | noproc}.

status_reset(Timeout) ->
    ?CATCH_EXIT(gen_server:call(?MODULE, status_reset, Timeout)).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get metadata.===
%% @end
%%-------------------------------------------------------------------------

-spec metadata_get() ->
    list({atom(), any()}) | #{}.

metadata_get() ->
    case erlang:get(?LOGGER_METADATA_PDICT_KEY) of
        ?LAGER_MD_KEY ->
            lager_metadata_get();
        undefined ->
            [];
        Map when is_map(Map) ->
            Map
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Set metadata.===
%% @end
%%-------------------------------------------------------------------------

-spec metadata_set(list({atom(), any()}) | #{}) ->
    ok.

metadata_set(L)
    when is_list(L) ->
    erlang:put(?LOGGER_METADATA_PDICT_KEY, ?LAGER_MD_KEY),
    lager_metadata_set(L);
metadata_set(Map)
    when is_map(Map) ->
    erlang:put(?LOGGER_METADATA_PDICT_KEY, Map),
    ok.

%%-------------------------------------------------------------------------
%% @doc
%% ===Lager formatter example with default output.===
%% @end
%%-------------------------------------------------------------------------

format(Msg, Config) ->
    format(Msg, Config, []).

%%-------------------------------------------------------------------------
%% @doc
%% ===Lager formatter example with default output.===
%% @end
%%-------------------------------------------------------------------------

-spec format(Msg :: {lager_msg,
                     Destinations :: list(),
                     Metadata :: list({atom(), any()}),
                     Severity :: debug | emergency | error | info | warning,
                     Datetime :: {string(), string()},
                     Timestamp :: erlang:timestamp(),
                     Message :: list()},
             Config :: list(),
             Colors :: any()) ->
    iolist().

format(Msg, _Config, _) ->
    {lager_msg,
     _Destinations,
     MetaData,
     Severity,
     _DateTime,
     Timestamp,
     Message} = Msg,
    Level = lager_severity_input(Severity),
    Defaults = [{function, undefined},
                {arity, undefined},
                {module, undefined},
                {file, undefined},
                {line, undefined},
                {node, undefined},
                {pid, undefined}],
    [Function, Arity, Module, File, Line, Node, PidStr |
     MetaDataNew] = cloudi_proplists:take_values(Defaults, MetaData),
    FileName = if
        File =:= undefined ->
            if
                is_atom(Module), Module /= undefined ->
                    erlang:atom_to_list(Module)
            end;
        is_list(File) ->
            File
    end,
    LogMessage = Message,
    format_line(Level, Timestamp, Node, PidStr,
                FileName, Line, Function, Arity,
                MetaDataNew, LogMessage).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create an ISO8601 timestamp from microseconds since the UNIX epoch.===
%% (The UNIX epoch is 1970-01-01T00:00:00.000000Z)
%% @end
%%-------------------------------------------------------------------------

-spec microseconds_to_string(TotalMicroSeconds :: non_neg_integer()) ->
    cloudi_timestamp:iso8601().

microseconds_to_string(TotalMicroSeconds) ->
    TotalSeconds = TotalMicroSeconds div 1000000,
    MegaSeconds = TotalSeconds div 1000000,
    Seconds = TotalSeconds - MegaSeconds * 1000000,
    MicroSeconds = TotalMicroSeconds - TotalSeconds * 1000000,
    timestamp_iso8601({MegaSeconds, Seconds, MicroSeconds}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create an ISO8601 timestamp from milliseconds since the UNIX epoch.===
%% (The UNIX epoch is 1970-01-01T00:00:00.000Z)
%% @end
%%-------------------------------------------------------------------------

-spec milliseconds_to_string(TotalMilliSeconds :: non_neg_integer()) ->
    cloudi_timestamp:iso8601().

milliseconds_to_string(TotalMilliSeconds) ->
    TotalSeconds = TotalMilliSeconds div 1000,
    MegaSeconds = TotalSeconds div 1000000,
    Seconds = TotalSeconds - MegaSeconds * 1000000,
    MilliSeconds = TotalMilliSeconds - TotalSeconds * 1000,
    [DateYYYY0, DateYYYY1, DateYYYY2, DateYYYY3, $-,
     DateMM0, DateMM1, $-, DateDD0, DateDD1, $T,
     TimeHH0, TimeHH1, $:, TimeMM0, TimeMM1, $:, TimeSS0, TimeSS1, $.,
     MicroSeconds0, MicroSeconds1, MicroSeconds2, _, _, _,
     $Z] = timestamp_iso8601({MegaSeconds, Seconds, MilliSeconds * 1000}),
    [DateYYYY0, DateYYYY1, DateYYYY2, DateYYYY3, $-,
     DateMM0, DateMM1, $-, DateDD0, DateDD1, $T,
     TimeHH0, TimeHH1, $:, TimeMM0, TimeMM1, $:, TimeSS0, TimeSS1, $.,
     MicroSeconds0, MicroSeconds1, MicroSeconds2, $Z].

%%-------------------------------------------------------------------------
%% @doc
%% ===Create an ISO8601 timestamp from seconds since the UNIX epoch.===
%% (The UNIX epoch is 1970-01-01T00:00:00Z)
%% @end
%%-------------------------------------------------------------------------

-spec seconds_to_string(TotalSeconds :: non_neg_integer()) ->
    cloudi_timestamp:iso8601_seconds().

seconds_to_string(TotalSeconds) ->
    MegaSeconds = TotalSeconds div 1000000,
    Seconds = TotalSeconds - MegaSeconds * 1000000,
    DateTimeUTC = calendar:now_to_universal_time({MegaSeconds, Seconds, 0}),
    datetime_to_string(DateTimeUTC).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create an ISO8601 timestamp from a datetime in UTC.===
%% @end
%%-------------------------------------------------------------------------

-spec datetime_to_string(DateTimeUTC :: calendar:datetime()) ->
    cloudi_timestamp:iso8601_seconds().

datetime_to_string(DateTimeUTC) ->
    datetime_iso8601(DateTimeUTC, undefined).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([#config_logging{file = FilePath,
                      file_sync = FileSync,
                      stdout = Stdout,
                      level = MainLevel,
                      queue_mode_async = QueueModeAsync,
                      queue_mode_sync = QueueModeSync,
                      queue_mode_overload = QueueModeOverload,
                      redirect = NodeLogger,
                      syslog = SyslogConfig,
                      formatters = FormattersConfig,
                      log_time_offset = LogTimeOffset,
                      aspects_log_before = AspectsLogBefore,
                      aspects_log_after = AspectsLogAfter}]) ->
    % due to queue_mode sync and overload it is best to have this process
    % running with high priority
    normal = erlang:process_flag(priority, high),
    StdoutPort = stdout_open(Stdout),
    FormattersLevel = case FormattersConfig of
        undefined ->
            undefined;
        #config_logging_formatters{level = FormattersLevel0} ->
            FormattersLevel0
    end,
    #state{mode = Mode} = State =
        #state{file_sync = FileSync,
               stdout = StdoutPort,
               main_level = MainLevel,
               queue_mode_async = QueueModeAsync,
               queue_mode_sync = QueueModeSync,
               queue_mode_overload = QueueModeOverload,
               formatters = FormattersConfig,
               formatters_level = FormattersLevel,
               log_time_offset = LogTimeOffset,
               log_time_offset_nanoseconds = time_offset_nanoseconds(),
               log_time_offset_monitor = erlang:monitor(time_offset,
                                                        clock_service),
               aspects_log_before = AspectsLogBefore,
               aspects_log_after = AspectsLogAfter,
               logger_node = node(),
               logger_self = self()},
    {SyslogResult,
     #state{syslog_level = SyslogLevel} = StateNext} = syslog_open(SyslogConfig,
                                                                   false,
                                                                   State),
    Level = log_level([MainLevel, SyslogLevel, FormattersLevel]),
    Destination = if
        NodeLogger == node(); NodeLogger =:= undefined ->
            ?MODULE;
        true ->
            {?MODULE, NodeLogger}
    end,
    false = erlang:process_flag(trap_exit, true),
    case load_interface_module(Level, Mode, Destination) of
        {ok, Binary} when Destination == ?MODULE ->
            case log_file_open(FilePath,
                               StateNext#state{interface_module = Binary,
                                               level = Level,
                                               destination = Destination}) of
                {ok, StateNew} ->
                    log_init(SyslogResult, StateNew);
                {error, Reason} ->
                    {stop, Reason}
            end;
        {ok, Binary} ->
            case ?LOG_T0_INFO("redirecting log output to ~ts",
                              [NodeLogger],
                              StateNext#state{interface_module = Binary,
                                              file_path = FilePath,
                                              level = Level,
                                              destination = ?MODULE}) of
                {ok, StateNew} ->
                    log_init(SyslogResult,
                             StateNew#state{destination = Destination});
                {{error, Reason}, _} ->
                    {stop, Reason}
            end;
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call({Level, Timestamp, Node, Pid,
             FileName, Line, Function, Arity,
             MetaData, LogMessage}, _, State) ->
    case log_message_internal(sync, Level, Timestamp, Node, Pid,
                              FileName, Line, Function, Arity,
                              MetaData, LogMessage, State) of
        {ok, StateNext} ->
            case log_mode_check(Timestamp, StateNext) of
                {ok, StateNew} ->
                    {reply, ok, StateNew};
                {error, Reason} ->
                    {stop, Reason, ok, StateNext}
            end;
        {{error, Reason}, StateNext} ->
            {stop, Reason, ok, StateNext}
    end;
handle_call(status, _,
            #state{mode = Mode,
                   mode_sync_start = SyncStart,
                   mode_sync_start_event = SyncStartEvent,
                   mode_sync_end = SyncEnd,
                   mode_sync_end_event = SyncEndEvent,
                   mode_sync_total = SyncTotal,
                   mode_overload_start = OverloadStart,
                   mode_overload_start_event = OverloadStartEvent,
                   mode_overload_end = OverloadEnd,
                   mode_overload_end_event = OverloadEndEvent,
                   mode_overload_total = OverloadTotal,
                   time_offset_change = TimeOffsetChange,
                   time_offset_event = TimeOffsetEvent,
                   file_counts = FileCounts,
                   error_read_count = ErrorReadCount,
                   error_read_types = ErrorReadTypes,
                   error_write_count = ErrorWriteCount,
                   error_write_types = ErrorWriteTypes,
                   error_sync_count = ErrorSyncCount,
                   error_sync_types = ErrorSyncTypes} = State) ->
    TimeOffset = erlang:time_offset(),
    Status0 = if
        ErrorReadCount > 0 ->
            [{file_read_fail_count, erlang:integer_to_list(ErrorReadCount)},
             {file_read_fail_types, ErrorReadTypes}];
        ErrorReadCount == 0 ->
            []
    end,
    Status1 = if
        ErrorWriteCount > 0 ->
            [{file_write_fail_count, erlang:integer_to_list(ErrorWriteCount)},
             {file_write_fail_types, ErrorWriteTypes} | Status0];
        ErrorWriteCount == 0 ->
            Status0
    end,
    Status2 = if
        ErrorSyncCount > 0 ->
            [{file_sync_fail_count, erlang:integer_to_list(ErrorSyncCount)},
             {file_sync_fail_types, ErrorSyncTypes} | Status1];
        ErrorSyncCount == 0 ->
            Status1
    end,
    Status3 = case maps:find(trace, FileCounts) of
        {ok, FileCountTrace} ->
            [{file_messages_trace,
              erlang:integer_to_list(FileCountTrace)} | Status2];
        error ->
            Status2
    end,
    Status4 = case maps:find(debug, FileCounts) of
        {ok, FileCountDebug} ->
            [{file_messages_debug,
              erlang:integer_to_list(FileCountDebug)} | Status3];
        error ->
            Status3
    end,
    Status5 = case maps:find(info, FileCounts) of
        {ok, FileCountInfo} ->
            [{file_messages_info,
              erlang:integer_to_list(FileCountInfo)} | Status4];
        error ->
            Status4
    end,
    Status6 = case maps:find(warn, FileCounts) of
        {ok, FileCountWarn} ->
            [{file_messages_warn,
              erlang:integer_to_list(FileCountWarn)} | Status5];
        error ->
            Status5
    end,
    Status7 = case maps:find(error, FileCounts) of
        {ok, FileCountError} ->
            [{file_messages_error,
              erlang:integer_to_list(FileCountError)} | Status6];
        error ->
            Status6
    end,
    Status8 = case maps:find(fatal, FileCounts) of
        {ok, FileCountFatal} ->
            [{file_messages_fatal,
              erlang:integer_to_list(FileCountFatal)} | Status7];
        error ->
            Status7
    end,
    Status9 = if
        TimeOffsetChange =:= undefined ->
            Status8;
        is_list(TimeOffsetChange) ->
            [{time_offset_last_change,
              TimeOffsetChange},
             {time_offset_last_event,
              TimeOffsetEvent} | Status8]
    end,
    Status10 = if
        OverloadStart =:= undefined ->
            Status9;
        OverloadEnd =:= undefined ->
            OverloadStartMicroSeconds = cloudi_timestamp:
                                        convert(OverloadStart + TimeOffset,
                                                native, microsecond),
            [{queue_mode_overload_last_start,
              microseconds_to_string(OverloadStartMicroSeconds)},
             {queue_mode_overload_last_start_event,
              OverloadStartEvent} | Status9];
        is_list(OverloadTotal) ->
            OverloadStartMicroSeconds = cloudi_timestamp:
                                        convert(OverloadStart + TimeOffset,
                                                native, microsecond),
            OverloadEndMicroSeconds = cloudi_timestamp:
                                      convert(OverloadEnd + TimeOffset,
                                              native, microsecond),
            [{queue_mode_overload_last_start,
              microseconds_to_string(OverloadStartMicroSeconds)},
             {queue_mode_overload_last_start_event,
              OverloadStartEvent},
             {queue_mode_overload_last_end,
              microseconds_to_string(OverloadEndMicroSeconds)},
             {queue_mode_overload_last_end_event,
              OverloadEndEvent},
             {queue_mode_overload_last_total,
              OverloadTotal} | Status9]
    end,
    Status11 = if
        SyncStart =:= undefined ->
            Status10;
        SyncEnd =:= undefined ->
            SyncStartMicroSeconds = cloudi_timestamp:
                                    convert(SyncStart + TimeOffset,
                                            native, microsecond),
            [{queue_mode_sync_last_start,
              microseconds_to_string(SyncStartMicroSeconds)},
             {queue_mode_sync_last_start_event,
              SyncStartEvent} | Status10];
        is_list(SyncTotal) ->
            SyncStartMicroSeconds = cloudi_timestamp:
                                    convert(SyncStart + TimeOffset,
                                            native, microsecond),
            SyncEndMicroSeconds = cloudi_timestamp:
                                  convert(SyncEnd + TimeOffset,
                                          native, microsecond),
            [{queue_mode_sync_last_start,
              microseconds_to_string(SyncStartMicroSeconds)},
             {queue_mode_sync_last_start_event,
              SyncStartEvent},
             {queue_mode_sync_last_end,
              microseconds_to_string(SyncEndMicroSeconds)},
             {queue_mode_sync_last_end_event,
              SyncEndEvent},
             {queue_mode_sync_last_total,
              SyncTotal} | Status10]
    end,
    StatusN = [{queue_mode, Mode} | Status11],
    {reply, {ok, StatusN}, State};
handle_call(status_reset, _, State) ->
    {reply, ok,
     State#state{mode_sync_start = undefined,
                 mode_sync_start_event = undefined,
                 mode_sync_end = undefined,
                 mode_sync_end_event = undefined,
                 mode_sync_total = undefined,
                 mode_overload_start = undefined,
                 mode_overload_start_event = undefined,
                 mode_overload_end = undefined,
                 mode_overload_end_event = undefined,
                 mode_overload_total = undefined,
                 time_offset_change = undefined,
                 time_offset_event = undefined,
                 file_counts = #{},
                 error_read_count = 0,
                 error_read_types = [],
                 error_write_count = 0,
                 error_write_types = [],
                 error_sync_count = 0,
                 error_sync_types = []}};
handle_call(Request, _, State) ->
    {stop, cloudi_string:format("Unknown call \"~w\"", [Request]),
     error, State}.

handle_cast({set, LoggingConfig}, State) ->
    case log_config_set(LoggingConfig, State) of
        {ok, StateNext} ->
            case log_level_update(StateNext) of
                {ok, StateNew} ->
                    {noreply, StateNew};
                {{error, Reason}, StateNew} ->
                    {stop, Reason, StateNew}
            end;
        {{error, Reason}, StateNew} ->
            {stop, Reason, StateNew}
    end;
handle_cast({file_set, FilePath}, State) ->
    case log_config_file_set(FilePath, State) of
        {ok, StateNew} ->
            {noreply, StateNew};
        {{error, Reason}, StateNew} ->
            {stop, Reason, StateNew}
    end;
handle_cast({stdout_set, Stdout}, State) ->
    {ok, StateNew} = log_config_stdout_set(Stdout, State),
    {noreply, StateNew};
handle_cast({level_set, MainLevel}, State) ->
    case log_config_main_level_set(MainLevel, State) of
        {ok, StateNext} ->
            case log_level_update(StateNext) of
                {ok, StateNew} ->
                    {noreply, StateNew};
                {{error, Reason}, StateNew} ->
                    {stop, Reason, StateNew}
            end;
        {{error, Reason}, StateNew} ->
            {stop, Reason, StateNew}
    end;
handle_cast({syslog_set, SyslogConfig}, State) ->
    case log_config_syslog_set(SyslogConfig, State) of
        {ok, StateNext} ->
            case log_level_update(StateNext) of
                {ok, StateNew} ->
                    {noreply, StateNew};
                {{error, Reason}, StateNew} ->
                    {stop, Reason, StateNew}
            end;
        {{error, Reason}, StateNew} ->
            {stop, Reason, StateNew}
    end;
handle_cast({formatters_set, FormattersConfigNew}, State) ->
    case log_config_formatters_set(FormattersConfigNew, State) of
        {ok, StateNext} ->
            case log_level_update(StateNext) of
                {ok, StateNew} ->
                    {noreply, StateNew};
                {{error, Reason}, StateNew} ->
                    {stop, Reason, StateNew}
            end;
        {{error, Reason}, StateNew} ->
            {stop, Reason, StateNew}
    end;
handle_cast({redirect_update, Node}, State) ->
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
             FileName, Line, Function, Arity,
             MetaData, LogMessage}, State) ->
    case log_message_internal(async, Level, Timestamp, Node, Pid,
                              FileName, Line, Function, Arity,
                              MetaData, LogMessage, State) of
        {ok, StateNext} ->
            case log_mode_check(Timestamp, StateNext) of
                {ok, StateNew} ->
                    {noreply, StateNew};
                {error, Reason} ->
                    {stop, Reason, StateNext}
            end;
        {{error, Reason}, StateNext} ->
            {stop, Reason, StateNext}
    end;
handle_cast(Request, State) ->
    {stop, cloudi_string:format("Unknown cast \"~w\"", [Request]), State}.

handle_info({'DOWN', _, process, Process, Info},
            #state{syslog = Syslog} = State) ->
    {Entity, StateUpdated} = if
        Process == Syslog ->
            {"syslog",
             State#state{syslog = undefined,
                         syslog_level = undefined}};
        true ->
            % will happen if syslog is stopped successfully (asynchronously)
            % but should not occur with other processes
            {io_lib:format("process(~w)", [Process]), State}
    end,
    case log_level_update(StateUpdated) of
        {ok, StateNext}
            when Info =:= normal ->
            {noreply, StateNext};
        {ok, StateNext} ->
            case ?LOG_T0_ERROR("~s died: ~tw",
                               [Entity, Info], StateNext) of
                {ok, StateNew} ->
                    {noreply, StateNew};
                {{error, Reason}, StateNew} ->
                    {stop, Reason, StateNew}
            end;
        {{error, Reason}, StateNext} ->
            {stop, Reason, StateNext}
    end;
handle_info({'CHANGE', Monitor, time_offset, clock_service, TimeOffset},
            #state{log_time_offset = LogTimeOffset,
                   log_time_offset_nanoseconds = NanoSecondsOld,
                   log_time_offset_monitor = Monitor} = State) ->
    Timestamp = cloudi_timestamp:timestamp(),
    NanoSecondsNew = time_offset_to_nanoseconds(TimeOffset),
    TimeOffsetChange = nanoseconds_to_seconds_change_string(NanoSecondsOld,
                                                            NanoSecondsNew),
    TimeOffsetEvent = timestamp_iso8601(Timestamp),
    case ?LOG_AT_T0(LogTimeOffset, Timestamp,
                    "Erlang time_offset changed ~s",
                    [TimeOffsetChange],
                    State#state{log_time_offset_nanoseconds = NanoSecondsNew,
                                time_offset_change = TimeOffsetChange,
                                time_offset_event = TimeOffsetEvent}) of
        {ok, StateNew} ->
            {noreply, StateNew};
        {{error, Reason}, StateNew} ->
            {stop, Reason, StateNew}
    end;
handle_info({'EXIT', _, Reason},
            #state{logger_self = Self} = State) ->
    if
        Reason =:= shutdown;
        element(1, Reason) =:= shutdown ->
            TerminateTimeMax = cloudi_timestamp:milliseconds_monotonic() +
                               ?TIMEOUT_TERMINATE_MAX,
            true = ?TERMINATE_DELAY < ?TIMEOUT_TERMINATE_MAX,
            _ = erlang:send_after(?TERMINATE_DELAY, Self,
                                  {terminate, Reason, TerminateTimeMax}),
            {noreply, State};
        true ->
            {stop, Reason, State}
    end;
handle_info({terminate, Reason, TerminateTimeMax} = Terminate,
            #state{logger_self = Self} = State) ->
    {message_queue_len,
     MessageQueueLength} = erlang:process_info(Self, message_queue_len),
    TerminateDelay = if
        MessageQueueLength > 0 ->
            RemainingMilliSeconds = TerminateTimeMax -
                                    cloudi_timestamp:milliseconds_monotonic(),
            if
                RemainingMilliSeconds =< 0 ->
                    undefined;
                RemainingMilliSeconds < ?TERMINATE_DELAY ->
                    RemainingMilliSeconds;
                true ->
                    ?TERMINATE_DELAY
            end;
        true ->
            undefined
    end,
    if
        TerminateDelay =:= undefined ->
            {stop, Reason, State};
        is_integer(TerminateDelay) ->
            _ = erlang:send_after(TerminateDelay, Self, Terminate),
            {noreply, State}
    end;
handle_info(file_sync, State) ->
    case log_file_sync(State) of
        {ok, StateNew} ->
            {noreply, StateNew#state{file_sync_timer = undefined}};
        {{error, Reason}, StateNew} ->
            {stop, Reason, StateNew}
    end;
handle_info(Request, State) ->
    {stop, cloudi_string:format("Unknown info \"~w\"", [Request]), State}.

terminate(_, #state{file_fd = FileFd,
                    stdout = StdoutPort,
                    syslog = Syslog,
                    log_time_offset_monitor = Monitor}) ->
    _ = (catch file:close(FileFd)),
    ok = stdout_close(StdoutPort),
    ok = syslog_close(Syslog),
    true = erlang:demonitor(Monitor),
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

log_init(ok, State) ->
    {ok, State};
log_init({error, Reason}, State) ->
    case ?LOG_T0_ERROR("syslog error: ~tp", [Reason], State) of
        {ok, _} ->
            {stop, syslog};
        {{error, Reason}, _} ->
            {stop, Reason}
    end.

log_config_set(#config_logging{file = FilePath,
                               stdout = Stdout,
                               level = MainLevel,
                               queue_mode_async = QueueModeAsync,
                               queue_mode_sync = QueueModeSync,
                               queue_mode_overload = QueueModeOverload,
                               redirect = NodeLogger,
                               syslog = SyslogConfig,
                               formatters = FormattersConfig,
                               log_time_offset = LogTimeOffset,
                               aspects_log_before = AspectsLogBefore,
                               aspects_log_after = AspectsLogAfter},
               State) ->
    case accum([{MainLevel, fun log_config_main_level_set/2},
                {FilePath, fun log_config_file_set/2},
                {Stdout, fun log_config_stdout_set/2},
                {SyslogConfig, fun log_config_syslog_set/2},
                {FormattersConfig, fun log_config_formatters_set/2}],
               State#state{queue_pending = 0,
                           queue_mode_async = QueueModeAsync,
                           queue_mode_sync = QueueModeSync,
                           queue_mode_overload = QueueModeOverload,
                           log_time_offset = LogTimeOffset,
                           aspects_log_before = AspectsLogBefore,
                           aspects_log_after = AspectsLogAfter}) of
        {ok, _} = Success ->
            ok = cloudi_core_i_nodes:logging_redirect_set(NodeLogger),
            Success;
        {{error, _}, _} = ErrorResult ->
            ErrorResult
    end.

log_config_main_level_set(MainLevel,
                          #state{main_level = MainLevel} = State) ->
    {ok, State};
log_config_main_level_set(MainLevelNew,
                          #state{main_level = MainLevelOld} = State) ->
    case ?LOG_T0_INFO("changing main loglevel from ~s to ~s",
                      [MainLevelOld, MainLevelNew], State) of
        {ok, StateNew} ->
            {ok, StateNew#state{main_level = MainLevelNew}};
        {{error, _}, _} = ErrorResult ->
            ErrorResult
    end.

log_config_file_set(FilePath,
                    #state{file_path = FilePath} = State) ->
    {ok, State};
log_config_file_set(FilePathNew,
                    #state{file_path = FilePathOld} = State) ->
    FilePathNewStr = if
        is_list(FilePathNew) ->
            io_lib:format("\"~ts\"", [FilePathNew]);
        FilePathNew =:= undefined ->
            "'undefined'"
    end,
    FilePathOldStr = if
        is_list(FilePathOld) ->
            io_lib:format("\"~ts\"", [FilePathOld]);
        FilePathOld =:= undefined ->
            "'undefined'"
    end,
    case ?LOG_T0_INFO("changing file path from ~ts to ~ts",
                      [FilePathOldStr, FilePathNewStr], State) of
        {ok, #state{file_fd = FileFdOld} = StateNew} ->
            _ = file:close(FileFdOld),
            {ok, StateNew#state{file_path = FilePathNew,
                                file_fd = undefined,
                                file_inode = undefined}};
        {{error, _}, _} = ErrorResult ->
            ErrorResult
    end.

log_config_stdout_set(Stdout,
                      #state{stdout = StdoutPort} = State)
    when Stdout =:= is_port(StdoutPort) ->
    {ok, State};
log_config_stdout_set(Stdout,
                      #state{stdout = StdoutPort} = State) ->
    StdoutPortNew = if
        Stdout =:= true ->
            stdout_open(true);
        Stdout =:= false ->
            ok = stdout_close(StdoutPort),
            undefined
    end,
    {ok, State#state{stdout = StdoutPortNew}}.

log_config_syslog_set(SyslogConfig,
                      #state{syslog = SyslogOld,
                             syslog_level = SyslogLevelOld} = State) ->
    SyslogLevelNew = case SyslogConfig of
        undefined ->
            undefined;
        #config_logging_syslog{level = SyslogLevelNew0} ->
            SyslogLevelNew0
    end,
    SwitchF = fun(StateSwitch) ->
        ok = syslog_close(SyslogOld),
        syslog_open(SyslogConfig, true, StateSwitch)
    end,
    if
        SyslogLevelNew /= SyslogLevelOld ->
            case ?LOG_T0_INFO("changing syslog loglevel from ~s to ~s",
                              [SyslogLevelOld, SyslogLevelNew], State) of
                {ok, StateNew} ->
                    SwitchF(StateNew);
                {{error, _}, _} = ErrorResult ->
                    ErrorResult
            end;
        true ->
            SwitchF(State)
    end.

log_config_formatters_set(FormattersConfigNew,
                          #state{formatters_level =
                                     FormattersLevelOld} = State) ->
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
            case ?LOG_T0_INFO("changing formatters loglevel from ~s to ~s",
                              [FormattersLevelOld, FormattersLevelNew],
                              State) of
                {ok, StateNew} ->
                    {ok, SwitchF(StateNew)};
                {{error, _}, _} = ErrorResult ->
                    ErrorResult
            end;
        true ->
            {ok, SwitchF(State)}
    end.

log_level_update(#state{main_level = MainLevel,
                        level = LevelOld,
                        mode = Mode,
                        destination = Destination,
                        syslog_level = SyslogLevel,
                        formatters_level = FormattersLevel} = State) ->
    case log_level([MainLevel, SyslogLevel, FormattersLevel]) of
        LevelOld ->
            {ok, State};
        LevelNew ->
            case load_interface_module(LevelNew, Mode, Destination) of
                {ok, Binary} ->
                    StateNew = State#state{
                        interface_module = Binary,
                        level = LevelNew},
                    ?LOG_T1_INFO("changed loglevel from ~s to ~s",
                                 [LevelOld, LevelNew], StateNew),
                    {ok, StateNew};
                {error, _} = Error ->
                    {Error, State}
            end
    end.

-spec format_line(Level :: cloudi_service_api:loglevel(),
                  Timestamp :: erlang:timestamp(),
                  Node :: node(),
                  Pid :: pid() | string() | undefined,
                  FileName :: nonempty_string(),
                  Line :: non_neg_integer(),
                  Function :: atom(),
                  Arity :: arity() | undefined,
                  MetaData :: any(),
                  LogMessage :: iodata()) ->
    iolist(). % utf8 encoded strings

format_line(Level, Timestamp, Node, Pid,
            FileName, Line, Function, Arity, MetaData, LogMessage) ->
    LineStr = if
        Line =:= 0 ->
            "";
        is_integer(Line), Line > 0 ->
            int_to_dec_list(Line)
    end,
    FunctionArity = if
        Function =:= undefined ->
            "";
        Arity =:= undefined ->
            erlang:atom_to_binary(Function, utf8);
        true ->
            [erlang:atom_to_binary(Function, utf8),
             [$/ | int_to_dec_list(Arity)]]
    end,
    PidStr = if
        is_pid(Pid) ->
            erlang:pid_to_list(Pid);
        is_list(Pid) ->
            Pid;
        Pid =:= undefined ->
            ""
    end,
    NodeBin = erlang:atom_to_binary(Node, utf8),
    MetaDataStr = if
        MetaData == []; map_size(MetaData) == 0 ->
            "";
        true ->
            io_lib:format("~tp~n", [MetaData])
    end,
    [timestamp_iso8601(Timestamp), $\s, log_level_to_string(Level), $\s,
     $(,
     FileName, $:,
     LineStr, $:,
     FunctionArity, $:,
     PidStr, $:,
     NodeBin,
     $), $\n,
     MetaDataStr, LogMessage, $\n].

timestamp_iso8601({_, _, MicroSeconds} = Timestamp) ->
    datetime_iso8601(calendar:now_to_universal_time(Timestamp), MicroSeconds).

% ISO 8601 for date/time http://www.w3.org/TR/NOTE-datetime
datetime_iso8601({{DateYYYY, DateMM, DateDD},
                  {TimeHH, TimeMM, TimeSS}},
                 undefined) ->
    [DateYYYY0, DateYYYY1,
     DateYYYY2, DateYYYY3] = int_to_dec_list(DateYYYY, 4, $0),
    [DateMM0, DateMM1] = int_to_dec_list(DateMM, 2, $0),
    [DateDD0, DateDD1] = int_to_dec_list(DateDD, 2, $0),
    [TimeHH0, TimeHH1] = int_to_dec_list(TimeHH, 2, $0),
    [TimeMM0, TimeMM1] = int_to_dec_list(TimeMM, 2, $0),
    [TimeSS0, TimeSS1] = int_to_dec_list(TimeSS, 2, $0),
    [DateYYYY0, DateYYYY1, DateYYYY2, DateYYYY3, $-,
     DateMM0, DateMM1, $-, DateDD0, DateDD1, $T,
     TimeHH0, TimeHH1, $:, TimeMM0, TimeMM1, $:, TimeSS0, TimeSS1, $Z];
datetime_iso8601({{DateYYYY, DateMM, DateDD},
                  {TimeHH, TimeMM, TimeSS}},
                 MicroSeconds) ->
    [DateYYYY0, DateYYYY1,
     DateYYYY2, DateYYYY3] = int_to_dec_list(DateYYYY, 4, $0),
    [DateMM0, DateMM1] = int_to_dec_list(DateMM, 2, $0),
    [DateDD0, DateDD1] = int_to_dec_list(DateDD, 2, $0),
    [TimeHH0, TimeHH1] = int_to_dec_list(TimeHH, 2, $0),
    [TimeMM0, TimeMM1] = int_to_dec_list(TimeMM, 2, $0),
    [TimeSS0, TimeSS1] = int_to_dec_list(TimeSS, 2, $0),
    [MicroSeconds0, MicroSeconds1,
     MicroSeconds2, MicroSeconds3,
     MicroSeconds4, MicroSeconds5] = int_to_dec_list(MicroSeconds, 6, $0),
    [DateYYYY0, DateYYYY1, DateYYYY2, DateYYYY3, $-,
     DateMM0, DateMM1, $-, DateDD0, DateDD1, $T,
     TimeHH0, TimeHH1, $:, TimeMM0, TimeMM1, $:, TimeSS0, TimeSS1, $.,
     MicroSeconds0, MicroSeconds1,
     MicroSeconds2, MicroSeconds3,
     MicroSeconds4, MicroSeconds5, $Z].

log_message_formatter_call(Level, Timestamp, Node, Pid,
                           FileName, Line, Function, Arity,
                           MetaData, LogMessage,
                           #config_logging_formatter{
                               output = undefined,
                               formatter = Formatter,
                               formatter_config = FormatterConfig},
                           #state{logger_node = ErrorNode,
                                  logger_self = ErrorSelf}) ->
    % A formatter module has:
    % required: format(Msg, Config)
    % optional: format(Msg, Config, Colors)
    Msg = lager_msg(Level, Timestamp, Node, Pid,
                    FileName, Line, Function, Arity,
                    MetaData, LogMessage),
    try Formatter:format(Msg, FormatterConfig)
    catch
        ErrorType:Error:ErrorStackTrace ->
            ErrorMessage = cloudi_string:
                           format_to_binary("formatter(~tp) ~tp ~tp~n~tp",
                                            [Formatter, ErrorType, Error,
                                             ErrorStackTrace]),
            [format_line(Level, Timestamp, Node, Pid,
                         FileName, Line, Function, Arity,
                         MetaData, LogMessage),
             format_line(error, timestamp_increment(Timestamp),
                         ErrorNode, ErrorSelf, ?FILE, ?LINE,
                         undefined, undefined, [], ErrorMessage)]
    end;
log_message_formatter_call(Level, Timestamp, Node, Pid,
                           FileName, Line, Function, Arity,
                           MetaData, LogMessage,
                           #config_logging_formatter{
                               output = Output,
                               output_name = OutputName},
                           #state{logger_node = ErrorNode,
                                  logger_self = ErrorSelf}) ->
    Msg = lager_msg(Level, Timestamp, Node, Pid,
                    FileName, Line, Function, Arity,
                    MetaData, LogMessage),
    try gen_event:notify(OutputName, {log, Msg}) of
        ok ->
            format_line(Level, Timestamp, Node, Pid,
                        FileName, Line, Function, Arity,
                        MetaData, LogMessage)
    catch
        error:badarg ->
            % output module is not currently running,
            % it likely exceeded the maximum restart intensity
            % (which is logged elsewhere via the Erlang/OTP kernel logger)
            format_line(Level, Timestamp, Node, Pid,
                        FileName, Line, Function, Arity,
                        MetaData, LogMessage);
        ErrorType:Error:ErrorStackTrace ->
            ErrorMessage = cloudi_string:
                           format_to_binary("output(~tp) ~tp ~tp~n~tp",
                                            [Output, ErrorType, Error,
                                             ErrorStackTrace]),
            [format_line(Level, Timestamp, Node, Pid,
                         FileName, Line, Function, Arity,
                         MetaData, LogMessage),
             format_line(error, timestamp_increment(Timestamp),
                         ErrorNode, ErrorSelf, ?FILE, ?LINE,
                         undefined, undefined, [], ErrorMessage)]
    end.

log_message_formatter(Level, Timestamp, Node, Pid,
                      FileName, Line, Function, Arity,
                      MetaData, LogMessage,
                      #config_logging_formatter{
                          level = FormatterLevel} = FormatterConfig, State) ->
    case log_level_allowed(FormatterLevel, Level) of
        true ->
            log_message_formatter_call(Level, Timestamp, Node, Pid,
                                       FileName, Line, Function, Arity,
                                       MetaData, LogMessage,
                                       FormatterConfig, State);
        false ->
            format_line(Level, Timestamp, Node, Pid,
                        FileName, Line, Function, Arity,
                        MetaData, LogMessage)
    end.

log_message_formatters(Level, Timestamp, Node, Pid,
                       FileName, Line, Function, Arity,
                       MetaData, LogMessage,
                       undefined, _) ->
    format_line(Level, Timestamp, Node, Pid,
                FileName, Line, Function, Arity,
                MetaData, LogMessage);
log_message_formatters(Level, Timestamp, Node, Pid,
                       FileName, Line, Function, Arity,
                       MetaData, LogMessage,
                       #config_logging_formatters{
                           default = Default,
                           lookup = Lookup}, State) ->
    case cloudi_x_keys1value:find(FileName, Lookup) of
        {ok, FormatterConfig} ->
            log_message_formatter(Level, Timestamp, Node, Pid,
                                  FileName, Line, Function, Arity,
                                  MetaData, LogMessage,
                                  FormatterConfig, State);
        error ->
            if
                Default =:= undefined ->
                    format_line(Level, Timestamp, Node, Pid,
                                FileName, Line, Function, Arity,
                                MetaData, LogMessage);
                true ->
                    log_message_formatter(Level, Timestamp, Node, Pid,
                                          FileName, Line, Function, Arity,
                                          MetaData, LogMessage,
                                          Default, State)
            end
    end.

log_message_external(ModeInterface, Process,
                     Level, Module, Line,
                     Function, Arity, Format, Args)
    when is_atom(Module) ->
    % Compatibility with CloudI =< 2.0.4
    FileName = erlang:atom_to_list(Module),
    log_message_external(ModeInterface, Process,
                         Level, FileName, Line,
                         Function, Arity, Format, Args);
log_message_external(ModeInterface, Process,
                     Level, [_ | _] = FileName, Line,
                     Function, Arity, Format, Args)
    when is_atom(Level), is_integer(Line), Line >= 0,
         is_atom(Function),
         (Arity =:= undefined) orelse
         (is_integer(Arity) andalso (Arity >= 0)) ->
    Timestamp = cloudi_timestamp:timestamp(),
    case flooding_logger(Timestamp, Process) of
        {true, _} when ModeInterface =:= async ->
            ok;
        {_, FloodingWarning} ->
            MetaData = metadata_get(),
            LogMessage0 = if
                is_list(Format), Args =:= undefined ->
                    unicode:characters_to_binary(Format);
                true ->
                    log_message_safe(Format, Args)
            end,
            LogMessageN = if
                FloodingWarning =:= undefined; ModeInterface =:= sync ->
                    LogMessage0;
                is_binary(FloodingWarning) ->
                    [LogMessage0, FloodingWarning]
            end,
            if
                ModeInterface =:= async ->
                    gen_server:cast(Process,
                                    {Level, Timestamp, node(), self(),
                                     FileName, Line, Function, Arity,
                                     MetaData, LogMessageN});
                ModeInterface =:= sync ->
                    gen_server:call(Process,
                                    {Level, Timestamp, node(), self(),
                                     FileName, Line, Function, Arity,
                                     MetaData, LogMessageN},
                                    infinity)
            end
    end.

flooding_logger_warning(SecondsRemaining, Delta, Remote) ->
    Location = if
        Remote =:= true ->
            "remotely";
        Remote =:= false ->
            "locally"
    end,
    cloudi_string:format_to_binary("~n"
        "... (~w logged/second async stopped process from~n"
        "     logging ~s for ~.2f seconds)",
        [1000000 div Delta, Location, SecondsRemaining]).

% determine if a single process has sent too many logging messages
flooding_logger(Timestamp1, Process) ->
    case erlang:get(?LOGGER_FLOODING_PDICT_KEY) of
        undefined ->
            erlang:put(?LOGGER_FLOODING_PDICT_KEY,
                       {Timestamp1, 1, false}),
            {false, undefined};
        {Timestamp0, Count0, Flooding} ->
            Count1 = Count0 + 1,
            MicroSecondsElapsed = timer:now_diff(Timestamp1, Timestamp0),
            {Delta, Remote} = if
                is_tuple(Process) ->
                    {?LOGGER_FLOODING_DELTA_REMOTE, true};
                true ->
                    {?LOGGER_FLOODING_DELTA_LOCAL, false}
            end,
            if
                (MicroSecondsElapsed > ?LOGGER_FLOODING_INTERVAL_MAX) orelse
                (MicroSecondsElapsed < 0) ->
                    erlang:put(?LOGGER_FLOODING_PDICT_KEY,
                               {Timestamp1, 1, false}),
                    {false, undefined};
                (Flooding =:= false) andalso
                (MicroSecondsElapsed > ?LOGGER_FLOODING_INTERVAL_MIN) andalso
                (MicroSecondsElapsed div Count1 < Delta) ->
                    erlang:put(?LOGGER_FLOODING_PDICT_KEY,
                               {Timestamp0, Count1, true}),
                    SecondsRemaining = (?LOGGER_FLOODING_INTERVAL_MAX -
                                        MicroSecondsElapsed) / 1000000,
                    {false,
                     flooding_logger_warning(SecondsRemaining, Delta, Remote)};
                true ->
                    erlang:put(?LOGGER_FLOODING_PDICT_KEY,
                               {Timestamp0, Count1, Flooding}),
                    {Flooding, undefined}
            end
    end.

log_message_internal_t0(LevelCheck,
                        Line, Function, Arity, Format, Args, State) ->
    log_message_internal_t0(LevelCheck, undefined,
                            Line, Function, Arity, Format, Args, State).

log_message_internal_t0(off, _, _, _, _, _, _, State) ->
    {ok, State};
log_message_internal_t0(LevelCheck, TimestampOld,
                        Line, Function, Arity, Format, Args,
                        #state{level = Level,
                               destination = Destination,
                               logger_node = Node,
                               logger_self = Self} = State)
    when LevelCheck =:= fatal; LevelCheck =:= error; LevelCheck =:= warn;
         LevelCheck =:= info; LevelCheck =:= debug; LevelCheck =:= trace ->
    case log_level_allowed(Level, LevelCheck) of
        true ->
            LogMessage = log_message(Format, Args),
            Timestamp = if
                TimestampOld =:= undefined ->
                    cloudi_timestamp:timestamp();
                tuple_size(TimestampOld) == 3 ->
                    TimestampOld
            end,
            if
                Destination =:= ?MODULE ->
                    log_message_internal(sync,
                                         LevelCheck, Timestamp, Node, Self,
                                         ?FILE, Line, Function, Arity,
                                         [], LogMessage, State);
                true ->
                    ok = gen_server:cast(Destination,
                                         {LevelCheck, Timestamp, Node, Self,
                                          ?FILE, Line, Function, Arity,
                                          [], LogMessage}),
                    {ok, State}
            end;
        false ->
            {ok, State}
    end.

log_message_internal_t1(LevelCheck, Line, Function, Arity, Format, Args,
                        #state{level = Level,
                               destination = Destination,
                               logger_node = Node,
                               logger_self = Self})
    when LevelCheck =:= fatal; LevelCheck =:= error; LevelCheck =:= warn;
         LevelCheck =:= info; LevelCheck =:= debug; LevelCheck =:= trace ->
    case log_level_allowed(Level, LevelCheck) of
        true ->
            LogMessage = log_message(Format, Args),
            Timestamp = cloudi_timestamp:timestamp(),
            gen_server:cast(Destination,
                            {LevelCheck, Timestamp, Node, Self,
                             ?FILE, Line, Function, Arity,
                             [], LogMessage});
        false ->
            ok
    end.

log_message_internal(async, _, _, _, _, _, _, _, _, _, _,
                     #state{mode = overload} = State) ->
    {ok, State};
log_message_internal(_, Level, Timestamp, Node, Pid,
                     FileName, Line, Function, Arity,
                     MetaData, LogMessage,
                     #state{main_level = MainLevel,
                            stdout = StdoutPort,
                            syslog_level = SyslogLevel,
                            formatters = FormattersConfig,
                            aspects_log_before = AspectsLogBefore,
                            aspects_log_after = AspectsLogAfter} = State) ->
    true = (Level =:= fatal) orelse (Level =:= error) orelse
           (Level =:= warn) orelse (Level =:= info) orelse
           (Level =:= debug) orelse (Level =:= trace),
    Message = log_message_formatters(Level, Timestamp, Node, Pid,
                                     FileName, Line, Function, Arity,
                                     MetaData, LogMessage,
                                     FormattersConfig, State),
    ok = aspects_log(AspectsLogBefore,
                     Level, Timestamp, Node, Pid,
                     FileName, Line, Function, Arity,
                     MetaData, LogMessage),
    {FileResult, StateNew} = case log_level_allowed(MainLevel, Level) of
        true ->
            ok = log_stdout(Message, StdoutPort),
            log_file(Level, Message, State);
        false ->
            {ok, State}
    end,
    case log_level_allowed(SyslogLevel, Level) of
        true ->
            ok = log_syslog(Level, Timestamp, Message, StateNew);
        false ->
            ok
    end,
    ok = aspects_log(AspectsLogAfter,
                     Level, Timestamp, Node, Pid,
                     FileName, Line, Function, Arity,
                     MetaData, LogMessage),
    {FileResult, StateNew}.

-spec log_message_safe(Format :: list(),
                       Args :: list()) ->
    binary().

log_message_safe(Format, Args) ->
    try log_message(Format, Args)
    catch
        error:badarg ->
            cloudi_string:format_to_binary("INVALID LOG INPUT: ~tp ~tp",
                                           [Format, Args])
    end.

-spec log_message(Format :: list(),
                  Args :: list()) ->
    binary().

log_message(Format, Args) ->
    LogMessageUnicode = cloudi_string:format(Format, Args),
    unicode:characters_to_binary(LogMessageUnicode).

log_level([_ | _] = L) ->
    cloudi_core_i_configuration:logging_level_highest([off | L]).

log_file(_, _, #state{file_path = undefined} = State) ->
    {ok, State};
log_file(Level, Message, State) ->
    case log_file_ready(State) of
        {ok, StateNew} ->
            log_file_write(Level, Message, StateNew);
        {error_tracked, StateNew} ->
            {ok, StateNew};
        {{error, _}, _} = Error ->
            Error
    end.

log_file_sync(#state{file_path = undefined} = State) ->
    {ok, State};
log_file_sync(State) ->
    case log_file_ready(State) of
        {ok, #state{file_fd = FileFd,
                    error_sync_count = ErrorSyncCount,
                    error_sync_types = ErrorSyncTypes} = StateNew} = Success ->
            case file:datasync(FileFd) of
                ok ->
                    Success;
                {error, Reason} ->
                    true = is_atom(Reason),
                    ErrorSyncTypesNew = log_error_type(ErrorSyncTypes, Reason),
                    {ok,
                     StateNew#state{error_sync_count = ErrorSyncCount + 1,
                                    error_sync_types = ErrorSyncTypesNew}}
            end;
        {error_tracked, StateNew} ->
            {ok, StateNew};
        {{error, _}, _} = Error ->
            Error
    end.

log_file_ready(#state{file_path = FilePath,
                      file_fd = FileFdOld,
                      file_inode = FileInodeOld,
                      error_read_count = ErrorReadCount,
                      error_read_types = ErrorReadTypes} = State) ->
    case file:read_file_info(FilePath, [raw]) of
        {ok, #file_info{inode = FileInodeNew}} ->
            if
                FileInodeNew == FileInodeOld ->
                    {ok, State};
                true ->
                    case log_file_reopen(FileInodeNew, State) of
                        {ok, _} = Success ->
                            Success;
                        {error, _} = Error ->
                            {Error, State#state{file_fd = undefined,
                                                file_inode = undefined}}
                    end
            end;
        {error, enoent} ->
            _ = file:close(FileFdOld),
            case log_file_open(FilePath, State) of
                {ok, _} = Success ->
                    Success;
                {error, _} ->
                    ErrorReadTypesNew = log_error_type(ErrorReadTypes, enoent),
                    {error_tracked,
                     State#state{file_fd = undefined,
                                 file_inode = undefined,
                                 error_read_count = ErrorReadCount + 1,
                                 error_read_types = ErrorReadTypesNew}}
            end;
        {error, Reason} ->
            _ = file:close(FileFdOld),
            true = is_atom(Reason),
            ErrorReadTypesNew = log_error_type(ErrorReadTypes, Reason),
            {error_tracked,
             State#state{file_fd = undefined,
                         file_inode = undefined,
                         error_read_count = ErrorReadCount + 1,
                         error_read_types = ErrorReadTypesNew}}
    end.

log_file_open(undefined, State) ->
    {ok, State};
log_file_open(FilePath, State) ->
    case file:open(FilePath, [raw, append]) of
        {ok, FileFd} ->
            case file:read_file_info(FilePath, [raw]) of
                {ok, #file_info{inode = FileInode}} ->
                    {ok, State#state{file_path = FilePath,
                                     file_fd = FileFd,
                                     file_inode = FileInode}};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

log_file_reopen(FileInode,
                #state{file_path = FilePath,
                       file_fd = FileFdOld} = State) ->
    _ = file:close(FileFdOld),
    case file:open(FilePath, [raw, append]) of
        {ok, FileFdNew} ->
            {ok, State#state{file_fd = FileFdNew,
                             file_inode = FileInode}};
        {error, _} = Error ->
            Error
    end.

log_file_write(Level, Message,
               #state{file_fd = FileFd,
                      file_sync = FileSync,
                      file_sync_timer = FileSyncTimer,
                      logger_self = Self,
                      file_counts = FileCounts,
                      error_write_count = ErrorWriteCount,
                      error_write_types = ErrorWriteTypes,
                      error_sync_count = ErrorSyncCount,
                      error_sync_types = ErrorSyncTypes} = State) ->
    FileCountsNew = maps:update_with(Level, fun(Count) ->
        Count + 1
    end, 1, FileCounts),
    case file:write(FileFd, Message) of
        ok when FileSync == 0 ->
            case file:datasync(FileFd) of
                ok ->
                    {ok, State#state{file_counts = FileCountsNew}};
                {error, Reason} ->
                    true = is_atom(Reason),
                    ErrorSyncTypesNew = log_error_type(ErrorSyncTypes, Reason),
                    {ok,
                     State#state{file_counts = FileCountsNew,
                                 error_sync_count = ErrorSyncCount + 1,
                                 error_sync_types = ErrorSyncTypesNew}}
            end;
        ok ->
            FileSyncTimerNew = if
                FileSyncTimer =:= undefined ->
                    erlang:send_after(FileSync, Self, file_sync);
                is_reference(FileSyncTimer) ->
                    FileSyncTimer
            end,
            {ok,
             State#state{file_sync_timer = FileSyncTimerNew,
                         file_counts = FileCountsNew}};
        {error, Reason} ->
            true = is_atom(Reason),
            ErrorWriteTypesNew = log_error_type(ErrorWriteTypes, Reason),
            {ok,
             State#state{error_write_count = ErrorWriteCount + 1,
                         error_write_types = ErrorWriteTypesNew}}
    end.

log_error_type([], Reason) ->
    [Reason];
log_error_type([Reason | _] = L, Reason) ->
    L;
log_error_type([ReasonOld | L], Reason) ->
    [ReasonOld | log_error_type(L, Reason)].

log_stdout(_, undefined) ->
    ok;
log_stdout(Message, StdoutPort) when is_port(StdoutPort) ->
    true = erlang:port_command(StdoutPort, Message),
    ok.

log_syslog(Level, Timestamp, Message,
           #state{syslog = Syslog}) ->
    SyslogSeverity = log_level_to_syslog_severity(Level),
    ok = cloudi_x_syslog_socket:send(Syslog, SyslogSeverity,
                                     Timestamp, Message),
    ok.

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
    case ?LOG_T0_INFO("redirecting log output to ~ts",
                      [NodeLogger], State) of
        {ok, StateNext} ->
            case load_interface_module(Level, Mode, DestinationNew) of
                {ok, Binary} ->
                    ?LOG_T1_INFO("redirected log output from ~ts to ~ts",
                                 [node(), NodeLogger], StateNext),
                    {ok, StateNext#state{interface_module = Binary,
                                         destination = DestinationNew}};
                {error, Reason} ->
                    {error, Reason, StateNext}
            end;
        {{error, _}, _} = Error ->
            Error
    end.

log_mode_check(Timestamp,
               #state{level = Level,
                      queue_pending = 0,
                      queue_mode_async = QueueModeAsync,
                      queue_mode_sync = QueueModeSync,
                      queue_mode_overload = QueueModeOverload,
                      mode = ModeOld,
                      destination = Destination,
                      logger_self = Self} = State) ->
    {message_queue_len,
     QueueLength} = erlang:process_info(Self, message_queue_len),
    QueueModeOverloadMin = QueueModeOverload - ?LOGGER_MODE_OVERLOAD_OFFSET,
    ModeNew = if
        ModeOld =:= async,
        QueueLength >= QueueModeSync ->
            sync;
        ModeOld =:= sync ->
            if
                QueueLength =< QueueModeAsync ->
                    async;
                QueueLength >= QueueModeOverload ->
                    overload;
                true ->
                    sync
            end;
        ModeOld =:= overload,
        QueueLength =< QueueModeOverloadMin ->
            sync;
        true ->
            ModeOld
    end,
    QueuePending = if
        ModeNew =:= overload ->
            QueueLength - QueueModeOverloadMin;
        ModeNew =:= sync ->
            if
                QueueLength =< QueueModeOverloadMin div 2 ->
                    QueueLength - QueueModeAsync;
                QueueLength < QueueModeOverloadMin ->
                    QueueModeOverloadMin div 10;
                QueueLength >= QueueModeOverloadMin ->
                    ?LOGGER_MODE_OVERLOAD_OFFSET div 10
            end;
        true ->
            0
    end,
    if
        ModeNew /= ModeOld ->
            case load_interface_module(Level, ModeNew, Destination) of
                {ok, Binary} ->
                    log_mode_changed(ModeNew, ModeOld, Timestamp,
                                     State#state{interface_module = Binary,
                                                 queue_pending = QueuePending,
                                                 mode = ModeNew});
                {error, _} = Error ->
                    Error
            end;
        true ->
            {ok, State#state{queue_pending = QueuePending}}
    end;
log_mode_check(_, #state{queue_pending = QueuePending} = State) ->
    true = QueuePending > 0,
    {ok, State#state{queue_pending = QueuePending - 1}}.

log_mode_changed(sync, async, Timestamp, State) ->
    {ok, State#state{mode_sync_start = cloudi_timestamp:native_monotonic(),
                     mode_sync_start_event = timestamp_iso8601(Timestamp),
                     mode_sync_end = undefined,
                     mode_sync_end_event = undefined,
                     mode_sync_total = undefined}};
log_mode_changed(async, sync, Timestamp,
                 #state{mode_sync_start = SyncStart} = State) ->
    SyncEnd = cloudi_timestamp:native_monotonic(),
    SyncEndEvent = timestamp_iso8601(Timestamp),
    SyncNanoSeconds = cloudi_timestamp:
                      convert(SyncEnd - SyncStart,
                              native, nanosecond),
    SyncTotal = cloudi_timestamp:
                nanoseconds_to_string(SyncNanoSeconds),
    {ok, State#state{mode_sync_end = SyncEnd,
                     mode_sync_end_event = SyncEndEvent,
                     mode_sync_total = SyncTotal}};
log_mode_changed(overload, sync, Timestamp, State) ->
    {ok, State#state{mode_overload_start = cloudi_timestamp:native_monotonic(),
                     mode_overload_start_event = timestamp_iso8601(Timestamp),
                     mode_overload_end = undefined,
                     mode_overload_end_event = undefined,
                     mode_overload_total = undefined}};
log_mode_changed(sync, overload, Timestamp,
                 #state{mode_overload_start = OverloadStart} = State) ->
    OverloadEnd = cloudi_timestamp:native_monotonic(),
    OverloadEndEvent = timestamp_iso8601(Timestamp),
    OverloadNanoSeconds = cloudi_timestamp:
                          convert(OverloadEnd - OverloadStart,
                                  native, nanosecond),
    OverloadTotal = cloudi_timestamp:
                    nanoseconds_to_string(OverloadNanoSeconds),
    StateNew = State#state{mode_overload_end = OverloadEnd,
                           mode_overload_end_event = OverloadEndEvent,
                           mode_overload_total = OverloadTotal},
    ?LOG_T0_ERROR("logging overload occurred for ~s",
                  [OverloadTotal], StateNew);
log_mode_changed(_, _, _, State) ->
    {ok, State}.

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

log_level_to_syslog_severity(fatal) ->
    critical;
log_level_to_syslog_severity(error) ->
    error;
log_level_to_syslog_severity(warn) ->
    warning;
log_level_to_syslog_severity(info) ->
    notice;
log_level_to_syslog_severity(debug) ->
    informational;
log_level_to_syslog_severity(trace) ->
    debug.

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

interface(Level, overload, Destination) ->
    cloudi_string:format(?INTERFACE_MODULE_OVERLOAD_CODE(Level),
                         ?INTERFACE_MODULE_OVERLOAD_ARGS(Level,
                                                         Destination));
interface(Level, ModeInterface, Destination)
    when ModeInterface =:= async; ModeInterface =:= sync ->
    cloudi_string:format(?INTERFACE_MODULE_NORMAL_CODE(Level),
                         ?INTERFACE_MODULE_NORMAL_ARGS(Level,
                                                       ModeInterface,
                                                       Destination)).

load_interface_module(undefined, _, _) ->
    {error, logging_level_undefined};
load_interface_module(Level, Mode, Destination) when is_atom(Level) ->
    ModuleText = interface(Level, Mode, Destination),
    {ok, Module, Binary} = merl:compile(merl:quote(ModuleText)),
    cloudi_core_i_logger_interface = Module,
    % ensure no old code exists
    _ = code:purge(Module),
    % load the new current code
    case code:load_binary(Module,
                          erlang:atom_to_list(Module) ++ ".erl",
                          Binary) of
        {module, Module} ->
            % remove the old code
            _ = code:soft_purge(Module),
            {ok, Binary};
        {error, _} = Error ->
            Error
    end.

stdout_open(false) ->
    undefined;
stdout_open(true) ->
    erlang:open_port({fd, 0, 1}, [out, stream]).

stdout_close(undefined) ->
    ok;
stdout_close(StdoutPort) when is_port(StdoutPort) ->
    _ = (catch erlang:port_close(StdoutPort)),
    ok.

syslog_open(undefined, _, State) ->
    {ok, State#state{syslog = undefined,
                     syslog_level = undefined}};
syslog_open(#config_logging_syslog{identity = SyslogIdentity,
                                   facility = SyslogFacility,
                                   level = SyslogLevel,
                                   transport = SyslogTransport,
                                   transport_options = SyslogTransportOptions,
                                   protocol = SyslogProtocol,
                                   path = SyslogPath,
                                   host = SyslogHost,
                                   port = SyslogPort},
            LogError, State) ->
    Options = [{app_name, SyslogIdentity},
               {facility, SyslogFacility},
               {transport, SyslogTransport},
               {transport_options, SyslogTransportOptions},
               {protocol, SyslogProtocol},
               {utf8, true},
               {path, SyslogPath},
               {host, SyslogHost},
               {port, SyslogPort},
               {timeout, 5000}],
    case cloudi_x_syslog_socket:start_monitor(Options) of
        {ok, Syslog} ->
            {ok, State#state{syslog = Syslog,
                             syslog_level = SyslogLevel}};
        {error, Reason} = Error ->
            if
                LogError =:= true ->
                    ?LOG_T0_ERROR("syslog error: ~tp", [Reason],
                                  State#state{syslog = undefined,
                                              syslog_level = undefined});
                LogError =:= false ->
                    {Error, State}
            end
    end.

syslog_close(undefined) ->
    ok;
syslog_close(Syslog) when is_pid(Syslog) ->
    cloudi_x_syslog_socket:stop_monitor(Syslog). % asynchronous stop

filepath_exists(FilePath) ->
    case file:open(FilePath, [raw, append]) of
        {ok, Fd} ->
            file:close(Fd);
        {error, _} = Error ->
            Error
    end.

time_offset_nanoseconds() ->
    erlang:time_offset(nanosecond).
time_offset_to_nanoseconds(TimeOffset) ->
    erlang:convert_time_unit(TimeOffset, native, nanosecond).

aspects_log([], _, _, _, _, _, _, _, _, _, _) ->
    ok;
aspects_log([{M, F} | L], Level, Timestamp, Node, Pid,
            FileName, Line, Function, Arity, MetaData, LogMessage) ->
    try M:F(Level, Timestamp, Node, Pid,
            FileName, Line, Function, Arity, MetaData, LogMessage) of
        _ ->
            aspects_log(L, Level, Timestamp, Node, Pid,
                        FileName, Line, Function, Arity, MetaData, LogMessage)
    catch
        _:_ ->
            aspects_log(L, Level, Timestamp, Node, Pid,
                        FileName, Line, Function, Arity, MetaData, LogMessage)
    end;
aspects_log([F | L], Level, Timestamp, Node, Pid,
            FileName, Line, Function, Arity, MetaData, LogMessage) ->
    try F(Level, Timestamp, Node, Pid,
          FileName, Line, Function, Arity, MetaData, LogMessage) of
        _ ->
            aspects_log(L, Level, Timestamp, Node, Pid,
                        FileName, Line, Function, Arity, MetaData, LogMessage)
    catch
        _:_ ->
            aspects_log(L, Level, Timestamp, Node, Pid,
                        FileName, Line, Function, Arity, MetaData, LogMessage)
    end.

nanoseconds_to_seconds_change_string(NanoSecondsOld, NanoSecondsNew) ->
    {Sign, NanoSecondsChange} = if
        NanoSecondsNew >= NanoSecondsOld ->
            {$+, NanoSecondsNew - NanoSecondsOld};
        true ->
            {$-, NanoSecondsOld - NanoSecondsNew}
    end,
    Str = int_to_dec_list(NanoSecondsChange, 9, $0),
    [NanoSeconds8, NanoSeconds7, NanoSeconds6,
     NanoSeconds5, NanoSeconds4, NanoSeconds3,
     NanoSeconds2, NanoSeconds1, NanoSeconds0 | Seconds] = lists:reverse(Str),
    SecondsFractionStr = [$.,
                          NanoSeconds0, NanoSeconds1, NanoSeconds2,
                          NanoSeconds3, NanoSeconds4, NanoSeconds5,
                          NanoSeconds6, NanoSeconds7, NanoSeconds8, $ ,
                          $s, $e, $c, $o, $n, $d, $s],
    if
        Seconds == [] ->
            [Sign, $0 | SecondsFractionStr];
        true ->
            [Sign | lists:reverse(Seconds, SecondsFractionStr)]
    end.

int_to_dec_list(I) when is_integer(I), I >= 0 ->
    int_to_dec_list([], I).

int_to_dec_list(L, I)
    when I < 10 ->
    [int_to_dec(I) | L];
int_to_dec_list(L, I) ->
    int_to_dec_list([int_to_dec(I rem 10) | L], I div 10).

int_to_dec_list(I, N, Char) when is_integer(I), I >= 0 ->
    int_to_dec_list([], I, 1, N, Char).

int_to_dec_list(L, I, Count, N, Char)
    when I < 10 ->
    int_to_list_pad([int_to_dec(I) | L], N - Count, Char);
int_to_dec_list(L, I, Count, N, Char) ->
    int_to_dec_list([int_to_dec(I rem 10) | L], I div 10, Count + 1, N, Char).

int_to_list_pad(L, Count, _) when Count =< 0 ->
    L;
int_to_list_pad(L, Count, Char) ->
    int_to_list_pad([Char | L], Count - 1, Char).

int_to_dec(I) when 0 =< I, I =< 9 ->
    I + $0.

-spec accum(L :: nonempty_list({any(),
                                fun((any(), #state{}) ->
                                    {ok, #state{}} |
                                    {{error, any()}, #state{}})}),
            State :: #state{}) ->
    {ok, #state{}} | {{error, any()}, #state{}}.

accum([], State) ->
    {ok, State};
accum([{Value, F} | L], State) ->
    case F(Value, State) of
        {ok, StateNew} ->
            accum(L, StateNew);
        {{error, _}, _} = Error ->
            Error
    end.

%%%------------------------------------------------------------------------
%%% lager integration based on lager source code
%%% (lager is under the Apache version 2.0 license and
%%%  was developed by Basho Technologies)
%%%------------------------------------------------------------------------

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

% from lager_util:i2l/1
lager_i2l(I) when I < 10 -> [$0, $0 + I];
lager_i2l(I) -> erlang:integer_to_list(I).
% from lager_util:i3l/1
lager_i3l(I) when I < 100 -> [$0 | lager_i2l(I)];
lager_i3l(I) -> erlang:integer_to_list(I).

% based on lager_util:maybe_utc/1 and lager_util:format_time/1
lager_datetime_format({_, _, MicroSeconds} = Timestamp) ->
    TimeMS = MicroSeconds div 1000 rem 1000,
    {{DateYY,
      DateMM,
      DateDD},
     {TimeHH,
      TimeMM,
      TimeSS}} = calendar:now_to_universal_time(Timestamp),
    {[erlang:integer_to_list(DateYY), $-,
      lager_i2l(DateMM), $-, lager_i2l(DateDD)],
     [lager_i2l(TimeHH), $:, lager_i2l(TimeMM), $:,
      lager_i2l(TimeSS), $., lager_i3l(TimeMS), $ , $U, $T, $C]}.

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
        metadata :: list({any(), any()}),
        severity :: debug | emergency | error | info | warning,
        datetime :: {iolist(), iolist()},
        timestamp :: erlang:timestamp(),
        message :: list()
    }).

-spec lager_msg(Level :: debug | error | fatal | info | trace | warn,
                Timestamp :: erlang:timestamp(),
                Node :: node(),
                Pid :: pid(),
                FileName :: nonempty_string(),
                Line :: non_neg_integer(),
                Function :: atom(),
                Arity :: non_neg_integer() | undefined,
                MetaData :: list({atom(), any()}) | #{},
                LogMessage :: iolist()) ->
    #lager_msg{}.

% based on lager_msg:new/5
lager_msg(Level, Timestamp, Node, Pid,
          FileName, Line, Function, _Arity,
          MetaData0, LogMessage) ->
    Destinations = [], % not using TraceFilters
    MetaData1 = if
        is_list(MetaData0) ->
            MetaData0;
        is_map(MetaData0) ->
            maps:to_list(MetaData0)
    end,
    MetaData2 = if
        Function =:= undefined ->
            MetaData1;
        true ->
            [{function, Function} | MetaData1]
    end,
    Module = case cloudi_string:splitr($., FileName, input) of
        {[], _} ->
            undefined;
        {ModuleStr, _} ->
            try erlang:list_to_existing_atom(ModuleStr)
            catch
                error:badarg ->
                    undefined
            end
    end,
    MetaDataN = [{module, Module},
                 {file, FileName},
                 {line, Line},
                 {node, Node},
                 {pid, erlang:pid_to_list(Pid)} | MetaData2],
    Severity = lager_severity_output(Level),
    DateTime = lager_datetime_format(Timestamp),
    Message = if
        is_list(LogMessage) ->
            LogMessage;
        is_binary(LogMessage) ->
            [LogMessage]
    end,
    % create lager_msg record manually
    {lager_msg,
     Destinations,
     MetaDataN,
     Severity,
     DateTime,
     Timestamp,
     Message}.

