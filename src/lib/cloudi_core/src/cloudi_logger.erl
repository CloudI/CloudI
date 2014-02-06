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
%%% @version 1.3.2 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_logger).
-author('mjtruog [at] gmail (dot) com').

-behaviour(gen_server).

%% external interface
-export([start_link/1,
         current_function/0,
         change_loglevel/1,
         redirect/1,
         fatal/6, error/6, warn/6, info/6, debug/6, trace/6]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("cloudi_configuration.hrl").
-include("cloudi_constants.hrl").
-include_lib("kernel/include/file.hrl").

%% logging macros used only within this module
-define(LOG_INFO_T0(Format, Args, State),
    log_message_internal_t0(info, ?LINE, Format, Args, State)).
-define(LOG_INFO_T1(Format, Args, State),
    log_message_internal_t1(info, ?LINE, Format, Args, State)).

-record(state,
    {
        file_path = undefined :: undefined | string(),
        file_level = undefined :: undefined | cloudi_service_api:loglevel(),
        interface_module :: binary(),
        fd = undefined :: undefined | file:fd(),
        inode = undefined :: undefined | non_neg_integer(),
        level :: cloudi_service_api:loglevel(),
        mode = async :: async | sync,
        destination :: ?MODULE | {?MODULE, node()},
        syslog = undefined :: undefined | port(),
        syslog_level = undefined :: undefined | cloudi_service_api:loglevel()
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
%% ===Change the file output log level.===
%% @end
%%-------------------------------------------------------------------------

-spec change_loglevel(Level :: atom()) ->
    'ok'.

change_loglevel(Level)
    when Level =:= fatal; Level =:= error; Level =:= warn;
         Level =:= info; Level =:= debug; Level =:= trace;
         Level =:= off ->
    gen_server:cast(?MODULE, {change_loglevel, Level}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Redirect this node's logging to a different node.===
%% @end
%%-------------------------------------------------------------------------

-spec redirect(Node :: atom()) ->
    'ok'.

redirect(Node) ->
    gen_server:cast(?MODULE, {redirect, Node}).

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

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([#config_logging{file = FilePath,
                      level = FileLevel,
                      redirect = NodeLogger,
                      syslog = SyslogConfig}]) ->
    #state{mode = Mode} = State = #state{},
    Destination = if
        NodeLogger == node(); NodeLogger =:= undefined ->
            ?MODULE;
        true ->
            {?MODULE, NodeLogger}
    end,
    #state{level = Level} = NextState = case SyslogConfig of
        undefined ->
            State#state{file_level = FileLevel,
                        level = FileLevel};
        #config_logging_syslog{identity = SyslogIdentity,
                               facility = SyslogFacility,
                               level = SyslogLevel} ->
            syslog:load(),
            {ok, Syslog} = syslog:open(SyslogIdentity,
                                       [ndelay, pid],
                                       SyslogFacility),
            State#state{file_level = FileLevel,
                        level = log_level_highest([FileLevel, SyslogLevel]),
                        syslog = Syslog,
                        syslog_level = SyslogLevel}
    end,
    case load_interface_module(Level, Mode, Destination) of
        {ok, Binary} when Destination == ?MODULE ->
            case log_file_open(FilePath,
                               NextState#state{interface_module = Binary,
                                               destination = Destination}) of
                {ok, _} = Success ->
                    Success;
                {error, Reason} ->
                    {stop, Reason}
            end;
        {ok, Binary} ->
            case ?LOG_INFO_T0("redirecting log output to ~p",
                              [NodeLogger],
                              NextState#state{file_path = FilePath,
                                              interface_module = Binary,
                                              destination = ?MODULE}) of
                {ok, NewState} ->
                    {ok, NewState#state{destination = Destination}};
                {{error, Reason}, _} ->
                    {stop, Reason}
            end;
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call({Level, Now, Node, Pid,
             Module, Line, LogMessage}, _, State) ->
    case log_message_internal(Level, Now, Node, Pid,
                              Module, Line, LogMessage, State) of
        {ok, NextState} ->
            case log_mode_check(NextState) of
                {ok, NewState} ->
                    {reply, ok, NewState};
                {error, Reason} ->
                    {stop, Reason, NextState}
            end;
        {{error, Reason}, NextState} ->
            {stop, Reason, NextState}
    end;
handle_call(Request, _, State) ->
    {stop, cloudi_string:format("Unknown call \"~p\"~n", [Request]),
     error, State}.

handle_cast({change_loglevel, _},
            #state{file_path = undefined} = State) ->
    {noreply, State};
handle_cast({change_loglevel, FileLevel},
            #state{file_level = FileLevel} = State) ->
    {noreply, State};
handle_cast({change_loglevel, FileLevelNew},
            #state{file_level = FileLevelOld,
                   level = LevelOld,
                   mode = Mode,
                   destination = Destination,
                   syslog_level = SyslogLevel} = State) ->
    case ?LOG_INFO_T0("changing file loglevel from ~p to ~p",
                      [FileLevelOld, FileLevelNew], State) of
        {ok, NextState} ->
            case log_level_highest([FileLevelNew, SyslogLevel]) of
                LevelOld ->
                    {noreply, NextState#state{file_level = FileLevelNew}};
                LevelNew ->
                    case load_interface_module(LevelNew, Mode, Destination) of
                        {ok, Binary} ->
                            NewState = NextState#state{
                                file_level = FileLevelNew,
                                interface_module = Binary,
                                level = LevelNew},
                            ?LOG_INFO_T1("changed loglevel from ~p to ~p",
                                         [LevelOld, LevelNew], NewState),
                            {noreply, NewState};
                        {error, Reason} ->
                            {stop, Reason, NextState}
                    end
            end;
        {{error, Reason}, NextState} ->
            {stop, Reason, NextState}
    end;
handle_cast({redirect, Node}, State) ->
    Destination = if
        Node == node(); Node =:= undefined ->
            ?MODULE;
        true ->
            {?MODULE, Node}
    end,
    case log_redirect(Node, Destination, State) of
        {ok, NewState} ->
            {noreply, NewState};
        {error, Reason, NewState} ->
            {stop, Reason, NewState}
    end;
handle_cast({Level, Now, Node, Pid,
             Module, Line, LogMessage}, State) ->
    case log_message_internal(Level, Now, Node, Pid,
                              Module, Line, LogMessage, State) of
        {ok, NextState} ->
            case log_mode_check(NextState) of
                {ok, NewState} ->
                    {noreply, NewState};
                {error, Reason} ->
                    {stop, Reason, NextState}
            end;
        {{error, Reason}, NextState} ->
            {stop, Reason, NextState}
    end;
handle_cast(Request, State) ->
    {stop, cloudi_string:format("Unknown cast \"~p\"~n", [Request]), State}.

handle_info(Request, State) ->
    {stop, cloudi_string:format("Unknown info \"~p\"~n", [Request]), State}.

terminate(_, #state{fd = Fd,
                    syslog = Syslog}) ->
    file:close(Fd),
    (catch syslog:close(Syslog)),
    syslog:unload(),
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

log_message_external(Mode, Process, Level, Module, Line, Format, Args)
    when is_atom(Level), is_atom(Module), is_integer(Line) ->
    Now = erlang:now(),
    case flooding_logger(Now) of
        true ->
            ok;
        false ->
            LogMessage = try cloudi_string:format(Format, Args)
            catch
                error:badarg ->
                    cloudi_string:format("INVALID LOG INPUT: ~p ~p",
                                         [Format, Args])
            end,
            if
                Mode =:= async ->
                    gen_server:cast(Process,
                                    {Level, Now, node(), self(),
                                     Module, Line, LogMessage});
                Mode =:= sync ->
                    gen_server:call(Process,
                                    {Level, Now, node(), self(),
                                     Module, Line, LogMessage}, infinity)
            end
    end.

%% every 10 seconds, determine if the process has sent too many logging messages
flooding_logger(Now2) ->
    case erlang:get(cloudi_logger) of
        undefined ->
            erlang:put(cloudi_logger, {Now2, 1}),
            false;
        {Now1, Count1} ->
            Count2 = Count1 + 1,
            MicroSecondsElapsed = timer:now_diff(Now2, Now1),
            if
                MicroSecondsElapsed > 10000000 ->
                    erlang:put(cloudi_logger, {Now2, 1}),
                    false;
                (MicroSecondsElapsed / Count2) < ?LOGGER_FLOODING_DELTA ->
                    erlang:put(cloudi_logger, {Now1, Count2}),
                    true;
                true ->
                    erlang:put(cloudi_logger, {Now1, Count2}),
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
                                 ?MODULE, Line, LogMessage, State);
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
                             ?MODULE, Line, LogMessage});
        false ->
            ok
    end.

log_message_internal(Level, {_, _, MicroSeconds} = Now, Node, Pid,
                     Module, Line, LogMessage,
                     #state{file_level = FileLevel,
                            syslog_level = SyslogLevel} = State0)
    when Level =:= fatal; Level =:= error; Level =:= warn;
         Level =:= info; Level =:= debug; Level =:= trace ->
    Description = [io_lib:format(" ~s~n", [S]) ||
                   S <- string:tokens(LogMessage, "\n")],
    {{DateYYYY, DateMM, DateDD},
     {TimeHH, TimeMM, TimeSS}} = calendar:now_to_universal_time(Now),
    % ISO 8601 for date/time http://www.w3.org/TR/NOTE-datetime
    Message = cloudi_string:format("~4..0w-~2..0w-~2..0wT"
                                   "~2..0w:~2..0w:~2..0w.~6..0wZ ~s "
                                   "(~p:~p:~p:~p)~n~s",
                                   [DateYYYY, DateMM, DateDD,
                                    TimeHH, TimeMM, TimeSS, MicroSeconds,
                                    log_level_to_string(Level),
                                    Module, Line, Pid, Node, Description]),
    {FileResult, State1} = case log_level_allowed(FileLevel, Level) of
        true ->
            log_file(Message, State0);
        false ->
            {ok, State0}
    end,
    {SyslogResult, State2} = case log_level_allowed(SyslogLevel, Level) of
        true ->
            log_syslog(Level, Message, State1);
        false ->
            {ok, State1}
    end,
    log_message_internal_result(FileResult, SyslogResult, State2).

log_message_internal_result(ok, ok, State) ->
    {ok, State};
log_message_internal_result({error, _} = Error, _, State) ->
    {Error, State}.

log_file(Message,
         #state{file_path = FilePath,
                fd = OldFd,
                inode = OldInode} = State) ->
    case file:read_file_info(FilePath) of
        {ok, #file_info{inode = CurrentInode}} ->
            if
                CurrentInode == OldInode ->
                    file:write(OldFd, Message),
                    file:datasync(OldFd),
                    {ok, State};
                true ->
                    file:close(OldFd),
                    case log_file_reopen(FilePath,
                                         CurrentInode, State) of
                        {ok, #state{fd = NewFd} = NewState} ->
                            file:write(NewFd, Message),
                            file:datasync(NewFd),
                            {ok, NewState};
                        {error, _} = Error ->
                            {Error, State#state{fd = undefined}}
                    end
            end;
        {error, enoent} ->
            file:close(OldFd),
            case log_file_open(FilePath, State) of
                {ok, #state{fd = NewFd} = NewState} ->
                    file:write(NewFd, Message),
                    file:datasync(NewFd),
                    {ok, NewState};
                {error, _} = Error ->
                    {Error, State#state{fd = undefined}}
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
log_redirect(Node, NewDestination,
             #state{level = Level,
                    mode = Mode} = State) ->
    NodeLogger = if
        NewDestination =:= ?MODULE ->
            node();
        true ->
            Node
    end,
    case ?LOG_INFO_T0("redirecting log output to ~p",
                      [NodeLogger], State) of
        {ok, NextState} ->
            case load_interface_module(Level, Mode, NewDestination) of
                {ok, Binary} ->
                    ?LOG_INFO_T1("redirected log output from ~p to ~p",
                                 [node(), NodeLogger], NextState),
                    {ok, NextState#state{interface_module = Binary,
                                         destination = NewDestination}};
                {error, Reason} ->
                    {error, Reason, NextState}
            end;
        {{error, _}, _} = Error ->
            Error
    end.

log_mode_check(#state{level = Level,
                      mode = Mode,
                      destination = Destination} = State) ->
    {message_queue_len,
     MessageQueueLength} = erlang:process_info(self(), message_queue_len),
    NewMode = if
        Mode =:= async,
        MessageQueueLength >= ?LOGGER_MSG_QUEUE_SYNC ->
            sync;
        Mode =:= sync,
        MessageQueueLength =< ?LOGGER_MSG_QUEUE_ASYNC ->
            async;
        true ->
            Mode
    end,
    if
        NewMode /= Mode ->
            case load_interface_module(Level, NewMode, Destination) of
                {ok, Binary} ->
                    {ok, State#state{interface_module = Binary,
                                     mode = NewMode}};
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

log_level_highest([_ | _] = Levels) ->
    [Level | _] = lists:dropwhile(fun(Highest) ->
        not lists:member(Highest, Levels)
    end, [trace, debug, info, warn, error, fatal, off, undefined]),
    Level.

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

-define(INTERFACE_MODULE_HEADER,
    "
    -module(cloudi_logger_interface).
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
        cloudi_logger:fatal(~p, ~p, Module, Line, Format, Arguments).
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
        cloudi_logger:fatal(~p, ~p, Module, Line, Format, Arguments).
    error(Module, Line, Format, Arguments) ->
        cloudi_logger:error(~p, ~p, Module, Line, Format, Arguments).
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
        cloudi_logger:fatal(~p, ~p, Module, Line, Format, Arguments).
    error(Module, Line, Format, Arguments) ->
        cloudi_logger:error(~p, ~p, Module, Line, Format, Arguments).
    warn(Module, Line, Format, Arguments) ->
        cloudi_logger:warn(~p, ~p, Module, Line, Format, Arguments).
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
        cloudi_logger:fatal(~p, ~p, Module, Line, Format, Arguments).
    error(Module, Line, Format, Arguments) ->
        cloudi_logger:error(~p, ~p, Module, Line, Format, Arguments).
    warn(Module, Line, Format, Arguments) ->
        cloudi_logger:warn(~p, ~p, Module, Line, Format, Arguments).
    info(Module, Line, Format, Arguments) ->
        cloudi_logger:info(~p, ~p, Module, Line, Format, Arguments).
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
        cloudi_logger:fatal(~p, ~p, Module, Line, Format, Arguments).
    error(Module, Line, Format, Arguments) ->
        cloudi_logger:error(~p, ~p, Module, Line, Format, Arguments).
    warn(Module, Line, Format, Arguments) ->
        cloudi_logger:warn(~p, ~p, Module, Line, Format, Arguments).
    info(Module, Line, Format, Arguments) ->
        cloudi_logger:info(~p, ~p, Module, Line, Format, Arguments).
    debug(Module, Line, Format, Arguments) ->
        cloudi_logger:debug(~p, ~p, Module, Line, Format, Arguments).
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
        cloudi_logger:fatal(~p, ~p, Module, Line, Format, Arguments).
    error(Module, Line, Format, Arguments) ->
        cloudi_logger:error(~p, ~p, Module, Line, Format, Arguments).
    warn(Module, Line, Format, Arguments) ->
        cloudi_logger:warn(~p, ~p, Module, Line, Format, Arguments).
    info(Module, Line, Format, Arguments) ->
        cloudi_logger:info(~p, ~p, Module, Line, Format, Arguments).
    debug(Module, Line, Format, Arguments) ->
        cloudi_logger:debug(~p, ~p, Module, Line, Format, Arguments).
    trace(Module, Line, Format, Arguments) ->
        cloudi_logger:trace(~p, ~p, Module, Line, Format, Arguments).
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
    code:purge(cloudi_logger_interface),
    % load the new current code
    case code:load_binary(Module, "cloudi_logger_interface.erl", Binary) of
        {module, Module} ->
            % remove the old code
            code:soft_purge(cloudi_logger_interface),
            {ok, Binary};
        {error, _} = Error ->
            Error
    end.

