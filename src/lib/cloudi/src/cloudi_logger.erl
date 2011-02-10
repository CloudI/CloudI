%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Cloudi Logger==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2009-2011, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2009-2011 Michael Truog
%%% @version 0.1.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_logger).
-author('mjtruog [at] gmail (dot) com').

-behaviour(gen_server).

%% external interface
-export([start_link/1,
         load_interface_module/1, flush/0, reopen/0,
         critical/5, error/5, warning/5, info/5, debug/5]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("cloudi_configuration.hrl").

-define(LOGLEVEL_FLUSH_THRESHOLD, 2).   % critical/error is flushed immediately
-define(LOG_DATA_FLUSH_TIMEOUT, 60000). % 1 minute

-record(state_last_log_entry,
    {
        key = "",
        message = "",
        count = 1
    }).

-record(state,
    {
        loglevel_names = {
            "CRITICAL",
            "ERROR",
            "WARNING",
            "INFO",
            "DEBUG"
        },
        filename = "",
        interface_module = undefined,
        fd = undefined,
        flush_timer = undefined,
        last_log_entry = erlang:make_tuple(5, #state_last_log_entry{})
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
%% ===Load the binary for the logger interface module.===
%% @end
%%-------------------------------------------------------------------------

load_interface_module(Node) when is_atom(Node) ->
    Binary = gen_server:call(?MODULE, interface_module),
    case rpc:call(Node, code, load_binary,
                  [cloudi_logger_interface,
                   "cloudi_logger_interface.erl", Binary]) of
        {badrpc, Reason} ->
            {error, Reason};
        {error, _} = Error ->
            Error;
        {module, cloud_logger_interface} ->
            ok
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Flush all logging data to the log file.===
%% @end
%%-------------------------------------------------------------------------

-spec flush() -> 'ok'.

flush() ->
    gen_server:call(?MODULE, flush).

%%-------------------------------------------------------------------------
%% @doc
%% ===Reopen the log files so that they can be rotated elsewhere.===
%% @end
%%-------------------------------------------------------------------------

-spec reopen() -> 'ok'.

reopen() ->
    gen_server:cast(?MODULE, reopen).

%%-------------------------------------------------------------------------
%% @doc
%% ===Critical log message,===
%% which indicates the system has failed and can not continue.
%% Called with ?LOG_CRITICAL(Format, []).
%% @end
%%-------------------------------------------------------------------------

-spec critical(Process :: atom() | {atom(), atom()},
               Module :: atom(),
               Line :: integer(),
               Format :: string(),
               Args :: list()) ->
    'ok'.

critical(Process, Module, Line, [_|_] = Format, Args)
    when is_atom(Module), is_integer(Line), is_list(Args) ->
    log_message(Process, 1, Module, Line, Format, Args).

%%-------------------------------------------------------------------------
%% @doc
%% ===Error log message,===
%% which indicates a subsystem has failed but the failure is not critical.
%% Called with ?LOG_ERROR(Format, []).
%% @end
%%-------------------------------------------------------------------------

-spec error(Process :: atom() | {atom(), atom()},
            Module :: atom(),
            Line :: integer(),
            Format :: string(),
            Args :: list()) ->
    'ok'.

error(Process, Module, Line, [_|_] = Format, Args)
    when is_atom(Module), is_integer(Line), is_list(Args) ->
    log_message(Process, 2, Module, Line, Format, Args).

%%-------------------------------------------------------------------------
%% @doc
%% ===Warning log message,===
%% which indicates an unexpected occurance was found in a subsystem.
%% Called with ?LOG_WARNING(Format, []).
%% @end
%%-------------------------------------------------------------------------

-spec warning(Process :: atom() | {atom(), atom()},
              Module :: atom(),
              Line :: integer(),
              Format :: string(),
              Args :: list(any())) ->
    'ok'.

warning(Process, Module, Line, [_|_] = Format, Args)
    when is_atom(Module), is_integer(Line), is_list(Args) ->
    log_message(Process, 3, Module, Line, Format, Args).

%%-------------------------------------------------------------------------
%% @doc
%% ===Info log message,===
%% which indicates a subsystem has changed state.
%% Called with ?LOG_INFO(Format, []).
%% @end
%%-------------------------------------------------------------------------

-spec info(Process :: atom() | {atom(), atom()},
           Module :: atom(),
           Line :: integer(),
           Format :: string(),
           Args :: list(any())) ->
    'ok'.

info(Process, Module, Line, [_|_] = Format, Args)
    when is_atom(Module), is_integer(Line), is_list(Args) ->
    log_message(Process, 4, Module, Line, Format, Args).

%%-------------------------------------------------------------------------
%% @doc
%% ===Debug log message,===
%% which reports subsystem data that should be useful for debugging.
%% Called with ?LOG_DEBUG(Format, []).
%% @end
%%-------------------------------------------------------------------------

-spec debug(Process :: atom() | {atom(), atom()},
            Module :: atom(),
            Line :: integer(),
            Format :: string(),
            Args :: list(any())) ->
    'ok'.

debug(Process, Module, Line, [_|_] = Format, Args)
    when is_atom(Module), is_integer(Line), is_list(Args) ->
    log_message(Process, 5, Module, Line, Format, Args).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([#config_logging{loglevel = Level,
                      filename = FileName}]) ->
    case file:open(FileName, [append, raw]) of
        {ok, Fd} ->
            case create_interface_module(get_loglevel_integer(Level)) of
                {ok, Binary} ->
                    FlushTimer = erlang:send_after(?LOG_DATA_FLUSH_TIMEOUT,
                                                   self(), flush),
                    {ok, #state{filename = FileName,
                                interface_module = Binary,
                                fd = Fd,
                                flush_timer = FlushTimer}};
                {error, Reason} ->
                    {stop, Reason}
            end;
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call(interface_module, _, #state{interface_module = Binary} = State) ->
    {reply, Binary, State, hibernate};

handle_call(flush, _, State) ->
    {reply, ok, output_queued(State), hibernate};

handle_call(Request, _, State) ->
    {stop, string2:format("Unknown call \"~p\"~n", [Request]), error, State}.

handle_cast(reopen, #state{filename = FileName, fd = Fd} = State) ->
    file:close(Fd),
    case file:rename(FileName, filename:rootname(FileName) ++ "-old.log") of
        ok ->
            case reopen_sasl() of
                ok ->
                    case file:open(FileName, [append, raw]) of
                        {ok, NewFd} ->
                            {noreply, State#state{fd = NewFd}};
                        {error, Reason} ->
                            {stop, Reason, State#state{fd = undefined}}
                    end;
                {error, Reason} ->
                    {stop, Reason, State#state{fd = undefined}}
            end;
        {error, Reason} ->
            {stop, Reason, State#state{fd = undefined}}
    end;

handle_cast({log, Level, {_, _, MicroSeconds} = Now,
             Module, Line, Pid, Format, Args},
            #state{loglevel_names = LogLevelNames} = State) ->
    Description = io_lib:format(Format, Args),
    {{DateYYYY, DateMM, DateDD},
     {TimeHH, TimeMM, TimeSS}} = calendar:now_to_universal_time(Now),
    % ISO 8601 for date/time http://www.w3.org/TR/NOTE-datetime
    Message = string2:format("~4..0w-~2..0w-~2..0wT"
                             "~2..0w:~2..0w:~2..0w.~6..0wZ ~s "
                             "~~s (~p:~p:~p) ~s~n",
                             [DateYYYY, DateMM, DateDD,
                              TimeHH, TimeMM, TimeSS, MicroSeconds,
                              erlang:element(Level, LogLevelNames),
                              Module, Line, Pid, Description]),
    % used to determine duplicate entries
    Key = io_lib:format("~p,~p,~p", [Module, Line, Description]),
    {noreply, output(Key, Message, Level, State)};

handle_cast(Request, State) ->
    {stop, string2:format("Unknown cast \"~p\"~n", [Request]), State}.

handle_info(flush, State) ->
    NewState = output_queued(State),
    FlushTimer = erlang:send_after(?LOG_DATA_FLUSH_TIMEOUT, self(), flush),
    {noreply, NewState#state{flush_timer = FlushTimer}, hibernate};

handle_info(Request, State) ->
    {stop, string2:format("Unknown info \"~p\"~n", [Request]), State}.

terminate(_, #state{fd = Fd, flush_timer = FlushTimer} = State) ->
    output_queued(State),
    if
        Fd /= undefined ->
            file:close(Fd);
        true ->
            ok
    end,
    erlang:cancel_timer(FlushTimer),
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

log_message(Process, Level, Module, Line, [_|_] = Format, Args)
    when is_integer(Level), is_atom(Module), is_integer(Line) ->
    gen_server:cast(Process, {log, Level, erlang:now(),
                              Module, Line, self(), Format, Args}).

%% convert a loglevel atom into an integer value
%% that has meaning limited to this module
get_loglevel_integer(Level) when is_integer(Level) ->
    Level;
get_loglevel_integer(Level) when is_atom(Level) ->
    get_loglevel_integer(0,
        [undefined, critical, error, warning, info, debug], Level).

get_loglevel_integer(_, [], _) ->
    0;
get_loglevel_integer(LevelInteger, [LevelAtom | _], LevelAtom)
    when is_integer(LevelInteger), is_atom(LevelAtom) ->
    LevelInteger;
get_loglevel_integer(LevelInteger, [_ | OtherLevelAtoms], LevelAtom)
    when is_integer(LevelInteger), is_atom(LevelAtom) ->
    get_loglevel_integer(LevelInteger + 1, OtherLevelAtoms, LevelAtom).

%% determine what types of events sasl wants to log
%% taken from lib/erlang/lib/sasl-2.1.6/src/sasl.erl
get_sasl_error_logger_type() ->
    case application:get_env(sasl, errlog_type) of
        {ok, error} -> error;
        {ok, progress} -> progress;
        {ok, all} -> all;
        {ok, Bad} -> exit({bad_config, {sasl, {errlog_type, Bad}}});
        _ -> all
    end.

%% reopen the sasl log file after moving the file
reopen_sasl() ->
    case application:get_env(sasl, sasl_error_logger) of
        {ok, {file, SASLFileName}} ->
            error_logger:delete_report_handler(sasl_report_file_h),
            OldSASLFileName = filename:rootname(SASLFileName) ++ "-old.log",
            Result = file:rename(SASLFileName, OldSASLFileName),
            error_logger:add_report_handler(sasl_report_file_h,
                {SASLFileName, get_sasl_error_logger_type()}),
            Result;
        undefined ->
            {error, "sasl sasl_error_logger application env not set"};
        _ ->
            {error, "sasl sasl_error_logger application env invalid"}
    end.

%% append all queued log messages
output_queued(#state{fd = Fd,
                     last_log_entry = LastLogEntry} = State) ->
    PreviousEntries = lists:foldl(fun(Level, L) ->
        [erlang:element(Level, LastLogEntry) | L]
    end, [], lists:seq(?LOGLEVEL_FLUSH_THRESHOLD + 1,
                       erlang:size(LastLogEntry))),
    % order the log messages based on the timestamp
    lists:foreach(fun(Entry) ->
        output_to_file(Entry#state_last_log_entry.message,
                       Entry#state_last_log_entry.count, Fd)
    end, lists:keysort(#state_last_log_entry.message, PreviousEntries)),
    ok = file:sync(Fd),
    State#state{last_log_entry = erlang:make_tuple(5, #state_last_log_entry{})}.

%% take the log data as strings to append 
output(Key, Message, Level,
       #state{fd = Fd,
              last_log_entry = LastLogEntry} = State) ->
    if
        Level =< ?LOGLEVEL_FLUSH_THRESHOLD ->
            % write to the file and flush
            output_to_file(Message, 1, Fd),
            ok = file:sync(Fd),
            State;
        true ->
            % store the message or count repetitions
            % write the last message to the file for this logging level
            % if the current message is a unique one
            PreviousEntry = erlang:element(Level, LastLogEntry),
            State#state{last_log_entry = (if
                PreviousEntry#state_last_log_entry.key == Key ->
                    erlang:setelement(Level, LastLogEntry,
                        PreviousEntry#state_last_log_entry{
                            count = 1 + PreviousEntry#state_last_log_entry.count
                        });
                true ->
                    output_to_file(
                        PreviousEntry#state_last_log_entry.message,
                        PreviousEntry#state_last_log_entry.count, Fd),
                    erlang:setelement(Level, LastLogEntry,
                        #state_last_log_entry{
                            key = Key,
                            message = Message
                        })
            end)}
    end.

%% append the time and log data to the file,
%% with the repetition count if necessary
output_to_file("", _, _) ->
    ok;
output_to_file(Message, 1, Fd)
    when is_list(Message) ->
    file:write(Fd, io_lib:format(Message, ["---"]));
output_to_file(Message, Count, Fd)
    when is_list(Message), is_integer(Count) ->
    file:write(Fd, io_lib:format(Message, [io_lib:format("(x~p)", [Count])])).

-define(INTERFACE_MODULE_HEADER,
    "
    -module(cloudi_logger_interface).
    -author('mjtruog [at] gmail (dot) com').
    -export([critical/4, error/4, warning/4, info/4, debug/4]).").
get_interface_module_code(0, _) ->
    ?INTERFACE_MODULE_HEADER
    "
    critical(_, _, _, _) -> ok.
    error(_, _, _, _) -> ok.
    warning(_, _, _, _) -> ok.
    info(_, _, _, _) -> ok.
    debug(_, _, _, _) -> ok.
    ";
get_interface_module_code(1, Process) ->
    string2:format(
    ?INTERFACE_MODULE_HEADER
    "
    critical(Module, Line, Format, Arguments) ->
        cloudi_logger:critical(~p, Module, Line, Format, Arguments).
    error(_, _, _, _) -> ok.
    warning(_, _, _, _) -> ok.
    info(_, _, _, _) -> ok.
    debug(_, _, _, _) -> ok.
    ", [Process]);
get_interface_module_code(2, Process) ->
    string2:format(
    ?INTERFACE_MODULE_HEADER
    "
    critical(Module, Line, Format, Arguments) ->
        cloudi_logger:critical(~p, Module, Line, Format, Arguments).
    error(Module, Line, Format, Arguments) ->
        cloudi_logger:error(~p, Module, Line, Format, Arguments).
    warning(_, _, _, _) -> ok.
    info(_, _, _, _) -> ok.
    debug(_, _, _, _) -> ok.
    ", [Process, Process]);
get_interface_module_code(3, Process) ->
    string2:format(
    ?INTERFACE_MODULE_HEADER
    "
    critical(Module, Line, Format, Arguments) ->
        cloudi_logger:critical(~p, Module, Line, Format, Arguments).
    error(Module, Line, Format, Arguments) ->
        cloudi_logger:error(~p, Module, Line, Format, Arguments).
    warning(Module, Line, Format, Arguments) ->
        cloudi_logger:warning(~p, Module, Line, Format, Arguments).
    info(_, _, _, _) -> ok.
    debug(_, _, _, _) -> ok.
    ", [Process, Process, Process]);
get_interface_module_code(4, Process) ->
    string2:format(
    ?INTERFACE_MODULE_HEADER
    "
    critical(Module, Line, Format, Arguments) ->
        cloudi_logger:critical(~p, Module, Line, Format, Arguments).
    error(Module, Line, Format, Arguments) ->
        cloudi_logger:error(~p, Module, Line, Format, Arguments).
    warning(Module, Line, Format, Arguments) ->
        cloudi_logger:warning(~p, Module, Line, Format, Arguments).
    info(Module, Line, Format, Arguments) ->
        cloudi_logger:info(~p, Module, Line, Format, Arguments).
    debug(_, _, _, _) -> ok.
    ", [Process, Process, Process, Process]);
get_interface_module_code(_, Process) -> % 5
    string2:format(
    ?INTERFACE_MODULE_HEADER
    "
    critical(Module, Line, Format, Arguments) ->
        cloudi_logger:critical(~p, Module, Line, Format, Arguments).
    error(Module, Line, Format, Arguments) ->
        cloudi_logger:error(~p, Module, Line, Format, Arguments).
    warning(Module, Line, Format, Arguments) ->
        cloudi_logger:warning(~p, Module, Line, Format, Arguments).
    info(Module, Line, Format, Arguments) ->
        cloudi_logger:info(~p, Module, Line, Format, Arguments).
    debug(Module, Line, Format, Arguments) ->
        cloudi_logger:debug(~p, Module, Line, Format, Arguments).
    ", [Process, Process, Process, Process, Process]).

create_interface_module(Level) when is_integer(Level) ->
    code:delete(cloudi_logger_interface),
    % do not purge the module, but let it get purged after the new one is loaded
    {Module, Binary} = dynamic_compile:from_string(
        get_interface_module_code(Level, ?MODULE)),
    case code:load_binary(Module, "cloudi_logger_interface.erl", Binary) of
        {module, Module} ->
            {ok, Binary};
        {error, _} = Error ->
            Error
    end.

