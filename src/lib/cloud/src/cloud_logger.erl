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

-module(cloud_logger).
-author('mjtruog [at] gmail (dot) com').

-behaviour(gen_server).

%% external interface
-export([start_link/1, flush/0, reopen/0,
         critical/4, error/4, warning/4, info/4, debug/4]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("cloud_configuration.hrl").

-define(LOGLEVEL_FLUSH_THRESHOLD, 2).   % critical/error is flushed immediately
-define(LOG_DATA_FLUSH_TIMEOUT, 60000). % 1 minute

-record(state_last_log_entry,
    {
    key = "",
    message = "",
    count = 1}).

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
    fd = undefined,
    flush_timer = undefined,
    last_log_entry = erlang:make_tuple(5, #state_last_log_entry{})}).

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
%% ===Critical log message without arguments,===
%% which indicates the system has failed and can not continue.
%% Called with ?LOG_CRITICAL(Format, []).
%% @end
%%-------------------------------------------------------------------------

-spec critical(Module :: atom(),
               Line :: integer(),
               Format :: string(),
               Args :: list(any())) ->
    'ok'.

critical(Module, Line, [_|_] = Format, [])
    when is_atom(Module), is_integer(Line) ->
    log_message(1, Module, Line, Format, []);

%%-------------------------------------------------------------------------
%% @doc
%% ===Critical log message with arguments,===
%% which indicates the system has failed and can not continue.
%% Called with ?LOG_CRITICAL(Format, Args).
%% @end
%%-------------------------------------------------------------------------

critical(Module, Line, [_|_] = Format, [_|_] = Args)
    when is_atom(Module), is_integer(Line) ->
    log_message(1, Module, Line, Format, Args).

%%-------------------------------------------------------------------------
%% @doc
%% ===Error log message without arguments,===
%% which indicates a subsystem has failed but the failure is not critical.
%% Called with ?LOG_ERROR(Format, []).
%% @end
%%-------------------------------------------------------------------------

-spec error(Module :: atom(),
            Line :: integer(),
            Format :: string(),
            Args :: list(any())) ->
    'ok'.

error(Module, Line, [_|_] = Format, [])
    when is_atom(Module), is_integer(Line) ->
    log_message(2, Module, Line, Format, []);

%%-------------------------------------------------------------------------
%% @doc
%% ===Error log message with arguments,===
%% which indicates a subsystem has failed but the failure is not critical.
%% Called with ?LOG_ERROR(Format, Args).
%% @end
%%-------------------------------------------------------------------------

error(Module, Line, [_|_] = Format, [_|_] = Args)
    when is_atom(Module), is_integer(Line) ->
    log_message(2, Module, Line, Format, Args).

%%-------------------------------------------------------------------------
%% @doc
%% ===Warning log message without arguments,===
%% which indicates an unexpected occurance was found in a subsystem.
%% Called with ?LOG_WARNING(Format, []).
%% @end
%%-------------------------------------------------------------------------

-spec warning(Module :: atom(),
              Line :: integer(),
              Format :: string(),
              Args :: list(any())) ->
    'ok'.

warning(Module, Line, [_|_] = Format, [])
    when is_atom(Module), is_integer(Line) ->
    log_message(3, Module, Line, Format, []);

%%-------------------------------------------------------------------------
%% @doc
%% ===Warning log message with arguments,===
%% which indicates an unexpected occurance was found in a subsystem.
%% Called with ?LOG_WARNING(Format, Args).
%% @end
%%-------------------------------------------------------------------------

warning(Module, Line, [_|_] = Format, [_|_] = Args)
    when is_atom(Module), is_integer(Line) ->
    log_message(3, Module, Line, Format, Args).

%%-------------------------------------------------------------------------
%% @doc
%% ===Info log message without arguments,===
%% which indicates a subsystem has changed state.
%% Called with ?LOG_INFO(Format, []).
%% @end
%%-------------------------------------------------------------------------

-spec info(Module :: atom(),
           Line :: integer(),
           Format :: string(),
           Args :: list(any())) ->
    'ok'.

info(Module, Line, [_|_] = Format, [])
    when is_atom(Module), is_integer(Line) ->
    log_message(4, Module, Line, Format, []);

%%-------------------------------------------------------------------------
%% @doc
%% ===Info log message with arguments,===
%% which indicates a subsystem has changed state.
%% Called with ?LOG_INFO(Format, Args).
%% @end
%%-------------------------------------------------------------------------

info(Module, Line, [_|_] = Format, [_|_] = Args)
    when is_atom(Module), is_integer(Line) ->
    log_message(4, Module, Line, Format, Args).

%%-------------------------------------------------------------------------
%% @doc
%% ===Debug log message without arguments,===
%% which reports subsystem data that should be useful for debugging.
%% Called with ?LOG_DEBUG(Format, []).
%% @end
%%-------------------------------------------------------------------------

-spec debug(Module :: atom(),
            Line :: integer(),
            Format :: string(),
            Args :: list(any())) ->
    'ok'.

debug(Module, Line, [_|_] = Format, [])
    when is_atom(Module), is_integer(Line) ->
    log_message(5, Module, Line, Format, []);

%%-------------------------------------------------------------------------
%% @doc
%% ===Debug log message with arguments,===
%% which reports subsystem data that should be useful for debugging.
%% Called with ?LOG_DEBUG(Format, Args).
%% @end
%%-------------------------------------------------------------------------

debug(Module, Line, [_|_] = Format, [_|_] = Args)
    when is_atom(Module), is_integer(Line) ->
    log_message(5, Module, Line, Format, Args).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([#config_logging{loglevel = Level, filename = FileName}]) ->
    case file:open(FileName, [append, raw]) of
        {ok, Fd} ->
            cloud_logger = ets:new(cloud_logger,
                [named_table, protected, set]),
            true = ets:insert(cloud_logger,
                {loglevel, get_loglevel_integer(Level)}),
            FlushTimer = erlang:send_after(
                ?LOG_DATA_FLUSH_TIMEOUT, self(), flush),
            {ok, #state{
                filename = FileName,
                fd = Fd,
                flush_timer = FlushTimer}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call(flush, _, State) ->
    {reply, ok, append_queued_log_messages(State), hibernate};

handle_call(Request, _, State) ->
    {stop, string_extensions:format("Unknown call \"~p\"", [Request]),
     error, State}.

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

handle_cast({log, Level, {_, _, MicroSeconds},
             {{DateYYYY, DateMM, DateDD}, {TimeHH, TimeMM, TimeSS}},
             Module, Line, Pid, Format, Args},
            #state{loglevel_names = LogLevelNames} = State) ->
    Description = io_lib:format(Format, Args),
    % ISO 8601 for date/time http://www.w3.org/TR/NOTE-datetime
    Message = string_extensions:format(
        "~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w.~6..0wZ ~s "
        "~~s (~p:~p:~p) ~s~n", [
        DateYYYY, DateMM, DateDD, TimeHH, TimeMM, TimeSS, MicroSeconds,
        erlang:element(Level, LogLevelNames), Module, Line, Pid, Description]),
    % used to determine duplicate entries
    Key = io_lib:format("~p,~p,~p", [Module, Line, Description]),
    {noreply, append_log_message(Key, Message, Level, State)};

handle_cast(Request, State) ->
    error_logger:warning_msg("(~p:~p:~p) Unknown cast \"~p\"~n",
        [?MODULE, ?LINE, self(), Request]),
    {noreply, State}.

handle_info(flush, State) ->
    NewState = append_queued_log_messages(State),
    FlushTimer = erlang:send_after(?LOG_DATA_FLUSH_TIMEOUT, self(), flush),
    {noreply, NewState#state{flush_timer = FlushTimer}, hibernate};

handle_info(Request, State) ->
    error_logger:warning_msg("(~p:~p:~p) Unknown info \"~p\"~n",
        [?MODULE, ?LINE, self(), Request]),
    {noreply, State}.

terminate(_, #state{fd = Fd, flush_timer = FlushTimer} = State) ->
    append_queued_log_messages(State),
    if
        Fd /= undefined ->
            file:close(Fd);
        true ->
            ok
    end,
    erlang:cancel_timer(FlushTimer),
    ets:delete(cloud_logger),
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

log_message(Level, Module, Line, [_|_] = Format, Args)
    when is_integer(Level), is_atom(Module), is_integer(Line) ->
    [{loglevel, LevelLimit}] = ets:lookup(cloud_logger, loglevel),
    if
        Level =< LevelLimit ->
            gen_server:cast(?MODULE, 
                {log,
                 Level,
                 erlang:now(),
                 erlang:universaltime(),
                 Module, Line, self(), Format, Args});
        true ->
            ok
    end.

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
append_queued_log_messages(
    #state{fd = Fd, last_log_entry = LastLogEntry} = State) ->
    PreviousEntries = lists:foldl(fun(Level, L) ->
        [erlang:element(Level, LastLogEntry) | L]
    end, [],
    lists:seq(?LOGLEVEL_FLUSH_THRESHOLD + 1, erlang:size(LastLogEntry))),
    % order the log messages based on the timestamp
    lists:foreach(fun(Entry) ->
        append_log_message_to_file(
            Entry#state_last_log_entry.message,
            Entry#state_last_log_entry.count, Fd)
    end, lists:keysort(#state_last_log_entry.message, PreviousEntries)),
    file_flush(Fd),
    State#state{last_log_entry = erlang:make_tuple(5, #state_last_log_entry{})}.

%% take the log data as strings to append 
append_log_message(Key, Message, Level,
    #state{fd = Fd, last_log_entry = LastLogEntry} = State) ->
    if
        Level =< ?LOGLEVEL_FLUSH_THRESHOLD ->
            % write to the file and flush
            append_log_message_to_file(Message, 1, Fd),
            file_flush(Fd),
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
                    append_log_message_to_file(
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
append_log_message_to_file("", _, _) ->
    ok;
append_log_message_to_file(Message, 1, Fd)
    when is_list(Message) ->
    file:write(Fd, io_lib:format(Message, ["---"]));
append_log_message_to_file(Message, Count, Fd)
    when is_list(Message), is_integer(Count) ->
    file:write(Fd, io_lib:format(Message, [io_lib:format("(x~p)", [Count])])).

%% flush the file descriptor
file_flush(Fd) ->
    case file:sync(Fd) of
        ok ->
            ok;
        {error, Reason} ->
            error_logger:error_msg("(~p:~p:~p) unable to flush file: ~p~n",
                [?MODULE, ?LINE, self(), Reason])
    end.

