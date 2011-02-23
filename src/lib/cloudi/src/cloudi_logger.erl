%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Logger==
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
-export([start_link/1, reopen/0,
         fatal/5, error/5, warn/5, info/5, debug/5, trace/5]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("cloudi_configuration.hrl").

-record(state,
    {
        file_path,
        interface_module,
        fd
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

-spec fatal(Process :: atom(),
            Module :: atom(),
            Line :: integer(),
            Format :: string(),
            Args :: list()) ->
    'ok'.

fatal(Process, Module, Line, Format, Args) ->
    log_message(Process, fatal, Module, Line, Format, Args).

%%-------------------------------------------------------------------------
%% @doc
%% ===Error log message,===
%% which indicates a subsystem has failed but the failure is not fatal.
%% Called with ?LOG_ERROR(Format, []).
%% @end
%%-------------------------------------------------------------------------

-spec error(Process :: atom(),
            Module :: atom(),
            Line :: integer(),
            Format :: string(),
            Args :: list()) ->
    'ok'.

error(Process, Module, Line, Format, Args) ->
    log_message(Process, error, Module, Line, Format, Args).

%%-------------------------------------------------------------------------
%% @doc
%% ===Warning log message,===
%% which indicates an unexpected occurance was found in a subsystem.
%% Called with ?LOG_WARNING(Format, []).
%% @end
%%-------------------------------------------------------------------------

-spec warn(Process :: atom(),
           Module :: atom(),
           Line :: integer(),
           Format :: string(),
           Args :: list()) ->
    'ok'.

warn(Process, Module, Line, Format, Args) ->
    log_message(Process, warn, Module, Line, Format, Args).

%%-------------------------------------------------------------------------
%% @doc
%% ===Info log message,===
%% which indicates a subsystem has changed state.
%% Called with ?LOG_INFO(Format, []).
%% @end
%%-------------------------------------------------------------------------

-spec info(Process :: atom(),
           Module :: atom(),
           Line :: integer(),
           Format :: string(),
           Args :: list()) ->
    'ok'.

info(Process, Module, Line, Format, Args) ->
    log_message(Process, info, Module, Line, Format, Args).

%%-------------------------------------------------------------------------
%% @doc
%% ===Debug log message,===
%% which reports subsystem data that should be useful for debugging.
%% Called with ?LOG_DEBUG(Format, []).
%% @end
%%-------------------------------------------------------------------------

-spec debug(Process :: atom(),
            Module :: atom(),
            Line :: integer(),
            Format :: string(),
            Args :: list()) ->
    'ok'.

debug(Process, Module, Line, Format, Args) ->
    log_message(Process, debug, Module, Line, Format, Args).

%%-------------------------------------------------------------------------
%% @doc
%% ===Trace log message,===
%% which reports subsystem data that is only for tracing execution.
%% Called with ?LOG_TRACE(Format, []).
%% @end
%%-------------------------------------------------------------------------

-spec trace(Process :: atom(),
            Module :: atom(),
            Line :: integer(),
            Format :: string(),
            Args :: list()) ->
    'ok'.

trace(Process, Module, Line, Format, Args) ->
    log_message(Process, trace, Module, Line, Format, Args).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([#config_logging{level = Level,
                      file = FilePath}]) ->
    case file:open(FilePath, [append, raw]) of
        {ok, Fd} ->
            case load_interface_module(Level) of
                {ok, Binary} ->
                    {ok, #state{file_path = FilePath,
                                interface_module = Binary,
                                fd = Fd}};
                {error, Reason} ->
                    {stop, Reason}
            end;
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call(Request, _, State) ->
    {stop, string2:format("Unknown call \"~p\"~n", [Request]), error, State}.

handle_cast(reopen, #state{file_path = FilePath, fd = Fd} = State) ->
    file:close(Fd),
    case file:rename(FilePath, filename:rootname(FilePath) ++ "-old.log") of
        ok ->
            case reopen_sasl() of
                ok ->
                    case file:open(FilePath, [append, raw]) of
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

handle_cast({Level, {_, _, MicroSeconds} = Now, Pid,
             Module, Line, Format, Args},
            #state{fd = Fd} = State)
    when Level =:= fatal; Level =:= error; Level =:= warn;
         Level =:= info; Level =:= debug; Level =:= trace ->
    Description = lists:map(fun(S) ->
      io_lib:format(" ~s~n", [S])
    end, string:tokens(string2:format(Format, Args), "\n")),
    {{DateYYYY, DateMM, DateDD},
     {TimeHH, TimeMM, TimeSS}} = calendar:now_to_universal_time(Now),
    % ISO 8601 for date/time http://www.w3.org/TR/NOTE-datetime
    Message = string2:format("~4..0w-~2..0w-~2..0wT"
                             "~2..0w:~2..0w:~2..0w.~6..0wZ ~s"
                             " (~p:~p:~p)~n~s",
                             [DateYYYY, DateMM, DateDD,
                              TimeHH, TimeMM, TimeSS, MicroSeconds,
                              level_to_string(Level),
                              Module, Line, Pid, Description]),
    file:write(Fd, Message),
    %file:sync(Fd),
    {noreply, State};

handle_cast(Request, State) ->
    {stop, string2:format("Unknown cast \"~p\"~n", [Request]), State}.

handle_info(Request, State) ->
    {stop, string2:format("Unknown info \"~p\"~n", [Request]), State}.

terminate(_, #state{fd = Fd}) ->
    file:close(Fd),
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

log_message(Process, Level, Module, Line, Format, Args)
    when is_atom(Level), is_atom(Module), is_integer(Line) ->
    gen_server:cast(Process, {Level, erlang:now(), self(),
                              Module, Line, Format, Args}).

level_to_string(fatal) ->
    "FATAL";
level_to_string(error) ->
    "ERROR";
level_to_string(warn) ->
    "WARN ";
level_to_string(info) ->
    "INFO ";
level_to_string(debug) ->
    "DEBUG";
level_to_string(trace) ->
    "TRACE".

-define(INTERFACE_MODULE_HEADER,
    "
    -module(cloudi_logger_interface).
    -author('mjtruog [at] gmail (dot) com').
    -export([fatal/4, error/4, warn/4, info/4, debug/4]).").
get_interface_module_code(off, _) ->
    ?INTERFACE_MODULE_HEADER
    "
    fatal(_, _, _, _) -> ok.
    error(_, _, _, _) -> ok.
    warn(_, _, _, _) -> ok.
    info(_, _, _, _) -> ok.
    debug(_, _, _, _) -> ok.
    trace(_, _, _, _) -> ok.
    ";
get_interface_module_code(fatal, Process) ->
    string2:format(
    ?INTERFACE_MODULE_HEADER
    "
    fatal(Module, Line, Format, Arguments) ->
        cloudi_logger:fatal(~p, Module, Line, Format, Arguments).
    error(_, _, _, _) -> ok.
    warn(_, _, _, _) -> ok.
    info(_, _, _, _) -> ok.
    debug(_, _, _, _) -> ok.
    trace(_, _, _, _) -> ok.
    ", [Process]);
get_interface_module_code(error, Process) ->
    string2:format(
    ?INTERFACE_MODULE_HEADER
    "
    fatal(Module, Line, Format, Arguments) ->
        cloudi_logger:fatal(~p, Module, Line, Format, Arguments).
    error(Module, Line, Format, Arguments) ->
        cloudi_logger:error(~p, Module, Line, Format, Arguments).
    warn(_, _, _, _) -> ok.
    info(_, _, _, _) -> ok.
    debug(_, _, _, _) -> ok.
    trace(_, _, _, _) -> ok.
    ", [Process, Process]);
get_interface_module_code(warn, Process) ->
    string2:format(
    ?INTERFACE_MODULE_HEADER
    "
    fatal(Module, Line, Format, Arguments) ->
        cloudi_logger:fatal(~p, Module, Line, Format, Arguments).
    error(Module, Line, Format, Arguments) ->
        cloudi_logger:error(~p, Module, Line, Format, Arguments).
    warn(Module, Line, Format, Arguments) ->
        cloudi_logger:warn(~p, Module, Line, Format, Arguments).
    info(_, _, _, _) -> ok.
    debug(_, _, _, _) -> ok.
    trace(_, _, _, _) -> ok.
    ", [Process, Process, Process]);
get_interface_module_code(info, Process) ->
    string2:format(
    ?INTERFACE_MODULE_HEADER
    "
    fatal(Module, Line, Format, Arguments) ->
        cloudi_logger:fatal(~p, Module, Line, Format, Arguments).
    error(Module, Line, Format, Arguments) ->
        cloudi_logger:error(~p, Module, Line, Format, Arguments).
    warn(Module, Line, Format, Arguments) ->
        cloudi_logger:warn(~p, Module, Line, Format, Arguments).
    info(Module, Line, Format, Arguments) ->
        cloudi_logger:info(~p, Module, Line, Format, Arguments).
    debug(_, _, _, _) -> ok.
    trace(_, _, _, _) -> ok.
    ", [Process, Process, Process, Process]);
get_interface_module_code(debug, Process) ->
    string2:format(
    ?INTERFACE_MODULE_HEADER
    "
    fatal(Module, Line, Format, Arguments) ->
        cloudi_logger:fatal(~p, Module, Line, Format, Arguments).
    error(Module, Line, Format, Arguments) ->
        cloudi_logger:error(~p, Module, Line, Format, Arguments).
    warn(Module, Line, Format, Arguments) ->
        cloudi_logger:warn(~p, Module, Line, Format, Arguments).
    info(Module, Line, Format, Arguments) ->
        cloudi_logger:info(~p, Module, Line, Format, Arguments).
    debug(Module, Line, Format, Arguments) ->
        cloudi_logger:debug(~p, Module, Line, Format, Arguments).
    trace(_, _, _, _) -> ok.
    ", [Process, Process, Process, Process, Process]);
get_interface_module_code(trace, Process) ->
    string2:format(
    ?INTERFACE_MODULE_HEADER
    "
    fatal(Module, Line, Format, Arguments) ->
        cloudi_logger:fatal(~p, Module, Line, Format, Arguments).
    error(Module, Line, Format, Arguments) ->
        cloudi_logger:error(~p, Module, Line, Format, Arguments).
    warn(Module, Line, Format, Arguments) ->
        cloudi_logger:warn(~p, Module, Line, Format, Arguments).
    info(Module, Line, Format, Arguments) ->
        cloudi_logger:info(~p, Module, Line, Format, Arguments).
    debug(Module, Line, Format, Arguments) ->
        cloudi_logger:debug(~p, Module, Line, Format, Arguments).
    trace(Module, Line, Format, Arguments) ->
        cloudi_logger:trace(~p, Module, Line, Format, Arguments).
    ", [Process, Process, Process, Process, Process, Process]).

load_interface_module(Level) when is_atom(Level) ->
    code:delete(cloudi_logger_interface),
    % do not purge the module, but let it get purged after the new one is loaded
    {Module, Binary} =
        dynamic_compile:from_string(get_interface_module_code(Level, ?MODULE)),
    case code:load_binary(Module, "cloudi_logger_interface.erl", Binary) of
        {module, Module} ->
            {ok, Binary};
        {error, _} = Error ->
            Error
    end.

%% determine what types of events sasl wants to log
%% taken from lib/erlang/lib/sasl-2.1.6/src/sasl.erl
get_sasl_error_logger_type() ->
    case application:get_env(sasl, errlog_type) of
        {ok, error} ->
            error;
        {ok, progress} ->
            progress;
        {ok, all} ->
            all;
        {ok, Bad} ->
            exit({bad_config, {sasl, {errlog_type, Bad}}});
        _ ->
            all
    end.

%% reopen the sasl log file after moving the file
reopen_sasl() ->
    case application:get_env(sasl, sasl_error_logger) of
        {ok, {file, SASLFilePath}} ->
            error_logger:delete_report_handler(sasl_report_file_h),
            OldSASLFilePath = filename:rootname(SASLFilePath) ++ "-old.log",
            Result = file:rename(SASLFilePath, OldSASLFilePath),
            error_logger:add_report_handler(sasl_report_file_h,
                {SASLFilePath, get_sasl_error_logger_type()}),
            Result;
        undefined ->
            {error, "sasl sasl_error_logger application env not set"};
        _ ->
            {error, "sasl sasl_error_logger application env invalid"}
    end.

