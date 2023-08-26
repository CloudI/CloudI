%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Erlang Kernel Logger Integration==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2023 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2023 Michael Truog
%%% @version 2.0.7 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_logger_kernel).
-author('mjtruog at protonmail dot com').

%% external interface
-export([filter_out_supervisor_started/2]).

%%-------------------------------------------------------------------------
%% @doc
%% ===Erlang kernel logger filter for supervisor report started messages.===
%% {fun cloudi_logger_kernel:filter_out_supervisor_started/2,
%%  [tls_dyn_connection_sup]}
%% will prevent the logging of supervisor report started messages from
%% the tls_dyn_connection_sup module.
%% @end
%%-------------------------------------------------------------------------

-spec filter_out_supervisor_started(logger:log_event(),
                                    Supervisors :: nonempty_list(module())) ->
    ignore | stop.

filter_out_supervisor_started(#{level := info,
                                meta := #{domain := _,
                                          error_logger := _,
                                          file := _,
                                          gl := _,
                                          line := _,
                                          logger_formatter := _,
                                          mfa := {supervisor,
                                                  report_progress, 2},
                                          pid := _,
                                          report_cb := _,
                                          time := _},
                                msg := {report,
                                        #{label := {supervisor, progress},
                                          report := Report}}},
                              Supervisors) ->
    case Report of
        [{supervisor, {_, Supervisor}},
         {started, _}] ->
            case lists:member(Supervisor, Supervisors) of
                true ->
                    stop;
                false ->
                    ignore
            end;
        _ ->
            ignore
    end;
filter_out_supervisor_started(_, _) ->
    ignore.

