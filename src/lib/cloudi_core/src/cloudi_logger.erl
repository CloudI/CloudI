%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Logger Configuration==
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
%%% @version 2.0.6 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_logger).
-author('mjtruog at protonmail dot com').

%% external interface
-export([aspect_stop_on_fatal/10,
         aspect_stop_on_error/10]).

%%-------------------------------------------------------------------------
%% @doc
%% ===Fatal stop function for aspects_log_after logging_set configuration.===
%% Add as {cloudi_logger, aspect_stop_on_fatal}.
%% With the aspect function added to the configuration, CloudI log output
%% will cause CloudI to stop execution after fatal log output.
%% @end
%%-------------------------------------------------------------------------

-spec aspect_stop_on_fatal(Level :: cloudi_service_api:loglevel_on(),
                           Timestamp :: erlang:timestamp(),
                           Node :: node(),
                           Pid :: pid(),
                           FileName :: nonempty_string(),
                           Line :: non_neg_integer(),
                           Function :: atom() | undefined,
                           Arity :: arity() | undefined,
                           MetaData :: list({atom(), any()}) | #{},
                           LogMessage :: iodata()) ->
    ok.

aspect_stop_on_fatal(fatal, _, _, _, _, _, _, _, _, _) ->
    init:stop(1);
aspect_stop_on_fatal(_, _, _, _, _, _, _, _, _, _) ->
    ok.

%%-------------------------------------------------------------------------
%% @doc
%% ===Error stop function for aspects_log_after logging_set configuration.===
%% Add as {cloudi_logger, aspect_stop_on_error}.
%% With the aspect function added to the configuration, CloudI log output
%% will cause CloudI to stop execution after fatal or error log output.
%% @end
%%-------------------------------------------------------------------------

-spec aspect_stop_on_error(Level :: cloudi_service_api:loglevel_on(),
                           Timestamp :: erlang:timestamp(),
                           Node :: node(),
                           Pid :: pid(),
                           FileName :: nonempty_string(),
                           Line :: non_neg_integer(),
                           Function :: atom() | undefined,
                           Arity :: arity() | undefined,
                           MetaData :: list({atom(), any()}) | #{},
                           LogMessage :: iodata()) ->
    ok.

aspect_stop_on_error(Level, _, _, _, _, _, _, _, _, _)
    when Level =:= fatal orelse Level =:= error ->
    init:stop(1);
aspect_stop_on_error(_, _, _, _, _, _, _, _, _, _) ->
    ok.

