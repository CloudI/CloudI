%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Service for the msg_size Test==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2011-2020 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2011-2020 Michael Truog
%%% @version 2.0.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_test_msg_size).
-author('mjtruog at protonmail dot com').

-behaviour(cloudi_service).

%% external interface
-export([aspect_init/4,
         aspect_request/10,
         aspect_request/11,
         aspect_terminate/3]).

%% cloudi_service callbacks
-export([cloudi_service_init/4,
         cloudi_service_handle_request/11,
         cloudi_service_terminate/3]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").

-define(SUFFIXES_REQUIRED,
        ["cxx", "java", "javascript", "perl",
         "php", "python", "python_c", "ruby"]).
-define(SUFFIXES_OPTIONAL,
        ["go", "haskell", "ocaml"]).

-record(state,
    {
        service :: module() | list(string()),
        request_count = 0 :: non_neg_integer(),
        elapsed_seconds = undefined :: float() | undefined,
        suffixes = undefined :: nonempty_list(string()) | undefined
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

% for external services
aspect_init(CommandLine, _, _, undefined) ->
    {ok, #state{service = CommandLine}};
aspect_init(CommandLine, _, _, #state{} = State) ->
    % an update occurred to get to this function clause
    % when a new OS process is initialized with older state data
    {ok, State#state{service = CommandLine}}.

% for external services
aspect_request(_, _, _, _, _, _, _, TransId, _,
               #state{request_count = Count} = State) ->
    true = is_binary(TransId),
    {ok, State#state{request_count = Count + 1,
                     elapsed_seconds = elapsed_seconds(TransId)}}.
 
% for internal services
aspect_request(_, _, _, _, _, _, _, TransId, _,
               #state{request_count = Count} = State, _) ->
    true = is_binary(TransId),
    {ok, State#state{request_count = Count + 1,
                     elapsed_seconds = elapsed_seconds(TransId)}}.
 
% for internal and external services
aspect_terminate(_, _, undefined) ->
    % aspect_init/4 was not called due to init not completing 
    {ok, undefined};
aspect_terminate(_, _, #state{service = Service,
                              elapsed_seconds = undefined} = State) ->
    ?LOG_WARN("msg_size 0 requests/second "
              "forwarded for~n~p",
              [Service]),
    {ok, State};
aspect_terminate(_, _, #state{service = Service,
                              request_count = Count,
                              elapsed_seconds = ElapsedSeconds} = State) ->
    % to trigger this:
    % cloudi_service_api:services_remove([element(1, S) || S <- element(2, cloudi_service_api:services(infinity)), (element(2, element(2, S)) == "/tests/msg_size/")], infinity).
    ?LOG_INFO("msg_size ~p requests/second "
              "(during ~p seconds) forwarded for~n~p",
              [erlang:round((Count / ElapsedSeconds) * 10.0) / 10.0,
               erlang:round(ElapsedSeconds * 10.0) / 10.0,
               Service]),
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(_Args, Prefix, _Timeout, Dispatcher) ->
    Suffixes = lists:sort(lists:foldl(fun(Suffix, L) ->
        case cloudi_service:get_pid(Dispatcher, Prefix ++ Suffix, limit_min) of
            {ok, _} ->
                [Suffix | L];
            {error, _} ->
                L
        end
    end, ?SUFFIXES_REQUIRED, ?SUFFIXES_OPTIONAL)),
    cloudi_service:subscribe(Dispatcher, "erlang"),
    {ok,
     #state{service = ?MODULE,
            suffixes = Suffixes}}.

cloudi_service_handle_request(_RequestType, _Name, Pattern,
                              RequestInfo, Request,
                              Timeout, Priority, _TransId, _Pid,
                              #state{suffixes = [Suffix | Suffixes]} = State,
                              _Dispatcher) ->
    2097152 = erlang:byte_size(Request), % from cxx service
    -5 = Priority,                       % from cxx service
    <<I:32/unsigned-integer-native, Rest/binary>> = Request,
    NewI = if
        I == 4294967295 ->
            0;
        true ->
            I + 1
    end,
    NewRequest = <<NewI:32/unsigned-integer-native, Rest/binary>>,
    NewName = cloudi_string:beforer($/, Pattern, input) ++ [$/ | Suffix],
    ?LOG_INFO("forward #~w erlang to ~s (with timeout ~w ms)",
              [NewI, NewName, Timeout]),
    {forward, NewName, RequestInfo, NewRequest,
     State#state{suffixes = Suffixes ++ [Suffix]}}.

cloudi_service_terminate(_Reason, _Timeout, _State) ->
    ?LOG_INFO("terminate msg_size erlang", []),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

elapsed_seconds(TransId) ->
    % original timeout is defined in cxx service source code
    % (600000 milliseconds timeout value with a
    %  100 millisecond penalty for each forward along with
    %  the added request_timeout_adjustment and any queuing delays)
    % (assuming the system time doesn't change during the test's execution)
    (cloudi_x_uuid:get_v1_time(os) -
     cloudi_x_uuid:get_v1_time(TransId)) / 1000000.0.

