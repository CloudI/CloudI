%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Service for the msg_size Test==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2011-2017, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2011-2017 Michael Truog
%%% @version 1.6.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_msg_size).
-author('mjtruog [at] gmail (dot) com').

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

-define(SUFFIXES_OPTIONAL, ["go"]).

-record(state,
    {
        service :: module() | [string()],
        request_count = 0 :: non_neg_integer(),
        elapsed_seconds = undefined :: float() | undefined,
        suffixes = ["cxx", "java", "javascript",
                    "perl", "php", "python", "python_c", "ruby"]
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

% for external services
aspect_init(CommandLine, _, _, undefined) ->
    {ok, #state{service = CommandLine}}.

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

    % Core i7 2670QM 2.2GHz 4 cores, 8 hyper-threads
    % L2:4×256KB L3:6MB RAM:8GB:DDR3-1333MHz
    % Sandy Bridge-HE-4 (Socket G2)
    % Erlang R16B03-1, Ubuntu 12.04.3 LTS (GNU/Linux 3.2.0-29-generic x86_64)

    % ~28 requests/second split between each non-Erlang suffix
    % then split between each programming language count_process
    % (cloudi_service_msg_size acts as a middleman, so it handles the total)

    {ok, State}.

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(_Args, Prefix, _Timeout, Dispatcher) ->
    State0 = #state{service = ?MODULE},
    StateN = lists:foldl(fun(Suffix, #state{suffixes = L} = State1) ->
        case cloudi_service:get_pid(Dispatcher, Prefix ++ Suffix, limit_min) of
            {ok, _} ->
                State1#state{suffixes = L ++ [Suffix]};
            {error, _} ->
                State1
        end
    end, State0, ?SUFFIXES_OPTIONAL),
    cloudi_service:subscribe(Dispatcher, "erlang"),
    {ok, StateN}.

cloudi_service_handle_request(_Type, _Name, Pattern, RequestInfo, Request,
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

cloudi_service_terminate(_Reason, _Timeout, #state{}) ->
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

