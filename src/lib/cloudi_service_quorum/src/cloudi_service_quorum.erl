%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Quorum Service==
%%% Provide quorum for service requests.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2013, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2013 Michael Truog
%%% @version 1.3.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_quorum).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloudi_service).

%% external interface

%% cloudi_service callbacks
-export([cloudi_service_init/3,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/2]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").
-include_lib("cloudi_core/include/cloudi_service.hrl").

-define(DEFAULT_QUORUM,                      0.51). % percent or integer count
-define(DEFAULT_USE_RESPONSE_INFO,           true). % check for match

-record(request,
    {
        type :: cloudi_service:request_type(),
        name :: cloudi_service:service_name(),
        pattern :: cloudi_service:service_name_pattern(),
        timeout :: cloudi_service:timeout_milliseconds(),
        pid :: pid(),
        required_count :: pos_integer(),
        responses = [] :: list({{cloudi_service:response_info(),
                                 cloudi_service:response()},
                                pos_integer()}) % response -> count
    }).

-record(state,
    {
        prefix :: string(),
        quorum :: number(),
        use_response_info :: boolean(),
        requests = dict:new() :: dict(), % Original TransId -> #request{}
        pending = dict:new() :: dict() % TransId -> Original TransId
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, _Prefix, Dispatcher) ->
    Defaults = [
        {quorum,                   ?DEFAULT_QUORUM},
        {use_response_info,        ?DEFAULT_USE_RESPONSE_INFO}],
    [Quorum, UseResponseInfo] = cloudi_proplists:take_values(Defaults, Args),
    true = ((is_float(Quorum) andalso
             (Quorum > 0.0) andalso (Quorum =< 1.0)) orelse
            (is_integer(Quorum) andalso
             (Quorum > 0))),
    true = is_boolean(UseResponseInfo),
    cloudi_service:subscribe(Dispatcher, "*"),
    {ok, #state{quorum = Quorum}}.

cloudi_service_handle_request(Type, Name, Pattern, RequestInfo, Request,
                              Timeout, Priority, TransId, Pid,
                              #state{quorum = Quorum,
                                     requests = Requests,
                                     pending = Pending} = State,
                              Dispatcher) ->
    Parameters = [_ | _] = cloudi_service:service_name_parse(Name, Pattern),
    QuorumName = lists:last(Parameters),
    case cloudi_service:mcast_async_active(Dispatcher, QuorumName,
                                           RequestInfo, Request,
                                           Timeout, Priority) of
        {ok, QuorumTransIds} ->
            {Count, NewPending} = pending_store(QuorumTransIds,
                                                Pending, TransId),
            RequiredCount = if
                is_integer(Quorum) ->
                    Quorum;
                is_float(Quorum) ->
                    erlang:round(Quorum * Count + 0.5) % ceil
            end,
            Request = #request{type = Type,
                               name = Name,
                               pattern = Pattern,
                               timeout = Timeout,
                               pid = Pid,
                               required_count = RequiredCount},
            {noreply, State#state{requests = dict:store(TransId,
                                                        Request,
                                                        Requests),
                                  pending = NewPending}};
        {error, timeout} ->
            {reply, <<>>, State};
        {error, Reason} ->
            ?LOG_ERROR("request to ~p failed: ~p", [QuorumName, Reason]),
            {reply, <<>>, State}
    end.

cloudi_service_handle_info(#return_async_active{response_info = ResponseInfo,
                                                response = Response,
                                                trans_id = TransId},
                           #state{use_response_info = UseResponseInfo,
                                  requests = Requests,
                                  pending = Pending} = State,
                           Dispatcher) ->
    OriginalTransId = dict:fetch(TransId, Pending),
    Key = if
        UseResponseInfo =:= true ->
            {ResponseInfo, Response};
        UseResponseInfo =:= false ->
            Response
    end,
    {noreply, State#state{requests = request_check(Key, ResponseInfo, Response,
                                                   OriginalTransId, Requests,
                                                   Dispatcher),
                          pending = dict:erase(TransId, Pending)}};

cloudi_service_handle_info(#timeout_async_active{trans_id = TransId},
                           #state{requests = Requests,
                                  pending = Pending} = State,
                           _Dispatcher) ->
    OriginalTransId = dict:fetch(TransId, Pending),
    {noreply, State#state{requests = dict:erase(OriginalTransId, Requests),
                          pending = dict:erase(TransId, Pending)}};

cloudi_service_handle_info(Request, State, _) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_service_terminate(_, #state{}) ->
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

pending_store(QuorumTransIds, Pending, OriginalTransId) ->
    pending_store(QuorumTransIds, 0, Pending, OriginalTransId).

pending_store([], Count, Pending, _OriginalTransId)
    when Count > 0 ->
    {Count, Pending};
pending_store([TransId | L], Count, Pending, OriginalTransId) ->
    pending_store(L, Count + 1, dict:store(TransId, OriginalTransId, Pending),
                  OriginalTransId).

request_check(Key, ResponseInfo, Response, OriginalTransId, Requests,
              Dispatcher) ->
    case dict:find(OriginalTransId, Requests) of
        {ok, #request{% return data
                      type = ResponseType,
                      name = ResponseName,
                      pattern = ResponsePattern,
                      timeout = ResponseTimeout,
                      pid = ResponsePid,
                      % quorum data
                      required_count = RequiredCount,
                      responses = Responses} = Request} ->
            Count = case orddict:find(Key, Responses) of
                {ok, I} ->
                    I + 1;
                error ->
                    1
            end,
            if
                Count == RequiredCount ->
                    cloudi_service:return_nothrow(Dispatcher, ResponseType,
                                                  ResponseName,
                                                  ResponsePattern,
                                                  ResponseInfo, Response,
                                                  ResponseTimeout,
                                                  OriginalTransId,
                                                  ResponsePid),
                    dict:erase(OriginalTransId, Requests);
                true ->
                    NewResponses = orddict:store(Key, Count, Responses),
                    NewRequest = Request#request{responses = NewResponses},
                    dict:store(OriginalTransId, NewRequest, Requests)
            end;
        error ->
            % already met quorum and was returned
            Requests
    end.

