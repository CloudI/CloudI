%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI ZeroMQ Integration==
%%% Provide a way of sending/receiving through ZeroMQ.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2011-2013, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2011-2013 Michael Truog
%%% @version 1.3.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_zeromq).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloudi_service).

%% external interface

%% cloudi_service callbacks
-export([cloudi_service_init/3,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/2]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").

-define(DEFAULT_ENDIAN,                    native). % ZeroMQ binary
                                                    % integer sizes for metadata
-define(DEFAULT_PROCESS_METADATA,           false). % RequestInfo/ResponseInfo
                                                    % tunneling service request
                                                    % meta-data through ZeroMQ
                                                    % w/4-byte big endian header

-record(state,
    {
        context :: erlzmq:erlzmq_context(),
        endian :: big | little | native,
        process_metadata :: boolean(),
        publish :: cloudi_x_trie:cloudi_x_trie(),
                % NameInternal -> [{NameExternal, Socket} | _]
        request :: cloudi_x_trie:cloudi_x_trie(),
                % Name -> Socket
        push :: cloudi_x_trie:cloudi_x_trie(),
             % Name -> Socket
        receives :: dict(), % Socket -> {reply, Name}
                            % Socket -> [{subscribe,
                            %             {BinaryMax, BinaryPattern,
                            %              NameExternal -> NameInternal}} | _]
                            % Socket -> {request, F(Response)}
        reply_replies = dict:new() :: dict()    % TransId -> Socket
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, Prefix, Dispatcher) ->
    {SubscribeL, L1} = cloudi_proplists:partition(subscribe, Args),
    {PublishL, L2} = cloudi_proplists:partition(publish, L1),
    {RequestL, L3} = cloudi_proplists:partition(outbound, L2),
    {ReplyL, L4} = cloudi_proplists:partition(inbound, L3),
    {PullL, L5} = cloudi_proplists:partition(pull, L4),
    {PushL, L6} = cloudi_proplists:partition(push, L5),
    Defaults = [
        {endian,                   ?DEFAULT_ENDIAN},
        {process_metadata,         ?DEFAULT_PROCESS_METADATA}],
    [Endian, ProcessMetaData] = case L6 of
        [] ->
            cloudi_proplists:take_values(Defaults, []);
        [{options, Options}] ->
            cloudi_proplists:take_values(Defaults, Options)
    end,
    true = (Endian =:= big orelse
            Endian =:= little orelse
            Endian =:= native),
    true = is_boolean(ProcessMetaData),

    {ok, Context} = erlzmq:context(),
    Service = cloudi_service:self(Dispatcher),
    ReceivesZMQ1 = dict:new(),
    Publish = lists:foldl(fun({publish,
                               {[{[I1a | _], [I1b | _]} | _] = NamePairL,
                                [[I2 | _] | _] = EndpointL}}, D) ->
        true = is_integer(I1a) and is_integer(I1b) and is_integer(I2),
        {ok, S} = erlzmq:socket(Context, [pub]),
        lists:foreach(fun(Endpoint) ->
            ok = erlzmq:bind(S, Endpoint)
        end, EndpointL),
        lists:foldl(fun({NameInternal, NameExternal}, DD) ->
            cloudi_service:subscribe(Dispatcher, NameInternal),
            cloudi_x_trie:append(Prefix ++ NameInternal,
                        {erlang:list_to_binary(NameExternal), S}, DD)
        end, D, NamePairL)
    end, cloudi_x_trie:new(), PublishL),
    ReceivesZMQ2 = lists:foldl(fun({subscribe,
                                   {[{[I1a | _], [I1b | _]} | _] = NamePairL,
                                    [[I2 | _] | _] = EndpointL}}, D) ->
        true = is_integer(I1a) and is_integer(I1b) and is_integer(I2),
        {ok, S} = erlzmq:socket(Context, [sub, {active_pid, Service}]),
        lists:foreach(fun(Endpoint) ->
            ok = erlzmq:connect(S, Endpoint)
        end, EndpointL),
        NameLookup = lists:foldl(fun({NameInternal, NameExternal}, DD) ->
            NameExternalBin = erlang:list_to_binary(NameExternal),
            ok = erlzmq:setsockopt(S, subscribe, NameExternalBin),
            dict:append(NameExternalBin, Prefix ++ NameInternal, DD)
        end, dict:new(), NamePairL),
        NameL = dict:fetch_keys(NameLookup),
        NameMax = lists:foldl(fun(N, M) ->
            erlang:max(erlang:byte_size(N), M)
        end, 0, NameL),
        dict:store(S, {subscribe, {NameMax,
                                   binary:compile_pattern(NameL),
                                   NameLookup}}, D)
    end, ReceivesZMQ1, SubscribeL),
    Request = lists:foldl(fun({outbound,
                               {[I1 | _] = Name,
                                [[I2 | _] | _] = EndpointL}}, D) ->
        true = is_integer(I1) and is_integer(I2),
        cloudi_service:subscribe(Dispatcher, Name),
        {ok, S} = erlzmq:socket(Context, [req, {active_pid, Service}]),
        lists:foreach(fun(Endpoint) ->
            ok = erlzmq:bind(S, Endpoint)
        end, EndpointL),
        cloudi_x_trie:store(Prefix ++ Name, S, D)
    end, cloudi_x_trie:new(), RequestL),
    ReceivesZMQ3 = lists:foldl(fun({inbound,
                                    {[I1 | _] = Name,
                                     [[I2 | _] | _] = EndpointL}}, D) ->
        true = is_integer(I1) and is_integer(I2),
        {ok, S} = erlzmq:socket(Context, [rep, {active_pid, Service}]),
        lists:foreach(fun(Endpoint) ->
            ok = erlzmq:connect(S, Endpoint)
        end, EndpointL),
        dict:store(S, {reply, Prefix ++ Name}, D)
    end, ReceivesZMQ2, ReplyL),
    Push = lists:foldl(fun({push,
                            {[I1 | _] = Name,
                             [[I2 | _] | _] = EndpointL}}, D) ->
        true = is_integer(I1) and is_integer(I2),
        cloudi_service:subscribe(Dispatcher, Name),
        {ok, S} = erlzmq:socket(Context, [push, {active_pid, Service}]),
        lists:foreach(fun(Endpoint) ->
            ok = erlzmq:bind(S, Endpoint)
        end, EndpointL),
        cloudi_x_trie:store(Prefix ++ Name, S, D)
    end, cloudi_x_trie:new(), PushL),
    ReceivesZMQ4 = lists:foldl(fun({pull,
                                    {[I1 | _] = Name,
                                     [[I2 | _] | _] = EndpointL}}, D) ->
        true = is_integer(I1) and is_integer(I2),
        {ok, S} = erlzmq:socket(Context, [pull, {active_pid, Service}]),
        lists:foreach(fun(Endpoint) ->
            ok = erlzmq:connect(S, Endpoint)
        end, EndpointL),
        dict:store(S, {pull, Prefix ++ Name}, D)
    end, ReceivesZMQ3, PullL),
    {ok, #state{context = Context,
                endian = Endian,
                process_metadata = ProcessMetaData,
                publish = Publish,
                request = Request,
                push = Push,
                receives = ReceivesZMQ4}}.

cloudi_service_handle_request(Type, Name, Pattern, RequestInfo, Request,
                              Timeout, _Priority, TransId, Pid,
                              #state{process_metadata = ProcessMetaData,
                                     endian = Endian,
                                     publish = PublishZMQ,
                                     request = RequestZMQ,
                                     push = PushZMQ,
                                     receives = ReceivesZMQ} = State,
                              Dispatcher) ->
    true = is_binary(Request),
    Outgoing = outgoing(ProcessMetaData, Endian, RequestInfo, Request),
    case cloudi_x_trie:find(Pattern, PublishZMQ) of
        {ok, PublishL} ->
            lists:foreach(fun({NameZMQ, S}) ->
                ok = erlzmq:send(S, <<NameZMQ/binary, Outgoing/binary>>)
            end, PublishL);
        error ->
            ok
    end,
    case cloudi_x_trie:find(Pattern, PushZMQ) of
        {ok, PushS} ->
            ok = erlzmq:send(PushS, Outgoing);
        error ->
            ok
    end,
    case cloudi_x_trie:find(Pattern, RequestZMQ) of
        {ok, RequestS} ->
            ok = erlzmq:send(RequestS, Outgoing),
            F = fun(ResponseInfo, Response) ->
                cloudi_service:return_nothrow(Dispatcher, Type, Name, Pattern,
                                              ResponseInfo, Response,
                                              Timeout, TransId, Pid)
            end,
            {noreply, State#state{receives = dict:store(RequestS,
                                                        {request, F},
                                                        ReceivesZMQ)}};
        error ->
            % successful publish operations don't need to store a response
            % erroneous synchronous operations will get a timeout
            {reply, <<>>, State}
    end.

cloudi_service_handle_info({zmq, S, Incoming, _},
                           #state{process_metadata = ProcessMetaData,
                                  endian = Endian,
                                  receives = ReceivesZMQ,
                                  reply_replies = ReplyReplies} = State,
                           Dispatcher) ->
    case dict:find(S, ReceivesZMQ) of
        {ok, {request, F}} ->
            {ResponseInfo, Response} = incoming(ProcessMetaData, Endian,
                                                Incoming),
            F(ResponseInfo, Response),
            {noreply, State#state{receives = dict:erase(S, ReceivesZMQ)}};
        {ok, {reply, Name}} ->
            {RequestInfo, Request} = incoming(ProcessMetaData, Endian,
                                              Incoming),
            case cloudi_service:send_async_active(Dispatcher, Name,
                                                  RequestInfo, Request,
                                                  undefined, undefined) of
                {ok, TransId} ->
                    {noreply,
                     State#state{reply_replies = dict:store(TransId, S,
                                                            ReplyReplies)}};
                {error, _} ->
                    ok = erlzmq:send(S, <<>>),
                    {noreply, State}
            end;
        {ok, {pull, Name}} ->
            {RequestInfo, Request} = incoming(ProcessMetaData, Endian,
                                              Incoming),
            cloudi_service:send_async(Dispatcher, Name,
                                      RequestInfo, Request,
                                      undefined, undefined),
            {noreply, State};
        {ok, {subscribe, {Max, Pattern, Lookup}}} ->
            {0, Length} = binary:match(Incoming, Pattern,
                                       [{scope, {0, Max}}]),
            {NameZMQ, IncomingRest} = erlang:split_binary(Incoming, Length),
            {RequestInfo, Request} = incoming(ProcessMetaData, Endian,
                                              IncomingRest),
            lists:foreach(fun(Name) ->
                cloudi_service:send_async(Dispatcher, Name,
                                          RequestInfo, Request,
                                          undefined, undefined)
            end, dict:fetch(NameZMQ, Lookup)),
            {noreply, State}
    end;

cloudi_service_handle_info({'return_async_active', _Name, _Pattern,
                            ResponseInfo, Response,
                            _Timeout, TransId},
                           #state{process_metadata = ProcessMetaData,
                                  endian = Endian,
                                  reply_replies = ReplyReplies} = State,
                           _Dispatcher) ->
    true = is_binary(Response),
    Outgoing = outgoing(ProcessMetaData, Endian, ResponseInfo, Response),
    S = dict:fetch(TransId, ReplyReplies),
    ok = erlzmq:send(S, Outgoing),
    {noreply, State#state{reply_replies = dict:erase(TransId, ReplyReplies)}};

cloudi_service_handle_info({'timeout_async_active', TransId},
                           #state{reply_replies = ReplyReplies} = State,
                           _Dispatcher) ->
    S = dict:fetch(TransId, ReplyReplies),
    ok = erlzmq:send(S, <<>>),
    {noreply, State#state{reply_replies = dict:erase(TransId, ReplyReplies)}};

cloudi_service_handle_info(Request, State, _) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_service_terminate(_, undefined) ->
    ok;
cloudi_service_terminate(_, #state{context = Context,
                                   publish = Publish,
                                   request = Request,
                                   push = Push,
                                   receives = ReceivesZMQ}) ->
    cloudi_x_trie:foreach(fun(_, L) ->
        lists:foreach(fun({_, S}) ->
            erlzmq:close(S)
        end, L)
    end, Publish),
    cloudi_x_trie:foreach(fun(_, S) ->
        erlzmq:close(S)
    end, Request),
    cloudi_x_trie:foreach(fun(_, S) ->
        erlzmq:close(S)
    end, Push),
    dict:map(fun(S, _) ->
        erlzmq:close(S)
    end, ReceivesZMQ),
    erlzmq:term(Context),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

outgoing(true, Endian, RequestInfo, Request) ->
    MetaData = if
        is_binary(RequestInfo) ->
            RequestInfo;
        true ->
            cloudi_service:request_info_key_value_new(RequestInfo)
    end,
    MetaDataSize = erlang:byte_size(MetaData),
    if
        Endian =:= big ->
            <<MetaDataSize:32/unsigned-integer-big,
              MetaData/binary, Request/binary>>;
        Endian =:= little ->
            <<MetaDataSize:32/unsigned-integer-little,
              MetaData/binary, Request/binary>>;
        Endian =:= native ->
            <<MetaDataSize:32/unsigned-integer-native,
              MetaData/binary, Request/binary>>
    end;
outgoing(false, _Endian, _RequestInfo, Request) ->
    Request.

incoming(_, _, <<>>) ->
    {<<>>, <<>>};
incoming(true, big, Incoming) ->
    <<MetaDataSize:32/unsigned-integer-big,
      IncomingRest/binary>> = Incoming,
    incoming_metadata_split(MetaDataSize, IncomingRest);
incoming(true, little, Incoming) ->
    <<MetaDataSize:32/unsigned-integer-little,
      IncomingRest/binary>> = Incoming,
    incoming_metadata_split(MetaDataSize, IncomingRest);
incoming(true, native, Incoming) ->
    <<MetaDataSize:32/unsigned-integer-native,
      IncomingRest/binary>> = Incoming,
    incoming_metadata_split(MetaDataSize, IncomingRest);
incoming(false, _Endian, Incoming) ->
    {<<>>, Incoming}.

-compile({inline, [{incoming_metadata_split, 2}]}).
incoming_metadata_split(MetaDataSize, IncomingRest) ->
    Size = erlang:byte_size(IncomingRest),
    RequestInfo = erlang:binary_part(IncomingRest, {0, MetaDataSize}),
    Request = erlang:binary_part(IncomingRest, {Size, MetaDataSize - Size}),
    {RequestInfo, Request}.

