%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2014-2015, Michael Truog <mjtruog at gmail dot com>
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
%%%------------------------------------------------------------------------

% for advanced cloudi_service behaviour implementations that
% create multiple child processes that need to send service requests with the
% lowest latency possible

% to use these functions, the module implementing the cloudi_service 
% behaviour needs to provide the following variables
% (assigned normally within cloudi_service_init/3):
% Dispatcher = cloudi_service:dispatcher(Dispatcher),
% TimeoutSync = cloudi_service:timeout_sync(Dispatcher),
% TimeoutAsync = cloudi_service:timeout_async(Dispatcher),

-compile({nowarn_unused_function,
          [{send_async_minimal, 6},
           {send_async_minimal, 7},
           {send_sync_minimal, 6},
           {send_sync_minimal, 7},
           {recv_async_minimal, 2},
           {recv_asyncs_minimal, 2},
           {recv_asyncs_minimal, 4}]}).

-spec send_async_minimal(Dispatcher :: cloudi_service:dispatcher(),
                         Name :: cloudi_service:service_name(),
                         RequestInfo :: any(),
                         Request :: any(),
                         Timeout :: cloudi_service:timeout_value_milliseconds(),
                         Self :: pid()) ->
    {ok, TransId :: cloudi_service:trans_id()} |
    {error, timeout}.

send_async_minimal(Dispatcher, Name, RequestInfo, Request,
                   Timeout, Self) ->
    send_async_minimal(Dispatcher, Name, RequestInfo, Request,
                       Timeout, undefined, Self).

-spec send_async_minimal(Dispatcher :: cloudi_service:dispatcher(),
                         Name :: cloudi_service:service_name(),
                         RequestInfo :: any(),
                         Request :: any(),
                         Timeout :: cloudi_service:timeout_value_milliseconds(),
                         Destination :: cloudi_service:pattern_pid() |
                                        undefined,
                         Self :: pid()) ->
    {ok, TransId :: cloudi_service:trans_id()} |
    {error, timeout}.

send_async_minimal(Dispatcher, Name, RequestInfo, Request,
                   Timeout, Destination, Self) ->
    Dispatcher ! {'cloudi_service_send_async_minimal',
                  Name, RequestInfo, Request,
                  Timeout, Destination, Self},
    receive
        {'cloudi_service_send_async_minimal', timeout} ->
            {error, timeout};
        {'cloudi_service_send_async_minimal', TransId} ->
            {ok, TransId}
    after
        Timeout ->
            {error, timeout}
    end.

-spec send_sync_minimal(Dispatcher :: cloudi_service:dispatcher(),
                        Name :: cloudi_service:service_name(),
                        RequestInfo :: any(),
                        Request :: any(),
                        Timeout :: cloudi_service:timeout_value_milliseconds(),
                        Self :: pid()) ->
    {ok, ResponseInfo :: any(), Response :: any()} |
    {error, timeout}.

send_sync_minimal(Dispatcher, Name, RequestInfo, Request,
                  Timeout, Self) ->
    send_sync_minimal(Dispatcher, Name, RequestInfo, Request,
                      Timeout, undefined, Self).

-spec send_sync_minimal(Dispatcher :: cloudi_service:dispatcher(),
                        Name :: cloudi_service:service_name(),
                        RequestInfo :: any(),
                        Request :: any(),
                        Timeout :: cloudi_service:timeout_value_milliseconds(),
                        Destination :: cloudi_service:pattern_pid() |
                                       undefined,
                        Self :: pid()) ->
    {ok, ResponseInfo :: any(), Response :: any()} |
    {error, timeout}.

send_sync_minimal(Dispatcher, Name, RequestInfo, Request,
                  Timeout, Destination, Self) ->
    Dispatcher ! {'cloudi_service_send_sync_minimal',
                  Name, RequestInfo, Request,
                  Timeout, Destination, Self},
    receive
        {'cloudi_service_send_sync_minimal', timeout} ->
            {error, timeout};
        {'cloudi_service_send_sync_minimal', TransId} ->
            receive
                {'cloudi_service_return_sync',
                 _Name, _Pattern, <<>>, <<>>,
                 _OldTimeout, TransId, Self} ->
                    {error, timeout};
                {'cloudi_service_return_sync',
                 _Name, _Pattern, ResponseInfo, Response,
                 _OldTimeout, TransId, Self} ->
                    {ok, ResponseInfo, Response}
            after
                Timeout ->
                    {error, timeout}
            end
    after
        Timeout ->
            {error, timeout}
    end.

-spec recv_async_minimal(Timeout :: cloudi_service:timeout_value_milliseconds(),
                         TransId :: cloudi_service:trans_id()) ->
    {ok, ResponseInfo :: any(), Response :: any()} |
    {error, timeout}.

recv_async_minimal(Timeout, TransId) ->
    receive
        {'cloudi_service_return_async',
         _Name, _Pattern, <<>>, <<>>,
         _OldTimeout, TransId, _Self} ->
            {error, timeout};
        {'cloudi_service_return_async',
         _Name, _Pattern, ResponseInfo, Response,
         _OldTimeout, TransId, _Self} ->
            {ok, ResponseInfo, Response}
    after
        Timeout ->
            {error, timeout}
    end.

-spec recv_asyncs_minimal(Timeout :: cloudi:timeout_value_milliseconds(),
                          TransIdList :: list(cloudi:trans_id())) ->
    {ok, list({ResponseInfo :: any(),
               Response :: any(), TransId :: cloudi:trans_id()})} |
    {error, timeout}.

recv_asyncs_minimal(Timeout, TransIdList) ->
    recv_asyncs_minimal([], erlang:length(TransIdList), TransIdList, Timeout).

recv_asyncs_minimal(L, 0, TransIdList, _Timeout) ->
    Result = lists:map(fun(TransId) ->
        case lists:keyfind(TransId, 3, L) of
            false ->
                {<<>>, <<>>, TransId};
            {_, _, _} = Entry ->
                Entry
        end
    end, TransIdList),
    {ok, Result};
recv_asyncs_minimal(L, I, TransIdList, Timeout) ->
    receive
        {'cloudi_service_return_async',
         _Name, _Pattern, ResponseInfo, Response,
         _OldTimeout, TransId, _Self} ->
            recv_asyncs_minimal([{ResponseInfo, Response, TransId} | L],
                                I - 1, TransIdList, Timeout)
    after
        Timeout ->
            if
                L == [] ->
                    {error, timeout};
                true ->
                    recv_asyncs_minimal(L, 0, TransIdList, Timeout)
            end
    end.

