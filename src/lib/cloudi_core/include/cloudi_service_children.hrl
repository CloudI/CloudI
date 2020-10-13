%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%%
%%% MIT License
%%%
%%% Copyright (c) 2014-2020 Michael Truog <mjtruog at protonmail dot com>
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
%%%------------------------------------------------------------------------

% for advanced cloudi_service behaviour implementations that
% create multiple child processes that need to send service requests with the
% lowest latency possible

% to use these functions, the module implementing the cloudi_service 
% behaviour needs to provide the following variables
% (assigned normally within cloudi_service_init/4):
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

send_async_minimal(Dispatcher, Name, RequestInfo, Request,
                   Timeout, Self) ->
    send_async_minimal(Dispatcher, Name, RequestInfo, Request,
                       Timeout, undefined, Self).

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

send_sync_minimal(Dispatcher, Name, RequestInfo, Request,
                  Timeout, Self) ->
    send_sync_minimal(Dispatcher, Name, RequestInfo, Request,
                      Timeout, undefined, Self).

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

recv_asyncs_minimal(Timeout, TransIdList) ->
    recv_asyncs_minimal([], erlang:length(TransIdList), TransIdList, Timeout).

recv_asyncs_minimal(L, 0, TransIdList, _Timeout) ->
    Result = [begin
        case lists:keyfind(TransId, 3, L) of
            false ->
                {<<>>, <<>>, TransId};
            {_, _, _} = Entry ->
                Entry
        end
    end || TransId <- TransIdList],
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

