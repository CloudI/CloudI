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
% Context = create_context(Dispatcher),

-compile({nowarn_unused_function,
          [{create_context, 1},
           {send_async_minimal, 6},
           {send_async_minimal, 7},
           {send_sync_minimal, 6},
           {send_sync_minimal, 7},
           {recv_asyncs_minimal, 2},
           {recv_asyncs_minimal, 4}]}).

-spec create_context(Dispatcher :: cloudi_service:dispatcher()) ->
    cloudi:context().

create_context(Dispatcher) ->
    % use a special Context object to minimize latency/memory consumption
    % and avoid timer usage for lazy destination refresh methods
    ContextOptions = cloudi_service:context_options(Dispatcher),
    cloudi:new([{groups_static, true} |
                lists:keydelete(groups, 1, ContextOptions)]).

-spec send_async_minimal(Dispatcher :: cloudi_service:dispatcher(),
                         Context :: cloudi:context(),
                         Name :: cloudi:service_name(),
                         RequestInfo :: any(),
                         Request :: any(),
                         Self :: pid()) ->
    {{ok, TransId :: <<_:128>>}, NewContext :: cloudi:context()}.

send_async_minimal(Dispatcher, Context, Name,
                   RequestInfo, Request, Self) ->
    Timeout = cloudi:timeout_async(Context),
    Priority = cloudi:priority_default(Context),
    {TransId, NewContext} = cloudi:trans_id(Context),
    Dispatcher ! {'cloudi_service_forward_async_retry',
                  Name, RequestInfo, Request,
                  Timeout, Priority, TransId, Self},
    {{ok, TransId}, NewContext}.

-spec send_async_minimal(Dispatcher :: cloudi_service:dispatcher(),
                         Context :: cloudi:context(),
                         Name :: cloudi:service_name(),
                         RequestInfo :: any(),
                         Request :: any(),
                         Destination :: cloudi:pattern_pid() | undefined,
                         Self :: pid()) ->
    {{ok, TransId :: <<_:128>>}, NewContext :: cloudi:context()}.

send_async_minimal(Dispatcher, Context, Name,
                   RequestInfo, Request, undefined, Self) ->
    send_async_minimal(Dispatcher, Context, Name,
                       RequestInfo, Request, Self);
send_async_minimal(_Dispatcher, Context, Name,
                   RequestInfo, Request, {Pattern, Pid}, Self) ->
    Timeout = cloudi:timeout_async(Context),
    Priority = cloudi:priority_default(Context),
    {TransId, NewContext} = cloudi:trans_id(Context),
    Pid ! {'cloudi_service_send_async',
           Name, Pattern, RequestInfo, Request,
           Timeout, Priority, TransId, Self},
    {{ok, TransId}, NewContext}.

-spec send_sync_minimal(Dispatcher :: cloudi_service:dispatcher(),
                        Context :: cloudi:context(),
                        Name :: cloudi:service_name(),
                        RequestInfo :: any(),
                        Request :: any(),
                        Self :: pid()) ->
    {{ok, ResponseInfo :: any(), Response :: any()} |
     {error, timeout}, NewContext :: cloudi:context()}.

send_sync_minimal(Dispatcher, Context, Name,
                  RequestInfo, Request, Self) ->
    Timeout = cloudi:timeout_sync(Context),
    Priority = cloudi:priority_default(Context),
    {TransId, NewContext} = cloudi:trans_id(Context),
    Dispatcher ! {'cloudi_service_forward_sync_retry',
                  Name, RequestInfo, Request,
                  Timeout, Priority, TransId, Self},
    receive
        {'cloudi_service_return_sync',
         _Name, _Pattern, <<>>, <<>>,
         _OldTimeout, TransId, Self} ->
            {{error, timeout}, NewContext};
        {'cloudi_service_return_sync',
         _Name, _Pattern, ResponseInfo, Response,
         _OldTimeout, TransId, Self} ->
            {{ok, ResponseInfo, Response}, NewContext}
    after
        Timeout ->
            {{error, timeout}, NewContext}
    end.

-spec send_sync_minimal(Dispatcher :: cloudi_service:dispatcher(),
                        Context :: cloudi:context(),
                        Name :: cloudi:service_name(),
                        RequestInfo :: any(),
                        Request :: any(),
                        Destination :: cloudi:pattern_pid() | undefined,
                        Self :: pid()) ->
    {{ok, ResponseInfo :: any(), Response :: any()} |
     {error, timeout}, NewContext :: cloudi:context()}.

send_sync_minimal(Dispatcher, Context, Name,
                  RequestInfo, Request, undefined, Self) ->
    send_sync_minimal(Dispatcher, Context, Name,
                      RequestInfo, Request, Self);
send_sync_minimal(_Dispatcher, Context, Name,
                  RequestInfo, Request, {Pattern, Pid}, Self) ->
    Timeout = cloudi:timeout_sync(Context),
    Priority = cloudi:priority_default(Context),
    {TransId, NewContext} = cloudi:trans_id(Context),
    Pid ! {'cloudi_service_send_sync',
           Name, Pattern, RequestInfo, Request,
           Timeout, Priority, TransId, Self},
    receive
        {'cloudi_service_return_sync',
         _Name, _Pattern, <<>>, <<>>,
         _OldTimeout, TransId, Self} ->
            {{error, timeout}, NewContext};
        {'cloudi_service_return_sync',
         _Name, _Pattern, ResponseInfo, Response,
         _OldTimeout, TransId, Self} ->
            {{ok, ResponseInfo, Response}, NewContext}
    after
        Timeout ->
            {{error, timeout}, NewContext}
    end.

-spec recv_asyncs_minimal(Context :: cloudi:context(),
                          TransIdList :: list(cloudi:trans_id())) ->
    {{ok, list({ResponseInfo :: any(),
                Response :: any(), TransId :: cloudi:trans_id()})} |
     {error, timeout}, NewContext :: cloudi:context()}.

recv_asyncs_minimal(Context, TransIdList) ->
    {recv_asyncs_minimal([{<<>>, <<>>, TransId} ||
                          TransId <- TransIdList], [],
                         erlang:length(TransIdList),
                         cloudi:timeout_sync(Context)), Context}.

recv_asyncs_minimal([], L, 0, _Timeout) ->
    {ok, lists:reverse(L)};
recv_asyncs_minimal([], L, I, Timeout)
    when Timeout >= 500 ->
    receive after 500 -> ok end,
    recv_asyncs_minimal(lists:reverse(L), [], I, Timeout - 500);
recv_asyncs_minimal([], L, I, _Timeout) ->
    if
        erlang:length(L) == I ->
            {error, timeout};
        true ->
            {ok, lists:reverse(L)}
    end;
recv_asyncs_minimal([{<<>>, <<>>, TransId} = Entry | Results], L,
                    I, Timeout) ->
    receive
        {'cloudi_service_return_async',
         _Name, _Pattern, <<>>, <<>>,
         _OldTimeout, TransId, _Self} ->
            recv_asyncs_minimal(Results,
                                [Entry | L],
                                I - 1, Timeout);
        {'cloudi_service_return_async',
         _Name, _Pattern, ResponseInfo, Response,
         _OldTimeout, TransId, _Self} ->
            recv_asyncs_minimal(Results,
                                [{ResponseInfo, Response, TransId} | L],
                                I - 1, Timeout)
    after
        0 ->
            recv_asyncs_minimal(Results, [Entry | L], I, Timeout)
    end;
recv_asyncs_minimal([{_, _, _} = Entry | Results], L, I, Timeout) ->
    recv_asyncs_minimal(Results, [Entry | L], I, Timeout).

