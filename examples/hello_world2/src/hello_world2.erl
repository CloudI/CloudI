%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:

-module(hello_world2).
-behaviour(cloudi_service).

%% cloudi_service callbacks
-export([cloudi_service_init/4,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/3]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").

-record(state,
    {
    }).

cloudi_service_init(_Args, _Prefix, _Timeout, Dispatcher) ->
    cloudi_service:subscribe(Dispatcher, "hello_world2/get"),
    {ok, #state{}}.

cloudi_service_handle_request(_Type, _Name, _Pattern, _RequestInfo, _Request,
                              _Timeout, _Priority, _TransId, _Pid,
                              #state{} = State, _Dispatcher) ->
    {reply, <<"Hello World!">>, State}.

cloudi_service_handle_info(Request, State, _Dispatcher) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_service_terminate(_Reason, _Timeout, #state{}) ->
    ok.
