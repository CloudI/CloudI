%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Router Service==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2014-2016, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2014-2016 Michael Truog
%%% @version 1.5.2 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_router).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloudi_service).

%% external interface

%% cloudi_service callbacks
-export([cloudi_service_init/4,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/3]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").
-include_lib("cloudi_core/include/cloudi_service.hrl").

-define(DEFAULT_ADD_PREFIX,                        true). % to destinations
-define(DEFAULT_VALIDATE_REQUEST_INFO,        undefined).
-define(DEFAULT_VALIDATE_REQUEST,             undefined).
-define(DEFAULT_FAILURES_SOURCE_DIE,              false).
-define(DEFAULT_FAILURES_SOURCE_MAX_COUNT,            2). % see below:
        % (similar to the MaxR configuration value for services)
-define(DEFAULT_FAILURES_SOURCE_MAX_PERIOD,          60). % seconds, see below:
        % (similar to the MaxT configuration value for services)
        % If you want the source service to eventually fail,
        % use the service's MaxT/MaxR as the failures_source_max_period value
        % (e.g., 300/5 == 60 seconds).  Can also use the value 'infinity'
        % to accumulate a failure count indefinitely.

% destinations configuration arguments
-define(DEFAULT_MODE,                       round_robin).
-define(DEFAULT_PARAMETERS_ALLOWED,                true).
-define(DEFAULT_PARAMETERS_STRICT_MATCHING,        true).

-record(destination,
    {
        mode = ?DEFAULT_MODE :: random | round_robin,
        service_names = [] :: list(string()),
        index = 1 :: pos_integer(),
        parameters_allowed = ?DEFAULT_PARAMETERS_ALLOWED
            :: boolean(),
        parameters_strict_matching = ?DEFAULT_PARAMETERS_STRICT_MATCHING
            :: boolean(),
        parameters_selected = [] :: list(pos_integer()),
        length = 1 :: pos_integer()
    }).

-ifdef(ERLANG_OTP_VERSION_16).
-type dict_proxy(_Key, _Value) :: dict().
-else.
-type dict_proxy(Key, Value) :: dict:dict(Key, Value).
-endif.
-record(state,
    {
        validate_request_info :: undefined | fun((any()) -> boolean()),
        validate_request :: undefined | fun((any(), any()) -> boolean()),
        failures_source_die :: boolean(),
        failures_source_max_count :: pos_integer(),
        failures_source_max_period :: infinity | pos_integer(),
        failures_source = dict:new()
            :: dict_proxy(pid(), list(erlang:timestamp())),
        destinations
            :: cloudi_x_trie:cloudi_x_trie() % pattern -> #destination{}
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, Prefix, _Timeout, Dispatcher) ->
    Defaults = [
        {add_prefix,                    ?DEFAULT_ADD_PREFIX},
        {validate_request_info,         ?DEFAULT_VALIDATE_REQUEST_INFO},
        {validate_request,              ?DEFAULT_VALIDATE_REQUEST},
        {failures_source_die,           ?DEFAULT_FAILURES_SOURCE_DIE},
        {failures_source_max_count,     ?DEFAULT_FAILURES_SOURCE_MAX_COUNT},
        {failures_source_max_period,    ?DEFAULT_FAILURES_SOURCE_MAX_PERIOD},
        {destinations,                  []}],
    [AddPrefix,
     ValidateRequestInfo0, ValidateRequest0,
     FailuresSrcDie, FailuresSrcMaxCount, FailuresSrcMaxPeriod,
     DestinationsL] = cloudi_proplists:take_values(Defaults, Args),
    true = is_boolean(AddPrefix),
    ValidateRequestInfo1 = cloudi_args_type:
                           function_optional(ValidateRequestInfo0, 1),
    ValidateRequest1 = cloudi_args_type:
                       function_optional(ValidateRequest0, 2),
    true = is_boolean(FailuresSrcDie),
    true = is_integer(FailuresSrcMaxCount) andalso (FailuresSrcMaxCount > 0),
    true = (FailuresSrcMaxPeriod =:= infinity) orelse
           (is_integer(FailuresSrcMaxPeriod) andalso
            (FailuresSrcMaxPeriod > 0)),
    true = is_list(DestinationsL) andalso
           (erlang:length(DestinationsL) > 0),
    ConfigDefaults = [
        {mode,                       ?DEFAULT_MODE},
        {parameters_allowed,         ?DEFAULT_PARAMETERS_ALLOWED},
        {parameters_strict_matching, ?DEFAULT_PARAMETERS_STRICT_MATCHING},
        {parameters_selected,        []},
        {service_names,              []}],
    Destinations = lists:foldl(fun({PatternSuffix, L}, D) ->
        cloudi_service:subscribe(Dispatcher, PatternSuffix),
        case L of
            [I | _] when is_integer(I) ->
                Names = if
                    AddPrefix =:= true ->
                        [Prefix ++ L];
                    AddPrefix =:= false ->
                        [L]
                end,
                cloudi_x_trie:store(Prefix ++ PatternSuffix,
                                    #destination{service_names = Names}, D);
            [_ | _] ->
                [Mode,
                 ParametersAllowed,
                 ParametersStrictMatching,
                 ParametersSelected,
                 Names0] = cloudi_proplists:take_values(ConfigDefaults, L),
                true = is_atom(Mode) andalso
                       ((Mode =:= random) orelse
                        (Mode =:= round_robin)),
                true = is_boolean(ParametersAllowed),
                true = is_boolean(ParametersStrictMatching),
                true = is_list(ParametersSelected),
                true = ((ParametersSelected == []) orelse
                        ((ParametersSelected /= []) andalso
                         (ParametersAllowed =:= true))),
                true = lists:all(fun(I) -> is_integer(I) andalso I > 0 end,
                                 ParametersSelected),
                true = is_list(Names0),
                Length = erlang:length(Names0),
                true = (Length > 0),
                Names1 = if
                    AddPrefix =:= true ->
                        [Prefix ++ Suffix || Suffix <- Names0];
                    AddPrefix =:= false ->
                        Names0
                end,
                true = ((ParametersAllowed =:= true) orelse
                        ((ParametersAllowed =:= false) andalso
                         (lists:any(fun cloudi_x_trie:is_pattern/1,
                                    Names1) =:= false))),
                Destination = #destination{mode = Mode,
                                           parameters_allowed =
                                               ParametersAllowed,
                                           parameters_strict_matching =
                                               ParametersStrictMatching,
                                           parameters_selected =
                                               ParametersSelected,
                                           service_names = Names1,
                                           length = Length},
                cloudi_x_trie:store(Prefix ++ PatternSuffix, Destination, D)
        end
    end, cloudi_x_trie:new(), DestinationsL),
    {ok, #state{validate_request_info = ValidateRequestInfo1,
                validate_request = ValidateRequest1,
                failures_source_die = FailuresSrcDie,
                failures_source_max_count = FailuresSrcMaxCount,
                failures_source_max_period = FailuresSrcMaxPeriod,
                destinations = Destinations}}.

cloudi_service_handle_request(_Type, Name, Pattern, RequestInfo, Request,
                              Timeout, Priority, _TransId, SrcPid,
                              #state{validate_request_info = RequestInfoF,
                                     validate_request = RequestF,
                                     destinations = Destinations} = State,
                              _Dispatcher) ->
    case validate(RequestInfoF, RequestF,
                  RequestInfo, Request) of
        true ->
            case cloudi_x_trie:find(Pattern, Destinations) of
                {ok, #destination{} = Destination} ->
                    {NextName, NewDestination} = destination_pick(Destination),
                    Parameters = cloudi_service_name:parse(Name,
                                                                   Pattern),
                    case name_parameters(NextName, Parameters,
                                         NewDestination) of
                        {ok, NewName} ->
                            NewDestinations = cloudi_x_trie:
                                              store(Pattern,
                                                    NewDestination,
                                                    Destinations),
                            {forward, NewName, RequestInfo, Request,
                             Timeout, Priority,
                             State#state{destinations = NewDestinations}};
                        {error, Reason} ->
                            ?LOG_ERROR("(~p -> ~p) error: ~p",
                                       [Name, NextName, Reason]),
                            request_failed(SrcPid, State)
                    end;
                error ->
                    request_failed(SrcPid, State)
            end;
        false ->
            request_failed(SrcPid, State)
    end.

cloudi_service_handle_info({'DOWN', _MonitorRef, process, Pid, _Info},
                           #state{failures_source_die = FailuresSrcDie,
                                  failures_source = FailuresSrc} = State,
                           _Dispatcher) ->
    NewFailuresSrc = if
        FailuresSrcDie =:= true ->
            dict:erase(Pid, FailuresSrc);
        FailuresSrcDie =:= false ->
            FailuresSrc
    end,
    {noreply, State#state{failures_source = NewFailuresSrc}};

cloudi_service_handle_info(Request, State, _Dispatcher) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_service_terminate(_Reason, _Timeout, #state{}) ->
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

destination_pick(#destination{service_names = [Name]} = Destination) ->
    {Name, Destination};
destination_pick(#destination{mode = round_robin,
                              service_names = Names,
                              index = Index,
                              length = Length} = Destination) ->
    NewIndex = if
        Index == Length ->
            1;
        true ->
            Index + 1
    end,
    Name = lists:nth(Index, Names),
    {Name, Destination#destination{index = NewIndex}};
destination_pick(#destination{mode = random,
                              service_names = Names,
                              length = Length} = Destination) ->
    Index = cloudi_x_quickrand:uniform(Length),
    Name = lists:nth(Index, Names),
    {Name, Destination#destination{index = Index}}.

name_parameters(Name, [], #destination{}) ->
    {ok, Name};
name_parameters(_, [_ | _],
                #destination{parameters_allowed = false}) ->
    {error, parameters_not_allowed};
name_parameters(Pattern, Parameters,
                #destination{parameters_strict_matching =
                                 ParametersStrictMatching,
                             parameters_selected =
                                 ParametersSelected}) ->
    cloudi_service_name:new(Pattern, Parameters, ParametersSelected,
                                    ParametersStrictMatching).

validate_f_return(Value) when is_boolean(Value) ->
    Value.

validate(undefined, undefined, _, _) ->
    true;
validate(undefined, RF, RInfo, R) ->
    validate_f_return(RF(RInfo, R));
validate(RInfoF, undefined, RInfo, _) ->
    validate_f_return(RInfoF(RInfo));
validate(RInfoF, RF, RInfo, R) ->
    validate_f_return(RInfoF(RInfo)) andalso validate_f_return(RF(RInfo, R)).

request_failed(SrcPid,
               #state{failures_source_die = FailuresSrcDie,
                      failures_source_max_count = FailuresSrcMaxCount,
                      failures_source_max_period = FailuresSrcMaxPeriod,
                      failures_source = FailuresSrc} = State) ->
    {DeadSrc, NewFailuresSrc} = failure(FailuresSrcDie,
                                        FailuresSrcMaxCount,
                                        FailuresSrcMaxPeriod,
                                        SrcPid, FailuresSrc),
    if
        DeadSrc =:= true ->
            {noreply,
             State#state{failures_source = NewFailuresSrc}};
        DeadSrc =:= false ->
            {reply, <<>>,
             State#state{failures_source = NewFailuresSrc}}
    end.

failure(false, _, _, _, Failures) ->
    {false, Failures};
failure(true, MaxCount, MaxPeriod, Pid, Failures) ->
    case erlang:is_process_alive(Pid) of
        true ->
            SecondsNow = cloudi_timestamp:seconds(),
            case dict:find(Pid, Failures) of
                {ok, FailureList} ->
                    failure_check(SecondsNow, FailureList,
                                  MaxCount, MaxPeriod, Pid, Failures);
                error ->
                    erlang:monitor(process, Pid),
                    failure_check(SecondsNow, [],
                                  MaxCount, MaxPeriod, Pid, Failures)
            end;
        false ->
            {true, Failures}
    end.

failure_store(FailureList, MaxCount, Pid, Failures) ->
    NewFailures = dict:store(Pid, FailureList, Failures),
    if
        erlang:length(FailureList) == MaxCount ->
            failure_kill(Pid),
            {true, NewFailures};
        true ->
            {false, NewFailures}
    end.

failure_check(SecondsNow, FailureList, MaxCount, infinity, Pid, Failures) ->
    failure_store([SecondsNow | FailureList], MaxCount, Pid, Failures);
failure_check(SecondsNow, FailureList, MaxCount, MaxPeriod, Pid, Failures) ->
    NewFailureList = cloudi_timestamp:seconds_filter(FailureList,
                                                     SecondsNow, MaxPeriod),
    failure_store([SecondsNow | NewFailureList], MaxCount, Pid, Failures).

failure_kill(Pid) ->
    erlang:exit(Pid, cloudi_service_router).
