%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Router Service==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2014-2018 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2014-2018 Michael Truog
%%% @version 1.7.5 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_router).
-author('mjtruog at protonmail dot com').

-behaviour(cloudi_service).

%% external interface

%% cloudi_service callbacks
-export([cloudi_service_init/4,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/3]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").
-include_lib("cloudi_core/include/cloudi_service.hrl").

-define(DEFAULT_SSH,                    undefined).
        % Enable a ssh server for receiving remote service requests.
        % The user_dir and system_dir options are required options.
        % The ip option and port option will likely need to be set
        % based on your network configuration.
        %
        % Example setup from scratch may be like:
        % $ ssh-keygen -t rsa -f system_dir/ssh_host_rsa_key
        % $ ssh-keygen -t rsa -f user_dir/id_rsa
        % $ cat user_dir/id_rsa.pub >> user_dir/authorized_keys
        % $ chmod 600 user_dir/authorized_keys
        %
        % (user_dir could be the path to ${HOME}/.ssh and
        %  system_dir could be /etc/ssh
        %  if permissions and configuration files are correct there)
-define(DEFAULT_ADD_PREFIX,                  true). % to destinations
-define(DEFAULT_VALIDATE_REQUEST_INFO,  undefined).
-define(DEFAULT_VALIDATE_REQUEST,       undefined).
-define(DEFAULT_FAILURES_SOURCE_DIE,        false).
-define(DEFAULT_FAILURES_SOURCE_MAX_COUNT,      2). % see below:
        % (similar to the MaxR configuration value for services)
-define(DEFAULT_FAILURES_SOURCE_MAX_PERIOD,    60). % seconds, see below:
        % (similar to the MaxT configuration value for services)
        % If you want the source service to eventually fail,
        % use the service's MaxT/MaxR as the failures_source_max_period value
        % (e.g., 300/5 == 60 seconds).  Can also use the value 'infinity'
        % to accumulate a failure count indefinitely.

% destinations configuration arguments
-define(DEFAULT_REMOTE,                 undefined).
        % Should the service request be routed to a remote host?
        % Takes a list of options, with only the host_name option required.
        % Defaults are taken from the ssh server configuration,
        % if it was configured.
-define(DEFAULT_MODE,                 round_robin).
-define(DEFAULT_PARAMETERS_ALLOWED,          true).
-define(DEFAULT_PARAMETERS_STRICT_MATCHING,  true).

-record(destination,
    {
        remote :: undefined | cloudi_service_router_client:state(),
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

-record(state,
    {
        ssh :: undefined | cloudi_service_router_ssh_server:state(),
        validate_request_info :: undefined | fun((any()) -> boolean()),
        validate_request :: undefined | fun((any(), any()) -> boolean()),
        failures_source_die :: boolean(),
        failures_source_max_count :: pos_integer(),
        failures_source_max_period :: infinity | pos_integer(),
        failures_source = #{}
            :: #{pid() := list(cloudi_timestamp:seconds_monotonic())},
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
        {ssh,                           ?DEFAULT_SSH},
        {add_prefix,                    ?DEFAULT_ADD_PREFIX},
        {validate_request_info,         ?DEFAULT_VALIDATE_REQUEST_INFO},
        {validate_request,              ?DEFAULT_VALIDATE_REQUEST},
        {failures_source_die,           ?DEFAULT_FAILURES_SOURCE_DIE},
        {failures_source_max_count,     ?DEFAULT_FAILURES_SOURCE_MAX_COUNT},
        {failures_source_max_period,    ?DEFAULT_FAILURES_SOURCE_MAX_PERIOD},
        {destinations,                  []}],
    [SSHOptions, AddPrefix,
     ValidateRequestInfo0, ValidateRequest0,
     FailuresSrcDie, FailuresSrcMaxCount, FailuresSrcMaxPeriod,
     DestinationsL] = cloudi_proplists:take_values(Defaults, Args),
    Environment = cloudi_environment:lookup(),
    SSH = cloudi_service_router_ssh_server:new(SSHOptions,
                                               Environment,
                                               Dispatcher),
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
    true = is_list(DestinationsL),
    ConfigDefaults = [
        {remote,                        ?DEFAULT_REMOTE},
        {mode,                          ?DEFAULT_MODE},
        {parameters_allowed,            ?DEFAULT_PARAMETERS_ALLOWED},
        {parameters_strict_matching,    ?DEFAULT_PARAMETERS_STRICT_MATCHING},
        {parameters_selected,           []},
        {service_names,                 []}],
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
                [RemoteOptions,
                 Mode,
                 ParametersAllowed,
                 ParametersStrictMatching,
                 ParametersSelected,
                 Names0] = cloudi_proplists:take_values(ConfigDefaults, L),
                Remote = cloudi_service_router_client:new(RemoteOptions,
                                                          Environment,
                                                          SSH),
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
                         (lists:any(fun cloudi_service_name:pattern/1,
                                    Names1) =:= false))),
                Destination = #destination{remote = Remote,
                                           mode = Mode,
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
    {ok, #state{ssh = SSH,
                validate_request_info = ValidateRequestInfo1,
                validate_request = ValidateRequest1,
                failures_source_die = FailuresSrcDie,
                failures_source_max_count = FailuresSrcMaxCount,
                failures_source_max_period = FailuresSrcMaxPeriod,
                destinations = Destinations}}.

cloudi_service_handle_request(Type, Name, Pattern, RequestInfo, Request,
                              Timeout, Priority, TransId, SrcPid,
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
                    Parameters = cloudi_service_name:parse(Name, Pattern),
                    case name_parameters(NextName, Parameters,
                                         NewDestination) of
                        {ok, NewName} ->
                            NewDestinations = cloudi_x_trie:
                                              store(Pattern,
                                                    NewDestination,
                                                    Destinations),
                            forward(Type, Name, Pattern, NewName,
                                    RequestInfo, Request,
                                    Timeout, Priority, TransId, SrcPid,
                                    NewDestination,
                                    State#state{
                                        destinations = NewDestinations});
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
            maps:remove(Pid, FailuresSrc);
        FailuresSrcDie =:= false ->
            FailuresSrc
    end,
    {noreply, State#state{failures_source = NewFailuresSrc}};

cloudi_service_handle_info(Request, State, _Dispatcher) ->
    {stop, cloudi_string:format("Unknown info \"~w\"", [Request]), State}.

cloudi_service_terminate(_Reason, _Timeout, undefined) ->
    ok;
cloudi_service_terminate(_Reason, _Timeout,
                         #state{ssh = SSH}) ->
    ok = cloudi_service_router_ssh_server:destroy(SSH),
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

forward(_, _, _, NewName, RequestInfo, Request,
        Timeout, Priority, _, _,
        #destination{remote = undefined}, State) ->
    {forward, NewName, RequestInfo, Request, Timeout, Priority, State};
forward(Type, Name, Pattern, NewName, RequestInfo, Request,
        Timeout, Priority, TransId, SrcPid,
        #destination{remote = Remote}, State) ->
    Forward = cloudi_service_router_client:
              forward(Type, Name, Pattern, NewName, RequestInfo, Request,
                      Timeout, Priority, TransId, SrcPid, Remote),
    if
        Forward =:= ok ->
            {noreply, State};
        Forward =:= timeout ->
            {reply, <<>>, State}
    end.

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
            SecondsNow = cloudi_timestamp:seconds_monotonic(),
            case maps:find(Pid, Failures) of
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

failure_store(FailureList, FailureCount, MaxCount, Pid, Failures) ->
    NewFailures = maps:put(Pid, FailureList, Failures),
    if
        FailureCount == MaxCount ->
            failure_kill(Pid),
            {true, NewFailures};
        true ->
            {false, NewFailures}
    end.

failure_check(SecondsNow, FailureList, MaxCount, infinity, Pid, Failures) ->
    NewFailureCount = erlang:length(FailureList),
    failure_store([SecondsNow | FailureList], NewFailureCount + 1,
                  MaxCount, Pid, Failures);
failure_check(SecondsNow, FailureList, MaxCount, MaxPeriod, Pid, Failures) ->
    {NewFailureCount,
     NewFailureList} = cloudi_timestamp:seconds_filter_monotonic(FailureList,
                                                                 SecondsNow,
                                                                 MaxPeriod),
    failure_store([SecondsNow | NewFailureList], NewFailureCount + 1,
                  MaxCount, Pid, Failures).

failure_kill(Pid) ->
    erlang:exit(Pid, cloudi_service_router).
