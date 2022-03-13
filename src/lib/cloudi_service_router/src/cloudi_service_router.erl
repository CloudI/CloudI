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
%%% Copyright (c) 2014-2022 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2014-2022 Michael Truog
%%% @version 2.0.5 {@date} {@time}
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
-define(DEFAULT_ADD_PREFIX,                  true).
        % The service configuration prefix is automatically added
        % as a prefix for each of the destinations (all service_names strings).
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
        % Load-balance among separate service names with
        % 'round_robin' order or 'random' order.
-define(DEFAULT_PARAMETERS_ALLOWED,          true).
        % Parameters (strings matching the wildcard characters * and ?)
        % are allowed to be used for constructing destination service names.
-define(DEFAULT_PARAMETERS_STRICT_MATCHING,  true).
        % All * and ? wildcard characters must be used to create exact
        % service names.  That means the parameters_selected list must have
        % indexes to select each parameter
        % (each wildcard character in the incoming service name pattern).
-define(DEFAULT_PARAMETERS_SELECTED,           []).
        % A list of parameter indexes (1-based index) to be used when
        % constructing destination service names.  That means that the first
        % instance of * or ? is index 1.  If [2] was provided, the second
        % instance of * or ? would be used to substitute the first * or ?
        % in all the destination service_names strings.
-define(DEFAULT_HTTP_REDIRECT,          undefined).
        % A HTTP 301 redirect response will be provided.
        % The incoming service request may have been sent from the
        % HTTP/HTTPS server services cloudi_service_http_cowboy or
        % cloudi_service_http_elli.  The URL string provided may have parameter
        % substitution but will not have a prefix added due to add_prefix.
        % The destination URL string will be provided as the
        % "Location" HTTP header value.  If http_redirect is used,
        % remote, mode and service_names are not used.
-define(DEFAULT_SERVICE_NAMES,                 []).
        % The destination service names to be used for forwarding
        % service requests.  If * or ? wildcard characters are used
        % in the service name strings, parameters_allowed needs to be true.
        % The destination service name strings may have consecutive
        % wildcard characters for substitution.

-record(destination,
    {
        index = 1
            :: pos_integer(),
        length = 0
            :: non_neg_integer(),
        remote
            :: undefined | cloudi_service_router_client:state(),
        mode = ?DEFAULT_MODE
            :: random | round_robin,
        parameters_allowed = ?DEFAULT_PARAMETERS_ALLOWED
            :: boolean(),
        parameters_strict_matching = ?DEFAULT_PARAMETERS_STRICT_MATCHING
            :: boolean(),
        parameters_selected = []
            :: list(pos_integer()),
        http_redirect = ?DEFAULT_HTTP_REDIRECT
            :: undefined | string(),
        service_names = []
            :: list(string())
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
        {parameters_selected,           ?DEFAULT_PARAMETERS_SELECTED},
        {http_redirect,                 ?DEFAULT_HTTP_REDIRECT},
        {service_names,                 ?DEFAULT_SERVICE_NAMES}],
    Destinations = lists:foldl(fun({PatternSuffix, L}, D) ->
        true = is_tuple(hd(L)),
        cloudi_service:subscribe(Dispatcher, PatternSuffix),
        [RemoteOptions,
         Mode,
         ParametersAllowed,
         ParametersStrictMatching,
         ParametersSelected,
         HttpRedirect,
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
        NamesN = if
            AddPrefix =:= true ->
                [Prefix ++ Suffix || Suffix <- Names0];
            AddPrefix =:= false ->
                Names0
        end,
        if
            HttpRedirect =:= undefined ->
                true = (Length > 0),
                true = ((ParametersAllowed =:= true) orelse
                        ((ParametersAllowed =:= false) andalso
                         (lists:any(fun cloudi_service_name:pattern/1,
                                    NamesN) =:= false))),
                ok;
            is_integer(hd(HttpRedirect)) ->
                undefined = Remote,
                true = (Length == 0),
                true = ((ParametersAllowed =:= true) orelse
                        ((ParametersAllowed =:= false) andalso
                         cloudi_service_name:pattern(HttpRedirect))),
                ok
        end,
        Destination = #destination{length = Length,
                                   remote = Remote,
                                   mode = Mode,
                                   parameters_allowed =
                                       ParametersAllowed,
                                   parameters_strict_matching =
                                       ParametersStrictMatching,
                                   parameters_selected =
                                       ParametersSelected,
                                   http_redirect = HttpRedirect,
                                   service_names = NamesN},
        cloudi_x_trie:store(Prefix ++ PatternSuffix, Destination, D)
    end, cloudi_x_trie:new(), DestinationsL),
    {ok, #state{ssh = SSH,
                validate_request_info = ValidateRequestInfo1,
                validate_request = ValidateRequest1,
                failures_source_die = FailuresSrcDie,
                failures_source_max_count = FailuresSrcMaxCount,
                failures_source_max_period = FailuresSrcMaxPeriod,
                destinations = Destinations}}.

cloudi_service_handle_request(RequestType, Name, Pattern, RequestInfo, Request,
                              Timeout, Priority, TransId, SrcPid,
                              #state{validate_request_info = RequestInfoF,
                                     validate_request = RequestF,
                                     destinations = Destinations} = State,
                              _Dispatcher) ->
    case validate(RequestInfoF, RequestF,
                  RequestInfo, Request) of
        true ->
            case cloudi_x_trie:find(Pattern, Destinations) of
                {ok,
                 #destination{http_redirect = undefined} = Destination} ->
                    {NameNext, DestinationNew} = destination_pick(Destination),
                    Parameters = cloudi_service_name:parse(Name, Pattern),
                    case name_parameters(NameNext, Parameters,
                                         DestinationNew) of
                        {ok, NameNew} ->
                            DestinationsNew = cloudi_x_trie:
                                              store(Pattern,
                                                    DestinationNew,
                                                    Destinations),
                            forward(RequestType, Name, Pattern, NameNew,
                                    RequestInfo, Request,
                                    Timeout, Priority, TransId, SrcPid,
                                    DestinationNew,
                                    State#state{
                                        destinations = DestinationsNew});
                        {error, Reason} ->
                            ?LOG_ERROR("(~p -> ~p) error: ~p",
                                       [Name, NameNext, Reason]),
                            request_failed(SrcPid, State)
                    end;
                {ok,
                 #destination{http_redirect = HttpRedirect} = Destination} ->
                    Parameters = cloudi_service_name:parse(Name, Pattern),
                    case name_parameters(HttpRedirect, Parameters,
                                         Destination) of
                        {ok, HttpRedirectNew} ->
                            {reply,
                             [{<<"status">>, <<"301">>},
                              {<<"location">>,
                               erlang:list_to_binary(HttpRedirectNew)}],
                             <<>>, State};
                        {error, Reason} ->
                            ?LOG_ERROR("(~p http_redirect ~p) error: ~p",
                                       [Name, HttpRedirect, Reason]),
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
    FailuresSrcNew = if
        FailuresSrcDie =:= true ->
            maps:remove(Pid, FailuresSrc);
        FailuresSrcDie =:= false ->
            FailuresSrc
    end,
    {noreply, State#state{failures_source = FailuresSrcNew}};

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
destination_pick(#destination{index = Index,
                              length = Length,
                              mode = round_robin,
                              service_names = Names} = Destination) ->
    IndexNew = if
        Index == Length ->
            1;
        true ->
            Index + 1
    end,
    Name = lists:nth(Index, Names),
    {Name, Destination#destination{index = IndexNew}};
destination_pick(#destination{length = Length,
                              mode = random,
                              service_names = Names} = Destination) ->
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

forward(_, _, _, NameNew, RequestInfo, Request,
        Timeout, Priority, _, _,
        #destination{remote = undefined}, State) ->
    {forward, NameNew, RequestInfo, Request, Timeout, Priority, State};
forward(RequestType, Name, Pattern, NameNew, RequestInfo, Request,
        Timeout, Priority, TransId, SrcPid,
        #destination{remote = Remote}, State) ->
    Forward = cloudi_service_router_client:
              forward(RequestType, Name, Pattern, NameNew, RequestInfo, Request,
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
    {DeadSrc, FailuresSrcNew} = failure(FailuresSrcDie,
                                        FailuresSrcMaxCount,
                                        FailuresSrcMaxPeriod,
                                        SrcPid, FailuresSrc),
    if
        DeadSrc =:= true ->
            {noreply,
             State#state{failures_source = FailuresSrcNew}};
        DeadSrc =:= false ->
            {reply, <<>>,
             State#state{failures_source = FailuresSrcNew}}
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
    FailuresNew = maps:put(Pid, FailureList, Failures),
    if
        FailureCount == MaxCount ->
            failure_kill(Pid),
            {true, FailuresNew};
        true ->
            {false, FailuresNew}
    end.

failure_check(SecondsNow, FailureList, MaxCount, infinity, Pid, Failures) ->
    FailureCountNew = erlang:length(FailureList),
    failure_store([SecondsNow | FailureList], FailureCountNew + 1,
                  MaxCount, Pid, Failures);
failure_check(SecondsNow, FailureList, MaxCount, MaxPeriod, Pid, Failures) ->
    {FailureCountNew,
     FailureListNew} = cloudi_timestamp:seconds_filter_monotonic(FailureList,
                                                                 SecondsNow,
                                                                 MaxPeriod),
    failure_store([SecondsNow | FailureListNew], FailureCountNew + 1,
                  MaxCount, Pid, Failures).

failure_kill(Pid) ->
    erlang:exit(Pid, cloudi_service_router).
