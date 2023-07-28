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
%%% Copyright (c) 2014-2023 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2014-2023 Michael Truog
%%% @version 2.0.7 {@date} {@time}
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

-include_lib("cloudi_core/include/cloudi_constants.hrl").
-include_lib("cloudi_core/include/cloudi_logger.hrl").
-include_lib("cloudi_core/include/cloudi_service.hrl").

-define(DEFAULT_SSH,                           undefined).
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
-define(DEFAULT_ADD_PREFIX,                         true).
        % The service configuration prefix is automatically added
        % as a prefix for each of the destinations (all service_names strings).
-define(DEFAULT_PARAMETERS_ALLOWED_DEFAULT,         true).
        % Parameters (strings matching the wildcard characters * and ?)
        % are allowed to be used for constructing destination service names.
-define(DEFAULT_PARAMETERS_STRICT_MATCHING_DEFAULT, true).
        % All * and ? wildcard characters must be used to create exact
        % service names.  That means the parameters_selected list must have
        % indexes to select each parameter
        % (each wildcard character in the incoming service name pattern).
-define(DEFAULT_HTTP_REDIRECT_HEALTH,          undefined).
        % If set to the service prefix of a cloudi_service_health_check
        % service instance, periodic requests will obtain the host
        % health status for the configured hosts so the health status
        % can be used for http_redirect_secondaries failover.
-define(DEFAULT_HTTP_REDIRECT_HEALTH_REFRESH,         15). % seconds
        % How frequently to request the health status of hosts from
        % the configured cloudi_service_health_check instance.
-define(DEFAULT_HTTP_REDIRECT_GET_DEFAULT,         false).
        % If true, use HTTP redirect status codes 302/301 and the
        % client is allowed to change the request to use the GET method.
        % If false, use HTTP redirect status codes 307/308 and the
        % client is not allowed to change the HTTP request method.
-define(DEFAULT_VALIDATE_REQUEST_INFO,         undefined).
-define(DEFAULT_VALIDATE_REQUEST,              undefined).
-define(DEFAULT_FAILURES_SOURCE_DIE,               false).
-define(DEFAULT_FAILURES_SOURCE_MAX_COUNT,             2). % see below:
        % (similar to the MaxR configuration value for services)
-define(DEFAULT_FAILURES_SOURCE_MAX_PERIOD,           60). % seconds, see below:
        % (similar to the MaxT configuration value for services)
        % If you want the source service to eventually fail,
        % use the service's MaxT/MaxR as the failures_source_max_period value
        % (e.g., 300/5 == 60 seconds).  Can also use the value 'infinity'
        % to accumulate a failure count indefinitely.

% destinations configuration arguments
-define(DEFAULT_REMOTE,                        undefined).
        % Should the service request be routed to a remote host?
        % Takes a list of options, with only the host_name option required.
        % Defaults are taken from the ssh server configuration,
        % if it was configured.
-define(DEFAULT_MODE,                        round_robin).
        % Load-balance among separate service names with
        % 'round_robin' order or 'random' order.
% DEFAULT_PARAMETERS_ALLOWED is set based on
% DEFAULT_PARAMETERS_ALLOWED_DEFAULT
% DEFAULT_PARAMETERS_STRICT_MATCHING is set based on
% DEFAULT_PARAMETERS_STRICT_MATCHING_DEFAULT
-define(DEFAULT_PARAMETERS_SELECTED,                  []).
        % A list of parameter indexes (1-based index) to be used when
        % constructing destination service names.  That means that the first
        % instance of * or ? is index 1.  If [2] was provided, the second
        % instance of * or ? would be used to substitute the first * or ?
        % in all the destination service_names strings.
-define(DEFAULT_HTTP_REDIRECT,                 undefined).
        % A HTTP redirect response will be provided.
        % The incoming service request may have been sent from the
        % HTTP/HTTPS server services cloudi_service_http_cowboy or
        % cloudi_service_http_elli.  The URL string provided may have parameter
        % substitution but will not have a prefix added due to add_prefix.
        % The destination URL string will be provided as the
        % "Location" HTTP header value.  If http_redirect is used,
        % remote, mode and service_names are not used.
-define(DEFAULT_HTTP_REDIRECT_SECONDARIES,            []).
        % A list of alternative HTTP redirect URL strings can be provided
        % for use if http_redirect_health was set.
        % A temporary redirect occurs for failover with the URL strings
        % provided while the primary (permanent) redirect is unhealthy
        % based on the data provided from cloudi_service_health_check.
% DEFAULT_HTTP_REDIRECT_GET is set based on
% DEFAULT_HTTP_REDIRECT_GET_DEFAULT
-define(DEFAULT_SERVICE_NAMES,                        []).
        % The destination service names to be used for forwarding
        % service requests.  If * or ? wildcard characters are used
        % in the service name strings, parameters_allowed needs to be true.
        % The destination service name strings may have consecutive
        % wildcard characters for substitution.

% ensure a timeout does not delay the http_redirect_health_refresh
-define(TIMEOUT_DELTA, 100). % milliseconds

-type http_redirect_health_refresh() ::
    1..(?TIMEOUT_MAX_ERLANG div 1000).

-record(destination_forward,
    {
        parameters_allowed
            :: boolean(),
        parameters_strict_matching
            :: boolean(),
        parameters_selected
            :: list(pos_integer()),
        remote
            :: undefined | cloudi_service_router_client:state(),
        index = 1
            :: pos_integer(),
        length
            :: pos_integer(),
        mode
            :: random | round_robin,
        service_names
            :: tuple()
    }).

-record(destination_return,
    {
        parameters_allowed
            :: boolean(),
        parameters_strict_matching
            :: boolean(),
        parameters_selected
            :: list(pos_integer()),
        http_redirect
            :: cloudi:nonempty_bytestring(),
        http_redirect_secondaries
            :: list(cloudi:nonempty_bytestring()),
        http_redirect_get
            :: boolean()
    }).

-record(state,
    {
        service
            :: cloudi_service:source(),
        ssh
            :: undefined | cloudi_service_router_ssh_server:state(),
        http_redirect_health = http_redirect_health([])
            % hostname -> boolean()
            :: cloudi_x_trie:cloudi_x_trie(),
        http_redirect_health_name
            :: undefined | cloudi_service:service_name(),
        http_redirect_health_refresh
            :: http_redirect_health_refresh(),
        http_redirect_health_id = undefined
            :: undefined | cloudi_service:trans_id(),
        validate_request_info
            :: undefined | fun((any()) -> boolean()),
        validate_request
            :: undefined | fun((any(), any()) -> boolean()),
        failures_source_die
            :: boolean(),
        failures_source_max_count
            :: pos_integer(),
        failures_source_max_period
            :: infinity | pos_integer(),
        failures_source = #{}
            :: #{pid() := list(cloudi_timestamp:seconds_monotonic())},
        destinations
            % pattern -> #destination_forward{} | #destination_return{}
            :: cloudi_x_trie:cloudi_x_trie()
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
        {parameters_allowed_default,    ?DEFAULT_PARAMETERS_ALLOWED_DEFAULT},
        {parameters_strict_matching_default,
         ?DEFAULT_PARAMETERS_STRICT_MATCHING_DEFAULT},
        {http_redirect_health,          ?DEFAULT_HTTP_REDIRECT_HEALTH},
        {http_redirect_health_refresh,  ?DEFAULT_HTTP_REDIRECT_HEALTH_REFRESH},
        {http_redirect_get_default,     ?DEFAULT_HTTP_REDIRECT_GET_DEFAULT},
        {validate_request_info,         ?DEFAULT_VALIDATE_REQUEST_INFO},
        {validate_request,              ?DEFAULT_VALIDATE_REQUEST},
        {failures_source_die,           ?DEFAULT_FAILURES_SOURCE_DIE},
        {failures_source_max_count,     ?DEFAULT_FAILURES_SOURCE_MAX_COUNT},
        {failures_source_max_period,    ?DEFAULT_FAILURES_SOURCE_MAX_PERIOD},
        {destinations,                  []}],
    [SSHOptions, AddPrefix,
     ParametersAllowedDefault, ParametersStrictMatchingDefault,
     HttpRedirectHealth, HttpRedirectHealthRefresh, HttpRedirectGetDefault,
     ValidateRequestInfo0, ValidateRequest0,
     FailuresSrcDie, FailuresSrcMaxCount, FailuresSrcMaxPeriod,
     DestinationsL] = cloudi_proplists:take_values(Defaults, Args),
    Service = cloudi_service:self(Dispatcher),
    Environment = cloudi_environment:lookup(),
    SSH = cloudi_service_router_ssh_server:new(SSHOptions,
                                               Environment,
                                               Dispatcher),
    true = is_boolean(AddPrefix),
    true = is_boolean(ParametersAllowedDefault),
    true = is_boolean(ParametersStrictMatchingDefault),
    true = is_integer(HttpRedirectHealthRefresh) andalso
           (HttpRedirectHealthRefresh > 0) andalso
           (HttpRedirectHealthRefresh =< ?TIMEOUT_MAX_ERLANG div 1000),
    HttpRedirectHealthName = if
        HttpRedirectHealth =:= undefined ->
            undefined;
        is_list(HttpRedirectHealth) andalso
        is_integer(hd(HttpRedirectHealth)) ->
            false = cloudi_service_name:pattern(HttpRedirectHealth),
            erlang:send_after(HttpRedirectHealthRefresh * 1000, Service,
                              http_redirect_health),
            HttpRedirectHealth ++ "hosts.erl"
    end,
    true = is_boolean(HttpRedirectGetDefault),
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
        {parameters_allowed,            ParametersAllowedDefault},
        {parameters_strict_matching,    ParametersStrictMatchingDefault},
        {parameters_selected,           ?DEFAULT_PARAMETERS_SELECTED},
        {http_redirect,                 ?DEFAULT_HTTP_REDIRECT},
        {http_redirect_secondaries,     ?DEFAULT_HTTP_REDIRECT_SECONDARIES},
        {http_redirect_get,             HttpRedirectGetDefault},
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
         HttpRedirectSecondaries,
         HttpRedirectGet,
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
        true = is_boolean(HttpRedirectGet),
        true = is_list(Names0),
        Length = erlang:length(Names0),
        Destination = if
            HttpRedirect =:= undefined,
            HttpRedirectSecondaries == [] ->
                true = (Length > 0),
                NamesN = if
                    AddPrefix =:= true ->
                        [Prefix ++ Suffix || Suffix <- Names0];
                    AddPrefix =:= false ->
                        Names0
                end,
                true = lists:all(fun cloudi_x_trie:is_bytestring_nonempty/1,
                                 NamesN),
                #destination_forward{parameters_allowed = ParametersAllowed,
                                     parameters_strict_matching =
                                         ParametersStrictMatching,
                                     parameters_selected =
                                         ParametersSelected,
                                     remote = Remote,
                                     length = Length,
                                     mode = Mode,
                                     service_names =
                                         erlang:list_to_tuple(NamesN)};
            is_integer(hd(HttpRedirect)) ->
                undefined = Remote,
                true = (Length == 0),
                true = cloudi_x_trie:is_bytestring_nonempty(HttpRedirect),
                case HttpRedirectSecondaries of
                    [] ->
                        true;
                    [_ | _] ->
                        true = HttpRedirectHealth /= undefined,
                        true = lists:all(fun cloudi_x_trie:
                                             is_bytestring_nonempty/1,
                                         HttpRedirectSecondaries)
                end,
                #destination_return{parameters_allowed = ParametersAllowed,
                                    parameters_strict_matching =
                                        ParametersStrictMatching,
                                    parameters_selected =
                                        ParametersSelected,
                                    http_redirect = HttpRedirect,
                                    http_redirect_secondaries =
                                        HttpRedirectSecondaries,
                                    http_redirect_get = HttpRedirectGet}
        end,
        cloudi_x_trie:store(Prefix ++ PatternSuffix, Destination, D)
    end, cloudi_x_trie:new(), DestinationsL),
    {ok, #state{service = Service,
                ssh = SSH,
                http_redirect_health_name = HttpRedirectHealthName,
                http_redirect_health_refresh = HttpRedirectHealthRefresh,
                validate_request_info = ValidateRequestInfo1,
                validate_request = ValidateRequest1,
                failures_source_die = FailuresSrcDie,
                failures_source_max_count = FailuresSrcMaxCount,
                failures_source_max_period = FailuresSrcMaxPeriod,
                destinations = Destinations}}.

cloudi_service_handle_request(RequestType, Name, Pattern, RequestInfo, Request,
                              Timeout, Priority, TransId, Source,
                              #state{validate_request_info = RequestInfoF,
                                     validate_request = RequestF,
                                     destinations = Destinations} = State,
                              _Dispatcher) ->
    case validate(RequestInfoF, RequestF,
                  RequestInfo, Request) of
        true ->
            case cloudi_x_trie:find(Pattern, Destinations) of
                {ok, #destination_forward{} = Destination} ->
                    destination_forward(RequestType, Name, Pattern,
                                        RequestInfo, Request,
                                        Timeout, Priority, TransId, Source,
                                        Destination, State);
                {ok, #destination_return{} = Destination} ->
                    destination_return(RequestType, Name, Pattern,
                                       RequestInfo, Request,
                                       Timeout, Priority, TransId, Source,
                                       Destination, State);
                error ->
                    request_failed(Source, State)
            end;
        false ->
            request_failed(Source, State)
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
cloudi_service_handle_info(http_redirect_health,
                           #state{service = Service,
                                  http_redirect_health_name = HealthName,
                                  http_redirect_health_refresh = HealthRefresh
                                  } = State, Dispatcher) ->
    Timeout = HealthRefresh * 1000,
    erlang:send_after(Timeout, Service, http_redirect_health),
    case cloudi_service:send_async_active(Dispatcher, HealthName, <<>>,
                                          Timeout - ?TIMEOUT_DELTA) of
        {ok, TransId} ->
            {noreply, State#state{http_redirect_health_id = TransId}};
        {error, Reason} ->
            {stop, {http_redirect_health, Reason}, State}
    end;
cloudi_service_handle_info(#return_async_active{response = Response,
                                                trans_id = TransId},
                           #state{http_redirect_health_id = TransId} = State,
                           _Dispatcher) ->
    {noreply, State#state{http_redirect_health = http_redirect_health(Response),
                          http_redirect_health_id = undefined}};
cloudi_service_handle_info(#timeout_async_active{trans_id = TransId},
                           #state{http_redirect_health_id = TransId} = State,
                           _Dispatcher) ->
    {stop, {http_redirect_health, timeout}, State};
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

destination_forward(RequestType, Name, Pattern, RequestInfo, Request,
                    Timeout, Priority, TransId, Source,
                    Destination,
                    #state{destinations = Destinations} = State) ->
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
                    Timeout, Priority, TransId, Source,
                    DestinationNew,
                    State#state{
                        destinations = DestinationsNew});
        {error, Reason} ->
            ?LOG_ERROR("(~p -> ~p) error: ~p",
                       [Name, NameNext, Reason]),
            request_failed(Source, State)
    end.

destination_return(_RequestType, Name, Pattern, _RequestInfo, _Request,
                   _Timeout, _Priority, _TransId, Source,
                   Destination,
                   #state{http_redirect_health = HttpRedirectHealth} = State) ->
    Parameters = cloudi_service_name:parse(Name, Pattern),
    case http_redirect_pick(Parameters, Destination, HttpRedirectHealth) of
        {ok, HttpStatusCode, HttpRedirect} ->
            {reply,
             [{<<"status">>, HttpStatusCode},
              {<<"cache-control">>, <<"no-cache">>},
              {<<"location">>, HttpRedirect}],
             <<>>, State};
        {error, Reason, HttpRedirect} ->
            ?LOG_ERROR("(~p http_redirect ~p) error: ~p",
                       [Name, HttpRedirect, Reason]),
            request_failed(Source, State)
    end.

destination_pick(#destination_forward{service_names = {Name}} = Destination) ->
    {Name, Destination};
destination_pick(#destination_forward{index = Index,
                                      length = Length,
                                      mode = round_robin,
                                      service_names = Names} = Destination) ->
    IndexNew = if
        Index == Length ->
            1;
        true ->
            Index + 1
    end,
    Name = element(Index, Names),
    {Name, Destination#destination_forward{index = IndexNew}};
destination_pick(#destination_forward{length = Length,
                                      mode = random,
                                      service_names = Names} = Destination) ->
    Index = cloudi_x_quickrand:uniform(Length),
    Name = element(Index, Names),
    {Name, Destination}.

name_parameters(Name, [], _) ->
    {ok, Name};
name_parameters(_, error, _) ->
    % a mismatch between trie:find_match2/2 and trie:pattern2_parse/2
    % functionality should be impossible, but is handled here
    {error, parameters_invalid};
name_parameters(_, [_ | _],
                #destination_forward{parameters_allowed = false}) ->
    {error, parameters_not_allowed};
name_parameters(_, [_ | _],
                #destination_return{parameters_allowed = false}) ->
    {error, parameters_not_allowed};
name_parameters(Pattern, Parameters,
                #destination_forward{parameters_strict_matching =
                                         ParametersStrictMatching,
                                     parameters_selected =
                                         ParametersSelected}) ->
    cloudi_service_name:new(Pattern, Parameters, ParametersSelected,
                            ParametersStrictMatching);
name_parameters(Pattern, Parameters,
                #destination_return{parameters_strict_matching =
                                        ParametersStrictMatching,
                                    parameters_selected =
                                        ParametersSelected}) ->
    cloudi_service_name:new(Pattern, Parameters, ParametersSelected,
                            ParametersStrictMatching).

forward(_, _, _, NameNew, RequestInfo, Request,
        Timeout, Priority, _, _,
        #destination_forward{remote = undefined}, State) ->
    {forward, NameNew, RequestInfo, Request, Timeout, Priority, State};
forward(RequestType, Name, Pattern, NameNew, RequestInfo, Request,
        Timeout, Priority, TransId, Source,
        #destination_forward{remote = Remote}, State) ->
    Forward = cloudi_service_router_client:
              forward(RequestType, Name, Pattern, NameNew, RequestInfo, Request,
                      Timeout, Priority, TransId, Source, Remote),
    if
        Forward =:= ok ->
            {noreply, State};
        Forward =:= timeout ->
            {reply, <<>>, State}
    end.

http_redirect_pick(Parameters,
                   #destination_return{
                       http_redirect = HttpRedirect,
                       http_redirect_secondaries = HttpRedirectSecondaries,
                       http_redirect_get = HttpRedirectGet} = Destination,
                   HttpRedirectHealth) ->
    case name_parameters(HttpRedirect, Parameters, Destination) of
        {ok, HttpRedirectNew} ->
            HttpStatusCode = if
                HttpRedirectGet =:= true ->
                    <<"301">>;
                HttpRedirectGet =:= false ->
                    <<"308">>
            end,
            case cloudi_x_trie:find(http_redirect_hostname(HttpRedirectNew),
                                    HttpRedirectHealth) of
                {ok, false} ->
                    http_redirect_pick_secondary(HttpRedirectSecondaries,
                                                 Parameters,
                                                 HttpRedirectGet,
                                                 HttpStatusCode,
                                                 HttpRedirectNew,
                                                 Destination,
                                                 HttpRedirectHealth);
                _ ->
                    {ok, HttpStatusCode,
                     erlang:list_to_binary(HttpRedirectNew)}
            end;
        {error, Reason} ->
            {error, Reason, HttpRedirect}
    end.

http_redirect_pick_secondary([], _, _,
                             HttpStatusCodeDefault, HttpRedirectDefault,
                             _, _) ->
    % no secondary is healthy, so return the primary until something changes
    {ok, HttpStatusCodeDefault,
     erlang:list_to_binary(HttpRedirectDefault)};
http_redirect_pick_secondary([HttpRedirect | HttpRedirectL], Parameters,
                             HttpRedirectGet,
                             HttpStatusCodeDefault, HttpRedirectDefault,
                             Destination, HttpRedirectHealth) ->
    case name_parameters(HttpRedirect, Parameters, Destination) of
        {ok, HttpRedirectNew} ->
            case cloudi_x_trie:find(http_redirect_hostname(HttpRedirectNew),
                                    HttpRedirectHealth) of
                {ok, true} ->
                    HttpStatusCode = if
                        HttpRedirectGet =:= true ->
                            <<"302">>;
                        HttpRedirectGet =:= false ->
                            <<"307">>
                    end,
                    {ok, HttpStatusCode,
                     erlang:list_to_binary(HttpRedirectNew)};
                _ ->
                    http_redirect_pick_secondary(HttpRedirectL,
                                                 Parameters,
                                                 HttpRedirectGet,
                                                 HttpStatusCodeDefault,
                                                 HttpRedirectDefault,
                                                 Destination,
                                                 HttpRedirectHealth)
            end;
        {error, Reason} ->
            {error, Reason, HttpRedirect}
    end.

http_redirect_hostname(HttpRedirect) ->
    maps:get(host, uri_string:parse(HttpRedirect)).

http_redirect_health(HostsInfo) ->
    http_redirect_health(HostsInfo, cloudi_x_trie:new()).

http_redirect_health([], HttpRedirectHealth) ->
    HttpRedirectHealth;
http_redirect_health([{Hostname, Info} | HostsInfo], HttpRedirectHealth) ->
    {health_failed, HealthFailed} = lists:keyfind(health_failed, 1, Info),
    http_redirect_health(HostsInfo,
                         cloudi_x_trie:store(Hostname, not HealthFailed,
                                             HttpRedirectHealth)).

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

request_failed(Source,
               #state{failures_source_die = FailuresSrcDie,
                      failures_source_max_count = FailuresSrcMaxCount,
                      failures_source_max_period = FailuresSrcMaxPeriod,
                      failures_source = FailuresSrc} = State) ->
    {DeadSrc, FailuresSrcNew} = failure(FailuresSrcDie,
                                        FailuresSrcMaxCount,
                                        FailuresSrcMaxPeriod,
                                        Source, FailuresSrc),
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
