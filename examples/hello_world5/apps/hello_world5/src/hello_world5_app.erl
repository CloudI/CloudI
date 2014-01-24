%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
-module(hello_world5_app).

-behaviour(application).

%% application callbacks
-export([start/2,
         stop/1]).

-record(state,
    {
        service_ids = [] :: list(<<_:128>>)
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Start the Hello World 5 application.===
%% @end
%%-------------------------------------------------------------------------

-spec start(_StartType :: normal | {takeover, node()} | {failover, node()},
            _StartArgs :: any()) ->
    {ok, Pid :: pid()} |
    {ok, Pid :: pid(), State :: any()} |
    {error, Reason :: any()}.

start(_StartType, _StartArgs) ->
    Services = [
        {internal,
            "/cloudi/api/",
            cloudi_service_api_requests,
            [],
            none,
            5000, 5000, 5000, undefined, undefined, 1, 5, 300,
            [{automatic_loading, false}]},
        {internal,
            "/tests/http/",
            cloudi_service_http_cowboy,
            [{port, 6467}, {output, internal}],
            immediate_closest,
            5000, 5000, 5000, undefined, undefined, 1, 5, 300,
            [{duo_mode, true},
             {automatic_loading, false}]},
        % hello_world5 example
        {internal,
            "/examples/",
            hello_world5,
            [],
            none,
            5000, 5000, 5000, undefined, undefined, 1, 5, 300,
            [{automatic_loading, false}]},
        % can also use the proplist configuration format for the
        % hello_world5 example
        [%{type, internal}, % gets inferred from having module entry
         {prefix, "/examples/"},
         {module, hello_world5},
         %{args, []}, % default
         %{dest_refresh, immediate_closest}, % default
         %{timeout_init, 5000}, % default
         %{timeout_async, 5000}, % default
         %{timeout_sync, 5000}, % default
         %{dest_list_deny, undefined}, % default
         %{dest_list_allow, undefined}, % default
         %{count_process, 1}, % default
         %{max_r, 5}, % default
         %{max_t, 300}, % default
         {options, [{automatic_loading, false}]}]],
    case hello_world5_sup:start_link() of
        {ok, Pid} ->
            {ok, ServiceIds} = cloudi_service_api:services_add(Services,
                                                               infinity),
            {ok, Pid, #state{service_ids = ServiceIds}};
        {error, _} = Error ->
            Error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Stop the Hello World 5 application.===
%% @end
%%-------------------------------------------------------------------------

-spec stop(State :: any()) ->
    'ok'.

stop(#state{service_ids = ServiceIds}) ->
    ok = cloudi_service_api:services_remove(ServiceIds, infinity),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

