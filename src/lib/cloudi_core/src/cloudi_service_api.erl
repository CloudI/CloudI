%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Service API Module==
%%% A module that exposes dynamic configuration of CloudI.
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
%%% @version 1.3.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_api).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([acl_add/2,
         acl_remove/2,
         service_subscriptions/2,
         services_add/2,
         services_remove/2,
         services_restart/2,
         services_search/2,
         services/1,
         nodes_add/2,
         nodes_remove/2,
         nodes_alive/1,
         nodes_dead/1,
         nodes/1,
         loglevel_set/2,
         log_redirect/2,
         code_path_add/2,
         code_path_remove/2,
         code_path/1]).

-include("cloudi_constants.hrl").
-include("cloudi_service_api.hrl").

-type dest_refresh() ::
    lazy_closest | immediate_closest |
    lazy_furthest | immediate_furthest |
    lazy_random | immediate_random |
    lazy_local | immediate_local |
    lazy_remote | immediate_remote |
    lazy_newest | immediate_newest |
    lazy_oldest | immediate_oldest |
    none.
-export_type([dest_refresh/0]).

-type dest_refresh_delay_milliseconds() ::
    (?TIMEOUT_DELTA + 1)..?TIMEOUT_MAX_ERLANG.
-export_type([dest_refresh_delay_milliseconds/0]).

-type timeout_milliseconds() ::
    (?TIMEOUT_DELTA + 1)..?TIMEOUT_MAX.
-export_type([timeout_milliseconds/0]).

-type request_timeout_immediate_max_milliseconds() ::
    0..?TIMEOUT_MAX_ERLANG.
-export_type([request_timeout_immediate_max_milliseconds/0]).

-type response_timeout_immediate_max_milliseconds() ::
    0..?TIMEOUT_MAX_ERLANG.
-export_type([response_timeout_immediate_max_milliseconds/0]).

-type acl() ::
    list(atom() | cloudi:service_name_pattern()).
-export_type([acl/0]).

-type dest_list() ::
    acl() | undefined.
-export_type([dest_list/0]).

-type seconds() ::
    pos_integer().
-export_type([seconds/0]).

-type service_options_internal() ::
    list({priority_default, cloudi_service:priority()} |
         {queue_limit, undefined | pos_integer()} |
         {dest_refresh_start, dest_refresh_delay_milliseconds()} |
         {dest_refresh_delay, dest_refresh_delay_milliseconds()} |
         {request_timeout_adjustment, boolean()} |
         {request_timeout_immediate_max,
          request_timeout_immediate_max_milliseconds()} |
         {response_timeout_adjustment, boolean()} |
         {response_timeout_immediate_max,
          response_timeout_immediate_max_milliseconds()} |
         {scope, atom()} |
         {monkey_latency,
          list({time_uniform_min, pos_integer()} |
               {time_uniform_max, pos_integer()} |
               {time_gaussian_mean, pos_integer()} |
               {time_gaussian_stddev, float()} |
               {time_absolute, pos_integer()}) | system | false} |
         {monkey_chaos,
          list({probability_request, float()} |
               {probability_day, float()}) | system | false} |
         {application_name, undefined | atom()} |
         {request_pid_uses, infinity | pos_integer()} |
         {request_pid_options,
          list({fullsweep_after, non_neg_integer()} |
               {min_heap_size, non_neg_integer()} |
               {min_bin_vheap_size, non_neg_integer()})} |
         {info_pid_uses, infinity | pos_integer()} |
         {info_pid_options,
          list({fullsweep_after, non_neg_integer()} |
               {min_heap_size, non_neg_integer()} |
               {min_bin_vheap_size, non_neg_integer()})} |
         {duo_mode, boolean()} |
         {hibernate, boolean()} |
         {reload, boolean()} |
         {automatic_loading, boolean()}).
-type service_options_external() ::
    list({priority_default, cloudi_service:priority()} |
         {queue_limit, undefined | pos_integer()} |
         {dest_refresh_start, dest_refresh_delay_milliseconds()} |
         {dest_refresh_delay, dest_refresh_delay_milliseconds()} |
         {request_timeout_adjustment, boolean()} |
         {request_timeout_immediate_max,
          request_timeout_immediate_max_milliseconds()} |
         {response_timeout_adjustment, boolean()} |
         {response_timeout_immediate_max,
          response_timeout_immediate_max_milliseconds()} |
         {scope, atom()} |
         {monkey_latency,
          list({time_uniform_min, pos_integer()} |
               {time_uniform_max, pos_integer()} |
               {time_gaussian_mean, pos_integer()} |
               {time_gaussian_stddev, float()} |
               {time_absolute, pos_integer()}) | system | false} |
         {monkey_chaos,
          list({probability_request, float()} |
               {probability_day, float()}) | system | false}).
-export_type([service_options_internal/0,
              service_options_external/0]).

-type loglevel() :: fatal | error | warn | info | debug | trace | off.
-export_type([loglevel/0]).

-type service_id() :: <<_:128>>. % version 1 UUID (service instance id)
-type service_internal() :: #internal{}.
-type service_external() :: #external{}.
-type service_proplist() ::
    list({type, internal | external} |
         {prefix, cloudi:service_name_pattern()} |
         {module, atom() | file:filename()} |
         {file_path, file:filename()} |
         {args, list()} |
         {env, list({string(), string()})} |
         {dest_refresh, dest_refresh()} |
         {protocol, 'default' | 'local' | 'tcp' | 'udp'} |
         {buffer_size, 'default' | pos_integer()} |
         {timeout_init, timeout_milliseconds()} |
         {timeout_async, timeout_milliseconds()} |
         {timeout_sync, timeout_milliseconds()} |
         {dest_list_deny, dest_list()} |
         {dest_list_allow, dest_list()} |
         {count_process, pos_integer() | float()} |
         {count_thread, pos_integer() | float()} |
         {max_r, non_neg_integer()} |
         {max_t, seconds()} |
         {options, service_options_internal() | service_options_external()}).
-type service() :: #internal{} | #external{}.
-export_type([service_id/0,
              service_internal/0,
              service_external/0,
              service/0,
              service_proplist/0]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

% timeout for the functions below
-type api_timeout_milliseconds() ::
    (?TIMEOUT_DELTA + 1)..?TIMEOUT_MAX_ERLANG | infinity.
-export_type([api_timeout_milliseconds/0]).

%%-------------------------------------------------------------------------
%% @doc
%% ===Add ACL entries.===
%% Add more ACL entries to be later used when starting services. An ACL
%% entry is an Erlang atom() -> list(atom() | string()) relationship which
%% provides a logical grouping of service name patterns
%% (e.g., {api, ["/cloudi/api/"]}). When providing a service name pattern
%% for an ACL entry, a non-pattern will be assumed to be a prefix
%% (i.e., "/cloudi/api/" == "/cloudi/api/*").
%% @end
%%-------------------------------------------------------------------------

-spec acl_add(L :: list({atom(), acl()}), 
              Timeout :: api_timeout_milliseconds()) ->
    ok |
    {error, any()}.

acl_add([_ | _] = L, Timeout)
    when ((is_integer(Timeout) andalso
           (Timeout > ?TIMEOUT_DELTA) andalso
           (Timeout =< ?TIMEOUT_MAX_ERLANG)) orelse
          (Timeout =:= infinity)) ->
    cloudi_configurator:acl_add(L, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Remove ACL entries.===
%% Remove ACL entries that are no longer needed. Running services will
%% retain their configuration, so this impacts services that are started
%% in the future.
%% @end
%%-------------------------------------------------------------------------

-spec acl_remove(L :: list(atom()),
                 Timeout :: api_timeout_milliseconds()) ->
    ok |
    {error, any()}.

acl_remove([_ | _] = L, Timeout)
    when ((is_integer(Timeout) andalso
           (Timeout > ?TIMEOUT_DELTA) andalso
           (Timeout =< ?TIMEOUT_MAX_ERLANG)) orelse
          (Timeout =:= infinity)) ->
    cloudi_configurator:acl_remove(L, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a list of all service subscriptions.===
%% When a subscription on the same service name pattern occurred
%% multiple times, only a single entry is returned within the list.
%% @end
%%-------------------------------------------------------------------------

-spec service_subscriptions(ServiceId :: service_id(),
                            Timeout :: api_timeout_milliseconds()) ->
    {ok, list(cloudi_service:service_name_pattern())} |
    {error, any()}.

service_subscriptions(ServiceId, Timeout)
    when is_binary(ServiceId), byte_size(ServiceId) == 16,
         ((is_integer(Timeout) andalso
           (Timeout > ?TIMEOUT_DELTA) andalso
           (Timeout =< ?TIMEOUT_MAX_ERLANG)) orelse
          (Timeout =:= infinity)) ->
    cloudi_configurator:service_subscriptions(ServiceId, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Add service instances.===
%% Provide service configuration using the same syntax found in the
%% configuration file (i.e., /usr/local/etc/cloudi/cloudi.conf).
%% @end
%%-------------------------------------------------------------------------

-spec services_add(L :: list(#internal{} | #external{} | service_proplist()),
                   Timeout :: api_timeout_milliseconds()) ->
    {ok, list(service_id())} |
    {error, any()}.

services_add([_ | _] = L, Timeout)
    when ((is_integer(Timeout) andalso
           (Timeout > ?TIMEOUT_DELTA) andalso
           (Timeout =< ?TIMEOUT_MAX_ERLANG)) orelse
          (Timeout =:= infinity)) ->
    cloudi_configurator:services_add(L, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Remove service instances.===
%% Provide the Service UUIDs for the services that should be stopped.
%% The Service UUID is shown in the output of services/1. When the
%% service is stopped, its running instance is removed from CloudI, but
%% does not impact any other running instances (even if they are the same
%% service module or binary).
%% @end
%%-------------------------------------------------------------------------

-spec services_remove(L :: list(service_id()),
                      Timeout :: api_timeout_milliseconds()) ->
    ok |
    {error, any()}.

services_remove([_ | _] = L, Timeout)
    when ((is_integer(Timeout) andalso
           (Timeout > ?TIMEOUT_DELTA) andalso
           (Timeout =< ?TIMEOUT_MAX_ERLANG)) orelse
          (Timeout =:= infinity)) ->
    cloudi_configurator:services_remove(L, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Restart service instances.===
%% Provide the Service UUIDs for the services that should be restarted.
%% The Service UUID is shown in the output of services/1. When the service
%% is restarted, the old instance is stopped and a new instance is started.
%% During the restart delay, it is possible to lose queued service
%% requests and received asynchronous responses. Keeping the state
%% separate between the service instances is important to prevent failures
%% within the new instance.
%% @end
%%-------------------------------------------------------------------------

-spec services_restart(L :: list(service_id()), 
                       Timeout :: api_timeout_milliseconds()) ->
    ok |
    {error, any()}.

services_restart([_ | _] = L, Timeout)
    when ((is_integer(Timeout) andalso
           (Timeout > ?TIMEOUT_DELTA) andalso
           (Timeout =< ?TIMEOUT_MAX_ERLANG)) orelse
          (Timeout =:= infinity)) ->
    cloudi_configurator:services_restart(L, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Search service instances for matches on the provided service name.===
%% Multiple services may be returned for a single service name.  Only service
%% instances on the local Erlang node are searched.
%% @end
%%-------------------------------------------------------------------------

-spec services_search(ServiceName :: cloudi:service_name(),
                      Timeout :: api_timeout_milliseconds()) ->
    {ok, list({service_id(), #internal{}} |
              {service_id(), #external{}})} |
    {error, any()}.

services_search([_ | _] = ServiceName, Timeout)
    when ((is_integer(Timeout) andalso
           (Timeout > ?TIMEOUT_DELTA) andalso
           (Timeout =< ?TIMEOUT_MAX_ERLANG)) orelse
          (Timeout =:= infinity)) ->
    try cloudi_x_trie:is_pattern(ServiceName) of
        false ->
            cloudi_configurator:services_search(ServiceName, Timeout);
        true ->
            {error, service_name_invalid}
    catch
        exit:badarg ->
            {error, service_name_invalid}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===List all service instances with each service's UUID.===
%% @end
%%-------------------------------------------------------------------------

-spec services(Timeout :: api_timeout_milliseconds()) ->
    {ok, list({service_id(), #internal{}} |
              {service_id(), #external{}})} |
    {error, any()}.

services(Timeout)
    when ((is_integer(Timeout) andalso
           (Timeout > ?TIMEOUT_DELTA) andalso
           (Timeout =< ?TIMEOUT_MAX_ERLANG)) orelse
          (Timeout =:= infinity)) ->
    cloudi_configurator:services(Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Add CloudI nodes.===
%% Explicitly add a CloudI node name, so that services between all other
%% CloudI nodes and the added nodes can send each other service requests.
%% @end
%%-------------------------------------------------------------------------

-spec nodes_add(L :: list(node()),
                Timeout :: api_timeout_milliseconds()) ->
    ok |
    {error, any()}.

nodes_add([_ | _] = L, Timeout)
    when ((is_integer(Timeout) andalso
           (Timeout > ?TIMEOUT_DELTA) andalso
           (Timeout =< ?TIMEOUT_MAX_ERLANG)) orelse
          (Timeout =:= infinity)) ->
    cloudi_configurator:nodes_add(L, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Explicitly remove CloudI nodes.===
%% @end
%%-------------------------------------------------------------------------

-spec nodes_remove(L :: list(node()),
                   Timeout :: api_timeout_milliseconds()) ->
    ok |
    {error, any()}.

nodes_remove([_ | _] = L, Timeout)
    when ((is_integer(Timeout) andalso
           (Timeout > ?TIMEOUT_DELTA) andalso
           (Timeout =< ?TIMEOUT_MAX_ERLANG)) orelse
          (Timeout =:= infinity)) ->
    cloudi_configurator:nodes_remove(L, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===List all the CloudI nodes known to be connected.===
%% @end
%%-------------------------------------------------------------------------

-spec nodes_alive(Timeout :: api_timeout_milliseconds()) ->
    {ok, list(node())} |
    {error, any()}.

nodes_alive(Timeout)
    when ((is_integer(Timeout) andalso
           (Timeout > ?TIMEOUT_DELTA) andalso
           (Timeout =< ?TIMEOUT_MAX_ERLANG)) orelse
          (Timeout =:= infinity)) ->
    cloudi_nodes:alive(Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===List all the CloudI nodes that are disconnected but expected to reconnect.===
%% @end
%%-------------------------------------------------------------------------

-spec nodes_dead(Timeout :: api_timeout_milliseconds()) ->
    {ok, list(node())} |
    {error, any()}.

nodes_dead(Timeout)
    when ((is_integer(Timeout) andalso
           (Timeout > ?TIMEOUT_DELTA) andalso
           (Timeout =< ?TIMEOUT_MAX_ERLANG)) orelse
          (Timeout =:= infinity)) ->
    cloudi_nodes:dead(Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===List both the connected and disconnected CloudI nodes.===
%% @end
%%-------------------------------------------------------------------------

-spec nodes(Timeout :: api_timeout_milliseconds()) ->
    {ok, list(node())} |
    {error, any()}.

nodes(Timeout)
    when ((is_integer(Timeout) andalso
           (Timeout > ?TIMEOUT_DELTA) andalso
           (Timeout =< ?TIMEOUT_MAX_ERLANG)) orelse
          (Timeout =:= infinity)) ->
    cloudi_nodes:nodes(Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Modify the current loglevel.===
%% CloudI uses asynchronous logging with flow control (backpressure
%% handling) to prevent misbehaving services from causing instability.
%% @end
%%-------------------------------------------------------------------------

-spec loglevel_set(Level :: loglevel(),
                   Timeout :: api_timeout_milliseconds()) ->
    ok.

loglevel_set(Level, Timeout)
    when is_atom(Level),
         ((is_integer(Timeout) andalso
           (Timeout > ?TIMEOUT_DELTA) andalso
           (Timeout =< ?TIMEOUT_MAX_ERLANG)) orelse
          (Timeout =:= infinity)) ->
    cloudi_logger:change_loglevel(Level).

%%-------------------------------------------------------------------------
%% @doc
%% ===Redirect the log output.===
%% Redirect all local log output to a remote CloudI node.
%% Use 'undefined' as the node name to log locally.
%% @end
%%-------------------------------------------------------------------------

-spec log_redirect(Node :: undefined | node(),
                   Timeout :: api_timeout_milliseconds()) ->
    ok.

log_redirect(Node, Timeout)
    when is_atom(Node),
         ((is_integer(Timeout) andalso
           (Timeout > ?TIMEOUT_DELTA) andalso
           (Timeout =< ?TIMEOUT_MAX_ERLANG)) orelse
          (Timeout =:= infinity)) ->
    cloudi_nodes:logger_redirect(Node).

%%-------------------------------------------------------------------------
%% @doc
%% ===Add a directory to the CloudI Erlang VM code server's search paths.===
%% The path is always appended to the list of search paths (you should not
%% need to rely on search path order because of unique naming).
%% @end
%%-------------------------------------------------------------------------

-spec code_path_add(Dir :: file:filename(),
                    Timeout :: api_timeout_milliseconds()) ->
    ok |
    {error, any()}.

code_path_add(Dir, Timeout)
    when ((is_integer(Timeout) andalso
           (Timeout > ?TIMEOUT_DELTA) andalso
           (Timeout =< ?TIMEOUT_MAX_ERLANG)) orelse
          (Timeout =:= infinity)) ->
    case code:add_pathz(Dir) of
        true ->
            ok;
        {error, _} = Error ->
            Error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Remove a directory from the CloudI Erlang VM code server's search paths.===
%% This doesn't impact any running services, only services that will be
%% started in the future. 
%% @end
%%-------------------------------------------------------------------------

-spec code_path_remove(Dir :: file:filename(),
                       Timeout :: api_timeout_milliseconds()) ->
    ok |
    {error, any()}.

code_path_remove(Dir, Timeout)
    when ((is_integer(Timeout) andalso
           (Timeout > ?TIMEOUT_DELTA) andalso
           (Timeout =< ?TIMEOUT_MAX_ERLANG)) orelse
          (Timeout =:= infinity)) ->
    case code:del_path(Dir) of
        true ->
            ok;
        false ->
            {error, does_not_exist};
        {error, _} = Error ->
            Error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===List all the CloudI Erlang VM code server search paths.===
%% The order is the same order the directories are searched.
%% @end
%%-------------------------------------------------------------------------

-spec code_path(Timeout :: api_timeout_milliseconds()) ->
    {ok, list(file:filename())}.

code_path(Timeout)
    when ((is_integer(Timeout) andalso
           (Timeout > ?TIMEOUT_DELTA) andalso
           (Timeout =< ?TIMEOUT_MAX_ERLANG)) orelse
          (Timeout =:= infinity)) ->
    {ok, code:get_path()}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

