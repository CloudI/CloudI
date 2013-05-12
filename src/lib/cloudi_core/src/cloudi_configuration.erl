%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Configuration==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2009-2013, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2009-2013 Michael Truog
%%% @version 1.2.2 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_configuration).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([open/0, open/1,
         acl_add/2, acl_remove/2,
         services_add/3, services_remove/3, services_restart/3, services/1,
         nodes_add/2, nodes_remove/2]).

-include("cloudi_configuration.hrl").
-include("cloudi_logger.hrl").
-include("cloudi_constants.hrl").

-define(CONFIGURATION_FILE_NAME, "cloudi.conf").

% internal service parameters
% (same as the config_service_internal record, but the order is significant
%  since it is used within all configuration data)
-record(internal,
    {
        prefix,
        module,
        args,
        dest_refresh,
        timeout_init,
        timeout_async,
        timeout_sync,
        dest_list_deny,
        dest_list_allow,
        count_process,
        max_r,
        max_t,
        options
    }).
    
% external service parameters
% (same as the config_service_external record, but the order is significant
%  since it is used within all configuration data)
-record(external,
    {
        prefix,
        file_path,
        args,
        env,
        dest_refresh,
        protocol,
        buffer_size,
        timeout_init,
        timeout_async,
        timeout_sync,
        dest_list_deny,
        dest_list_allow,
        count_process,
        count_thread,
        max_r,
        max_t,
        options
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Parse the CloudI configuration file.===
%% ====logging:====
%%   `{logging, [{file, "path/to/log/file"}, {level, Level}]}'
%%
%%   The logging level is specified as an atom:
%%
%%   `off, fatal, error, warn, info, debug, trace'
%%
%% ====services:====
%%   `{services, [{internal, ServiceNamePrefix, ErlangModuleName, ModuleInitializationList, DestinationRefreshMethod, InitializationTimeout, DefaultAsynchronousTimeout, DefaultSynchronousTimeout, DestinationDenyList, DestinationAllowList, ProcessCount, MaxR, MaxT}, {external, ServiceNamePrefix, ExecutableFilePath, ExecutableCommandLineArguments, ExecutableEnvironmentalVariables, DestinationRefreshMethod, Protocol, ProtocolBufferSize, InitializationTimeout, DefaultAsynchronousTimeout, DefaultSynchronousTimeout, DestinationDenyList, DestinationAllowList, ProcessCount, ThreadCount, MaxR, MaxT}]}'
%%
%%   Services configuration defines all the necessary information for the
%%   lifetime of running the service.
%%   Every service defines a name prefix which provides scope for the
%%   service (ServiceNamePrefix) and typically uses the forward slash ('/')
%%   character as a path delimiter (though this convention is not required
%%   for service functionality). An internal service is an Erlang application
%%   or module that exists in the code search path and is started with a list of
%%   initialization arguments (ErlangModuleName and ModuleInitializationList).
%%   An external service is an executable that has integrated with the
%%   CloudI API and is provided as the executable file path
%%   (ExecutableFilePath). An external service also specifies the command line
%%   arguments and the environmental variables
%%   (ExecutableCommandLineArguments and ExecutableEnvironmentalVariables)
%%   that are used when executing the service.
%%
%%   Each service configuration then defines the destination refresh method
%%   (DestinationRefreshMethod) which may be set to: lazy_closest,
%%   lazy_furthest, lazy_random, lazy_local, lazy_remote, lazy_newest,
%%   lazy_oldest, immediate_closest, immediate_furthest, immediate_random,
%%   immediate_local, immediate_remote, immediate_newest, immediate_oldest,
%%   or none. A "lazy" destination refresh
%%   method prefix is used by services that send messages to only
%%   long-lived services and will avoid contention for doing service name
%%   lookups (i.e., the most scalable choice).  An "immediate" destination
%%   refresh method prefix is used by services that send messages to
%%   short-lived services.  A "closest" destination refresh method suffix
%%   always prefers to send to a service on the local machine rather than send
%%   to a remote machine, to minimize latency.  A "furthest" destination
%%   refresh method suffix always prefers to send to a service on a remote
%%   machine, for fault-tolerance.  A "random" destination refresh method
%%   suffix always selects a service randomly, to load-balance the requests
%%   among both local and remote service instances,  A "local" destination
%%   refresh method will only send to local service instances, for minimal
%%   latency.  A "remote" destination refresh method will only send to remote
%%   service instances, to always provide a fault-tolerance guarantee.
%%
%%   The InitializationTimeout timeout specifies how long an internal service
%%   can spend in its cloudi_service_init/3 function or how long an external
%%   service may take to instantiate the CloudI API data structure (for all
%%   of the configured threads). The DefaultAsynchronousTimeout and the
%%   DefaultSynchronousTimeout provide timeouts for any service function calls
%%   that do not specify a timeout.  The DestinationDenyList and the
%%   DestinationAllowList both accept an Access Control List (ACL) which
%%   explicitly denies or allows sending service messages to destinations
%%   that match based on the service name prefix.  Both parameters may be
%%   either "undefined" or a list of service name prefixes (the service name
%%   prefixes may also be supplied as aliases defined in the ACL configuration).
%%
%%   The ProcessCount for an internal service determines how many services with
%%   the configuration will run as Erlang processes. The ProcessCount for an
%%   external service determines how many Operating System processes will be
%%   created with the configuration information. The ThreadCount determines
%%   how many external service threads will be expected to create CloudI API
%%   objects (i.e., to become initialized). The MaxR and MaxT are parameters
%%   to manage the fault-tolerance of the service in the same way as an
%%   Erlang OTP Supervisor manages Erlang processes. The MaxR parameters is the
%%   number of restarts.  The MaxT parameter is the amount of time in seconds
%%   the restarts must occur in, for the service to be considered failed.
%%
%% ====Access Control List (ACL):====
%%
%%   `{acl, [{alias1, ["/service/name/prefix1", "/service/name/prefix2", alias2]}]}'
%%
%%   The DestinationDenyList and DestinationAllowList are both lists that
%%   explicitly deny or allow sending messages from a service (respectively).
%%   The ACL configuration provides a simple way to condense service
%%   configuration based on common service name prefixes.  The ACL atoms
%%   provide short aliases for the literal service name prefixes and may be
%%   used to define other ACLs (in a way that is both acyclic and unordered).
%%
%%   The strings used are typically common service name prefixes, but can
%%   also be patterns with "*" where "**" is forbidden, similar to
%%   service subscriptions.
%%
%% ====nodes:====
%%   `{nodes, [cloudi@hostname1, cloudi@hostname2]}'
%%   `{nodes, automatic}'
%%
%%   Remote CloudI nodes that are started separately
%%   (CloudI operates as a master-less system).  Instead of providing the
%%   exact node names within a list, you can also provide "automatic"
%%   to let nodefinder do automatic node discovery.
%%
%% @end
%%-------------------------------------------------------------------------

-spec open() -> #config{}.

open() ->
    {ok, Terms} = file:consult(?CONFIGURATION_FILE_NAME),
    new(Terms, #config{uuid_generator = uuid:new(self())}).

-spec open(Path :: string()) -> #config{}.

open(Path) when is_list(Path) ->
    {ok, Terms} = file:consult(Path),
    new(Terms, #config{uuid_generator = uuid:new(self())}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Add Access Control List (ACL) aliases (atom -> service name prefixes).===
%% @end
%%-------------------------------------------------------------------------
acl_add([{A, [_ | _]} | _] = Value, #config{acl = ACL} = Config)
    when is_atom(A) ->
    Config#config{acl = acl_lookup_add(Value, ACL)}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Remove Access Control List (ACL) aliases.===
%% @end
%%-------------------------------------------------------------------------
acl_remove([A | _] = Value, #config{acl = ACL} = Config)
    when is_atom(A) ->
    Config#config{acl = lists:foldl(fun(E, D) ->
                                        dict:erase(E, D)
                                    end, ACL, Value)}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Add services based on the configuration format.===
%% @end
%%-------------------------------------------------------------------------
services_add([T | _] = Value,
             #config{uuid_generator = UUID,
                     services = Services,
                     acl = ACL} = Config, Timeout)
    when is_record(T, internal); is_record(T, external) ->
    NextServices = services_acl_update([],
                                       services_validate([], Value, UUID), ACL),
    NewServices = services_add_service(NextServices, [], Timeout),
    Config#config{services = Services ++ NewServices}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Remove services based on their UUID.===
%% @end
%%-------------------------------------------------------------------------
services_remove([UUID | _] = Value,
                #config{services = Services} = Config, Timeout)
    when is_binary(UUID), byte_size(UUID) == 16 ->
    NewServices = lists:foldl(fun(ID, L) ->
        {ServiceList, NewL} = lists:partition(fun(S) ->
            if
                is_record(S, config_service_internal),
                S#config_service_internal.uuid == ID ->
                    true;
                is_record(S, config_service_external),
                S#config_service_external.uuid == ID ->
                    true;
                true ->
                    false
            end
        end, L),
        case ServiceList of
            [] ->
                ok;
            [Service] ->
                cloudi_configurator:service_stop(Service, Timeout)
        end,
        NewL
    end, Services, Value),
    Config#config{services = NewServices}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Restart services based on their UUID.===
%% @end
%%-------------------------------------------------------------------------
services_restart([UUID | _] = Value,
                 #config{services = Services} = Config, Timeout)
    when is_binary(UUID), byte_size(UUID) == 16 ->
    lists:foreach(fun(ID) ->
        ServiceList = lists:filter(fun(S) ->
            if
                is_record(S, config_service_internal),
                S#config_service_internal.uuid == ID ->
                    true;
                is_record(S, config_service_external),
                S#config_service_external.uuid == ID ->
                    true;
                true ->
                    false
            end
        end, Services),
        case ServiceList of
            [] ->
                ok;
            [Service] ->
                cloudi_configurator:service_restart(Service, Timeout)
        end
    end, Value),
    Config.

%%-------------------------------------------------------------------------
%% @doc
%% ===Display the currently running services (including their UUID).===
%% @end
%%-------------------------------------------------------------------------
services(#config{services = Services}) ->
    erlang:list_to_binary(cloudi_string:format("~p", [lists:map(fun(Service) ->
        if
            is_record(Service, config_service_internal) ->
                {Service#config_service_internal.uuid,
                 #internal{prefix =
                               Service#config_service_internal.prefix,
                           module =
                               Service#config_service_internal.module,
                           args =
                               Service#config_service_internal.args,
                           dest_refresh =
                               Service#config_service_internal.dest_refresh,
                           timeout_init =
                               Service#config_service_internal.timeout_init,
                           timeout_async =
                               Service#config_service_internal.timeout_async,
                           timeout_sync =
                               Service#config_service_internal.timeout_sync,
                           dest_list_deny =
                               Service#config_service_internal.dest_list_deny,
                           dest_list_allow =
                               Service#config_service_internal.dest_list_allow,
                           count_process =
                               Service#config_service_internal.count_process,
                           max_r =
                               Service#config_service_internal.max_r,
                           max_t =
                               Service#config_service_internal.max_t,
                           options =
                               Service#config_service_internal.options}};
            is_record(Service, config_service_external) ->
                {Service#config_service_external.uuid,
                 #external{prefix =
                               Service#config_service_external.prefix,
                           file_path =
                               Service#config_service_external.file_path,
                           args =
                               Service#config_service_external.args,
                           env =
                               Service#config_service_external.env,
                           dest_refresh =
                               Service#config_service_external.dest_refresh,
                           protocol =
                               Service#config_service_external.protocol,
                           buffer_size =
                               Service#config_service_external.buffer_size,
                           timeout_init =
                               Service#config_service_external.timeout_init,
                           timeout_async =
                               Service#config_service_external.timeout_async,
                           timeout_sync =
                               Service#config_service_external.timeout_sync,
                           dest_list_deny =
                               Service#config_service_external.dest_list_deny,
                           dest_list_allow =
                               Service#config_service_external.dest_list_allow,
                           count_process =
                               Service#config_service_external.count_process,
                           count_thread =
                               Service#config_service_external.count_thread,
                           max_r =
                               Service#config_service_external.max_r,
                           max_t =
                               Service#config_service_external.max_t,
                           options =
                               Service#config_service_external.options}}
        end
    end, Services)])).

%%-------------------------------------------------------------------------
%% @doc
%% ===Add CloudI nodes.===
%% @end
%%-------------------------------------------------------------------------
nodes_add([A | _] = Value, #config{nodes = Nodes} = Config)
    when is_atom(A) ->
    Config#config{nodes = Nodes ++ Value}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Remove CloudI nodes.===
%% @end
%%-------------------------------------------------------------------------
nodes_remove([A | _] = Value, #config{nodes = Nodes} = Config)
    when is_atom(A) ->
    NewNodes = lists:foldl(fun(N, L) ->
        lists:delete(N, L)
    end, Nodes, Value),
    Config#config{nodes = NewNodes}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

-spec new(list({atom(), any()}), #config{}) -> #config{}.

new([], #config{services = Services, acl = ACL} = Config) ->
    Config#config{services = services_acl_update([], Services, ACL)};

new([{'services', []} | Terms], Config) ->
    new(Terms, Config);
new([{'services', [T | _] = Value} | Terms],
    #config{uuid_generator = UUID} = Config)
    when is_record(T, internal); is_record(T, external) ->
    new(Terms, Config#config{services = services_validate([], Value, UUID)});
new([{'acl', []} | Terms], Config) ->
    new(Terms, Config);
new([{'acl', [{A, [_ | _]} | _] = Value} | Terms], Config)
    when is_atom(A) ->
    new(Terms, Config#config{acl = acl_lookup_new(Value)});
new([{'nodes', automatic} | Terms], Config) ->
    application:start(combonodefinder),
    new(Terms, Config);
new([{'nodes', []} | Terms], Config) ->
    new(Terms, Config);
new([{'nodes', [A | _] = Value} | Terms], Config)
    when is_atom(A) ->
    new(Terms, Config#config{nodes = lists:delete(node(), Value)});
new([{'logging', []} | Terms], Config) ->
    new(Terms, Config);
new([{'logging', [T | _] = Value} | Terms], Config)
    when is_atom(erlang:element(1, T)) ->
    Defaults = [
        {level, (Config#config.logging)#config_logging.level},
        {file, (Config#config.logging)#config_logging.file},
        {redirect, (Config#config.logging)#config_logging.redirect}],
    [Level, File, Redirect] = cloudi_proplists:take_values(Defaults, Value),
    true = ((Level =:= fatal) or (Level =:= error) or (Level =:= warn) or
            (Level =:= info) or (Level =:= debug) or (Level =:= trace) or
            (Level =:= off)),
    true = is_list(File),
    true = is_atom(Redirect),
    new(Terms, Config#config{logging = #config_logging{level = Level,
                                                       file = File,
                                                       redirect = Redirect}}).

services_add_service([], Added, _) ->
    lists:reverse(Added);
services_add_service([Service | Services], Added, Timeout) ->
    services_add_service(Services,
                         [cloudi_configurator:service_start(Service, Timeout) |
                          Added], Timeout).

services_acl_update(Output, [], _) ->
    lists:reverse(Output);
services_acl_update(Output, [Service | L], Lookup)
    when is_record(Service, config_service_internal) ->
    Deny = services_acl_update_list([], Service#config_service_internal.dest_list_deny,
                                Lookup),
    Allow = services_acl_update_list([], Service#config_service_internal.dest_list_allow,
                                 Lookup),
    services_acl_update([Service#config_service_internal{dest_list_deny = Deny,
                                             dest_list_allow = Allow} | Output],
                    L, Lookup);
services_acl_update(Output, [Service | L], Lookup)
    when is_record(Service, config_service_external) ->
    Deny = services_acl_update_list([], Service#config_service_external.dest_list_deny,
                                Lookup),
    Allow = services_acl_update_list([], Service#config_service_external.dest_list_allow,
                                 Lookup),
    services_acl_update([Service#config_service_external{dest_list_deny = Deny,
                                             dest_list_allow = Allow} | Output],
                    L, Lookup).

services_acl_update_list(_, undefined, _) ->
    undefined;
services_acl_update_list(Output, [], _) ->
    Output;
services_acl_update_list(Output, [E | L], Lookup)
    when is_atom(E) ->
    services_acl_update_list(dict:fetch(E, Lookup) ++ Output, L, Lookup);
services_acl_update_list(Output, [E | L], Lookup)
    when is_list(E), is_integer(erlang:hd(E)) ->
    case trie:is_pattern(E) of
        true ->
            services_acl_update_list([E | Output], L, Lookup);
        false ->
            services_acl_update_list([E ++ "*" | Output], L, Lookup)
    end.

services_validate(Output, [], _) ->
    lists:reverse(Output);
services_validate(Output, [Service | L], UUID)
    when is_record(Service, internal),
         is_list(Service#internal.prefix),
         is_list(Service#internal.args),
         is_atom(Service#internal.dest_refresh),
         is_integer(Service#internal.timeout_init),
         is_integer(Service#internal.timeout_async),
         is_integer(Service#internal.timeout_sync),
         is_number(Service#internal.count_process),
         is_integer(Service#internal.max_r),
         is_integer(Service#internal.max_t),
         is_list(Service#internal.options) ->
    true = is_atom(Service#internal.module) orelse
           is_list(Service#internal.module),
    true = (Service#internal.dest_refresh =:= immediate_closest) orelse
           (Service#internal.dest_refresh =:= lazy_closest) orelse
           (Service#internal.dest_refresh =:= immediate_furthest) orelse
           (Service#internal.dest_refresh =:= lazy_furthest) orelse
           (Service#internal.dest_refresh =:= immediate_random) orelse
           (Service#internal.dest_refresh =:= lazy_random) orelse
           (Service#internal.dest_refresh =:= immediate_local) orelse
           (Service#internal.dest_refresh =:= lazy_local) orelse
           (Service#internal.dest_refresh =:= immediate_remote) orelse
           (Service#internal.dest_refresh =:= lazy_remote) orelse
           (Service#internal.dest_refresh =:= immediate_newest) orelse
           (Service#internal.dest_refresh =:= lazy_newest) orelse
           (Service#internal.dest_refresh =:= immediate_oldest) orelse
           (Service#internal.dest_refresh =:= lazy_oldest) orelse
           (Service#internal.dest_refresh =:= none),
    true = Service#internal.timeout_init > 0,
    true = Service#internal.timeout_async > 0,
    true = Service#internal.timeout_sync > 0,
    true = is_list(Service#internal.dest_list_deny) orelse
           (Service#internal.dest_list_deny =:= undefined),
    true = is_list(Service#internal.dest_list_allow) orelse
           (Service#internal.dest_list_allow =:= undefined),
    true = Service#internal.max_r >= 0,
    true = Service#internal.max_t >= 0,
    C = #config_service_internal{
        prefix = Service#internal.prefix,
        module = Service#internal.module,
        args = Service#internal.args,
        dest_refresh = Service#internal.dest_refresh,
        timeout_init = Service#internal.timeout_init,
        timeout_async = Service#internal.timeout_async,
        timeout_sync = Service#internal.timeout_sync,
        dest_list_deny = Service#internal.dest_list_deny,
        dest_list_allow = Service#internal.dest_list_allow,
        count_process = Service#internal.count_process,
        max_r = Service#internal.max_r,
        max_t = Service#internal.max_t,
        options = services_validate_options(
            Service#internal.options
        ),
        uuid = uuid:get_v1(UUID)},
    services_validate([C | Output], L, UUID);
services_validate(Output, [Service | L], UUID)
    when is_record(Service, external),
         is_list(Service#external.prefix),
         is_integer(erlang:hd(Service#external.file_path)),
         is_list(Service#external.file_path),
         is_list(Service#external.args),
         is_list(Service#external.env),
         is_atom(Service#external.dest_refresh),
         is_atom(Service#external.protocol),
         is_integer(Service#external.buffer_size),
         is_integer(Service#external.timeout_init),
         is_integer(Service#external.timeout_async),
         is_integer(Service#external.timeout_sync),
         is_number(Service#external.count_process),
         is_number(Service#external.count_thread),
         is_integer(Service#external.max_r),
         is_integer(Service#external.max_t),
         is_list(Service#external.options) ->
    true = (Service#external.prefix == []) orelse
           is_integer(erlang:hd(Service#external.prefix)),
    true = (Service#external.args == []) orelse
           is_integer(erlang:hd(Service#external.args)),
    true = (Service#external.dest_refresh =:= immediate_closest) orelse
           (Service#external.dest_refresh =:= lazy_closest) orelse
           (Service#external.dest_refresh =:= immediate_furthest) orelse
           (Service#external.dest_refresh =:= lazy_furthest) orelse
           (Service#external.dest_refresh =:= immediate_random) orelse
           (Service#external.dest_refresh =:= lazy_random) orelse
           (Service#external.dest_refresh =:= immediate_local) orelse
           (Service#external.dest_refresh =:= lazy_local) orelse
           (Service#external.dest_refresh =:= immediate_remote) orelse
           (Service#external.dest_refresh =:= lazy_remote) orelse
           (Service#external.dest_refresh =:= immediate_newest) orelse
           (Service#external.dest_refresh =:= lazy_newest) orelse
           (Service#external.dest_refresh =:= immediate_oldest) orelse
           (Service#external.dest_refresh =:= lazy_oldest) orelse
           (Service#external.dest_refresh =:= none),
    true = (Service#external.protocol =:= tcp) orelse
           (Service#external.protocol =:= udp),
    true = Service#external.buffer_size >= 1024, % should be roughly 16436
    true = Service#external.timeout_init > 0,
    true = Service#external.timeout_async > 0,
    true = Service#external.timeout_sync > 0,
    true = is_list(Service#external.dest_list_deny) orelse
           (Service#external.dest_list_deny =:= undefined),
    true = is_list(Service#external.dest_list_allow) orelse
           (Service#external.dest_list_allow =:= undefined),
    true = Service#external.max_r >= 0,
    true = Service#external.max_t >= 0,
    % service options only relevant to internal services
    undefined = proplists:get_value(request_pid_uses,
                                    Service#external.options),
    undefined = proplists:get_value(request_pid_options,
                                    Service#external.options),
    undefined = proplists:get_value(info_pid_uses,
                                    Service#external.options),
    undefined = proplists:get_value(info_pid_options,
                                    Service#external.options),
    undefined = proplists:get_value(duo_mode,
                                    Service#external.options),
    C = #config_service_external{
        prefix = Service#external.prefix,
        file_path = Service#external.file_path,
        args = Service#external.args,
        env = Service#external.env,
        dest_refresh = Service#external.dest_refresh,
        protocol = Service#external.protocol,
        buffer_size = Service#external.buffer_size,
        timeout_init = Service#external.timeout_init,
        timeout_async = Service#external.timeout_async,
        timeout_sync = Service#external.timeout_sync,
        dest_list_deny = Service#external.dest_list_deny,
        dest_list_allow = Service#external.dest_list_allow,
        count_process = Service#external.count_process,
        count_thread = Service#external.count_thread,
        max_r = Service#external.max_r,
        max_t = Service#external.max_t,
        options = services_validate_options(
            Service#external.options
        ),
        uuid = uuid:get_v1(UUID)},
    services_validate([C | Output], L, UUID).

services_validate_options(OptionsList) ->
    Options = #config_service_options{},
    Defaults = [
        {priority_default,
         Options#config_service_options.priority_default},
        {queue_limit,
         Options#config_service_options.queue_limit},
        {dest_refresh_start,
         Options#config_service_options.dest_refresh_start},
        {dest_refresh_delay,
         Options#config_service_options.dest_refresh_delay},
        {request_timeout_adjustment,
         Options#config_service_options.request_timeout_adjustment},
        {response_timeout_adjustment,
         Options#config_service_options.response_timeout_adjustment},
        {request_pid_uses,
         Options#config_service_options.request_pid_uses},
        {request_pid_options,
         Options#config_service_options.request_pid_options},
        {info_pid_uses,
         Options#config_service_options.info_pid_uses},
        {info_pid_options,
         Options#config_service_options.info_pid_options},
        {duo_mode,
         Options#config_service_options.duo_mode}],
    [PriorityDefault, QueueLimit, DestRefreshStart, DestRefreshDelay,
     RequestTimeoutAdjustment, ResponseTimeoutAdjustment,
     RequestPidUses, RequestPidOptions, InfoPidUses, InfoPidOptions,
     DuoMode] =
        cloudi_proplists:take_values(Defaults, OptionsList),
    true = (PriorityDefault >= ?PRIORITY_HIGH) and
           (PriorityDefault =< ?PRIORITY_LOW),
    true = (QueueLimit =:= undefined) orelse
           (is_integer(QueueLimit) and (QueueLimit >= 1)),
    true = is_integer(DestRefreshStart) and (DestRefreshStart > 0),
    true = is_integer(DestRefreshDelay) and (DestRefreshDelay > 0),
    true = is_boolean(RequestTimeoutAdjustment),
    true = is_boolean(ResponseTimeoutAdjustment),
    true = (RequestPidUses =:= infinity) orelse
           (is_integer(RequestPidUses) and (RequestPidUses >= 1)),
    true = (InfoPidUses =:= infinity) orelse
           (is_integer(InfoPidUses) and (InfoPidUses >= 1)),
    true = is_boolean(DuoMode),
    false = (DuoMode =:= true) and (InfoPidUses =/= infinity),
    Options#config_service_options{
        priority_default = PriorityDefault,
        queue_limit = QueueLimit,
        dest_refresh_start = DestRefreshStart,
        dest_refresh_delay = DestRefreshDelay,
        request_timeout_adjustment = RequestTimeoutAdjustment,
        response_timeout_adjustment = ResponseTimeoutAdjustment,
        request_pid_uses = RequestPidUses,
        request_pid_options =
            services_validate_option_pid_options(RequestPidOptions),
        info_pid_uses = InfoPidUses,
        info_pid_options =
            services_validate_option_pid_options(InfoPidOptions),
        duo_mode = DuoMode}.

services_validate_option_pid_options([]) ->
    [link];
services_validate_option_pid_options([_ | _] = L) ->
    PidOptions0 = [link],
    PidOptions1 = case proplists:get_value(fullsweep_after, L) of
        undefined ->
            PidOptions0;
        V1 when is_integer(V1), V1 >= 0 ->
            [{fullsweep_after, V1} | PidOptions0]
    end,
    PidOptions2 = case proplists:get_value(min_heap_size, L) of
        undefined ->
            PidOptions1;
        V2 when is_integer(V2), V2 >= 0 ->
            [{min_heap_size, V2} | PidOptions1]
    end,
    PidOptions3 = case proplists:get_value(min_bin_vheap_size, L) of
        undefined ->
            PidOptions2;
        V3 when is_integer(V3), V3 >= 0 ->
            [{min_bin_vheap_size, V3} | PidOptions2]
    end,
    case proplists:get_value(priority, L) of
        undefined ->
            ok;
        _ ->
            ?LOG_WARN("priority ignored in pid_options", [])
    end,
    PidOptions3.

acl_lookup_new(L) ->
    acl_lookup_add(L, dict:new()).

acl_lookup_add(L, OldLookup) ->
    acl_expand(L, OldLookup, acl_store(L, OldLookup)).

acl_store([], Lookup) ->
    Lookup;
acl_store([{Key, [E | _] = Value} | L], Lookup)
    when is_atom(E); is_list(E) ->
    acl_store(L, dict:store(Key, Value, Lookup)).

acl_expand([], LookupFinal, _) ->
    LookupFinal;
acl_expand([{Key, Value} | L], LookupFinal, LookupConfig) ->
    NewValue = acl_expand_values([], Value, [], Key, LookupConfig),
    acl_expand(L, dict:store(Key, NewValue, LookupFinal), LookupConfig).

acl_expand_values(Output, [], _Path, _Key, _Lookup) ->
    lists:reverse(Output);
acl_expand_values(Output, [E | L], Path, Key, Lookup)
    when is_atom(E) ->
    case lists:member(E, Path) of
        true ->
            ?LOG_ERROR("cyclic ACL definition of ~p with ~p", [Key, E]),
            acl_expand_values(Output, L, Path, Key, Lookup);
        false ->
            case dict:find(E, Lookup) of
                error ->
                    ?LOG_ERROR("ACL definition of ~p missing", [E]),
                    acl_expand_values(Output, L, Path, Key, Lookup);
                {ok, OtherL} ->
                    acl_expand_values(acl_expand_values(Output, OtherL,
                                                        [E | Path], Key,
                                                        Lookup),
                                      L, Path, Key, Lookup)
            end
    end;
acl_expand_values(Output, [E | L], Path, Key, Lookup)
    when is_list(E), is_integer(erlang:hd(E)) ->
    case trie:is_pattern(E) of
        true ->
            acl_expand_values([E | Output], L, Path, Key, Lookup);
        false ->
            acl_expand_values([E ++ "*" | Output], L, Path, Key, Lookup)
    end.

