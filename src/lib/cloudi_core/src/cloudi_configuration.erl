%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
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
%%% @version 1.3.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_configuration).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([load/1,
         acl_add/2,
         acl_remove/2,
         services_add/3,
         services_remove/3,
         services_restart/3,
         services_search/2,
         services/1,
         nodes_add/2,
         nodes_remove/2]).

-include("cloudi_configuration.hrl").
-include("cloudi_logger.hrl").
-include("cloudi_service_api.hrl").
-include("cloudi_constants.hrl").

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Process the CloudI configuration data.===
%% ====logging:====
%%   `{logging, [{file, "path/to/log/file"}, {level, Level}]}'
%%
%%   The logging level is specified as an atom:
%%
%%   `off, fatal, error, warn, info, debug, trace'
%%
%% ====services:====
%%   `{services, [{internal, ServiceNamePrefix, ErlangModuleName, ModuleInitializationList, DestinationRefreshMethod, InitializationTimeout, DefaultAsynchronousTimeout, DefaultSynchronousTimeout, DestinationDenyList, DestinationAllowList, ProcessCount, MaxR, MaxT, ServiceOptions}, {external, ServiceNamePrefix, ExecutableFilePath, ExecutableCommandLineArguments, ExecutableEnvironmentalVariables, DestinationRefreshMethod, Protocol, ProtocolBufferSize, InitializationTimeout, DefaultAsynchronousTimeout, DefaultSynchronousTimeout, DestinationDenyList, DestinationAllowList, ProcessCount, ThreadCount, MaxR, MaxT, ServiceOptions}]}'
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
%%   to let cloudi_x_nodefinder do automatic node discovery.
%%
%% @end
%%-------------------------------------------------------------------------

-spec load(Path :: string() | list(tuple())) ->
    {ok, #config{}} |
    {error, any()}.

load([I | _] = Path) when is_integer(I) ->
    case file:consult(Path) of
        {ok, Terms} ->
            new(Terms, #config{uuid_generator = uuid_generator()});
        {error, enoent} = Error ->
            error_logger:error_msg("configuration file \"~s\" not found",
                                   [Path]),
            Error;
        {error, eacces} = Error ->
            error_logger:error_msg("configuration file \"~s\" not accessible",
                                   [Path]),
            Error;
        {error, Reason} = Error ->
            error_logger:error_msg("configuration file \"~s\" invalid: ~p",
                                   [Path, Reason]),
            Error
    end;
load([T | _] = Terms) when is_tuple(T) ->
    new(Terms, #config{uuid_generator = uuid_generator()});
load(Data) ->
    {error, {configuration_invalid, Data}}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Add Access Control List (ACL) aliases (atom -> service name prefixes).===
%% @end
%%-------------------------------------------------------------------------

-spec acl_add(Value :: list({atom(), cloudi_service_api:acl()}),
              Config :: #config{}) ->
    {ok, #config{}} |
    {error, any()}.

acl_add([{A, [_ | _]} | _] = Value, #config{acl = ACL} = Config)
    when is_atom(A) ->
    case acl_lookup_add(Value, ACL) of
        {ok, NewACL} ->
            {ok, Config#config{acl = NewACL}};
        {error, _} = Error ->
            Error
    end;
acl_add(Value, _) ->
    {error, {acl_invalid, Value}}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Remove Access Control List (ACL) aliases.===
%% @end
%%-------------------------------------------------------------------------

-spec acl_remove(Value :: list(atom()),
                 Config :: #config{}) ->
    {ok, #config{}} |
    {error, any()}.

acl_remove([A | _] = Value, #config{acl = ACL} = Config)
    when is_atom(A) ->
    NewACL = lists:foldl(fun(E, D) -> dict:erase(E, D) end, ACL, Value),
    {ok, Config#config{acl = NewACL}};
acl_remove(Value, _) ->
    {error, {acls_invalid, Value}}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Add services based on the configuration format.===
%% @end
%%-------------------------------------------------------------------------

-spec services_add(Value :: list(#internal{} | #external{} |
                                 cloudi_service_api:service_proplist()),
                   Config :: #config{},
                   Timeout :: cloudi_service_api:timeout_milliseconds() |
                              infinity) ->
    {ok, list(cloudi_service_api:service_id()), #config{}} |
    {error, any()}.

services_add([T | _] = Value,
             #config{uuid_generator = UUID,
                     services = Services,
                     acl = ACL} = Config, Timeout)
    when is_record(T, internal); is_record(T, external); is_list(T) ->
    case services_validate(Value, UUID) of
        {ok, ValidatedServices, IDs} ->
            case services_acl_update(ValidatedServices, ACL) of
                {ok, NextServices} ->
                    case services_add_service(NextServices, Timeout) of
                        {ok, NewServices} ->
                            {ok, IDs,
                             Config#config{services = Services ++
                                                      NewServices}};
                        {error, _} = Error ->
                            Error
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end;
services_add(Value, _, _) ->
    {error, {services_invalid, Value}}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Remove services based on their UUID.===
%% @end
%%-------------------------------------------------------------------------

-spec services_remove(Value :: list(cloudi_service_api:service_id()),
                      Config :: #config{},
                      Timeout :: cloudi_service_api:timeout_milliseconds() |
                                 infinity) ->
    {ok, #config{}} |
    {error, any()}.

services_remove([ID | _] = Value,
                #config{services = Services} = Config, Timeout)
    when is_binary(ID), byte_size(ID) == 16 ->
    case services_remove_uuid(Value, Services, Timeout) of
        {ok, NewServices} ->
            {ok, Config#config{services = NewServices}};
        {error, _} = Error ->
            Error
    end;
services_remove(Value, _, _) ->
    {error, {services_invalid, Value}}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Restart services based on their UUID.===
%% @end
%%-------------------------------------------------------------------------

-spec services_restart(Value :: list(cloudi_service_api:service_id()),
                       Config :: #config{},
                       Timeout :: cloudi_service_api:timeout_milliseconds() |
                                  infinity) ->
    {ok, #config{}} |
    {error, any()}.

services_restart([ID | _] = Value,
                 #config{services = Services} = Config, Timeout)
    when is_binary(ID), byte_size(ID) == 16 ->
    case services_restart_uuid(Value, Services, Timeout) of
        ok ->
            {ok, Config};
        {error, _} = Error ->
            Error
    end;
services_restart(Value, _, _) ->
    {error, {services_invalid, Value}}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Search services based on their UUID.===
%% @end
%%-------------------------------------------------------------------------

-spec services_search(Value :: list(cloudi_service_api:service_id()),
                      Config :: #config{}) ->
    list({cloudi_service_api:service_id(), #internal{}} |
         {cloudi_service_api:service_id(), #external{}}).

services_search([ID | _] = Value, Config)
    when is_binary(ID), byte_size(ID) == 16 ->
    lists:filter(fun({CheckID, _}) ->
        lists:member(CheckID, Value)
    end, services(Config)).

%%-------------------------------------------------------------------------
%% @doc
%% ===Display the currently running services (including their UUID).===
%% @end
%%-------------------------------------------------------------------------

-spec services(#config{}) ->
    list({cloudi_service_api:service_id(), #internal{}} |
         {cloudi_service_api:service_id(), #external{}}).

services(#config{services = Services}) ->
    lists:map(fun(Service) ->
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
                           options = services_format_options_internal(
                               Service#config_service_internal.options)}};
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
                           options = services_format_options_external(
                               Service#config_service_external.options)}}
        end
    end, Services).

%%-------------------------------------------------------------------------
%% @doc
%% ===Add CloudI nodes.===
%% @end
%%-------------------------------------------------------------------------

-spec nodes_add(Value :: list(node()),
                Config :: #config{}) ->
    {ok, #config{}} |
    {error, any()}.

nodes_add([A | _] = Value, #config{nodes = Nodes} = Config)
    when is_atom(A) ->
    case nodes_validate(Value) of
        ok ->
            {ok, Config#config{nodes = Nodes ++ Value}};
        {error, _} = Error ->
            Error
    end;
nodes_add(Value, _) ->
    {error, {nodes_invalid, Value}}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Remove CloudI nodes.===
%% @end
%%-------------------------------------------------------------------------

-spec nodes_remove(Value :: list(node()),
                   Config :: #config{}) ->
    {ok, #config{}} |
    {error, any()}.

nodes_remove([A | _] = Value, #config{nodes = Nodes} = Config)
    when is_atom(A) ->
    case nodes_remove_elements(Value, Nodes) of
        {ok, NewNodes} ->
            {ok, Config#config{nodes = NewNodes}};
        {error, _} = Error ->
            Error
    end;
nodes_remove(Value, _) ->
    {error, {nodes_invalid, Value}}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

-spec new(Terms :: list({atom(), any()}),
          Config :: #config{}) ->
    {ok, #config{}} |
    {error, any()}.

new([], #config{services = Services, acl = ACL} = Config) ->
    case services_acl_update(Services, ACL) of
        {ok, NewServices} ->
            {ok, Config#config{services = NewServices}};
        {error, _} = Error ->
            Error
    end;
new([{'services', []} | Terms], Config) ->
    new(Terms, Config);
new([{'services', [T | _] = Value} | Terms],
    #config{uuid_generator = UUID} = Config)
    when is_record(T, internal); is_record(T, external); is_list(T) ->
    case services_validate(Value, UUID) of
        {ok, NewServices, _IDs} ->
            new(Terms, Config#config{services = NewServices});
        {error, _} = Error ->
            Error
    end;
new([{'acl', []} | Terms], Config) ->
    new(Terms, Config);
new([{'acl', [{A, [_ | _]} | _] = Value} | Terms], Config)
    when is_atom(A) ->
    case acl_lookup_new(Value) of
        {ok, NewACL} ->
            new(Terms, Config#config{acl = NewACL});
        {error, _} = Error ->
            Error
    end;
new([{'nodes', automatic} | Terms], Config) ->
    case cloudi_x_reltool_util:ensure_application_started(
        cloudi_x_combonodefinder) of
        ok ->
            new(Terms, Config);
        {error, _} = Error ->
            Error
    end;
new([{'nodes', []} | Terms], Config) ->
    new(Terms, Config);
new([{'nodes', [A | _] = Value} | Terms], Config)
    when is_atom(A) ->
    Nodes = lists:delete(node(), lists:usort(Value)),
    case nodes_validate(Nodes) of
        ok ->
            new(Terms, Config#config{nodes = Nodes});
        {error, _} = Error ->
            Error
    end;
new([{'logging', []} | Terms], Config) ->
    new(Terms, Config);
new([{'logging', [T | _] = Value} | Terms], Config)
    when is_atom(element(1, T)) ->
    Defaults = [
        {level, (Config#config.logging)#config_logging.level},
        {file, (Config#config.logging)#config_logging.file},
        {redirect, (Config#config.logging)#config_logging.redirect}],
    case cloudi_proplists:take_values(Defaults, Value) of
        [Level, _, _ | _]
            when not ((Level =:= fatal) orelse (Level =:= error) orelse
                      (Level =:= warn) orelse (Level =:= info) orelse
                      (Level =:= debug) orelse (Level =:= trace) orelse
                      (Level =:= off)) ->
            {error, {logging_level_invalid, Level}};
        [_, File, _ | _]
            when not (is_list(File) andalso is_integer(hd(File))) ->
            {error, {logging_file_invalid, File}};
        [Level, File, Redirect] ->
            if
                Redirect =:= undefined ->
                    new(Terms,
                        Config#config{
                            logging = #config_logging{
                                level = Level,
                                file = File,
                                redirect = Redirect}});
                true ->
                    case nodes_validate([Redirect]) of
                        ok ->
                            new(Terms,
                                Config#config{
                                    logging = #config_logging{
                                        level = Level,
                                        file = File,
                                        redirect = Redirect}});
                        {error, _} = Error ->
                            Error
                    end
            end;
        [_, _, _ | Extra] ->
            {error, {logging_invalid, Extra}}
    end;
new([Term | _], _) ->
    {error, {invalid, Term}}.

uuid_generator() ->
    {ok, MacAddress} = application:get_env(cloudi_core, mac_address),
    cloudi_x_uuid:new(self(), [{timestamp_type, erlang},
                               {mac_address, MacAddress}]).

services_add_service(NextServices, Timeout) ->
    services_add_service(NextServices, [], Timeout).

services_add_service([], Added, _) ->
    {ok, lists:reverse(Added)};
services_add_service([Service | Services], Added, Timeout) ->
    case cloudi_configurator:service_start(Service, Timeout) of
        {ok, NewService} ->
            services_add_service(Services, [NewService | Added], Timeout);
        {error, _} = Error ->
            Error
    end.

services_acl_update([], _) ->
    {ok, []};
services_acl_update([_ | _] = Services, Lookup) ->
    services_acl_update(Services, [], Lookup).

services_acl_update([], Output, _) ->
    {ok, lists:reverse(Output)};
services_acl_update([#config_service_internal{
                        dest_list_deny = Deny,
                        dest_list_allow = Allow} = Service | L],
                    Output, Lookup) ->
    case services_acl_update_list(Deny, [], Lookup) of
        {ok, NewDeny} ->
            case services_acl_update_list(Allow, [], Lookup) of
                {ok, NewAllow} ->
                    services_acl_update(L,
                        [Service#config_service_internal{
                            dest_list_deny = NewDeny,
                            dest_list_allow = NewAllow} | Output], Lookup);
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end;
services_acl_update([#config_service_external{
                        dest_list_deny = Deny,
                        dest_list_allow = Allow} = Service | L],
                    Output, Lookup) ->
    case services_acl_update_list(Deny, [], Lookup) of
        {ok, NewDeny} ->
            case services_acl_update_list(Allow, [], Lookup) of
                {ok, NewAllow} ->
                    services_acl_update(L,
                        [Service#config_service_external{
                            dest_list_deny = NewDeny,
                            dest_list_allow = NewAllow} | Output], Lookup);
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

services_acl_update_list(undefined, _, _) ->
    {ok, undefined};
services_acl_update_list([], Output, _) ->
    {ok, lists:reverse(Output)};
services_acl_update_list([E | L], Output, Lookup)
    when is_atom(E) ->
    case dict:find(E, Lookup) of
        {ok, Value} ->
            services_acl_update_list(L, Value ++ Output, Lookup);
        error ->
            {error, {acl_not_found, E}}
    end;
services_acl_update_list([E | L], Output, Lookup)
    when is_list(E), is_integer(hd(E)) ->
    try cloudi_x_trie:is_pattern(E) of
        true ->
            services_acl_update_list(L, [E | Output], Lookup);
        false ->
            services_acl_update_list(L, [E ++ "*" | Output], Lookup)
    catch
        exit:badarg ->
            {error, {acl_invalid, E}}
    end.

services_format_options_internal(Options) ->
    Defaults = #config_service_options{},
    OptionsList0 = lists:reverse(services_format_options_external(Options)),
    OptionsList1 = if
        Options#config_service_options.application_name /=
        Defaults#config_service_options.application_name ->
            [{application_name,
              Options#config_service_options.application_name} |
             OptionsList0];
        true ->
            OptionsList0
    end,
    OptionsList2 = if
        Options#config_service_options.request_pid_uses /=
        Defaults#config_service_options.request_pid_uses ->
            [{request_pid_uses,
              Options#config_service_options.request_pid_uses} |
             OptionsList1];
        true ->
            OptionsList1
    end,
    OptionsList3 = if
        Options#config_service_options.request_pid_options /= [link] ->
            [{request_pid_options, lists:delete(link,
              Options#config_service_options.request_pid_options)} |
             OptionsList2];
        true ->
            OptionsList2
    end,
    OptionsList4 = if
        Options#config_service_options.info_pid_uses /=
        Defaults#config_service_options.info_pid_uses ->
            [{info_pid_uses,
              Options#config_service_options.info_pid_uses} |
             OptionsList3];
        true ->
            OptionsList3
    end,
    OptionsList5 = if
        Options#config_service_options.info_pid_options /= [link] ->
            [{info_pid_options, lists:delete(link,
              Options#config_service_options.info_pid_options)} |
             OptionsList4];
        true ->
            OptionsList4
    end,
    OptionsList6 = if
        Options#config_service_options.duo_mode /=
        Defaults#config_service_options.duo_mode ->
            [{duo_mode,
              Options#config_service_options.duo_mode} |
             OptionsList5];
        true ->
            OptionsList5
    end,
    OptionsList7 = if
        Options#config_service_options.hibernate /=
        Defaults#config_service_options.hibernate ->
            [{hibernate,
              cloudi_rate_based_configuration:hibernate_format(
                  Options#config_service_options.hibernate)} |
             OptionsList6];
        true ->
            OptionsList6
    end,
    OptionsList8 = if
        Options#config_service_options.reload /=
        Defaults#config_service_options.reload ->
            [{reload,
              Options#config_service_options.reload} |
             OptionsList7];
        true ->
            OptionsList7
    end,
    OptionsList9 = if
        Options#config_service_options.automatic_loading /=
        Defaults#config_service_options.automatic_loading ->
            [{automatic_loading,
              Options#config_service_options.automatic_loading} |
             OptionsList8];
        true ->
            OptionsList8
    end,
    lists:reverse(OptionsList9).

services_format_options_external(Options) ->
    Defaults = #config_service_options{},
    OptionsList0 = [],
    OptionsList1 = if
        Options#config_service_options.priority_default /=
        Defaults#config_service_options.priority_default ->
            [{priority_default,
              Options#config_service_options.priority_default} |
             OptionsList0];
        true ->
            OptionsList0
    end,
    OptionsList2 = if
        Options#config_service_options.queue_limit /=
        Defaults#config_service_options.queue_limit ->
            [{queue_limit,
              Options#config_service_options.queue_limit} |
             OptionsList1];
        true ->
            OptionsList1
    end,
    OptionsList3 = if
        Options#config_service_options.dest_refresh_start /=
        Defaults#config_service_options.dest_refresh_start ->
            [{dest_refresh_start,
              Options#config_service_options.dest_refresh_start} |
             OptionsList2];
        true ->
            OptionsList2
    end,
    OptionsList4 = if
        Options#config_service_options.dest_refresh_delay /=
        Defaults#config_service_options.dest_refresh_delay ->
            [{dest_refresh_delay,
              Options#config_service_options.dest_refresh_delay} |
             OptionsList3];
        true ->
            OptionsList3
    end,
    OptionsList5 = if
        Options#config_service_options.request_timeout_adjustment /=
        Defaults#config_service_options.request_timeout_adjustment ->
            [{request_timeout_adjustment,
              Options#config_service_options.request_timeout_adjustment} |
             OptionsList4];
        true ->
            OptionsList4
    end,
    OptionsList6 = if
        Options#config_service_options.request_timeout_immediate_max /=
        Defaults#config_service_options.request_timeout_immediate_max ->
            [{request_timeout_immediate_max,
              Options#config_service_options.request_timeout_immediate_max} |
             OptionsList5];
        true ->
            OptionsList5
    end,
    OptionsList7 = if
        Options#config_service_options.response_timeout_adjustment /=
        Defaults#config_service_options.response_timeout_adjustment ->
            [{response_timeout_adjustment,
              Options#config_service_options.response_timeout_adjustment} |
             OptionsList6];
        true ->
            OptionsList6
    end,
    OptionsList8 = if
        Options#config_service_options.response_timeout_immediate_max /=
        Defaults#config_service_options.response_timeout_immediate_max ->
            [{response_timeout_immediate_max,
              Options#config_service_options.response_timeout_immediate_max} |
             OptionsList7];
        true ->
            OptionsList7
    end,
    OptionsList9 = if
        Options#config_service_options.count_process_dynamic /=
        Defaults#config_service_options.count_process_dynamic ->
            [{count_process_dynamic,
              cloudi_rate_based_configuration:count_process_dynamic_format(
                  Options#config_service_options.count_process_dynamic)} |
             OptionsList8];
        true ->
            OptionsList8
    end,
    OptionsList10 = if
        Options#config_service_options.scope /= ?SCOPE_DEFAULT ->
            [{scope,
              Options#config_service_options.scope} |
             OptionsList9];
        true ->
            OptionsList9
    end,
    OptionsList11 = if
        Options#config_service_options.monkey_latency /=
        Defaults#config_service_options.monkey_latency ->
            [{monkey_latency,
              cloudi_runtime_testing:monkey_latency_format(
                  Options#config_service_options.monkey_latency)} |
             OptionsList10];
        true ->
            OptionsList10
    end,
    OptionsList12 = if
        Options#config_service_options.monkey_chaos /=
        Defaults#config_service_options.monkey_chaos ->
            [{monkey_chaos,
              cloudi_runtime_testing:monkey_chaos_format(
                  Options#config_service_options.monkey_chaos)} |
             OptionsList11];
        true ->
            OptionsList11
    end,
    lists:reverse(OptionsList12).

-spec services_validate(Services :: list(#internal{} | #external{} |
                                         cloudi_service_api:service_proplist()),
                        UUID :: cloudi_x_uuid:state()) ->
    {ok,
     list(#config_service_internal{} | #config_service_external{}),
     list(cloudi_service_api:service_id())} |
    {error, any()}.

services_validate([_ | _] = Services, UUID) ->
    services_validate(Services, [], [], UUID).

-define(CLOUDI_CORE_SUPPORT_INTERNAL,
services_validate([#internal{prefix = Prefix} | _], _, _, _)
    when not (is_list(Prefix) andalso is_integer(hd(Prefix))) ->
    {error, {service_internal_prefix_invalid, Prefix}};
services_validate([#internal{module = Module} | _], _, _, _)
    when not (is_atom(Module) or
              (is_list(Module) andalso is_integer(hd(Module)))) ->
    {error, {service_internal_module_invalid, Module}};
services_validate([#internal{args = Args} | _], _, _, _)
    when not is_list(Args) ->
    {error, {service_internal_args_invalid, Args}};
services_validate([#internal{dest_refresh = DestRefresh} | _], _, _, _)
    when not (is_atom(DestRefresh) andalso
              ((DestRefresh =:= immediate_closest) orelse
               (DestRefresh =:= lazy_closest) orelse
               (DestRefresh =:= immediate_furthest) orelse
               (DestRefresh =:= lazy_furthest) orelse
               (DestRefresh =:= immediate_random) orelse
               (DestRefresh =:= lazy_random) orelse
               (DestRefresh =:= immediate_local) orelse
               (DestRefresh =:= lazy_local) orelse
               (DestRefresh =:= immediate_remote) orelse
               (DestRefresh =:= lazy_remote) orelse
               (DestRefresh =:= immediate_newest) orelse
               (DestRefresh =:= lazy_newest) orelse
               (DestRefresh =:= immediate_oldest) orelse
               (DestRefresh =:= lazy_oldest) orelse
               (DestRefresh =:= none))) ->
    {error, {service_internal_dest_refresh_invalid, DestRefresh}};
services_validate([#internal{timeout_init = TimeoutInit} | _], _, _, _)
    when not (is_integer(TimeoutInit) andalso
              (TimeoutInit > ?TIMEOUT_DELTA) andalso
              (TimeoutInit =< ?TIMEOUT_MAX)) ->
    {error, {service_internal_timeout_init_invalid, TimeoutInit}};
services_validate([#internal{timeout_async = TimeoutAsync} | _], _, _, _)
    when not (is_integer(TimeoutAsync) andalso
              (TimeoutAsync > ?TIMEOUT_DELTA) andalso
              (TimeoutAsync =< ?TIMEOUT_MAX)) ->
    {error, {service_internal_timeout_async_invalid, TimeoutAsync}};
services_validate([#internal{timeout_sync = TimeoutSync} | _], _, _, _)
    when not (is_integer(TimeoutSync) andalso
              (TimeoutSync > ?TIMEOUT_DELTA) andalso
              (TimeoutSync =< ?TIMEOUT_MAX)) ->
    {error, {service_internal_timeout_sync_invalid, TimeoutSync}};
services_validate([#internal{dest_list_deny = DestListDeny} | _], _, _, _)
    when not (is_list(DestListDeny) or (DestListDeny =:= undefined)) ->
    {error, {service_internal_dest_list_deny_invalid, DestListDeny}};
services_validate([#internal{dest_list_allow = DestListAllow} | _], _, _, _)
    when not (is_list(DestListAllow) or (DestListAllow =:= undefined)) ->
    {error, {service_internal_dest_list_allow_invalid, DestListAllow}};
services_validate([#internal{count_process = CountProcess} | _], _, _, _)
    when not (is_number(CountProcess) andalso CountProcess > 0) ->
    {error, {service_internal_count_process_invalid, CountProcess}};
services_validate([#internal{max_r = MaxR} | _], _, _, _)
    when not (is_integer(MaxR) andalso MaxR >= 0) ->
    {error, {service_internal_max_r_invalid, MaxR}};
services_validate([#internal{max_t = MaxT} | _], _, _, _)
    when not (is_integer(MaxT) andalso MaxT >= 0) ->
    {error, {service_internal_max_t_invalid, MaxT}};
services_validate([#internal{options = Options} | _], _, _, _)
    when not is_list(Options) ->
    {error, {service_internal_options_invalid, Options}};
services_validate([#internal{
                       prefix = Prefix,
                       module = Module,
                       args = Args,
                       dest_refresh = DestRefresh,
                       timeout_init = TimeoutInit,
                       timeout_async = TimeoutAsync,
                       timeout_sync = TimeoutSync,
                       dest_list_deny = DestListDeny,
                       dest_list_allow = DestListAllow,
                       count_process = CountProcess,
                       max_r = MaxR,
                       max_t = MaxT,
                       options = Options} | L],
                  Output, IDs, UUID) ->
    FilePath = if
        is_atom(Module) ->
            undefined;
        is_list(Module) ->
            Module
    end,
    case service_name_valid(Prefix, service_internal_prefix_invalid) of
        ok ->
            case services_validate_options_internal(Options, CountProcess) of
                {ok, NewOptions} ->
                    ID = cloudi_x_uuid:get_v1(UUID),
                    services_validate(L,
                                      [#config_service_internal{
                                           prefix = Prefix,
                                           module = Module,
                                           file_path = FilePath,
                                           args = Args,
                                           dest_refresh = DestRefresh,
                                           timeout_init = TimeoutInit,
                                           timeout_async = TimeoutAsync,
                                           timeout_sync = TimeoutSync,
                                           dest_list_deny = DestListDeny,
                                           dest_list_allow = DestListAllow,
                                           count_process = CountProcess,
                                           max_r = MaxR,
                                           max_t = MaxT,
                                           options = NewOptions,
                                           uuid = ID} | Output],
                                      [ID | IDs],
                                      UUID);
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end).
-define(CLOUDI_CORE_SUPPORT_EXTERNAL,
services_validate([#external{prefix = Prefix} | _], _, _, _)
    when not (is_list(Prefix) andalso is_integer(hd(Prefix))) ->
    {error, {service_external_prefix_invalid, Prefix}};
services_validate([#external{file_path = FilePath} | _], _, _, _)
    when not (is_list(FilePath) andalso is_integer(hd(FilePath))) ->
    {error, {service_external_file_path_invalid, FilePath}};
services_validate([#external{args = Args} | _], _, _, _)
    when not (is_list(Args) andalso
              (Args == "" orelse is_integer(hd(Args)))) ->
    {error, {service_external_args_invalid, Args}};
services_validate([#external{env = Env} | _], _, _, _)
    when not is_list(Env) ->
    {error, {service_external_env_invalid, Env}};
services_validate([#external{dest_refresh = DestRefresh} | _], _, _, _)
    when not (is_atom(DestRefresh) andalso
              ((DestRefresh =:= immediate_closest) orelse
               (DestRefresh =:= lazy_closest) orelse
               (DestRefresh =:= immediate_furthest) orelse
               (DestRefresh =:= lazy_furthest) orelse
               (DestRefresh =:= immediate_random) orelse
               (DestRefresh =:= lazy_random) orelse
               (DestRefresh =:= immediate_local) orelse
               (DestRefresh =:= lazy_local) orelse
               (DestRefresh =:= immediate_remote) orelse
               (DestRefresh =:= lazy_remote) orelse
               (DestRefresh =:= immediate_newest) orelse
               (DestRefresh =:= lazy_newest) orelse
               (DestRefresh =:= immediate_oldest) orelse
               (DestRefresh =:= lazy_oldest) orelse
               (DestRefresh =:= none))) ->
    {error, {service_external_dest_refresh_invalid, DestRefresh}};
services_validate([#external{protocol = Protocol} | _], _, _, _)
    when not ((Protocol =:= default) orelse
              (Protocol =:= tcp) orelse
              (Protocol =:= udp) orelse
              (Protocol =:= local)) ->
    {error, {service_external_protocol_invalid, Protocol}};
services_validate([#external{buffer_size = BufferSize} | _], _, _, _)
    when not ((BufferSize =:= default) orelse
              (is_integer(BufferSize) andalso (BufferSize >= 1024))) ->
    {error, {service_external_buffer_size_invalid, BufferSize}};
services_validate([#external{timeout_init = TimeoutInit} | _], _, _, _)
    when not (is_integer(TimeoutInit) andalso
              (TimeoutInit > ?TIMEOUT_DELTA) andalso
              (TimeoutInit =< ?TIMEOUT_MAX)) ->
    {error, {service_external_timeout_init_invalid, TimeoutInit}};
services_validate([#external{timeout_async = TimeoutAsync} | _], _, _, _)
    when not (is_integer(TimeoutAsync) andalso
              (TimeoutAsync > ?TIMEOUT_DELTA) andalso
              (TimeoutAsync =< ?TIMEOUT_MAX)) ->
    {error, {service_external_timeout_async_invalid, TimeoutAsync}};
services_validate([#external{timeout_sync = TimeoutSync} | _], _, _, _)
    when not (is_integer(TimeoutSync) andalso
              (TimeoutSync > ?TIMEOUT_DELTA) andalso
              (TimeoutSync =< ?TIMEOUT_MAX)) ->
    {error, {service_external_timeout_sync_invalid, TimeoutSync}};
services_validate([#external{dest_list_deny = DestListDeny} | _], _, _, _)
    when not (is_list(DestListDeny) or (DestListDeny =:= undefined)) ->
    {error, {service_external_dest_list_deny_invalid, DestListDeny}};
services_validate([#external{dest_list_allow = DestListAllow} | _], _, _, _)
    when not (is_list(DestListAllow) or (DestListAllow =:= undefined)) ->
    {error, {service_external_dest_list_allow_invalid, DestListAllow}};
services_validate([#external{count_process = CountProcess} | _], _, _, _)
    when not (is_number(CountProcess) andalso CountProcess > 0) ->
    {error, {service_external_count_process_invalid, CountProcess}};
services_validate([#external{count_thread = CountThread} | _], _, _, _)
    when not (is_number(CountThread) andalso CountThread > 0) ->
    {error, {service_external_count_thread_invalid, CountThread}};
services_validate([#external{max_r = MaxR} | _], _, _, _)
    when not (is_integer(MaxR) andalso MaxR >= 0) ->
    {error, {service_external_max_r_invalid, MaxR}};
services_validate([#external{max_t = MaxT} | _], _, _, _)
    when not (is_integer(MaxT) andalso MaxT >= 0) ->
    {error, {service_external_max_t_invalid, MaxT}};
services_validate([#external{options = Options} | _], _, _, _)
    when not is_list(Options) ->
    {error, {service_external_options_invalid, Options}};
services_validate([#external{
                       prefix = Prefix,
                       file_path = FilePath,
                       args = Args,
                       env = Env,
                       dest_refresh = DestRefresh,
                       protocol = Protocol,
                       buffer_size = BufferSize,
                       timeout_init = TimeoutInit,
                       timeout_async = TimeoutAsync,
                       timeout_sync = TimeoutSync,
                       dest_list_deny = DestListDeny,
                       dest_list_allow = DestListAllow,
                       count_process = CountProcess,
                       count_thread = CountThread,
                       max_r = MaxR,
                       max_t = MaxT,
                       options = Options} | L],
                  Output, IDs, UUID) ->
    NewProtocol = if
        Protocol =:= default ->
            local;
        true ->
            Protocol
    end,
    NewBufferSize = if
        BufferSize =:= default ->
            if
                NewProtocol =:= tcp ->
                    16384; % Linux localhost (inet) MTU
                NewProtocol =:= udp ->
                    16384; % Linux localhost (inet) MTU
                NewProtocol =:= local ->
                    16384  % Linux localhost (inet) MTU for testing/comparison
            end;
        true ->
            BufferSize
    end,
    case service_name_valid(Prefix, service_external_prefix_invalid) of
        ok ->
            case services_validate_options_external(Options, CountProcess) of
                {ok, NewOptions} ->
                    ID = cloudi_x_uuid:get_v1(UUID),
                    services_validate(L,
                                      [#config_service_external{
                                           prefix = Prefix,
                                           file_path = FilePath,
                                           args = Args,
                                           env = Env,
                                           dest_refresh = DestRefresh,
                                           protocol = NewProtocol,
                                           buffer_size = NewBufferSize,
                                           timeout_init = TimeoutInit,
                                           timeout_async = TimeoutAsync,
                                           timeout_sync = TimeoutSync,
                                           dest_list_deny = DestListDeny,
                                           dest_list_allow = DestListAllow,
                                           count_process = CountProcess,
                                           count_thread = CountThread,
                                           max_r = MaxR,
                                           max_t = MaxT,
                                           options = NewOptions,
                                           uuid = ID} | Output],
                                      [ID | IDs],
                                      UUID);
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end).

services_validate([], Output, IDs, _) ->
    {ok, lists:reverse(Output), lists:reverse(IDs)};
services_validate([[_| _] = ServicePropList | L], Output, IDs, UUID) ->
    Defaults = [
        {type,                 undefined},
        {prefix,               ?DEFAULT_SERVICE_PREFIX},
        {module,               undefined},
        {file_path,            undefined},
        {args,                 []},
        {env,                  []},
        {dest_refresh,         ?DEFAULT_DEST_REFRESH},
        {protocol,             default},
        {buffer_size,          default},
        {timeout_init,         ?DEFAULT_TIMEOUT_INIT},
        {timeout_async,        ?DEFAULT_TIMEOUT_ASYNC},
        {timeout_sync,         ?DEFAULT_TIMEOUT_SYNC},
        {dest_list_deny,       undefined},
        {dest_list_allow,      undefined},
        {count_process,        1},
        {count_thread,         1},
        {max_r,                ?DEFAULT_MAX_R},
        {max_t,                ?DEFAULT_MAX_T},
        {options,              []}],
    [Type, Prefix, Module,
     FilePath, Args, Env, DestRefresh, Protocol, BufferSize,
     TimeoutInit, TimeoutAsync, TimeoutSync, DestListDeny,
     DestListAllow, CountProcess, CountThread,
     MaxR, MaxT, ConfigurationOptions | Extra] =
        cloudi_proplists:take_values(Defaults, ServicePropList),
    ServiceType = if
        Type =:= undefined ->
            if
                Module /= undefined, FilePath =:= undefined ->
                    internal;
                FilePath /= undefined, Module =:= undefined ->
                    external;
                true ->
                    undefined
            end;
        Type =:= internal, Module /= undefined ->
            internal;
        Type =:= external, FilePath /= undefined ->
            external;
        true ->
            undefined
    end,
    if
        ServiceType =:= undefined ->
            {error, {service_invalid, ServicePropList}};
        Extra /= [] ->
            if
                ServiceType =:= internal ->
                    {error, {service_internal_invalid, Extra}};
                ServiceType =:= external ->
                    {error, {service_external_invalid, Extra}}
            end;
        ServiceType =:= internal ->
            Service = #internal{prefix = Prefix,
                                module = Module,
                                args = Args,
                                dest_refresh = DestRefresh,
                                timeout_init = TimeoutInit,
                                timeout_async = TimeoutAsync,
                                timeout_sync = TimeoutSync,
                                dest_list_deny = DestListDeny,
                                dest_list_allow = DestListAllow,
                                count_process = CountProcess,
                                max_r = MaxR,
                                max_t = MaxT,
                                options = ConfigurationOptions},
            services_validate([Service | L], Output, IDs, UUID);
        ServiceType =:= external ->
            Service = #external{prefix = Prefix,
                                file_path = FilePath,
                                args = Args,
                                env = Env,
                                dest_refresh = DestRefresh,
                                protocol = Protocol,
                                buffer_size = BufferSize,
                                timeout_init = TimeoutInit,
                                timeout_async = TimeoutAsync,
                                timeout_sync = TimeoutSync,
                                dest_list_deny = DestListDeny,
                                dest_list_allow = DestListAllow,
                                count_process = CountProcess,
                                count_thread = CountThread,
                                max_r = MaxR,
                                max_t = MaxT,
                                options = ConfigurationOptions},
            services_validate([Service | L], Output, IDs, UUID)
    end;
?CLOUDI_CORE_SUPPORT_INTERNAL;
?CLOUDI_CORE_SUPPORT_EXTERNAL;
services_validate([Service | _], _, _, _) ->
    {error, {service_invalid, Service}}.

-spec services_validate_options_internal(OptionsList ::
                                             cloudi_service_api:
                                             service_options_internal(),
                                         CountProcess :: pos_integer()) ->
    {ok, #config_service_options{}} |
    {error, any()}.

services_validate_options_internal(OptionsList, CountProcess) ->
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
        {request_timeout_immediate_max,
         Options#config_service_options.request_timeout_immediate_max},
        {response_timeout_adjustment,
         Options#config_service_options.response_timeout_adjustment},
        {response_timeout_immediate_max,
         Options#config_service_options.response_timeout_immediate_max},
        {count_process_dynamic,
         Options#config_service_options.count_process_dynamic},
        {scope,
         Options#config_service_options.scope},
        {monkey_latency,
         Options#config_service_options.monkey_latency},
        {monkey_chaos,
         Options#config_service_options.monkey_chaos},
        {application_name,
         Options#config_service_options.application_name},
        {request_pid_uses,
         Options#config_service_options.request_pid_uses},
        {request_pid_options,
         Options#config_service_options.request_pid_options},
        {info_pid_uses,
         Options#config_service_options.info_pid_uses},
        {info_pid_options,
         Options#config_service_options.info_pid_options},
        {duo_mode,
         Options#config_service_options.duo_mode},
        {hibernate,
         Options#config_service_options.hibernate},
        {reload,
         Options#config_service_options.reload},
        {automatic_loading,
         Options#config_service_options.automatic_loading}],
    case cloudi_proplists:take_values(Defaults, OptionsList) of
        [PriorityDefault, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _, _, _]
        when not ((PriorityDefault >= ?PRIORITY_HIGH) andalso
                  (PriorityDefault =< ?PRIORITY_LOW)) ->
            {error, {service_options_priority_default_invalid,
                     PriorityDefault}};
        [_, QueueLimit, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _, _, _]
        when not ((QueueLimit =:= undefined) orelse
                  (is_integer(QueueLimit) andalso
                   (QueueLimit >= 1))) ->
            {error, {service_options_queue_limit_invalid,
                     QueueLimit}};
        [_, _, DestRefreshStart, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _, _, _]
        when not (is_integer(DestRefreshStart) andalso
                  (DestRefreshStart > ?TIMEOUT_DELTA) andalso
                  (DestRefreshStart =< ?TIMEOUT_MAX_ERLANG)) ->
            {error, {service_options_dest_refresh_start_invalid,
                     DestRefreshStart}};
        [_, _, _, DestRefreshDelay, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _, _, _]
        when not (is_integer(DestRefreshDelay) andalso
                  (DestRefreshDelay > ?TIMEOUT_DELTA) andalso
                  (DestRefreshDelay =< ?TIMEOUT_MAX_ERLANG)) ->
            {error, {service_options_dest_refresh_delay_invalid,
                     DestRefreshDelay}};
        [_, _, _, _, RequestTimeoutAdjustment, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _, _, _]
        when not is_boolean(RequestTimeoutAdjustment) ->
            {error, {service_options_request_timeout_adjustment_invalid,
                     RequestTimeoutAdjustment}};
        [_, _, _, _, _, RequestTimeoutImmediateMax, _, _, _, _,
         _, _, _, _, _, _, _, _, _, _, _]
        when not (is_integer(RequestTimeoutImmediateMax) andalso
                  (RequestTimeoutImmediateMax >= 0) andalso
                  (RequestTimeoutImmediateMax =< ?TIMEOUT_MAX_ERLANG)) ->
            {error, {service_options_request_timeout_immediate_max_invalid,
                     RequestTimeoutImmediateMax}};
        [_, _, _, _, _, _, ResponseTimeoutAdjustment, _, _, _,
         _, _, _, _, _, _, _, _, _, _, _]
        when not is_boolean(ResponseTimeoutAdjustment) ->
            {error, {service_options_response_timeout_adjustment_invalid,
                     ResponseTimeoutAdjustment}};
        [_, _, _, _, _, _, _, ResponseTimeoutImmediateMax, _, _,
         _, _, _, _, _, _, _, _, _, _, _]
        when not (is_integer(ResponseTimeoutImmediateMax) andalso
                  (ResponseTimeoutImmediateMax >= 0) andalso
                  (ResponseTimeoutImmediateMax =< ?TIMEOUT_MAX_ERLANG)) ->
            {error, {service_options_response_timeout_immediate_max_invalid,
                     ResponseTimeoutImmediateMax}};
        [_, _, _, _, _, _, _, _, CountProcessDynamic, _,
         _, _, _, _, _, _, _, _, _, _, _]
        when not ((CountProcessDynamic =:= false) orelse
                  is_list(CountProcessDynamic)) ->
            {error, {service_options_count_process_dynamic_invalid,
                     CountProcessDynamic}};
        [_, _, _, _, _, _, _, _, _, Scope,
         _, _, _, _, _, _, _, _, _, _, _]
        when not is_atom(Scope) ->
            {error, {service_options_scope_invalid,
                     Scope}};
        [_, _, _, _, _, _, _, _, _, _,
         MonkeyLatency, _, _, _, _, _, _, _, _, _, _]
        when not ((MonkeyLatency =:= false) orelse
                  (MonkeyLatency =:= system) orelse
                  is_list(MonkeyLatency)) ->
            {error, {service_options_monkey_latency_invalid,
                     MonkeyLatency}};
        [_, _, _, _, _, _, _, _, _, _,
         _, MonkeyChaos, _, _, _, _, _, _, _, _, _]
        when not ((MonkeyChaos =:= false) orelse
                  (MonkeyChaos =:= system) orelse
                  is_list(MonkeyChaos)) ->
            {error, {service_options_monkey_chaos_invalid,
                     MonkeyChaos}};
        [_, _, _, _, _, _, _, _, _, _,
         _, _, ApplicationName, _, _, _, _, _, _, _, _]
        when not is_atom(ApplicationName) ->
            {error, {service_options_application_name_invalid,
                     ApplicationName}};
        [_, _, _, _, _, _, _, _, _, _,
         _, _, _, RequestPidUses, _, _, _, _, _, _, _]
        when not ((RequestPidUses =:= infinity) orelse
                  (is_integer(RequestPidUses) andalso
                   (RequestPidUses >= 1))) ->
            {error, {service_options_request_pid_uses_invalid,
                     RequestPidUses}};
        [_, _, _, _, _, _, _, _, _, _,
         _, _, _, _, RequestPidOptions, _, _, _, _, _, _]
        when not is_list(RequestPidOptions) ->
            {error, {service_options_request_pid_options_invalid,
                     RequestPidOptions}};
        [_, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, InfoPidUses, _, _, _, _, _]
        when not ((InfoPidUses =:= infinity) orelse
                  (is_integer(InfoPidUses) andalso
                   (InfoPidUses >= 1))) ->
            {error, {service_options_info_pid_uses_invalid,
                     InfoPidUses}};
        [_, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, InfoPidOptions, _, _, _, _]
        when not is_list(InfoPidOptions) ->
            {error, {service_options_info_pid_options_invalid,
                     InfoPidOptions}};
        [_, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, DuoMode, _, _, _]
        when not is_boolean(DuoMode) ->
            {error, {service_options_duo_mode_invalid,
                     DuoMode}};
        [_, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, Hibernate, _, _]
        when not (is_boolean(Hibernate) orelse
                  is_list(Hibernate)) ->
            {error, {service_options_hibernate_invalid,
                     Hibernate}};
        [_, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _, Reload, _]
        when not is_boolean(Reload) ->
            {error, {service_options_reload_invalid,
                     Reload}};
        [_, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _, _, AutomaticLoading]
        when not is_boolean(AutomaticLoading) ->
            {error, {service_options_automatic_loading_invalid,
                     AutomaticLoading}};
        [PriorityDefault, QueueLimit, DestRefreshStart, DestRefreshDelay,
         RequestTimeoutAdjustment, RequestTimeoutImmediateMax,
         ResponseTimeoutAdjustment, ResponseTimeoutImmediateMax,
         CountProcessDynamic, Scope,
         MonkeyLatency, MonkeyChaos, ApplicationName,
         RequestPidUses, RequestPidOptions, InfoPidUses, InfoPidOptions,
         DuoMode, Hibernate, Reload, AutomaticLoading]
        when not ((DuoMode =:= true) andalso
                  (InfoPidUses =/= infinity)) ->
            case services_validate_options_internal_checks(CountProcessDynamic,
                                                           MonkeyLatency,
                                                           MonkeyChaos,
                                                           RequestPidOptions,
                                                           InfoPidOptions,
                                                           Hibernate,
                                                           CountProcess) of
                {ok,
                 NewCountProcessDynamic,
                 NewMonkeyLatency,
                 NewMonkeyChaos,
                 NewRequestPidOptions,
                 NewInfoPidOptions,
                 NewHibernate} ->
                    {ok,
                     Options#config_service_options{
                         priority_default =
                             PriorityDefault,
                         queue_limit =
                             QueueLimit,
                         dest_refresh_start =
                             DestRefreshStart,
                         dest_refresh_delay =
                             DestRefreshDelay,
                         request_timeout_adjustment =
                             RequestTimeoutAdjustment,
                         request_timeout_immediate_max =
                             RequestTimeoutImmediateMax,
                         response_timeout_adjustment =
                             ResponseTimeoutAdjustment,
                         response_timeout_immediate_max =
                             ResponseTimeoutImmediateMax,
                         count_process_dynamic =
                             NewCountProcessDynamic,
                         scope =
                             ?SCOPE_ASSIGN(Scope),
                         monkey_latency =
                             NewMonkeyLatency,
                         monkey_chaos =
                             NewMonkeyChaos,
                         application_name =
                             ApplicationName,
                         request_pid_uses =
                             RequestPidUses,
                         request_pid_options =
                             NewRequestPidOptions,
                         info_pid_uses =
                             InfoPidUses,
                         info_pid_options =
                             NewInfoPidOptions,
                         duo_mode =
                             DuoMode,
                         hibernate =
                             NewHibernate,
                         reload =
                             Reload,
                         automatic_loading =
                             AutomaticLoading}};
                {error, _} = Error ->
                    Error
            end;
        [_, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _, _, _] ->
            {error, {service_options_invalid, OptionsList}};
        [_, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _, _, _ | Extra] ->
            {error, {service_options_invalid, Extra}}
    end.

services_validate_options_internal_checks(CountProcessDynamic,
                                          MonkeyLatency,
                                          MonkeyChaos,
                                          RequestPidOptions,
                                          InfoPidOptions,
                                          Hibernate,
                                          CountProcess) ->
    case services_validate_options_external_checks(CountProcessDynamic,
                                                   MonkeyLatency,
                                                   MonkeyChaos,
                                                   CountProcess) of
        {ok,
         NewCountProcessDynamic,
         NewMonkeyLatency,
         NewMonkeyChaos} ->
            case services_validate_option_pid_options(RequestPidOptions) of
                {ok, NewRequestPidOptions} ->
                    case services_validate_option_pid_options(InfoPidOptions) of
                        {ok, NewInfoPidOptions} ->
                            case cloudi_rate_based_configuration
                                :hibernate_validate(Hibernate) of
                                {ok, NewHibernate} ->
                                    {ok,
                                     NewCountProcessDynamic,
                                     NewMonkeyLatency,
                                     NewMonkeyChaos,
                                     NewRequestPidOptions,
                                     NewInfoPidOptions,
                                     NewHibernate};
                                {error, _} = Error ->
                                    Error
                            end;
                        {error, _} = Error ->
                            Error
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

-spec services_validate_options_external(OptionsList ::
                                             cloudi_service_api:
                                             service_options_external(),
                                         CountProcess :: pos_integer()) ->
    {ok, #config_service_options{}} |
    {error, any()}.

services_validate_options_external(OptionsList, CountProcess) ->
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
        {request_timeout_immediate_max,
         Options#config_service_options.request_timeout_immediate_max},
        {response_timeout_adjustment,
         Options#config_service_options.response_timeout_adjustment},
        {response_timeout_immediate_max,
         Options#config_service_options.response_timeout_immediate_max},
        {count_process_dynamic,
         Options#config_service_options.count_process_dynamic},
        {scope,
         Options#config_service_options.scope},
        {monkey_latency,
         Options#config_service_options.monkey_latency},
        {monkey_chaos,
         Options#config_service_options.monkey_chaos}],
    case cloudi_proplists:take_values(Defaults, OptionsList) of
        [PriorityDefault, _, _, _, _, _, _, _, _, _,
         _, _]
        when not ((PriorityDefault >= ?PRIORITY_HIGH) andalso
                  (PriorityDefault =< ?PRIORITY_LOW)) ->
            {error, {service_options_priority_default_invalid,
                     PriorityDefault}};
        [_, QueueLimit, _, _, _, _, _, _, _, _,
         _, _]
        when not ((QueueLimit =:= undefined) orelse
                  (is_integer(QueueLimit) andalso
                   (QueueLimit >= 1))) ->
            {error, {service_options_queue_limit_invalid,
                     QueueLimit}};
        [_, _, DestRefreshStart, _, _, _, _, _, _, _,
         _, _]
        when not (is_integer(DestRefreshStart) andalso
                  (DestRefreshStart > ?TIMEOUT_DELTA) andalso
                  (DestRefreshStart =< ?TIMEOUT_MAX_ERLANG)) ->
            {error, {service_options_dest_refresh_start_invalid,
                     DestRefreshStart}};
        [_, _, _, DestRefreshDelay, _, _, _, _, _, _,
         _, _]
        when not (is_integer(DestRefreshDelay) andalso
                  (DestRefreshDelay > ?TIMEOUT_DELTA) andalso
                  (DestRefreshDelay =< ?TIMEOUT_MAX_ERLANG)) ->
            {error, {service_options_dest_refresh_delay_invalid,
                     DestRefreshDelay}};
        [_, _, _, _, RequestTimeoutAdjustment, _, _, _, _, _,
         _, _]
        when not is_boolean(RequestTimeoutAdjustment) ->
            {error, {service_options_request_timeout_adjustment_invalid,
                     RequestTimeoutAdjustment}};
        [_, _, _, _, _, RequestTimeoutImmediateMax, _, _, _, _,
         _, _]
        when not (is_integer(RequestTimeoutImmediateMax) andalso
                  (RequestTimeoutImmediateMax >= 0) andalso
                  (RequestTimeoutImmediateMax =< ?TIMEOUT_MAX_ERLANG)) ->
            {error, {service_options_request_timeout_immediate_max_invalid,
                     RequestTimeoutImmediateMax}};
        [_, _, _, _, _, _, ResponseTimeoutAdjustment, _, _, _,
         _, _]
        when not is_boolean(ResponseTimeoutAdjustment) ->
            {error, {service_options_response_timeout_adjustment_invalid,
                     ResponseTimeoutAdjustment}};
        [_, _, _, _, _, _, _, ResponseTimeoutImmediateMax, _, _,
         _, _]
        when not (is_integer(ResponseTimeoutImmediateMax) andalso
                  (ResponseTimeoutImmediateMax >= 0) andalso
                  (ResponseTimeoutImmediateMax =< ?TIMEOUT_MAX_ERLANG)) ->
            {error, {service_options_response_timeout_immediate_max_invalid,
                     ResponseTimeoutImmediateMax}};

        [_, _, _, _, _, _, _, _, CountProcessDynamic, _,
         _, _]
        when not ((CountProcessDynamic =:= false) orelse
                  is_list(CountProcessDynamic)) ->
            {error, {service_options_count_process_dynamic_invalid,
                     CountProcessDynamic}};
        [_, _, _, _, _, _, _, _, _, Scope,
         _, _]
        when not is_atom(Scope) ->
            {error, {service_options_scope_invalid,
                     Scope}};
        [_, _, _, _, _, _, _, _, _, _,
         MonkeyLatency, _]
        when not ((MonkeyLatency =:= false) orelse
                  (MonkeyLatency =:= system) orelse
                  is_list(MonkeyLatency)) ->
            {error, {service_options_monkey_latency_invalid,
                     MonkeyLatency}};
        [_, _, _, _, _, _, _, _, _, _,
         _, MonkeyChaos]
        when not ((MonkeyChaos =:= false) orelse
                  (MonkeyChaos =:= system) orelse
                  is_list(MonkeyChaos)) ->
            {error, {service_options_monkey_chaos_invalid,
                     MonkeyChaos}};
        [PriorityDefault, QueueLimit, DestRefreshStart, DestRefreshDelay,
         RequestTimeoutAdjustment, RequestTimeoutImmediateMax,
         ResponseTimeoutAdjustment, ResponseTimeoutImmediateMax,
         CountProcessDynamic, Scope, MonkeyLatency, MonkeyChaos] ->
            case services_validate_options_external_checks(CountProcessDynamic,
                                                           MonkeyLatency,
                                                           MonkeyChaos,
                                                           CountProcess) of
                {ok,
                 NewCountProcessDynamic,
                 NewMonkeyLatency,
                 NewMonkeyChaos} ->
                    {ok,
                     Options#config_service_options{
                         priority_default =
                             PriorityDefault,
                         queue_limit =
                             QueueLimit,
                         dest_refresh_start =
                             DestRefreshStart,
                         dest_refresh_delay =
                             DestRefreshDelay,
                         request_timeout_adjustment =
                             RequestTimeoutAdjustment,
                         request_timeout_immediate_max =
                             RequestTimeoutImmediateMax,
                         response_timeout_adjustment =
                             ResponseTimeoutAdjustment,
                         response_timeout_immediate_max =
                             ResponseTimeoutImmediateMax,
                         count_process_dynamic =
                             NewCountProcessDynamic,
                         scope =
                             ?SCOPE_ASSIGN(Scope),
                         monkey_latency =
                             NewMonkeyLatency,
                         monkey_chaos =
                             NewMonkeyChaos}};
                {error, _} = Error ->
                    Error
            end;
        [_, _, _, _, _, _, _, _, _, _,
         _, _ | Extra] ->
            {error, {service_options_invalid, Extra}}
    end.

services_validate_options_external_checks(CountProcessDynamic,
                                          MonkeyLatency,
                                          MonkeyChaos,
                                          CountProcess) ->
    case cloudi_rate_based_configuration:
         count_process_dynamic_validate(CountProcessDynamic, CountProcess) of
        {ok, NewCountProcessDynamic} ->
            case cloudi_runtime_testing:
                 monkey_latency_validate(MonkeyLatency) of
                {ok, NewMonkeyLatency} ->
                    case cloudi_runtime_testing:
                         monkey_chaos_validate(MonkeyChaos) of
                        {ok, NewMonkeyChaos} ->
                            {ok,
                             NewCountProcessDynamic,
                             NewMonkeyLatency,
                             NewMonkeyChaos};
                        {error, _} = Error ->
                            Error
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

services_validate_option_pid_options(OptionsList) ->
    services_validate_option_pid_options(OptionsList, [link]).

services_validate_option_pid_options([], Output) ->
    {ok, lists:reverse(Output)};
services_validate_option_pid_options([{fullsweep_after, V} = PidOption |
                                      OptionsList], Output)
    when is_integer(V), V >= 0 ->
    services_validate_option_pid_options(OptionsList, [PidOption | Output]);
services_validate_option_pid_options([{min_heap_size, V} = PidOption |
                                      OptionsList], Output)
    when is_integer(V), V >= 0 ->
    services_validate_option_pid_options(OptionsList, [PidOption | Output]);
services_validate_option_pid_options([{min_bin_vheap_size, V} = PidOption |
                                      OptionsList], Output)
    when is_integer(V), V >= 0 ->
    services_validate_option_pid_options(OptionsList, [PidOption | Output]);
services_validate_option_pid_options([PidOption | _], _) ->
    {error, {service_options_pid_invalid, PidOption}}.

acl_lookup_new(L) ->
    acl_lookup_add(L, dict:new()).

acl_lookup_add(L, OldLookup) ->
    case acl_store(L, OldLookup) of
        {ok, NewLookup} ->
            acl_expand(L, OldLookup, NewLookup);
        {error, _} = Error ->
            Error
    end.

acl_store([], Lookup) ->
    {ok, Lookup};
acl_store([{Key, [E | _] = Value} | L], Lookup)
    when is_atom(E); (is_list(E) andalso is_integer(hd(E))) ->
    acl_store(L, dict:store(Key, Value, Lookup));
acl_store([H | _], _) ->
    {error, {acl_invalid, H}}.

acl_expand([], LookupFinal, _) ->
    {ok, LookupFinal};
acl_expand([{Key, Value} | L], LookupFinal, LookupConfig) ->
    case acl_expand_values(Value, [], [], Key, LookupConfig) of
        {ok, NewValue} ->
            acl_expand(L, dict:store(Key, NewValue, LookupFinal),
                       LookupConfig);
        {error, _} = Error ->
            Error
    end.

acl_expand_values([], Output, _Path, _Key, _Lookup) ->
    {ok, lists:reverse(Output)};
acl_expand_values([E | L], Output, Path, Key, Lookup)
    when is_atom(E) ->
    case lists:member(E, Path) of
        true ->
            {error, {acl_cyclic, Key, E}};
        false ->
            case dict:find(E, Lookup) of
                error ->
                    {error, {acl_not_found, E}};
                {ok, OtherL} ->
                    case acl_expand_values(OtherL, Output,
                                           [E | Path], Key, Lookup) of
                        {ok, NewOutput} ->
                            acl_expand_values(L, NewOutput, Path, Key, Lookup);
                        {error, _} = Error ->
                            Error
                    end
            end
    end;
acl_expand_values([E | L], Output, Path, Key, Lookup)
    when is_list(E), is_integer(hd(E)) ->
    try cloudi_x_trie:is_pattern(E) of
        true ->
            acl_expand_values(L, [E | Output], Path, Key, Lookup);
        false ->
            acl_expand_values(L, [E ++ "*" | Output], Path, Key, Lookup)
    catch
        exit:badarg ->
            {error, {acl_invalid, E}}
    end;
acl_expand_values([E | _], _, _, _, _) ->
    {error, {acl_invalid, E}}.

services_remove_uuid(Value, Services, Timeout) ->
    services_remove_uuid(Value, [], Services, Timeout).

services_remove_uuid([], RemoveServices, Services, Timeout) ->
    case services_remove_all(lists:reverse(RemoveServices),
                             Services, Timeout) of
        {ok, _} = Success ->
            Success;
        {error, _} = Error ->
            Error
    end;
services_remove_uuid([ID | IDs], RemoveServices, Services, Timeout)
    when is_binary(ID), byte_size(ID) == 16 ->
    {ServiceList, NextServices} = lists:partition(fun(S) ->
        (is_record(S, config_service_internal) andalso
         (S#config_service_internal.uuid == ID)) orelse
        (is_record(S, config_service_external) andalso
         (S#config_service_external.uuid == ID))
    end, Services),
    case ServiceList of
        [] ->
            {error, {service_not_found, ID}};
        [Service] ->
            services_remove_uuid(IDs, [Service | RemoveServices],
                                 NextServices, Timeout)
    end;
services_remove_uuid([ID | _], _, _, _) ->
    {error, {service_invalid, ID}}.

services_remove_all([], Services, _) ->
    {ok, Services};
services_remove_all([Service | RemoveServices], Services, Timeout) ->
    Remove = if
        is_record(Service, config_service_internal) ->
            not lists:any(fun(S) ->
                is_record(S, config_service_internal) andalso
                (S#config_service_internal.module == 
                 Service#config_service_internal.module)
            end, Services);
        true ->
            false
    end,
    case cloudi_configurator:service_stop(Service, Remove, Timeout) of
        ok ->
            services_remove_all(RemoveServices, Services, Timeout);
        {error, _} = Error ->
            Error
    end.

services_restart_uuid(Value, Services, Timeout) ->
    services_restart_uuid(Value, [], Services, Timeout).

services_restart_uuid([], RestartServices, _, Timeout) ->
    case services_restart_all(lists:reverse(RestartServices), Timeout) of
        ok ->
            ok;
        {error, _} = Error ->
            Error
    end;
services_restart_uuid([ID | IDs], RestartServices, Services, Timeout)
    when is_binary(ID), byte_size(ID) == 16 ->
    ServiceList = lists:filter(fun(S) ->
        (is_record(S, config_service_internal) andalso
         (S#config_service_internal.uuid == ID)) orelse
        (is_record(S, config_service_external) andalso
         (S#config_service_external.uuid == ID))
    end, Services),
    case ServiceList of
        [] ->
            {error, {service_not_found, ID}};
        [Service] ->
            services_restart_uuid(IDs, [Service | RestartServices],
                                  Services, Timeout)
    end;
services_restart_uuid([ID | _], _, _, _) ->
    {error, {service_invalid, ID}}.

services_restart_all([], _) ->
    ok;
services_restart_all([Service | RestartServices], Timeout) ->
    case cloudi_configurator:service_restart(Service, Timeout) of
        ok ->
            services_restart_all(RestartServices, Timeout);
        {error, _} = Error ->
            Error
    end.

nodes_validate([]) ->
    ok;
nodes_validate([A | As])
    when is_atom(A) ->
    case lists:member($@, erlang:atom_to_list(A)) of
        true ->
            nodes_validate(As);
        false ->
            {error, {node_invalid, A}}
    end;
nodes_validate([A | _]) ->
    {error, {node_invalid, A}}.

nodes_remove_elements([], Nodes) ->
    {ok, Nodes};
nodes_remove_elements([A | As], Nodes)
    when is_atom(A) ->
    case cloudi_lists:delete_checked(A, Nodes) of
        false ->
            {error, {node_not_found, A}};
        NewNodes ->
            nodes_remove_elements(As, NewNodes)
    end;
nodes_remove_elements([A | _], _) ->
    {error, {node_invalid, A}}.

service_name_valid(Name, ErrorReason) ->
    try cloudi_x_trie:is_pattern(Name) of
        _ ->
            ok
    catch
        exit:badarg ->
            {error, {ErrorReason, Name}}
    end.

