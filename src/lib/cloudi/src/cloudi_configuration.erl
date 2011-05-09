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
%%% Copyright (c) 2009-2011, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2009-2011 Michael Truog
%%% @version 0.1.4 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_configuration).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([open/0, open/1,
         acl_add/2, acl_remove/2,
         jobs_add/2, jobs_remove/2, jobs/1,
         nodes_add/2, nodes_remove/2]).

-include("cloudi_configuration.hrl").
-include("cloudi_logger.hrl").
-include("cloudi_constants.hrl").

-define(CONFIGURATION_FILE_NAME, "cloudi.conf").

% internal job parameters
% (same as the config_job_internal record, but the order is significant
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
        max_t
    }).
    
% external job parameters
% (same as the config_job_external record, but the order is significant
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
        max_t
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
%%   Level is either one of the atoms:
%%
%%   `off, fatal, error, warn, info, debug, trace'
%%
%% ====jobs:====
%%   `{jobs, [{internal, ServiceNamePrefix, ErlangModuleName, ModuleInitializationList, DestinationRefreshMethod, InitializationTimeout, DefaultAsynchronousTimeout, DefaultSynchronousTimeout, DestinationDenyList, DestinationAllowList, ProcessCount, MaxR, MaxT}, {external, ServiceNamePrefix, ExecutableFilePath, ExecutableCommandLineArguments, ExecutableEnvironmentalVariables, DestinationRefreshMethod, Protocol, ProtocolBufferSize, InitializationTimeout, DefaultAsynchronousTimeout, DefaultSynchronousTimeout, DestinationDenyList, DestinationAllowList, ProcessCount, ThreadCount, MaxR, MaxT}]}'
%%
%%   Job configuration defines all the necessary information for the lifetime
%%   of running the job, which may be a service or a short-lived task.
%%   Every job defines a service name prefix which provides scope for the job
%%   (ServiceNamePrefix) and typically uses the forward slash ('/')
%%   character as a path delimiter (though this convention is not required
%%   for service functionality). An internal job is an Erlang module that
%%   exists in the code search path and is started with a list of
%%   initialization arguments (ErlangModuleName and ModuleInitializationList).
%%   An external job is an executable that has integrated with the CloudI API
%%   and is provided as the executable file path (ExecutableFilePath).
%%   An external job also specifies the command line arguments and the
%%   environmental variables (ExecutableCommandLineArguments and
%%   ExecutableEnvironmentalVariables) that are used when executing the job.
%%   Currently, the ThreadCount, Protocol, and ProtocolBufferSize are 
%%   typically provided as command line arguments to the executable, though
%%   they could be provided as environmental variables or be hard-coded
%%   within the job (least desirable choice).
%%
%%   Each job configuration then defines the destination refresh method
%%   (DestinationRefreshMethod) which may be set to: lazy_closest, lazy_random,
%%   immediate_closest, immediate_random, or none. A "lazy" destination refresh
%%   method prefix is used by services that send messages to only
%%   long-lived services and will avoid contention for doing service name
%%   lookups (i.e., the most scalable choice).  An "immediate" destination
%%   refresh method prefix is used by services that send messages to
%%   short-lived services.  A "closest" destination refresh method suffix
%%   always prefers to send to a service (with the same service name) on the
%%   local machine rather than send to a remote machine.  A "random"
%%   destination refresh method suffix always selects a service randomly,
%%   so the service message is uniformly distributed among all services that
%%   have subscribed to the same service name.
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
%% ====nodes:====
%%   `{nodes, [cloudi@hostname1, cloudi@hostname2]}'
%%
%%   Remote CloudI nodes that are started separately
%%   (CloudI operates as a master-less system).
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

acl_add([{A, [_ | _]} | _] = Value, #config{acl = ACL} = Config)
    when is_atom(A) ->
    Config#config{acl = acl_lookup_add(Value, ACL)}.

acl_remove([A | _] = Value, #config{acl = ACL} = Config)
    when is_atom(A) ->
    Config#config{acl = lists:foldl(fun(E, D) ->
                                        dict:erase(E, D)
                                    end, ACL, Value)}.

jobs_add([T | _] = Value, #config{uuid_generator = UUID,
                                  jobs = Jobs,
                                  acl = ACL} = Config)
    when is_record(T, internal); is_record(T, external) ->
    NewJobs = jobs_acl_update([], jobs_validate([], Value, UUID), ACL),
    lists:foreach(fun(J) -> cloudi_configurator:job_start(J) end, NewJobs),
    Config#config{jobs = Jobs ++ NewJobs}.

jobs_remove([UUID | _] = Value, #config{jobs = Jobs} = Config)
    when is_binary(UUID), byte_size(UUID) == 16 ->
    NewJobs = lists:foldl(fun(ID, L) ->
        {[Job], NewL} = lists:partition(fun(J) ->
            if
                is_record(J, config_job_internal),
                J#config_job_internal.uuid == ID ->
                    true;
                is_record(J, config_job_external),
                J#config_job_external.uuid == ID ->
                    true;
                true ->
                    false
            end
        end, L),
        cloudi_configurator:job_stop(Job),
        NewL
    end, Jobs, lists2:rsort(Value)),
    Config#config{jobs = NewJobs}.

jobs(#config{jobs = Jobs}) ->
    erlang:list_to_binary(string2:format("~p", [lists:map(fun(Job) ->
        if
            is_record(Job, config_job_internal) ->
                {Job#config_job_internal.uuid,
                 #internal{prefix = Job#config_job_internal.prefix,
                           module = Job#config_job_internal.module,
                           args = Job#config_job_internal.args,
                           dest_refresh = Job#config_job_internal.dest_refresh,
                           timeout_init = Job#config_job_internal.timeout_init,
                           timeout_async =
                               Job#config_job_internal.timeout_async,
                           timeout_sync =
                               Job#config_job_internal.timeout_sync,
                           dest_list_deny =
                               Job#config_job_internal.dest_list_deny,
                           dest_list_allow =
                               Job#config_job_internal.dest_list_allow,
                           count_process =
                               Job#config_job_internal.count_process,
                           max_r = Job#config_job_internal.max_r,
                           max_t = Job#config_job_internal.max_t}};
            is_record(Job, config_job_external) ->
                {Job#config_job_external.uuid,
                 #external{prefix = Job#config_job_external.prefix,
                           file_path = Job#config_job_external.file_path,
                           args = Job#config_job_external.args,
                           env = Job#config_job_external.env,
                           dest_refresh = Job#config_job_external.dest_refresh,
                           protocol = Job#config_job_external.protocol,
                           buffer_size = Job#config_job_external.buffer_size,
                           timeout_init = Job#config_job_external.timeout_init,
                           timeout_async =
                               Job#config_job_external.timeout_async,
                           timeout_sync =
                               Job#config_job_external.timeout_sync,
                           dest_list_deny =
                               Job#config_job_external.dest_list_deny,
                           dest_list_allow =
                               Job#config_job_external.dest_list_allow,
                           count_process =
                               Job#config_job_external.count_process,
                           count_thread = Job#config_job_external.count_thread,
                           max_r = Job#config_job_external.max_r,
                           max_t = Job#config_job_external.max_t}}
        end
    end, Jobs)])).

nodes_add([A | _] = Value, #config{nodes = Nodes} = Config)
    when is_atom(A) ->
    Config#config{nodes = Nodes ++ Value}.

nodes_remove([A | _] = Value, #config{nodes = Nodes} = Config)
    when is_atom(A) ->
    NewNodes = lists:foldl(fun(N, L) ->
        lists:delete(N, L)
    end, Nodes, Value),
    Config#config{nodes = NewNodes}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

new([], #config{jobs = Jobs, acl = ACL} = Config) ->
    Config#config{jobs = jobs_acl_update([], Jobs, ACL)};

new([{'jobs', []} | Terms], Config) ->
    new(Terms, Config);
new([{'jobs', [T | _] = Value} | Terms],
    #config{uuid_generator = UUID} = Config)
    when is_record(T, internal); is_record(T, external) ->
    new(Terms, Config#config{jobs = jobs_validate([], Value, UUID)});
new([{'acl', []} | Terms], Config) ->
    new(Terms, Config);
new([{'acl', [{A, [_ | _]} | _] = Value} | Terms], Config)
    when is_atom(A) ->
    new(Terms, Config#config{acl = acl_lookup_new(Value)});
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
        {file, (Config#config.logging)#config_logging.file}],
    [Level, File] = proplists2:take_values(Defaults, Value),
    new(Terms, Config#config{logging = #config_logging{level = Level,
                                                       file = File}}).

jobs_acl_update(Output, [], _) ->
    lists:reverse(Output);
jobs_acl_update(Output, [Job | L], Lookup)
    when is_record(Job, config_job_internal) ->
    Deny = jobs_acl_update_list([], Job#config_job_internal.dest_list_deny,
                                Lookup),
    Allow = jobs_acl_update_list([], Job#config_job_internal.dest_list_allow,
                                 Lookup),
    jobs_acl_update([Job#config_job_internal{dest_list_deny = Deny,
                                             dest_list_allow = Allow} | Output],
                    L, Lookup);
jobs_acl_update(Output, [Job | L], Lookup)
    when is_record(Job, config_job_external) ->
    Deny = jobs_acl_update_list([], Job#config_job_external.dest_list_deny,
                                Lookup),
    Allow = jobs_acl_update_list([], Job#config_job_external.dest_list_allow,
                                 Lookup),
    jobs_acl_update([Job#config_job_external{dest_list_deny = Deny,
                                             dest_list_allow = Allow} | Output],
                    L, Lookup).

jobs_acl_update_list(_, undefined, _) ->
    undefined;
jobs_acl_update_list(Output, [], _) ->
    Output;
jobs_acl_update_list(Output, [E | L], Lookup)
    when is_atom(E) ->
    jobs_acl_update_list(dict:fetch(E, Lookup) ++ Output, L, Lookup);
jobs_acl_update_list(Output, [E | L], Lookup)
    when is_list(E), is_integer(erlang:hd(E)) ->
    jobs_acl_update_list([E | Output], L, Lookup).

jobs_validate(Output, [], _) ->
    lists:reverse(Output);
jobs_validate(Output, [Job | L], UUID)
    when is_record(Job, internal),
         is_list(Job#internal.prefix),
         is_atom(Job#internal.module),
         is_list(Job#internal.args),
         is_atom(Job#internal.dest_refresh),
         is_integer(Job#internal.timeout_init),
         is_integer(Job#internal.timeout_async),
         is_integer(Job#internal.timeout_sync),
         is_integer(Job#internal.count_process),
         is_integer(Job#internal.max_r),
         is_integer(Job#internal.max_t) ->
    true = (Job#internal.dest_refresh == immediate_closest) or
           (Job#internal.dest_refresh == lazy_closest) or
           (Job#internal.dest_refresh == immediate_random) or
           (Job#internal.dest_refresh == lazy_random) or
           (Job#internal.dest_refresh == none),
    true = Job#internal.timeout_init > 0,
    true = Job#internal.timeout_async > ?TIMEOUT_DELTA,
    true = Job#internal.timeout_sync > ?TIMEOUT_DELTA,
    true = is_list(Job#internal.dest_list_deny) or
           (Job#internal.dest_list_deny == undefined),
    true = is_list(Job#internal.dest_list_allow) or
           (Job#internal.dest_list_allow == undefined),
    true = Job#internal.count_process >= 1,
    true = Job#internal.max_r >= 0,
    true = Job#internal.max_t >= 0,
    C = #config_job_internal{prefix = Job#internal.prefix,
                             module = Job#internal.module,
                             args = Job#internal.args,
                             dest_refresh = Job#internal.dest_refresh,
                             timeout_init = Job#internal.timeout_init,
                             timeout_async = Job#internal.timeout_async,
                             timeout_sync = Job#internal.timeout_sync,
                             dest_list_deny = Job#internal.dest_list_deny,
                             dest_list_allow = Job#internal.dest_list_allow,
                             count_process = Job#internal.count_process,
                             max_r = Job#internal.max_r,
                             max_t = Job#internal.max_t,
                             uuid = uuid:get_v1(UUID)},
    jobs_validate([C | Output], L, UUID);
jobs_validate(Output, [Job | L], UUID)
    when is_record(Job, external),
         is_list(Job#external.prefix),
         is_integer(erlang:hd(Job#external.file_path)),
         is_list(Job#external.file_path),
         is_list(Job#external.args),
         is_list(Job#external.env),
         is_atom(Job#external.dest_refresh),
         is_atom(Job#external.protocol),
         is_integer(Job#external.buffer_size),
         is_integer(Job#external.timeout_init),
         is_integer(Job#external.timeout_async),
         is_integer(Job#external.timeout_sync),
         is_integer(Job#external.count_process),
         is_integer(Job#external.count_thread),
         is_integer(Job#external.max_r),
         is_integer(Job#external.max_t) ->
    true = (Job#external.prefix == []) or
           is_integer(erlang:hd(Job#external.prefix)),
    true = (Job#external.args == []) or
           is_integer(erlang:hd(Job#external.args)),
    true = (Job#external.dest_refresh == immediate_closest) or
           (Job#external.dest_refresh == lazy_closest) or
           (Job#external.dest_refresh == immediate_random) or
           (Job#external.dest_refresh == lazy_random) or
           (Job#external.dest_refresh == none),
    true = (Job#external.protocol == tcp) or
           (Job#external.protocol == udp),
    true = Job#external.buffer_size >= 1024, % should be roughly 16436
    true = Job#external.timeout_init > 0,
    true = Job#external.timeout_async > ?TIMEOUT_DELTA,
    true = Job#external.timeout_sync > ?TIMEOUT_DELTA,
    true = is_list(Job#external.dest_list_deny) or
           (Job#external.dest_list_deny == undefined),
    true = is_list(Job#external.dest_list_allow) or
           (Job#external.dest_list_allow == undefined),
    true = Job#external.count_process >= 1,
    true = Job#external.count_thread >= 1,
    true = Job#external.max_r >= 0,
    true = Job#external.max_t >= 0,
    C = #config_job_external{prefix = Job#external.prefix,
                             file_path = Job#external.file_path,
                             args = Job#external.args,
                             env = Job#external.env,
                             dest_refresh = Job#external.dest_refresh,
                             protocol = Job#external.protocol,
                             buffer_size = Job#external.buffer_size,
                             timeout_init = Job#external.timeout_init,
                             timeout_async = Job#external.timeout_async,
                             timeout_sync = Job#external.timeout_sync,
                             dest_list_deny = Job#external.dest_list_deny,
                             dest_list_allow = Job#external.dest_list_allow,
                             count_process = Job#external.count_process,
                             count_thread = Job#external.count_thread,
                             max_r = Job#external.max_r,
                             max_t = Job#external.max_t,
                             uuid = uuid:get_v1(UUID)},
    jobs_validate([C | Output], L, UUID).

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
    acl_expand_values([E | Output], L, Path, Key, Lookup).

