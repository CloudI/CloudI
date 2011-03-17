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
%%% @version 0.1.2 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_configuration).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([open/0, open/1,
         acl_add/2, acl_remove/2,
         jobs_add/2, jobs_remove/2,
         nodes_add/2, nodes_remove/2]).

-include("cloudi_configuration.hrl").
-include("cloudi_logger.hrl").
-include("cloudi_constants.hrl").

-define(CONFIGURATION_FILE_NAME, "cloudi.conf").

% internal job parameters (same as config_job_internal)
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
    
% external job parameters (same as config_job_external)
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
%%   `{logging, "path/to/log/file", Level}'
%%
%%   Level is either one of the atoms:
%%
%%   `fatal, error, warn, info, debug, trace'
%%
%% ====jobs:====
%%   `{jobs, [{"cloud_job_uniquename.tag", [Argument1, Argument2, ...], Tasks, UseThreads}]}'
%%
%% ====nodes:====
%%   `{nodes, [node1@hostname]}'
%%
%%   Blah
%%
%% @end
%%-------------------------------------------------------------------------

-spec open() -> #config{}.

open() ->
    {ok, Terms} = file:consult(?CONFIGURATION_FILE_NAME),
    new(Terms, #config{}).

-spec open(Path :: string()) -> #config{}.

open(Path) when is_list(Path) ->
    {ok, Terms} = file:consult(Path),
    new(Terms, #config{}).

acl_add([{A, [_ | _]} | _] = Value, #config{acl = ACL} = Config)
    when is_atom(A) ->
    Config#config{acl = dict:merge(fun(_, L, _) -> L end,
                                   acl_lookup(Value), ACL)}.

acl_remove([A | _] = Value, #config{acl = ACL} = Config)
    when is_atom(A) ->
    Config#config{acl = lists:foldl(fun(E, D) ->
                                        dict:erase(E, D)
                                    end, ACL, Value)}.

jobs_add([T | _] = Value, #config{jobs = Jobs, acl = ACL} = Config)
    when is_record(T, internal); is_record(T, external) ->
    NewJobs = jobs_acl_update([], jobs_validate([], Value), ACL),
    lists:foreach(fun(J) -> cloudi_configurator:job_start(J) end, NewJobs),
    Config#config{jobs = NewJobs ++ Jobs}.

jobs_remove([I | _] = Value, #config{jobs = Jobs} = Config)
    when is_integer(I) ->
    NewJobs = lists:foldl(fun(Index, L) ->
        {NewL1, [Job | NewL2]} = lists:split(Index - 1, L),
        Type = erlang:element(1, Job),
        SupervisorIndex = lists:foldl(fun(J, C) ->
            if
                erlang:element(1, J) == Type ->
                    if
                        config_job_internal == Type ->
                            C + Job#config_job_internal.count_process;
                        config_job_external == Type ->
                            C + Job#config_job_external.count_process
                    end;
                true ->
                    C
            end
        end, 1, NewL1),
        cloudi_configurator:job_stop(SupervisorIndex, Job),
        NewL1 ++ NewL2
    end, Jobs, lists2:rsort(Value)),
    Config#config{jobs = NewJobs}.

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
new([{'jobs', [T | _] = Value} | Terms], Config)
    when is_record(T, internal); is_record(T, external) ->
    new(Terms, Config#config{jobs = jobs_validate([], Value)});
new([{'acl', []} | Terms], Config) ->
    new(Terms, Config);
new([{'acl', [{A, [_ | _]} | _] = Value} | Terms], Config)
    when is_atom(A) ->
    new(Terms, Config#config{acl = acl_lookup(Value)});
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

jobs_validate(Output, []) ->
    lists:reverse(Output);
jobs_validate(Output, [Job | L])
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
           (Job#internal.dest_refresh == lazy_random),
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
                             max_t = Job#internal.max_t},
    jobs_validate([C | Output], L);
jobs_validate(Output, [Job | L])
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
           (Job#external.dest_refresh == lazy_random),
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
                             max_t = Job#external.max_t},
    jobs_validate([C | Output], L).

acl_lookup(L) ->
    acl_expand(L, dict:new(), acl_store(L, dict:new())).

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

