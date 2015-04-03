%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Internal Service Reload==
%%% Perform module reloading for internal services, during service
%%% development.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2013-2015, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2013-2015 Michael Truog
%%% @version 1.4.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_core_i_services_internal_reload).
-author('mjtruog [at] gmail (dot) com').

-behaviour(gen_server).

%% external interface
-export([start_link/0,
         service_add/1,
         service_remove/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,
    {
        services = [],
        reload_start
    }).

-include("cloudi_logger.hrl").
-include("cloudi_core_i_constants.hrl").
-include_lib("kernel/include/file.hrl").

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

service_add(Service)
    when is_atom(Service) ->
    gen_server:cast(?MODULE, {service_add, Service}).

service_remove(Service)
    when is_atom(Service) ->
    gen_server:cast(?MODULE, {service_remove, Service}).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([]) ->
    {ok, #state{reload_start = reload_start()}}.

handle_call(Request, _, State) ->
    ?LOG_WARN("Unknown call \"~p\"", [Request]),
    {stop, cloudi_string:format("Unknown call \"~p\"", [Request]),
     error, State}.

handle_cast({service_add, Service},
            #state{services = Services} = State) ->
    case lists:member(Service, Services) of
        true ->
            {noreply, State};
        false ->
            Valid = case application:get_application(Service) of
                {ok, _} ->
                    true;
                undefined ->
                    case code:is_loaded(Service) of
                        {file, _} ->
                            true;
                        false ->
                            ?LOG_ERROR("service ~p does not exist", [Service]),
                            false
                    end
            end,
            if
                Valid =:= true ->
                    {noreply, State#state{services = [Service | Services]}};
                Valid =:= false ->
                    {noreply, State}
            end
    end;

handle_cast({service_remove, Service},
            #state{services = Services} = State) ->
    NewServices = lists:delete(Service, Services),
    {noreply, State#state{services = NewServices}};

handle_cast(Request, State) ->
    ?LOG_WARN("Unknown cast \"~p\"", [Request]),
    {noreply, State}.

handle_info(reload, #state{services = Services,
                           reload_start = ReloadStartOld} = State) ->
    ModuleFilePaths = lists:flatmap(fun(Service) ->
        case application:get_key(Service, modules) of
            undefined ->
                case code:is_loaded(Service) of
                    {file, ServiceFilePath} ->
                        [{Service, ServiceFilePath}];
                    false ->
                        % ignore missing service module
                        []
                end;
            {ok, Modules} ->
                lists:foldl(fun(M, L) ->
                    case code:is_loaded(M) of
                        {file, ServiceFilePath} ->
                            [{M, ServiceFilePath} | L];
                        false ->
                            % ignore missing application modules
                            L
                    end
                end, [], Modules)
        end
    end, Services),
    ReloadStartNew = reload_start(),
    lists:foreach(fun({Module, FilePath}) ->
        case file:read_file_info(FilePath, [{time, posix}]) of
            {ok, #file_info{mtime = Mtime}}
                when Mtime >= ReloadStartOld,
                     Mtime < ReloadStartNew ->
                % make sure no old code exists
                code:purge(Module),
                % load the new current code
                case code:load_file(Module) of
                    {module, Module} ->
                        % remove the old code
                        code:soft_purge(Module),
                        ok;
                    {error, _} ->
                        ok
                end;
            _ ->
                ok
        end
    end, ModuleFilePaths),
    {noreply, State#state{reload_start = ReloadStartNew}};

handle_info(Request, State) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

reload_start() ->
    ReloadStart = cloudi_timestamp:seconds(),
    erlang:send_after(?SERVICE_INTERNAL_RELOAD, self(), reload),
    ReloadStart.

