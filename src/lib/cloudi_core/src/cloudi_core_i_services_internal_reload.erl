%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Internal Service Reload==
%%% Perform module reloading for internal services, during service
%%% development.
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2013-2017 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2013-2017 Michael Truog
%%% @version 1.7.3 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_core_i_services_internal_reload).
-author('mjtruog at protonmail dot com').

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
        services = [] :: list(atom()),
        reload_start :: cloudi_timestamp:seconds_epoch()
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
        case file:read_file_info(FilePath, [raw, {time, posix}]) of
            {ok, #file_info{mtime = Mtime}}
                when Mtime >= ReloadStartOld,
                     Mtime < ReloadStartNew ->
                cloudi_x_reltool_util:module_reload(Module);
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
    ReloadStart = cloudi_timestamp:seconds_os(),
    erlang:send_after(?SERVICE_INTERNAL_RELOAD, self(), reload),
    ReloadStart.

