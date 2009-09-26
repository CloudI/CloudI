%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Cloudi Data Repository Interface Behavior==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2009, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2009 Michael Truog
%%% @version 0.0.2 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloud_data_interface).
-author('mjtruog [at] gmail (dot) com').

%% behavior callbacks
-export([behaviour_info/1]).

%% behavior external interface
-export([stop/1, do_queries/2]).

-include("cloud_logger.hrl").

%%%------------------------------------------------------------------------
%%% Callback functions from behavior
%%%------------------------------------------------------------------------

-spec behaviour_info(atom()) -> 'undefined' | [{atom(), byte()}].

behaviour_info(callbacks) ->
    [
        {start_link, 2},
        {handle_stop, 1},
        {handle_do_queries, 2}
    ];
behaviour_info(_) ->
    undefined.

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Stop the data module.===
%% @end
%%-------------------------------------------------------------------------

-spec stop(DataTitle :: atom()) -> 'ok' | 'error'.

stop(DataTitle) when is_atom(DataTitle) ->
    call_data(DataTitle, 'handle_stop', [DataTitle],
              error).

%%-------------------------------------------------------------------------
%% @doc
%% ===Do queries in the data module.===
%% Not all the queries provided will be handled by the data module.
%% @end
%%-------------------------------------------------------------------------

-spec do_queries(DataTitle :: atom(),
                 QueryList :: list({atom(), list(integer())})) ->
    {'ok', list({atom(), list(integer())})} |
    {'error', list({atom(), list(integer())})}.

do_queries(DataTitle, QueryList)
    when is_atom(DataTitle), is_list(QueryList) ->
    call_data(DataTitle, 'handle_do_queries', [DataTitle, QueryList],
              {error, QueryList}).

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

call_data(DataTitle, Function, Arguments, Error)
    when is_atom(DataTitle), is_atom(Function), is_list(Arguments) ->
    DataModuleString = 
        string_extensions:before_character($., erlang:atom_to_list(DataTitle)),
    try erlang:list_to_existing_atom(DataModuleString) of
        DataModule ->
            erlang:apply(DataModule, Function, Arguments)
    catch
        error:badarg ->
            ?LOG_ERROR("invalid data title \"~p\"", [DataTitle]),
            Error
    end.

