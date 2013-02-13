%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Proplists Extensions Module==
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
%%% @version 1.2.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_proplists).
-author('mjtruog [at] gmail (dot) com').

-export([delete_all/2,
         partition/2,
         take_value/3,
         take_values/2]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

-type proplist() :: list({atom(), any()}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Delete all the instances of the keys provided.===
%% @end
%%-------------------------------------------------------------------------

-spec delete_all(Keys :: list(atom()),
                 List :: proplist()) -> proplist().

delete_all([], List) ->
    List;

delete_all([Key | Keys], List)
    when is_atom(Key), is_list(List) ->
    delete_all(Keys, proplists:delete(Key, List)).

%%-------------------------------------------------------------------------
%% @doc
%% ===Partition the proplist based on a key.===
%% @end
%%-------------------------------------------------------------------------

-spec partition(Key :: atom(),
                List :: proplist()) -> {proplist(), proplist()}.

partition(Key, List)
    when is_atom(Key), is_list(List) ->
    lists:partition(fun({K, _}) -> K == Key end, List).

%%-------------------------------------------------------------------------
%% @doc
%% ===Remove a key from the proplist.===
%% Use the default value if the key does not exist
%% @end
%%-------------------------------------------------------------------------

-spec take_value(Key :: atom(),
                 List :: proplist(),
                 Default :: any()) -> {any(), proplist()}.

take_value(Key, List, Default)
    when is_atom(Key), is_list(List) ->
    case lists:keytake(Key, 1, List) of
        false ->
            {Default, List};
        {value, {Key, Value}, RemainingList} ->
            {Value, RemainingList}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Remove many keys from the proplist.===
%% The defaults are provided as a proplist
%% @end
%%-------------------------------------------------------------------------

-spec take_values(DefaultList :: proplist(),
                  List :: proplist()) -> list().

take_values(DefaultList, List)
    when is_list(DefaultList), is_list(List) ->
    take_values([], DefaultList, List).

take_values(Result, [], List)
    when is_list(Result), is_list(List) ->
    lists:reverse(Result) ++ List;

take_values(Result, [{Key, Default} | DefaultList], List)
    when is_list(Result), is_atom(Key), is_list(List) ->
    case lists:keytake(Key, 1, List) of
        false ->
            take_values([Default | Result], DefaultList, List);
        {value, {Key, Value}, RemainingList} ->
            take_values([Value | Result], DefaultList, RemainingList)
    end.

