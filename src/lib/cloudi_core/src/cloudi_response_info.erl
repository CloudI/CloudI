%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI ResponseInfo Creation==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2014, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2014 Michael Truog
%%% @version 1.3.3 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_response_info).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([lookup_content_type/0,
         lookup_content_type/1,
         lookup_content_type/2]).

-include("cloudi_response_info.hrl").

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Provide a lookup with common content types.===
%% @end
%%-------------------------------------------------------------------------

-spec lookup_content_type() ->
    cloudi_x_trie:cloudi_x_trie().

lookup_content_type() ->
    lookup_content_type(binary).

%%-------------------------------------------------------------------------
%% @doc
%% ===Provide a lookup with common content types while setting string type of the content type value.===
%% @end
%%-------------------------------------------------------------------------

-spec lookup_content_type(binary | list) ->
    cloudi_x_trie:cloudi_x_trie().

lookup_content_type(binary) ->
    cloudi_x_trie:new(lookup_content_type_data());
lookup_content_type(list) ->
    cloudi_x_trie:new([{E, {T, erlang:binary_to_list(V)}}
                       || {E, {T, V}} <- lookup_content_type_data()]).

%%-------------------------------------------------------------------------
%% @doc
%% ===Perform a lookup on a file extension with temporary data to minimize memory consumption.===
%% @end
%%-------------------------------------------------------------------------

-spec lookup_content_type(Format :: binary | list,
                          FileExtension :: string()) ->
    {ok, {request | attachment, binary() | string()}} | error.

lookup_content_type(Format, FileExtension) ->
    case lists:keyfind(FileExtension, 1, lookup_content_type_data()) of
        {_, {Type, Value} = Result} ->
            if
                Format =:= binary ->
                    {ok, Result};
                Format =:= list ->
                    {ok, {Type, erlang:binary_to_list(Value)}}
            end;
        false ->
            error
    end.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

lookup_content_type_data() ->
    ?LOOKUP_CONTENT_TYPE_DATA.
