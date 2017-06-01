%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI ResponseInfo Creation==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2014-2017 Michael Truog <mjtruog at gmail dot com>
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
%%% @author Michael Truog <mjtruog [at] gmail (dot) com>
%%% @copyright 2014-2017 Michael Truog
%%% @version 1.7.1 {@date} {@time}
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
