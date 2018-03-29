%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Response==
%%% Response format transform.
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2011-2017 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2011-2017 Michael Truog
%%% @version 1.7.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_response).
-author('mjtruog at protonmail dot com').

%% external interface
-export([external_format/2]).

-include("cloudi_core_i_constants.hrl").

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Encode outgoing external response data.===
%% @end
%%-------------------------------------------------------------------------

-spec external_format(Response :: any(),
                      Format :: cloudi_request:external_format()) ->
    binary().

external_format(Response, Format) ->
    if
        Format =:= erlang_string ->
            cloudi_string:term_to_binary(Response);
        Format =:= erlang_term ->
            erlang:term_to_binary(Response);
        Format =:= msgpack ->
            Outgoing = cloudi_x_msgpack:
                       pack(msgpack_response(Response),
                            [{map_format, map}]),
            true = is_binary(Outgoing),
            Outgoing
    end.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

msgpack_response(E)
    when is_atom(E) ->
    erlang:atom_to_binary(E, utf8);
msgpack_response(T)
    when is_tuple(T) ->
    [msgpack_response(E) || E <- erlang:tuple_to_list(T)];
msgpack_response(L)
    when is_list(L) ->
    [msgpack_response(E) || E <- L];
msgpack_response(E) ->
    E.

