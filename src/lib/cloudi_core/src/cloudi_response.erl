%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Response==
%%% Response format based on Request type.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2011-2013, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2011-2013 Michael Truog
%%% @version 1.3.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_response).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([new/2,
         new/3,
         new_external/3,
         new_internal/2]).

-include("cloudi_logger.hrl").

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

new(Input, Output) ->
    % TODO: remove old code path
    new_external(Input, Output, native).

new(Input, Output, Endian) ->
    % TODO: remove old code path
    new_external(Input, Output, Endian).

new_external(Input, Output, Endian)
    when is_binary(Input) ->
    % a straight-forward response format for external processes
    case Output of
        ok ->
            <<16#ffffffff:32>>;
        {ok, undefined} ->
            <<16#00000000:32>>;
        {ok, V} when is_binary(V) ->
            convert_binary_to_binary(V, Endian);
        {ok, V} when is_list(V) ->
            convert_list_to_binary(V, Endian);
        {ok, V} when is_atom(V) ->
            convert_string_to_binary(erlang:atom_to_list(V), Endian);
        {error, Reason} ->
            ?LOG_ERROR("response external: ~p", [Reason]),
            <<>>;
        _ when is_list(Output) ->
            convert_list_to_binary(Output, Endian);
        _ when is_binary(Output) ->
            Output
    end;
new_external(Input, Output, _Endian)
    when is_list(Input), is_integer(hd(Input)) ->
    % TODO: remove old code path
    if
        is_list(Output), is_integer(hd(Output)) ->
            Output;
        is_binary(Output) ->
            erlang:binary_to_list(Output);
        true ->
            cloudi_string:term_to_list(Output)
    end;
new_external(_, Output, _Endian) ->
    % TODO: remove old code path
    Output.

new_internal(Input, Output)
    when is_binary(Input) ->
    cloudi_string:term_to_binary(Output);
new_internal(Input, Output)
    when is_list(Input) ->
    cloudi_string:term_to_list(Output);
new_internal(_Input, Output) ->
    Output.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

convert_list_to_binary([], _Endian) ->
    <<16#00000000:32>>;
convert_list_to_binary([[I | _] | _] = L, Endian)
    when is_integer(I) ->
    % list of strings
    Length = erlang:length(L),
    LengthBin = if
        Endian =:= big ->
            <<Length:32/unsigned-integer-big>>;
        Endian =:= little ->
            <<Length:32/unsigned-integer-little>>;
        Endian =:= native ->
            <<Length:32/unsigned-integer-native>>
    end,
    erlang:iolist_to_binary([LengthBin |
        [convert_string_to_binary(E, Endian) || E <- L]]);
convert_list_to_binary([I | _] = L, Endian)
    when is_integer(I) ->
    % string
    convert_string_to_binary(L, Endian);
convert_list_to_binary([{V1, V2} | _] = L, Endian)
    when is_binary(V1), is_binary(V2) ->
    % list of binary pairs
    Length = erlang:length(L),
    LengthBin = if
        Endian =:= big ->
            <<Length:32/unsigned-integer-big>>;
        Endian =:= little ->
            <<Length:32/unsigned-integer-little>>;
        Endian =:= native ->
            <<Length:32/unsigned-integer-native>>
    end,
    erlang:iolist_to_binary([LengthBin |
        [[convert_binary_to_binary(Value1, Endian),
          convert_binary_to_binary(Value2, Endian)] ||
         {Value1, Value2} <- L]]);
convert_list_to_binary([A | _] = L, Endian)
    when is_atom(A) ->
    % list of atoms
    Length = erlang:length(L),
    LengthBin = if
        Endian =:= big ->
            <<Length:32/unsigned-integer-big>>;
        Endian =:= little ->
            <<Length:32/unsigned-integer-little>>;
        Endian =:= native ->
            <<Length:32/unsigned-integer-native>>
    end,
    erlang:iolist_to_binary([LengthBin |
        [convert_string_to_binary(erlang:atom_to_list(E), Endian) ||
         E <- L]]).
    
convert_string_to_binary(S, big) ->
    Length = erlang:length(S),
    Data = erlang:list_to_binary(S),
    <<Length:32/unsigned-integer-big, Data/binary>>;
convert_string_to_binary(S, little) ->
    Length = erlang:length(S),
    Data = erlang:list_to_binary(S),
    <<Length:32/unsigned-integer-little, Data/binary>>;
convert_string_to_binary(S, native) ->
    Length = erlang:length(S),
    Data = erlang:list_to_binary(S),
    <<Length:32/unsigned-integer-native, Data/binary>>.

convert_binary_to_binary(Data, big) ->
    Length = erlang:byte_size(Data),
    <<Length:32/unsigned-integer-big, Data/binary>>;
convert_binary_to_binary(Data, little) ->
    Length = erlang:byte_size(Data),
    <<Length:32/unsigned-integer-little, Data/binary>>;
convert_binary_to_binary(Data, native) ->
    Length = erlang:byte_size(Data),
    <<Length:32/unsigned-integer-native, Data/binary>>.

