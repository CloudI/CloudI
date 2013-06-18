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
%%% @version 1.2.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_response).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([new/2]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

new(Input, Output)
    when is_binary(Input) ->
    % a straight-forward response format for external processes
    if
        is_binary(Output) ->
            Output;
        is_list(Output) ->
            convert_list_to_binary(Output);
        is_tuple(Output) ->
            case Output of
                {error, _} ->
                    <<>>
            end;
        Output =:= ok ->
            <<16#ffffffff:32>>
    end;

new(Input, Output)
    when is_list(Input), is_integer(hd(Input)) ->
    if
        is_list(Output), is_integer(hd(Output)) ->
            Output;
        is_binary(Output) ->
            erlang:binary_to_list(Output);
        true ->
            cloudi_string:term_to_list(Output)
    end;

new(_, Output) ->
    Output.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

convert_list_to_binary([]) ->
    <<>>;
convert_list_to_binary([A | _] = L)
    when is_atom(A) ->
    % list of atoms
    Length = erlang:length(L),
    Result = [<<Length:32/unsigned-integer-native>> | lists:map(fun(E) ->
        convert_string_to_binary(erlang:atom_to_list(E))
    end, L)],
    erlang:iolist_to_binary(Result);
convert_list_to_binary([[I | _] | _] = L)
    when is_integer(I) ->
    % list of strings
    Length = erlang:length(L),
    Result = [<<Length:32/unsigned-integer-native>> | lists:map(fun(E) ->
        convert_string_to_binary(E)
    end, L)],
    erlang:iolist_to_binary(Result);
convert_list_to_binary([I | _] = L)
    when is_integer(I) ->
    % string
    convert_string_to_binary(L).
    
convert_string_to_binary(S) ->
    Length = erlang:length(S),
    Data = erlang:list_to_binary(S),
    <<Length:32/unsigned-integer-native, Data/binary>>.

