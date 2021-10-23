%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Service Name Creation and Parsing==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2014-2021 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2014-2021 Michael Truog
%%% @version 2.0.3 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_name).
-author('mjtruog at protonmail dot com').

%% external interface
-export([new/2,
         new/4,
         parse/2,
         parse_with_suffix/2,
         pattern/1,
         suffix/2]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Transform a service name pattern with parameters into an exact service name.===
%% The pattern input can contain consecutive * wildcard characters because
%% they are only used for a template.
%% @end
%%-------------------------------------------------------------------------

-spec new(Pattern :: cloudi:bytestring(),
          Parameters :: list(cloudi:bytestring())) ->
    {ok, cloudi:bytestring()} |
    {error, parameters_ignored | parameter_missing}.

new(Pattern, Parameters) ->
    cloudi_x_trie:pattern2_fill(Pattern, Parameters).

%%-------------------------------------------------------------------------
%% @doc
%% ===Transform a service name pattern with parameters into an exact service name.===
%% The pattern input can contain consecutive * wildcard characters because
%% they are only used for a template.
%% @end
%%-------------------------------------------------------------------------

-spec new(Pattern :: cloudi:bytestring(),
          Parameters :: list(cloudi:bytestring()),
          ParametersSelected :: list(pos_integer()),
          ParametersStrictMatching :: boolean()) ->
    {ok, cloudi:bytestring()} |
    {error,
     parameters_ignored | parameter_missing | parameters_selected_empty |
     {parameters_selected_ignored, list(pos_integer())} |
     {parameters_selected_missing, pos_integer()}}.

new(Pattern, Parameters, ParametersSelected, ParametersStrictMatching) ->
    cloudi_x_trie:pattern2_fill(Pattern, Parameters,
                                ParametersSelected, ParametersStrictMatching).

%%-------------------------------------------------------------------------
%% @doc
%% ===Parse a service name pattern to return parameters.===
%% @end
%%-------------------------------------------------------------------------

-spec parse(Name :: cloudi:bytestring(),
            Pattern :: cloudi:bytestring()) ->
    list(cloudi:nonempty_bytestring()) | error.

parse(Name, Pattern) ->
    cloudi_x_trie:pattern2_parse(Pattern, Name).

%%-------------------------------------------------------------------------
%% @doc
%% ===Parse a service name pattern and return the common suffix.===
%% @end
%%-------------------------------------------------------------------------

-spec parse_with_suffix(Name :: cloudi:bytestring(),
                        Pattern :: cloudi:bytestring()) ->
    {list(cloudi:nonempty_bytestring()), cloudi:bytestring()} | error.

parse_with_suffix(Name, Pattern) ->
    cloudi_x_trie:pattern2_parse(Pattern, Name, with_suffix).

%%-------------------------------------------------------------------------
%% @doc
%% ===Determine if a service name pattern contains wildcard characters.===
%% If not, the service name pattern would only be used with a service name
%% that is an exact match.
%% @end
%%-------------------------------------------------------------------------

-spec pattern(Pattern :: cloudi:bytestring()) ->
    boolean().

pattern(Pattern) ->
    cloudi_x_trie:is_pattern2_bytes(Pattern).

%%-------------------------------------------------------------------------
%% @doc
%% ===Provide the suffix of the service name or service pattern based on the service's configured prefix.===
%% @end
%%-------------------------------------------------------------------------

-spec suffix(Prefix :: cloudi:nonempty_bytestring(),
             NameOrPattern :: cloudi:nonempty_bytestring()) ->
    cloudi:bytestring().

suffix([PrefixC | _] = Prefix, [NameOrPatternC | _] = NameOrPattern)
    when is_integer(PrefixC), is_integer(NameOrPatternC) ->
    case cloudi_x_trie:is_pattern2_bytes(NameOrPattern) of
        true ->
            % handle as a pattern
            suffix_pattern_parse(Prefix, NameOrPattern);
        false ->
            % handle as a name
            case cloudi_x_trie:pattern2_suffix(Prefix, NameOrPattern) of
                error ->
                    erlang:exit(badarg);
                Suffix ->
                    Suffix
            end
    end;
suffix(_, _) ->
    erlang:exit(badarg).

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

suffix_pattern_parse([], Pattern) ->
    Pattern;
suffix_pattern_parse([C | Prefix], [C | Pattern]) ->
    suffix_pattern_parse(Prefix, Pattern);
suffix_pattern_parse([_ | _], _) ->
    erlang:exit(badarg).
