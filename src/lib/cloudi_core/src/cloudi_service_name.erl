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
%%% Copyright (c) 2014-2017 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2014-2017 Michael Truog
%%% @version 1.7.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_name).
-author('mjtruog at protonmail dot com').

%% external interface
-export([new/2,
         new/4,
         parse/2,
         parse_with_suffix/2,
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

-spec new(Pattern :: string(),
          Parameters :: list(string())) ->
    {ok, string()} |
    {error, parameters_ignored | parameter_missing}.

new(Pattern, Parameters) ->
    new_insert(Pattern, Parameters, true).

%%-------------------------------------------------------------------------
%% @doc
%% ===Transform a service name pattern with parameters into an exact service name.===
%% The pattern input can contain consecutive * wildcard characters because
%% they are only used for a template.
%% @end
%%-------------------------------------------------------------------------

-spec new(Pattern :: string(),
          Parameters :: list(string()),
          ParametersSelected :: list(pos_integer()),
          ParametersStrictMatching :: boolean()) ->
    {ok, string()} |
    {error,
     parameters_ignored | parameter_missing | parameters_selected_empty |
     {parameters_selected_ignored, list(pos_integer())} |
     {parameters_selected_missing, pos_integer()}}.

new(Pattern, Parameters, [], ParametersStrictMatching) ->
    new_insert(Pattern, Parameters, ParametersStrictMatching);
new(Pattern, Parameters, ParametersSelected, ParametersStrictMatching) ->
    new_select(Pattern, Parameters, ParametersSelected,
               ParametersStrictMatching).

%%-------------------------------------------------------------------------
%% @doc
%% ===Parse a service name pattern to return parameters.===
%% @end
%%-------------------------------------------------------------------------

-spec parse(Name :: string(),
            Pattern :: string()) ->
    list(string()) | error.

parse(Name, Pattern) ->
    cloudi_x_trie:pattern_parse(Pattern, Name).

%%-------------------------------------------------------------------------
%% @doc
%% ===Parse a service name pattern and return the common suffix.===
%% @end
%%-------------------------------------------------------------------------

-spec parse_with_suffix(Name :: string(),
                        Pattern :: string()) ->
    {list(string()), string()} | error.

parse_with_suffix(Name, Pattern) ->
    cloudi_x_trie:pattern_parse(Pattern, Name, with_suffix).

%%-------------------------------------------------------------------------
%% @doc
%% ===Provide the suffix of the service name or service pattern based on the service's configured prefix.===
%% @end
%%-------------------------------------------------------------------------

-spec suffix(Prefix :: string(),
             NameOrPattern :: string()) ->
    string().

suffix([PrefixC | _] = Prefix, [NameOrPatternC | _] = NameOrPattern)
    when is_integer(PrefixC), is_integer(NameOrPatternC) ->
    case lists:member($*, NameOrPattern) of
        true ->
            % handle as a pattern
            suffix_pattern_parse(Prefix, NameOrPattern);
        false ->
            % handle as a name
            case cloudi_x_trie:pattern_suffix(Prefix, NameOrPattern) of
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

new_strip([], NameOut) ->
    {ok, lists:reverse(NameOut)};
new_strip([$* | PatternIn], NameOut) ->
    new_strip(PatternIn, NameOut);
new_strip([C | PatternIn], NameOut) ->
    new_strip(PatternIn, [C | NameOut]).

new_insert([], NameOut,
           [], _) ->
    {ok, lists:reverse(NameOut)};
new_insert([], NameOut,
           [_ | _], ParametersStrictMatching) ->
    if
        ParametersStrictMatching =:= true ->
            {error, parameters_ignored};
        true ->
            {ok, lists:reverse(NameOut)}
    end;
new_insert([$* | PatternIn], NameOut,
           [], ParametersStrictMatching) ->
    if
        ParametersStrictMatching =:= true ->
            {error, parameter_missing};
        true ->
            new_strip(PatternIn, NameOut)
    end;
new_insert([$* | PatternIn], NameOut,
           [Parameter | Parameters], ParametersStrictMatching) ->
    new_insert(PatternIn, lists:reverse(Parameter) ++ NameOut,
               Parameters, ParametersStrictMatching);
new_insert([C | PatternIn], NameOut,
           Parameters, ParametersStrictMatching) ->
    new_insert(PatternIn, [C | NameOut],
               Parameters, ParametersStrictMatching).

new_insert(PatternIn, Parameters, ParametersStrictMatching) ->
    new_insert(PatternIn, [], Parameters, ParametersStrictMatching).

new_select([], NameOut, _,
           ParametersSelected, ParametersStrictMatching) ->
    if
        ParametersStrictMatching =:= true, ParametersSelected /= [] ->
            {error, {parameters_selected_ignored, ParametersSelected}};
        true ->
            {ok, lists:reverse(NameOut)}
    end;
new_select([$* | PatternIn], NameOut, _,
           [], ParametersStrictMatching) ->
    if
        ParametersStrictMatching =:= true ->
            {error, parameters_selected_empty};
        true ->
            new_strip(PatternIn, NameOut)
    end;
new_select([$* | PatternIn], NameOut, Parameters,
           [I | ParametersSelected],
           ParametersStrictMatching) ->
    try lists:nth(I, Parameters) of
        Parameter ->
            new_select(PatternIn, lists:reverse(Parameter) ++ NameOut,
                       Parameters, ParametersSelected,
                       ParametersStrictMatching)
    catch
        error:_ ->
            if
                ParametersStrictMatching =:= true ->
                    {error, {parameters_selected_missing, I}};
                true ->
                    new_strip(PatternIn, NameOut)
            end
    end;
new_select([C | PatternIn], NameOut, Parameters,
           ParametersSelected, ParametersStrictMatching) ->
    new_select(PatternIn, [C | NameOut], Parameters,
               ParametersSelected, ParametersStrictMatching).

new_select(PatternIn, Parameters,
           ParametersSelected, ParametersStrictMatching) ->
    new_select(PatternIn, [], Parameters,
               ParametersSelected, ParametersStrictMatching).

suffix_pattern_parse([], Pattern) ->
    Pattern;
suffix_pattern_parse([C | Prefix], [C | Pattern]) ->
    suffix_pattern_parse(Prefix, Pattern);
suffix_pattern_parse([_ | _], _) ->
    erlang:exit(badarg).
