%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Service Name Creation and Parsing==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2014-2015, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2014-2015 Michael Truog
%%% @version 1.4.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_name).
-author('mjtruog [at] gmail (dot) com').

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

suffix([_ | _] = Prefix, [_ | _] = NameOrPattern) ->
    suffix_parse(Prefix, NameOrPattern);
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

suffix_parse([], NameOrPattern) ->
    NameOrPattern;
suffix_parse([$*], [H | Pattern]) ->
    if
        H =:= $* ->
            Pattern;
        true ->
            ""
    end;
suffix_parse([$*, C | Prefix], [H | Name])
    when H =/= $* ->
    if
        C =:= $* ->
            erlang:exit(badarg);
        true ->
            suffix_parse(Prefix, suffix_pattern(Name, C))
    end;
suffix_parse([C | Prefix], [C | NameOrPattern]) ->
    suffix_parse(Prefix, NameOrPattern);
suffix_parse([_ | _], _) ->
    erlang:exit(badarg).

suffix_pattern([], _) ->
    erlang:exit(badarg);
suffix_pattern([$* | _], _) ->
    erlang:exit(badarg);
suffix_pattern([C | Name], C) ->
    Name;
suffix_pattern([_ | Name], C) ->
    suffix_pattern(Name, C).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

suffix_test() ->
    "." = cloudi_service_name:suffix("//", "//."),
    % Name
    "." = cloudi_service_name:suffix("/*/", "/./."),
    "." = cloudi_service_name:suffix("/*/", "/..../."),
    "" = cloudi_service_name:suffix("*", "."),
    "" = cloudi_service_name:suffix("*.", ".."),
    "." = cloudi_service_name:suffix("*.", "..."),
    % Pattern
    "." = cloudi_service_name:suffix("/*/", "/*/."),
    "." = cloudi_service_name:suffix("/*", "/*."),
    % errors
    {'EXIT', badarg} = (catch cloudi_service_name:suffix("/*/", "//.")),
    {'EXIT', badarg} = (catch cloudi_service_name:suffix("/*/", "/*")),
    {'EXIT', badarg} = (catch cloudi_service_name:suffix("", ".")),
    {'EXIT', badarg} = (catch cloudi_service_name:suffix(".", "")),
    ok.

-endif.
