%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Service Configuration Arguments Type Checking==
%%% Functions to simplify validation done during service initialization.
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2015-2017 Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2015-2017 Michael Truog
%%% @version 1.7.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_args_type).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([function_required/2,
         function_required_pick/2,
         function_optional/2,
         service_name_suffix/2,
         service_name_pattern_suffix/2]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

-spec function_required({{module(), arity()}} | {module(), atom()} | fun(),
                        Arity :: non_neg_integer()) ->
    fun().

function_required({{M, F}}, Arity)
    when is_atom(M), is_atom(F), is_integer(Arity), Arity >= 0 ->
    case erlang:function_exported(M, F, 0) of
        true ->
            Function = M:F(),
            if
                is_function(Function) ->
                    function_required(Function, Arity);
                true ->
                    ?LOG_ERROR_SYNC("function ~w:~w/~w does not "
                                    "return a function!", [M, F, 0]),
                    erlang:exit(badarg)
            end;
        false ->
            ?LOG_ERROR_SYNC("function ~w:~w/~w does not exist!", [M, F, 0]),
            erlang:exit(badarg)
    end;
function_required({M, F}, Arity)
    when is_atom(M), is_atom(F), is_integer(Arity), Arity >= 0 ->
    case erlang:function_exported(M, F, Arity) of
        true ->
            fun M:F/Arity;
        false ->
            ?LOG_ERROR_SYNC("function ~w:~w/~w does not exist!",
                            [M, F, Arity]),
            erlang:exit(badarg)
    end;
function_required(Function, Arity)
    when is_function(Function) ->
    if
        is_function(Function, Arity) ->
            Function;
        true ->
            ?LOG_ERROR_SYNC("function arity is not ~w!", [Arity]),
            erlang:exit(badarg)
    end;
function_required(Function, _) ->
    ?LOG_ERROR_SYNC("not a function: ~p", [Function]),
    erlang:exit(badarg).

-spec function_required_pick({{module(), atom()}} | {module(), atom()} | fun(),
                             ArityOrder :: nonempty_list(non_neg_integer())) ->
    {fun(), Arity :: non_neg_integer()}.

function_required_pick({{M, F}}, [_ | _] = ArityOrder)
    when is_atom(M), is_atom(F) ->
    case erlang:function_exported(M, F, 0) of
        true ->
            Function = M:F(),
            if
                is_function(Function) ->
                    function_required_pick(Function, ArityOrder);
                true ->
                    ?LOG_ERROR_SYNC("function ~w:~w/~w does not "
                                    "return a function!", [M, F, 0]),
                    erlang:exit(badarg)
            end;
        false ->
            ?LOG_ERROR_SYNC("function ~w:~w/~w does not exist!", [M, F, 0]),
            erlang:exit(badarg)
    end;
function_required_pick({M, F}, [_ | _] = ArityOrder)
    when is_atom(M), is_atom(F) ->
    function_required_pick_module(ArityOrder, M, F, ArityOrder);
function_required_pick(Function, [_ | _] = ArityOrder)
    when is_function(Function) ->
    function_required_pick_function(ArityOrder, Function, ArityOrder);
function_required_pick(Function, [_ | _]) ->
    ?LOG_ERROR_SYNC("not a function: ~p", [Function]),
    erlang:exit(badarg).

-spec function_optional(undefined |
                        {{module(), atom()}} | {module(), atom()} | fun(),
                        Arity :: non_neg_integer()) ->
    undefined | fun().

function_optional(undefined, _) ->
    undefined;
function_optional(Function, Arity) ->
    function_required(Function, Arity).

-spec service_name_suffix(Prefix :: cloudi:service_name_pattern(),
                          Name :: cloudi:service_name()) ->
    string().

service_name_suffix([PrefixC | _] = Prefix, [NameC | _] = Name)
    when is_integer(PrefixC), is_integer(NameC) ->
    case lists:member($*, Name) of
        true ->
            ?LOG_ERROR_SYNC("service name is pattern: \"~s\"", [Name]),
            erlang:exit(badarg);
        false ->
            case cloudi_x_trie:pattern_suffix(Prefix, Name) of
                error ->
                    ?LOG_ERROR_SYNC("prefix service name mismatch: "
                                    "\"~s\" \"~s\"", [Prefix, Name]),
                    erlang:exit(badarg);
                Suffix ->
                    Suffix
            end
    end;
service_name_suffix([PrefixC | _], Name)
    when is_integer(PrefixC) ->
    ?LOG_ERROR_SYNC("invalid service name: ~p", [Name]),
    erlang:exit(badarg);
service_name_suffix(Prefix, [NameC | _])
    when is_integer(NameC) ->
    ?LOG_ERROR_SYNC("invalid prefix: ~p", [Prefix]),
    erlang:exit(badarg).

-spec service_name_pattern_suffix(Prefix :: cloudi:service_name_pattern(),
                                  Pattern :: cloudi:service_name_pattern()) ->
    string().

service_name_pattern_suffix([PrefixC | _] = Prefix, [PatternC | _] = Pattern)
    when is_integer(PrefixC), is_integer(PatternC) ->
    case suffix_pattern_parse(Prefix, Pattern) of
        error ->
            ?LOG_ERROR_SYNC("prefix service name pattern mismatch: "
                            "\"~s\" \"~s\"", [Prefix, Pattern]),
            erlang:exit(badarg);
        Suffix ->
            Suffix
    end;
service_name_pattern_suffix([PrefixC | _], Pattern)
    when is_integer(PrefixC) ->
    ?LOG_ERROR_SYNC("invalid service name pattern: ~p", [Pattern]),
    erlang:exit(badarg);
service_name_pattern_suffix(Prefix, [PatternC | _])
    when is_integer(PatternC) ->
    ?LOG_ERROR_SYNC("invalid prefix: ~p", [Prefix]),
    erlang:exit(badarg).

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

function_required_pick_module([Arity | ArityL], M, F, ArityOrder)
    when is_integer(Arity), Arity >= 0 ->
    case erlang:function_exported(M, F, Arity) of
        true ->
            {fun M:F/Arity, Arity};
        false ->
            if
                ArityL == [] ->
                    ?LOG_ERROR_SYNC("function ~w:~w/~w does not exist!",
                                    [M, F, ArityOrder]),
                    erlang:exit(badarg);
                true ->
                    function_required_pick_module(ArityL, M, F, ArityOrder)
            end
    end;
function_required_pick_module(_, _, _, ArityOrder) ->
    erlang:exit({badarg, ArityOrder}).

function_required_pick_function([Arity | ArityL], Function, ArityOrder)
    when is_integer(Arity), Arity >= 0 ->
    if
        is_function(Function, Arity) ->
            {Function, Arity};
        ArityL == [] ->
            ?LOG_ERROR_SYNC("function arity is not in ~w!", [ArityOrder]),
            erlang:exit(badarg);
        true ->
            function_required_pick_function(ArityL, Function, ArityOrder)
    end;
function_required_pick_function(_, _, ArityOrder) ->
    erlang:exit({badarg, ArityOrder}).

suffix_pattern_parse([], Pattern) ->
    Pattern;
suffix_pattern_parse([C | Prefix], [C | Pattern]) ->
    suffix_pattern_parse(Prefix, Pattern);
suffix_pattern_parse([_ | _], _) ->
    error.

