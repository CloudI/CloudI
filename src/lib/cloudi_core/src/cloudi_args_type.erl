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
%%% Copyright (c) 2015-2023 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2015-2023 Michael Truog
%%% @version 2.0.7 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_args_type).
-author('mjtruog at protonmail dot com').

%% external interface
-export([function_required/2,
         function_required_pick/2,
         function_optional/2,
         function_optional_pick/2,
         function_optional_pick_any/2,
         period_to_milliseconds/3,
         period_to_milliseconds/4,
         period_to_seconds/2,
         priority/1,
         service_name/1,
         service_name_pattern/1,
         service_name_pattern_suffix/2,
         service_name_suffix/2,
         timeout_milliseconds/1,
         timeout_period/1,
         timeout_period_to_milliseconds/1]).

-include("cloudi_logger.hrl").
-include("cloudi_core_i_constants.hrl").

-type period() :: non_neg_integer() | limit_min | limit_max |
                  {pos_integer(), seconds | second} |
                  {pos_integer(), minutes | minute} |
                  {pos_integer(), hours | hour} |
                  {pos_integer(), days | day}.
-export_type([period/0]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

-spec function_required(fun() |
                        {module(), atom()} |
                        {{module(), atom()}} |
                        {{module(), atom(), list()}},
                        Arity :: non_neg_integer()) ->
    fun().

function_required({{M, F, A}}, Arity)
    when is_atom(M), is_atom(F), is_list(A), is_integer(Arity), Arity >= 0 ->
    ArityF = length(A),
    case erlang:function_exported(M, F, ArityF) of
        true ->
            Function = erlang:apply(M, F, A),
            if
                is_function(Function) ->
                    function_required(Function, Arity);
                true ->
                    ?LOG_ERROR_SYNC("function ~w:~tw/~w does not "
                                    "return a function!", [M, F, ArityF]),
                    erlang:exit(badarg)
            end;
        false ->
            ?LOG_ERROR_SYNC("function ~w:~tw/~w does not exist!",
                            [M, F, ArityF]),
            erlang:exit(badarg)
    end;
function_required({{M, F}}, Arity)
    when is_atom(M), is_atom(F), is_integer(Arity), Arity >= 0 ->
    case erlang:function_exported(M, F, 0) of
        true ->
            Function = M:F(),
            if
                is_function(Function) ->
                    function_required(Function, Arity);
                true ->
                    ?LOG_ERROR_SYNC("function ~w:~tw/~w does not "
                                    "return a function!", [M, F, 0]),
                    erlang:exit(badarg)
            end;
        false ->
            ?LOG_ERROR_SYNC("function ~w:~tw/~w does not exist!",
                            [M, F, 0]),
            erlang:exit(badarg)
    end;
function_required({M, F}, Arity)
    when is_atom(M), is_atom(F), is_integer(Arity), Arity >= 0 ->
    case erlang:function_exported(M, F, Arity) of
        true ->
            fun M:F/Arity;
        false ->
            ?LOG_ERROR_SYNC("function ~w:~tw/~w does not exist!",
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
    ?LOG_ERROR_SYNC("not a function: ~tp", [Function]),
    erlang:exit(badarg).

-spec function_required_pick(fun() |
                             {module(), atom()} |
                             {{module(), atom()}} |
                             {{module(), atom(), list()}},
                             ArityOrder :: nonempty_list(non_neg_integer())) ->
    {fun(), Arity :: non_neg_integer()}.

function_required_pick({{M, F, A}}, [_ | _] = ArityOrder)
    when is_atom(M), is_atom(F), is_list(A) ->
    ArityF = length(A),
    case erlang:function_exported(M, F, ArityF) of
        true ->
            Function = erlang:apply(M, F, A),
            if
                is_function(Function) ->
                    function_required_pick(Function, ArityOrder);
                true ->
                    ?LOG_ERROR_SYNC("function ~w:~tw/~w does not "
                                    "return a function!", [M, F, ArityF]),
                    erlang:exit(badarg)
            end;
        false ->
            ?LOG_ERROR_SYNC("function ~w:~tw/~w does not exist!",
                            [M, F, ArityF]),
            erlang:exit(badarg)
    end;
function_required_pick({{M, F}}, [_ | _] = ArityOrder)
    when is_atom(M), is_atom(F) ->
    case erlang:function_exported(M, F, 0) of
        true ->
            Function = M:F(),
            if
                is_function(Function) ->
                    function_required_pick(Function, ArityOrder);
                true ->
                    ?LOG_ERROR_SYNC("function ~w:~tw/~w does not "
                                    "return a function!", [M, F, 0]),
                    erlang:exit(badarg)
            end;
        false ->
            ?LOG_ERROR_SYNC("function ~w:~tw/~w does not exist!",
                            [M, F, 0]),
            erlang:exit(badarg)
    end;
function_required_pick({M, F}, [_ | _] = ArityOrder)
    when is_atom(M), is_atom(F) ->
    function_required_pick_module(ArityOrder, M, F, ArityOrder);
function_required_pick(Function, [_ | _] = ArityOrder)
    when is_function(Function) ->
    function_required_pick_function(ArityOrder, Function, ArityOrder);
function_required_pick(Function, [_ | _]) ->
    ?LOG_ERROR_SYNC("not a function: ~tp", [Function]),
    erlang:exit(badarg).

-spec function_optional(undefined | fun() |
                        {module(), atom()} |
                        {{module(), atom()}} |
                        {{module(), atom(), list()}},
                        Arity :: non_neg_integer()) ->
    undefined | fun().

function_optional(undefined, _) ->
    undefined;
function_optional(Function, Arity) ->
    function_required(Function, Arity).

-spec function_optional_pick(undefined | fun() |
                             {module(), atom()} |
                             {{module(), atom()}} |
                             {{module(), atom(), list()}},
                             ArityOrder :: nonempty_list(non_neg_integer())) ->
    undefined | {fun(), Arity :: non_neg_integer()}.

function_optional_pick(undefined, _) ->
    undefined;
function_optional_pick(Function, ArityOrder) ->
    function_required_pick(Function, ArityOrder).

-spec function_optional_pick_any(undefined | fun() |
                                 {module(), atom()} |
                                 {{module(), atom()}} |
                                 {{module(), atom(), list()}},
                                 ArityOrder ::
                                     nonempty_list(non_neg_integer())) ->
    undefined | fun().

function_optional_pick_any(undefined, _) ->
    undefined;
function_optional_pick_any(Function, ArityOrder) ->
    {FunctionNew, _} = function_required_pick(Function, ArityOrder),
    FunctionNew.

-spec period_to_milliseconds(Value :: period(),
                             Min :: non_neg_integer(),
                             Max :: pos_integer()) ->
    non_neg_integer().

period_to_milliseconds(_, Min, _)
    when not is_integer(Min) ->
    ?LOG_ERROR_SYNC("invalid period min: ~tp", [Min]),
    erlang:exit(badarg);
period_to_milliseconds(_, _, Max)
    when not is_integer(Max) ->
    ?LOG_ERROR_SYNC("invalid period max: ~tp", [Max]),
    erlang:exit(badarg);
period_to_milliseconds(_, Min, Max)
    when not (Min < Max) ->
    ?LOG_ERROR_SYNC("invalid period min/max: ~w >= ~w", [Min, Max]),
    erlang:exit(badarg);
period_to_milliseconds(limit_min, Min, _) ->
    Min;
period_to_milliseconds(limit_max, _, Max) ->
    Max;
period_to_milliseconds(Value, Min, Max)
    when is_integer(Value), Value >= 0 ->
    if
        Value < Min ->
            ?LOG_ERROR_SYNC("period ~w < ~w ", [Value, Min]),
            erlang:exit(badarg);
        Value > Max ->
            ?LOG_ERROR_SYNC("period ~w > ~w ", [Value, Max]),
            erlang:exit(badarg);
        true ->
            Value
    end;
period_to_milliseconds({Multiplier, Unit} = Value, Min, Max)
    when is_integer(Multiplier), Multiplier >= 1 ->
    ValueMilliSeconds = if
        Unit =:= seconds orelse Unit =:= second ->
            Multiplier * 1000;
        Unit =:= minutes orelse Unit =:= minute ->
            Multiplier * 60000;
        Unit =:= hours orelse Unit =:= hour ->
            Multiplier * 3600000;
        Unit =:= days orelse Unit =:= day ->
            Multiplier * 86400000;
        true ->
            ?LOG_ERROR_SYNC("invalid period unit: ~tp", [Unit]),
            erlang:exit(badarg)
    end,
    if
        ValueMilliSeconds < Min ->
            ?LOG_ERROR_SYNC("period ~w < ~w ", [Value, Min]),
            erlang:exit(badarg);
        ValueMilliSeconds > Max ->
            ?LOG_ERROR_SYNC("period ~w > ~w ", [Value, Max]),
            erlang:exit(badarg);
        true ->
            ValueMilliSeconds
    end;
period_to_milliseconds(Value, _, _) ->
    ?LOG_ERROR_SYNC("invalid period: ~tp", [Value]),
    erlang:exit(badarg).

-spec period_to_milliseconds(Value :: period() | undefined,
                             Min :: non_neg_integer(),
                             Max :: pos_integer(),
                             Default :: period()) ->
    non_neg_integer().

period_to_milliseconds(undefined, Min, Max, Default) ->
    period_to_milliseconds(Default, Min, Max);
period_to_milliseconds(Value, Min, Max, _) ->
    period_to_milliseconds(Value, Min, Max).

-spec period_to_seconds(Value :: cloudi_service_api:period_gte(),
                        Min :: non_neg_integer()) ->
    cloudi_service_api:seconds().

period_to_seconds(_, Min)
    when not is_integer(Min) ->
    ?LOG_ERROR_SYNC("invalid period min: ~tp", [Min]),
    erlang:exit(badarg);
period_to_seconds(limit_min, Min) ->
    Min;
period_to_seconds(Value, Min)
    when is_integer(Value), Value >= 0 ->
    if
        Value < Min ->
            ?LOG_ERROR_SYNC("period ~w < ~w ", [Value, Min]),
            erlang:exit(badarg);
        true ->
            Value
    end;
period_to_seconds({Multiplier, Unit}, _)
    when is_integer(Multiplier), Multiplier >= 1 ->
    if
        Unit =:= minutes orelse Unit =:= minute ->
            Multiplier * 60;
        Unit =:= hours orelse Unit =:= hour ->
            Multiplier * 3600;
        Unit =:= days orelse Unit =:= day ->
            Multiplier * 86400;
        true ->
            ?LOG_ERROR_SYNC("invalid period unit: ~tp", [Unit]),
            erlang:exit(badarg)
    end;
period_to_seconds(Value, _) ->
    ?LOG_ERROR_SYNC("invalid period: ~tp", [Value]),
    erlang:exit(badarg).

-spec priority(Priority :: cloudi:priority()) ->
    true.

priority(undefined) ->
    true;
priority(Priority)
    when is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    true;
priority(Priority) ->
    ?LOG_ERROR_SYNC("invalid priority: ~tp", [Priority]),
    erlang:exit(badarg).

-spec service_name(Name :: cloudi:service_name()) ->
    true.

service_name([NameC | _] = Name)
    when is_integer(NameC) ->
    case cloudi_x_trie:is_pattern2_bytes(Name) of
        true ->
            ?LOG_ERROR_SYNC("service name is pattern: \"~ts\"",
                            [cloudi_service_name:utf8_forced(Name)]),
            erlang:exit(badarg);
        false ->
            true
    end;
service_name(Name) ->
    ?LOG_ERROR_SYNC("invalid service name: ~tp", [Name]),
    erlang:exit(badarg).

-spec service_name_pattern(Pattern :: cloudi:service_name_pattern()) ->
    true.

service_name_pattern([PatternC | _] = Pattern)
    when is_integer(PatternC) ->
    _ = cloudi_x_trie:is_pattern2_bytes(Pattern),
    true;
service_name_pattern(Pattern) ->
    ?LOG_ERROR_SYNC("invalid service name pattern: ~tp", [Pattern]),
    erlang:exit(badarg).

-spec service_name_pattern_suffix(Prefix :: cloudi:service_name_pattern(),
                                  Pattern :: cloudi:service_name_pattern()) ->
    cloudi:service_name_pattern_suffix().

service_name_pattern_suffix([PrefixC | _] = Prefix, [PatternC | _] = Pattern)
    when is_integer(PrefixC), is_integer(PatternC) ->
    case suffix_pattern_parse(Prefix, Pattern) of
        error ->
            ?LOG_ERROR_SYNC("prefix service name pattern mismatch: "
                            "\"~ts\" \"~ts\"",
                            [cloudi_service_name:utf8_forced(Prefix),
                             cloudi_service_name:utf8_forced(Pattern)]),
            erlang:exit(badarg);
        Suffix ->
            Suffix
    end;
service_name_pattern_suffix([PrefixC | _], Pattern)
    when is_integer(PrefixC) ->
    ?LOG_ERROR_SYNC("invalid service name pattern: ~tp", [Pattern]),
    erlang:exit(badarg);
service_name_pattern_suffix(Prefix, [PatternC | _])
    when is_integer(PatternC) ->
    ?LOG_ERROR_SYNC("invalid prefix: ~tp", [Prefix]),
    erlang:exit(badarg).

-spec service_name_suffix(Prefix :: cloudi:service_name_pattern(),
                          Name :: cloudi:service_name()) ->
    string().

service_name_suffix([PrefixC | _] = Prefix, [NameC | _] = Name)
    when is_integer(PrefixC), is_integer(NameC) ->
    case cloudi_x_trie:is_pattern2_bytes(Name) of
        true ->
            ?LOG_ERROR_SYNC("service name is pattern: \"~ts\"",
                            [cloudi_service_name:utf8_forced(Name)]),
            erlang:exit(badarg);
        false ->
            case cloudi_x_trie:pattern2_suffix(Prefix, Name) of
                error ->
                    ?LOG_ERROR_SYNC("prefix service name mismatch: "
                                    "\"~ts\" \"~ts\"",
                                    [cloudi_service_name:utf8_forced(Prefix),
                                     cloudi_service_name:utf8_forced(Name)]),
                    erlang:exit(badarg);
                Suffix ->
                    Suffix
            end
    end;
service_name_suffix([PrefixC | _], Name)
    when is_integer(PrefixC) ->
    ?LOG_ERROR_SYNC("invalid service name: ~tp", [Name]),
    erlang:exit(badarg);
service_name_suffix(Prefix, [NameC | _])
    when is_integer(NameC) ->
    ?LOG_ERROR_SYNC("invalid prefix: ~tp", [Prefix]),
    erlang:exit(badarg).

-spec timeout_milliseconds(Timeout :: cloudi:timeout_milliseconds()) ->
    true.

timeout_milliseconds(undefined) ->
    true;
timeout_milliseconds(limit_min) ->
    true;
timeout_milliseconds(limit_max) ->
    true;
timeout_milliseconds(Timeout)
    when is_integer(Timeout), Timeout >= 0, Timeout =< ?TIMEOUT_MAX_ERLANG ->
    true;
timeout_milliseconds(Timeout) ->
    ?LOG_ERROR_SYNC("invalid timeout: ~tp", [Timeout]),
    erlang:exit(badarg).

-spec timeout_period(Timeout :: cloudi:timeout_period()) ->
    true.

timeout_period(undefined) ->
    true;
timeout_period(limit_min) ->
    true;
timeout_period(limit_max) ->
    true;
timeout_period(Timeout)
    when is_integer(Timeout), Timeout >= 0, Timeout =< ?TIMEOUT_MAX_ERLANG ->
    true;
timeout_period({Multiplier, Unit} = Timeout)
    when is_integer(Multiplier), Multiplier >= 1 ->
    if
        Unit =:= seconds orelse Unit =:= second,
        Multiplier =< ?TIMEOUT_MAX_ERLANG div 1000 ->
            true;
        Unit =:= minutes orelse Unit =:= minute,
        Multiplier =< ?TIMEOUT_MAX_ERLANG div 60000 ->
            true;
        Unit =:= hours orelse Unit =:= hour,
        Multiplier =< ?TIMEOUT_MAX_ERLANG div 3600000 ->
            true;
        Unit =:= days orelse Unit =:= day,
        Multiplier =< ?TIMEOUT_MAX_ERLANG div 86400000 ->
            true;
        true ->
            ?LOG_ERROR_SYNC("invalid timeout_period: ~tp", [Timeout]),
            erlang:exit(badarg)
    end;
timeout_period(Timeout) ->
    ?LOG_ERROR_SYNC("invalid timeout: ~tp", [Timeout]),
    erlang:exit(badarg).

-spec timeout_period_to_milliseconds(Timeout :: cloudi:timeout_period()) ->
    cloudi:timeout_milliseconds().

timeout_period_to_milliseconds(Timeout)
    when Timeout =:= undefined; Timeout =:= limit_min; Timeout =:= limit_max ->
    Timeout;
timeout_period_to_milliseconds(Timeout)
    when is_integer(Timeout), Timeout >= 0, Timeout =< ?TIMEOUT_MAX_ERLANG ->
    Timeout;
timeout_period_to_milliseconds({Multiplier, Unit})
    when is_integer(Multiplier), Multiplier >= 1 ->
    if
        Unit =:= seconds orelse Unit =:= second ->
            Multiplier * 1000;
        Unit =:= minutes orelse Unit =:= minute ->
            Multiplier * 60000;
        Unit =:= hours orelse Unit =:= hour ->
            Multiplier * 3600000;
        Unit =:= days orelse Unit =:= day ->
            Multiplier * 86400000
    end.

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
                    ?LOG_ERROR_SYNC("function ~w:~tw/~w does not exist!",
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

