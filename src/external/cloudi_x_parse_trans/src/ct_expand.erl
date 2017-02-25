%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% --------------------------------------------------
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%% --------------------------------------------------
%% File    : ct_expand.erl
%% @author  : Ulf Wiger <ulf@wiger.net>
%% @end
%% Created : 7 Apr 2010 by Ulf Wiger <ulf@wiger.net>
%%-------------------------------------------------------------------

%% @doc Compile-time expansion utility
%%
%% This module serves as an example of parse_trans-based transforms,
%% but might also be a useful utility in its own right.
%% The transform searches for calls to the pseudo-function
%% `ct_expand:term(Expr)', and then replaces the call site with the
%% result of evaluating `Expr' at compile-time.
%%
%% For example, the line
%%
%% `ct_expand:term(lists:sort([3,5,2,1,4]))'
%%
%% would be expanded at compile-time to `[1,2,3,4,5]'.
%%
%% ct_expand has now been extended to also evaluate calls to local functions.
%% See examples/ct_expand_test.erl for some examples.
%%
%% A debugging facility exists: passing the option {ct_expand_trace, Flags} as an option,
%% or adding a compiler attribute -ct_expand_trace(Flags) will enable a form of call trace.
%%
%% `Flags' can be `[]' (no trace) or `[F]', where `F' is `c' (call trace),
%% `r' (return trace), or `x' (exception trace)'.
%%
%% @end
-module(ct_expand).
-export([parse_transform/2]).

-export([extract_fun/3,
         lfun_rewrite/2]).

-type form()    :: any().
-type forms()   :: [form()].
-type options() :: [{atom(), any()}].


-spec parse_transform(forms(), options()) ->
    forms().
parse_transform(Forms, Options) ->
    Trace = ct_trace_opt(Options, Forms),
    case parse_trans:depth_first(fun(T,F,C,A) ->
                                         xform_fun(T,F,C,A,Forms, Trace)
                                 end, [], Forms, Options) of
        {error, Es} ->
            Es ++ Forms;
        {NewForms, _} ->
            parse_trans:revert(NewForms)
    end.

ct_trace_opt(Options, Forms) ->
    case proplists:get_value(ct_expand_trace, Options) of
        undefined ->
            case [Opt || {attribute,_,ct_expand_trace,Opt} <- Forms] of
                [] ->
                    [];
                [_|_] = L ->
                    lists:last(L)
            end;
        Flags when is_list(Flags) ->
            Flags
    end.

xform_fun(application, Form, _Ctxt, Acc, Forms, Trace) ->
    MFA = erl_syntax_lib:analyze_application(Form),
    case MFA of
        {?MODULE, {term, 1}} ->
            LFH = fun(Name, Args, Bs) ->
                          eval_lfun(
                            extract_fun(Name, length(Args), Forms),
                            Args, Bs, Forms, Trace)
                  end,
            Args = erl_syntax:application_arguments(Form),
            RevArgs = parse_trans:revert(Args),
            case erl_eval:exprs(RevArgs, [], {eval, LFH}) of
                {value, Value,[]} ->
                    {abstract(Value), Acc};
                Other ->
                    parse_trans:error(cannot_evaluate,?LINE,
                                      [{expr, RevArgs},
                                       {error, Other}])
            end;
        _ ->
            {Form, Acc}
    end;
xform_fun(_, Form, _Ctxt, Acc, _, _) ->
    {Form, Acc}.

extract_fun(Name, Arity, Forms) ->
    case [F_ || {function,_,N_,A_,_Cs} = F_ <- Forms,
                N_ == Name, A_ == Arity] of
        [] ->
            erlang:error({undef, [{Name, Arity}]});
        [FForm] ->
            FForm
    end.

eval_lfun({function,L,F,_,Clauses}, Args, Bs, Forms, Trace) ->
    try
        begin
            {ArgsV, Bs1} = lists:mapfoldl(
                             fun(A, Bs_) ->
                                     {value,AV,Bs1_} =
                                         erl_eval:expr(A, Bs_, lfh(Forms, Trace)),
                                     {abstract(AV), Bs1_}
                             end, Bs, Args),
            Expr = {call, L, {'fun', L, {clauses, lfun_rewrite(Clauses, Forms)}}, ArgsV},
            call_trace(Trace =/= [], L, F, ArgsV),
            {value, Ret, _} =
                erl_eval:expr(Expr, erl_eval:new_bindings(), lfh(Forms, Trace)),
            ret_trace(lists:member(r, Trace) orelse lists:member(x, Trace),
                      L, F, Args, Ret),
            %% restore bindings
            {value, Ret, Bs1}
        end
    catch
        error:Err ->
            exception_trace(lists:member(x, Trace), L, F, Args, Err),
            error(Err)
    end.

lfh(Forms, Trace) ->
    {eval, fun(Name, As, Bs1) ->
                   eval_lfun(
                     extract_fun(Name, length(As), Forms),
                     As, Bs1, Forms, Trace)
           end}.

call_trace(false, _, _, _) -> ok;
call_trace(true, L, F, As) ->
    io:fwrite("ct_expand (~w): call ~s~n", [L, pp_function(F, As)]).

pp_function(F, []) ->
    atom_to_list(F) ++ "()";
pp_function(F, [A|As]) ->
    lists:flatten([atom_to_list(F), "(",
                   [io_lib:fwrite("~w", [erl_parse:normalise(A)]) |
                    [[",", io_lib:fwrite("~w", [erl_parse:normalise(A_)])] || A_ <- As]],
                   ")"]).

ret_trace(false, _, _, _, _) -> ok;
ret_trace(true, L, F, Args, Res) ->
    io:fwrite("ct_expand (~w): returned from ~w/~w: ~w~n",
              [L, F, length(Args), Res]).

exception_trace(false, _, _, _, _) -> ok;
exception_trace(true, L, F, Args, Err) ->
    io:fwrite("ct_expand (~w): exception from ~w/~w: ~p~n", [L, F, length(Args), Err]).


lfun_rewrite(Exprs, Forms) ->
    parse_trans:plain_transform(
      fun({'fun',L,{function,F,A}}) ->
              {function,_,_,_,Cs} = extract_fun(F, A, Forms),
              {'fun',L,{clauses, Cs}};
         (_) ->
              continue
      end, Exprs).


%% abstract/1 - modified from erl_eval:abstract/1:
-type abstract_expr() :: term().
-spec abstract(Data) -> AbsTerm when
      Data :: term(),
      AbsTerm :: abstract_expr().
abstract(T) when is_function(T) ->
    case erlang:fun_info(T, module) of
        {module, erl_eval} ->
            case erl_eval:fun_data(T) of
                {fun_data, _Imports, Clauses} ->
                    {'fun', 0, {clauses, Clauses}};
                false ->
                    erlang:error(function_clause)  % mimicking erl_parse:abstract(T)
            end;
        _ ->
            erlang:error(function_clause)
    end;
abstract(T) when is_integer(T) -> {integer,0,T};
abstract(T) when is_float(T) -> {float,0,T};
abstract(T) when is_atom(T) -> {atom,0,T};
abstract([]) -> {nil,0};
abstract(B) when is_bitstring(B) ->
    {bin, 0, [abstract_byte(Byte, 0) || Byte <- bitstring_to_list(B)]};
abstract([C|T]) when is_integer(C), 0 =< C, C < 256 ->
    abstract_string(T, [C]);
abstract([H|T]) ->
    {cons,0,abstract(H),abstract(T)};
abstract(Tuple) when is_tuple(Tuple) ->
    {tuple,0,abstract_list(tuple_to_list(Tuple))}.

abstract_string([C|T], String) when is_integer(C), 0 =< C, C < 256 ->
    abstract_string(T, [C|String]);
abstract_string([], String) ->
    {string, 0, lists:reverse(String)};
abstract_string(T, String) ->
    not_string(String, abstract(T)).

not_string([C|T], Result) ->
    not_string(T, {cons, 0, {integer, 0, C}, Result});
not_string([], Result) ->
    Result.

abstract_list([H|T]) ->
    [abstract(H)|abstract_list(T)];
abstract_list([]) ->
    [].

abstract_byte(Byte, Line) when is_integer(Byte) ->
    {bin_element, Line, {integer, Line, Byte}, default, default};
abstract_byte(Bits, Line) ->
    Sz = bit_size(Bits),
    <<Val:Sz>> = Bits,
    {bin_element, Line, {integer, Line, Val}, {integer, Line, Sz}, default}.

