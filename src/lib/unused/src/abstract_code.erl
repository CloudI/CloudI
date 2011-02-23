%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Abstract Code Insertion==
%%% A method to get the abstract code representation directly from the
%%% binary beam module representation.
%%%
%%% Other methods could be used, but they are not as efficient.
%%% A more obvious method is to use "-compile(debug_info).", but
%%% this approach does not work in practice (despite the documentation)
%%% and would cause the generated code to be inefficient if it did work.
%%%
%%% Instead, the module that wants to provide its abstract code can use
%%% "-compile({parse_transform, abstract_code})." and a function will be added
%%% to the module as '$abstract_code$'/0, which will return a copy of the
%%% abstract code without the debug_info overhead.
%%%
%%% http://groups.google.com/group/erlang-programming/msg/05d09ff75309e856
%%% @end
%%% @author Ulf Wiger
%%%------------------------------------------------------------------------

-module(abstract_code).
-export([parse_transform/2]).

parse_transform(Forms, _) ->
    [EoF|Rev] = lists:reverse(Forms),
    Forms1 = lists:reverse([EoF,absfun(Forms)|Rev]),
    {Attrs,Funs} = lists:splitwith(fun(F) ->
                                           element(1,F) == attribute
                                   end, Forms1),
    Attrs ++ [{attribute,1,export,[{'$abstract_code$',0}]} | Funs].

absfun(Forms) ->
    {function, 1, '$abstract_code$', 0,
     [{clause,1,[],[],
       [erl_parse:abstract(Forms)]}]}. 

