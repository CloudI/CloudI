-module(pmod).

-export([parse_transform/2]).

-import(erl_syntax, [attribute_name/1, attribute_arguments/1,
		     list_elements/1,
		     atom_value/1, tuple_elements/1, integer_value/1]).

-record(st, {vars = [],
	     funs = ordsets:new()}).

parse_transform(Forms, Opts) ->
    case proplists:get_bool(trace, Opts) of
	true ->
	    dbg:tracer(),
	    dbg:tpl(?MODULE,x),
	    dbg:p(all,[c]);
	false ->
	    ok
    end,
    C = parse_trans:initial_context(Forms, Opts),
    St = parse_trans:do_inspect(fun inspect/4, #st{}, Forms, C),
    Res = parse_trans:revert(
	    export_funs(wrap_funs(create_new_f(Forms, St, C), St, C), St, C)),
    parse_trans:optionally_pretty_print(Res, Opts, C),
    Res.

inspect(attribute, Form, _C, Acc) ->
    case attr_name(Form) of
	pmod_vars ->
	    [] = Acc#st.vars,  %% assertion - should be a proper check
	    VarNames = [atom_value(V) ||
			   V <- list_elements(hd(attribute_arguments(Form)))],
	    {false, Acc#st{vars = VarNames}};
	pmod_funs ->
	    Fs = lists:foldl(fun(E, Acc1) ->
				     [Fun,Ay] = tuple_elements(E),
				     ordsets:add_element(
				       {atom_value(Fun), integer_value(Ay)},
				       Acc1)
			     end, Acc#st.funs,
			     list_elements(hd(attribute_arguments(Form)))),
	    {false, Acc#st{funs = Fs}};
	_ ->
	    {false, Acc}
    end;
inspect(_, _, _, Acc) ->
    {false, Acc}.

attr_name(F) ->
    atom_value(attribute_name(F)).

create_new_f(Forms, #st{vars = Vs}, C) ->
    Arity = length(Vs),
    Form = {function, 1, new, Arity,
	    [{clause, 1,
	      [{var,1,V} || V <- Vs],
	      [],
	      [{tuple, 1, [{atom, 1, parse_trans:context(module, C)},
			   {tuple, 1, [{var,1,V} || V <- Vs]}]}
	      ]}
	    ]},
    parse_trans:do_insert_forms(above, [Form], Forms, C).

wrap_funs(Forms, #st{vars = Vs, funs = Fs}, C) ->
    Mod = parse_trans:context(module, C),
    lists:foldl(
      fun({F,A}, Acc) ->
	      {NewForms, _} =
		  parse_trans:do_transform(
		    fun(T,Form,C1,Acc1) ->
			    wrap_fun(T,Form,C1,Acc1,F,A,Vs,Mod)
		    end, false, Acc, C),
	      NewForms
      end, Forms, Fs).

wrap_fun(function, Form, _, Acc, F, A, Vs,Mod) ->
    case erl_syntax:revert(Form) of
	{function, L, F, A, Cs} ->
	    {{function, L, F, A + 1,
	      [{clause, Lc,
		Args ++ [{tuple,1,[{atom,1,Mod},
				   {tuple,1,
				    fix_vars(Vs, F, A, Clause)}]}], Gs, B}
	       || {clause, Lc, Args, Gs, B} = Clause <- Cs]},
	     false, Acc};
	_ ->
	    {Form, false, Acc}
    end;
wrap_fun(_, Form, _, Acc, _, _, _, _) ->
    {Form, false, Acc}.

fix_vars(Vars, F, A, Clause) ->
    %% erl_syntax_lib:variables/1 doesn't seem to work with just a clause...
    Used = sets:to_list(erl_syntax_lib:variables({function,1,F,A,[Clause]})),
    [{var, 1, fix_var(V, Used)} || V <- Vars].

fix_var(V, Used) ->
    case lists:member(V, Used) of
	true -> V;
	false ->
	    list_to_atom("_" ++ atom_to_list(V))
    end.


export_funs(Forms, #st{vars = Vs, funs = Fs}, C) ->
    New = [{attribute,1,export,[{new,length(Vs)}]},
	   {attribute,1,export,[{F,A+1} || {F,A} <- Fs]}],
    parse_trans:do_insert_forms(above, New, Forms, C).
