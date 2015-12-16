%% @doc Code generation functions.
-module(glc_code).
-compile({nowarn_unused_function, {abstract_module,2}}).
-compile({nowarn_unused_function, {abstract_tables,1}}).
-compile({nowarn_unused_function, {abstract_reset,0}}).
-compile({nowarn_unused_function, {abstract_filter,2}}).
-compile({nowarn_unused_function, {abstract_filter_,4}}).
-compile({nowarn_unused_function, {abstract_opfilter,6}}).
-compile({nowarn_unused_function, {abstract_all,4}}).
-compile({nowarn_unused_function, {abstract_any,4}}).
-compile({nowarn_unused_function, {abstract_with,2}}).
-compile({nowarn_unused_function, {abstract_getkey,4}}).
-compile({nowarn_unused_function, {abstract_getkey_,4}}).
-compile({nowarn_unused_function, {abstract_getparam,3}}).
-compile({nowarn_unused_function, {abstract_getparam_,3}}).
-compile({nowarn_unused_function, {param_variable,1}}).
-compile({nowarn_unused_function, {field_variable,1}}).
-compile({nowarn_unused_function, {field_variable_,1}}).
-compile({nowarn_unused_function, {compile_forms,2}}).
-compile({nowarn_unused_function, {load_binary,2}}).


-export([
    compile/2
]).

-define(erl, erl_syntax).

-record(module, {
    'query' :: term(),
    tables :: [{atom(), atom()}],
    qtree :: term()
}).

-type syntaxTree() :: erl_syntax:syntaxTree().

-record(state, {
    event = undefined :: syntaxTree(),
    fields = [] :: [{atom(), syntaxTree()}],
    fieldc = 0 :: non_neg_integer(),
    paramvars = [] :: [{term(), syntaxTree()}],
    paramstab = undefined :: atom(),
    countstab = undefined :: atom()
}).

-type nextFun() :: fun((#state{}) -> [syntaxTree()]).

compile(Module, ModuleData) ->
    {ok, forms, Forms} = abstract_module(Module, ModuleData),
    {ok, Module, Binary} = compile_forms(Forms, [nowarn_unused_vars]),
    {ok, loaded, Module} = load_binary(Module, Binary),
    {ok, Module}.

%% abstract code geneation functions

%% @private Generate an abstract dispatch module.
-spec abstract_module(atom(), #module{}) -> {ok, forms, list()}.
abstract_module(Module, Data) ->
    Forms = [?erl:revert(E) || E <- abstract_module_(Module, Data)],
    case lists:keyfind(errors, 1, erl_syntax_lib:analyze_forms(Forms)) of
        false -> {ok, forms, Forms};
        {_, []} -> {ok, forms, Forms};
        {_, [_|_]}=Errors -> Errors
    end.

%% @private Generate an abstract dispatch module.
-spec abstract_module_(atom(), #module{}) -> [?erl:syntaxTree()].
abstract_module_(Module, #module{tables=Tables, qtree=Tree}=Data) ->
    {_, ParamsTable} = lists:keyfind(params, 1, Tables),
    {_, CountsTable} = lists:keyfind(counters, 1, Tables),
    AbstractMod = [
     %% -module(Module)
     ?erl:attribute(?erl:atom(module), [?erl:atom(Module)]),
     %% -export([
     ?erl:attribute(
       ?erl:atom(export),
       [?erl:list([
        %% info/1
        ?erl:arity_qualifier(
            ?erl:atom(info),
            ?erl:integer(1)),
        %% reset_counters/1
        ?erl:arity_qualifier(
            ?erl:atom(reset_counters),
            ?erl:integer(1)),
        %% table/1
        ?erl:arity_qualifier(
            ?erl:atom(table),
            ?erl:integer(1)),
        %% handle/1
        ?erl:arity_qualifier(
            ?erl:atom(handle),
            ?erl:integer(1))])]),
     %% ]).
     %% info(Name) -> Term.
     ?erl:function(
        ?erl:atom(info),
        abstract_info(Data) ++
        [?erl:clause(
            [?erl:underscore()], none,
                [abstract_apply(erlang, error, [?erl:atom(badarg)])])]),
     %% reset_counters(Name) -> boolean().
     ?erl:function(
        ?erl:atom(reset_counters),
        abstract_reset() ++
        [?erl:clause(
            [?erl:underscore()], none,
                [abstract_apply(erlang, error, [?erl:atom(badarg)])])]),
     %% table(Name) -> atom().
     ?erl:function(
        ?erl:atom(table),
        abstract_tables(Tables) ++
        [?erl:clause(
         [?erl:underscore()], none,
            [abstract_apply(erlang, error, [?erl:atom(badarg)])])]),
     %% handle(Event) - entry function
     ?erl:function(
       ?erl:atom(handle),
       [?erl:clause([?erl:variable("Event")], none,
         [abstract_count(input),
          ?erl:application(none,
            ?erl:atom(handle_), [?erl:variable("Event")])])]),
     %% input_(Node, App, Pid, Tags, Values) - filter roots
     ?erl:function(
        ?erl:atom(handle_),
        [?erl:clause([?erl:variable("Event")], none,
         abstract_filter(Tree, #state{
            event=?erl:variable("Event"),
            paramstab=ParamsTable,
            countstab=CountsTable}))])
    ],
    %% Transform Term -> Key to Key -> Term
    gr_param:transform(ParamsTable),
    AbstractMod.

%% @private Return the clauses of the table/1 function.
abstract_tables(Tables) ->
    [?erl:clause(
        [?erl:abstract(K)], none,
        [?erl:abstract(V)])
    || {K, V} <- Tables].

%% @private Return the clauses of the info/1 function.
abstract_info(#module{'query'=Query}) ->
    [?erl:clause([?erl:abstract(K)], none, V)
        || {K, V} <- [
        {'query', abstract_query(Query)},
        {input, abstract_getcount(input)},
        {filter, abstract_getcount(filter)},
        {output, abstract_getcount(output)}
    ]].


abstract_reset() ->
    [?erl:clause([?erl:abstract(K)], none, V)
        || {K, V} <- [
        {all, abstract_resetcount([input, filter, output])},
        {input, abstract_resetcount(input)},
        {filter, abstract_resetcount(filter)},
        {output, abstract_resetcount(output)}
    ]].


%% @private Return the original query as an expression.
abstract_query({with, _, _}) ->
    [?erl:abstract([])];
abstract_query(Query) ->
    [?erl:abstract(Query)].


%% @private Return a list of expressions to apply a filter.
%% @todo Allow mulitple functions to be specified using `with/2'.
-spec abstract_filter(glc_ops:op(), #state{}) -> [syntaxTree()].
abstract_filter({with, Cond, Fun}, State) ->
    abstract_filter_(Cond,
        _OnMatch=fun(State2) ->
            [abstract_count(output)] ++ abstract_with(Fun, State2) end,
        _OnNomatch=fun(_State2) -> [abstract_count(filter)] end, State);
abstract_filter(Cond, State) ->
    abstract_filter_(Cond,
        _OnMatch=fun(_State2) -> [abstract_count(output)] end,
        _OnNomatch=fun(_State2) -> [abstract_count(filter)] end, State).

%% @private Return a list of expressions to apply a filter.
%% A filter expects two continuation functions which generates the expressions
%% to apply when the filter matches or fails to match. The state passed to the
%% functions will contain all the variable bindings of previously accessed
%% fields and parameters.
-spec abstract_filter_(glc_ops:op(), nextFun(), nextFun(), #state{}) ->
        syntaxTree().
abstract_filter_({null, true}, OnMatch, _OnNomatch, State) ->
    OnMatch(State);
abstract_filter_({null, false}, _OnMatch, OnNomatch, State) ->
    OnNomatch(State);
abstract_filter_({Key, '*'}, OnMatch, OnNomatch, State) ->
    abstract_getkey(Key,
        _OnMatch=fun(#state{}=State2) -> OnMatch(State2) end,
        _OnNomatch=fun(State2) -> OnNomatch(State2) end, State);
abstract_filter_({Key, '!'}, OnMatch, OnNomatch, State) ->
    abstract_getkey(Key,
        _OnNomatch=fun(State2) -> OnNomatch(State2) end, 
        _OnMatch=fun(#state{}=State2) -> OnMatch(State2) end,
                    State);
abstract_filter_({Key, Op, Value}, OnMatch, OnNomatch, State)
        when Op =:= '>'; Op =:= '='; Op =:= '<' ->
    Op2 = case Op of '=' -> '=:='; Op -> Op end,
    abstract_opfilter(Key, Op2, Value, OnMatch, OnNomatch, State);
abstract_filter_({'any', Conds}, OnMatch, OnNomatch, State) ->
    abstract_any(Conds, OnMatch, OnNomatch, State);
abstract_filter_({'all', Conds}, OnMatch, OnNomatch, State) ->
    abstract_all(Conds, OnMatch, OnNomatch, State).

%% @private Return a branch based on a built in operator.
-spec abstract_opfilter(atom(), atom(), term(), nextFun(),
        nextFun(), #state{}) -> [syntaxTree()].
abstract_opfilter(Key, Opname, Value, OnMatch, OnNomatch, State) ->
    abstract_getkey(Key,
        _OnMatch=fun(#state{}=State2) ->
            [?erl:case_expr(
                abstract_apply(erlang, Opname, [
                        ?erl:variable(field_variable(Key)),
                        ?erl:abstract(Value)
                ]),
                [?erl:clause([?erl:atom(true)], none, 
                    OnMatch(State2)),
                 ?erl:clause([?erl:atom(false)], none,
                    OnNomatch(State2))])] end,
        _OnNomatch=fun(State2) -> OnNomatch(State2) end, State).

%% @private Generate an `all' filter.
%% An `all' filter is evaluated by testing all conditions that must hold. If
%% any of the conditions does not hold the evaluation is short circuted at that
%% point. This means that the `OnNomatch' branch is executed once for each
%% condition. The `OnMatch' branch is only executed once.
-spec abstract_all([glc_ops:op()], nextFun(), nextFun(), #state{}) ->
        [syntaxTree()].
abstract_all([H|T], OnMatch, OnNomatch, State) ->
    abstract_filter_(H,
        _OnMatch=fun(State2) -> abstract_all(T, OnMatch, OnNomatch, State2)
            end, OnNomatch, State);
abstract_all([], OnMatch, _OnNomatch, State) ->
    OnMatch(State).

%% @private
-spec abstract_any([glc_ops:op()], nextFun(), nextFun(), #state{}) ->
        [syntaxTree()].
abstract_any([H|T], OnMatch, OnNomatch, State) ->
    abstract_filter_(H, OnMatch,
        _OnNomatch=fun(State2) -> abstract_any(T, OnMatch, OnNomatch, State2)
        end, State);
abstract_any([], _OnMatch, OnNomatch, State) ->
    OnNomatch(State).

%% @private
-spec abstract_with(fun((gre:event()) -> term()), #state{}) -> [syntaxTree()].
abstract_with(Fun, State) when is_function(Fun, 1) ->
    abstract_getparam(Fun, fun(#state{event=Event, paramvars=Params}) ->
            {_, Fun2} = lists:keyfind(Fun, 1, Params),
            [?erl:application(none, Fun2, [Event])]
        end, State).

%% @private Bind the value of a field to a variable.
%% If the value of a field has already been bound to a variable the previous
%% binding is reused over re-accessing the value. The `OnMatch' function is
%% expected to access the variable stored in the state record. The `OnNomatch'
%% function must not attempt to access the variable.
-spec abstract_getkey(atom(), nextFun(), nextFun(), #state{}) ->
        [syntaxTree()].
abstract_getkey(Key, OnMatch, OnNomatch, #state{fields=Fields}=State) ->
    case lists:keyfind(Key, 1, Fields) of
        {Key, _Variable} -> OnMatch(State);
        false -> abstract_getkey_(Key, OnMatch, OnNomatch, State)
    end.


-spec abstract_getkey_(atom(), nextFun(), nextFun(), #state{}) ->
        [syntaxTree()].
abstract_getkey_(Key, OnMatch, OnNomatch, #state{
        event=Event, fields=Fields}=State) ->
    [?erl:case_expr(
        abstract_apply(gre, find, [?erl:atom(Key), Event]),
        [?erl:clause([
            ?erl:tuple([
                ?erl:atom(true),
                ?erl:variable(field_variable(Key))])], none,
             OnMatch(State#state{
                fields=[{Key, ?erl:variable(field_variable(Key))}
                    |Fields]})),
         ?erl:clause([
            ?erl:atom(false)], none,
            OnNomatch(State))
        ]
    )].

%% @private Bind the value of a parameter to a variable.
%% During code generation the parameter value is used as the identity of the
%% parameter. At runtime a unique integer is used as the identity.
-spec abstract_getparam(term(), nextFun(), #state{}) -> [syntaxTree()].
abstract_getparam(Term, OnBound, #state{paramvars=Params}=State) ->
    case lists:keyfind(Term, 1, Params) of
        {_, _Variable} -> OnBound(State);
        %% parameter not bound to variable in this scope.
        false -> abstract_getparam_(Term, OnBound, State)
    end.


-spec abstract_getparam_(term(), nextFun(), #state{}) -> [syntaxTree()].
abstract_getparam_(Term, OnBound, #state{paramstab=ParamsTable,
        paramvars=Params}=State) ->
    Key = case gr_param:lookup(ParamsTable, Term) of
        [{_, Key2}] ->
            Key2;
        [] ->
            Key2 = gr_param:info_size(ParamsTable),
            gr_param:insert(ParamsTable, {Term, Key2}),
            Key2
    end,
    [?erl:match_expr(
        param_variable(Key),
        abstract_apply(gr_param, lookup_element,
            [abstract_apply(table, [?erl:atom(params)]),
             ?erl:abstract(Key)]))
    ] ++ OnBound(State#state{paramvars=[{Term, param_variable(Key)}|Params]}).

%% @private Generate a variable name for the value of a field.
-spec field_variable(atom()) -> string().
field_variable(Key) ->
    "Field_" ++ field_variable_(atom_to_list(Key)).

%% @private Escape non-alphanumeric values.
-spec field_variable_(string()) -> string().
field_variable_([H|T]) when H >= $0, H =< $9 ->
    [H|field_variable_(T)];
field_variable_([H|T]) when H >= $A, H =< $Z ->
    [H|field_variable_(T)];
field_variable_([H|T]) when H >= $a, H =< $z ->
    [H|field_variable_(T)];
field_variable_([H|T]) ->
    "_" ++ integer_to_list(H, 16) ++ "_" ++ field_variable_(T);
field_variable_([]) ->
    [].

%% @private Generate a variable name for the value of a parameter.
-spec param_variable(integer()) -> syntaxTree().
param_variable(Key) ->
    ?erl:variable("Param_" ++ integer_to_list(Key)).

%% @ private Generate a list of field variable names.
%% Walk the query tree and generate a safe variable name string for each field
%% that is accessed by the conditions in the query. Only allow alpha-numeric.
%%-spec field_variables(glc_ops:op()) -> [{atom(), string()}].
%%field_variables(Query) ->
%%    lists:usort(field_variables_(Query)).

%%-spec field_variables(glc_ops:op()) -> [{atom(), string()}].
%%field_variables_({Key, '=', _Term}) ->
%%    [{Key, field_variable(Key)}].



%% @private Return an expression to increment a counter.
%% @todo Pass state record. Only Generate code if `statistics' is enabled.
-spec abstract_count(atom()) -> syntaxTree().
abstract_count(Counter) ->
    abstract_apply(gr_counter, update_counter,
        [abstract_apply(table, [?erl:atom(counters)]),
         ?erl:abstract(Counter),
         ?erl:abstract({2,1})]).


%% @private Return an expression to get the value of a counter.
%% @todo Pass state record. Only Generate code if `statistics' is enabled.
-spec abstract_getcount(atom()) -> [syntaxTree()].
abstract_getcount(Counter) ->
    [abstract_apply(gr_counter, lookup_element,
        [abstract_apply(table, [?erl:atom(counters)]),
         ?erl:abstract(Counter)])].

%% @private Return an expression to reset a counter.
-spec abstract_resetcount(atom() | [filter | input | output]) -> [syntaxTree()].
abstract_resetcount(Counter) ->
    [abstract_apply(gr_counter, reset_counters,
        [abstract_apply(table, [?erl:atom(counters)]),
         ?erl:abstract(Counter)])].



%% abstract code util functions


%% @private Compile an abstract module.
-spec compile_forms(term(), [term()]) -> {ok, atom(), binary()}.
compile_forms(Forms, Opts) ->
    case compile:forms(Forms, Opts) of
        {ok, Module, Binary} ->
            {ok, Module, Binary};
        {ok, Module, Binary, _Warnings} ->
            {ok, Module, Binary};
        Error ->
            erlang:error({compile_forms, Error})
    end.

%% @private Load a module binary.
-spec load_binary(atom(), binary()) -> {ok, loaded, atom()}.
load_binary(Module, Binary) ->
    case code:load_binary(Module, "", Binary) of
        {module, Module}  -> {ok, loaded, Module};
        {error, Reason} -> exit({error_loading_module, Module, Reason})
    end.

%% @private Apply an exported function.
-spec abstract_apply(atom(), atom(), [syntaxTree()]) -> syntaxTree().
abstract_apply(Module, Function, Arguments) ->
    ?erl:application(?erl:atom(Module), ?erl:atom(Function), Arguments).

%% @private Apply a module local function.
-spec abstract_apply(atom(), [syntaxTree()]) -> syntaxTree().
abstract_apply(Function, Arguments) ->
    ?erl:application(?erl:atom(Function), Arguments).
