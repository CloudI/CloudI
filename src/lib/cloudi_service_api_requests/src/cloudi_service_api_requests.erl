%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Service API Requests==
%%% A service that exposes dynamic configuration of CloudI.
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2011-2023 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2011-2023 Michael Truog
%%% @version 2.0.6 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_api_requests).
-author('mjtruog at protonmail dot com').

-behaviour(cloudi_service).

%% external interface
-export([from_erl/2,
         from_json/2,
         to_erl/2,
         to_erl/3,
         to_json/2,
         to_json/3]).

%% cloudi_service callbacks
-export([cloudi_service_init/4,
         cloudi_service_handle_request/11,
         cloudi_service_terminate/3]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").
-include_lib("cloudi_core/include/cloudi_service_api.hrl").

-record(state,
    {
        functions :: cloudi_x_trie:cloudi_x_trie(), % name -> {method,arity}
        prefix :: cloudi:service_name_pattern()
    }).

-define(FORMAT_ERLANG, "erl").
-define(FORMAT_JSON, "json").

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

-spec from_erl(_Method :: atom(),
               Request :: binary()) ->
    any().

from_erl(_Method, Request) ->
    convert_erlang_to_term(Request).

-spec from_json(Method :: atom(),
                Request :: binary()) ->
    any().

from_json(Method, Request) ->
    convert_json_to_term(Method, Request).

-spec to_erl(Method :: atom(),
             Result :: any()) ->
    binary().

to_erl(Method, Result) ->
    to_erl(Method, Result, true).

-spec to_erl(Method :: atom(),
             Result :: any(),
             Space :: boolean()) ->
    binary().

to_erl(Method, Result, Space) ->
    convert_term_to_erlang(convert_api_data(Method, Result), Space).

-spec to_json(Method :: atom(),
              Result :: any()) ->
    binary().

to_json(Method, Result) ->
    to_json(Method, Result, true).

-spec to_json(Method :: atom(),
              Result :: any(),
              Space :: boolean()) ->
    binary().

to_json(Method, Result, Space) ->
    convert_term_to_json(convert_api_data(Method, Result), Method, Space).

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init([], Prefix, _Timeout, Dispatcher) ->
    false = cloudi_service_name:pattern(Prefix),
    Exports = cloudi_x_reltool_util:module_exports(cloudi_service_api),
    CloudIServiceAPI = lists:foldl(fun({Method, Arity} = F, Functions) ->
        % service names are (prefix)rpc/(method)(format-extension)
        % (content-type hint as a file extension)
        FormatSuffix = if
            Arity == 1 ->
                "/get";
            Arity == 2 ->
                "/post"
        end,
        MethodName = erlang:atom_to_list(Method),
        lists:foreach(fun(Format) ->
            FormatName = "rpc/" ++ MethodName ++ "." ++ Format,
            cloudi_service:subscribe(Dispatcher, FormatName),
            cloudi_service:subscribe(Dispatcher, FormatName ++ FormatSuffix)
        end, [?FORMAT_ERLANG, ?FORMAT_JSON]),
        cloudi_x_trie:store(MethodName, F, Functions)
    end, cloudi_x_trie:new(), Exports),
    % service names for JSON-RPC are:
    cloudi_service:subscribe(Dispatcher, "rpc.json"),
    cloudi_service:subscribe(Dispatcher, "rpc.json/post"),
    {ok, #state{functions = CloudIServiceAPI,
                prefix = Prefix}}.

cloudi_service_handle_request(_RequestType, Name, _Pattern,
                              _RequestInfo, Request,
                              Timeout, _Priority, _TransId, _Source,
                              #state{prefix = Prefix} = State, _Dispatcher) ->
    Response = case cloudi_service_name:suffix(Prefix, Name) of
        "rpc/" ++ MethodSuffix ->
            format_rpc(cloudi_string:beforel($/, MethodSuffix, input),
                       Request, Timeout, State);
        "rpc.json" ++ _ ->
            format_json_rpc(Request, Timeout, State)
    end,
    {reply, Response, State}.

cloudi_service_terminate(_Reason, _Timeout, _State) ->
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

format_rpc(MethodFormat, Request, Timeout,
           #state{functions = Functions}) ->
    {MethodName, Format} = cloudi_string:splitr($., MethodFormat),
    {Method, Arity} = cloudi_x_trie:fetch(MethodName, Functions),
    case Format of
        ?FORMAT_ERLANG ->
            format_erlang_call(Method, Arity, Request, Timeout);
        ?FORMAT_JSON ->
            format_json_call(Method, Arity, Request, Timeout)
    end.

format_erlang_call(_, _, Request, _)
    when not is_binary(Request) ->
    <<>>;
format_erlang_call(Method, 1, _, Timeout) ->
    try cloudi_service_api_call(Method, Timeout) of
        Result ->
            convert_term_to_erlang(Result)
    catch
        ErrorType:Error ->
            ?LOG_DEBUG("~p ~p", [ErrorType, Error]),
            <<>>
    end;
format_erlang_call(Method, 2, Request, Timeout) ->
    Arg1 = convert_erlang_to_term(Request),
    try cloudi_service_api_call(Method, Arg1, Timeout) of
        Result ->
            convert_term_to_erlang(Result)
    catch
        exit:invalid_input ->
            convert_term_to_erlang({error, timeout});
        ErrorType:Error ->
            ?LOG_DEBUG("~p ~p", [ErrorType, Error]),
            <<>>
    end.

format_json_call(_, _, Request, _)
    when not is_binary(Request) ->
    <<>>;
format_json_call(Method, 1, _, Timeout) ->
    try cloudi_service_api_call(Method, Timeout) of
        Result ->
            convert_term_to_json(Result, Method, true)
    catch
        ErrorType:Error ->
            ?LOG_DEBUG("~p ~p", [ErrorType, Error]),
            <<>>
    end;
format_json_call(Method, 2, Request, Timeout) ->
    Arg1 = convert_json_to_term(Method, Request),
    try cloudi_service_api_call(Method, Arg1, Timeout) of
        Result ->
            convert_term_to_json(Result, Method, true)
    catch
        exit:invalid_input ->
            convert_term_to_json({error, timeout}, Method, true);
        ErrorType:Error ->
            ?LOG_DEBUG("~p ~p", [ErrorType, Error]),
            <<>>
    end.

-spec format_json_rpc(Request :: any(),
                      Timeout :: cloudi:timeout_value_milliseconds(),
                      State :: #state{}) -> binary().

format_json_rpc(Request, _, _)
    when not is_binary(Request) ->
    cloudi_json_rpc:error_parsing();
format_json_rpc(Request, Timeout,
                #state{functions = Functions}) ->
    try cloudi_json_rpc:request_to_term(Request) of
        {MethodNameBin, Params, Id} ->
            MethodName = erlang:binary_to_list(MethodNameBin),
            case cloudi_x_trie:find(MethodName, Functions) of
                error ->
                    cloudi_json_rpc:error_method_not_found(Id);
                {ok, {Method, 1}} when Params == [] ->
                    format_json_rpc_call(Method, 1, undefined, Timeout, Id);
                {ok, {Method, 2}} when length(Params) == 1 ->
                    format_json_rpc_call(Method, 2, hd(Params), Timeout, Id);
                {ok, _} ->
                    cloudi_json_rpc:error_invalid_params(Id)
            end
    catch
        _:_ ->
            cloudi_json_rpc:error_invalid_request()
    end.

format_json_rpc_call(Method, 1, _, Timeout, Id) ->
    try cloudi_service_api_call(Method, Timeout) of
        Result ->
            ResultJSON = convert_term_to_json(Result, Method, false),
            cloudi_json_rpc:response_to_json(ResultJSON, Id)
    catch
        ErrorType:Error ->
            ?LOG_DEBUG("~p ~p", [ErrorType, Error]),
            <<>>
    end;
format_json_rpc_call(_, 2, Param, _, Id)
    when not is_binary(Param) ->
    cloudi_json_rpc:error_invalid_params(Id);
format_json_rpc_call(Method, 2, Param, Timeout, Id) ->
    Arg1 = convert_json_to_term(Method, Param),
    try cloudi_service_api_call(Method, Arg1, Timeout) of
        Result ->
            ResultJSON = convert_term_to_json(Result, Method, false),
            cloudi_json_rpc:response_to_json(ResultJSON, Id)
    catch
        exit:invalid_input ->
            <<>>;
        ErrorType:Error ->
            ?LOG_DEBUG("~p ~p", [ErrorType, Error]),
            <<>>
    end.

cloudi_service_api_call(Method, Timeout) ->
    convert_api_data(Method, cloudi_service_api:Method(Timeout)).

cloudi_service_api_call(_, invalid, _) ->
    erlang:exit(invalid_input);
cloudi_service_api_call(Method, Input, Timeout) ->
    convert_api_data(Method, cloudi_service_api:Method(Input, Timeout)).

convert_api_data(Method,
                 {ok, L})
    when Method =:= services;
         Method =:= services_search;
         Method =:= services_status ->
    {ok, [{service_id(ID), Data} || {ID, Data} <- L]};
convert_api_data(code_status,
                 {ok, Status}) ->
    {runtime_cloudi_changes,
     RuntimeChanges} = lists:keyfind(runtime_cloudi_changes, 1, Status),
    RuntimeChangesNew = lists:map(fun(RuntimeChange) ->
        {service_ids,
         IDs} = lists:keyfind(service_ids, 1, RuntimeChange),
        lists:keyreplace(service_ids, 1, RuntimeChange,
                         {service_ids, [service_id(ID) || ID <- IDs]})
    end, RuntimeChanges),
    StatusNew = lists:keyreplace(runtime_cloudi_changes, 1, Status,
                                 {runtime_cloudi_changes,
                                  RuntimeChangesNew}),
    {ok, StatusNew};
convert_api_data(services_add,
                 {ok, L}) ->
    {ok, [service_id(ID) || ID <- L]};
convert_api_data(services_update,
                 {ok, SuccessSet}) ->
    {ok, [[service_id(ID) || ID <- L] || L <- SuccessSet]};
convert_api_data(services_update,
                 {error, {ErrorL, Reason}, SuccessSet}) ->
    {error,
     {[service_id(ID) || ID <- ErrorL], Reason},
     [[service_id(ID) || ID <- L] || L <- SuccessSet]};
convert_api_data(_Method, Result) ->
    Result.

convert_erlang_to_term(Request) ->
    try cloudi_string:binary_to_term(Request)
    catch
        _:_ ->
            invalid
    end.

convert_term_to_erlang(Result) ->
    convert_term_to_erlang(Result, true).

convert_term_to_erlang({ok, Result}, Space) ->
    convert_term_to_erlang_string(Result, Space);
convert_term_to_erlang(Result, Space) ->
    convert_term_to_erlang_string(Result, Space).

convert_term_to_erlang_string(Result, true) ->
    cloudi_string:format_to_binary("~p", [Result]);
convert_term_to_erlang_string(Result, false) ->
    cloudi_string:term_to_binary_compact(Result).

convert_json_to_term(Method, Request)
    when Method =:= acl_add ->
    case json_decode(Request) of
        [_ | _] = L ->
            convert_json_to_term_acl_add(L);
        _ ->
            invalid
    end;
convert_json_to_term(Method, Request)
    when Method =:= acl_remove;
         Method =:= nodes_status ->
    case json_decode(Request) of
        List when is_list(List) ->
            convert_json_to_term_atoms_existing(List);
        _ ->
            invalid
    end;
convert_json_to_term(Method, Request)
    when Method =:= service_subscriptions;
         Method =:= logging_file_set;
         Method =:= code_path_add;
         Method =:= code_path_remove ->
    case json_decode(Request) of
        Binary when is_binary(Binary) ->
            erlang:binary_to_list(Binary);
        _ ->
            invalid
    end;
convert_json_to_term(Method, Request)
    when Method =:= services_add ->
    case json_decode(Request) of
        [_ | _] = L ->
            convert_json_to_term_services(L);
        _ ->
            invalid
    end;
convert_json_to_term(Method, Request)
    when Method =:= services_remove;
         Method =:= services_restart;
         Method =:= services_suspend;
         Method =:= services_resume;
         Method =:= services_status ->
    case json_decode(Request) of
        List when is_list(List) ->
            convert_json_to_term_strings(List);
        _ ->
            invalid
    end;
convert_json_to_term(Method, Request)
    when Method =:= services_search ->
    case json_decode(Request) of
        Binary when is_binary(Binary) ->
            erlang:binary_to_list(Binary);
        [{ScopeBinary, NameBinary}]
            when is_binary(ScopeBinary),
                 is_binary(NameBinary) ->
            {erlang:binary_to_existing_atom(ScopeBinary, utf8),
             erlang:binary_to_list(NameBinary)};
        _ ->
            invalid
    end;
convert_json_to_term(Method, Request)
    when Method =:= services_update ->
    case json_decode(Request) of
        List when is_list(List) ->
            convert_json_to_term_updates(List);
        _ ->
            invalid
    end;
convert_json_to_term(Method, Request)
    when Method =:= nodes_add;
         Method =:= nodes_remove ->
    case json_decode(Request) of
        List when is_list(List) ->
            convert_json_to_term_atoms(List);
        _ ->
            invalid
    end;
convert_json_to_term(Method, Request)
    when Method =:= logging_level_set;
         Method =:= logging_stdout_set ->
    case json_decode(Request) of
        Binary when is_binary(Binary) ->
            erlang:binary_to_existing_atom(Binary, utf8);
        _ ->
            invalid
    end;
convert_json_to_term(Method, Request)
    when Method =:= nodes_set;
         Method =:= logging_set ->
    case json_decode(Request) of
        List when is_list(List) ->
            convert_json_to_term_options(List);
        _ ->
            invalid
    end;
convert_json_to_term(Method, Request)
    when Method =:= logging_syslog_set;
         Method =:= logging_formatters_set ->
    case json_decode(Request) of
        List when is_list(List) ->
            convert_json_to_term_options(List);
        <<"undefined">> ->
            undefined;
        _ ->
            invalid
    end;
convert_json_to_term(Method, Request)
    when Method =:= logging_redirect_set ->
    case json_decode(Request) of
        Binary when is_binary(Binary) ->
            erlang:binary_to_atom(Binary, utf8);
        _ ->
            invalid
    end.

convert_json_to_term_acl_add(AddL) ->
    convert_json_to_term_acl_add(AddL, []).

convert_json_to_term_acl_add([], L) ->
    lists:reverse(L);
convert_json_to_term_acl_add([{KeyBinary, ValueL} | AddL], L) ->
    Key = erlang:binary_to_atom(KeyBinary, utf8),
    Value = if
        is_binary(ValueL) ->
            cloudi_string:binary_to_term(ValueL);
        is_list(ValueL) ->
            convert_json_to_term_strings(ValueL)
    end,
    convert_json_to_term_acl_add(AddL, [{Key, Value} | L]);
convert_json_to_term_acl_add([Unknown | AddL], L) ->
    convert_json_to_term_acl_add(AddL, [Unknown | L]).

convert_json_to_term_services([]) ->
    [];
convert_json_to_term_services([Service | L]) ->
    [convert_json_to_term_service(Service) |
     convert_json_to_term_services(L)].

convert_json_to_term_service(Service) ->
    convert_json_to_term_service(Service, []).

convert_json_to_term_service([], L) ->
    lists:reverse(L);
convert_json_to_term_service([{<<"type">>, TypeBinary} | Service], L) ->
    Type = erlang:binary_to_existing_atom(TypeBinary, utf8),
    convert_json_to_term_service(Service, [{type, Type} | L]);
convert_json_to_term_service([{<<"prefix">>, PrefixBinary} | Service], L) ->
    Prefix = erlang:binary_to_list(PrefixBinary),
    convert_json_to_term_service(Service, [{prefix, Prefix} | L]);
convert_json_to_term_service([{<<"module">>, ModuleBinary} | Service], L) ->
    Module = case erlang:binary_to_list(ModuleBinary) of
        [$' | ModuleName0] ->
            [$' | ModuleNameN] = lists:reverse(ModuleName0),
            erlang:list_to_atom(lists:reverse(ModuleNameN));
        [$" | FileName0] ->
            [$" | FileNameN] = lists:reverse(FileName0),
            lists:reverse(FileNameN);
        _ ->
            erlang:binary_to_atom(ModuleBinary, utf8)
    end,
    convert_json_to_term_service(Service, [{module, Module} | L]);
convert_json_to_term_service([{<<"file_path">>, FileBinary} | Service], L) ->
    FilePath = erlang:binary_to_list(FileBinary),
    convert_json_to_term_service(Service, [{file_path, FilePath} | L]);
convert_json_to_term_service([{<<"args">>, ArgsBinary} | Service], L) ->
    Args = case erlang:binary_to_list(ArgsBinary) of
        [$[ | _] = ArgsList ->
            cloudi_string:list_to_term(ArgsList);
        ArgsString ->
            ArgsString
    end,
    convert_json_to_term_service(Service, [{args, Args} | L]);
convert_json_to_term_service([{<<"env">>, EnvL} | Service], L)
    when is_list(EnvL) ->
    Env = [cloudi_string:splitl($=, erlang:binary_to_list(EnvString))
           || EnvString <- EnvL],
    convert_json_to_term_service(Service, [{env, Env} | L]);
convert_json_to_term_service([{<<"dest_refresh">>, Binary} | Service], L) ->
    DestRefresh = erlang:binary_to_existing_atom(Binary, utf8),
    convert_json_to_term_service(Service, [{dest_refresh, DestRefresh} | L]);
convert_json_to_term_service([{<<"protocol">>, Binary} | Service], L) ->
    Protocol = erlang:binary_to_existing_atom(Binary, utf8),
    convert_json_to_term_service(Service, [{protocol, Protocol} | L]);
convert_json_to_term_service([{<<"buffer_size">>, Value} | Service], L) ->
    BufferSize = if
        Value == <<"default">> ->
            default;
        true ->
            Value
    end,
    convert_json_to_term_service(Service, [{buffer_size, BufferSize} | L]);
convert_json_to_term_service([{<<"timeout_init">>, Value} | Service], L) ->
    TimeoutInit = convert_json_to_term_period(Value),
    convert_json_to_term_service(Service, [{timeout_init, TimeoutInit} | L]);
convert_json_to_term_service([{<<"timeout_async">>, Value} | Service], L) ->
    TimeoutAsync = convert_json_to_term_period(Value),
    convert_json_to_term_service(Service, [{timeout_async, TimeoutAsync} | L]);
convert_json_to_term_service([{<<"timeout_sync">>, Value} | Service], L) ->
    TimeoutSync = convert_json_to_term_period(Value),
    convert_json_to_term_service(Service, [{timeout_sync, TimeoutSync} | L]);
convert_json_to_term_service([{<<"dest_list_deny">>, DenyL} | Service], L) ->
    Deny = if
        is_binary(DenyL) ->
            cloudi_string:binary_to_term(DenyL);
        is_list(DenyL) ->
            convert_json_to_term_strings(DenyL)
    end,
    convert_json_to_term_service(Service, [{dest_list_deny, Deny} | L]);
convert_json_to_term_service([{<<"dest_list_allow">>, AllowL} | Service], L) ->
    Allow = if
        is_binary(AllowL) ->
            cloudi_string:binary_to_term(AllowL);
        is_list(AllowL) ->
            convert_json_to_term_strings(AllowL)
    end,
    convert_json_to_term_service(Service, [{dest_list_allow, Allow} | L]);
convert_json_to_term_service([{<<"count_process">>, Value} | Service], L) ->
    convert_json_to_term_service(Service, [{count_process, Value} | L]);
convert_json_to_term_service([{<<"count_thread">>, Value} | Service], L) ->
    convert_json_to_term_service(Service, [{count_thread, Value} | L]);
convert_json_to_term_service([{<<"max_r">>, Value} | Service], L) ->
    convert_json_to_term_service(Service, [{max_r, Value} | L]);
convert_json_to_term_service([{<<"max_t">>, Value} | Service], L) ->
    MaxT = convert_json_to_term_period(Value),
    convert_json_to_term_service(Service, [{max_t, MaxT} | L]);
convert_json_to_term_service([{<<"options">>, OptionsL} | Service], L) ->
    Options = if
        is_binary(OptionsL) ->
            cloudi_string:binary_to_term(OptionsL);
        is_list(OptionsL) ->
            convert_json_to_term_options(OptionsL)
    end,
    convert_json_to_term_service(Service, [{options, Options} | L]);
convert_json_to_term_service([Unknown | Service], L) ->
    convert_json_to_term_service(Service, [Unknown | L]).

convert_json_to_term_updates([]) ->
    [];
convert_json_to_term_updates([{ServiceId, Options} | Update]) ->
    [{erlang:binary_to_list(ServiceId),
      convert_json_to_term_options(Options)} |
     convert_json_to_term_updates(Update)].

convert_json_to_term_options([]) ->
    [];
convert_json_to_term_options([{Key, Value} | Options]) ->
    [{erlang:binary_to_existing_atom(Key, utf8),
      convert_json_to_term_option(Value)} |
     convert_json_to_term_options(Options)].

convert_json_to_term_option(<<"#{", _/binary>> = Value) ->
    cloudi_string:binary_to_term(Value);
convert_json_to_term_option(<<"[", _/binary>> = Value) ->
    cloudi_string:binary_to_term(Value);
convert_json_to_term_option(<<"{", _/binary>> = Value) ->
    cloudi_string:binary_to_term(Value);
convert_json_to_term_option(<<"\"", _/binary>> = Value) ->
    cloudi_string:binary_to_term(Value);
convert_json_to_term_option(<<"'", _/binary>> = Value) ->
    cloudi_string:binary_to_term(Value);
convert_json_to_term_option(Value)
    when is_binary(Value) ->
    erlang:binary_to_list(Value);
convert_json_to_term_option(Value)
    when is_number(Value); is_boolean(Value) ->
    Value;
convert_json_to_term_option(Value)
    when is_list(Value) ->
    convert_json_to_term_options(Value).

convert_json_to_term_strings([]) ->
    [];
convert_json_to_term_strings([H | L])
    when is_binary(H) ->
    [erlang:binary_to_list(H) |
     convert_json_to_term_strings(L)];
convert_json_to_term_strings([_ | _]) ->
    invalid.

convert_json_to_term_atoms_existing([]) ->
    [];
convert_json_to_term_atoms_existing([H | L])
    when is_binary(H) ->
    [erlang:binary_to_existing_atom(H, utf8) |
     convert_json_to_term_atoms_existing(L)];
convert_json_to_term_atoms_existing([_ | _]) ->
    invalid.

convert_json_to_term_atoms([]) ->
    [];
convert_json_to_term_atoms([H | L])
    when is_binary(H) ->
    [erlang:binary_to_atom(H, utf8) |
     convert_json_to_term_atoms(L)];
convert_json_to_term_atoms([_ | _]) ->
    invalid.

convert_json_to_term_period(<<"limit_max">>) ->
    limit_max;
convert_json_to_term_period(<<"limit_min">>) ->
    limit_min;
convert_json_to_term_period(<<"{", _/binary>> = Value) ->
    cloudi_string:binary_to_term(Value);
convert_json_to_term_period([{Unit, Multiplier}])
    when is_integer(Multiplier),
         Unit == <<"seconds">> orelse Unit == <<"minutes">> orelse
         Unit == <<"hours">> orelse Unit == <<"days">> orelse
         Unit == <<"second">> orelse Unit == <<"minute">> orelse
         Unit == <<"hour">> orelse Unit == <<"day">> ->
    {Multiplier, erlang:binary_to_existing_atom(Unit, utf8)};
convert_json_to_term_period(Value)
    when is_integer(Value) ->
    Value.

convert_term_to_json(ok, _, Space) ->
    json_encode([{<<"success">>, true}], Space);
convert_term_to_json({error, Reason}, _, Space) ->
    ReasonBinary = cloudi_string:term_to_binary_compact(Reason),
    json_encode([{<<"success">>, false},
                 {<<"error">>, ReasonBinary}], Space);
convert_term_to_json({error, {ErrorL, Reason}, SuccessL},
                     services_update, Space) ->
    ReasonBinary = cloudi_string:term_to_binary_compact(Reason),
    json_encode([{<<"success">>, false},
                 {<<"success_ids">>,
                  convert_term_to_json_strings(SuccessL)},
                 {<<"error">>, ReasonBinary},
                 {<<"error_ids">>,
                  convert_term_to_json_strings(ErrorL)}], Space);
convert_term_to_json({ok, Services}, Method, Space)
    when Method =:= services;
         Method =:= services_search ->
    json_encode([{<<"success">>, true},
                 {erlang:atom_to_binary(Method, utf8),
                  [convert_term_to_json_service(Service, Id)
                   || {Id, Service} <- Services]}], Space);
convert_term_to_json({ok, Statuses}, services_status = Method, Space) ->
    json_encode([{<<"success">>, true},
                 {erlang:atom_to_binary(Method, utf8),
                  [[{<<"id">>, erlang:list_to_binary(Id)} |
                    convert_term_to_json_service_status(Status)]
                   || {Id, Status} <- Statuses]}], Space);
convert_term_to_json({ok, SuccessL}, Method, Space)
    when Method =:= services_add;
         Method =:= services_update;
         Method =:= code_path ->
    json_encode([{<<"success">>, true},
                 {erlang:atom_to_binary(Method, utf8),
                  convert_term_to_json_strings(SuccessL)}], Space);
convert_term_to_json({ok, SuccessL}, Method, Space)
    when Method =:= nodes;
         Method =:= nodes_alive;
         Method =:= nodes_dead ->
    json_encode([{<<"success">>, true},
                 {erlang:atom_to_binary(Method, utf8),
                  convert_term_to_json_atoms(SuccessL)}], Space);
convert_term_to_json({ok, Statuses}, nodes_status = Method, Space) ->
    json_encode([{<<"success">>, true},
                 {erlang:atom_to_binary(Method, utf8),
                  [[{<<"node">>, erlang:atom_to_binary(Node, utf8)} |
                    convert_term_to_json_options(Status)]
                   || {Node, Status} <- Statuses]}], Space);
convert_term_to_json({ok, Options}, logging = Method, Space) ->
    json_encode([{<<"success">>, true},
                 {erlang:atom_to_binary(Method, utf8),
                  convert_term_to_json_logging_options(Options)}], Space);
convert_term_to_json({ok, Options}, Method, Space)
    when Method =:= acl;
         Method =:= nodes_get;
         Method =:= logging_status;
         Method =:= code_status ->
    json_encode([{<<"success">>, true},
                 {erlang:atom_to_binary(Method, utf8),
                  convert_term_to_json_options(Options)}], Space).

convert_term_to_json_service(#internal{prefix = Prefix,
                                       module = Module,
                                       args = Args,
                                       dest_refresh = DestRefresh,
                                       timeout_init = TimeoutInit,
                                       timeout_async = TimeoutAsync,
                                       timeout_sync = TimeoutSync,
                                       dest_list_deny = DestListDeny,
                                       dest_list_allow = DestListAllow,
                                       count_process = CountProcess,
                                       max_r = MaxR,
                                       max_t = MaxT,
                                       options = Options}, Id) ->
    Service0 = [{<<"count_process">>, CountProcess},
                {<<"max_r">>, MaxR},
                {<<"max_t">>, convert_term_to_json_period_seconds(MaxT)},
                {<<"options">>, convert_term_to_json_service_options(Options)}],
    Service1 = if
        DestListAllow =:= undefined ->
            Service0;
        is_list(DestListAllow) ->
            [{<<"dest_list_allow">>,
              [cloudi_service_name:utf8(DestAllow)
               || DestAllow <- DestListAllow]} | Service0]
    end,
    Service2 = if
        DestListDeny =:= undefined ->
            Service1;
        is_list(DestListDeny) ->
            [{<<"dest_list_deny">>,
              [cloudi_service_name:utf8(DestDeny)
               || DestDeny <- DestListDeny]} | Service1]
    end,
    Service3 = [{<<"timeout_init">>,
                 convert_term_to_json_period_milliseconds(TimeoutInit)},
                {<<"timeout_async">>,
                 convert_term_to_json_period_milliseconds(TimeoutAsync)},
                {<<"timeout_sync">>,
                 convert_term_to_json_period_milliseconds(TimeoutSync)} |
                Service2],
    ServiceN = if
        DestRefresh =:= none ->
            Service3;
        is_atom(DestRefresh) ->
            [{<<"dest_refresh">>,
              erlang:atom_to_binary(DestRefresh, utf8)} | Service3]
    end,
    ModuleValue = if
        is_atom(Module) ->
            erlang:atom_to_binary(Module, utf8);
        is_list(Module) ->
            unicode:characters_to_binary(Module)
    end,
    [{<<"id">>, erlang:list_to_binary(Id)},
     {<<"type">>, <<"internal">>},
     {<<"prefix">>, cloudi_service_name:utf8(Prefix)},
     {<<"module">>, ModuleValue},
     {<<"args">>, cloudi_string:term_to_binary_compact(Args)} | ServiceN];
convert_term_to_json_service(#external{prefix = Prefix,
                                       file_path = FilePath,
                                       args = Args,
                                       env = Env,
                                       dest_refresh = DestRefresh,
                                       protocol = Protocol,
                                       buffer_size = BufferSize,
                                       timeout_init = TimeoutInit,
                                       timeout_async = TimeoutAsync,
                                       timeout_sync = TimeoutSync,
                                       dest_list_deny = DestListDeny,
                                       dest_list_allow = DestListAllow,
                                       count_process = CountProcess,
                                       count_thread = CountThread,
                                       max_r = MaxR,
                                       max_t = MaxT,
                                       options = Options}, Id) ->
    Service0 = [{<<"count_process">>, CountProcess},
                {<<"count_thread">>, CountThread},
                {<<"max_r">>, MaxR},
                {<<"max_t">>, convert_term_to_json_period_seconds(MaxT)},
                {<<"options">>, convert_term_to_json_service_options(Options)}],
    Service1 = if
        DestListAllow =:= undefined ->
            Service0;
        is_list(DestListAllow) ->
            [{<<"dest_list_allow">>,
              [cloudi_service_name:utf8(DestAllow)
               || DestAllow <- DestListAllow]} | Service0]
    end,
    Service2 = if
        DestListDeny =:= undefined ->
            Service1;
        is_list(DestListDeny) ->
            [{<<"dest_list_deny">>,
              [cloudi_service_name:utf8(DestDeny)
               || DestDeny <- DestListDeny]} | Service1]
    end,
    Service3 = [{<<"protocol">>, erlang:atom_to_binary(Protocol, utf8)},
                {<<"buffer_size">>, BufferSize},
                {<<"timeout_init">>,
                 convert_term_to_json_period_milliseconds(TimeoutInit)},
                {<<"timeout_async">>,
                 convert_term_to_json_period_milliseconds(TimeoutAsync)},
                {<<"timeout_sync">>,
                 convert_term_to_json_period_milliseconds(TimeoutSync)} |
                Service2],
    ServiceN = if
        DestRefresh =:= none ->
            Service3;
        is_atom(DestRefresh) ->
            [{<<"dest_refresh">>,
              erlang:atom_to_binary(DestRefresh, utf8)} | Service3]
    end,
    [{<<"id">>, erlang:list_to_binary(Id)},
     {<<"type">>, <<"external">>},
     {<<"prefix">>, cloudi_service_name:utf8(Prefix)},
     {<<"file_path">>, unicode:characters_to_binary(FilePath)},
     {<<"args">>, unicode:characters_to_binary(Args)},
     {<<"env">>,
      [unicode:characters_to_binary(Key ++ "=" ++ Value)
       || {Key, Value} <- Env]} | ServiceN].

convert_term_to_json_service_options([]) ->
    [];
convert_term_to_json_service_options([{Key, Value} | Options])
    when Key =:= aspects_init_after orelse
         Key =:= aspects_request_before orelse
         Key =:= aspects_request_after orelse
         Key =:= aspects_info_before orelse
         Key =:= aspects_info_after orelse
         Key =:= aspects_terminate_before orelse
         Key =:= aspects_suspend orelse
         Key =:= aspects_resume ->
    [{erlang:atom_to_binary(Key, utf8),
      cloudi_string:term_to_binary_compact(Value)} |
     convert_term_to_json_service_options(Options)];
convert_term_to_json_service_options([{Key, Value} | Options]) ->
    [{erlang:atom_to_binary(Key, utf8),
      convert_term_to_json_option(Value)} |
     convert_term_to_json_service_options(Options)].

convert_term_to_json_service_status([]) ->
    [];
convert_term_to_json_service_status([{pids_os, Value} | Options]) ->
    [{<<"pids_os">>, Value} |
     convert_term_to_json_service_status(Options)];
convert_term_to_json_service_status([{pids_erlang, Value} | Options]) ->
    [{<<"pids_erlang">>,
      [erlang:list_to_binary(erlang:pid_to_list(Pid)) || Pid <- Value]} |
     convert_term_to_json_service_status(Options)];
convert_term_to_json_service_status([{Key, Value} | Options]) ->
    [{erlang:atom_to_binary(Key, utf8),
      convert_term_to_json_option(Value)} |
     convert_term_to_json_service_status(Options)].

convert_term_to_json_logging_options([]) ->
    [];
convert_term_to_json_logging_options([{Key, Value} | Options])
    when Key =:= aspects_log_before orelse
         Key =:= aspects_log_after ->
    [{erlang:atom_to_binary(Key, utf8),
      cloudi_string:term_to_binary_compact(Value)} |
     convert_term_to_json_logging_options(Options)];
convert_term_to_json_logging_options([{Key, Value} | Options]) ->
    [{erlang:atom_to_binary(Key, utf8),
      convert_term_to_json_option(Value)} |
     convert_term_to_json_logging_options(Options)].

convert_term_to_json_option([] = Value) ->
    Value;
convert_term_to_json_option([{Key, _} | _] = Value)
    when is_atom(Key) ->
    convert_term_to_json_options(Value);
convert_term_to_json_option([[{Key, _} | _] | _] = Value)
    when is_atom(Key) ->
    [convert_term_to_json_options(Options)
     || Options <- Value];
convert_term_to_json_option([H | _] = Value)
    when is_integer(H), H > 0 ->
    erlang:list_to_binary(Value);
convert_term_to_json_option([[H | _] | _] = Value)
    when is_integer(H), H > 0 ->
    convert_term_to_json_strings(Value);
convert_term_to_json_option([A | _] = Value)
    when is_atom(A) ->
    convert_term_to_json_atoms(Value);
convert_term_to_json_option([_ | _] = Value) ->
    cloudi_string:term_to_binary_compact(Value);
convert_term_to_json_option(Value)
    when is_number(Value) ->
    Value;
convert_term_to_json_option(Value)
    when is_atom(Value) ->
    if
        is_boolean(Value) ->
            Value;
        true ->
            erlang:atom_to_binary(Value, utf8)
    end;
convert_term_to_json_option(Value)
    when is_tuple(Value); is_map(Value) ->
    cloudi_string:term_to_binary_compact(Value).

convert_term_to_json_options([]) ->
    [];
convert_term_to_json_options([{Key, Value} | Options]) ->
    [{erlang:atom_to_binary(Key, utf8),
      convert_term_to_json_option(Value)} |
     convert_term_to_json_options(Options)].

convert_term_to_json_strings([]) ->
    [];
convert_term_to_json_strings([S | L]) ->
    [erlang:list_to_binary(S) |
     convert_term_to_json_strings(L)].

convert_term_to_json_atoms([]) ->
    [];
convert_term_to_json_atoms([A | L]) ->
    [erlang:atom_to_binary(A, utf8) |
     convert_term_to_json_atoms(L)].

convert_term_to_json_period_milliseconds(limit_max) ->
    <<"limit_max">>;
convert_term_to_json_period_milliseconds(limit_min) ->
    <<"limit_min">>;
convert_term_to_json_period_milliseconds(Timeout) ->
    cloudi_args_type:timeout_period_to_milliseconds(Timeout).

convert_term_to_json_period_seconds(limit_min) ->
    <<"limit_min">>;
convert_term_to_json_period_seconds(Value) ->
    cloudi_args_type:period_to_seconds(Value, 0).

json_encode(Term, true) ->
    cloudi_x_jsx:encode(Term, [{indent, 1}]);
json_encode(Term, false) ->
    cloudi_x_jsx:encode(Term).

json_decode(Binary) ->
    try cloudi_x_jsx:decode(Binary, [{return_maps, false}])
    catch
        _:_ ->
            invalid
    end.

service_id(ID) ->
    cloudi_x_uuid:uuid_to_string(ID, list_nodash).

