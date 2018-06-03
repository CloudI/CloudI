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
%%% Copyright (c) 2011-2018 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2011-2018 Michael Truog
%%% @version 1.7.4 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_api_requests).
-author('mjtruog at protonmail dot com').

-behaviour(cloudi_service).

%% external interface

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

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init([], Prefix, _Timeout, Dispatcher) ->
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

cloudi_service_handle_request(_Type, Name, _Pattern, _RequestInfo, Request,
                              Timeout, _Priority, _TransId, _Pid,
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
    Arg1 = convert_json_to_term(Request, Method),
    try cloudi_service_api_call(Method, Arg1, Timeout) of
        Result ->
            convert_term_to_json(Result, Method, true)
    catch
        ErrorType:Error ->
            ?LOG_DEBUG("~p ~p", [ErrorType, Error]),
            <<>>
    end.

-spec format_json_rpc(Request :: any(),
                      Timeout :: cloudi:timeout_value_milliseconds(),
                      State :: #state{}) -> binary().

format_json_rpc(Request, _, _)
    when not is_binary(Request) ->
    cloudi_json_rpc:response_to_json(<<"not_binary">>, 0);
format_json_rpc(Request, Timeout,
                #state{functions = Functions}) ->
    try cloudi_json_rpc:request_to_term(Request) of
        {MethodNameBin, Params, Id} ->
            MethodName = erlang:binary_to_list(MethodNameBin),
            case cloudi_x_trie:find(MethodName, Functions) of
                error ->
                    cloudi_json_rpc:response_to_json(null, 1,
                                                     <<"invalid_method">>, Id);
                {ok, {Method, 1}} when Params == [] ->
                    format_json_rpc_call(Method, 1, undefined, Timeout, Id);
                {ok, {Method, 2}} when length(Params) == 1 ->
                    format_json_rpc_call(Method, 2, hd(Params), Timeout, Id);
                {ok, _} ->
                    cloudi_json_rpc:response_to_json(null, 2,
                                                     <<"invalid_params">>, Id)
            end
    catch
        _:_ ->
            cloudi_json_rpc:response_to_json(null, 0,
                                             <<"invalid_json_rpc">>, 0)
    end.

format_json_rpc_call(Method, 1, _, Timeout, Id) ->
    try cloudi_service_api_call(Method, Timeout) of
        Result ->
            cloudi_json_rpc:response_to_json(
                convert_term_to_json(Result, Method, false), Id)
    catch
        ErrorType:Error ->
            ?LOG_DEBUG("~p ~p", [ErrorType, Error]),
            <<>>
    end;
format_json_rpc_call(_, 2, Param, _, Id)
    when not is_binary(Param) ->
    cloudi_json_rpc:response_to_json(null, 3,
                                     <<"invalid_param">>, Id);
format_json_rpc_call(Method, 2, Param, Timeout, Id) ->
    Arg1 = convert_erlang_to_term(Param),
    try cloudi_service_api_call(Method, Arg1, Timeout) of
        Result ->
            cloudi_json_rpc:response_to_json(
                convert_term_to_json(Result, Method, false), Id)
    catch
        ErrorType:Error ->
            ?LOG_DEBUG("~p ~p", [ErrorType, Error]),
            <<>>
    end.

cloudi_service_api_call(services = Method, Timeout) ->
    case cloudi_service_api:Method(Timeout) of
        {ok, L} ->
            {ok, [{service_id(UUID), Data} ||
                  {UUID, Data} <- L]};
        {error, _} = Error ->
            Error
    end;
cloudi_service_api_call(Method, Timeout) ->
    cloudi_service_api:Method(Timeout).

cloudi_service_api_call(Method, Input, Timeout)
    when Method =:= services_search;
         Method =:= services_status ->
    case cloudi_service_api:Method(Input, Timeout) of
        {ok, L} ->
            {ok, [{service_id(UUID), Data} ||
                  {UUID, Data} <- L]};
        {error, _} = Error ->
            Error
    end;
cloudi_service_api_call(services_add = Method, Input, Timeout) ->
    case cloudi_service_api:Method(Input, Timeout) of
        {ok, L} ->
            {ok, [service_id(UUID) || UUID <- L]};
        {error, _} = Error ->
            Error
    end;
cloudi_service_api_call(services_update = Method, Input, Timeout) ->
    case cloudi_service_api:Method(Input, Timeout) of
        {ok, SuccessSet} ->
            {ok, [[service_id(UUID) || UUID <- L] || L <- SuccessSet]};
        {error, {ErrorL, Reason}, SuccessSet} ->
            {error,
             {[service_id(UUID) || UUID <- ErrorL], Reason},
             [[service_id(UUID) || UUID <- L] || L <- SuccessSet]};
        {error, _} = Error ->
            Error
    end;
cloudi_service_api_call(Method, Input, Timeout) ->
    cloudi_service_api:Method(Input, Timeout).

service_id(ID) ->
    cloudi_x_uuid:uuid_to_string(ID, list_nodash).

convert_erlang_to_term(Request) ->
    cloudi_string:binary_to_term(Request).

convert_term_to_erlang({ok, Result}) ->
    convert_term_to_erlang_string(Result);
convert_term_to_erlang(Result) ->
    convert_term_to_erlang_string(Result).

convert_term_to_erlang_string(Result) ->
    cloudi_string:format_to_binary("~p", [Result]).

convert_json_to_term(_Request, Method)
    when Method =:= acl_add ->
    invalid; %XXX
convert_json_to_term(Request, Method)
    when Method =:= acl_remove ->
    case json_decode(Request) of
        List when is_list(List) ->
            convert_json_to_term_atoms_existing(List);
        _ ->
            invalid
    end;
convert_json_to_term(Request, Method)
    when Method =:= service_subscriptions ->
    case json_decode(Request) of
        Binary when is_binary(Binary) ->
            erlang:binary_to_list(Binary);
        _ ->
            invalid
    end;
convert_json_to_term(_Request, Method)
    when Method =:= services_add ->
    invalid; %XXX
convert_json_to_term(Request, Method)
    when Method =:= services_remove;
         Method =:= services_restart;
         Method =:= services_status ->
    case json_decode(Request) of
        List when is_list(List) ->
            convert_json_to_term_strings(List);
        _ ->
            invalid
    end;
convert_json_to_term(_Request, Method)
    when Method =:= services_search ->
    invalid; %XXX
convert_json_to_term(_Request, Method)
    when Method =:= services_update ->
    invalid; %XXX
convert_json_to_term(_Request, Method)
    when Method =:= nodes_set ->
    invalid; %XXX
convert_json_to_term(Request, Method)
    when Method =:= nodes_add;
         Method =:= nodes_remove ->
    case json_decode(Request) of
        List when is_list(List) ->
            convert_json_to_term_atoms(List);
        _ ->
            invalid
    end;
convert_json_to_term(_Request, Method)
    when Method =:= logging_set ->
    invalid; %XXX
convert_json_to_term(_Request, Method)
    when Method =:= logging_set ->
    invalid; %XXX
convert_json_to_term(_Request, Method)
    when Method =:= logging_file_set ->
    invalid; %XXX
convert_json_to_term(_Request, Method)
    when Method =:= logging_level_set ->
    invalid; %XXX
convert_json_to_term(_Request, Method)
    when Method =:= logging_stdout_set ->
    invalid; %XXX
convert_json_to_term(_Request, Method)
    when Method =:= logging_syslog_set ->
    invalid; %XXX
convert_json_to_term(_Request, Method)
    when Method =:= logging_formatters_set ->
    invalid; %XXX
convert_json_to_term(_Request, Method)
    when Method =:= logging_redirect_set ->
    invalid; %XXX
convert_json_to_term(_Request, Method)
    when Method =:= code_path_add ->
    invalid; %XXX
convert_json_to_term(_Request, Method)
    when Method =:= code_path_remove ->
    invalid. %XXX

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
                    convert_term_to_json_options(Status)]
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
convert_term_to_json({ok, Options}, Method, Space)
    when Method =:= acl;
         Method =:= nodes_get;
         Method =:= logging ->
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
                {<<"max_t">>, MaxT},
                {<<"options">>, convert_term_to_json_options(Options)}],
    Service1 = if
        DestListAllow =:= undefined ->
            Service0;
        is_list(DestListAllow) ->
            [{<<"dest_list_allow">>,
              [erlang:list_to_binary(DestAllow)
               || DestAllow <- DestListAllow]} | Service0]
    end,
    Service2 = if
        DestListDeny =:= undefined ->
            Service1;
        is_list(DestListDeny) ->
            [{<<"dest_list_deny">>,
              [erlang:list_to_binary(DestDeny)
               || DestDeny <- DestListDeny]} | Service1]
    end,
    Service3 = [{<<"timeout_init">>, TimeoutInit},
                {<<"timeout_async">>, TimeoutAsync},
                {<<"timeout_sync">>, TimeoutSync} | Service2],
    ServiceN = if
        DestRefresh =:= none ->
            Service3;
        is_atom(DestRefresh) ->
            [{<<"dest_refresh">>,
              erlang:atom_to_binary(DestRefresh, utf8)} | Service3]
    end,
    [{<<"id">>, erlang:list_to_binary(Id)},
     {<<"prefix">>, erlang:list_to_binary(Prefix)},
     {<<"module">>, erlang:atom_to_binary(Module, utf8)},
     {<<"args">>, convert_term_to_json_options(Args)} | ServiceN];
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
                {<<"max_t">>, MaxT},
                {<<"options">>, convert_term_to_json_options(Options)}],
    Service1 = if
        DestListAllow =:= undefined ->
            Service0;
        is_list(DestListAllow) ->
            [{<<"dest_list_allow">>,
              [erlang:list_to_binary(DestAllow)
               || DestAllow <- DestListAllow]} | Service0]
    end,
    Service2 = if
        DestListDeny =:= undefined ->
            Service1;
        is_list(DestListDeny) ->
            [{<<"dest_list_deny">>,
              [erlang:list_to_binary(DestDeny)
               || DestDeny <- DestListDeny]} | Service1]
    end,
    Service3 = [{<<"protocol">>, erlang:atom_to_binary(Protocol, utf8)},
                {<<"buffer_size">>, BufferSize},
                {<<"timeout_init">>, TimeoutInit},
                {<<"timeout_async">>, TimeoutAsync},
                {<<"timeout_sync">>, TimeoutSync} | Service2],
    ServiceN = if
        DestRefresh =:= none ->
            Service3;
        is_atom(DestRefresh) ->
            [{<<"dest_refresh">>,
              erlang:atom_to_binary(DestRefresh, utf8)} | Service3]
    end,
    [{<<"id">>, erlang:list_to_binary(Id)},
     {<<"prefix">>, erlang:list_to_binary(Prefix)},
     {<<"file_path">>, erlang:list_to_binary(FilePath)},
     {<<"args">>, erlang:list_to_binary(Args)},
     {<<"env">>,
      [erlang:list_to_binary(Key ++ "=" ++ Value)
       || {Key, Value} <- Env]} | ServiceN].

convert_term_to_json_option([{Key, _} | _] = Value)
    when is_atom(Key) ->
    convert_term_to_json_options(Value);
convert_term_to_json_option([H | _] = Value)
    when is_integer(H), H > 0 ->
    erlang:list_to_binary(Value);
convert_term_to_json_option([[H | _] | _] = Value)
    when is_integer(H), H > 0 ->
    convert_term_to_json_strings(Value);
convert_term_to_json_option([A | _] = Value)
    when is_atom(A) ->
    convert_term_to_json_atoms(Value);
convert_term_to_json_option([] = Value) ->
    Value;
convert_term_to_json_option([_ | _] = Value) ->
    cloudi_string:term_to_binary_compact(Value);
convert_term_to_json_option(Value)
    when is_number(Value) ->
    Value;
convert_term_to_json_option(Value)
    when is_atom(Value) ->
    erlang:atom_to_binary(Value, utf8);
convert_term_to_json_option(Value)
    when is_tuple(Value); is_map(Value) ->
    cloudi_string:term_to_binary_compact(Value).

convert_term_to_json_options(Options) ->
    [{erlang:atom_to_binary(Key, utf8),
      convert_term_to_json_option(Value)} || {Key, Value} <- Options].
         
convert_term_to_json_strings(L) ->
    [erlang:list_to_binary(S) || S <- L].

convert_term_to_json_atoms(L) ->
    [erlang:atom_to_binary(A, utf8) || A <- L].

json_encode(Term, true) ->
    cloudi_x_jsx:encode(Term, [{indent, 1}]);
json_encode(Term, false) ->
    cloudi_x_jsx:encode(Term).

json_decode(Binary) ->
    cloudi_x_jsx:decode(Binary).

