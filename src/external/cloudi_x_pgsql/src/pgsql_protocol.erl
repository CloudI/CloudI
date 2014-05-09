%%% @doc Module for packet encoding and decoding.
%%%
-module(pgsql_protocol).
-vsn("3").
% -include_lib("commonlib/include/compile_time.hrl").
-include("pgsql_internal.hrl").

-export([
    encode_startup_message/1,
    encode_ssl_request_message/0,
    encode_password_message/1,
    encode_query_message/1,
    encode_parse_message/3,
    encode_bind_message/4,  % deprecated
    encode_bind_message/5,
    encode_describe_message/2,
    encode_execute_message/2,
    encode_sync_message/0,
    encode_flush_message/0,
    encode_cancel_message/2,
    
    decode_message/2,
    decode_row/4,
    
    bind_requires_statement_description/1
    ]).

%%====================================================================
%% Constants
%%====================================================================
-define(PROTOCOL_VERSION_MAJOR, 3).
-define(PROTOCOL_VERSION_MINOR, 0).

-define(POSTGRESQL_GD_EPOCH, 730485). % ?_value(calendar:date_to_gregorian_days({2000,1,1}))).
-define(POSTGRESQL_GS_EPOCH, 63113904000). % ?_value(calendar:datetime_to_gregorian_seconds({{2000,1,1}, {0,0,0}}))).

%%====================================================================
%% Public API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Encode the startup message.
%%
-spec encode_startup_message([{iodata(), iodata()}]) -> binary().
encode_startup_message(Parameters) ->
    EncodedParams = [[iolist_to_binary(Key), 0, iolist_to_binary(Value), 0] || {Key, Value} <- Parameters],
    Packet = list_to_binary([<<?PROTOCOL_VERSION_MAJOR:16/integer, ?PROTOCOL_VERSION_MINOR:16/integer>>, EncodedParams, 0]),
    Size = byte_size(Packet) + 4,
    <<Size:32/integer, Packet/binary>>.

%%--------------------------------------------------------------------
%% @doc Encode the ssl request message.
%%
-spec encode_ssl_request_message() -> binary().
encode_ssl_request_message() ->
    <<8:32/integer, 1234:16/integer, 5679:16/integer>>.

%%--------------------------------------------------------------------
%% @doc Encode a password.
%%
-spec encode_password_message(iodata()) -> binary().
encode_password_message(Password) ->
    encode_string_message($p, Password).

%%--------------------------------------------------------------------
%% @doc Encode a query.
%%
-spec encode_query_message(iodata()) -> binary().
encode_query_message(Query) ->
    encode_string_message($Q, Query).

%%--------------------------------------------------------------------
%% @doc Encode a parse message.
%%
-spec encode_parse_message(iodata(), iodata(), [pgsql_oid()]) -> binary().
encode_parse_message(PreparedStatementName, Query, DataTypes) ->
    PreparedStatementNameBin = iolist_to_binary(PreparedStatementName),
    QueryBin = iolist_to_binary(Query),
    DataTypesBin = list_to_binary([<<DataTypeOid:32/integer>> || DataTypeOid <- DataTypes]),
    DataTypesCount = length(DataTypes),
    Packet = <<PreparedStatementNameBin/binary, 0, QueryBin/binary, 0, DataTypesCount:16/integer, DataTypesBin/binary>>,
    PacketLen = byte_size(Packet) + 4,
    <<$P, PacketLen:32/integer, Packet/binary>>.

%%--------------------------------------------------------------------
%% @doc Encode a bind message.
%%
-spec encode_bind_message(iodata(), iodata(), [any()], boolean()) -> binary().
encode_bind_message(PortalName, StatementName, Parameters, IntegerDateTimes) ->
    encode_bind_message(PortalName, StatementName, Parameters, [], IntegerDateTimes).

-spec encode_bind_message(iodata(), iodata(), [any()], [pgsql_oid()], boolean()) -> binary().
encode_bind_message(PortalName, StatementName, Parameters, ParametersDataTypes, IntegerDateTimes) ->
    PortalNameBin = iolist_to_binary(PortalName),
    StatementNameBin = iolist_to_binary(StatementName),
    ParametersCount = length(Parameters),
    ParametersCountBin = <<ParametersCount:16/integer>>,
    ParametersWithTypes = case ParametersDataTypes of
        [] -> [{Parameter, undefined} || Parameter <- Parameters];
        _ -> lists:zip(Parameters, ParametersDataTypes)
    end,
    ParametersL = [encode_parameter(Parameter, Type, IntegerDateTimes) || {Parameter, Type} <- ParametersWithTypes],
    {ParametersFormats, ParametersValues} = lists:unzip(ParametersL),
    ParametersFormatsAllText = lists:all(fun(Format) -> Format =:= text end, ParametersFormats),
    ParametersFormatsBin = if
        ParametersFormatsAllText -> <<0:16/integer>>;
        true ->
            [ParametersCountBin | [encode_format(Format) || Format <- ParametersFormats]]
    end,
    Results = <<1:16/integer, 1:16/integer>>,   % We want all results in binary format.
    Packet = list_to_binary([PortalNameBin, 0, StatementNameBin, 0, ParametersFormatsBin, ParametersCountBin, ParametersValues, Results]),
    PacketLen = byte_size(Packet) + 4,
    <<$B, PacketLen:32/integer, Packet/binary>>.

encode_format(text) -> <<0:16/integer>>;
encode_format(binary) -> <<1:16/integer>>.

%%--------------------------------------------------------------------
%% @doc Encode a parameter.
%% All parameters are currently encoded in text format except binaries that are
%% encoded as binaries.
%%
-spec encode_parameter(any(), pgsql_oid() | undefined, boolean()) -> {text | binary, binary()}.
encode_parameter({array, List}, Type, IntegerDateTimes) ->
    encode_array(List, Type, IntegerDateTimes);
encode_parameter(Binary, _Type, _IntegerDateTimes) when is_binary(Binary) ->
    % Encode the binary as text if it is a UUID.
    IsUUID = case Binary of
        <<_A:8/binary, $-, _B:4/binary, $-, _C:4/binary, $-, _D:4/binary, $-, _E:12/binary>> ->
            case io_lib:fread("~16u-~16u-~16u-~16u-~16u", binary_to_list(Binary)) of
                {ok,[_AI, _BI, _CI, _DI, _EI],[]} -> true;
                _ -> false
            end;
        _ -> false
    end,
    Type = if
        IsUUID -> text;
        true -> binary
    end,
    Size = byte_size(Binary),
    {Type, <<Size:32/integer, Binary/binary>>};
encode_parameter(String, _Type, _IntegerDateTimes) when is_list(String) ->
    Binary = list_to_binary(String),
    Size = byte_size(Binary),
    {text, <<Size:32/integer, Binary/binary>>};
encode_parameter(Float, _Type, _IntegerDateTimes) when is_float(Float) ->
    FloatStrBin = list_to_binary(float_to_list(Float)),
    Size = byte_size(FloatStrBin),
    {text, <<Size:32/integer, FloatStrBin/binary>>};
encode_parameter(Integer, _Type, _IntegerDateTimes) when is_integer(Integer) ->
    IntegerStr = integer_to_list(Integer),
    IntegerStrBin = list_to_binary(IntegerStr),
    IntegerStrLen = byte_size(IntegerStrBin),
    {text, <<IntegerStrLen:32/integer, IntegerStrBin/binary>>};
encode_parameter(null, _Type, _IntegerDateTimes) ->
    {text, <<-1:32/integer>>};
encode_parameter(true, _Type, _IntegerDateTimes) ->
    {text, <<1:32/integer, $t>>};
encode_parameter(false, _Type, _IntegerDateTimes) ->
    {text, <<1:32/integer, $f>>};
encode_parameter({{Year, Month, Day}, {Hour, Min, Sec}}, Type, IntegerDateTimes) ->
    encode_parameter(lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B", [Year, Month, Day, Hour, Min, Sec])), Type, IntegerDateTimes);
encode_parameter({Hour, Min, Sec}, Type, IntegerDateTimes) when Hour >= 0 andalso Hour < 24 andalso Min >= 0 andalso Min < 60 andalso Sec > 0 andalso Sec =< 60 ->
    encode_parameter(lists:flatten(io_lib:format("~2.10.0B:~2.10.0B:~2.10.0B", [Hour, Min, Sec])), Type, IntegerDateTimes);
encode_parameter({Year, Month, Day}, Type, IntegerDateTimes) when Month > 0 andalso Month =< 12 andalso Day > 0 andalso Day =< 31 ->
    encode_parameter(lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B", [Year, Month, Day])), Type, IntegerDateTimes);
encode_parameter({point, P}, _Type, _IntegerDateTimes) ->
    Binary = encode_point_text(P),
    Size = byte_size(Binary),
    {text, <<Size:32/integer, Binary/binary>>};
encode_parameter({lseg, P1, P2}, _Type, _IntegerDateTimes) ->
    P1Bin = encode_point_text(P1),
    P2Bin = encode_point_text(P2),
    Binary = <<$[, P1Bin/binary, $,, P2Bin/binary, $]>>,
    Size = byte_size(Binary),
    {text, <<Size:32/integer, Binary/binary>>};
encode_parameter({box, P1, P2}, _Type, _IntegerDateTimes) ->
    P1Bin = encode_point_text(P1),
    P2Bin = encode_point_text(P2),
    Binary = <<$(, P1Bin/binary, $,, P2Bin/binary, $)>>,
    Size = byte_size(Binary),
    {text, <<Size:32/integer, Binary/binary>>};
encode_parameter({path, open, [_|_]=PList}, _Type, _IntegerDateTimes) ->
    Binary = encode_points_text($[, $], PList),
    Size = byte_size(Binary),
    {text, <<Size:32/integer, Binary/binary>>};
encode_parameter({path, closed, [_|_]=PList}, _Type, _IntegerDateTimes) ->
    Binary = encode_points_text($(, $), PList),
    Size = byte_size(Binary),
    {text, <<Size:32/integer, Binary/binary>>};
encode_parameter({polygon, [_|_]=PList}, _Type, _IntegerDateTimes) ->
    Binary = encode_points_text($(, $), PList),
    Size = byte_size(Binary),
    {text, <<Size:32/integer, Binary/binary>>};
encode_parameter(Value, _Type, _IntegerDateTimes) ->
    throw({badarg, Value}).

encode_point_text({X, Y}) ->
    XBin = list_to_binary(case is_integer(X) of true -> integer_to_list(X); false -> float_to_list(X) end),
    YBin = list_to_binary(case is_integer(Y) of true -> integer_to_list(Y); false -> float_to_list(Y) end),
    <<$(, XBin/binary, $,, YBin/binary, $)>>.

encode_points_text(Prefix, Suffix, [PHead|PTail]) ->
    PHeadBin = encode_point_text(PHead),
    PTailBin = list_to_binary([begin PBin = encode_point_text(P), <<$,, PBin/binary>> end || P <- PTail]),
    <<Prefix, PHeadBin/binary, PTailBin/binary, Suffix>>.

encode_array(Elements, ArrayType, IntegerDateTimes) ->
    ElementType = array_type_to_element_type(ArrayType),
    {EncodingType, ArrayElements} = encode_array_elements(Elements, ElementType, IntegerDateTimes, undefined, []),
    case EncodingType of
        binary ->
            encode_array_binary(ArrayElements, ElementType);
        undefined when ElementType =/= undefined ->
            encode_array_binary(ArrayElements, ElementType);
        _ ->
            encode_array_text(ArrayElements, [])
    end.

array_type_to_element_type(undefined) -> undefined;
array_type_to_element_type(?BYTEAARRAYOID) -> ?BYTEAOID;
array_type_to_element_type(?TEXTARRAYOID) -> ?TEXTOID;
array_type_to_element_type(_OtherType) -> undefined.

encode_array_elements([{array, SubArray} | Tail], ElementType, IntegerDateTimes, EncodingType, Acc) ->
    {NewEncodingType, SubArrayElements} = encode_array_elements(SubArray, ElementType, IntegerDateTimes, EncodingType, []),
    encode_array_elements(Tail, ElementType, IntegerDateTimes, NewEncodingType, [{array, SubArrayElements} | Acc]);
encode_array_elements([null | Tail], ElementType, IntegerDateTimes, EncodingType, Acc) ->
    encode_array_elements(Tail, ElementType, IntegerDateTimes, EncodingType, [null | Acc]);
encode_array_elements([Element | Tail], ElementType, IntegerDateTimes, undefined, Acc) ->
    {EncodingType, Encoded} = encode_parameter(Element, ElementType, IntegerDateTimes),
    encode_array_elements(Tail, ElementType, IntegerDateTimes, EncodingType, [Encoded | Acc]);
encode_array_elements([Element | Tail], ElementType, IntegerDateTimes, EncodingType, Acc) ->
    {EncodingType, Encoded} = encode_parameter(Element, ElementType, IntegerDateTimes),
    encode_array_elements(Tail, ElementType, IntegerDateTimes, EncodingType, [Encoded | Acc]);
encode_array_elements([], _ElementType, _IntegerDateTimes, EncodingType, Acc) ->
    {EncodingType, lists:reverse(Acc)}.

encode_array_text([null | Tail], Acc) ->
    encode_array_text(Tail, [<<"NULL">> | Acc]);
encode_array_text([<<_TextSize:32/integer, Text/binary>> | Tail], Acc) ->
    Escaped = escape_array_text(Text),
    encode_array_text(Tail, [Escaped | Acc]);
encode_array_text([{array, SubArray} | Tail], Acc) when is_list(SubArray) ->
    SubArrayEncoded = encode_array_text(SubArray, []),
    encode_array_text(Tail, [SubArrayEncoded | Acc]);
encode_array_text([], Acc) ->
    StrList = lists:reverse(Acc),
    JoinedStrings = case StrList of
        [] -> [];
        [StrListHead | StrListTail] -> [StrListHead, [[<<",">>, Str] || Str <- StrListTail]]
    end,
    Binary = list_to_binary([<<"{">>, JoinedStrings, <<"}">>]),
    Size = byte_size(Binary),
    {text, <<Size:32, Binary/binary>>}.
    
escape_array_text(Text) when byte_size(Text) =:= 4 ->
    case string:to_lower(unicode:characters_to_list(Text)) of
        "null" -> <<$", Text/binary, $">>;
        _ -> escape_array_text0(Text, [])
    end;
escape_array_text(Text) -> escape_array_text0(Text, []).

escape_array_text0(Text, Acc) ->
    case binary:match(Text, [<<$">>, <<$,>>, <<$ >>, <<$\\>>]) of
        nomatch when Acc =:= [] -> Text;
        nomatch ->
            list_to_binary([<<$">>, lists:reverse(Acc, [Text]), <<$">>]);
        {Index, 1} ->
            {Prefix, Rest1} = split_binary(Text, Index),
            {ToEscape, Rest2} = split_binary(Rest1, 1),
            NewAcc = [<<Prefix/binary, $\\, ToEscape/binary>> | Acc],
            escape_array_text0(Rest2, NewAcc)
    end.

encode_array_binary(ArrayElements, ElementTypeOID) ->
    {HasNulls, Rows} = encode_array_binary_row(ArrayElements, false, []),
    Dims = get_array_dims(ArrayElements),
    Header = encode_array_binary_header(Dims, HasNulls, ElementTypeOID),
    Encoded = list_to_binary([Header, Rows]),
    Size = byte_size(Encoded),
    {binary, <<Size:32/integer, Encoded/binary>>}.

encode_array_binary_row([null | Tail], _HasNull, Acc) ->
    encode_array_binary_row(Tail, true, [<<-1:32/integer>> | Acc]);
encode_array_binary_row([<<_BinarySize:32/integer, _BinaryVal/binary>> = Binary | Tail], HasNull, Acc) ->
    encode_array_binary_row(Tail, HasNull, [Binary | Acc]);
encode_array_binary_row([{array, Elements} | Tail], HasNull, Acc) ->
    {NewHasNull, Row} = encode_array_binary_row(Elements, HasNull, []),
    encode_array_binary_row(Tail, NewHasNull, [Row | Acc]);
encode_array_binary_row([], HasNull, AccRow) ->
    {HasNull, lists:reverse(AccRow)}.

get_array_dims([{array, SubElements} | _] = Row) ->
    Dims0 = get_array_dims(SubElements),
    Dim = length(Row),
    [Dim | Dims0];
get_array_dims(Row) ->
    Dim = length(Row),
    [Dim].

encode_array_binary_header(Dims, HasNulls, ElementTypeOID) ->
    NDims = length(Dims),
    Flags = if
        HasNulls -> 1;
        true -> 0
    end,
    EncodedDimensions = [<<Dim:32/integer, 1:32/integer>> || Dim <- Dims],
    [<<
        NDims:32/integer,
        Flags:32/integer,
        ElementTypeOID:32/integer
    >>,
    EncodedDimensions].

%%--------------------------------------------------------------------
%% @doc Determine if we need the statement description with these parameters.
%% We currently only require statement descriptions if we have arrays of
%% binaries.
-spec bind_requires_statement_description([any()]) -> boolean().
bind_requires_statement_description([]) -> false;
bind_requires_statement_description([{array, [{array, SubArrayElems} | SubArrayT]} | Tail]) ->
    bind_requires_statement_description([{array, SubArrayElems}, {array, SubArrayT} | Tail]);
bind_requires_statement_description([{array, [ArrayElem | _]} | _]) when is_binary(ArrayElem) -> true;
bind_requires_statement_description([{array, [null | ArrayElemsT]} | Tail]) ->
    bind_requires_statement_description([{array, ArrayElemsT} | Tail]);
bind_requires_statement_description([{array, []} | Tail]) ->
    bind_requires_statement_description(Tail);
bind_requires_statement_description([_OtherParam | Tail]) ->
    bind_requires_statement_description(Tail).

%%--------------------------------------------------------------------
%% @doc Encode a describe message.
%%
-spec encode_describe_message(portal | statement, iodata()) -> binary().
encode_describe_message(PortalOrStatement, Name) ->
    NameBin = iolist_to_binary(Name),
    MessageLen = byte_size(NameBin) + 6,
    WhatByte = case PortalOrStatement of
        portal -> $P;
        statement -> $S
    end,
    <<$D, MessageLen:32/integer, WhatByte, NameBin/binary, 0>>.

%%--------------------------------------------------------------------
%% @doc Encode an execute message.
%%
-spec encode_execute_message(iodata(), non_neg_integer()) -> binary().
encode_execute_message(PortalName, MaxRows) ->
    PortalNameBin = iolist_to_binary(PortalName),
    MessageLen = byte_size(PortalNameBin) + 9,
    <<$E, MessageLen:32/integer, PortalNameBin/binary, 0, MaxRows:32/integer>>.

%%--------------------------------------------------------------------
%% @doc Encode a sync message.
%%
-spec encode_sync_message() -> binary().
encode_sync_message() ->
    <<$S, 4:32/integer>>.

%%--------------------------------------------------------------------
%% @doc Encode a flush message.
%%
-spec encode_flush_message() -> binary().
encode_flush_message() ->
    <<$H, 4:32/integer>>.

%%--------------------------------------------------------------------
%% @doc Encode a flush message.
%%
-spec encode_cancel_message(integer(), integer()) -> binary().
encode_cancel_message(ProcID, Secret) ->
    <<16:32/integer, 80877102:32/integer, ProcID:32/integer, Secret:32/integer>>.

%%--------------------------------------------------------------------
%% @doc Encode a string message.
%%
-spec encode_string_message(byte(), iodata()) -> binary().
encode_string_message(Identifier, String) ->    
    StringBin = iolist_to_binary(String),
    MessageLen = byte_size(StringBin) + 5,
    <<Identifier, MessageLen:32/integer, StringBin/binary, 0>>.

%%--------------------------------------------------------------------
%% @doc Decode a message.
%%
-spec decode_message(byte(), binary()) -> {ok, pgsql_backend_message()} | {error, any()}.
decode_message($R, Payload) -> decode_authentication_message(Payload);
decode_message($K, Payload) -> decode_backend_key_data_message(Payload);
decode_message($2, Payload) -> decode_bind_complete_message(Payload);
decode_message($3, Payload) -> decode_close_complete_message(Payload);
decode_message($C, Payload) -> decode_command_complete_message(Payload);
decode_message($d, Payload) -> decode_copy_data_message(Payload);
decode_message($c, Payload) -> decode_copy_done_message(Payload);
decode_message($G, Payload) -> decode_copy_in_response_message(Payload);
decode_message($H, Payload) -> decode_copy_out_response_message(Payload);
decode_message($W, Payload) -> decode_copy_both_response_message(Payload);
decode_message($D, Payload) -> decode_data_row_message(Payload);
decode_message($I, Payload) -> decode_empty_query_response_message(Payload);
decode_message($E, Payload) -> decode_error_response_message(Payload);
decode_message($V, Payload) -> decode_function_call_response_message(Payload);
decode_message($n, Payload) -> decode_no_data_message(Payload);
decode_message($N, Payload) -> decode_notice_response_message(Payload);
decode_message($A, Payload) -> decode_notification_response_message(Payload);
decode_message($t, Payload) -> decode_parameter_description_message(Payload);
decode_message($S, Payload) -> decode_parameter_status_message(Payload);
decode_message($1, Payload) -> decode_parse_complete_message(Payload);
decode_message($s, Payload) -> decode_portal_suspended_message(Payload);
decode_message($Z, Payload) -> decode_ready_for_query_message(Payload);
decode_message($T, Payload) -> decode_row_description_message(Payload);
decode_message(Other, _) ->
    {error, {unknown_message_type, Other}}.

decode_authentication_message(<<0:32/integer>>) ->
    {ok, #authentication_ok{}};
decode_authentication_message(<<2:32/integer>>) ->
    {ok, #authentication_kerberos_v5{}};
decode_authentication_message(<<3:32/integer>>) ->
    {ok, #authentication_cleartext_password{}};
decode_authentication_message(<<5:32/integer, Salt:4/binary>>) ->
    {ok, #authentication_md5_password{salt = Salt}};
decode_authentication_message(<<6:32/integer>>) ->
    {ok, #authentication_scm_credential{}};
decode_authentication_message(<<7:32/integer>>) ->
    {ok, #authentication_gss{}};
decode_authentication_message(<<9:32/integer>>) ->
    {ok, #authentication_sspi{}};
decode_authentication_message(<<8:32/integer, Rest/binary>>) ->
    {ok, #authentication_gss_continue{data = Rest}};
decode_authentication_message(Payload) ->
    {error, {unknown_message, authentication, Payload}}.

decode_backend_key_data_message(<<ProcID:32/integer, Secret:32/integer>>) ->
    {ok, #backend_key_data{procid = ProcID, secret = Secret}};
decode_backend_key_data_message(Payload) ->
    {error, {unknown_message, backend_key_data, Payload}}.

decode_bind_complete_message(<<>>) -> {ok, #bind_complete{}};
decode_bind_complete_message(Payload) ->
    {error, {unknown_message, bind_complete, Payload}}.

decode_close_complete_message(<<>>) -> {ok, #close_complete{}};
decode_close_complete_message(Payload) ->
    {error, {unknown_message, close_complete, Payload}}.

decode_command_complete_message(Payload) ->
    case decode_string(Payload) of
        {ok, String, <<>>} -> {ok, #command_complete{command_tag = String}};
        _ -> {error, {unknown_message, command_complete, Payload}}
    end.

decode_copy_data_message(Payload) -> {ok, #copy_data{data = Payload}}.

decode_copy_done_message(<<>>) -> {ok, #copy_done{}};
decode_copy_done_message(Payload) ->
    {error, {unknown_message, copy_done, Payload}}.

decode_copy_in_response_message(Payload) ->
    case decode_copy_response_message(Payload) of
        {ok, {OverallFormat, N, ColumnFormats}} -> {ok, #copy_in_response{format = OverallFormat, columns = N, column_formats = ColumnFormats}};
        {error, _} -> {error, {unknow_message, copy_in_response, Payload}}
    end.

decode_copy_out_response_message(Payload) ->
    case decode_copy_response_message(Payload) of
        {ok, {OverallFormat, N, ColumnFormats}} -> {ok, #copy_out_response{format = OverallFormat, columns = N, column_formats = ColumnFormats}};
        {error, _} -> {error, {unknow_message, copy_out_response, Payload}}
    end.

decode_copy_both_response_message(Payload) ->
    case decode_copy_response_message(Payload) of
        {ok, {OverallFormat, N, ColumnFormats}} -> {ok, #copy_both_response{format = OverallFormat, columns = N, column_formats = ColumnFormats}};
        {error, _} -> {error, {unknow_message, copy_both_response, Payload}}
    end.

decode_data_row_message(<<N:16/integer, Rest/binary>> = Payload) ->
    case decode_data_row_values(N, Rest) of
        {ok, Values} -> {ok, #data_row{values = Values}};
        {error, _} ->
            {error, {unknow_message, data_row, Payload}}
    end;
decode_data_row_message(Payload) ->
    {error, {unknow_message, data_row, Payload}}.

decode_data_row_values(Columns, Binary) ->
    decode_data_row_values0(Binary, Columns, []).

decode_data_row_values0(<<>>, 0, Acc) -> {ok, lists:reverse(Acc)};
decode_data_row_values0(<<-1:32/signed-integer, Rest/binary>>, N, Acc) when N > 0 ->
    decode_data_row_values0(Rest, N - 1, [null | Acc]);
decode_data_row_values0(<<ValueLen:32/integer, ValueBin:ValueLen/binary, Rest/binary>>, N, Acc) when N > 0 ->
    decode_data_row_values0(Rest, N - 1, [ValueBin | Acc]);
decode_data_row_values0(<<_/binary>>, _N, _Acc) -> {error, invalid_value_len}.
        
decode_empty_query_response_message(<<>>) -> {ok, #empty_query_response{}};
decode_empty_query_response_message(Payload) ->
    {error, {unknown_message, empty_query_response, Payload}}.

decode_error_response_message(Payload) ->
    case decode_error_and_notice_message_fields(Payload) of
        {ok, Fields} -> {ok, #error_response{fields = Fields}};
        {error, _} -> {error, {unknown_message, error_response, Payload}}
    end.

decode_function_call_response_message(<<-1:32/signed-integer>>) -> {ok, #function_call_response{value = null}};
decode_function_call_response_message(<<Len:32/integer, Value:Len/binary>>) -> {ok, #function_call_response{value = Value}};
decode_function_call_response_message(Payload) ->
    {error, {unknown_message, function_call_response, Payload}}.

decode_no_data_message(<<>>) -> {ok, #no_data{}};
decode_no_data_message(Payload) ->
    {error, {unknown_message, no_data, Payload}}.

decode_notice_response_message(Payload) ->
    case decode_error_and_notice_message_fields(Payload) of
        {ok, Fields} -> {ok, #notice_response{fields = Fields}};
        {error, _} -> {error, {unknown_message, notice_response, Payload}}
    end.

decode_notification_response_message(<<ProcID:32/integer, Rest0/binary>> = Payload) ->
    case decode_string(Rest0) of
        {ok, Channel, Rest1} ->
            case decode_string(Rest1) of
                {ok, PayloadStr, <<>>} -> {ok, #notification_response{procid = ProcID, channel = Channel, payload = PayloadStr}};
                {error, _} -> {error, {unknown_message, notification_response, Payload}}
            end;
        {error, _} -> {error, {unknown_message, notification_response, Payload}}
    end;
decode_notification_response_message(Payload) ->
    {error, {unknown_message, notification_response, Payload}}.

decode_parameter_description_message(<<Count:16/integer, Rest/binary>> = Payload) ->
    ParameterDataTypes = decode_parameter_data_types(Rest),
    if
        Count =:= length(ParameterDataTypes) ->
            {ok, #parameter_description{count = Count, data_types = ParameterDataTypes}};
        true ->
            {error, {unknown_message, parameter_description, Payload}}
    end;
decode_parameter_description_message(Payload) ->
    {error, {unknown_message, parameter_description, Payload}}.

decode_parameter_status_message(Payload) ->
    case decode_string(Payload) of
        {ok, Name, Rest0} ->
            case decode_string(Rest0) of
                {ok, Value, <<>>} -> {ok, #parameter_status{name = Name, value = Value}};
                {error, _} -> {error, {unknown_message, parameter_status, Payload}}
            end;
        {error, _} -> {error, {unknown_message, parameter_status, Payload}}
    end.

decode_parse_complete_message(<<>>) -> {ok, #parse_complete{}};
decode_parse_complete_message(Payload) ->
    {error, {unknown_message, parse_complete, Payload}}.

decode_portal_suspended_message(<<>>) -> {ok, #portal_suspended{}};
decode_portal_suspended_message(Payload) ->
    {error, {unknown_message, portal_suspended, Payload}}.

decode_ready_for_query_message(<<$I>>) -> {ok, #ready_for_query{transaction_status = idle}};
decode_ready_for_query_message(<<$T>>) -> {ok, #ready_for_query{transaction_status = transaction}};
decode_ready_for_query_message(<<$E>>) -> {ok, #ready_for_query{transaction_status = error}};
decode_ready_for_query_message(Payload) ->
    {error, {unknown_message, ready_for_query, Payload}}.

decode_row_description_message(<<Count:16/integer, Rest/binary>> = Payload) when Count >= 0 ->
    case decode_row_description_message0(Count, Rest, []) of
        {ok, Fields} -> {ok, #row_description{count = Count, fields = Fields}};
        {error, _} ->
            {error, {unknown_message, row_description, Payload}}
    end;
decode_row_description_message(Payload) ->
    {error, {unknown_message, row_description, Payload}}.

decode_row_description_message0(0, <<>>, Acc) -> {ok, lists:reverse(Acc)};
decode_row_description_message0(Count, Binary, Acc) ->
    case decode_string(Binary) of
        {ok, FieldName, <<TableOid:32/integer, AttrNum:16/integer, DataTypeOid:32/integer, DataTypeSize:16/integer, TypeModifier:32/integer, FormatCode:16/integer, Tail/binary>>} ->
            case decode_format_code(FormatCode) of
                {ok, Format} ->
                    Field = #row_description_field{
                        name = FieldName,
                        table_oid = TableOid,
                        attr_number = AttrNum,
                        data_type_oid = DataTypeOid,
                        data_type_size = DataTypeSize,
                        type_modifier = TypeModifier,
                        format = Format},
                    decode_row_description_message0(Count - 1, Tail, [Field | Acc]);
                {error, _} = Error -> Error
            end;
        {error, _} = Error -> Error;
        _ -> {error, unknown_message}
    end.

%%% Helper functions.

decode_copy_response_message(<<Format:8/integer, N:16/integer, Rest/binary>>) when Format =:= 0 orelse Format =:= 1 ->
    {ok, OverallFormat} = decode_format_code(Format),
    if
        byte_size(Rest) =:= N * 2 ->
            case decode_format_codes(Rest) of
                {ok, ColumnFormats} ->
                    {ok, {OverallFormat, N, ColumnFormats}};
                {error, _} -> {error, column_formats}
            end;
        true ->
            {error, column_formats_size}
    end;
decode_copy_response_message(Payload) ->
    {error, {unknown_message, copy_response, Payload}}.

decode_error_and_notice_message_fields(Binary) ->
    decode_error_and_notice_message_fields0(Binary, []).

decode_error_and_notice_message_fields0(<<0>>, Acc) -> {ok, lists:reverse(Acc)};
decode_error_and_notice_message_fields0(<<FieldType, Rest0/binary>>, Acc) ->
    case decode_string(Rest0) of
        {ok, FieldString, Rest1} ->
            FieldTypeSym = decode_error_and_mention_field_type(FieldType),
            Field = {FieldTypeSym, FieldString},
            NewAcc = [Field | Acc],
            decode_error_and_notice_message_fields0(Rest1, NewAcc);
        {error, _} = Error -> Error
    end;
decode_error_and_notice_message_fields0(Bin, _Acc) -> {error, {badarg, Bin}}.

-spec decode_error_and_mention_field_type(byte()) -> pgsql_error:pgsql_error_and_mention_field_type().
decode_error_and_mention_field_type($S) -> severity;
decode_error_and_mention_field_type($C) -> code;
decode_error_and_mention_field_type($M) -> message;
decode_error_and_mention_field_type($D) -> detail;
decode_error_and_mention_field_type($H) -> hint;
decode_error_and_mention_field_type($P) -> position;
decode_error_and_mention_field_type($p) -> internal_position;
decode_error_and_mention_field_type($q) -> internal_query;
decode_error_and_mention_field_type($W) -> where;
decode_error_and_mention_field_type($F) -> file;
decode_error_and_mention_field_type($L) -> line;
decode_error_and_mention_field_type($R) -> routine;
decode_error_and_mention_field_type(Other) -> {unknown, Other}.

decode_parameter_data_types(Binary) ->
    decode_parameter_data_types0(Binary, []).

decode_parameter_data_types0(<<>>, Acc) -> lists:reverse(Acc);
decode_parameter_data_types0(<<Oid:32/integer, Tail/binary>>, Acc) ->
    decode_parameter_data_types0(Tail, [Oid | Acc]).

-spec decode_format_code(integer()) -> {ok, pgsql_format()} | {error, any()}.
decode_format_code(0) -> {ok, text};
decode_format_code(1) -> {ok, binary};
decode_format_code(_Other) -> {error, unknown_format_code}.

-spec decode_format_codes(binary()) -> {ok, [pgsql_format()]} | {error, any()}.
decode_format_codes(Binary) ->
    decode_format_codes0(Binary, []).

decode_format_codes0(<<FormatCode:16/integer, Tail/binary>>, Acc) ->
    case decode_format_code(FormatCode) of
        {ok, Format} ->
            decode_format_codes0(Tail, [Format | Acc]);
        {error, _} = Error -> Error
    end;
decode_format_codes0(<<>>, Acc) -> {ok, lists:reverse(Acc)}.

-spec decode_string(binary()) -> {ok, binary(), binary()} | {error, not_null_terminated}.
decode_string(Binary) ->
    case binary:match(Binary, <<0>>) of
        nomatch -> {error, not_null_terminated};
        {Position, 1} ->
            {String, <<0, Rest/binary>>} = split_binary(Binary, Position),
            {ok, String, Rest}
    end.

%%--------------------------------------------------------------------
%% @doc Decode a row format.
%%
-spec decode_row([#row_description_field{}], [binary()], gb_trees:tree(pos_integer(), atom()), boolean()) -> tuple().
decode_row(Descs, Values, OIDMap, IntegerDateTimes) ->
    decode_row0(Descs, Values, OIDMap, IntegerDateTimes, []).

decode_row0([Desc | DescsT], [Value | ValuesT], OIDMap, IntegerDateTimes, Acc) ->
    DecodedValue = decode_value(Desc, Value, OIDMap, IntegerDateTimes),
    decode_row0(DescsT, ValuesT, OIDMap, IntegerDateTimes, [DecodedValue | Acc]);
decode_row0([], [], _OIDMap, _IntegerDateTimes, Acc) ->
    list_to_tuple(lists:reverse(Acc)).

decode_value(_Desc, null, _OIDMap, _IntegerDateTimes) -> null;
decode_value(#row_description_field{data_type_oid = TypeOID, format = text}, Value, OIDMap, _IntegerDateTimes) ->
    decode_value_text(TypeOID, Value, OIDMap);
decode_value(#row_description_field{data_type_oid = DataTypeOID, format = binary}, Value, OIDMap, IntegerDateTimes) ->
    decode_value_bin(DataTypeOID, Value, OIDMap, IntegerDateTimes).

decode_value_text(TypeOID, Value, _OIDMap) when TypeOID =:= ?INT8OID orelse TypeOID =:= ?INT2OID orelse TypeOID =:= ?INT4OID orelse TypeOID =:= ?OIDOID ->
    list_to_integer(binary_to_list(Value));
decode_value_text(TypeOID, Value, _OIDMap) when TypeOID =:= ?FLOAT4OID orelse TypeOID =:= ?FLOAT8OID orelse TypeOID =:= ?NUMERICOID ->
    case Value of
        <<"NaN">> -> 'NaN';
        <<"Infinity">> -> 'Infinity';
        <<"-Infinity">> -> '-Infinity';
        _ ->
            FloatStr = binary_to_list(Value),
            case lists:member($., FloatStr) of
                true -> list_to_float(FloatStr);
                false when TypeOID =:= ?NUMERICOID -> list_to_integer(FloatStr);
                false -> list_to_integer(FloatStr) * 1.0
            end
    end;
decode_value_text(?BOOLOID, <<"t">>, _OIDMap) -> true;
decode_value_text(?BOOLOID, <<"f">>, _OIDMap) -> false;
decode_value_text(?BYTEAOID, Value, _OIDMap) ->
    <<"\\x", HexEncoded/binary>> = Value,
    Decoded = decode_hex(HexEncoded),
    list_to_binary(Decoded);
decode_value_text(?DATEOID, Value, _OIDMap) ->
    {ok, [Year, Month, Day], []} = io_lib:fread("~u-~u-~u", binary_to_list(Value)),
    {Year, Month, Day};
decode_value_text(?TIMEOID, Value, _OIDMap) ->
    {ok, [Hour, Min], SecsStr} = io_lib:fread("~u:~u:", binary_to_list(Value)),
    Secs = case lists:member($., SecsStr) of
        true ->
            list_to_float(SecsStr);
        false ->
            list_to_integer(SecsStr)
    end,
    {Hour, Min, Secs};
decode_value_text(?TIMETZOID, Value, _OIDMap) ->
    {ok, [Hour, Min], SecsStr0} = io_lib:fread("~u:~u:", binary_to_list(Value)),
    SecsStr = case string:tokens(SecsStr0, "+") of
        [SecsStr1, _TZ] -> SecsStr1;
        [SecsStr1] -> SecsStr1
    end,
    Secs = case lists:member($., SecsStr) of
        true ->
            list_to_float(SecsStr);
        false ->
            list_to_integer(SecsStr)
    end,
    {Hour, Min, Secs};
decode_value_text(TypeOID, <<"infinity">>, _OIDMap) when (TypeOID =:= ?TIMESTAMPOID orelse TypeOID =:= ?TIMESTAMPTZOID) -> infinity;
decode_value_text(TypeOID, <<"-infinity">>, _OIDMap) when (TypeOID =:= ?TIMESTAMPOID orelse TypeOID =:= ?TIMESTAMPTZOID) -> '-infinity';
decode_value_text(?TIMESTAMPOID, Value, _OIDMap) ->
    {ok, [Year, Month, Day, Hour, Min], SecsStr} = io_lib:fread("~u-~u-~u ~u:~u:", binary_to_list(Value)),
    Secs = case lists:member($., SecsStr) of
        true ->
            list_to_float(SecsStr);
        false ->
            list_to_integer(SecsStr)
    end,
    {{Year, Month, Day}, {Hour, Min, Secs}};
decode_value_text(?TIMESTAMPTZOID, Value, _OIDMap) ->
    {ok, [Year, Month, Day, Hour, Min], SecsStr0} = io_lib:fread("~u-~u-~u ~u:~u:", binary_to_list(Value)),
    SecsStr = case string:tokens(SecsStr0, "+") of
        [SecsStr1, _TZ] -> SecsStr1;
        [SecsStr1] -> SecsStr1
    end,
    Secs = case lists:member($., SecsStr) of
        true ->
            list_to_float(SecsStr);
        false ->
            list_to_integer(SecsStr)
    end,
    {{Year, Month, Day}, {Hour, Min, Secs}};
decode_value_text(?POINTOID, Value, _OIDMap) ->
    {P, []} = decode_point_text(binary_to_list(Value)),
    {point, P};
decode_value_text(?LSEGOID, Value, _OIDMap) ->
    [$[|P1Str] = binary_to_list(Value),
    {P1, [$,|P2Str]} = decode_point_text(P1Str),
    {P2, "]"} = decode_point_text(P2Str),
    {lseg, P1, P2};
decode_value_text(?BOXOID, Value, _OIDMap) ->
    P1Str = binary_to_list(Value),
    {P1, [$,|P2Str]} = decode_point_text(P1Str),
    {P2, []} = decode_point_text(P2Str),
    {box, P1, P2};
decode_value_text(?PATHOID, <<$[,_/binary>> = Value, _OIDMap) ->
    {Points, []} = decode_points_text($[, $], binary_to_list(Value)),
    {path, open, Points};
decode_value_text(?PATHOID, <<$(,_/binary>> = Value, _OIDMap) ->
    {Points, []} = decode_points_text($(, $), binary_to_list(Value)),
    {path, closed, Points};
decode_value_text(?POLYGONOID, Value, _OIDMap) ->
    {Points, []} = decode_points_text($(, $), binary_to_list(Value)),
    {polygon, Points};
decode_value_text(?VOIDOID, _Value, _OIDMap) -> null;
decode_value_text(TypeOID, Value, _OIDMap) when TypeOID =:= ?TEXTOID
            orelse TypeOID =:= ?UUIDOID
            orelse TypeOID =:= ?NAMEOID
            orelse TypeOID =:= ?VARCHAROID
             -> Value;
decode_value_text(TypeOID, Value, OIDMap) ->
    Type = decode_oid(TypeOID, OIDMap),
    if not is_atom(Type) -> {Type, Value};
        true ->
            case atom_to_list(Type) of
                [$_ | ContentType] -> % Array
                    OIDContentType = type_to_oid(list_to_atom(ContentType), OIDMap),
                    {R, _} = decode_array_text(OIDContentType, OIDMap, Value, []),
                    R;
                _ -> {Type, Value}
            end
    end.

decode_point_text(Str) ->
    {X, AfterX} =
        case io_lib:fread("(~f,", Str) of
            {ok, [X0], AfterX0} -> {X0, AfterX0};
            {error, {fread, float}} ->
                {ok, [X0], AfterX0} = io_lib:fread("(~d,", Str),
                {X0 * 1.0, AfterX0}
        end,
    {Y, AfterY} =
        case io_lib:fread("~f)", AfterX) of
            {ok, [Y0], AfterY0} -> {Y0, AfterY0};
            {error, {fread, float}} ->
                {ok, [Y0], AfterY0} = io_lib:fread("~d)", AfterX),
                {Y0 * 1.0, AfterY0}
        end,
    {{X, Y}, AfterY}.

decode_points_text(Prefix, Suffix, PStr) ->
    decode_points_text_aux(Prefix, Suffix, PStr, []).

decode_points_text_aux(Prefix, Suffix, [Before|PStr], PAcc) when Before =:= $, orelse Before =:= Prefix ->
    {P, AfterP} = decode_point_text(PStr),
    decode_points_text_aux(Prefix, Suffix, AfterP, [P|PAcc]);
decode_points_text_aux(_, Suffix, [Suffix|After], PAcc) ->
    {lists:reverse(PAcc), After}.


type_to_oid(Type, OIDMap) ->
    List = gb_trees:to_list(OIDMap),
    {OIDType, _} = lists:keyfind(Type, 2, List),
    OIDType.


decode_array_text(_Type, _OIDMap, <<>>, [Acc]) ->
    {Acc, <<>>};
decode_array_text(Type, OIDMap, <<"{", Next/binary>>, Acc) ->
    {R, Next2} = decode_array_text(Type, OIDMap, Next, []),
    decode_array_text(Type, OIDMap, Next2, [{array, R} | Acc]);
decode_array_text(_Type, _OIDMap, <<"}", Next/binary>>, Acc) ->
    {lists:reverse(Acc), Next};
decode_array_text(Type, OIDMap, <<",", Next/binary>>, Acc) ->
    decode_array_text(Type, OIDMap, Next, Acc);
decode_array_text(Type, OIDMap, Content, Acc) ->
    {Count, Next} = decode_array_text_find(Content, 0),
    {Value, _} = split_binary(Content, Count),
    Element = decode_value_text(Type, Value, OIDMap),
    decode_array_text(Type, OIDMap, Next, [Element|Acc]).

decode_array_text_find(<<",", Next/binary>>, Count) ->
    {Count, Next};
decode_array_text_find(<<"}", _Next/binary>> = N, Count) ->
    {Count, N};
decode_array_text_find(<<_, Next/binary>>, Count) ->
    decode_array_text_find(Next, Count+1).


decode_value_bin(?BOOLOID, <<0>>, _OIDMap, _IntegerDateTimes) -> false;
decode_value_bin(?BOOLOID, <<1>>, _OIDMap, _IntegerDateTimes) -> true;
decode_value_bin(?BYTEAOID, Value, _OIDMap, _IntegerDateTimes) -> Value;
decode_value_bin(?NAMEOID, Value, _OIDMap, _IntegerDateTimes) -> Value;
decode_value_bin(?INT8OID, <<Value:64/signed-integer>>, _OIDMap, _IntegerDateTimes) -> Value;
decode_value_bin(?INT2OID, <<Value:16/signed-integer>>, _OIDMap, _IntegerDateTimes) -> Value;
decode_value_bin(?INT4OID, <<Value:32/signed-integer>>, _OIDMap, _IntegerDateTimes) -> Value;
decode_value_bin(?OIDOID, <<Value:32/signed-integer>>, _OIDMap, _IntegerDateTimes) -> Value;
decode_value_bin(?TEXTOID, Value, _OIDMap, _IntegerDateTimes) -> Value;
decode_value_bin(?VARCHAROID, Value, _OIDMap, _IntegerDateTimes) -> Value;
decode_value_bin(?FLOAT4OID, <<Value:32/float>>, _OIDMap, _IntegerDateTimes) -> Value;
decode_value_bin(?FLOAT4OID, <<127,192,0,0>>, _OIDMap, _IntegerDateTimes) -> 'NaN';
decode_value_bin(?FLOAT4OID, <<127,128,0,0>>, _OIDMap, _IntegerDateTimes) -> 'Infinity';
decode_value_bin(?FLOAT4OID, <<255,128,0,0>>, _OIDMap, _IntegerDateTimes) -> '-Infinity';
decode_value_bin(?FLOAT8OID, <<Value:64/float>>, _OIDMap, _IntegerDateTimes) -> Value;
decode_value_bin(?FLOAT8OID, <<127,248,0,0,0,0,0,0>>, _OIDMap, _IntegerDateTimes) -> 'NaN';
decode_value_bin(?FLOAT8OID, <<127,240,0,0,0,0,0,0>>, _OIDMap, _IntegerDateTimes) -> 'Infinity';
decode_value_bin(?FLOAT8OID, <<255,240,0,0,0,0,0,0>>, _OIDMap, _IntegerDateTimes) -> '-Infinity';
decode_value_bin(?UUIDOID, Value, _OIDMap, _IntegerDateTimes) ->
    <<UUID_A:32/integer, UUID_B:16/integer, UUID_C:16/integer, UUID_D:16/integer, UUID_E:48/integer>> = Value,
    UUIDStr = io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b", [UUID_A, UUID_B, UUID_C, UUID_D, UUID_E]),
    list_to_binary(UUIDStr);
decode_value_bin(?DATEOID, <<Date:32/signed-integer>>, _OIDMap, true) -> calendar:gregorian_days_to_date(Date + ?POSTGRESQL_GD_EPOCH);
decode_value_bin(?TIMEOID, <<Time:64/signed-integer>>, _OIDMap, true) -> decode_time_int(Time);
decode_value_bin(?TIMETZOID, <<Time:64/signed-integer, _TZ:32/integer>>, _OIDMap, true) -> decode_time_int(Time);
decode_value_bin(?TIMESTAMPOID, <<16#7FFFFFFFFFFFFFFF:64/signed-integer>>, _OIDMap, true) -> infinity;
decode_value_bin(?TIMESTAMPOID, <<-16#8000000000000000:64/signed-integer>>, _OIDMap, true) -> '-infinity';
decode_value_bin(?TIMESTAMPOID, <<Timestamp:64/signed-integer>>, _OIDMap, true) -> decode_timestamp_int(Timestamp);
decode_value_bin(?TIMESTAMPTZOID, <<16#7FFFFFFFFFFFFFFF:64/signed-integer>>, _OIDMap, true) -> infinity;
decode_value_bin(?TIMESTAMPTZOID, <<-16#8000000000000000:64/signed-integer>>, _OIDMap, true) -> '-infinity';
decode_value_bin(?TIMESTAMPTZOID, <<Timestamp:64/signed-integer>>, _OIDMap, true) -> decode_timestamp_int(Timestamp);
decode_value_bin(?NUMERICOID, NumericBin, _OIDMap, _IntegerDateTimes) -> decode_numeric_bin(NumericBin);
decode_value_bin(?POINTOID, <<X:64/float, Y:64/float>>, _OIDMap, _IntegerDateTimes) -> {point, {X, Y}};
decode_value_bin(?LSEGOID, <<P1X:64/float, P1Y:64/float, P2X:64/float, P2Y:64/float>>, _OIDMap, _IntegerDateTimes) -> {lseg, {P1X, P1Y}, {P2X, P2Y}};
decode_value_bin(?BOXOID, <<P1X:64/float, P1Y:64/float, P2X:64/float, P2Y:64/float>>, _OIDMap, _IntegerDateTimes) -> {box, {P1X, P1Y}, {P2X, P2Y}};
decode_value_bin(?PATHOID, <<1:8/unsigned-integer, PointsBin/binary>>, _OIDMap, _IntegerDateTimes) -> {path, closed, decode_points_bin(PointsBin)};
decode_value_bin(?PATHOID, <<0:8/unsigned-integer, PointsBin/binary>>, _OIDMap, _IntegerDateTimes) -> {path, open, decode_points_bin(PointsBin)};
decode_value_bin(?POLYGONOID, Points, _OIDMap, _IntegerDateTimes) -> {polygon, decode_points_bin(Points)};
decode_value_bin(?VOIDOID, <<>>, _OIDMap, _IntegerDateTimes) -> null;
decode_value_bin(TypeOID, Value, OIDMap, IntegerDateTimes) ->
    Type = decode_oid(TypeOID, OIDMap),
    if not is_atom(Type) -> {Type, Value};
        true ->
            case atom_to_list(Type) of
                [$_ | _] -> % Array
                    decode_array_bin(Value, OIDMap, IntegerDateTimes);
                _ -> {Type, Value}
            end
    end.

decode_points_bin(<<N:32/unsigned-integer, Points/binary>>) ->
    decode_points_bin(N, Points, []).

decode_points_bin(0, <<>>, Acc) ->
    lists:reverse(Acc);
decode_points_bin(N, <<PX:64/float, PY:64/float, Tail/binary>>, Acc) when N > 0 ->
    decode_points_bin(N - 1, Tail, [{PX, PY}|Acc]).

decode_array_bin(<<Dimensions:32/signed-integer, _Flags:32/signed-integer, ElementOID:32/signed-integer, Remaining/binary>>, OIDMap, IntegerDateTimes) ->
    {RemainingData, DimsInfo} = lists:foldl(fun(_Pos, {Bin, Acc}) ->
                <<Nbr:32/signed-integer, LBound:32/signed-integer, Next/binary>> = Bin,
                {Next, [{Nbr, LBound} | Acc]}
        end, {Remaining, []}, lists:seq(1, Dimensions)),
    DataList = decode_array_bin_aux(ElementOID, RemainingData, OIDMap, IntegerDateTimes, []),
    Expanded = expand(DataList, DimsInfo),
    Expanded.

expand([], []) ->
    {array, []};
expand([List], []) ->
    List;
expand(List, [{Nbr,_}|NextDim]) ->
    List2 = expand_aux(List, Nbr, Nbr, [], []),
    expand(List2, NextDim).

expand_aux([], 0, _, Current, Acc) ->
    lists:reverse([{array, lists:reverse(Current)} | Acc]);
expand_aux(List, 0, Nbr, Current, Acc) ->
    expand_aux(List, Nbr, Nbr, [], [ {array, lists:reverse(Current)} | Acc]);
expand_aux([E|Next], Level, Nbr, Current, Acc) ->
    expand_aux(Next, Level-1, Nbr, [E | Current], Acc).


decode_array_bin_aux(_ElementOID, <<>>, _OIDMap, _IntegerDateTimes, Acc) ->
    lists:reverse(Acc);
decode_array_bin_aux(ElementOID, <<-1:32/signed-integer, Rest/binary>>, OIDMap, IntegerDateTimes, Acc) ->
    decode_array_bin_aux(ElementOID, Rest, OIDMap, IntegerDateTimes, [null | Acc]);
decode_array_bin_aux(ElementOID, <<Size:32/signed-integer, Next/binary>>, OIDMap, IntegerDateTimes, Acc) ->
    {ValueBin, Rest} = split_binary(Next, Size),
    Value = decode_value_bin(ElementOID, ValueBin, OIDMap, IntegerDateTimes),
    decode_array_bin_aux(ElementOID, Rest, OIDMap, IntegerDateTimes, [Value | Acc]).

decode_numeric_bin(<<0:16/unsigned, _Weight:16, 16#C000:16/unsigned, 0:16/unsigned>>) -> 'NaN';
decode_numeric_bin(<<Len:16/unsigned, Weight:16/signed, Sign:16/unsigned, DScale:16/unsigned, Tail/binary>>) when Sign =:= 16#0000 orelse Sign =:= 16#4000 ->
    Len = byte_size(Tail) div 2,
    {ValueInt, DecShift} = decode_numeric_bin0(Tail, Weight, 0),
    ValueDec = decode_numeric_bin_scale(ValueInt, DecShift),
    SignedDec = case Sign of
        16#0000 -> ValueDec;
        16#4000 -> -ValueDec
    end,
    % Convert to float if there are digits after the decimal point.
    if
        DScale > 0 andalso is_integer(SignedDec) -> SignedDec * 1.0;
        true -> SignedDec
    end.

-define(NBASE, 10000).

decode_numeric_bin0(<<>>, Weight, Acc) -> {Acc, Weight};
decode_numeric_bin0(<<Digit:16, Tail/binary>>, Weight, Acc) when Digit >= 0 andalso Digit < ?NBASE ->
    NewAcc = (Acc * ?NBASE) + Digit,
    decode_numeric_bin0(Tail, Weight - 1, NewAcc).

decode_numeric_bin_scale(Value, -1) -> Value;
decode_numeric_bin_scale(Value, DecShift) when DecShift < 0 ->
    NewValue = Value / ?NBASE,
    decode_numeric_bin_scale(NewValue, DecShift + 1);
decode_numeric_bin_scale(Value, DecShift) when DecShift >= 0 ->
    NewValue = Value * ?NBASE,
    decode_numeric_bin_scale(NewValue, DecShift - 1).

decode_oid(Oid, OIDMap) ->
    case gb_trees:lookup(Oid, OIDMap) of
        {value, OIDName} -> OIDName;
        none -> Oid
    end.

decode_hex(<<X, Y, Tail/binary>>) ->
    [erlang:list_to_integer([X, Y], 16) | decode_hex(Tail)];
decode_hex(<<>>) -> [].

decode_time_int(Time) ->
    Seconds = Time div 1000000,
    USecs = Time rem 1000000,
    {Hour, Min, Secs0} = calendar:seconds_to_time(Seconds),
    case USecs of
        0 -> {Hour, Min, Secs0};
        _ -> {Hour, Min, Secs0 + USecs / 1000000}
    end.

decode_timestamp_int(Timestamp) ->
    TimestampSecs = Timestamp div 1000000,
    USecs = Timestamp rem 1000000,
    {Date, {Hour, Min, Secs0}} = calendar:gregorian_seconds_to_datetime(TimestampSecs + ?POSTGRESQL_GS_EPOCH),
    Time = case USecs of
        0 -> {Hour, Min, Secs0};
        _ -> {Hour, Min, Secs0 + USecs / 1000000}
    end,
    {Date, Time}.
