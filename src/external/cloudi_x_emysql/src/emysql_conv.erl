%%% @doc conversion routines between Emysql Data types and common formats
%%% @end
%%% @private
-module(emysql_conv).

-include("emysql.hrl").

%% Conversion routines
-export([
         as_dict/1,
         as_json/1,
         as_proplist/1,
         as_record/3,
         as_record/4
]).

%% @see emysql:as_dict/1
as_dict(Res = #result_packet{}) ->
    dict:from_list(lists:flatten(as_proplist(Res))).

%% @see emysql:as_proplist/1
as_proplist(#result_packet{field_list=_Cols,rows=_Rows}) when _Cols =:= undefined,
                                                              _Rows =:= undefined ->
    [];
as_proplist(#result_packet{field_list=_Cols,rows=_Rows}) when is_list(_Cols),
                                                              _Rows =:= undefined ->
    [];
as_proplist(#result_packet{field_list=_Cols,rows=_Rows}) when is_list(_Cols),
                                                              _Rows =:= [] ->
    [];
as_proplist(Res = #result_packet{field_list=Cols,rows=Rows}) when is_list(Cols),
                                                                  is_list(Rows) ->
    Fields = emysql:field_names(Res),
    [begin
        [{K, V} || {K, V} <- lists:zip(Fields, R)]
    end || R <- Rows].

%% @see emysql:as_record/1
as_record(Result = #result_packet{}, RecordName, Fields, Fun) when is_atom(RecordName), is_list(Fields), is_function(Fun) ->
    Columns = Result#result_packet.field_list,

    S = lists:seq(1, length(Columns)),
    P = lists:zip([ binary_to_atom(C1#field.name, utf8) || C1 <- Columns ], S),
    F = fun(FieldName) ->
        case proplists:lookup(FieldName, P) of
            none ->
                    fun(_) -> undefined end;
            {FieldName, Pos} ->
                    fun(Row) -> lists:nth(Pos, Row) end
        end
    end,
    Fs = [ F(FieldName) || FieldName <- Fields ],
    F1 = fun(Row) ->
        RecordData = [ Fx(Row) || Fx <- Fs ],
        Fun(list_to_tuple([RecordName|RecordData]))
    end,
    [ F1(Row) || Row <- Result#result_packet.rows ].

as_record(Result = #result_packet{}, RecordName, Fields) when is_atom(RecordName), is_list(Fields) ->
    as_record(Result, RecordName, Fields, fun(A) -> A end).

%% @see emysql:as_json/1
as_json(#result_packet { rows = Rows } = Result) ->
    Fields = emysql:field_names(Result),
    [begin
        [{K, json_val(V)} || {K, V} <- lists:zip(Fields, Row)]
    end || Row <- Rows].

json_val(undefined) ->
    null;
json_val({date,{Year,Month,Day}}) ->
    iolist_to_binary(
        io_lib:format("~4.4.0w-~2.2.0w-~2.2.0w",
                      [Year, Month, Day]));
json_val({datetime,{ {Year,Month,Day}, {Hour,Min,Sec} }}) ->
    iolist_to_binary(
        io_lib:format("~4.4.0w-~2.2.0w-~2.2.0wT~2.2.0w:~2.2.0w:~2.2.0wZ",
                      [Year, Month, Day, Hour, Min, Sec]));
json_val(Value) ->
    Value.
