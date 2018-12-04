%% > Parse
%% < ParseComplete
%% > Describe
%% < ParameterDescription
%% < RowDescription | NoData
-module(epgsql_cmd_parse).
-behaviour(epgsql_command).
-export([init/1, execute/2, handle_message/4]).
-export_type([response/0]).

-include("epgsql.hrl").
-include("epgsql_protocol.hrl").

-type response() :: {ok, #statement{}} | {error, epgsql:query_error()}.

-record(parse,
        {name :: iodata(),
         sql :: iodata(),
         types :: [atom()],
         parameter_typenames = [] :: [epgsql:type_name() | {array, epgsql:type_name()}],
         parameter_descr = [] :: [epgsql_oid_db:oid_info()]}).

%% FIXME: make it use oids instead of type names!
init({Name, Sql, Types}) ->
    #parse{name = Name, sql = Sql, types = Types}.

execute(Sock, #parse{name = Name, sql = Sql, types = Types} = St) ->
    Codec = epgsql_sock:get_codec(Sock),
    Bin = epgsql_wire:encode_types(Types, Codec),
    epgsql_sock:send_multi(
      Sock,
      [
       {?PARSE, [Name, 0, Sql, 0, Bin]},
       {?DESCRIBE, [?PREPARED_STATEMENT, Name, 0]},
       {?FLUSH, []}
      ]),
    {ok, Sock, St}.

handle_message(?PARSE_COMPLETE, <<>>, Sock, _State) ->
    {noaction, Sock};
handle_message(?PARAMETER_DESCRIPTION, Bin, Sock, State) ->
    Codec = epgsql_sock:get_codec(Sock),
    TypeInfos = epgsql_wire:decode_parameters(Bin, Codec),
    OidInfos = [epgsql_binary:typeinfo_to_oid_info(Type, Codec) || Type <- TypeInfos],
    TypeNames = [epgsql_binary:typeinfo_to_name_array(Type, Codec) || Type <- TypeInfos],
    Sock2 = epgsql_sock:notify(Sock, {types, TypeNames}),
    {noaction, Sock2, State#parse{parameter_descr = OidInfos,
                                  parameter_typenames = TypeNames}};
handle_message(?ROW_DESCRIPTION, <<Count:?int16, Bin/binary>>, Sock,
               #parse{name = Name, parameter_descr = Params,
                      parameter_typenames = TypeNames}) ->
    Codec = epgsql_sock:get_codec(Sock),
    Columns = epgsql_wire:decode_columns(Count, Bin, Codec),
    Columns2 = [Col#column{format = epgsql_wire:format(Col, Codec)}
                || Col <- Columns],
    Result = {ok, #statement{name = Name,
                             types = TypeNames,
                             columns = Columns2,
                             parameter_info = Params}},
    {finish, Result, {columns, Columns2}, Sock};
handle_message(?NO_DATA, <<>>, Sock, #parse{name = Name, parameter_descr = Params,
                                            parameter_typenames = TypeNames}) ->
    Result = {ok, #statement{name = Name,
                             types = TypeNames,
                             parameter_info = Params,
                             columns = []}},
    {finish, Result, no_data, Sock};
handle_message(?ERROR, Error, _Sock, _State) ->
    Result = {error, Error},
    {sync_required, Result};
handle_message(_, _, _, _) ->
    unknown.
