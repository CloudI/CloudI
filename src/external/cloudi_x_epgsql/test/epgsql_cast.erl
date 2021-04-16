%%% Copyright (C) 2008 - Will Glozer.  All rights reserved.
%%% Copyright (C) 2011 - Anton Lebedevich.  All rights reserved.
%%%
%%% Emulates original epgsql API over epgsqla for original tests

-module(epgsql_cast).

-export([connect/1, connect/2, connect/3, connect/4, close/1]).
-export([get_parameter/2, set_notice_receiver/2, get_cmd_status/1, squery/2, equery/2, equery/3]).
-export([prepared_query/3]).
-export([parse/2, parse/3, parse/4, describe/2, describe/3]).
-export([bind/3, bind/4, execute/2, execute/3, execute/4, execute_batch/2, execute_batch/3]).
-export([close/2, close/3, sync/1]).
-export([receive_result/2, sync_on_error/2]).

-include("epgsql.hrl").

%% -- client interface --

connect(Opts) ->
    Ref = epgsqla:connect(Opts),
    await_connect(Ref, Opts).

connect(Host, Opts) ->
    Ref = epgsqla:connect(Host, Opts),
    await_connect(Ref, Opts).

connect(Host, Username, Opts) ->
    Ref = epgsqla:connect(Host, Username, Opts),
    await_connect(Ref, Opts).

connect(Host, Username, Password, Opts) ->
    Ref = epgsqla:connect(Host, Username, Password, Opts),
    %% TODO connect timeout
    await_connect(Ref, Opts).

await_connect(Ref, Opts0) ->
    Opts = epgsql:to_map(Opts0),
    Timeout = maps:get(timeout, Opts, 5000),
    receive
        {C, Ref, connected} ->
            {ok, C};
        {_C, Ref, Error = {error, _}} ->
            Error
    after Timeout ->
            error(timeout)
    end.

close(C) ->
    epgsqla:close(C).

get_parameter(C, Name) ->
    epgsqla:get_parameter(C, Name).

set_notice_receiver(C, PidOrName) ->
    epgsqla:set_notice_receiver(C, PidOrName).

get_cmd_status(C) ->
    epgsqla:get_cmd_status(C).

squery(C, Sql) ->
    Ref = epgsqla:squery(C, Sql),
    receive_result(C, Ref).

equery(C, Sql) ->
    equery(C, Sql, []).

%% TODO add fast_equery command that doesn't need parsed statement
equery(C, Sql, Parameters) ->
    case parse(C, Sql) of
        {ok, #statement{types = Types} = S} ->
            Typed_Parameters = lists:zip(Types, Parameters),
            Ref = epgsqla:equery(C, S, Typed_Parameters),
            receive_result(C, Ref);
        Error ->
            Error
    end.

prepared_query(C, #statement{types = Types} = Stmt, Parameters) ->
    TypedParameters = lists:zip(Types, Parameters),
    Ref = epgsqla:prepared_query(C, Stmt, TypedParameters),
    receive_result(C, Ref);
prepared_query(C, Name, Parameters) ->
    case describe(C, statement, Name) of
        {ok, S} ->
            prepared_query(C, S, Parameters);
        Error ->
            Error
    end.

%% parse

parse(C, Sql) ->
    parse(C, "", Sql, []).

parse(C, Sql, Types) ->
    parse(C, "", Sql, Types).

parse(C, Name, Sql, Types) ->
    Ref = epgsqla:parse(C, Name, Sql, Types),
    sync_on_error(C, receive_result(C, Ref)).

%% bind

bind(C, Statement, Parameters) ->
    bind(C, Statement, "", Parameters).

bind(C, Statement, PortalName, Parameters) ->
    Ref = epgsqla:bind(C, Statement, PortalName, Parameters),
    sync_on_error(C, receive_result(C, Ref)).

%% execute

execute(C, S) ->
    execute(C, S, "", 0).

execute(C, S, N) ->
    execute(C, S, "", N).

execute(C, S, PortalName, N) ->
    Ref = epgsqla:execute(C, S, PortalName, N),
    receive_result(C, Ref).

execute_batch(C, Batch) ->
    Ref = epgsqla:execute_batch(C, Batch),
    receive_result(C, Ref).

execute_batch(C, #statement{columns = Cols} = Stmt, Batch) ->
    Ref = epgsqla:execute_batch(C, Stmt, Batch),
    {Cols, receive_result(C, Ref)};
execute_batch(C, Sql, Batch) ->
    case parse(C, Sql) of
        {ok, #statement{} = S} ->
            execute_batch(C, S, Batch);
        Error ->
            Error
    end.

%% statement/portal functions

describe(C, #statement{name = Name}) ->
    describe(C, statement, Name).

describe(C, Type, Name) ->
    Ref = epgsqla:describe(C, Type, Name),
    %% TODO unknown result format of Describe portal
    sync_on_error(C, receive_result(C, Ref)).

close(C, #statement{name = Name}) ->
    close(C, statement, Name).

close(C, Type, Name) ->
    Ref = epgsqla:close(C, Type, Name),
    receive_result(C, Ref).

sync(C) ->
    Ref = epgsqla:sync(C),
    receive_result(C, Ref).

receive_result(C, Ref) ->
    %% TODO timeout
    receive
        {C, Ref, Result} ->
            Result;
        %% TODO no 'EXIT' for not linked processes
        {'EXIT', C, _Reason} ->
            {error, closed}
    end.

sync_on_error(C, Error = {error, _}) ->
    Ref = epgsqla:sync(C),
    receive_result(C, Ref),
    Error;

sync_on_error(_C, R) ->
    R.

