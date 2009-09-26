%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
-module(epgsql_wrapper).

%% this code was not tested

%% emulate the epgsql interface here:
%% http://bitbucket.org/will/epgsql/overview/
%% as a temporary measure to rely on the old native Postgres interface here:
%% http://svn.process-one.net/ejabberd-modules/pgsql/trunk/
%% (taken at r952 2009-05-06 10:29:39)
-export([connect/4,
         close/1,
         squery/2,
         with_transaction/2]).

-define(QUERY_TIMEOUT, 5000).

connect(Host, Username, Password, Opts) ->
    pgsql_proto:start_link([
        {host, Host},
        {user, Username},
        {password, Password}] ++ Opts).
    % {pgsql_notice, Notice} message handled in handle_info

close(Db) ->
    gen_server:call(Db, terminate),
    ok.

squery(Db, Query) ->
    ConvertLog = fun(F, [H | Lin], Lout) ->
        case H of
            [] ->
                Lout;
            {error, Error} ->
                F(F, Lin, Lout ++ [{error, Error}]);
            {Command, Cols, Rows} ->
                io:format("received command type 1 ?: ~p (~p, ~p)~n",
                          [Command, Cols, Rows]),
                F(F, Lin, Lout ++ [{ok, 0, Cols, Rows}]);
            Command when is_list(Command) ->
                io:format("received command type 2 ?: ~p~n",
                          [Command]),
                F(F, Lin, Lout ++ [{ok, 0}])
        end
    end,
    try gen_server:call(Db, {squery, Query}, ?QUERY_TIMEOUT) of
        {ok, Log} ->
            case ConvertLog(ConvertLog, Log, []) of
                [Result] ->
                    Result;
                Results ->
                    Results
            end
    catch
        exit:{timeout, _} ->
            {error, timeout}
    end.

with_transaction(Db, F) ->
    try
        {ok, 0, [], []} = squery(Db, "BEGIN"),
        R = F(Db),
        {ok, 0, [], []} = squery(Db, "COMMIT"),
        R
    catch
        _:Why ->
            squery(Db, "ROLLBACK"),
            {rollback, Why}
    end.

