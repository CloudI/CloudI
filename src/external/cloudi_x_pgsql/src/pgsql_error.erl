%% -*- coding: utf-8 -*-
-module(pgsql_error).

-vsn("1").

-export([
         is_integrity_constraint_violation/1
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec is_integrity_constraint_violation({pgsql_error, _, _} | {pgsql_error, _}) -> boolean().
is_integrity_constraint_violation({pgsql_error, _Status, [LogEntry | _] = _Log}) ->
    % Old format.
    Class = get_sqlstate_class(LogEntry),
    Class =:= "23";
is_integrity_constraint_violation({pgsql_error, Fields}) ->
    case lists:keyfind(code, 1, Fields) of
        {code, <<"23", _SubClass:3/binary>>} -> true;   %% iso 9075-2 §22.1
        {code, <<_Other:5/binary>>} -> false;
        false -> false
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% iso 9075-2 §22.1
get_sqlstate_class(LogEntry) ->
    State = get_sqlstate(LogEntry),
    lists:sublist(State, 1, 2).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_sqlstate(LogEntry) ->
    {code, SqlState} = lists:keyfind(code, 1, LogEntry),
    SqlState.
