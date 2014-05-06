-record(epgsql_wg_column,    {name, type, size, modifier, format}).
-record(epgsql_wg_statement, {name, columns, types}).

-record(epgsql_wg_error,  {severity, code, message, extra}).
