{application, pgsql,
 [{description, "PostgreSQL Client "
                "(old ejabberd interface, r952 2009-05-06 10:29:39)"},
  {vsn, "1.0"},
  {modules, [epgsql_wrapper,
             pgsql,
             pgsql_proto,
             pgsql_tcp,
             pgsql_util]},
  {registered, []},
  {applications, [kernel, stdlib]},
  {included_applications, []}]}.
