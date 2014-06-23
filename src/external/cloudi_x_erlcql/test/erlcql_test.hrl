-import(erlcql_test, [create_keyspace/0,
                      keyspace_exists/1,
                      drop_keyspace/1,
                      gen_table_name/0,
                      single_query/1,
                      'query'/2,
                      execute/3,
                      start_client/1,
                      stop_client/1]).

-define(c(Name, Config), proplists:get_value(Name, Config)).
