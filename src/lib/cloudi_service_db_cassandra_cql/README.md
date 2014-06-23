cloudi_service_db_cassandra_cql
===============================

**CloudI DB Cassandra CQL Service**

CloudI layer on top of erlcql from [https://github.com/rpt/erlcql.git](http://)

NOTE: set service config `count_process` to desired pool size

`erlcql_client` results will be wrapped in `cloudi:send_sync` result:
     `{ok, ErlcqlClientResult}`

As a result all successful responses from `erlcql_client` will be of the form
        `{ok, {ok, Response}}`

All failures from `erlcql_client` will be of the form
        `{ok, {error, Reason}}`

`send_sync` errors will be standard 'CloudI' errors
