# CloudI Service API

## PURPOSE

Provide an interface for dynamically configuring CloudI.

## INTEGRATION

### API

Supported request formats:

* JSON-RPC
* Erlang term format
* native Erlang function calls ([`cloudi_service_api`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_core/src/cloudi_service_api.erl) module) within the same Erlang VM

CloudI allows you to call dynamic configuration functions from any CloudI
process (assuming it is not blocked by an ACL).  The [default configuration](https://github.com/CloudI/CloudI/blob/master/src/cloudi.conf.in)
initializes the [CloudI service responsible for dynamic configuration](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_services_internal/src/cloudi_service_api_requests.erl)
which subscribes to service names based on the functions defined in the [`cloudi_service_api`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_core/src/cloudi_service_api.erl) module:

* `"/cloudi/api/rpc.json"`
* `"/cloudi/api/rpc/*.erl"` (see below)
  * `"/cloudi/api/rpc/acl_add.erl"`
  * `"/cloudi/api/rpc/acl_remove.erl"`
  * `"/cloudi/api/rpc/acl.erl"`
  * `"/cloudi/api/rpc/service_subscriptions.erl"`
  * `"/cloudi/api/rpc/services_add.erl"`
  * `"/cloudi/api/rpc/services_remove.erl"`
  * `"/cloudi/api/rpc/services_restart.erl"`
  * `"/cloudi/api/rpc/services_search.erl"`
  * `"/cloudi/api/rpc/services.erl"`
  * `"/cloudi/api/rpc/nodes_set.erl"`
  * `"/cloudi/api/rpc/nodes_get.erl"`
  * `"/cloudi/api/rpc/nodes_add.erl"`
  * `"/cloudi/api/rpc/nodes_remove.erl"`
  * `"/cloudi/api/rpc/nodes_alive.erl"`
  * `"/cloudi/api/rpc/nodes_dead.erl"`
  * `"/cloudi/api/rpc/nodes.erl"`
  * `"/cloudi/api/rpc/logging_file_set.erl"`
  * `"/cloudi/api/rpc/logging_level_set.erl"`
  * `"/cloudi/api/rpc/logging_syslog_set.erl"`
  * `"/cloudi/api/rpc/logging_formatters_set"`
  * `"/cloudi/api/rpc/logging_redirect_set.erl"`
  * `"/cloudi/api/rpc/logging.erl"`
  * `"/cloudi/api/rpc/code_path_add.erl"`
  * `"/cloudi/api/rpc/code_path_remove.erl"`
  * `"/cloudi/api/rpc/code_path.erl"`

The [default configuration](https://github.com/CloudI/CloudI/blob/master/src/cloudi.conf.in)
expects HTTP requests to port 6464 with the proper service path and format
(listed above) to call the CloudI service interface.

### DASHBOARD

The CloudI dashboard provides a web UI with all the CloudI Service API
functionality available to the user visually.  The installed configuration
provides the web UI on port 6464 (i.e., [http://localhost:6464/cloudi/](http://localhost:6464/cloudi/)).

### EXAMPLES

CloudI Service API integration example:

* [Python](https://github.com/CloudI/CloudI/blob/master/src/service_api/python/cloudi_service_api.py) (JSON-RPC over HTTP)

CloudI Service API usage examples:

* [Python](https://github.com/CloudI/CloudI/blob/master/src/tests/service_api/logging_off.py) (Turn logging off)
* [Python](https://github.com/CloudI/CloudI/blob/master/src/tests/service_api/logging_on.py) (Turn logging on)
* [Python](https://github.com/CloudI/CloudI/blob/master/src/tests/service_api/path.py) (Add to the Erlang code search path)
* [Python](https://github.com/CloudI/CloudI/blob/master/src/tests/service_api/run.py) (Add/Remove nodes, Add/Remove ACL entries, Start/Stop services)

Please see the [CloudI Service API documentation](http://cloudi.org/api.html#CloudI)
for more details.
