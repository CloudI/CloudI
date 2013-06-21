# CloudI Service API

## PURPOSE

Provide an interface for dynamically configuring CloudI.

## INTEGRATION

### API

Supported request formats:

* JSON-RPC
* Erlang term format
* native Erlang function calls (`cloudi_service_api` module) within the same Erlang VM

CloudI allows you to call dynamic configuration functions from any CloudI
process (assuming it is not blocked by an ACL).  The [default configuration](https://github.com/okeuday/CloudI/blob/master/src/cloudi.conf.in)
initializes the [CloudI process responsible for dynamic configuration](https://github.com/okeuday/CloudI/blob/master/src/lib/cloudi_services_internal/src/cloudi_service_api_requests.erl)
as a service subscribing to:

* `"/cloudi/api/json_rpc/"`
* `"/cloudi/api/erlang/*"` (see below)
  * `"/cloudi/api/erlang/acl_add"`
  * `"/cloudi/api/erlang/acl_remove"`
  * `"/cloudi/api/erlang/services_add"`
  * `"/cloudi/api/erlang/services_remove"`
  * `"/cloudi/api/erlang/services_restart"`
  * `"/cloudi/api/erlang/services"`
  * `"/cloudi/api/erlang/nodes_add"`
  * `"/cloudi/api/erlang/nodes_remove"`
  * `"/cloudi/api/erlang/nodes_alive"`
  * `"/cloudi/api/erlang/nodes_dead"`
  * `"/cloudi/api/erlang/nodes"`
  * `"/cloudi/api/erlang/loglevel_set"`
  * `"/cloudi/api/erlang/log_redirect"`
  * `"/cloudi/api/erlang/code_path_add"`
  * `"/cloudi/api/erlang/code_path_remove"`
  * `"/cloudi/api/erlang/code_path"`

The [default configuration](https://github.com/okeuday/CloudI/blob/master/src/cloudi.conf.in)
expects HTTP requests to port 6464 with the proper service path and format
(listed above) to call the CloudI service interface.

### EXAMPLES

CloudI Service API integration example:

* [Python](https://github.com/okeuday/CloudI/blob/master/src/service_api/python/cloudi_service_api.py) (JSON-RPC over HTTP)

CloudI Service API usage examples:

* [Python](https://github.com/okeuday/CloudI/blob/master/src/tests/service_api/logging_off.py) (Turn logging off)
* [Python](https://github.com/okeuday/CloudI/blob/master/src/tests/service_api/logging_on.py) (Turn logging on)
* [Python](https://github.com/okeuday/CloudI/blob/master/src/tests/service_api/path.py) (Add to the Erlang code search path)
* [Python](https://github.com/okeuday/CloudI/blob/master/src/tests/service_api/run.py) (Add/Remove nodes, Add/Remove ACL entries, Start/Stop services)

Please see the [FAQ](http://cloudi.org/faq.html) for more details
(a graphical interface will be added soon, which will provide more explicit
examples for utilizing the CloudI Service API).
