#CloudI Job API

## PURPOSE

Provide an interface for dynamically configuring CloudI.

## HOW

Supported request formats:

* JSON-RPC
* Erlang term format

CloudI allows you to call dynamic configuration functions from any CloudI
process (assuming it is not blocked by an ACL).  The [default configuration](https://github.com/okeuday/CloudI/blob/master/src/cloudi.conf.in)
initializes the [CloudI process responsible for dynamic configuration](https://github.com/okeuday/CloudI/blob/master/src/lib/cloudi/src/cloudi_job_api.erl)
as a service subscribing to:

* "/cloudi/api/json_rpc/"
* "/cloudi/api/erlang/*" (see below)
  * "/cloudi/api/erlang/acl_add"
  * "/cloudi/api/erlang/acl_remove"
  * "/cloudi/api/erlang/jobs_add"
  * "/cloudi/api/erlang/jobs_remove"
  * "/cloudi/api/erlang/nodes_add"
  * "/cloudi/api/erlang/nodes_remove"
  * "/cloudi/api/erlang/nodes_alive"
  * "/cloudi/api/erlang/nodes_dead"
  * "/cloudi/api/erlang/nodes"

The [default configuration](https://github.com/okeuday/CloudI/blob/master/src/cloudi.conf.in)
expects HTTP requests to port 6464 with the proper service path and format
(listed above) to call the CloudI job interface.

Examples exist:

* [Python](https://github.com/okeuday/CloudI/blob/master/src/job_api/python/cloudi_job_api.py) (JSON-RPC over HTTP)

