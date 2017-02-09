# CloudI API

## PURPOSE

Provide an interface for external software to utilize CloudI's scalability,
fault-tolerance, messaging, dynamic configuration, and other features.

## INTEGRATION

### Supported languages:

* `C`
* `C++`
* `Elixir`
* `Erlang >= 19.0`
* `Go >= 1.6`
* `Java`
* `Javascript/node.js >= 0.8.0`
* `Perl >= 5.10`
* `PHP >= 5.3.6`
* `Python >= 2.7.0`
* `Ruby >= 1.9.0`

C/C++, Go, Java, Javascript, Perl, PHP, Python, and Ruby all have an
implementation of the CloudI API that is completely done in that programming
language (to avoid extra complexity).  Python does have a Python with C
integration CloudI API that is the Python module `cloudi_c`
(instead of the Python module `cloudi`).  Any use of the CloudI API
in these programming languages (that are not running in the Erlang VM,
i.e., "external" services) is executed as separate OS processes with separate
threads (using any native threading library) with CloudI as the application
server.

Erlang (or Elixir) integration uses the `cloudi_service` behavior to
create "internal" CloudI services.  Typically, the module name implementing
the `cloudi_service` behavior is built as an Erlang/OTP application
with the same name.

An example [SWIG](http://www.swig.org/) interface file is provided based on the
C CloudI API in [cloudi.i](https://github.com/CloudI/CloudI/blob/develop/src/api/c/cloudi.i).
SWIG could be used to create a CloudI API for Lua, R, Go, Ocaml, D, etc.

### Examples exist within the integration tests:

#### [C/C++](http://cloudi.org/faq.html#6_C)

* [`src/tests/http_req` (C, no threads) Basic HTTP Request Test](https://github.com/CloudI/CloudI/blob/master/src/tests/http_req/c_src/main.c)
* [`src/tests/msg_size` (C++, no threads) Basic Message Size (Forwarding) Test](https://github.com/CloudI/CloudI/blob/master/src/tests/msg_size/cxx_src/main.cpp)
* [`src/tests/messaging` (C++, threads) Basic Messaging Test](https://github.com/CloudI/CloudI/blob/master/src/tests/messaging/cxx_src/main.cpp)
* [`src/tests/null` (C, no threads) Basic Null Response Test](https://github.com/CloudI/CloudI/blob/master/src/tests/null/c_src/main.c)
* [`src/tests/hexpi` (C++, threads) Hexadecimal PI Test](https://github.com/CloudI/CloudI/blob/master/src/tests/hexpi/cxx_src/main.cpp)

#### [Erlang](http://cloudi.org/faq.html#6_Erlang)

* [`src/tests/http_req` Basic HTTP Request Test](https://github.com/CloudI/CloudI/blob/master/src/tests/http_req/src/cloudi_service_http_req.erl)
* [`src/tests/msg_size` Basic Message Size (Forwarding) Test](https://github.com/CloudI/CloudI/blob/master/src/tests/msg_size/src/cloudi_service_msg_size.erl)
* `src/tests/messaging` Basic Messaging Test ([Sequence 1](https://github.com/CloudI/CloudI/blob/master/src/tests/messaging/src/cloudi_service_messaging_sequence1.erl), [Sequence 2](https://github.com/CloudI/CloudI/blob/master/src/tests/messaging/src/cloudi_service_messaging_sequence2.erl), [Sequence 3](https://github.com/CloudI/CloudI/blob/master/src/tests/messaging/src/cloudi_service_messaging_sequence3.erl), [Sequence 4](https://github.com/CloudI/CloudI/blob/master/src/tests/messaging/src/cloudi_service_messaging_sequence4.erl))
* [`src/tests/null` Basic Null Response Test](https://github.com/CloudI/CloudI/blob/master/src/tests/null/src/cloudi_service_null.erl)
* [Hexadecimal PI Fault-Tolerant Map/Reduce Test](https://github.com/CloudI/CloudI/blob/master/src/tests/hexpi/src/cloudi_service_hexpi.erl)

#### [Go](http://cloudi.org/faq.html#6_Go)

* [`src/tests/http_req` Basic HTTP Request Test](https://github.com/CloudI/CloudI/blob/develop/src/tests/http_req/gopath/src/http_req_go/main.go)
* [`src/tests/msg_size` Basic Message Size (Forwarding) Test](https://github.com/CloudI/CloudI/blob/develop/src/tests/msg_size/gopath/src/msg_size_go/main.go)
* [`src/tests/messaging` Basic Messaging Test](https://github.com/CloudI/CloudI/blob/develop/src/tests/messaging/gopath/src/messaging_go/main.go)
* [`src/tests/null` Basic Null Response Test](https://github.com/CloudI/CloudI/blob/develop/src/tests/null/gopath/src/null_go/main.go)

#### [Java](http://cloudi.org/faq.html#6_Java)

* [`src/tests/http_req` Basic HTTP Request Test](https://github.com/CloudI/CloudI/blob/master/src/tests/http_req/org/cloudi/tests/http_req/Task.java)
* [`src/tests/msg_size` Basic Message Size (Forwarding) Test](https://github.com/CloudI/CloudI/blob/master/src/tests/msg_size/org/cloudi/tests/msg_size/Task.java)
* [`src/tests/messaging` Basic Messaging Test](https://github.com/CloudI/CloudI/blob/master/src/tests/messaging/org/cloudi/tests/messaging/Task.java)
* [`src/tests/null` Basic Null Response Test](https://github.com/CloudI/CloudI/blob/master/src/tests/null/org/cloudi/tests/null_/Task.java)

#### [Javascript](http://cloudi.org/faq.html#6_JavaScript)

* [`src/tests/http_req` Basic HTTP Request Test](https://github.com/CloudI/CloudI/blob/master/src/tests/http_req/http_req.js)
* [`src/tests/msg_size` Basic Message Size (Forwarding) Test](https://github.com/CloudI/CloudI/blob/master/src/tests/msg_size/msg_size.js)
* [`src/tests/messaging` Basic Messaging Test](https://github.com/CloudI/CloudI/blob/master/src/tests/messaging/messaging.js)
* [`src/tests/null` Basic Null Response Test](https://github.com/CloudI/CloudI/blob/master/src/tests/null/null.js)

#### [Perl](http://cloudi.org/faq.html#6_Perl)

* [`src/tests/http_req` Basic HTTP Request Test](https://github.com/CloudI/CloudI/blob/master/src/tests/http_req/http_req.pl)
* [`src/tests/msg_size` Basic Message Size (Forwarding) Test](https://github.com/CloudI/CloudI/blob/master/src/tests/msg_size/msg_size.pl)
* [`src/tests/messaging` Basic Messaging Test](https://github.com/CloudI/CloudI/blob/master/src/tests/messaging/MessagingTask.pm)
* [`src/tests/null` Basic Null Response Test](https://github.com/CloudI/CloudI/blob/master/src/tests/null/null.pl)

#### [PHP](http://cloudi.org/faq.html#6_PHP)

* [`src/tests/http_req` Basic HTTP Request Test](https://github.com/CloudI/CloudI/blob/master/src/tests/http_req/http_req.php)
* [`src/tests/msg_size` Basic Message Size (Forwarding) Test](https://github.com/CloudI/CloudI/blob/master/src/tests/msg_size/msg_size.php)
* [`src/tests/messaging` Basic Messaging Test](https://github.com/CloudI/CloudI/blob/master/src/tests/messaging/messaging.php)
* [`src/tests/null` Basic Null Response Test](https://github.com/CloudI/CloudI/blob/master/src/tests/null/null.php)

#### [Python](http://cloudi.org/faq.html#6_Python)

* [`src/tests/http_req` Basic HTTP Request Test](https://github.com/CloudI/CloudI/blob/master/src/tests/http_req/http_req.py)
* [`src/tests/msg_size` Basic Message Size (Forwarding) Test](https://github.com/CloudI/CloudI/blob/master/src/tests/msg_size/msg_size.py)
* [`src/tests/messaging` Basic Messaging Test](https://github.com/CloudI/CloudI/blob/master/src/tests/messaging/messaging.py)
* [`src/tests/null` Basic Null Response Test](https://github.com/CloudI/CloudI/blob/master/src/tests/null/null.py)

#### [Ruby](http://cloudi.org/faq.html#6_Ruby)

* [`src/tests/http_req` Basic HTTP Request Test](https://github.com/CloudI/CloudI/blob/master/src/tests/http_req/http_req.rb)
* [`src/tests/msg_size` Basic Message Size (Forwarding) Test](https://github.com/CloudI/CloudI/blob/master/src/tests/msg_size/msg_size.rb)
* [`src/tests/messaging` Basic Messaging Test](https://github.com/CloudI/CloudI/blob/master/src/tests/messaging/messaging.rb)
* [`src/tests/null` Basic Null Response Test](https://github.com/CloudI/CloudI/blob/master/src/tests/null/null.rb)

Please see the [CloudI API documentation](http://cloudi.org/api.html#Service)
for more details.

