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
* `Haskell (GHC >= 7.10.3 and cabal-install >= 1.22)`
* `Java`
* `Javascript/node.js >= 0.8.0`
* `OCaml >= 4.03.0`
* `Perl >= 5.10 (with Compress::Zlib)`
* `PHP >= 5.3.6`
* `Python >= 2.7.0`
* `Ruby >= 1.9.0`

C/C++, Go, Haskell, Java, Javascript, OCaml, Perl, PHP, Python, and Ruby
all have an implementation of the CloudI API that is completely done in that
programming language (to avoid extra complexity).
Python does have a Python with C integration CloudI API that is the
Python module `cloudi_c` (instead of the Python module `cloudi`).
Any use of the CloudI API in these programming languages
(that are not running in the Erlang VM, i.e., "external" services)
is executed as separate OS processes with separate threads
(using any native threading library) with CloudI as the application server.

Erlang (or Elixir) integration uses the `cloudi_service` behavior to
create "internal" CloudI services.  Typically, the module name implementing
the `cloudi_service` behavior is built as an Erlang/OTP application
with the same name.

An example [SWIG](http://www.swig.org/) interface file is provided based on the
C CloudI API in [cloudi.i](https://github.com/CloudI/CloudI/blob/develop/src/api/c/cloudi.i).
SWIG could be used to create a CloudI API for Lua, R, D, etc.

### Examples exist within the integration tests:

#### [C/C++](https://cloudi.org/faq.html#6_C)

* [`src/tests/count` (C, no threads) Basic Count Test](https://github.com/CloudI/CloudI/blob/master/src/tests/count/c/main.c)
* [`src/tests/http_req` (C, no threads) Basic HTTP Request Test](https://github.com/CloudI/CloudI/blob/master/src/tests/http_req/c/main.c)
* [`src/tests/msg_size` (C++, no threads) Basic Message Size (Forwarding) Test](https://github.com/CloudI/CloudI/blob/master/src/tests/msg_size/cxx/main.cpp)
* [`src/tests/messaging` (C++, threads) Basic Messaging Test](https://github.com/CloudI/CloudI/blob/master/src/tests/messaging/cxx/main.cpp)
* [`src/tests/null` (C, no threads) Basic Null Response Test](https://github.com/CloudI/CloudI/blob/master/src/tests/null/c/main.c)
* [`src/tests/hexpi` (C++, threads) Hexadecimal PI Test](https://github.com/CloudI/CloudI/blob/master/src/tests/hexpi/cxx/main.cpp)

#### [Erlang](https://cloudi.org/faq.html#6_Erlang)

* [`src/tests/count` Basic Count Test](https://github.com/CloudI/CloudI/blob/master/src/tests/count/erlang/src/cloudi_service_test_count.erl)
* [`src/tests/http_req` Basic HTTP Request Test](https://github.com/CloudI/CloudI/blob/master/src/tests/http_req/erlang/src/cloudi_service_test_http_req.erl)
* [`src/tests/msg_size` Basic Message Size (Forwarding) Test](https://github.com/CloudI/CloudI/blob/master/src/tests/msg_size/erlang/src/cloudi_service_test_msg_size.erl)
* `src/tests/messaging` Basic Messaging Test ([Sequence 1](https://github.com/CloudI/CloudI/blob/master/src/tests/messaging/erlang/src/cloudi_service_test_messaging_sequence1.erl), [Sequence 2](https://github.com/CloudI/CloudI/blob/develop/src/tests/messaging/erlang/src/cloudi_service_test_messaging_sequence2.erl), [Sequence 3](https://github.com/CloudI/CloudI/blob/develop/src/tests/messaging/erlang/src/cloudi_service_test_messaging_sequence3.erl), [Sequence 4](https://github.com/CloudI/CloudI/blob/develop/src/tests/messaging/erlang/src/cloudi_service_test_messaging_sequence4.erl))
* [`src/tests/null` Basic Null Response Test](https://github.com/CloudI/CloudI/blob/master/src/tests/null/erlang/src/cloudi_service_test_null.erl)
* [Hexadecimal PI Fault-Tolerant Map/Reduce Test](https://github.com/CloudI/CloudI/blob/master/src/tests/hexpi/erlang/src/cloudi_service_test_hexpi.erl)

#### [Go](https://cloudi.org/faq.html#6_Go)

* [`src/tests/count` Basic Count Test](https://github.com/CloudI/CloudI/blob/master/src/tests/count/gopath/src/count_go/main.go)
* [`src/tests/http_req` Basic HTTP Request Test](https://github.com/CloudI/CloudI/blob/master/src/tests/http_req/gopath/src/http_req_go/main.go)
* [`src/tests/msg_size` Basic Message Size (Forwarding) Test](https://github.com/CloudI/CloudI/blob/master/src/tests/msg_size/gopath/src/msg_size_go/main.go)
* [`src/tests/messaging` Basic Messaging Test](https://github.com/CloudI/CloudI/blob/master/src/tests/messaging/gopath/src/messaging_go/main.go)
* [`src/tests/null` Basic Null Response Test](https://github.com/CloudI/CloudI/blob/master/src/tests/null/gopath/src/null_go/main.go)

#### [Haskell](https://cloudi.org/faq.html#6_Haskell)

* [`src/tests/count` Basic Count Test](https://github.com/CloudI/CloudI/blob/master/src/tests/count/haskell/Main.hs)
* [`src/tests/http_req` Basic HTTP Request Test](https://github.com/CloudI/CloudI/blob/master/src/tests/http_req/haskell/Main.hs)
* [`src/tests/msg_size` Basic Message Size (Forwarding) Test](https://github.com/CloudI/CloudI/blob/master/src/tests/msg_size/haskell/Main.hs)
* [`src/tests/messaging` Basic Messaging Test](https://github.com/CloudI/CloudI/blob/master/src/tests/messaging/haskell/Main.hs)
* [`src/tests/null` Basic Null Response Test](https://github.com/CloudI/CloudI/blob/master/src/tests/null/haskell/Main.hs)

#### [Java](https://cloudi.org/faq.html#6_Java)

* [`src/tests/count` Basic Count Test](https://github.com/CloudI/CloudI/blob/master/src/tests/count/java/org/cloudi/tests/count/Task.java)
* [`src/tests/http_req` Basic HTTP Request Test](https://github.com/CloudI/CloudI/blob/master/src/tests/http_req/java/org/cloudi/tests/http_req/Task.java)
* [`src/tests/msg_size` Basic Message Size (Forwarding) Test](https://github.com/CloudI/CloudI/blob/master/src/tests/msg_size/java/org/cloudi/tests/msg_size/Task.java)
* [`src/tests/messaging` Basic Messaging Test](https://github.com/CloudI/CloudI/blob/master/src/tests/messaging/java/org/cloudi/tests/messaging/Task.java)
* [`src/tests/null` Basic Null Response Test](https://github.com/CloudI/CloudI/blob/master/src/tests/null/java/org/cloudi/tests/null_/Task.java)

#### [Javascript](https://cloudi.org/faq.html#6_JavaScript)

* [`src/tests/count` Basic Count Test](https://github.com/CloudI/CloudI/blob/master/src/tests/count/count.js)
* [`src/tests/http_req` Basic HTTP Request Test](https://github.com/CloudI/CloudI/blob/master/src/tests/http_req/http_req.js)
* [`src/tests/msg_size` Basic Message Size (Forwarding) Test](https://github.com/CloudI/CloudI/blob/master/src/tests/msg_size/msg_size.js)
* [`src/tests/messaging` Basic Messaging Test](https://github.com/CloudI/CloudI/blob/master/src/tests/messaging/messaging.js)
* [`src/tests/null` Basic Null Response Test](https://github.com/CloudI/CloudI/blob/master/src/tests/null/null.js)

#### [OCaml](https://cloudi.org/faq.html#6_OCaml)

* [`src/tests/count` Basic Count Test](https://github.com/CloudI/CloudI/blob/master/src/tests/count/ocaml/main.ml)
* [`src/tests/http_req` Basic HTTP Request Test](https://github.com/CloudI/CloudI/blob/master/src/tests/http_req/ocaml/main.ml)
* [`src/tests/msg_size` Basic Message Size (Forwarding) Test](https://github.com/CloudI/CloudI/blob/master/src/tests/msg_size/ocaml/main.ml)
* [`src/tests/messaging` Basic Messaging Test](https://github.com/CloudI/CloudI/blob/master/src/tests/messaging/ocaml/main.ml)
* [`src/tests/null` Basic Null Response Test](https://github.com/CloudI/CloudI/blob/master/src/tests/null/ocaml/main.ml)

#### [Perl](https://cloudi.org/faq.html#6_Perl)

* [`src/tests/count` Basic Count Test](https://github.com/CloudI/CloudI/blob/master/src/tests/count/count.pl)
* [`src/tests/http_req` Basic HTTP Request Test](https://github.com/CloudI/CloudI/blob/master/src/tests/http_req/http_req.pl)
* [`src/tests/msg_size` Basic Message Size (Forwarding) Test](https://github.com/CloudI/CloudI/blob/master/src/tests/msg_size/msg_size.pl)
* [`src/tests/messaging` Basic Messaging Test](https://github.com/CloudI/CloudI/blob/master/src/tests/messaging/MessagingTask.pm)
* [`src/tests/null` Basic Null Response Test](https://github.com/CloudI/CloudI/blob/master/src/tests/null/null.pl)

#### [PHP](https://cloudi.org/faq.html#6_PHP)

* [`src/tests/count` Basic Count Test](https://github.com/CloudI/CloudI/blob/master/src/tests/count/count.php)
* [`src/tests/http_req` Basic HTTP Request Test](https://github.com/CloudI/CloudI/blob/master/src/tests/http_req/http_req.php)
* [`src/tests/msg_size` Basic Message Size (Forwarding) Test](https://github.com/CloudI/CloudI/blob/master/src/tests/msg_size/msg_size.php)
* [`src/tests/messaging` Basic Messaging Test](https://github.com/CloudI/CloudI/blob/master/src/tests/messaging/messaging.php)
* [`src/tests/null` Basic Null Response Test](https://github.com/CloudI/CloudI/blob/master/src/tests/null/null.php)

#### [Python](https://cloudi.org/faq.html#6_Python)

* [`src/tests/count` Basic Count Test](https://github.com/CloudI/CloudI/blob/master/src/tests/count/count.py)
* [`src/tests/http_req` Basic HTTP Request Test](https://github.com/CloudI/CloudI/blob/master/src/tests/http_req/http_req.py)
* [`src/tests/msg_size` Basic Message Size (Forwarding) Test](https://github.com/CloudI/CloudI/blob/master/src/tests/msg_size/msg_size.py)
* [`src/tests/messaging` Basic Messaging Test](https://github.com/CloudI/CloudI/blob/master/src/tests/messaging/messaging.py)
* [`src/tests/null` Basic Null Response Test](https://github.com/CloudI/CloudI/blob/master/src/tests/null/null.py)

#### [Ruby](https://cloudi.org/faq.html#6_Ruby)

* [`src/tests/count` Basic Count Test](https://github.com/CloudI/CloudI/blob/master/src/tests/count/count.rb)
* [`src/tests/http_req` Basic HTTP Request Test](https://github.com/CloudI/CloudI/blob/master/src/tests/http_req/http_req.rb)
* [`src/tests/msg_size` Basic Message Size (Forwarding) Test](https://github.com/CloudI/CloudI/blob/master/src/tests/msg_size/msg_size.rb)
* [`src/tests/messaging` Basic Messaging Test](https://github.com/CloudI/CloudI/blob/master/src/tests/messaging/messaging.rb)
* [`src/tests/null` Basic Null Response Test](https://github.com/CloudI/CloudI/blob/master/src/tests/null/null.rb)

Please see the [CloudI API documentation](https://cloudi.org/api.html#Service)
for more details.

