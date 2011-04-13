#CloudI API

## PURPOSE

Provide an interface for external software to utilize CloudI's scalability,
fault-tolerance, messaging, dynamic configuration, and other features.

## HOW

Supported languages:

* Erlang >= R14B01
* C
* C++
* Java
* Python >= 2.5.0
* Ruby >= 1.9.0

For Erlang integration, use the cloudi_job behavior which is included within
the cloudi application.  The Erlang code is expected to run in a separate
application within the same Erlang VM as CloudI.  Erlang supports writing
very robust software but Erlang development within CloudI requires more
scrutiny because the Erlang code can easily access CloudI internals to
bypass the ACL functionality and manipulate the system in malicious ways.

C++, Java, Python, and Ruby all use the same function names that exist within
the cloudi_job Erlang behavior, but each language makes function calls into
its own native CloudI library.  The external languages are ran as separate
OS processes with separate threads (using any native threading library) that
initialize the CloudI library and call its event-loop (in the "poll" function).

Examples exist within the tests:

* [Erlang](https://github.com/okeuday/CloudI/blob/master/src/tests/flood/src/cloudi_job_flood.erl) ([flood test](https://github.com/okeuday/CloudI/blob/master/src/tests/flood/) control module)
* [C](https://github.com/okeuday/CloudI/blob/master/src/tests/http_req/c_src/main.c) ([http_req test](https://github.com/okeuday/CloudI/blob/master/src/tests/http_req/) HTTP handler)
* [C++](https://github.com/okeuday/CloudI/blob/master/src/tests/hexpi/cxx_src/main.cpp) ([hexpi test](https://github.com/okeuday/CloudI/blob/master/src/tests/hexpi/) worker threads)
* [Java](https://github.com/okeuday/CloudI/tree/master/src/tests/http/service/org/cloudi/tests/http) ([http test](https://github.com/okeuday/CloudI/blob/master/src/tests/http/) HTTP handler)
* [Python](https://github.com/okeuday/CloudI/blob/master/src/tests/http/service/service.py) ([http test](https://github.com/okeuday/CloudI/blob/master/src/tests/http/) HTTP handler)
* [Ruby](https://github.com/okeuday/CloudI/blob/master/src/tests/http/service/service.rb) ([http test](https://github.com/okeuday/CloudI/blob/master/src/tests/http/) HTTP handler)

