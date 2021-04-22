# PI as a Hexadecimal Fraction

hexpi uses a C++ CloudI service to calculate individual digits of PI in
hexadecimal based on the digits requested from an
Erlang CloudI service (`cloudi_service_test_hexpi`).
`cloudi_service_test_hexpi` is implemented using the
`cloudi_service_map_reduce` behaviour to manage the map-reduce input/output
of the iterative algorithm.

The C++ CloudI service contains the iterative algorithm for calculating
hexadecimal digits of PI (the Bailey–Borwein–Plouffe formula).
`cloudi_service_test_hexpi` will adjust the number of digits requested
based on the amount of time previous service requests took for their
calculations.

When the hexpi C++ CloudI service and Erlang CloudI service are ran together
with the other integration tests, each segment of PI in hexadecimal is stored
using the `cloudi_service_filesystem` Erlang service in hexpi.txt
(with HTTP bytes range requests).  The file is accessible at
[http://localhost:6464/tests/http_req/hexpi.txt](http://localhost:6464/tests/http_req/hexpi.txt).

## What does PI look like in hexadecimal?

Python example:

    >>> hexpi = '3.243f6a8885a3'
    >>> pi = int(hexpi[0], 16) + sum([
            int(c, 16) / (16.0 ** i) for i, c in enumerate(hexpi[2:], start=1)
        ])
    >>> import math
    >>> math.pi == pi
