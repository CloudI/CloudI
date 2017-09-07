Hardware
--------

    Core i7 2700K 3.5GHz 1 cpu, 4 cores/cpu, 2 hts/core
    L2:4x256KB L3:8MB RAM:16GB:DDR3-1333MHz
    Sandy Bridge-HE-4 (LGA1155)

Software
---------

    Ubuntu 12.04.05 LTS (3.2.0-20-generic x86_64)
    Erlang 19.3
    CloudI 1.7.1 (42457952ad1223bfd2ca089c703df40f953ddbfd)
    GCC version 4.6.3 (Ubuntu/Linaro 4.6.3-1ubuntu5)
    Go (go gc) go1.7.5 linux/amd64
    GHC 7.10.3
    Java OpenJDK 1.8.0_91 (1.8.0_91-8u91-b14-0ubuntu4~12.04-b14)
    NodeJS v6.10.2
    OCaml 4.03.0
    Perl v5.14.2
    PHP 5.3.10-1ubuntu3.15 (Zend Engine v2.3.0)
    Python 2.7.3/3.2.3
    Ruby 1.9.3p0 (2011-10-30 revision 33570) [x86_64-linux]

Test
----

Use cloudi_service_request_rate to send service requests to the
http_req integration test written in a supported programming language.
While keeping the total concurrency at 4 (for this 4 core machine),
with 2 senders and 2 receivers, vary the receiver service configuration
to determine the effect on the maximum stable request rate that can be
sustained (for at least 2 minutes without any errors or timeouts).
The destination refresh method (a service configuration parameter)
and the duo_mode (a service configuration option) are settings for the
sender service (cloudi_service_request_rate).

Max Stable (Total) Request Rate (requests/second): C
----------------------------------------------------

    req/sec  dest_refresh_method duo_mode sender receiver
      21476                 lazy     true      2        2 (processes,-O0)
 
Max Stable (Total) Request Rate (requests/second): Erlang
---------------------------------------------------------

    req/sec  dest_refresh_method duo_mode sender receiver
      31081                 lazy     true      2        2
 
Max Stable (Total) Request Rate (requests/second): Go
-----------------------------------------------------

    req/sec  dest_refresh_method duo_mode sender receiver
      17944                 lazy     true      2        2 (processes)
      18007                 lazy     true      2        2 (threads)
 
Max Stable (Total) Request Rate (requests/second): Haskell
----------------------------------------------------------

    req/sec  dest_refresh_method duo_mode sender receiver
      17888                 lazy     true      2        2 (processes)
      19959                 lazy     true      2        2 (threads)
 
Max Stable (Total) Request Rate (requests/second): Java
-------------------------------------------------------

    req/sec  dest_refresh_method duo_mode sender receiver
      19386                 lazy     true      2        2 (processes)
      20121                 lazy     true      2        2 (threads)
 
Max Stable (Total) Request Rate (requests/second): Javascript
-------------------------------------------------------------

    req/sec  dest_refresh_method duo_mode sender receiver
      10856                 lazy     true      2        2 (processes)
 
Max Stable (Total) Request Rate (requests/second): OCaml
--------------------------------------------------------

    req/sec  dest_refresh_method duo_mode sender receiver
      22477                 lazy     true      2        2 (threads)
      22585                 lazy     true      2        2 (processes)

Max Stable (Total) Request Rate (requests/second): Perl
-------------------------------------------------------

    req/sec  dest_refresh_method duo_mode sender receiver
      13126                 lazy     true      2        2 (threads)
      13502                 lazy     true      2        2 (processes)

Max Stable (Total) Request Rate (requests/second): PHP
------------------------------------------------------

    req/sec  dest_refresh_method duo_mode sender receiver
      14512                 lazy     true      2        2 (processes)
 
Max Stable (Total) Request Rate (requests/second): Python
---------------------------------------------------------

    req/sec  dest_refresh_method duo_mode sender receiver
      10452                 lazy     true      2        2 (v3 threads)
      10985                 lazy     true      2        2 (v2 threads)
      14121                 lazy     true      2        2 (v3 processes)
      14424                 lazy     true      2        2 (v2 processes)
      20213                 lazy     true      2        2 (v2/C threads)
      21334                 lazy     true      2        2 (v2/C processes)

Max Stable (Total) Request Rate (requests/second): Ruby
-------------------------------------------------------

    req/sec  dest_refresh_method duo_mode sender receiver
      10673                 lazy     true      2        2 (threads)
      17245                 lazy     true      2        2 (processes)

