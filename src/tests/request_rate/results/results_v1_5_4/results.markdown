Hardware
--------

    Core i7 2700K 3.5GHz 1 cpu, 4 cores/cpu, 2 hts/core
    L2:4x256KB L3:8MB RAM:16GB:DDR3-1333MHz
    Sandy Bridge-HE-4 (LGA1155)

    WDC WD10EZEX (1TB)
    max UDMA/133 (133 MB/s), SATA rev 2 (3.0 Gbps, 300 MB/s)
    4096 byte physical blocks, 512 byte logical blocks

Software
---------

    Ubuntu 12.04.05 LTS (3.2.0-20-generic x86_64)
    Erlang 19.1
    CloudI 1.5.4 (v1.5.4-rc5 cloudi_core tag)
    GCC version 4.6.3 (Ubuntu/Linaro 4.6.3-1ubuntu5)
    Java OpenJDK 1.8.0_91 (1.8.0_91-8u91-b14-0ubuntu4~12.04-b14)

Test
----

Use cloudi_service_request_rate to send service requests to the
http_req integration test written in a supported programming language.
Vary the number of sending service processes and the number of
receiving service processes (either threads or OS processes when testing
external services) to determine the effect on the maximum stable request
rate that can be sustained (for at least 2 minutes without any errors or
timeouts).  The destination refresh method (a service configuration
parameter, either immediate_closest or lazy_closest) and the
duo_mode (a service configuration option) are both settings for the
sender (cloudi_service_request_rate) and the queue (cloudi_service_queue).

The harddisk used for cloudi_service_queue storage is limited to
a max of 133 MB/s but the disk write throughput during testing was
5-25 MB/s total (based on iotop) split between 5 threads
(the erl command line option +A 5 sets the number of file module threads).

Max Stable (Total) Request Rate (requests/second): Erlang
---------------------------------------------------------

    wo/queue  w/queue dest_refresh_method duo_mode sender queue receiver
        1649      911           immediate    false      1     1        1
        1627      877                lazy    false      1     1        1
       23638     1545           immediate     true      1     1        1
       27247     1549                lazy     true      1     1        1
                 2036                lazy     true      1     2        1 
                 2259                lazy     true      1     4        1 
                 2261                lazy     true      1     8        1 
       25013     1549                lazy     true      1     1        2
                 2095                lazy     true      1     2        2
       23981     1460                lazy     true      1     1        4
       36920     2217           immediate     true      2     2        2
       39952     2118                lazy     true      2     2        2
       42213     2176                lazy     true      2     2        4
                 2227                lazy     true      2     4        4
       52196     2259           immediate     true      4     4        4
       61657     2193                lazy     true      4     4        4
 
Max Stable (Total) Request Rate: C (without threads)
----------------------------------------------------

    wo/queue  w/queue dest_refresh_method duo_mode sender queue receiver
       1510       890           immediate    false      1     1        1
       1607       909                lazy    false      1     1        1
      19417      1398           immediate     true      1     1        1
      16147      1453                lazy     true      1     1        1
      24567      1485                lazy     true      1     1        2
      21828      1496                lazy     true      1     1        4
      24034      2097           immediate     true      2     2        2
      22413      2064                lazy     true      2     2        2
      33715      2201                lazy     true      2     2        4
      23260      2128           immediate     true      4     4        4
      25673      2283                lazy     true      4     4        4
 
Max Stable (Total) Request Rate: Java (with threads)
----------------------------------------------------

    wo/queue  w/queue dest_refresh_method duo_mode sender queue receiver
       1526       899           immediate    false      1     1        1
       1489       871                lazy    false      1     1        1
      21481      1475           immediate     true      1     1        1
      15879      1497                lazy     true      1     1        1
      22574      1494                lazy     true      1     1        2
      21334      1478                lazy     true      1     1        4
      23334      2130           immediate     true      2     2        2
      18953      2142                lazy     true      2     2        2
      33695      1932                lazy     true      2     2        4
      23073      2178           immediate     true      4     4        4
      28282      2121                lazy     true      4     4        4
 
