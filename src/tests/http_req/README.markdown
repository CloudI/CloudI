#CloudI Loadtesting

## ABOUT

The following requests are available (see
[https://github.com/CloudI/loadtests/](https://github.com/CloudI/loadtests/)
in `tests/http_req/loadtest/results_v*/*_tsung_output.tar.bz2`):

    http://localhost:8080/tests/http_req/erlang.xml?value=42
    http://localhost:8080/tests/http_req/c.xml?value=42
    http://localhost:8080/tests/http_req/java.xml?value=42
    http://localhost:8080/tests/http_req/javascript.xml?value=42
    http://localhost:8080/tests/http_req/perl.xml?value=42
    http://localhost:8080/tests/http_req/php.xml?value=42
    http://localhost:8080/tests/http_req/python.xml?value=42
    http://localhost:8080/tests/http_req/python_c.xml?value=42
    http://localhost:8080/tests/http_req/ruby.xml?value=42

Which all give the following response, from the associated programming language:

    <http_test><value>42</value></http_test>

The test's task is simple usage of an XML response to a HTTP GET request
(based on an [older misultin loadtest](http://www.ostinelli.net/a-comparison-between-misultin-mochiweb-cowboy-nodejs-and-tornadoweb/)),
which requires minimal processing in each programming
language.  The misultin support in CloudI has been removed (in version 1.2.0),
so now cowboy is the preferred HTTP server with integration provided by
`cloudi_service_http_cowboy`.  The loadtest results from the version 1.0.0
release only used `cloudi_service_http_misultin` (older misultin integration)
but the loadtest results from the version 1.1.0 release used both
`cloudi_service_http_misultin` and `cloudi_service_http_cowboy`.  The
version 1.1.0 results showed cowboy performance was superior to misultin and
justified the removal of misultin in version 1.2.0 (due to the parameterized
module usage and the lack of active development).

The CloudI loadtesting uses [Tsung](http://tsung.erlang-projects.org/)
to produce dependable loadtesting results
(the Tsung configuration files are within
 [https://github.com/CloudI/loadtests/](https://github.com/CloudI/loadtests/)
 in `tests/http_req/loadtest/results_v*/*_tsung_output.tar.bz2`
 in the `setup` directory).  Testing after CloudI version 1.2.2 required
using machine #2 instead of machine #1 due to overheating and being poorly
suited for loadtesting.  Despite both machines having similar technical
specifications, machine #2 has much lower latency during testing and the
results are more trustworthy.

##CONFIGURATION

The general software configuration files are in
[https://github.com/CloudI/loadtests/](https://github.com/CloudI/loadtests/)
in `tests/http_req/loadtest/results_v*/*_tsung_output.tar.bz2`
(inside the `setup` directory).

###Hardware

####Machine 1 `(<= 1.2.2)`

    Core i7 2670QM 2.2GHz 1 cpu, 4 cores/cpu, 2 hts/core
    L2:4×256KB L3:6MB RAM:8GB:DDR3-1333MHz
    Sandy Bridge-HE-4 (Socket G2)

####Machine 2 `(1.4.0)`

    Core i7 2700K 3.5GHz 1 cpu, 4 cores/cpu, 2 hts/core
    L2:4x256KB L3:8MB RAM:16GB:DDR3-1333MHz
    Sandy Bridge-HE-4 (LGA1155)

####Network

    Gigabit ethernet (Netgear JGS516)

###Software

    Ubuntu 12.04 LTS (GNU/Linux 3.2.0 x86_64)

    Erlang source compilation configuration:
    ./configure --enable-threads --enable-smp-support --enable-kernel-poll --disable-hipe

Settings added to /etc/sysctl.conf

    # Maximum TCP Receive Window
    net.core.rmem_max = 33554432
    # Maximum TCP Send Window
    net.core.wmem_max = 33554432
    # others
    net.ipv4.tcp_rmem = 4096 16384 33554432
    net.ipv4.tcp_wmem = 4096 16384 33554432
    net.ipv4.tcp_syncookies = 1
    # this gives the kernel more memory for tcp which you need with many (100k+) open socket connections
    net.ipv4.tcp_mem = 786432 1048576 26777216
    net.ipv4.tcp_max_tw_buckets = 360000
    net.core.netdev_max_backlog = 2500
    vm.min_free_kbytes = 65536
    vm.swappiness = 0
    net.ipv4.ip_local_port_range = 1024 65535
    net.core.somaxconn = 65535

Setting added to /etc/security/limits.conf

    *                -       nofile          65535

Newer Linux distributions have ACPI control over CPU frequencies which may
ramp down the CPU speed during a loadtest.  On Ubuntu, you can use the
package "cpufrequtils" to set the criteria used for adjusting the CPU
frequency.  Usually, this involves setting the CPU governor to "performance".
Example configuration modifications are shown below for Ubuntu 12.04:

    cat > /etc/default/cpufrequtils <<EOF
    #!/bin/sh
    ENABLE="true"
    GOVERNOR="performance"
    MAX_SPEED="2201000"
    MIN_SPEED="2201000"
    EOF
    update-rc.d -f ondemand remove
    cat > /etc/default/loadcpufreq <<EOF
    #!/bin/sh
    ENABLE="false"
    EOF

If the CPU is automatically ramped down during the loadtest due to CPU
temperature guidelines (check with "cpufreq-info"), you may reset the original
settings with "service cpufrequtils restart".  Do not use "cpufreq-info"
during a loadtest, since it skews the results by causing abnormal latency.
If the CPU is automatically ramped down during the loadtest, the results
are considered invalid and will likely contain request latency spikes.

##RESULTS

###Summaries

* [CloudI version 1.4.0](https://github.com/CloudI/loadtests/raw/master/tests/http_req/loadtest/results_v1_4_0/201412_summary.pdf)
* [CloudI version 1.2.2](https://github.com/CloudI/loadtests/raw/master/tests/http_req/loadtest/results_v1_2_2/201306_summary.pdf)
* [CloudI version 1.2.1](https://github.com/CloudI/loadtests/raw/master/tests/http_req/loadtest/results_v1_2_1/201303_summary.pdf)
* [CloudI version 1.1.0](https://github.com/CloudI/loadtests/raw/master/tests/http_req/loadtest/results_v1_1_0/201210_summary.pdf)

###Tsung Output Explanation

[`loadtest/results_v1_4_0/`](https://github.com/CloudI/loadtests/raw/master/tests/http_req/loadtest/results_v1_4_0/201412_tsung_output.tar.bz2):
* compares default service configuration with the usage of the service configuration options `request_timeout_adjustment` and `response_timeout_adjustment` set to true (both default to false) as the "time-adj" variation
* compares external service configuration with tcp unix domain socket usage (`local` or `default` for the protocol service configuration option) and tcp inet socket usage (`tcp` for the protocol service configuration option) as the "time-adj" variation compared with the "tcp/time-adj" variation
* compares R16B03-1 usage with 17.4 usage along with the usage of Erlang VM configuration options (where arg1 is "+Muacul 0" and arg2 is "+secio true")

[`loadtest/results_v1_2_2/201306_20k_10kreqs_local/`](https://github.com/CloudI/loadtests/raw/master/tests/http_req/loadtest/results_v1_2_2/201306_tsung_output.tar.bz2):
* shows the default performance when using the `local` protocol (unix domain sockets) for external services without request or response timeout adjustment service options

[`loadtest/results_v1_2_2/201306_20k_10kreqs_local_with_request/`](https://github.com/CloudI/loadtests/raw/master/tests/http_req/loadtest/results_v1_2_2/201306_tsung_output.tar.bz2):
* shows the performance when using the `local` protocol (unix domain sockets) for external services with the request timeout adjustment service option

[`loadtest/results_v1_2_2/201306_20k_10kreqs_local_with_response/`](https://github.com/CloudI/loadtests/raw/master/tests/http_req/loadtest/results_v1_2_2/201306_tsung_output.tar.bz2):
* shows the performance when using the `local` protocol (unix domain sockets) for external services with the response timeout adjustment service option

[`loadtest/results_v1_2_2/201306_20k_10kreqs_tcp_1_2_1/`](https://github.com/CloudI/loadtests/raw/master/tests/http_req/loadtest/results_v1_2_2/201306_tsung_output.tar.bz2):
* shows the default performance when using the `tcp` protocol (inet sockets) for external services without request or response timeout adjustment service options in CloudI 1.2.1 running on Ubuntu 12.04.2

[`loadtest/results_v1_2_2/201306_20k_10kreqs_local_cpg_ets/`](https://github.com/CloudI/loadtests/raw/master/tests/http_req/loadtest/results_v1_2_2/201306_tsung_output.tar.bz2) and [`loadtest/results_v1_2_2/201306_20k_10kreqs_local_cpg_no_ets/`](https://github.com/CloudI/loadtests/raw/master/tests/http_req/loadtest/results_v1_2_2/201306_tsung_output.tar.bz2):
* Test an `immediate` destination refresh method with CPG using ETS to determine if ETS lowers CloudI request latency (it doesn't)

[`loadtest/results_v1_2_2/201306_4k_10kreqs/`](https://github.com/CloudI/loadtests/raw/master/tests/http_req/loadtest/results_v1_2_2/201306_tsung_output.tar.bz2) and [`loadtest/results_v1_2_2/201306_4k_20kreqs/`](https://github.com/CloudI/loadtests/raw/master/tests/http_req/loadtest/results_v1_2_2/201306_tsung_output.tar.bz2):
* Test to determine any throughput limit within CloudI (currently 10kreqs is difficult to exceed within a single CloudI node, probably because of the `erlang:now/0` function call for each request v1 UUID)

[`loadtest/results_v1_2_1/201303_20k_10kreqs_duo_with_request/`](https://github.com/CloudI/loadtests/raw/master/tests/http_req/loadtest/results_v1_2_1/201303_tsung_output.tar.bz2):
* shows the latency due to adjusting the request timeout based on the service's request handling latency, in CloudI version 1.2.1

[`loadtest/results_v1_2_1/201303_20k_10kreqs_duo_with_response/`](https://github.com/CloudI/loadtests/raw/master/tests/http_req/loadtest/results_v1_2_1/201303_tsung_output.tar.bz2):
* shows the latency due to adjusting the response timeout which incurs a smaller latency penalty, in CloudI version 1.2.1

[`loadtest/results_v1_2_1/201303_20k_10kreqs_duo_without_adjustment/`](https://github.com/CloudI/loadtests/raw/master/tests/http_req/loadtest/results_v1_2_1/201303_tsung_output.tar.bz2):
* shows the default cowboy configuration for CloudI version 1.2.1 and how it provides better performance than CloudI version 1.1.0 for CloudI API implementations in C/C++, Java, and Erlang

[`loadtest/results_v1_2_1/201303_20k_10kreqs_single_without_adjustment/`](https://github.com/CloudI/loadtests/raw/master/tests/http_req/loadtest/results_v1_2_1/201303_tsung_output.tar.bz2):
* used the default cowboy configuration from CloudI version 1.2.0 to show performance problems when relying on a `non-duo_mode` service, which was previously the default before CloudI version 1.2.0

[`loadtest/results_v1_1_0/201210_20k_10kreqs_misultin/`](https://github.com/CloudI/loadtests/raw/master/tests/http_req/loadtest/results_v1_1_0/201210_tsung_output.tar.bz2):
* same test as [`loadtest/results_v1_0_0/201206_20k_10kreqs/`](https://github.com/CloudI/loadtests/raw/master/tests/http_req/loadtest/results_v1_0_0/201206_tsung_output.tar.bz2), but with Erlang R15B02 and CloudI version 1.1.0

[`loadtest/results_v1_1_0/201210_20k_10kreqs_cowboy/`](https://github.com/CloudI/loadtests/raw/master/tests/http_req/loadtest/results_v1_1_0/201210_tsung_output.tar.bz2):
* used to compare [cowboy with misultin](https://github.com/CloudI/loadtests/raw/master/tests/http_req/loadtest/results_v1_1_0/201210_summary.pdf)

[`loadtest/results_v1_1_0/201210_40k_10kreqs_misultin/`](https://github.com/CloudI/loadtests/raw/master/tests/http_req/loadtest/results_v1_1_0/201210_tsung_output.tar.bz2) and [`loadtest/results_v1_1_0/201210_40k_10kreqs_cowboy/`](https://github.com/CloudI/loadtests/raw/master/tests/http_req/loadtest/results_v1_1_0/201210_tsung_output.tar.bz2):
* shows more latency with 40,000 concurrent connections open ( [summary](https://github.com/CloudI/loadtests/raw/master/tests/http_req/loadtest/results_v1_1_0/201210_summary.pdf) ) for external programming languages (i.e., any programming languages not running on the Erlang VM)

[`loadtest/results_v1_0_0/201206_20k_10kreqs/`](https://github.com/CloudI/loadtests/raw/master/tests/http_req/loadtest/results_v1_0_0/201206_tsung_output.tar.bz2):
* 20,000 concurrent connections open
* 10,000 requests/second maintained for 10 minutes
* each supported programming language tested separately to determine [cumulative latency due to load](http://cloudi.org/faq.html#5_LoadTesting)
* used `Ubuntu 12.04 LTS (GNU/Linux 3.2.0-20-generic x86_64)` with `Erlang R15B01`

##INFORMATION

Any confusion about how to do benchmarks should go here (httpref results during 1 minute on localhost are useless, but typical on the internet):
* [`http://www.mnot.net/blog/2011/05/18/http_benchmark_rules`](http://www.mnot.net/blog/2011/05/18/http_benchmark_rules)

Interesting historical connection count test:
* [`http://www.metabrew.com/article/a-million-user-comet-application-with-mochiweb-part-1`](http://www.metabrew.com/article/a-million-user-comet-application-with-mochiweb-part-1)
* [`http://www.metabrew.com/article/a-million-user-comet-application-with-mochiweb-part-2`](http://www.metabrew.com/article/a-million-user-comet-application-with-mochiweb-part-2)
* [`http://www.metabrew.com/article/a-million-user-comet-application-with-mochiweb-part-3`](http://www.metabrew.com/article/a-million-user-comet-application-with-mochiweb-part-3)
* [`http://blog.whatsapp.com/index.php/2012/01/1-million-is-so-2011/`](http://blog.whatsapp.com/index.php/2012/01/1-million-is-so-2011/)

The `http_req` source code uses the same XML request/response data used in
misultin HTTP server testing:
* [`http://www.ostinelli.net/a-comparison-between-misultin-mochiweb-cowboy-nodejs-and-tornadoweb/`](http://www.ostinelli.net/a-comparison-between-misultin-mochiweb-cowboy-nodejs-and-tornadoweb/)

