#CloudI Loadtesting

## ABOUT

The following requests are available (see `loadtest/results/*/setup/cloudi.conf`):

    http://localhost:8080/tests/http_req/erlang.xml?value=42
    http://localhost:8080/tests/http_req/c.xml?value=42
    http://localhost:8080/tests/http_req/java.xml?value=42
    http://localhost:8080/tests/http_req/python.xml?value=42
    http://localhost:8080/tests/http_req/python_c.xml?value=42
    http://localhost:8080/tests/http_req/ruby.xml?value=42

Which all give the following response, from the associated programming language:

    <http_test><value>42</value></http_test>

The loadtest's test data is based on the testing in
http://www.ostinelli.net/a-comparison-between-misultin-mochiweb-cowboy-nodejs-and-tornadoweb/ .  The test's task is simple usage of an XML response to a
HTTP GET request, which requires minimal processing in each programming
language.  The CloudI HTTP jobs that currently exist
are `cloudi_job_http_misultin` and `cloudi_job_http_cowboy`.
The loadtest results from the version 1.0.0 release only
used `cloudi_job_http_misultin`.  The CloudI loadtesting uses Tsung to
produce dependable loadtesting results
(see `loadtest/results_v*/*/setup/http_req_*.xml`).

##CONFIGURATION

The general software configuration files are in `loadtest/results_v*/*/setup/`

###Hardware

    Core i7 2670QM 2.2GHz 4 cores, 8 hyper-threads
    L2:4×256KB L3:6MB RAM:8GB:DDR3-1333MHz
    Sandy Bridge-HE-4 (Socket G2)
    Gigabit ethernet (Netgear JGS516)

###Software

    Ubuntu 12.04 LTS (GNU/Linux 3.2.0-20-generic x86_64)

    Erlang R15B01 configuration:
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
    # this gives the kernel more memory for tcp which you need with many (100k+) ope
    n socket connections
    net.ipv4.tcp_mem = 786432 1048576 26777216
    net.ipv4.tcp_max_tw_buckets = 360000
    net.core.netdev_max_backlog = 2500
    vm.min_free_kbytes = 65536
    vm.swappiness = 0
    net.ipv4.ip_local_port_range = 1024 65535
    net.core.somaxconn = 65535

Setting added to /etc/security/limits.conf

    *                -       nofile          65535

##RESULTS

[loadtest/results_v1_0_0/201206_20k_10kreqs/](https://github.com/okeuday/CloudI/tree/master/src/tests/http_req/loadtest/results_v1_0_0/201206_20k_10kreqs):
* 20,000 concurrent connections open
* 10,000 requests/second maintained for 10 minutes
* each supported programming language tested separately to determine [cumulative latency due to load](http://cloudi.org/faq.html#5_LoadTesting)
* used Ubuntu 12.04 LTS (GNU/Linux 3.2.0-20-generic x86_64) with Erlang R15B01
[loadtest/results_v1_1_0/201210_20k_10kreqs_misultin/](https://github.com/okeuday/CloudI/tree/master/src/tests/http_req/loadtest/results_v1_1_0/201210_20k_10kreqs_misultin):
* same test as [loadtest/results_v1_0_0/201206_20k_10kreqs/](https://github.com/okeuday/CloudI/tree/master/src/tests/http_req/loadtest/results_v1_0_0/201206_20k_10kreqs), but with Erlang R15B02 and CloudI version 1.1.0

[loadtest/results_v1_1_0/201210_20k_10kreqs_cowboy/](https://github.com/okeuday/CloudI/tree/master/src/tests/http_req/loadtest/results_v1_1_0/201210_20k_10kreqs_cowboy):
* used to compare [cowboy with misultin](https://github.com/okeuday/CloudI/tree/master/src/tests/http_req/loadtest/results_v1_1_0/201210_summary.pdf)

[loadtest/results_v1_1_0/201210_40k_10kreqs_misultin/](https://github.com/okeuday/CloudI/tree/master/src/tests/http_req/loadtest/results_v1_1_0/201210_40k_10kreqs_misultin) and [loadtest/results_v1_1_0/201210_40k_10kreqs_cowboy/](https://github.com/okeuday/CloudI/tree/master/src/tests/http_req/loadtest/results_v1_1_0/201210_40k_10kreqs_cowboy):
* shows more latency with 40,000 concurrent connections open ( [summary](https://github.com/okeuday/CloudI/tree/master/src/tests/http_req/loadtest/results_v1_1_0/201210_summary.pdf) ) for external programming languages (i.e., any programming languages not running on the Erlang VM)

##INFORMATION

Any confusion about how to do benchmarks should go here (httpref results during 1 minute on localhost are useless, but typical on the internet):
* http://www.mnot.net/blog/2011/05/18/http_benchmark_rules

interesting historical loadtest, only localhost usage... need more interfaces:
* http://www.metabrew.com/article/a-million-user-comet-application-with-mochiweb-part-1
* http://www.metabrew.com/article/a-million-user-comet-application-with-mochiweb-part-2
* http://www.metabrew.com/article/a-million-user-comet-application-with-mochiweb-part-3

