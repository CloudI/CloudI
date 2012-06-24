#CloudI Loadtesting

## ABOUT

The following requests are available (see loadtest/setup/cloudi.conf):
http://localhost:8080/tests/http_req/erlang.xml?value=42
http://localhost:8080/tests/http_req/c.xml?value=42
http://localhost:8080/tests/http_req/java.xml?value=42
http://localhost:8080/tests/http_req/python.xml?value=42
http://localhost:8080/tests/http_req/ruby.xml?value=42

Which all give the following response, from the associated programming language:
<http_test><value>42</value></http_test>

The loadtest is test data is based on the testing in
http://www.ostinelli.net/a-comparison-between-misultin-mochiweb-cowboy-nodejs-and-tornadoweb/ .
Misultin is used within CloudI currently, but the results of the previous
misultin testing is only loosely related to CloudI loadtesting.
The CloudI loadtesting is using Tsung instead of httperf, so that it is
easier to get more formalized loadtest information.
(see loadtest/setup/http_req_*.xml).

##CONFIGURATION

The general software configuration files are in loadtest/setup/

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
