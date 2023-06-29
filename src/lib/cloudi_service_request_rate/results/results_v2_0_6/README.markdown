# 2.0.6 `cloudi_service_request_rate` Testing

## Hardware

    Dell PowerEdge R710
    Xeon X5650 2.66GHz 2 cpu, 6 core/cpu, 2 hts/core
    L1:384KB L2:1.5MB L3:12MB RAM:125GB:DDR3-1333MT/s
    Westmere-EP (LGA 1366/Socket B)

## Software

    Ubuntu 20.04.2 LTS (5.4.0-65-generic x86_64)
    Erlang 24.3.4.12
    CloudI 2.0.6 (69bad70deb1ba1d5cb783fe9b9f4839db99b0920 (v2.0.6))
    
    ATS2/Postiats 0.3.13
    GCC version 9.3.0 (Ubuntu 9.3.0-17ubuntu1~20.04)
    Go (go gc) 1.13.8 linux/amd64
    GHC 8.6.5
    Java OpenJDK 11.0.10 (build 11.0.10+9-Ubuntu-0ubuntu1.20.04)
    NodeJS v10.19.0
    OCaml 4.08.1
    Perl v5.30.0
    PHP 7.4.3 (Zend Engine v3.4.0)
    Python 2.7.18/3.8.5
    Ruby 2.7.0p0 (2019-12-25 revision 647ee6f091)
    rustc 1.69.0 (84c898d65 2023-04-16)

## Test Description

Use `cloudi_service_request_rate` to send service requests to the
`http_req` integration test written in a supported programming language.
The receiving service concurrency was always set to 12.
The `cloudi_service_request_rate` `count_process` configuration value
was set to 13 to have 12 sending processes with a separate data
collection process.  With 12 receivers and 12 senders all 24 hyperthreads
are utilized on this 12 core machine.  The request rate is sustained for
at least 2 minutes while no errors or timeouts occur during a 5 second
time period (each CloudI service request uses a 5 second timeout value).

The destination refresh method of the sender is set to `lazy` to avoid
contention for the scope cpg Erlang process.  The `duo_mode` service
configuration option of the sender is set to `true` for handling
higher throughput with 2 CloudI service Erlang processes
(instead of 1 CloudI service Erlang process).

The Linux kernel arguments included "mitigations=off" and the scheduler
was set to "none".

Erlang/OTP compilation was done with backwards compatible options:

    git clone --branch OTP-24.3.4.12 --depth 1 https://github.com/erlang/otp.git otp_src_24.3.4.12
    cd otp_src_24.3.4.12
    export ERL_TOP=`pwd`
    ./otp_build autoconf
    CFLAGS="-fPIE -O2 -g" CXXFLAGS="-fPIE -O2 -g" ./configure --enable-threads --enable-smp-support --enable-kernel-poll --disable-hipe --enable-dirty-schedulers --disable-erlang-mandir
    make
    make install

CloudI was installed with the default location (/usr/local) and the
configuration file (/usr/local/etc/cloudi/cloudi.conf) was empty.

    wget https://osdn.net/dl/cloudi/cloudi-2.0.6.tar.gz
    tar zxvf cloudi-2.0.6.tar.gz
    cd cloudi-2.0.6/src
    ./configure --enable-ats2-support --enable-go-support --enable-haskell-support --enable-ocaml-support --enable-rust-support --with-integration-tests-ran
    ./make_dev
    sudo ./make_dev install
    sudo mv /usr/local/etc/cloudi/cloudi.conf /usr/local/etc/cloudi/cloudi_tests.conf
    sudo touch /usr/local/etc/cloudi/cloudi.conf
    sudo sh -c 'echo -e "# default scheduler bind type\n+sbt db\n" >> /usr/local/etc/cloudi/cloudi.args'
    sudo cloudi start
    sudo cloudi attach

All tests were ran using the Erlang/OTP shell to call the
CloudI Service API with the `cloudi_service_api` module.
The configuration ran first is shown below:

    > TestConcurrency = 12.
    > Bind1 = "1,13,3,15,5,17,7,19,9,21,11,23".
    > Bind2 = "0,12,2,14,4,16,6,18,8,20,10,22,0".

When compared to the [2.0.5 prerelease testing](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_request_rate/results/results_v2_0_4#readme),
the bind service configuration option was used for the ATS, C and Erlang
services and Erlang/OTP 24.3.4.12 was used instead of Erlang/OTP 23.3.4.8.

## ATS2 processes

Execution:

    > {ok, [ATSReceiverID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {file_path, "/usr/local/lib/cloudi-2.0.6/tests/http_req/http_req_ats2"}, {dest_refresh, none}, {count_process, TestConcurrency}, {options, [{bind, Bind1}]}]], infinity).
    > {ok, [ATSSenderID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_request_rate}, {args, [{request_rate, dynamic}, {service_name, "/tests/http_req/ats2.xml/get"}]}, {dest_refresh, lazy_closest}, {count_process, TestConcurrency + 1}, {options, [{duo_mode, true}, {bind, Bind2}]}]], infinity).

    > ok = cloudi_service_api:services_remove([ATSSenderID, ATSReceiverID], infinity).

Log Output:

    stable at 40184.8 [40184.8 .. 40184.9] requests/second
      request latency (distribution undefined):
        mean                 1448089 us
        stddev                205071.47
        skewness                  -0.12 (approximately_symmetric)
        kurtosis                  -0.69 (platykurtic)
    
          0% (minimum)        947118 us
         10%                 1166283 us
         20%                 1257266 us
         30%                 1327013 us
         40%                 1394326 us
         50%                 1458326 us
         75%                 1610116 us
         80%                 1635036 us
         90%                 1708804 us
         95%                 1759727 us
         99%                 1875069 us
         99.9%               1954939 us
         99.99%              1993980 us
         99.999%             1994980 us
        100% (maximum)       1995031 us

## C processes

Execution:

    > {ok, [CReceiverID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {file_path, "/usr/local/lib/cloudi-2.0.6/tests/http_req/http_req_c"}, {dest_refresh, none}, {count_process, TestConcurrency}, {options, [{bind, Bind1}]}]], infinity).
    > {ok, [CSenderID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_request_rate}, {args, [{request_rate, dynamic}, {service_name, "/tests/http_req/c.xml/get"}]}, {dest_refresh, lazy_closest}, {count_process, TestConcurrency + 1}, {options, [{duo_mode, true}, {bind, Bind2}]}]], infinity).

    > ok = cloudi_service_api:services_remove([CSenderID, CReceiverID], infinity).

Log Output:

    stable at 44022.2 [44022.2 .. 44022.2] requests/second
      request latency (distribution uniform):
        mean                 1613587 us
        stddev                289961.29
        skewness                   0.05 (approximately_symmetric)
        kurtosis                  -1.12 (platykurtic)
    
          0% (minimum)        952209 us
         10%                 1244079 us
         20%                 1331171 us
         30%                 1415322 us
         40%                 1498677 us
         50%                 1592717 us
         75%                 1867514 us
         80%                 1936954 us
         90%                 2028320 us
         95%                 2066046 us
         99%                 2098860 us
         99.9%               2118345 us
         99.99%              2120819 us
         99.999%             2121285 us
        100% (maximum)       2121474 us

## Erlang

Execution:

    > ok = cloudi_service_api:code_path_add("/usr/local/lib/cloudi-2.0.6/tests/http_req/erlang/ebin", infinity).
    > {ok, [ErlangReceiverID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_test_http_req}, {dest_refresh, none}, {count_process, TestConcurrency}, {options, [{bind, Bind1}]}]], infinity).
    > {ok, [ErlangSenderID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_request_rate}, {args, [{request_rate, dynamic}, {service_name, "/tests/http_req/erlang.xml/get"}]}, {dest_refresh, lazy_closest}, {count_process, TestConcurrency + 1}, {options, [{duo_mode, true}, {bind, Bind2}]}]], infinity).

    > ok = cloudi_service_api:services_remove([ErlangSenderID, ErlangReceiverID], infinity).
    > ok = cloudi_service_api:code_path_remove("/usr/local/lib/cloudi-2.0.6/tests/http_req/erlang/ebin", infinity).

Log Output:

    stable at 108774.0 [108744.7 .. 108789.1] requests/second
      request latency (distribution undefined):
        mean                 1917948 us
        stddev                787800.98
        skewness                   0.22 (approximately_symmetric)
        kurtosis                  -0.94 (platykurtic)
    
          0% (minimum)        454546 us
         10%                  885928 us
         20%                 1068396 us
         30%                 1349497 us
         40%                 1645876 us
         50%                 1909549 us
         75%                 2503026 us
         80%                 2616286 us
         90%                 3017793 us
         95%                 3307404 us
         99%                 3587920 us
         99.9%               3690182 us
         99.99%              3704826 us
         99.999%             3706836 us
        100% (maximum)       3706995 us

## Go threads

Execution:

    > {ok, [GoThreadsReceiverID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {file_path, "/usr/local/lib/cloudi-2.0.6/tests/http_req/http_req_go"}, {dest_refresh, none}, {count_thread, TestConcurrency}]], infinity).
    > {ok, [GoThreadsSenderID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_request_rate}, {args, [{request_rate, dynamic}, {service_name, "/tests/http_req/go.xml/get"}]}, {dest_refresh, lazy_closest}, {count_process, TestConcurrency + 1}, {options, [{duo_mode, true}]}]], infinity).

    > ok = cloudi_service_api:services_remove([GoThreadsSenderID, GoThreadsReceiverID], infinity).

Log Output:

    stable at 40250.9 [40249.4 .. 40252.4] requests/second
      request latency (distribution uniform):
        mean                 1827395 us
        stddev                563662.10
        skewness                   0.21 (approximately_symmetric)
        kurtosis                  -1.27 (platykurtic)
    
          0% (minimum)        998013 us
         10%                 1122719 us
         20%                 1205208 us
         30%                 1360654 us
         40%                 1580732 us
         50%                 1784350 us
         75%                 2314805 us
         80%                 2418495 us
         90%                 2648476 us
         95%                 2763074 us
         99%                 2850463 us
         99.9%               2880247 us
         99.99%              2883658 us
         99.999%             2885081 us
        100% (maximum)       2885133 us

## Go processes

Execution:

    > {ok, [GoProcessesReceiverID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {file_path, "/usr/local/lib/cloudi-2.0.6/tests/http_req/http_req_go"}, {dest_refresh, none}, {count_process, TestConcurrency}]], infinity).
    > {ok, [GoProcessesSenderID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_request_rate}, {args, [{request_rate, dynamic}, {service_name, "/tests/http_req/go.xml/get"}]}, {dest_refresh, lazy_closest}, {count_process, TestConcurrency + 1}, {options, [{duo_mode, true}]}]], infinity).

    > ok = cloudi_service_api:services_remove([GoProcessesSenderID, GoProcessesReceiverID], infinity).

Log Output:

    stable at 42506.4 [42502.8 .. 42509.9] requests/second
      request latency (distribution undefined):
        mean                 1922745 us
        stddev                471669.12
        skewness                   0.53 (moderately_skewed)
        kurtosis                  -0.98 (platykurtic)
    
          0% (minimum)       1188960 us
         10%                 1386984 us
         20%                 1477156 us
         30%                 1571374 us
         40%                 1668900 us
         50%                 1779005 us
         75%                 2311146 us
         80%                 2427250 us
         90%                 2665376 us
         95%                 2785370 us
         99%                 2895081 us
         99.9%               2933699 us
         99.99%              2938241 us
         99.999%             2939098 us
        100% (maximum)       2939186 us

## Haskell threads

Execution:

    > {ok, [HaskellThreadsReceiverID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {file_path, "/usr/local/lib/cloudi-2.0.6/tests/http_req/http_req_haskell"}, {dest_refresh, none}, {count_thread, TestConcurrency}]], infinity).
    > {ok, [HaskellThreadsSenderID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_request_rate}, {args, [{request_rate, dynamic}, {service_name, "/tests/http_req/haskell.xml/get"}]}, {dest_refresh, lazy_closest}, {count_process, TestConcurrency + 1}, {options, [{duo_mode, true}]}]], infinity).

    > ok = cloudi_service_api:services_remove([HaskellThreadsSenderID, HaskellThreadsReceiverID], infinity).

Log Output:

    stable at 10336.7 [10336.3 .. 10337.1] requests/second
      request latency (distribution uniform):
        mean                 2218700 us
        stddev               1081070.79
        skewness                  -0.06 (approximately_symmetric)
        kurtosis                  -1.12 (platykurtic)
    
          0% (minimum)        259254 us
         10%                  680734 us
         20%                 1146893 us
         30%                 1509161 us
         40%                 1893146 us
         50%                 2228693 us
         75%                 3154238 us
         80%                 3346645 us
         90%                 3680814 us
         95%                 3899555 us
         99%                 4052691 us
         99.9%               4081006 us
         99.99%              4083748 us
         99.999%             4083931 us
        100% (maximum)       4083931 us

## Haskell processes

Execution:

    > {ok, [HaskellProcessesReceiverID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {file_path, "/usr/local/lib/cloudi-2.0.6/tests/http_req/http_req_haskell"}, {dest_refresh, none}, {count_process, TestConcurrency}]], infinity).
    > {ok, [HaskellProcessesSenderID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_request_rate}, {args, [{request_rate, dynamic}, {service_name, "/tests/http_req/haskell.xml/get"}]}, {dest_refresh, lazy_closest}, {count_process, TestConcurrency + 1}, {options, [{duo_mode, true}]}]], infinity).

    > ok = cloudi_service_api:services_remove([HaskellProcessesSenderID, HaskellProcessesReceiverID], infinity).

Log Output:

    stable at 41194.2 [41194.1 .. 41194.3] requests/second
      request latency (distribution undefined):
        mean                 1978981 us
        stddev                560838.24
        skewness                   0.31 (approximately_symmetric)
        kurtosis                  -1.27 (platykurtic)
    
          0% (minimum)       1137505 us
         10%                 1316491 us
         20%                 1409313 us
         30%                 1502410 us
         40%                 1649060 us
         50%                 1898097 us
         75%                 2474624 us
         80%                 2582170 us
         90%                 2805807 us
         95%                 2910358 us
         99%                 3056213 us
         99.9%               3098199 us
         99.99%              3100483 us
         99.999%             3101030 us
        100% (maximum)       3101148 us

## Java threads

Execution:

    > {ok, [JavaThreadsReceiverID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {file_path, "/usr/bin/java"}, {args, "-Dfile.encoding=UTF-8 -server -ea:org.cloudi... -jar /usr/local/lib/cloudi-2.0.6/tests/http_req/java/http_req.jar"}, {dest_refresh, none}, {count_thread, TestConcurrency}]], infinity).
    > {ok, [JavaThreadsSenderID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_request_rate}, {args, [{request_rate, dynamic}, {service_name, "/tests/http_req/java.xml/get"}]}, {dest_refresh, lazy_closest}, {count_process, TestConcurrency + 1}, {options, [{duo_mode, true}]}]], infinity).

    > ok = cloudi_service_api:services_remove([JavaThreadsSenderID, JavaThreadsReceiverID], infinity).

Log Output:

    stable at 38029.3 [38029.3 .. 38029.3] requests/second
      request latency (distribution undefined):
        mean                 1815827 us
        stddev                549360.70
        skewness                   0.52 (moderately_skewed)
        kurtosis                  -0.92 (platykurtic)
    
          0% (minimum)       1021663 us
         10%                 1196996 us
         20%                 1281181 us
         30%                 1364411 us
         40%                 1488287 us
         50%                 1716737 us
         75%                 2240719 us
         80%                 2350853 us
         90%                 2672606 us
         95%                 2806260 us
         99%                 3044766 us
         99.9%               3056151 us
         99.99%              3056750 us
         99.999%             3056995 us
        100% (maximum)       3057073 us

## Java processes

Execution:

    > {ok, [JavaProcessesReceiverID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {file_path, "/usr/bin/java"}, {args, "-Dfile.encoding=UTF-8 -server -ea:org.cloudi... -jar /usr/local/lib/cloudi-2.0.6/tests/http_req/java/http_req.jar"}, {dest_refresh, none}, {count_process, TestConcurrency}]], infinity).
    > {ok, [JavaProcessesSenderID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_request_rate}, {args, [{request_rate, dynamic}, {service_name, "/tests/http_req/java.xml/get"}]}, {dest_refresh, lazy_closest}, {count_process, TestConcurrency + 1}, {options, [{duo_mode, true}]}]], infinity).

    > ok = cloudi_service_api:services_remove([JavaProcessesSenderID, JavaProcessesReceiverID], infinity).

Log Output:

    stable at 39937.2 [39931.9 .. 39940.9] requests/second
      request latency (distribution undefined):
        mean                 1730985 us
        stddev                396023.36
        skewness                   0.16 (approximately_symmetric)
        kurtosis                  -0.54 (platykurtic)
    
          0% (minimum)        604103 us
         10%                 1297478 us
         20%                 1382556 us
         30%                 1471110 us
         40%                 1559851 us
         50%                 1650558 us
         75%                 2058758 us
         80%                 2141718 us
         90%                 2303460 us
         95%                 2409857 us
         99%                 2564675 us
         99.9%               2601681 us
         99.99%              2603183 us
         99.999%             2603681 us
        100% (maximum)       2603687 us

## JavaScript processes

Execution:

    > {ok, [JavaScriptReceiverID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {file_path, "/usr/bin/nodejs"}, {args, "/usr/local/lib/cloudi-2.0.6/tests/http_req/http_req.js"}, {env, [{"NODE_PATH", "/usr/local/lib/cloudi-2.0.6/api/javascript/"}]}, {dest_refresh, none}, {count_process, TestConcurrency}]], infinity).
    > {ok, [JavaScriptSenderID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_request_rate}, {args, [{request_rate, dynamic}, {service_name, "/tests/http_req/javascript.xml/get"}]}, {dest_refresh, lazy_closest}, {count_process, TestConcurrency + 1}, {options, [{duo_mode, true}]}]], infinity).

    > ok = cloudi_service_api:services_remove([JavaScriptSenderID, JavaScriptReceiverID], infinity).

Log Output:

    stable at 16934.6 [16934.2 .. 16935.0] requests/second
      request latency (distribution undefined):
        mean                  488769 us
        stddev                228763.96
        skewness                   0.40 (approximately_symmetric)
        kurtosis                  -0.35 (platykurtic)
    
          0% (minimum)         66132 us
         10%                  184283 us
         20%                  274696 us
         30%                  357039 us
         40%                  428231 us
         50%                  480300 us
         75%                  613618 us
         80%                  669363 us
         90%                  830961 us
         95%                  923986 us
         99%                 1023594 us
         99.9%               1126176 us
         99.99%              1131389 us
         99.999%             1132077 us
        100% (maximum)       1132113 us

## OCaml threads

Execution:

    > {ok, [OCamlThreadsReceiverID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {file_path, "/usr/local/lib/cloudi-2.0.6/tests/http_req/http_req_ocaml"}, {dest_refresh, none}, {count_thread, TestConcurrency}]], infinity).
    > {ok, [OCamlThreadsSenderID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_request_rate}, {args, [{request_rate, dynamic}, {service_name, "/tests/http_req/ocaml.xml/get"}]}, {dest_refresh, lazy_closest}, {count_process, TestConcurrency + 1}, {options, [{duo_mode, true}]}]], infinity).

    > ok = cloudi_service_api:services_remove([OCamlThreadsSenderID, OCamlThreadsReceiverID], infinity).

Log Output:

    stable at 20615.7 [20615.2 .. 20616.2] requests/second
      request latency (distribution undefined):
        mean                 1768486 us
        stddev                928359.08
        skewness                   0.32 (approximately_symmetric)
        kurtosis                  -1.17 (platykurtic)
    
          0% (minimum)        502124 us
         10%                  572389 us
         20%                  806166 us
         30%                 1078825 us
         40%                 1349441 us
         50%                 1615605 us
         75%                 2573946 us
         80%                 2772230 us
         90%                 3165306 us
         95%                 3356137 us
         99%                 3515043 us
         99.9%               3547119 us
         99.99%              3548792 us
         99.999%             3549033 us
        100% (maximum)       3549063 us

## OCaml processes

Execution:

    > {ok, [OCamlProcessesReceiverID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {file_path, "/usr/local/lib/cloudi-2.0.6/tests/http_req/http_req_ocaml"}, {dest_refresh, none}, {count_process, TestConcurrency}]], infinity).
    > {ok, [OCamlProcessesSenderID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_request_rate}, {args, [{request_rate, dynamic}, {service_name, "/tests/http_req/ocaml.xml/get"}]}, {dest_refresh, lazy_closest}, {count_process, TestConcurrency + 1}, {options, [{duo_mode, true}]}]], infinity).

    > ok = cloudi_service_api:services_remove([OCamlProcessesSenderID, OCamlProcessesReceiverID], infinity).

Log Output:

    stable at 40847.9 [40847.5 .. 40848.2] requests/second
      request latency (distribution undefined):
        mean                 1614580 us
        stddev                293690.88
        skewness                   0.47 (approximately_symmetric)
        kurtosis                  -0.87 (platykurtic)
    
          0% (minimum)       1094027 us
         10%                 1263673 us
         20%                 1336648 us
         30%                 1409074 us
         40%                 1480733 us
         50%                 1546700 us
         75%                 1853850 us
         80%                 1922114 us
         90%                 2057861 us
         95%                 2135888 us
         99%                 2277398 us
         99.9%               2301932 us
         99.99%              2304230 us
         99.999%             2304415 us
        100% (maximum)       2304515 us

## Perl threads

Execution:

    > {ok, [PerlThreadsReceiverID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {file_path, "/usr/bin/perl"}, {args, "/usr/local/lib/cloudi-2.0.6/tests/http_req/http_req.pl"}, {env, [{"PERL5LIB", "/usr/local/lib/cloudi-2.0.6/api/perl/"}]}, {dest_refresh, none}, {count_thread, TestConcurrency}]], infinity).
    > {ok, [PerlThreadsSenderID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_request_rate}, {args, [{request_rate, dynamic}, {service_name, "/tests/http_req/perl.xml/get"}]}, {dest_refresh, lazy_closest}, {count_process, TestConcurrency + 1}, {options, [{duo_mode, true}]}]], infinity).

    > ok = cloudi_service_api:services_remove([PerlThreadsSenderID, PerlThreadsReceiverID], infinity).

Log Output:

    stable at 20334.5 [20332.6 .. 20335.9] requests/second
      request latency (distribution uniform):
        mean                 2052299 us
        stddev               1011024.70
        skewness                   0.16 (approximately_symmetric)
        kurtosis                  -1.24 (platykurtic)
    
          0% (minimum)        570657 us
         10%                  690142 us
         20%                  969685 us
         30%                 1298140 us
         40%                 1654127 us
         50%                 2000200 us
         75%                 2915366 us
         80%                 3146351 us
         90%                 3510014 us
         95%                 3673755 us
         99%                 3883237 us
         99.9%               3955363 us
         99.99%              3963979 us
         99.999%             3964488 us
        100% (maximum)       3964556 us

## Perl processes

Execution:

    > {ok, [PerlProcessesReceiverID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {file_path, "/usr/bin/perl"}, {args, "/usr/local/lib/cloudi-2.0.6/tests/http_req/http_req.pl"}, {env, [{"PERL5LIB", "/usr/local/lib/cloudi-2.0.6/api/perl/"}]}, {dest_refresh, none}, {count_process, TestConcurrency}]], infinity).
    > {ok, [PerlProcessesSenderID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_request_rate}, {args, [{request_rate, dynamic}, {service_name, "/tests/http_req/perl.xml/get"}]}, {dest_refresh, lazy_closest}, {count_process, TestConcurrency + 1}, {options, [{duo_mode, true}]}]], infinity).

    > ok = cloudi_service_api:services_remove([PerlProcessesSenderID, PerlProcessesReceiverID], infinity).

Log Output:

    stable at 14745.1 [14744.7 .. 14745.4] requests/second
      request latency (distribution uniform):
        mean                 1474965 us
        stddev                636462.40
        skewness                   0.04 (approximately_symmetric)
        kurtosis                  -1.34 (platykurtic)
    
          0% (minimum)        489384 us
         10%                  614964 us
         20%                  713507 us
         30%                 1019070 us
         40%                 1214464 us
         50%                 1467586 us
         75%                 2076131 us
         80%                 2176973 us
         90%                 2327699 us
         95%                 2422205 us
         99%                 2581109 us
         99.9%               2708827 us
         99.99%              2720012 us
         99.999%             2720358 us
        100% (maximum)       2720365 us

## PHP processes

Execution:

    > {ok, [PHPReceiverID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {file_path, "/usr/bin/php"}, {args, "-d zend.assertions=1 -d include_path='/usr/local/lib/cloudi-2.0.6/api/php/' -f /usr/local/lib/cloudi-2.0.6/tests/http_req/http_req.php"}, {dest_refresh, none}, {count_process, TestConcurrency}]], infinity).
    > {ok, [PHPSenderID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_request_rate}, {args, [{request_rate, dynamic}, {service_name, "/tests/http_req/php.xml/get"}]}, {dest_refresh, lazy_closest}, {count_process, TestConcurrency + 1}, {options, [{duo_mode, true}]}]], infinity).

    > ok = cloudi_service_api:services_remove([PHPSenderID, PHPReceiverID], infinity).

Log Output:

    stable at 37883.2 [37883.2 .. 37883.2] requests/second
      request latency (distribution uniform):
        mean                 1933229 us
        stddev                586771.16
        skewness                   0.23 (approximately_symmetric)
        kurtosis                  -1.30 (platykurtic)
    
          0% (minimum)       1089253 us
         10%                 1219194 us
         20%                 1308099 us
         30%                 1400919 us
         40%                 1634560 us
         50%                 1901927 us
         75%                 2426471 us
         80%                 2538367 us
         90%                 2773380 us
         95%                 2909227 us
         99%                 3031928 us
         99.9%               3078731 us
         99.99%              3082477 us
         99.999%             3082826 us
        100% (maximum)       3082841 us

## Python2 threads

Execution:

    > {ok, [Python2ThreadsReceiverID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {file_path, "/usr/bin/python2"}, {args, "/usr/local/lib/cloudi-2.0.6/tests/http_req/http_req.py"}, {env, [{"PYTHONPATH", "/usr/local/lib/cloudi-2.0.6/api/python/"}, {"LANG", "en_US.UTF-8"}]}, {dest_refresh, none}, {count_thread, TestConcurrency}]], infinity).
    > {ok, [Python2ThreadsSenderID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_request_rate}, {args, [{request_rate, dynamic}, {service_name, "/tests/http_req/python.xml/get"}]}, {dest_refresh, lazy_closest}, {count_process, TestConcurrency + 1}, {options, [{duo_mode, true}]}]], infinity).

    > ok = cloudi_service_api:services_remove([Python2ThreadsSenderID, Python2ThreadsReceiverID], infinity).

Log Output:

    stable at 3391.0 [3366.7 .. 3395.7] requests/second
      request latency (distribution uniform):
        mean                 2482480 us
        stddev               1416632.29
        skewness                   0.00 (approximately_symmetric)
        kurtosis                  -1.20 (platykurtic)
    
          0% (minimum)         76937 us
         10%                  519920 us
         20%                 1013932 us
         30%                 1504274 us
         40%                 1991922 us
         50%                 2477816 us
         75%                 3709026 us
         80%                 3955177 us
         90%                 4453601 us
         95%                 4697720 us
         99%                 4876144 us
         99.9%               4908713 us
         99.99%              4911405 us
         99.999%             4911542 us
        100% (maximum)       4911542 us

## Python3 threads

Execution:

    > {ok, [Python3ThreadsReceiverID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {file_path, "/usr/bin/python3"}, {args, "/usr/local/lib/cloudi-2.0.6/tests/http_req/http_req.py"}, {env, [{"PYTHONPATH", "/usr/local/lib/cloudi-2.0.6/api/python/"}, {"LANG", "en_US.UTF-8"}]}, {dest_refresh, none}, {count_thread, TestConcurrency}]], infinity).
    > {ok, [Python3ThreadsSenderID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_request_rate}, {args, [{request_rate, dynamic}, {service_name, "/tests/http_req/python.xml/get"}]}, {dest_refresh, lazy_closest}, {count_process, TestConcurrency + 1}, {options, [{duo_mode, true}]}]], infinity).

    > ok = cloudi_service_api:services_remove([Python3ThreadsSenderID, Python3ThreadsReceiverID], infinity).

Log Output:

    stable at 4707.5 [4698.3 .. 4709.7] requests/second
      request latency (distribution uniform):
        mean                 2507548 us
        stddev               1402321.92
        skewness                  -0.05 (approximately_symmetric)
        kurtosis                  -1.19 (platykurtic)
    
          0% (minimum)        108924 us
         10%                  529602 us
         20%                 1038655 us
         30%                 1563319 us
         40%                 2062666 us
         50%                 2535247 us
         75%                 3720888 us
         80%                 3959058 us
         90%                 4431928 us
         95%                 4656476 us
         99%                 4837672 us
         99.9%               4878263 us
         99.99%              4883783 us
         99.999%             4887363 us
        100% (maximum)       4887363 us

## Python2 processes

Execution:

    > {ok, [Python2ProcessesReceiverID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {file_path, "/usr/bin/python2"}, {args, "/usr/local/lib/cloudi-2.0.6/tests/http_req/http_req.py"}, {env, [{"PYTHONPATH", "/usr/local/lib/cloudi-2.0.6/api/python/"}, {"LANG", "en_US.UTF-8"}]}, {dest_refresh, none}, {count_process, TestConcurrency}]], infinity).
    > {ok, [Python2ProcessesSenderID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_request_rate}, {args, [{request_rate, dynamic}, {service_name, "/tests/http_req/python.xml/get"}]}, {dest_refresh, lazy_closest}, {count_process, TestConcurrency + 1}, {options, [{duo_mode, true}]}]], infinity).

    > ok = cloudi_service_api:services_remove([Python2ProcessesSenderID, Python2ProcessesReceiverID], infinity).

Log Output:

    stable at 26385.7 [26351.5 .. 26391.4] requests/second
      request latency (distribution uniform):
        mean                 2125169 us
        stddev                933506.80
        skewness                   0.10 (approximately_symmetric)
        kurtosis                  -1.23 (platykurtic)
    
          0% (minimum)        737264 us
         10%                  862546 us
         20%                 1045886 us
         30%                 1433000 us
         40%                 1812453 us
         50%                 2146003 us
         75%                 2894541 us
         80%                 3081932 us
         90%                 3457420 us
         95%                 3646457 us
         99%                 3770993 us
         99.9%               3854508 us
         99.99%              3858021 us
         99.999%             3858696 us
        100% (maximum)       3858712 us

## Python3 processes

Execution:

    > {ok, [Python3ProcessesReceiverID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {file_path, "/usr/bin/python3"}, {args, "/usr/local/lib/cloudi-2.0.6/tests/http_req/http_req.py"}, {env, [{"PYTHONPATH", "/usr/local/lib/cloudi-2.0.6/api/python/"}, {"LANG", "en_US.UTF-8"}]}, {dest_refresh, none}, {count_process, TestConcurrency}]], infinity).
    > {ok, [Python3ProcessesSenderID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_request_rate}, {args, [{request_rate, dynamic}, {service_name, "/tests/http_req/python.xml/get"}]}, {dest_refresh, lazy_closest}, {count_process, TestConcurrency + 1}, {options, [{duo_mode, true}]}]], infinity).

    > ok = cloudi_service_api:services_remove([Python3ProcessesSenderID, Python3ProcessesReceiverID], infinity).

Log Output:

    stable at 29371.3 [29370.6 .. 29372.0] requests/second
      request latency (distribution undefined):
        mean                 2017089 us
        stddev                813333.50
        skewness                   0.14 (approximately_symmetric)
        kurtosis                  -1.38 (platykurtic)
    
          0% (minimum)        855435 us
         10%                  989318 us
         20%                 1070723 us
         30%                 1330163 us
         40%                 1674487 us
         50%                 1994340 us
         75%                 2758169 us
         80%                 2920571 us
         90%                 3161756 us
         95%                 3284838 us
         99%                 3408859 us
         99.9%               3433663 us
         99.99%              3439582 us
         99.999%             3440340 us
        100% (maximum)       3440392 us

## Ruby threads

Execution:

    > {ok, [RubyThreadsReceiverID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {file_path, "/usr/bin/ruby"}, {args, "/usr/local/lib/cloudi-2.0.6/tests/http_req/http_req.rb"}, {env, [{"RUBYLIB", "/usr/local/lib/cloudi-2.0.6/api/ruby/"}]}, {dest_refresh, none}, {count_thread, TestConcurrency}]], infinity).
    > {ok, [RubyThreadsSenderID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_request_rate}, {args, [{request_rate, dynamic}, {service_name, "/tests/http_req/ruby.xml/get"}]}, {dest_refresh, lazy_closest}, {count_process, TestConcurrency + 1}, {options, [{duo_mode, true}]}]], infinity).

    > ok = cloudi_service_api:services_remove([RubyThreadsSenderID, RubyThreadsReceiverID], infinity).

Log Output:

    stable at 3473.8 [3416.5 .. 3482.6] requests/second
      request latency (distribution uniform):
        mean                 2399031 us
        stddev               1360914.16
        skewness                  -0.01 (approximately_symmetric)
        kurtosis                  -1.19 (platykurtic)
    
          0% (minimum)         84759 us
         10%                  505433 us
         20%                  985757 us
         30%                 1463922 us
         40%                 1935881 us
         50%                 2409773 us
         75%                 3572757 us
         80%                 3804253 us
         90%                 4278667 us
         95%                 4518381 us
         99%                 4713854 us
         99.9%               4755270 us
         99.99%              4758607 us
         99.999%             4758631 us
        100% (maximum)       4758631 us

## Ruby processes

Execution:

    > {ok, [RubyProcessesReceiverID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {file_path, "/usr/bin/ruby"}, {args, "/usr/local/lib/cloudi-2.0.6/tests/http_req/http_req.rb"}, {env, [{"RUBYLIB", "/usr/local/lib/cloudi-2.0.6/api/ruby/"}]}, {dest_refresh, none}, {count_process, TestConcurrency}]], infinity).
    > {ok, [RubyProcessesSenderID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_request_rate}, {args, [{request_rate, dynamic}, {service_name, "/tests/http_req/ruby.xml/get"}]}, {dest_refresh, lazy_closest}, {count_process, TestConcurrency + 1}, {options, [{duo_mode, true}]}]], infinity).

    > ok = cloudi_service_api:services_remove([RubyProcessesSenderID, RubyProcessesReceiverID], infinity).

Log Output:

    stable at 19379.1 [19378.9 .. 19379.4] requests/second
      request latency (distribution uniform):
        mean                 1469526 us
        stddev                566323.44
        skewness                  -0.11 (approximately_symmetric)
        kurtosis                  -1.27 (platykurtic)
    
          0% (minimum)        576002 us
         10%                  650949 us
         20%                  782761 us
         30%                 1081329 us
         40%                 1333678 us
         50%                 1529612 us
         75%                 1946426 us
         80%                 2037161 us
         90%                 2221026 us
         95%                 2315331 us
         99%                 2393030 us
         99.9%               2433742 us
         99.99%              2438403 us
         99.999%             2439236 us
        100% (maximum)       2439270 us

## Rust threads

Execution:

    > {ok, [RustThreadsReceiverID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {file_path, "/usr/local/lib/cloudi-2.0.6/tests/http_req/http_req_rust"}, {env, [{"RUST_BACKTRACE", "full"}]}, {dest_refresh, none}, {count_thread, TestConcurrency}]], infinity).
    > {ok, [RustThreadsSenderID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_request_rate}, {args, [{request_rate, dynamic}, {service_name, "/tests/http_req/rust.xml/get"}]}, {dest_refresh, lazy_closest}, {count_process, TestConcurrency + 1}, {options, [{duo_mode, true}]}]], infinity).

    > ok = cloudi_service_api:services_remove([RustThreadsSenderID, RustThreadsReceiverID], infinity).

Log Output:

    stable at 36993.0 [36993.0 .. 36993.0] requests/second
      request latency (distribution uniform):
        mean                 2054881 us
        stddev                743910.64
        skewness                   0.23 (approximately_symmetric)
        kurtosis                  -1.34 (platykurtic)
    
          0% (minimum)       1013795 us
         10%                 1145905 us
         20%                 1225932 us
         30%                 1423063 us
         40%                 1696578 us
         50%                 1982312 us
         75%                 2729298 us
         80%                 2873207 us
         90%                 3142367 us
         95%                 3254725 us
         99%                 3360250 us
         99.9%               3404969 us
         99.99%              3410459 us
         99.999%             3411211 us
        100% (maximum)       3411346 us

## Rust processes

Execution:

    > {ok, [RustProcessesReceiverID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {file_path, "/usr/local/lib/cloudi-2.0.6/tests/http_req/http_req_rust"}, {env, [{"RUST_BACKTRACE", "full"}]}, {dest_refresh, none}, {count_process, TestConcurrency}]], infinity).
    > {ok, [RustProcessesSenderID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_request_rate}, {args, [{request_rate, dynamic}, {service_name, "/tests/http_req/rust.xml/get"}]}, {dest_refresh, lazy_closest}, {count_process, TestConcurrency + 1}, {options, [{duo_mode, true}]}]], infinity).

    > ok = cloudi_service_api:services_remove([RustProcessesSenderID, RustProcessesReceiverID], infinity).

Log Output:

    stable at 37068.4 [37068.4 .. 37068.4] requests/second
      request latency (distribution uniform):
        mean                 1927264 us
        stddev                587346.21
        skewness                   0.21 (approximately_symmetric)
        kurtosis                  -1.34 (platykurtic)
    
          0% (minimum)       1070343 us
         10%                 1204629 us
         20%                 1292017 us
         30%                 1393728 us
         40%                 1663032 us
         50%                 1884767 us
         75%                 2456693 us
         80%                 2572563 us
         90%                 2761273 us
         95%                 2864559 us
         99%                 2983245 us
         99.9%               3008163 us
         99.99%              3010523 us
         99.999%             3010789 us
        100% (maximum)       3010847 us

## Throughput Summary

     108774.0 requests/second       Erlang
      44022.2 requests/second       C processes
      42506.4 requests/second       Go processes
      41194.2 requests/second       Haskell processes
      40847.9 requests/second       OCaml processes
      40250.9 requests/second       Go threads
      40184.8 requests/second       ATS2 processes
      39937.2 requests/second       Java processes
      38029.3 requests/second       Java threads
      37883.2 requests/second       PHP processes
      37068.4 requests/second       Rust processes
      36993.0 requests/second       Rust threads
      29371.3 requests/second       Python3 processes
      26385.7 requests/second       Python2 processes
      20615.7 requests/second       OCaml threads
      20334.5 requests/second       Perl threads
      19379.1 requests/second       Ruby processes
      16934.6 requests/second       JavaScript processes
      14745.1 requests/second       Perl processes
      10336.7 requests/second       Haskell threads
       4707.5 requests/second       Python3 threads
       3473.8 requests/second       Ruby threads
       3391.0 requests/second       Python2 threads

## Latency Summary

### Mean

     2507548 us                    Python3 threads
     2482480 us                    Python2 threads
     2399031 us                    Ruby threads
     2218700 us                    Haskell threads
     2125169 us                    Python2 processes
     2054881 us                    Rust threads
     2052299 us                    Perl threads
     2017089 us                    Python3 processes
     1768486 us                    OCaml threads
     1978981 us                    Haskell processes
     1933229 us                    PHP processes
     1927264 us                    Rust processes
     1922745 us                    Go processes
     1917948 us                    Erlang
     1827395 us                    Go threads
     1815827 us                    Java threads
     1730985 us                    Java processes
     1614580 us                    OCaml processes
     1613587 us                    C processes
     1474965 us                    Perl processes
     1469526 us                    Ruby processes
     1448089 us                    ATS2 processes
      488769 us                    JavaScript processes

### Median

     2535247 us                    Python3 threads
     2477816 us                    Python2 threads
     2409773 us                    Ruby threads
     2228693 us                    Haskell threads
     2146003 us                    Python2 processes
     2000200 us                    Perl threads
     1994340 us                    Python3 processes
     1982312 us                    Rust threads
     1909549 us                    Erlang
     1901927 us                    PHP processes
     1898097 us                    Haskell processes
     1884767 us                    Rust processes
     1784350 us                    Go threads
     1779005 us                    Go processes
     1716737 us                    Java threads
     1650558 us                    Java processes
     1615605 us                    OCaml threads
     1592717 us                    C processes
     1546700 us                    OCaml processes
     1529612 us                    Ruby processes
     1467586 us                    Perl processes
     1458326 us                    ATS2 processes
      480300 us                    JavaScript processes

### Maximum

     4911542 us                    Python2 threads
     4887363 us                    Python3 threads
     4758631 us                    Ruby threads
     4083931 us                    Haskell threads
     3964556 us                    Perl threads
     3858712 us                    Python2 processes
     3706995 us                    Erlang
     3549063 us                    OCaml threads
     3440392 us                    Python3 processes
     3411346 us                    Rust threads
     3101148 us                    Haskell processes
     3082841 us                    PHP processes
     3057073 us                    Java threads
     3010847 us                    Rust processes
     2939186 us                    Go processes
     2885133 us                    Go threads
     2720365 us                    Perl processes
     2603687 us                    Java processes
     2439270 us                    Ruby processes
     2304515 us                    OCaml processes
     2121474 us                    C processes
     1995031 us                    ATS2 processes
     1132113 us                    JavaScript processes

