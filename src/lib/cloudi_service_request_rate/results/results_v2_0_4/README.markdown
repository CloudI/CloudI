# 2.0.4 `cloudi_service_request_rate` Testing

## Hardware

    Dell PowerEdge R710
    Xeon X5650 2.66GHz 2 cpu, 6 core/cpu, 2 hts/core
    L1:384KB L2:1.5MB L3:12MB RAM:72GB:DDR3-1333MT/s
    Westmere-EP (LGA 1366/Socket B)

## Software

    Ubuntu 20.04.2 LTS (5.4.0-65-generic x86_64)
    Erlang 23.3.4.8
    CloudI 2.0.4 (e0ba57df711b15a0e41790e4999a81134df1cfac on 2022-08-24)
    
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
    
    wrk 4.2.0

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

Integration tests compilation used -O0 for C/C++ compilation.

All tests were ran using the Erlang/OTP shell to call the
CloudI Service API with the `cloudi_service_api` module.
The configuration ran first is shown below:

    > TestConcurrency = 12.

## ATS2 processes

Execution:

    > {ok, [ATSReceiverID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {file_path, "/usr/local/lib/cloudi-2.0.4/tests/http_req/http_req_ats2"}, {dest_refresh, none}, {count_process, TestConcurrency}]], infinity).
    > {ok, [ATSSenderID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_request_rate}, {args, [{request_rate, dynamic}, {service_name, "/tests/http_req/ats2.xml/get"}]}, {dest_refresh, lazy_closest}, {count_process, TestConcurrency + 1}, {options, [{duo_mode, true}]}]], infinity).

    > ok = cloudi_service_api:services_remove([ATSSenderID, ATSReceiverID], infinity).

Log Output:

    stable at 42534.9 [42534.9 .. 42534.9] requests/second
      request latency (distribution undefined):
        mean                 1847774 us
        stddev                348580.45
        skewness                   0.62 (moderately_skewed)
        kurtosis                  -0.28 (platykurtic)
    
          0% (minimum)       1252603 us
         10%                 1429193 us
         20%                 1527072 us
         30%                 1619972 us
         40%                 1711703 us
         50%                 1797586 us
         75%                 2062046 us
         80%                 2162432 us
         90%                 2366781 us
         95%                 2521561 us
         99%                 2792762 us
         99.9%               2819852 us
         99.99%              2823322 us
         99.999%             2823738 us
        100% (maximum)       2823842 us

## C processes

Execution:

    > {ok, [CReceiverID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {file_path, "/usr/local/lib/cloudi-2.0.4/tests/http_req/http_req_c"}, {dest_refresh, none}, {count_process, TestConcurrency}]], infinity).
    > {ok, [CSenderID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_request_rate}, {args, [{request_rate, dynamic}, {service_name, "/tests/http_req/c.xml/get"}]}, {dest_refresh, lazy_closest}, {count_process, TestConcurrency + 1}, {options, [{duo_mode, true}]}]], infinity).

    > ok = cloudi_service_api:services_remove([CSenderID, CReceiverID], infinity).

Log Output:

    stable at 36388.2 [36388.2 .. 36388.2] requests/second
      request latency (distribution undefined):
        mean                 1587514 us
        stddev                266024.55
        skewness                   0.53 (moderately_skewed)
        kurtosis                  -0.35 (platykurtic)
    
          0% (minimum)       1086625 us
         10%                 1257991 us
         20%                 1335990 us
         30%                 1413916 us
         40%                 1482434 us
         50%                 1558375 us
         75%                 1755614 us
         80%                 1809430 us
         90%                 1979327 us
         95%                 2074102 us
         99%                 2313087 us
         99.9%               2331823 us
         99.99%              2334448 us
         99.999%             2334721 us
        100% (maximum)       2334732 us

## Erlang

Execution:

    > ok = cloudi_service_api:code_path_add("/usr/local/lib/cloudi-2.0.4/tests/http_req/erlang/ebin", infinity).
    > {ok, [ErlangReceiverID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_test_http_req}, {dest_refresh, none}, {count_process, TestConcurrency}]], infinity).
    > {ok, [ErlangSenderID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_request_rate}, {args, [{request_rate, dynamic}, {service_name, "/tests/http_req/erlang.xml/get"}]}, {dest_refresh, lazy_closest}, {count_process, TestConcurrency + 1}, {options, [{duo_mode, true}]}]], infinity).

    > ok = cloudi_service_api:services_remove([ErlangSenderID, ErlangReceiverID], infinity).
    > ok = cloudi_service_api:code_path_remove("/usr/local/lib/cloudi-2.0.4/tests/http_req/erlang/ebin", infinity).

Log Output:

    stable at 80951.3 [80930.7 .. 80965.6] requests/second
      request latency (distribution undefined):
        mean                 2061989 us
        stddev                580233.96
        skewness                   0.58 (moderately_skewed)
        kurtosis                  -1.05 (platykurtic)
    
          0% (minimum)       1206338 us
         10%                 1470893 us
         20%                 1505340 us
         30%                 1558604 us
         40%                 1652240 us
         50%                 1889406 us
         75%                 2556097 us
         80%                 2693246 us
         90%                 2965931 us
         95%                 3109815 us
         99%                 3288127 us
         99.9%               3404160 us
         99.99%              3419044 us
         99.999%             3424507 us
        100% (maximum)       3425465 us

## Go threads

Execution:

    > {ok, [GoThreadsReceiverID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {file_path, "/usr/local/lib/cloudi-2.0.4/tests/http_req/http_req_go"}, {dest_refresh, none}, {count_thread, TestConcurrency}]], infinity).
    > {ok, [GoThreadsSenderID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_request_rate}, {args, [{request_rate, dynamic}, {service_name, "/tests/http_req/go.xml/get"}]}, {dest_refresh, lazy_closest}, {count_process, TestConcurrency + 1}, {options, [{duo_mode, true}]}]], infinity).

    > ok = cloudi_service_api:services_remove([GoThreadsSenderID, GoThreadsReceiverID], infinity).

Log Output:

    stable at 32440.4 [32440.1 .. 32440.8] requests/second
      request latency (distribution undefined):
        mean                 1485560 us
        stddev                344114.56
        skewness                  -0.23 (approximately_symmetric)
        kurtosis                  -0.83 (platykurtic)
    
          0% (minimum)        688011 us
         10%                  993535 us
         20%                 1199789 us
         30%                 1287427 us
         40%                 1370996 us
         50%                 1458034 us
         75%                 1807816 us
         80%                 1856434 us
         90%                 1941269 us
         95%                 1979125 us
         99%                 2025718 us
         99.9%               2052975 us
         99.99%              2055057 us
         99.999%             2055787 us
        100% (maximum)       2055952 us

## Go processes

Execution:

    > {ok, [GoProcessesReceiverID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {file_path, "/usr/local/lib/cloudi-2.0.4/tests/http_req/http_req_go"}, {dest_refresh, none}, {count_process, TestConcurrency}]], infinity).
    > {ok, [GoProcessesSenderID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_request_rate}, {args, [{request_rate, dynamic}, {service_name, "/tests/http_req/go.xml/get"}]}, {dest_refresh, lazy_closest}, {count_process, TestConcurrency + 1}, {options, [{duo_mode, true}]}]], infinity).

    > ok = cloudi_service_api:services_remove([GoProcessesSenderID, GoProcessesReceiverID], infinity).

Log Output:

    stable at 34119.4 [34118.3 .. 34120.4] requests/second
      request latency (distribution undefined):
        mean                 1663841 us
        stddev                288011.92
        skewness                   0.20 (approximately_symmetric)
        kurtosis                  -1.01 (platykurtic)
    
          0% (minimum)       1115858 us
         10%                 1286318 us
         20%                 1376329 us
         30%                 1465169 us
         40%                 1558894 us
         50%                 1646570 us
         75%                 1891173 us
         80%                 1951370 us
         90%                 2087160 us
         95%                 2162044 us
         99%                 2221226 us
         99.9%               2243026 us
         99.99%              2245528 us
         99.999%             2246710 us
        100% (maximum)       2246821 us

## Haskell threads

Execution:

    > {ok, [HaskellThreadsReceiverID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {file_path, "/usr/local/lib/cloudi-2.0.4/tests/http_req/http_req_haskell"}, {dest_refresh, none}, {count_thread, TestConcurrency}]], infinity).
    > {ok, [HaskellThreadsSenderID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_request_rate}, {args, [{request_rate, dynamic}, {service_name, "/tests/http_req/haskell.xml/get"}]}, {dest_refresh, lazy_closest}, {count_process, TestConcurrency + 1}, {options, [{duo_mode, true}]}]], infinity).

    > ok = cloudi_service_api:services_remove([HaskellThreadsSenderID, HaskellThreadsReceiverID], infinity).

Log Output:

    stable at 12623.0 [12556.4 .. 12626.7] requests/second
      request latency (distribution uniform):
        mean                 2283409 us
        stddev               1096960.09
        skewness                   0.04 (approximately_symmetric)
        kurtosis                  -1.14 (platykurtic)
    
          0% (minimum)        422564 us
         10%                  786452 us
         20%                 1156870 us
         30%                 1538315 us
         40%                 1883187 us
         50%                 2249625 us
         75%                 3221364 us
         80%                 3421750 us
         90%                 3823454 us
         95%                 4046378 us
         99%                 4212660 us
         99.9%               4257449 us
         99.99%              4260214 us
         99.999%             4260375 us
        100% (maximum)       4260375 us

## Haskell processes

Execution:

    > {ok, [HaskellProcessesReceiverID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {file_path, "/usr/local/lib/cloudi-2.0.4/tests/http_req/http_req_haskell"}, {dest_refresh, none}, {count_process, TestConcurrency}]], infinity).
    > {ok, [HaskellProcessesSenderID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_request_rate}, {args, [{request_rate, dynamic}, {service_name, "/tests/http_req/haskell.xml/get"}]}, {dest_refresh, lazy_closest}, {count_process, TestConcurrency + 1}, {options, [{duo_mode, true}]}]], infinity).

    > ok = cloudi_service_api:services_remove([HaskellProcessesSenderID, HaskellProcessesReceiverID], infinity).

Log Output:

    stable at 38555.8 [38555.8 .. 38555.8] requests/second
      request latency (distribution undefined):
        mean                 1869989 us
        stddev                346130.19
        skewness                   0.57 (moderately_skewed)
        kurtosis                  -0.57 (platykurtic)
    
          0% (minimum)       1284892 us
         10%                 1461207 us
         20%                 1549097 us
         30%                 1635620 us
         40%                 1718535 us
         50%                 1810230 us
         75%                 2102078 us
         80%                 2204488 us
         90%                 2410753 us
         95%                 2516396 us
         99%                 2702115 us
         99.9%               2775099 us
         99.99%              2780363 us
         99.999%             2781033 us
        100% (maximum)       2781188 us

## Java threads

Execution:

    > {ok, [JavaThreadsReceiverID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {file_path, "/usr/bin/java"}, {args, "-Dfile.encoding=UTF-8 -server -ea:org.cloudi... -jar /usr/local/lib/cloudi-2.0.4/tests/http_req/java/http_req.jar"}, {dest_refresh, none}, {count_thread, TestConcurrency}]], infinity).
    > {ok, [JavaThreadsSenderID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_request_rate}, {args, [{request_rate, dynamic}, {service_name, "/tests/http_req/java.xml/get"}]}, {dest_refresh, lazy_closest}, {count_process, TestConcurrency + 1}, {options, [{duo_mode, true}]}]], infinity).

    > ok = cloudi_service_api:services_remove([JavaThreadsSenderID, JavaThreadsReceiverID], infinity).

Log Output:

    stable at 38816.6 [38816.6 .. 38816.6] requests/second
      request latency (distribution undefined):
        mean                 1764623 us
        stddev                200904.20
        skewness                  -0.12 (approximately_symmetric)
        kurtosis                  -1.02 (platykurtic)
    
          0% (minimum)       1324697 us
         10%                 1477319 us
         20%                 1566641 us
         30%                 1638926 us
         40%                 1709832 us
         50%                 1771334 us
         75%                 1933984 us
         80%                 1965436 us
         90%                 2029198 us
         95%                 2065768 us
         99%                 2123425 us
         99.9%               2178042 us
         99.99%              2179122 us
         99.999%             2179382 us
        100% (maximum)       2179465 us

## Java processes

Execution:

    > {ok, [JavaProcessesReceiverID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {file_path, "/usr/bin/java"}, {args, "-Dfile.encoding=UTF-8 -server -ea:org.cloudi... -jar /usr/local/lib/cloudi-2.0.4/tests/http_req/java/http_req.jar"}, {dest_refresh, none}, {count_process, TestConcurrency}]], infinity).
    > {ok, [JavaProcessesSenderID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_request_rate}, {args, [{request_rate, dynamic}, {service_name, "/tests/http_req/java.xml/get"}]}, {dest_refresh, lazy_closest}, {count_process, TestConcurrency + 1}, {options, [{duo_mode, true}]}]], infinity).

    > ok = cloudi_service_api:services_remove([JavaProcessesSenderID, JavaProcessesReceiverID], infinity).

Log Output:

    stable at 40433.0 [40433.0 .. 40433.0] requests/second
      request latency (distribution undefined):
        mean                 1872209 us
        stddev                275338.30
        skewness                   0.10 (approximately_symmetric)
        kurtosis                  -0.89 (platykurtic)
    
          0% (minimum)       1239319 us
         10%                 1500137 us
         20%                 1600284 us
         30%                 1691013 us
         40%                 1778356 us
         50%                 1870969 us
         75%                 2084093 us
         80%                 2132264 us
         90%                 2236783 us
         95%                 2337259 us
         99%                 2450899 us
         99.9%               2468554 us
         99.99%              2471907 us
         99.999%             2472284 us
        100% (maximum)       2472441 us

## JavaScript processes

Execution:

    > {ok, [JavaScriptReceiverID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {file_path, "/usr/bin/nodejs"}, {args, "/usr/local/lib/cloudi-2.0.4/tests/http_req/http_req.js"}, {env, [{"NODE_PATH", "/usr/local/lib/cloudi-2.0.4/api/javascript/"}]}, {dest_refresh, none}, {count_process, TestConcurrency}]], infinity).
    > {ok, [JavaScriptSenderID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_request_rate}, {args, [{request_rate, dynamic}, {service_name, "/tests/http_req/javascript.xml/get"}]}, {dest_refresh, lazy_closest}, {count_process, TestConcurrency + 1}, {options, [{duo_mode, true}]}]], infinity).

    > ok = cloudi_service_api:services_remove([JavaScriptSenderID, JavaScriptReceiverID], infinity).

Log Output:

    stable at 21574.8 [21574.1 .. 21575.3] requests/second
      request latency (distribution uniform):
        mean                 2043822 us
        stddev                894876.18
        skewness                   0.12 (approximately_symmetric)
        kurtosis                  -1.29 (platykurtic)
    
          0% (minimum)        728662 us
         10%                  860387 us
         20%                 1023014 us
         30%                 1354177 us
         40%                 1694274 us
         50%                 2022723 us
         75%                 2824655 us
         80%                 2992648 us
         90%                 3314769 us
         95%                 3465446 us
         99%                 3636885 us
         99.9%               3696028 us
         99.99%              3698695 us
         99.999%             3701430 us
        100% (maximum)       3701576 us

## OCaml threads

Execution:

    > {ok, [OCamlThreadsReceiverID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {file_path, "/usr/local/lib/cloudi-2.0.4/tests/http_req/http_req_ocaml"}, {dest_refresh, none}, {count_thread, TestConcurrency}]], infinity).
    > {ok, [OCamlThreadsSenderID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_request_rate}, {args, [{request_rate, dynamic}, {service_name, "/tests/http_req/ocaml.xml/get"}]}, {dest_refresh, lazy_closest}, {count_process, TestConcurrency + 1}, {options, [{duo_mode, true}]}]], infinity).

    > ok = cloudi_service_api:services_remove([OCamlThreadsSenderID, OCamlThreadsReceiverID], infinity).

Log Output:

    stable at 17761.0 [17760.8 .. 17761.1] requests/second
      request latency (distribution uniform):
        mean                 2091896 us
        stddev               1079084.06
        skewness                   0.07 (approximately_symmetric)
        kurtosis                  -1.26 (platykurtic)
    
          0% (minimum)        471101 us
         10%                  557649 us
         20%                  924155 us
         30%                 1322635 us
         40%                 1684800 us
         50%                 2068717 us
         75%                 3043880 us
         80%                 3232302 us
         90%                 3608108 us
         95%                 3794531 us
         99%                 3934415 us
         99.9%               3963231 us
         99.99%              3965258 us
         99.999%             3965487 us
        100% (maximum)       3965488 us

## OCaml processes

Execution:

    > {ok, [OCamlProcessesReceiverID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {file_path, "/usr/local/lib/cloudi-2.0.4/tests/http_req/http_req_ocaml"}, {dest_refresh, none}, {count_process, TestConcurrency}]], infinity).
    > {ok, [OCamlProcessesSenderID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_request_rate}, {args, [{request_rate, dynamic}, {service_name, "/tests/http_req/ocaml.xml/get"}]}, {dest_refresh, lazy_closest}, {count_process, TestConcurrency + 1}, {options, [{duo_mode, true}]}]], infinity).

    > ok = cloudi_service_api:services_remove([OCamlProcessesSenderID, OCamlProcessesReceiverID], infinity).

Log Output:

    stable at 35443.1 [35443.1 .. 35443.1] requests/second
      request latency (distribution uniform):
        mean                 1408975 us
        stddev                186770.39
        skewness                  -0.06 (approximately_symmetric)
        kurtosis                  -1.08 (platykurtic)
    
          0% (minimum)       1010675 us
         10%                 1143418 us
         20%                 1220282 us
         30%                 1287684 us
         40%                 1354902 us
         50%                 1415581 us
         75%                 1566218 us
         80%                 1598275 us
         90%                 1660769 us
         95%                 1695666 us
         99%                 1744304 us
         99.9%               1771130 us
         99.99%              1780165 us
         99.999%             1781093 us
        100% (maximum)       1781117 us

## Perl threads

Execution:

    > {ok, [PerlThreadsReceiverID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {file_path, "/usr/bin/perl"}, {args, "/usr/local/lib/cloudi-2.0.4/tests/http_req/http_req.pl"}, {env, [{"PERL5LIB", "/usr/local/lib/cloudi-2.0.4/api/perl/"}]}, {dest_refresh, none}, {count_thread, TestConcurrency}]], infinity).
    > {ok, [PerlThreadsSenderID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_request_rate}, {args, [{request_rate, dynamic}, {service_name, "/tests/http_req/perl.xml/get"}]}, {dest_refresh, lazy_closest}, {count_process, TestConcurrency + 1}, {options, [{duo_mode, true}]}]], infinity).

    > ok = cloudi_service_api:services_remove([PerlThreadsSenderID, PerlThreadsReceiverID], infinity).

Log Output:

    stable at 23293.3 [22854.3 .. 23344.0] requests/second
      request latency (distribution undefined):
        mean                 2088324 us
        stddev                948292.97
        skewness                   0.29 (approximately_symmetric)
        kurtosis                  -1.20 (platykurtic)
    
          0% (minimum)        791426 us
         10%                  923167 us
         20%                 1010598 us
         30%                 1334352 us
         40%                 1643340 us
         50%                 1965962 us
         75%                 2908182 us
         80%                 3097457 us
         90%                 3454186 us
         95%                 3697793 us
         99%                 3931672 us
         99.9%               3994796 us
         99.99%              4028056 us
         99.999%             4044525 us
        100% (maximum)       4044620 us

## Perl processes

Execution:

    > {ok, [PerlProcessesReceiverID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {file_path, "/usr/bin/perl"}, {args, "/usr/local/lib/cloudi-2.0.4/tests/http_req/http_req.pl"}, {env, [{"PERL5LIB", "/usr/local/lib/cloudi-2.0.4/api/perl/"}]}, {dest_refresh, none}, {count_process, TestConcurrency}]], infinity).
    > {ok, [PerlProcessesSenderID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_request_rate}, {args, [{request_rate, dynamic}, {service_name, "/tests/http_req/perl.xml/get"}]}, {dest_refresh, lazy_closest}, {count_process, TestConcurrency + 1}, {options, [{duo_mode, true}]}]], infinity).

    > ok = cloudi_service_api:services_remove([PerlProcessesSenderID, PerlProcessesReceiverID], infinity).

Log Output:

    stable at 22960.5 [22956.6 .. 22962.3] requests/second
      request latency (distribution uniform):
        mean                 2008084 us
        stddev                836556.66
        skewness                   0.20 (approximately_symmetric)
        kurtosis                  -1.28 (platykurtic)
    
          0% (minimum)        792495 us
         10%                  957605 us
         20%                 1043753 us
         30%                 1334653 us
         40%                 1650676 us
         50%                 1957721 us
         75%                 2737289 us
         80%                 2902770 us
         90%                 3221792 us
         95%                 3367723 us
         99%                 3522150 us
         99.9%               3613549 us
         99.99%              3626311 us
         99.999%             3626955 us
        100% (maximum)       3627020 us

## PHP processes

Execution:

    > {ok, [PHPReceiverID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {file_path, "/usr/bin/php"}, {args, "-d zend.assertions=1 -d include_path='/usr/local/lib/cloudi-2.0.4/api/php/' -f /usr/local/lib/cloudi-2.0.4/tests/http_req/http_req.php"}, {dest_refresh, none}, {count_process, TestConcurrency}]], infinity).
    > {ok, [PHPSenderID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_request_rate}, {args, [{request_rate, dynamic}, {service_name, "/tests/http_req/php.xml/get"}]}, {dest_refresh, lazy_closest}, {count_process, TestConcurrency + 1}, {options, [{duo_mode, true}]}]], infinity).

    > ok = cloudi_service_api:services_remove([PHPSenderID, PHPReceiverID], infinity).

Log Output:

    stable at 36974.9 [36974.9 .. 36974.9] requests/second
      request latency (distribution undefined):
        mean                 1873441 us
        stddev                473751.44
        skewness                   0.47 (approximately_symmetric)
        kurtosis                  -1.06 (platykurtic)
    
          0% (minimum)       1127796 us
         10%                 1328794 us
         20%                 1416750 us
         30%                 1508924 us
         40%                 1608655 us
         50%                 1743160 us
         75%                 2276164 us
         80%                 2383880 us
         90%                 2597616 us
         95%                 2717351 us
         99%                 2852793 us
         99.9%               2898289 us
         99.99%              2902592 us
         99.999%             2903055 us
        100% (maximum)       2903178 us

## Python2 threads

Execution:

    > {ok, [Python2ThreadsReceiverID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {file_path, "/usr/bin/python2"}, {args, "/usr/local/lib/cloudi-2.0.4/tests/http_req/http_req.py"}, {env, [{"PYTHONPATH", "/usr/local/lib/cloudi-2.0.4/api/python/"}, {"LANG", "en_US.UTF-8"}]}, {dest_refresh, none}, {count_thread, TestConcurrency}]], infinity).
    > {ok, [Python2ThreadsSenderID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_request_rate}, {args, [{request_rate, dynamic}, {service_name, "/tests/http_req/python.xml/get"}]}, {dest_refresh, lazy_closest}, {count_process, TestConcurrency + 1}, {options, [{duo_mode, true}]}]], infinity).

    > ok = cloudi_service_api:services_remove([Python2ThreadsSenderID, Python2ThreadsReceiverID], infinity).

Log Output:

    stable at 3517.4 [3516.9 .. 3517.7] requests/second
      request latency (distribution uniform):
        mean                 2447157 us
        stddev               1397148.47
        skewness                   0.00 (approximately_symmetric)
        kurtosis                  -1.21 (platykurtic)
    
          0% (minimum)        102203 us
         10%                  510238 us
         20%                  996157 us
         30%                 1477777 us
         40%                 1960091 us
         50%                 2444363 us
         75%                 3662332 us
         80%                 3904493 us
         90%                 4381339 us
         95%                 4626464 us
         99%                 4813250 us
         99.9%               4843347 us
         99.99%              4845469 us
         99.999%             4845645 us
        100% (maximum)       4845645 us

## Python3 threads

Execution:

    > {ok, [Python3ThreadsReceiverID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {file_path, "/usr/bin/python3"}, {args, "/usr/local/lib/cloudi-2.0.4/tests/http_req/http_req.py"}, {env, [{"PYTHONPATH", "/usr/local/lib/cloudi-2.0.4/api/python/"}, {"LANG", "en_US.UTF-8"}]}, {dest_refresh, none}, {count_thread, TestConcurrency}]], infinity).
    > {ok, [Python3ThreadsSenderID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_request_rate}, {args, [{request_rate, dynamic}, {service_name, "/tests/http_req/python.xml/get"}]}, {dest_refresh, lazy_closest}, {count_process, TestConcurrency + 1}, {options, [{duo_mode, true}]}]], infinity).

    > ok = cloudi_service_api:services_remove([Python3ThreadsSenderID, Python3ThreadsReceiverID], infinity).

Log Output:

    stable at 4676.4 [4676.3 .. 4676.4] requests/second
      request latency (distribution uniform):
        mean                 2356205 us
        stddev               1341943.53
        skewness                   0.00 (approximately_symmetric)
        kurtosis                  -1.21 (platykurtic)
    
          0% (minimum)        140961 us
         10%                  485862 us
         20%                  958389 us
         30%                 1420593 us
         40%                 1889323 us
         50%                 2355693 us
         75%                 3522245 us
         80%                 3753870 us
         90%                 4214720 us
         95%                 4444780 us
         99%                 4632612 us
         99.9%               4671014 us
         99.99%              4672001 us
         99.999%             4672162 us
        100% (maximum)       4672162 us

## Python2 processes

Execution:

    > {ok, [Python2ProcessesReceiverID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {file_path, "/usr/bin/python2"}, {args, "/usr/local/lib/cloudi-2.0.4/tests/http_req/http_req.py"}, {env, [{"PYTHONPATH", "/usr/local/lib/cloudi-2.0.4/api/python/"}, {"LANG", "en_US.UTF-8"}]}, {dest_refresh, none}, {count_process, TestConcurrency}]], infinity).
    > {ok, [Python2ProcessesSenderID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_request_rate}, {args, [{request_rate, dynamic}, {service_name, "/tests/http_req/python.xml/get"}]}, {dest_refresh, lazy_closest}, {count_process, TestConcurrency + 1}, {options, [{duo_mode, true}]}]], infinity).

    > ok = cloudi_service_api:services_remove([Python2ProcessesSenderID, Python2ProcessesReceiverID], infinity).

Log Output:

    stable at 25671.6 [25671.4 .. 25671.7] requests/second
      request latency (distribution uniform):
        mean                 2115914 us
        stddev                896116.76
        skewness                   0.16 (approximately_symmetric)
        kurtosis                  -1.30 (platykurtic)
    
          0% (minimum)        763964 us
         10%                  977729 us
         20%                 1073842 us
         30%                 1373181 us
         40%                 1742207 us
         50%                 2092815 us
         75%                 2888150 us
         80%                 3050798 us
         90%                 3405677 us
         95%                 3564192 us
         99%                 3727283 us
         99.9%               3782082 us
         99.99%              3786525 us
         99.999%             3787193 us
        100% (maximum)       3787213 us

## Python3 processes

Execution:

    > {ok, [Python3ProcessesReceiverID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {file_path, "/usr/bin/python3"}, {args, "/usr/local/lib/cloudi-2.0.4/tests/http_req/http_req.py"}, {env, [{"PYTHONPATH", "/usr/local/lib/cloudi-2.0.4/api/python/"}, {"LANG", "en_US.UTF-8"}]}, {dest_refresh, none}, {count_process, TestConcurrency}]], infinity).
    > {ok, [Python3ProcessesSenderID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_request_rate}, {args, [{request_rate, dynamic}, {service_name, "/tests/http_req/python.xml/get"}]}, {dest_refresh, lazy_closest}, {count_process, TestConcurrency + 1}, {options, [{duo_mode, true}]}]], infinity).

    > ok = cloudi_service_api:services_remove([Python3ProcessesSenderID, Python3ProcessesReceiverID], infinity).

Log Output:

    stable at 26962.9 [26962.4 .. 26963.4] requests/second
      request latency (distribution undefined):
        mean                 1788484 us
        stddev                657765.85
        skewness                   0.25 (approximately_symmetric)
        kurtosis                  -1.32 (platykurtic)
    
          0% (minimum)        810465 us
         10%                 1005463 us
         20%                 1091448 us
         30%                 1214353 us
         40%                 1439098 us
         50%                 1724140 us
         75%                 2369443 us
         80%                 2498005 us
         90%                 2756998 us
         95%                 2868865 us
         99%                 2969984 us
         99.9%               3017512 us
         99.99%              3019380 us
         99.999%             3020281 us
        100% (maximum)       3020348 us

## Python3/C threads

Execution:

    > {ok, [PythonC3ThreadsReceiverID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {file_path, "/usr/bin/python3"}, {args, "/usr/local/lib/cloudi-2.0.4/tests/http_req/http_req_c.py"}, {env, [{"PYTHONPATH", "/usr/local/lib/cloudi-2.0.4/api/python/"}, {"LANG", "en_US.UTF-8"}]}, {dest_refresh, none}, {count_thread, TestConcurrency}]], infinity).
    > {ok, [PythonC3ThreadsSenderID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_request_rate}, {args, [{request_rate, dynamic}, {service_name, "/tests/http_req/python_c.xml/get"}]}, {dest_refresh, lazy_closest}, {count_process, TestConcurrency + 1}, {options, [{duo_mode, true}]}]], infinity).

    > ok = cloudi_service_api:services_remove([PythonC3ThreadsSenderID, PythonC3ThreadsReceiverID], infinity).

Log Output:

    stable at 4642.6 [4605.8 .. 4645.5] requests/second
      request latency (distribution uniform):
        mean                 2419548 us
        stddev               1343207.76
        skewness                  -0.06 (approximately_symmetric)
        kurtosis                  -1.19 (platykurtic)
    
          0% (minimum)        128990 us
         10%                  511201 us
         20%                 1028088 us
         30%                 1508462 us
         40%                 1991872 us
         50%                 2455484 us
         75%                 3580876 us
         80%                 3794087 us
         90%                 4244215 us
         95%                 4473941 us
         99%                 4665508 us
         99.9%               4703210 us
         99.99%              4706445 us
         99.999%             4706564 us
        100% (maximum)       4706564 us

## Python3/C processes

Execution:

    > {ok, [PythonC3ProcessesReceiverID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {file_path, "/usr/bin/python3"}, {args, "/usr/local/lib/cloudi-2.0.4/tests/http_req/http_req_c.py"}, {env, [{"PYTHONPATH", "/usr/local/lib/cloudi-2.0.4/api/python/"}, {"LANG", "en_US.UTF-8"}]}, {dest_refresh, none}, {count_process, TestConcurrency}]], infinity).
    > {ok, [PythonC3ProcessesSenderID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_request_rate}, {args, [{request_rate, dynamic}, {service_name, "/tests/http_req/python_c.xml/get"}]}, {dest_refresh, lazy_closest}, {count_process, TestConcurrency + 1}, {options, [{duo_mode, true}]}]], infinity).

    > ok = cloudi_service_api:services_remove([PythonC3ProcessesSenderID, PythonC3ProcessesReceiverID], infinity).

Log Output:

    stable at 27882.9 [27882.9 .. 27882.9] requests/second
      request latency (distribution undefined):
        mean                 1892512 us
        stddev                716319.54
        skewness                   0.30 (approximately_symmetric)
        kurtosis                  -1.26 (platykurtic)
    
          0% (minimum)        906345 us
         10%                 1038392 us
         20%                 1117257 us
         30%                 1250414 us
         40%                 1541128 us
         50%                 1825899 us
         75%                 2509564 us
         80%                 2657208 us
         90%                 2959613 us
         95%                 3098550 us
         99%                 3206073 us
         99.9%               3291274 us
         99.99%              3294934 us
         99.999%             3295641 us
        100% (maximum)       3295650 us

## Ruby threads

Execution:

    > {ok, [RubyThreadsReceiverID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {file_path, "/usr/bin/ruby"}, {args, "/usr/local/lib/cloudi-2.0.4/tests/http_req/http_req.rb"}, {env, [{"RUBYLIB", "/usr/local/lib/cloudi-2.0.4/api/ruby/"}]}, {dest_refresh, none}, {count_thread, TestConcurrency}]], infinity).
    > {ok, [RubyThreadsSenderID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_request_rate}, {args, [{request_rate, dynamic}, {service_name, "/tests/http_req/ruby.xml/get"}]}, {dest_refresh, lazy_closest}, {count_process, TestConcurrency + 1}, {options, [{duo_mode, true}]}]], infinity).

    > ok = cloudi_service_api:services_remove([RubyThreadsSenderID, RubyThreadsReceiverID], infinity).

Log Output:

    stable at 3512.2 [3481.6 .. 3516.7] requests/second
      request latency (distribution uniform):
        mean                 2401081 us
        stddev               1371217.52
        skewness                   0.01 (approximately_symmetric)
        kurtosis                  -1.20 (platykurtic)
    
          0% (minimum)        104429 us
         10%                  500273 us
         20%                  975175 us
         30%                 1451276 us
         40%                 1927911 us
         50%                 2397755 us
         75%                 3583566 us
         80%                 3820914 us
         90%                 4303618 us
         95%                 4550097 us
         99%                 4750008 us
         99.9%               4788989 us
         99.99%              4793782 us
         99.999%             4794190 us
        100% (maximum)       4794190 us

## Ruby processes

Execution:

    > {ok, [RubyProcessesReceiverID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {file_path, "/usr/bin/ruby"}, {args, "/usr/local/lib/cloudi-2.0.4/tests/http_req/http_req.rb"}, {env, [{"RUBYLIB", "/usr/local/lib/cloudi-2.0.4/api/ruby/"}]}, {dest_refresh, none}, {count_process, TestConcurrency}]], infinity).
    > {ok, [RubyProcessesSenderID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_request_rate}, {args, [{request_rate, dynamic}, {service_name, "/tests/http_req/ruby.xml/get"}]}, {dest_refresh, lazy_closest}, {count_process, TestConcurrency + 1}, {options, [{duo_mode, true}]}]], infinity).

    > ok = cloudi_service_api:services_remove([RubyProcessesSenderID, RubyProcessesReceiverID], infinity).

Log Output:

    stable at 24639.7 [24639.7 .. 24639.7] requests/second
      request latency (distribution uniform):
        mean                 2040994 us
        stddev                857632.57
        skewness                   0.18 (approximately_symmetric)
        kurtosis                  -1.36 (platykurtic)
    
          0% (minimum)        835044 us
         10%                  975493 us
         20%                 1062675 us
         30%                 1306294 us
         40%                 1651114 us
         50%                 1986536 us
         75%                 2820055 us
         80%                 2968426 us
         90%                 3258215 us
         95%                 3411415 us
         99%                 3541038 us
         99.9%               3634345 us
         99.99%              3646810 us
         99.999%             3647598 us
        100% (maximum)       3647660 us

## Throughput Summary

    80951.3 requests/second       Erlang
    42534.9 requests/second       ATS2 processes
    40433.0 requests/second       Java processes
    38816.6 requests/second       Java threads
    38555.8 requests/second       Haskell processes
    36974.9 requests/second       PHP processes
    36388.2 requests/second       C processes
    35443.1 requests/second       OCaml processes
    34119.4 requests/second       Go processes
    32440.4 requests/second       Go threads
    27882.9 requests/second       Python3/C processes
    26962.9 requests/second       Python3 processes
    25671.6 requests/second       Python2 processes
    24639.7 requests/second       Ruby processes
    23293.3 requests/second       Perl threads
    22960.5 requests/second       Perl processes
    21574.8 requests/second       JavaScript processes
    17761.0 requests/second       OCaml threads
    12623.0 requests/second       Haskell threads
     4676.4 requests/second       Python3 threads
     4642.6 requests/second       Python3/C threads
     3517.4 requests/second       Python2 threads
     3512.2 requests/second       Ruby threads

## Latency Summary

### Mean

    2447157 us                    Python2 threads
    2419548 us                    Python3/C threads
    2401081 us                    Ruby threads
    2356205 us                    Python3 threads
    2283409 us                    Haskell threads
    2115914 us                    Python2 processes
    2091896 us                    OCaml threads
    2088324 us                    Perl threads
    2061989 us                    Erlang
    2043822 us                    JavaScript processes
    2040994 us                    Ruby processes
    2008084 us                    Perl processes
    1892512 us                    Python3/C processes
    1873441 us                    PHP processes
    1872209 us                    Java processes
    1869989 us                    Haskell processes
    1847774 us                    ATS2 processes
    1788484 us                    Python3 processes
    1764623 us                    Java threads
    1663841 us                    Go processes
    1587514 us                    C processes
    1485560 us                    Go threads
    1408975 us                    OCaml processes

### Median

    2455484 us                    Python3/C threads
    2444363 us                    Python2 threads
    2397755 us                    Ruby threads
    2355693 us                    Python3 threads
    2249625 us                    Haskell threads
    2092815 us                    Python2 processes
    2068717 us                    OCaml threads
    2022723 us                    JavaScript processes
    1986536 us                    Ruby processes
    1965962 us                    Perl threads
    1957721 us                    Perl processes
    1889406 us                    Erlang
    1870969 us                    Java processes
    1825899 us                    Python3/C processes
    1810230 us                    Haskell processes
    1797586 us                    ATS2 processes
    1771334 us                    Java threads
    1743160 us                    PHP processes
    1724140 us                    Python3 processes
    1646570 us                    Go processes
    1558375 us                    C processes
    1458034 us                    Go threads
    1415581 us                    OCaml processes

### Maximum

    4845645 us                    Python2 threads
    4794190 us                    Ruby threads
    4706564 us                    Python3/C threads
    4672162 us                    Python3 threads
    4260375 us                    Haskell threads
    4044620 us                    Perl threads
    3965488 us                    OCaml threads
    3787213 us                    Python2 processes
    3701576 us                    JavaScript processes
    3647660 us                    Ruby processes
    3627020 us                    Perl processes
    3425465 us                    Erlang
    3295650 us                    Python3/C processes
    3020348 us                    Python3 processes
    2903178 us                    PHP processes
    2823842 us                    ATS2 processes
    2781188 us                    Haskell processes
    2472441 us                    Java processes
    2334732 us                    C processes
    2246821 us                    Go processes
    2179465 us                    Java threads
    2055952 us                    Go threads
    1781117 us                    OCaml processes

## HTTP Server Comparison

For comparison [wrk](https://github/wg/wrk) was used to send HTTP requests
through `cloudi_service_http_cowboy` from a remote machine.

Execution:

    > ok = cloudi_service_api:code_path_add("/usr/local/lib/cloudi-2.0.4/tests/http_req/erlang/ebin", infinity).
    > {ok, [HTTPReceiverID]} = cloudi_service_api:services_add([[{prefix, "/tests/http_req/"}, {module, cloudi_service_test_http_req}, {dest_refresh, none}, {count_process, TestConcurrency}]], infinity).
    > {ok, [HTTPSenderID]} = cloudi_service_api:services_add([[{module, cloudi_service_http_cowboy}, {args, [{ip, {0, 0, 0, 0}}, {port, 6464}]}, {dest_refresh, lazy_closest}, {count_process, TestConcurrency}, {options, [{duo_mode, true}]}]], infinity).

    $ ./wrk --connections 144 --duration 5m --threads 12 --script ./percentiles.lua http://machine:6464/tests/http_req/erlang.xml
    $ ./wrk --connections 12 --duration 5m --threads 12 --script ./percentiles.lua http://machine:6464/tests/http_req/erlang.xml

    > ok = cloudi_service_api:services_remove([HTTPSenderID, HTTPReceiverID], infinity).
    > ok = cloudi_service_api:code_path_remove("/usr/local/lib/cloudi-2.0.4/tests/http_req/erlang/ebin", infinity).

Shell Output:

    Running 5m test @ http://machine:6464/tests/http_req/erlang.xml
      12 threads and 144 connections
      Thread Stats   Avg      Stdev     Max   +/- Stdev
        Latency     2.03ms    0.92ms  31.76ms   78.42%
        Req/Sec     6.01k   180.77    11.99k    85.71%
      21540630 requests in 5.00m, 3.76GB read
    Requests/sec:  71778.23
    Transfer/sec:     12.81MB
    
         mean                   2026 us
           0% (minimum)          307 us
          10%                   1124 us
          20%                   1324 us
          30%                   1498 us
          40%                   1669 us
          50%                   1851 us
          75%                   2424 us
          80%                   2594 us
          90%                   3112 us
          95%                   3633 us
          99%                   4914 us
        99.9%                   9250 us
       99.99%                  16458 us
      99.999%                  24898 us
         100% (maximum)        31757 us

    Running 5m test @ http://machine:6464/tests/http_req/erlang.xml
      12 threads and 12 connections
      Thread Stats   Avg      Stdev     Max   +/- Stdev
        Latency   610.15us  131.39us  12.83ms   71.77%
        Req/Sec     1.63k   115.16     2.20k    62.22%
      5835256 requests in 5.00m, 1.02GB read
    Requests/sec:  19444.39
    Transfer/sec:      3.47MB
    
         mean                    610 us
           0% (minimum)          254 us
          10%                    461 us
          20%                    503 us
          30%                    535 us
          40%                    565 us
          50%                    595 us
          75%                    684 us
          80%                    708 us
          90%                    778 us
          95%                    842 us
          99%                    980 us
        99.9%                   1168 us
       99.99%                   1398 us
      99.999%                   6642 us
         100% (maximum)        12829 us

