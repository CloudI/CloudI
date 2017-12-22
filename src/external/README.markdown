External Dependencies
=====================

All CloudI Erlang dependencies are moved to exist with a `"cloudi_x_"`
prefix to avoid any conflicts when CloudI shares the Erlang VM with other
applications that may share common dependencies (`erlzmq2` is the only
exception due to its C integration).

C `zeromq/v?/zeromq`
--------------------
`ZeroMQ 2.2.0 and 3.2.4`

- `http://download.zeromq.org/zeromq-2.2.0.tar.gz`
- `http://download.zeromq.org/zeromq-3.2.4.tar.gz`
- `Fri Jan 17 11:48:27 PST 2014`
- `LGPL` (requires explicit enable from configure)

C++ `backward-cpp`
------------------
`for creating a backtrace in C++`

- `https://github.com/bombela/backward-cpp`
- `commit c9251fdd25195a17f638de2d314c422cc4a3b193`
- `Thu Jun  1 14:32:43 PDT 2017`
- `MIT`

C++ `booster (only booster/backtrace.h)`
----------------------------------------
`booster/backtrace.h, from booster, from CppCMS,
 for creating a backtrace in C++`

- `https://svn.code.sf.net/p/cppcms/code/framework/trunk/booster/`
- `revision r2237 2013-01-06 12:34:05 -0800 (CppCMS 1.0.4)`
- `Wed Nov 27 12:15:07 PST 2013`
- `Boost Software License v1`

C++ `GEPD`
----------
`Generic Erlang Port [Driver]`

- `https://github.com/okeuday/GEPD`
- `(src/lib/cloudi_core/cxx_src/cloudi_os_spawn_hrl.h) (erlang_functions_hrl.h)`
- `(src/lib/cloudi_core/cxx_src/port.cpp)`
- `(src/lib/cloudi_core/cxx_src/port.hpp)`
- `(src/lib/cloudi_core/cxx_src/pchar_len_t.h)`
- `commit fc5cd53b0be1951a8819a811c66812732369f85a`
- `Wed May 31 16:47:09 PDT 2017`
- `MIT`

Erlang `bear`
-------------
`Statistics functions for Erlang`

- `https://github.com/folsom-project/bear`
- `commit cdd853442a20d36f2104f2be4dafcf94b32a8b06 (0.8.5)`
- `Tue Sep 12 13:35:26 PDT 2017`
- `Apache License 2.0`

Erlang `certifi`
----------------
`SSL Certificates for Erlang`

- `https://github.com/certifi/erlang-certifi`
- `commit aad712ca0251ffed5d56c54c432aca04536bd18f (2.0.0)`
- `Tue Sep 12 13:35:26 PDT 2017`
- `BSD`

Erlang `cowboy`
---------------
`Erlang HTTP Server`

- `https://github.com/ninenines/cowboy`
- `commit 3d9078018d7f0a83a359b70c698d35e35fbb94f9 (1.0.1)`
- `Mon Nov 10 09:11:52 PST 2014`
- `MIT`

Erlang `cowlib`
---------------
`cowboy Protocols`

- `https://github.com/ninenines/cowlib`
- `commit d544a494af4dbc810fc9c15eaf5cc050cced1501 (1.0.0)`
- `Mon Aug 11 10:57:20 PDT 2014`
- `MIT`

Erlang `elli`
-------------
`Erlang HTTP Server`

- `https://github.com/knutin/elli`
- `commit 0cae1521190459bdb16958be7350191df710d799 (v1.0.5)`
- `Wed May 18 19:39:55 PDT 2016`
- `MIT`

Erlang `emysql`
---------------
 `Erlang MySQL (native) Driver`
 
- `https://github.com/okeuday/emysql`
- `commit 1b58bc3a0d08608824c5bc52c31551fdf357ec32 (v0.4.2_CloudI)`
- `Wed May 18 19:39:55 PDT 2016`
- `MIT`

Erlang `epgsql`
---------------
`Erlang PostgreSQL (native) Driver (community (newer) fork)`

- `https://github.com/okeuday/epgsql_new`
- `commit cebd2c88cf146b09553e6ec78ba4ede8309c52df (branch devel)`
- `Wed Jul 16 11:33:44 PDT 2014`
- `BSD`

Erlang `exometer`
-----------------
`Erlang instrumentation package`

- `https://github.com/Feuerlabs/exometer`
- `commit 7a7bd8d2b52de4d90f65aa3f6044b0e988319b9e`
- `Tue Sep 12 13:35:26 PDT 2017`
- `Mozilla Public License 2.0`

Erlang `exometer_core`
----------------------
`Erlang instrumentation package core`

- `https://github.com/Feuerlabs/exometer_core`
- `commit c4afd6d095c8eef7c5447133a033b9165f53da02 (1.5.0)`
- `Tue Sep 12 13:35:26 PDT 2017`
- `Mozilla Public License 2.0`

Erlang `exometer_influxdb`
--------------------------
`Exometer InfluxDB reporter`

- `https://github.com/travelping/exometer_influxdb`
- `commit f484d1e82d15319b110c1bce82b4afaddfc094d9`
- `Tue Sep 12 13:35:26 PDT 2017`
- `Mozilla Public License 2.0`

Erlang `folsom`
---------------
`Erlang Events and Metrics`

- `https://github.com/folsom-project/folsom`
- `commit 0948c5f7a2221cc7c4975d042b13c84bb9387517 (0.8.5)`
- `Tue Sep 12 13:35:26 PDT 2017`
- `Apache License 2.0`

Erlang `hackney`
----------------
`HTTP client library in Erlang`

- `https://github.com/benoitc/hackney`
- `commit ac85ef2a918e1c14875c60c611038f161e0ea2a3 (1.9.0)`
- `Tue Sep 12 13:35:26 PDT 2017`
- `Apache License 2.0`

Erlang `hut`
------------
`helper library for making Erlang libraries logging framework agnostic`

- `https://github.com/tolbrino/hut`
- `commit b200e5acbfe6e52e0470eeabc68d83437bde6866`
- `Mon Mar  6 10:12:42 PST 2017`
- `MIT`

Erlang `idna`
-------------
`A pure Erlang IDNA implementation`

- `https://github.com/benoitc/erlang-idna`
- `commit b452d8fe4c15be8146f5ca15d4f0c08720a98be4 (5.1.0)`
- `Tue Sep 12 13:35:26 PDT 2017`
- `BSD`

Erlang `jinterface`
-------------------
`Binary Erlang Term Encoding Java Source Code`

- `http://erlang.org/download/otp_src_18.3.tar.gz (jinterface-1.6.1)`
- `(in otp_src_18.3/lib/jinterface/java_src/com/ericsson/otp/erlang/)`
- `repository location: src/api/java/com/ericsson/otp/erlang/`
- `Wed Sep 14 16:35:57 PDT 2016`
- `Apache License 2.0`

Erlang `jsx`
------------
`Erlang JSON Parsing`

- `https://github.com/talentdeficit/jsx`
- `commit 3074d4865b3385a050badf7828ad31490d860df5 (2.8.0)`
- `Tue May 17 11:17:32 PDT 2016`
- `MIT`

Erlang `metrics`
----------------
`A generic interface to different metrics systems in Erlang`

- `https://github.com/benoitc/erlang-metrics`
- `commit c6eb4dcf29f9e907539915e2ab996f40c2ec7e8e (1.0.1)`
- `Fri Feb 24 10:27:11 PST 2017`
- `BSD`

Erlang `mimerl`
---------------
`library to handle mimetypes`

- `https://github.com/benoitc/mimerl`
- `commit 678aba028a690da6822c87410d475841c048b6bf (1.0.2)`
- `Fri Feb 24 10:27:11 PST 2017`
- `MIT`

Erlang `msgpack`
----------------
`MessagePack in Erlang`

- `https://github.com/msgpack/msgpack-erlang`
- `commit c3aed39befd42ddd97edb41da265369230564956`
- `Fri May  5 16:34:18 PDT 2017`
- `Apache License 2.0`

Erlang `nodefinder`
-------------------
`Strategies for automatic node discovery in Erlang`

- `https://github.com/okeuday/nodefinder`
- `commit 0aae09e15a86731111a9edd9f3dd2a5ab1d76f12`
- `Thu Dec 21 21:04:42 PST 2017`
- `MIT`

Erlang `parse_trans`
--------------------
`Erlang parse transforms`

- `https://github.com/uwiger/parse_trans`
- `commit 6f3645afb43c7c57d61b54ef59aecab288ce1013 (3.0.0)`
- `Fri Feb 24 10:27:11 PST 2017`
- `Apache License 2.0`

Erlang `pgsql`
--------------
`Erlang PostgreSQL (native) driver (semiocast branch)`

- `https://github.com/semiocast/pgsql`
- `commit e40786b5c3e76dcfe502cbad702ed1b5e50b616e`
- `Tue Sep  1 21:59:06 PDT 2015`
- `BSD`

Erlang `proper`
---------------
`PropEr (PROPerty-based testing tool for ERlang)`

- `https://github.com/manopapad/proper`
- `commit 5f0d69c831b6c9f52535c3d1846efca480f6190d (v1.2)`
- `Sun Jun 26 22:55:05 PDT 2016`
- `GPLv3` (build/test-time dependency only)

Erlang `ranch`
--------------
`Erlang Socket acceptor pool for TCP protocols`

- `https://github.com/ninenines/ranch`
- `commit adf1822defc2b7cfdc7aca112adabfa1d614043c (1.0.0)`
- `Mon Aug 11 10:57:20 PDT 2014`
- `MIT`

Erlang `rebar`
--------------
`Erlang OTP-compliant build tool`

- `https://github.com/rebar/rebar`
- `commit ebb474b7593527969a8ce7d43b42f00e4498c391 (2.1.0-pre)`
- `Tue Feb 12 12:32:53 PST 2013`
- `Apache License 2.0`

Erlang `setup`
--------------
`Erlang Setup Application`

- `https://github.com/uwiger/setup`
- `commit a0b5a237b6359795af24257fee41d97bf1602819 (1.8.2)`
- `Tue Sep 12 13:35:26 PDT 2017`
- `Apache License 2.0`

Erlang `ssl_verify_fun`
-----------------------
`SSL verification for Erlang`

- `https://github.com/deadtrickster/ssl_verify_fun.erl`
- `commit 33406f6decdcb9f03cf1e69e34728a288af156a0 (1.1.1)`
- `Fri Feb 24 10:27:11 PST 2017`
- `MIT`

Erlang `unicode_util_compat`
-----------------------
`unicode_util compatibility library for Erlang < 20`

- `https://github.com/benoitc/unicode_util_compat`
- `commit 38d7bc105f51159e8ea3279c40121db9db1e652f (0.3.1)`
- `Tue Sep 12 13:35:26 PDT 2017`
- `Apache License 2.0`

Erlang `zeromq/v?/erlzmq`
-------------------------
`ZeroMQ (http://www.zeromq.org/) Erlang Driver`

- `https://github.com/okeuday/erlzmq2 (branch master and 3.x)`
- `commit 9171c69d94b2af72dbe74cfce8f4b61266026e33` (master)
- `commit e75f3c8cdf050fc08c00a184d91c4bcbace7c683` (3.x)
- `Mon Mar 11 12:52:17 PDT 2013`
- `BSD`

Haskell `binary`
----------------
`Haskell binary serialisation`

- `https://hackage.haskell.org/package/binary`
- `https://hackage.haskell.org/package/binary-0.9.0.0/binary-0.9.0.0.tar.gz`
- `repository location: src/api/haskell/external/`
- `Thu Mar 16 15:43:25 PDT 2017`
- `BSD`

Haskell `bytestring`
--------------------
`Haskell ByteString: Fast, Packed Strings of Bytes`

- `https://hackage.haskell.org/package/bytestring`
- `https://hackage.haskell.org/package/bytestring-0.10.8.1/bytestring-0.10.8.1.tar.gz`
- `repository location: src/api/haskell/external/`
- `Thu Mar 16 15:43:25 PDT 2017`
- `BSD`

Haskell `network`
-----------------
`Haskell Low-level networking interface`

- `https://hackage.haskell.org/package/network`
- `https://hackage.haskell.org/package/network-2.6.3.1/network-2.6.3.1.tar.gz`
- `repository location: src/api/haskell/external/`
- `Thu Mar 16 15:43:25 PDT 2017`
- `BSD`

Haskell `unix`
--------------
`Haskell POSIX functionality`

- `https://hackage.haskell.org/package/unix`
- `https://hackage.haskell.org/package/unix-2.7.2.1/unix-2.7.2.1.tar.gz`
- `repository location: src/api/haskell/external/`
- `Thu Mar 16 15:43:25 PDT 2017`
- `BSD`

Haskell `zlib`
--------------
`Haskell Compression and Decompression in the gzip and zlib formats`

- `https://hackage.haskell.org/package/zlib`
- `https://hackage.haskell.org/package/zlib-0.6.1.2/zlib-0.6.1.2.tar.gz`
- `repository location: src/api/haskell/external/`
- `Thu Mar 16 15:43:25 PDT 2017`
- `BSD`

M4 `m4/ax_boost_base.m4, m4/ax_boost_thread.m4, m4/ax_boost_system.m4`
----------------------------------------------------------------------
`autoconf m4 macros for boost detection`

- `http://git.savannah.gnu.org/gitweb/?p=autoconf-archive.git;a=blob_plain;f=m4/ax_boost_base.m4`
- `http://git.savannah.gnu.org/gitweb/?p=autoconf-archive.git;a=blob_plain;f=m4/ax_boost_thread.m4` (required local modification!)
- `http://git.savannah.gnu.org/gitweb/?p=autoconf-archive.git;a=blob_plain;f=m4/ax_boost_system.m4`
- `repository location: src/m4/`
- `Mon Sep 19 17:55:21 UTC 2016`
- `MIT-like`

M4 `m4/ax_check_class.m4, m4/ax_check_rqrd_class.m4, m4/ax_prog_jar.m4, m4/ax_prog_javac.m4, m4/ax_prog_javac_works.m4, m4/ax_prog_java.m4, m4/ax_prog_java_works.m4, m4/ax_try_compile_java.m4`
--------------------------------------------------------------------------------
`autoconf m4 macros for java detection`

- `http://www.gnu.org/software/autoconf-archive/ax_check_class.html` (required local modification!)
- `http://www.gnu.org/software/autoconf-archive/ax_check_rqrd_class.html`
- `http://www.gnu.org/software/autoconf-archive/ax_prog_jar.html`
- `http://www.gnu.org/software/autoconf-archive/ax_prog_javac.html`
- `http://www.gnu.org/software/autoconf-archive/ax_prog_javac_works.html`
- `http://www.gnu.org/software/autoconf-archive/ax_prog_java.html`
- `http://www.gnu.org/software/autoconf-archive/ax_prog_java_works.html` (required local modification!)
- `http://www.gnu.org/software/autoconf-archive/ax_try_compile_java.html`
- `repository location: src/m4/`
- `Mon Sep 19 17:55:21 UTC 2016`
- `GPL` (build-time dependency only)

M4 `m4/ax_lib_socket_nsl.m4`
----------------------------

- `http://www.gnu.org/software/autoconf-archive/ax_lib_socket_nsl.html`
- `repository location: src/m4/`
- `Mon Sep 19 17:55:21 UTC 2016`
- `MIT-like`

M4 `m4/ax_prog_python_version.m4`
---------------------------------
`autoconf m4 macros for python detection`

- `http://www.gnu.org/software/autoconf-archive/ax_prog_python_version.html`
- `repository location: src/m4/`
- `Mon Sep 19 17:55:21 UTC 2016`
- `MIT-like`

M4 `m4/ax_prog_ruby_version.m4, m4/ax_compare_version.m4`
---------------------------------------------------------
`autoconf m4 macros for ruby detection`

- `http://www.gnu.org/software/autoconf-archive/ax_prog_ruby_version.html`
- `http://www.gnu.org/software/autoconf-archive/ax_compare_version.html`
- `repository location: src/m4/`
- `Mon Sep 19 17:55:21 UTC 2016`
- `MIT-like`

Python `jsonrpclib`
-------------------
`Python JSON-RPC library`

- `https://github.com/joshmarshall/jsonrpclib`
- `repository location: src/service_api/python/jsonrpclib/`
- `commit efcf5eee0e12ef8a148c`
- `Tue Apr 12 18:07:44 PDT 2011`
- `Apache License 2.0`

