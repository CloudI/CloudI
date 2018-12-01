External Dependencies
=====================

All CloudI Erlang dependencies are moved to exist with a `"cloudi_x_"`
prefix to avoid any conflicts when CloudI shares the Erlang VM with other
applications that may share common dependencies.

C++ `backward-cpp`
------------------
`for creating a backtrace in C++`

- `https://github.com/bombela/backward-cpp`
- `commit 84ae4f5e80381aca765a0810d4c811acae3cd7c7 (v1.4)`
- `Sat Sep 22 18:01:11 PDT 2018`
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
- `commit b31a294c4c7d6f2c124b9d524c480fbeb3fbbb8a (0.8.7)`
- `Wed Nov 14 20:15:29 UTC 2018`
- `Apache License 2.0`

Erlang `certifi`
----------------
`SSL Certificates for Erlang`

- `https://github.com/certifi/erlang-certifi`
- `commit aad712ca0251ffed5d56c54c432aca04536bd18f (2.0.0)`
- `Tue Sep 12 13:35:26 PDT 2017`
- `BSD`

Erlang `cowboy1`
----------------
`Erlang HTTP Server`

- `https://github.com/ninenines/cowboy`
- `commit 0c15b9216b7467390623db84e169b32c68e85bcb (1.1.2)`
- `Fri Nov 30 17:21:05 PST 2018`
- `MIT`

Erlang `cowlib1`
----------------
`cowboy Protocols`

- `https://github.com/ninenines/cowlib`
- `commit 45f750db410a4b08c68d142ad0af839f544c5d3d (1.0.2)`
- `Fri Nov 30 17:21:05 PST 2018`
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

- `https://github.com/Feuerlabs/exometer (branch tb-upgrade)`
- `commit 4b0fcbc35ed6a1417022b6b2b6048c006aa88bca`
- `Wed Nov 14 20:15:29 UTC 2018`
- `Mozilla Public License 2.0`

Erlang `exometer_core`
----------------------
`Erlang instrumentation package core`

- `https://github.com/Feuerlabs/exometer_core`
- `commit 547f28bc93c7cb3d3f1174fb4c510667a4ebb645 (v1.5.7)`
- `Wed Nov 14 20:15:29 UTC 2018`
- `Mozilla Public License 2.0`

Erlang `exometer_influxdb`
--------------------------
`Exometer InfluxDB reporter`

- `https://github.com/travelping/exometer_influxdb`
- `commit df48129d1e47dd263eb28e7d2e453f1b9d66bb97 (0.6.0)`
- `Wed Nov 14 20:15:29 UTC 2018`
- `Mozilla Public License 2.0`

Erlang `folsom`
---------------
`Erlang Events and Metrics`

- `https://github.com/folsom-project/folsom`
- `commit 9309bad9ffadeebbefe97521577c7480c7cfcd8a (0.8.7)`
- `Wed Nov 14 20:15:29 UTC 2018`
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
- `commit 37e281ff39d5767221314210f46d46c78ad530bf (v1.2.1)`
- `Wed Nov 14 20:15:29 UTC 2018`
- `MIT`

Erlang `idna`
-------------
`A pure Erlang IDNA implementation`

- `https://github.com/benoitc/erlang-idna`
- `commit b452d8fe4c15be8146f5ca15d4f0c08720a98be4 (5.1.0)`
- `Tue Sep 12 13:35:26 PDT 2017`
- `BSD`

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
- `commit 0e07c074f18678852e152aba169be2254e18e4c9 (v1.7.4)`
- `Sun Sep 16 01:34:47 PDT 2018`
- `MIT`

Erlang `parse_trans`
--------------------
`Erlang parse transforms`

- `https://github.com/uwiger/parse_trans`
- `commit 76abb347c3c1d00fb0ccf9e4b43e22b3d2288484 (3.3.0)`
- `Wed Nov 14 20:15:29 UTC 2018`
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

- `https://github.com/proper-testing/proper`
- `commit 16f9f7cb181f41ffb882c4a31052275dfad16ee1`
- `Sat Oct 20 19:09:19 PDT 2018`
- `GPLv3` (build/test-time dependency only)

Erlang `ranch1`
---------------
`Erlang Socket acceptor pool for TCP protocols`

- `https://github.com/ninenines/ranch`
- `commit a004ad710eddd0c21aaccc30d5633a76b06164b5 (1.3.2)`
- `Fri Nov 30 17:21:05 PST 2018`
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
- `commit eda3352e7555b9faf42b5242d95fc386bccbe123 (2.0.2)`
- `Wed Nov 14 20:15:29 UTC 2018`
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

Java `jinterface`
-----------------
`Binary Erlang Term Encoding Java Source Code`

- `http://erlang.org/download/otp_src_18.3.tar.gz (jinterface-1.6.1)`
- `(in otp_src_18.3/lib/jinterface/java_src/com/ericsson/otp/erlang/)`
- `repository location: src/api/java/com/ericsson/otp/erlang/`
- `Wed Sep 14 16:35:57 PDT 2016`
- `Apache License 2.0`

M4 `m4/ax_boost_base.m4, m4/ax_boost_system.m4, m4/ax_boost_thread.m4, m4/ax_compare_version.m4, m4/ax_compiler_version.m4, m4/ax_cxx_exceptions.m4`
--------------------------------------------------------------------------------
`autoconf m4 macros for c++ and boost detection`

- `https://git.savannah.gnu.org/git/autoconf-archive`
- `commit bcdf5ad0c5981a3c01da3216fbf42b6d1e1ffab0`
- `Sat Oct 27 17:49:52 PDT 2018`
- `MIT-like`

M4 `m4/ax_compiler_vendor.m4`
-----------------------------
`autoconf m4 macro for c++ compiler vendor detection`

- `https://git.savannah.gnu.org/git/autoconf-archive`
- `commit bcdf5ad0c5981a3c01da3216fbf42b6d1e1ffab0`
- `Sat Oct 27 17:49:52 PDT 2018`
- `GPL` (build-time dependency only)

M4 `m4/ax_check_class.m4, m4/ax_check_rqrd_class.m4, m4/ax_prog_javac.m4, m4/ax_prog_javac_works.m4, m4/ax_prog_java.m4, m4/ax_prog_java_works.m4`
--------------------------------------------------------------------------------
`autoconf m4 macros for java detection`

- `https://git.savannah.gnu.org/git/autoconf-archive`
- `commit bcdf5ad0c5981a3c01da3216fbf42b6d1e1ffab0`
- `Sat Oct 27 17:49:52 PDT 2018`
- `GPL` (build-time dependency only)

M4 `m4/ax_prog_jar.m4, m4/ax_try_compile_java.m4`
-------------------------------------------------
`autoconf m4 macros for java detection`

- `https://git.savannah.gnu.org/git/autoconf-archive`
- `commit bcdf5ad0c5981a3c01da3216fbf42b6d1e1ffab0`
- `Sat Oct 27 17:49:52 PDT 2018`
- `MIT-like`

M4 `m4/ax_lib_socket_nsl.m4`
----------------------------
`autoconf m4 macros for socket compatibility`

- `https://git.savannah.gnu.org/git/autoconf-archive`
- `commit bcdf5ad0c5981a3c01da3216fbf42b6d1e1ffab0`
- `Sat Oct 27 17:49:52 PDT 2018`
- `MIT-like`

M4 `m4/ax_prog_python_version.m4`
---------------------------------
`autoconf m4 macros for python detection`

- `https://git.savannah.gnu.org/git/autoconf-archive`
- `commit bcdf5ad0c5981a3c01da3216fbf42b6d1e1ffab0`
- `Sat Oct 27 17:49:52 PDT 2018`
- `MIT-like`

M4 `m4/ax_prog_ruby_version.m4, m4/ax_compare_version.m4`
---------------------------------------------------------
`autoconf m4 macros for ruby detection`

- `https://git.savannah.gnu.org/git/autoconf-archive`
- `commit bcdf5ad0c5981a3c01da3216fbf42b6d1e1ffab0`
- `Sat Oct 27 17:49:52 PDT 2018`
- `MIT-like`

OCaml `num`
-----------
`core distribution legacy Num library (separate with OCaml >= 4.06)`

- `https://github.com/ocaml/num/`
- `repository location: src/api/ocaml/external/`
- `Fri Dec 29 00:58:57 UTC 2017`
- `LGPL 2.1 w/link-exception`

Python `jsonrpclib`
-------------------
`Python JSON-RPC library`

- `https://github.com/tcalmant/jsonrpclib/`
- `repository location: src/service_api/python/jsonrpclib/`
- `commit a8fac9cee4e11dc29fa22ce82dca3cbde6bcd309 (v0.3.1)`
- `Mon Jun 25 11:52:30 PDT 2018`
- `Apache License 2.0`

