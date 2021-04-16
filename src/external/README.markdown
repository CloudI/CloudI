External Dependencies
=====================

All CloudI Erlang dependencies are moved to exist with a `"cloudi_x_"`
prefix to avoid any conflicts when CloudI shares the Erlang VM with other
applications that may share common dependencies.

C++ `backward-cpp`
------------------
`for creating a backtrace in C++`

- `https://github.com/bombela/backward-cpp`
- `commit cc475eb3b0fed934ea0897e96578f712048c3b53`
- `Wed Apr 15 16:02:20 UTC 2020`
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
- `commit 0b03eed5127e2872598aa6bff3de29c5c4fa1f63`
- `Fri Jul  3 02:15:17 UTC 2020`
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
- `commit 549670ddfe96727a703a295c75c423d8a2c50718 (2.5.2)`
- `Sat May 30 23:57:14 UTC 2020`
- `BSD`

Erlang `cowboy1`
----------------
`Erlang HTTP Server`

- `https://github.com/ninenines/cowboy`
- `commit 0c15b9216b7467390623db84e169b32c68e85bcb (1.1.2)`
- `Sun Dec  2 00:06:04 UTC 2018`
- `MIT`

Erlang `cowlib1`
----------------
`cowboy Protocols`

- `https://github.com/ninenines/cowlib`
- `commit 45f750db410a4b08c68d142ad0af839f544c5d3d (1.0.2)`
- `Sun Dec  2 00:06:04 UTC 2018`
- `MIT`

Erlang `elli`
-------------
`Erlang HTTP Server`

- `https://github.com/elli-lib/elli`
- `commit 968afee385f46c053fd46858713261db7dc03af4 (3.3.0)`
- `Sun Aug 23 02:07:36 UTC 2020`
- `MIT`

Erlang `emysql`
---------------
 `Erlang MySQL (native) Driver (Eonblast fork)`
 
- `https://github.com/okeuday/emysql`
- `commit 1b58bc3a0d08608824c5bc52c31551fdf357ec32 (v0.4.2_CloudI)`
- `Wed May 18 19:39:55 PDT 2016`
- `MIT`

Erlang `epgsql`
---------------
`Erlang PostgreSQL (native) Driver (epgsql community fork)`

- `https://github.com/epgsql/epgsql`
- `commit b3138f76759750ab5e6202766cb39acd88c8ff54 (4.2.1)`
- `Mon Dec  3 20:45:23 UTC 2018`
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
- `commit b27104776fdab5d19bb376f683c8d156c84e5a5a`
- `Sat May 30 23:57:14 UTC 2020`
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
- `commit f3e9292db22c807e73f57a8422402d6b423ddf5f (1.16.0)`
- `Sat May 30 23:57:14 UTC 2020`
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
- `commit cda8d41eec752469053db6155ca8afeeeb2b5387 (6.0.1)`
- `Sat May 30 23:57:14 UTC 2020`
- `BSD`

Erlang `jsx`
------------
`Erlang JSON Parsing`

- `https://github.com/talentdeficit/jsx`
- `commit 7c7a69c167aecd241a6676ab7e5a7665c782b976`
- `Fri Sep 18 03:50:49 UTC 2020`
- `MIT`

Erlang `metrics`
----------------
`A generic interface to different metrics systems in Erlang`

- `https://github.com/benoitc/erlang-metrics`
- `commit c6eb4dcf29f9e907539915e2ab996f40c2ec7e8e (1.0.1)`
- `Mon Dec  3 20:45:23 UTC 2018`
- `BSD`

Erlang `mimerl`
---------------
`library to handle mimetypes`

- `https://github.com/benoitc/mimerl`
- `commit 5a1b22a8fada5b3b40438da00a6923cb87a42bbc (1.2.0)`
- `Sat May 30 23:57:14 UTC 2020`
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
- `commit 280a9a4033b5bb309b3431521f6fb8153b75d027 (v2.0.1)`
- `Fri Nov 27 19:52:40 UTC 2020`
- `MIT`

Erlang `parse_trans`
--------------------
`Erlang parse transforms`

- `https://github.com/uwiger/parse_trans`
- `commit 8ba366f81789c913cd63d69c6d1da948c200d18a (3.3.1)`
- `Fri Apr 16 02:13:25 UTC 2021`
- `Apache License 2.0`

Erlang `pgsql`
--------------
`Erlang PostgreSQL (native) driver (semiocast driver)`

- `https://github.com/semiocast/pgsql`
- `commit 890df880a55eac3364da0082e7e9a4790c4399fe (v26.0.2)`
- `Mon Dec  3 20:45:23 UTC 2018`
- `BSD`

Erlang `proper`
---------------
`PropEr (PROPerty-based testing tool for ERlang)`

- `https://github.com/proper-testing/proper`
- `commit cca1edf641b18543098cf534600eb018bb38d16d`
- `Thu Apr 15 23:07:33 UTC 2021`
- `GPLv3` (build/test-time dependency only)

Erlang `ranch1`
---------------
`Erlang Socket acceptor pool for TCP protocols`

- `https://github.com/ninenines/ranch`
- `commit a004ad710eddd0c21aaccc30d5633a76b06164b5 (1.3.2)`
- `Sun Dec  2 00:06:04 UTC 2018`
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
- `commit 87550a18b3f5c8f9cd062bc41ba5ee719f21dd0a (1.1.6)`
- `Sat May 30 23:57:14 UTC 2020`
- `MIT`

Erlang `unicode_util_compat`
-----------------------
`unicode_util compatibility library for Erlang < 21`

- `https://github.com/benoitc/unicode_util_compat`
- `commit 6b43143b6fddaffc6cfcc3dc02f0ba136cf047f2 (0.5.0)`
- `Sat May 30 23:57:14 UTC 2020`
- `Apache License 2.0`

Haskell `binary`
----------------
`Haskell binary serialisation`

- `https://hackage.haskell.org/package/binary`
- `https://hackage.haskell.org/package/binary-0.8.7.0/binary-0.8.7.0.tar.gz`
- `repository location: src/api/haskell/external/`
- `Sat Sep  7 23:58:26 UTC 2019`
- `BSD`

Haskell `bytestring`
--------------------
`Haskell ByteString: Fast, Packed Strings of Bytes`

- `https://hackage.haskell.org/package/bytestring`
- `https://hackage.haskell.org/package/bytestring-0.10.10.0/bytestring-0.10.10.0.tar.gz`
- `repository location: src/api/haskell/external/`
- `Sat Sep  7 23:58:26 UTC 2019`
- `BSD`

Haskell `network`
-----------------
`Haskell Low-level networking interface`

- `https://hackage.haskell.org/package/network`
- `https://hackage.haskell.org/package/network-3.1.0.1/network-3.1.0.1.tar.gz`
- `repository location: src/api/haskell/external/`
- `Sat Sep  7 23:58:26 UTC 2019`
- `BSD`

Haskell `unix`
--------------
`Haskell POSIX functionality`

- `https://hackage.haskell.org/package/unix`
- `https://hackage.haskell.org/package/unix-2.7.2.2/unix-2.7.2.2.tar.gz`
- `repository location: src/api/haskell/external/`
- `Sat Sep  7 23:58:26 UTC 2019`
- `BSD`

Haskell `zlib`
--------------
`Haskell Compression and Decompression in the gzip and zlib formats`

- `https://hackage.haskell.org/package/zlib`
- `https://hackage.haskell.org/package/zlib-0.6.2.1/zlib-0.6.2.1.tar.gz`
- `repository location: src/api/haskell/external/`
- `Sat Sep  7 23:58:26 UTC 2019`
- `BSD`

Java `jinterface`
-----------------
`Binary Erlang Term Encoding Java Source Code`

- `https://github.com/erlang/otp (jinterface-1.9.1)`
- `(in lib/jinterface/java_src/com/ericsson/otp/erlang/)`
- `commit 3b2c1dd3fdf1fec362cff3a88e188ef989ee79e5 (OTP-21.3.8.1)`
- `repository location: src/api/java/com/ericsson/otp/erlang/`
- `Sun May 19 01:29:55 UTC 2019`
- `Apache License 2.0`

M4 `m4/ax_boost_base.m4, m4/ax_boost_system.m4, m4/ax_boost_thread.m4, m4/ax_compare_version.m4, m4/ax_compiler_version.m4, m4/ax_cxx_exceptions.m4`
--------------------------------------------------------------------------------
`autoconf m4 macros for c++ and boost detection`

- `https://git.savannah.gnu.org/cgit/autoconf-archive.git/tree/`
- `commit bcdf5ad0c5981a3c01da3216fbf42b6d1e1ffab0`
- `Sat Oct 27 17:49:52 PDT 2018`
- `MIT-like`

M4 `m4/ax_compiler_vendor.m4`
-----------------------------
`autoconf m4 macro for c++ compiler vendor detection`

- `https://git.savannah.gnu.org/cgit/autoconf-archive.git/tree/`
- `commit bcdf5ad0c5981a3c01da3216fbf42b6d1e1ffab0`
- `Sat Oct 27 17:49:52 PDT 2018`
- `GPL` (build-time dependency only)

M4 `m4/ax_check_class.m4, m4/ax_check_rqrd_class.m4, m4/ax_prog_javac.m4, m4/ax_prog_javac_works.m4, m4/ax_prog_java.m4, m4/ax_prog_java_works.m4`
--------------------------------------------------------------------------------
`autoconf m4 macros for java detection`

- `https://git.savannah.gnu.org/cgit/autoconf-archive.git/tree/`
- `commit bcdf5ad0c5981a3c01da3216fbf42b6d1e1ffab0`
- `Sat Oct 27 17:49:52 PDT 2018`
- `GPL` (build-time dependency only)

M4 `m4/ax_prog_jar.m4, m4/ax_try_compile_java.m4`
-------------------------------------------------
`autoconf m4 macros for java detection`

- `https://git.savannah.gnu.org/cgit/autoconf-archive.git/tree/`
- `commit bcdf5ad0c5981a3c01da3216fbf42b6d1e1ffab0`
- `Sat Oct 27 17:49:52 PDT 2018`
- `MIT-like`

M4 `m4/ax_lib_socket_nsl.m4`
----------------------------
`autoconf m4 macros for socket compatibility`

- `https://git.savannah.gnu.org/cgit/autoconf-archive.git/tree/`
- `commit bcdf5ad0c5981a3c01da3216fbf42b6d1e1ffab0`
- `Sat Oct 27 17:49:52 PDT 2018`
- `MIT-like`

M4 `m4/ax_prog_python_version.m4`
---------------------------------
`autoconf m4 macros for python detection`

- `https://git.savannah.gnu.org/cgit/autoconf-archive.git/tree/`
- `commit bcdf5ad0c5981a3c01da3216fbf42b6d1e1ffab0`
- `Sat Oct 27 17:49:52 PDT 2018`
- `MIT-like`

M4 `m4/ax_prog_ruby_version.m4, m4/ax_compare_version.m4`
---------------------------------------------------------
`autoconf m4 macros for ruby detection`

- `https://git.savannah.gnu.org/cgit/autoconf-archive.git/tree/`
- `commit bcdf5ad0c5981a3c01da3216fbf42b6d1e1ffab0`
- `Sat Oct 27 17:49:52 PDT 2018`
- `MIT-like`

M4 `m4/ax_recursive_eval.m4`
-----------------------------
`autoconf m4 macro for recursive autoconf variable expansion`

- `https://git.savannah.gnu.org/cgit/autoconf-archive.git/tree/`
- `commit fb4f1ee20fcc1df13552bfc483be379900a5abfc`
- `Thu Dec 13 05:07:21 UTC 2018`
- `GPL` (build-time dependency only)

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

