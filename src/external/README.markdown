External Dependencies
=====================

All CloudI Erlang dependencies are moved to exist with a `"cloudi_x_"`
prefix to avoid any conflicts when CloudI shares the Erlang VM with other
applications that may share common dependencies.

C++ `backward-cpp`
------------------
`for creating a backtrace in C++`

- `https://github.com/okeuday/backward-cpp`
- `commit c5e783c4e8914f1103d55ae891a39759cd5c9dea (CloudI)`
- `Wed Aug 18 05:40:23 UTC 2021`
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
- `commit 22849b90a3cc8909a2cba9cc8b83385fe388387b (1.0)`
- `Fri Apr 16 04:03:35 UTC 2021`
- `Apache License 2.0`

Erlang `certifi`
----------------
`SSL Certificates for Erlang`

- `https://github.com/certifi/erlang-certifi`
- `commit 0be2c6a16f3c3f9c98a44bf22caac6a05b43b18e (2.6.1)`
- `Fri Apr 16 04:40:29 UTC 2021`
- `BSD`

Erlang `cowboy`
---------------
`Erlang HTTP Server`

- `https://github.com/ninenines/cowboy`
- `commit 04ca4c5d31a92d4d3de087bbd7d6021dc4a6d409 (2.9.0)`
- `Fri May 14 19:17:48 UTC 2021`
- `MIT`

Erlang `cowboy1`
----------------
`Erlang HTTP Server`

- `https://github.com/ninenines/cowboy`
- `commit 0c15b9216b7467390623db84e169b32c68e85bcb (1.1.2)`
- `Sun Dec  2 00:06:04 UTC 2018`
- `MIT`

Erlang `cowlib`
----------------
`cowboy Protocols`

- `https://github.com/ninenines/cowlib`
- `commit e9448e5628c8c1d9083223ff973af8de31a566d1 (2.11.0)`
- `Fri May 14 19:17:48 UTC 2021`
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
- `commit f811a09926892dbd1359afe44a9bfa8f6907b322 (devel)`
- `Fri Apr 16 02:47:47 UTC 2021`
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
- `commit 358d5c6724b823104f122ca4f16439ae0e767c82`
- `Fri Apr 16 04:03:35 UTC 2021`
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
- `commit 62fd0714e6f0b4e7833880afe371a9c882ea0fc2 (1.0)`
- `Fri Apr 16 04:03:35 UTC 2021`
- `Apache License 2.0`

Erlang `hackney`
----------------
`HTTP client library in Erlang`

- `https://github.com/benoitc/hackney`
- `commit 6e79b2bb11a77389d3ba9ff3a0828a45796fe7a8 (1.17.4)`
- `Fri Apr 16 04:40:29 UTC 2021`
- `Apache License 2.0`

Erlang `hut`
------------
`helper library for making Erlang libraries logging framework agnostic`

- `https://github.com/tolbrino/hut`
- `commit 9a9ac53401524548d6b6079d07e010ddb5cb15a1 (v1.3.0)`
- `Fri Apr 16 04:03:35 UTC 2021`
- `MIT`

Erlang `idna`
-------------
`A pure Erlang IDNA implementation`

- `https://github.com/benoitc/erlang-idna`
- `commit 792832450c7017c3b91f3f2ddd267533492146eb (6.1.1)`
- `Fri Apr 16 04:40:29 UTC 2021`
- `BSD`

Erlang `jsx`
------------
`Erlang JSON Parsing`

- `https://github.com/talentdeficit/jsx`
- `commit bb9b3e570a7efe331eed0900c3a5188043a850d7 (v3.1.0)`
- `Fri Apr 16 19:41:55 UTC 2021`
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
- `commit c3299646f53d8a0c26280e8f8143d6683a589630 (v2.0.2)`
- `Fri May 28 01:35:58 UTC 2021`
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
- `commit 1daf130b0dfaea1f867751e43d82d7be7ebaa457 (v1.4)`
- `Thu May 27 08:52:16 UTC 2021`
- `GPLv3` (build/test-time dependency only)

Erlang `ranch`
---------------
`Erlang Socket acceptor pool for TCP protocols`

- `https://github.com/ninenines/ranch`
- `commit a692f44567034dacf5efcaa24a24183788594eb7 (1.8.0)`
- `Fri May 14 19:17:48 UTC 2021`
- `MIT`

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
- `commit 0c658d8a946ca756da2847259c79791887209d37 (0.7.0)`
- `Fri Apr 16 04:40:29 UTC 2021`
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
- `commit ec34d9b7a8f8dd29ca794cc7acd183c9fd01f53d`
- `Mon May 10 19:21:06 UTC 2021`
- `MIT-like`

M4 `m4/ax_compiler_vendor.m4`
-----------------------------
`autoconf m4 macro for c++ compiler vendor detection`

- `https://git.savannah.gnu.org/cgit/autoconf-archive.git/tree/`
- `commit ec34d9b7a8f8dd29ca794cc7acd183c9fd01f53d`
- `Mon May 10 19:21:06 UTC 2021`
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

M4 `m4/ax_pthread.m4`
---------------------
`autoconf m4 macro for POSIX threads detection`

- `https://git.savannah.gnu.org/cgit/autoconf-archive.git/tree/`
- `commit ec34d9b7a8f8dd29ca794cc7acd183c9fd01f53d`
- `Mon May 10 19:21:06 UTC 2021`
- `GPL` (build-time dependency only)

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

