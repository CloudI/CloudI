External Dependencies
=====================

All CloudI Erlang dependencies are now moved to exist with a `"cloudi_x_"`
prefix to avoid any conflicts when CloudI shares the Erlang VM with other
applications that may share common dependencies (`erlzmq2` is an
exception due to its C integration, `cloudi_x_syslog` was manually modified).

`backward-cpp`
--------------
`for creating a backtrace in C++`

- `https://github.com/bombela/backward-cpp`
- `commit 77fc8d9fe4f2521558199206670fc3611dc75291`
- `Sat Oct 24 11:03:12 PDT 2015`
- `MIT`

`bear`
------
`Statistics functions for Erlang`

- `https://github.com/boundary/bear`
- `commit 119234548783af19b8ec75c879c5062676b92571 (0.8.2)`
- `Fri Dec 11 15:51:53 PST 2015`
- `Apache License 2.0`

`booster (only booster/backtrace.h)`
------------------------------------
`booster/backtrace.h, from booster, from CppCMS,
 for creating a backtrace in C++`

- `https://svn.code.sf.net/p/cppcms/code/framework/trunk/booster/`
- `revision r2237 2013-01-06 12:34:05 -0800 (CppCMS 1.0.4)`
- `Wed Nov 27 12:15:07 PST 2013`
- `Boost Software License v1`

`cowboy`
--------
`Erlang HTTP Server`

- `https://github.com/ninenines/cowboy`
- `commit 3d9078018d7f0a83a359b70c698d35e35fbb94f9 (1.0.1)`
- `Mon Nov 10 09:11:52 PST 2014`
- `MIT`

`cowlib`
--------
`cowboy Protocols`

- `https://github.com/ninenines/cowlib`
- `commit d544a494af4dbc810fc9c15eaf5cc050cced1501 (1.0.0)`
- `Mon Aug 11 10:57:20 PDT 2014`
- `MIT`

`dynamic_compile`
-----------------
`Dynamic compilation of Erlang modules`

- `https://github.com/okeuday/dynamic_compile`
- `commit 2684dcf703b6bac70421dff717293772c5da8f05 (v1.0.0)`
- `Mon Oct 13 13:58:48 PDT 2014`
- `MIT`

`ecouchdb (erlang_couchdb)`
---------------------------
`Erlang CouchDB (native) Driver`

- `https://github.com/okeuday/ecouchdb`
- `commit dd8bf6d694327feea202c6552cdb09c650831ba5`
- `Tue Oct 22 11:58:41 PDT 2013`
- `MIT`

`eini`
------
 `An Erlang INI parser`
 
- `https://github.com/accense/eini`
- `commit 27f384d85d83d394d03828d78d5cb9afe3308749 (1.2.1)`
- `Tue May 17 11:17:32 PDT 2016`
- `Apache License 2.0`

`elli`
------
`Erlang HTTP Server`

- `https://github.com/knutin/elli`
- `commit 4fbdf68198949ea926a5762e6f438265a04ac605 (v1.0.4)`
- `Wed Dec 23 18:41:56 PST 2015`
- `MIT`

`ememcached (previously mcerlang)`
----------------------------------
`Erlang memcached Driver`

- `https://github.com/okeuday/ememcached`
- `commit a44340caa9b14cefb590ba47c3baa2ac83270dcc`
- `Tue Oct 22 11:58:41 PDT 2013`
- `MIT`

`emysql`
--------
 `Erlang MySQL (native) Driver`
 
- `https://github.com/okeuday/emysql`
- `commit 9281bcf4850d26ac4968ab7f93bb443328b465d3 (v0.4.1_CloudI)`
- `Thu Sep 18 16:41:01 PDT 2014`
- `MIT`

`epgsql`
--------
`Erlang PostgreSQL (native) Driver (community (newer) fork)`

- `https://github.com/okeuday/epgsql_new`
- `commit cebd2c88cf146b09553e6ec78ba4ede8309c52df (branch devel)`
- `Wed Jul 16 11:33:44 PDT 2014`
- `BSD`

`epgsql_wg`
-----------
`Erlang PostgreSQL (native) Driver (Will Glozer (older) fork with additions)`

- `https://github.com/okeuday/epgsql_wg`
- `commit b74e913679a450ff754d8a3cb022948e308d0509 (1.5.0)`
- `Mon May  5 12:36:52 PDT 2014`
- `BSD`

`erlasticsearch`
----------------
`Erlang ElasticSearch (thrift) Driver`

- `https://github.com/dieswaytoofast/erlasticsearch`
- `commit 56ebf0d6f1aaab31c7d0f25d17ad4383f7c9051c`
- `Wed Dec  3 10:00:52 PST 2014`
- `New BSD`

`erlang_cassandra`
------------------
`Erlang Cassandra (thrift) Driver`

- `https://github.com/dieswaytoofast/erlang_cassandra`
- `commit 6d026bfb06f8efd53e63a32f9b887a01bcc550ad`
- `Sat May 31 13:01:02 PDT 2014`
- `New BSD`

`erlcloud`
----------
`Cloud Service Integration APIs`

- `https://github.com/okeuday/erlcloud`
- `commit 55d8e5c1246c5af48dc349a9dac41ecf938217f9 (fixes_for_19_0)`
- `Tue May 17 11:17:32 PDT 2016`
- `MIT`

`erlcql`
--------
`Cassandra native protocol CQL client for Erlang`

- `https://github.com/rpt/erlcql`
- `commit 44ff63e3048140c7877faffda94a0b90ab19deb5 (branch develop)`
- `Wed Jun 25 13:01:35 PDT 2014`
- `MIT`

`exometer`
----------
`Erlang instrumentation package`

- `https://github.com/Feuerlabs/exometer`
- `commit 25aa831bebd9cbd83eb0187c0fd7b3f56a42907f (1.2.1)`
- `Fri Dec 11 15:51:53 PST 2015`
- `Mozilla Public License 2.0`

`exometer_core`
---------------
`Erlang instrumentation package core`

- `https://github.com/Feuerlabs/exometer_core`
- `commit 88588f26f226210a1fc9e70271d8a0611ba83d30 (1.2)`
- `Fri Dec 11 15:51:53 PST 2015`
- `Mozilla Public License 2.0`

`folsom`
--------
`Erlang Events and Metrics`

- `https://github.com/boundary/folsom`
- `commit 38e2cce7c64ce1f0a3a918d90394cfc0a940b1ba (0.8.2)`
- `Fri Dec 11 15:51:53 PST 2015`
- `Apache License 2.0`

`goldrush`
----------
`Small, Fast Event Processing for Erlang`

- `https://github.com/DeadZen/goldrush`
- `commit 71e63212f12c25827e0c1b4198d37d5d018a7fec (0.1.6)`
- `Fri Dec 11 15:51:53 PST 2015`
- `MIT`

`jinterface`
------------
`Binary Erlang Term Encoding Java Source Code`

- `http://www.erlang.org/download/otp_src_18.0.tar.gz (jinterface-1.6)`
- `(in otp_src_18.0/lib/jinterface/java_src/com/ericsson/otp/erlang/)`
- `Sat Aug  1 19:56:12 PDT 2015`
- `Apache License 2.0`

`jsonrpclib`
------------
`Python JSON-RPC library`

- `https://github.com/joshmarshall/jsonrpclib`
- `commit efcf5eee0e12ef8a148c`
- `Tue Apr 12 18:07:44 PDT 2011`
- `Apache License 2.0`

`jsx`
-----
`Erlang JSON Parsing`

- `https://github.com/talentdeficit/jsx`
- `commit 3074d4865b3385a050badf7828ad31490d860df5 (2.8.0)`
- `Tue May 17 11:17:32 PDT 2016`
- `MIT`

`lager`
-------
`Erlang Logging`

- `https://github.com/basho/lager`
- `commit b6b6cebcb27ccff8acc59ae775acebc2f52e4926 (2.0.3)`
  `with https://github.com/basho/lager/pull/321/commits/d35670e01a3c6f9f9bcb3150217d26cc92513586`
- `Fri Dec 11 15:51:53 PST 2015`
- `Apache License 2.0`

`lhttpc`
--------
`A lightweight Erlang HTTP/1.1 client`

- `https://github.com/talko/lhttpc`
- `commit 2d01a5f9c7543b77bdd57f44c92e72801f46e423 (1.4.0)`
- `Tue May 17 11:17:32 PDT 2016`
- `BSD`

`lz4`
-----
`LZ4 bindings for Erlang`

- `https://github.com/szktty/erlang-lz4`
- `commit 86361e5c95200fa3b4fcba3f9d6c871f8c48a7f1`
- `Fri Jun 20 12:11:49 PDT 2014`
- `New BSD`

`m4/ax_boost_base.m4, m4/ax_boost_thread.m4, m4/ax_boost_system.m4`
-------------------------------------------------------------------
`autoconf m4 macros for boost detection`

- `http://git.savannah.gnu.org/gitweb/?p=autoconf-archive.git;a=blob_plain;f=m4/ax_boost_base.m4`
- `http://git.savannah.gnu.org/gitweb/?p=autoconf-archive.git;a=blob_plain;f=m4/ax_boost_thread.m4`
- `http://git.savannah.gnu.org/gitweb/?p=autoconf-archive.git;a=blob_plain;f=m4/ax_boost_system.m4`
- `Tue Oct  7 09:34:37 PDT 2014`
- `MIT-like`

`m4/ax_check_class.m4, m4/ax_check_rqrd_class.m4, m4/ax_prog_jar.m4, m4/ax_prog_javac.m4, m4/ax_prog_javac_works.m4, m4/ax_prog_java.m4, m4/ax_prog_java_works.m4, m4/ax_try_compile_java.m4`
--------------------------------------------------------------------------------
`autoconf m4 macros for java detection`

- `http://www.gnu.org/software/autoconf-archive/ax_check_class.html`
- `http://www.gnu.org/software/autoconf-archive/ax_check_rqrd_class.html`
- `http://www.gnu.org/software/autoconf-archive/ax_prog_jar.html`
- `http://www.gnu.org/software/autoconf-archive/ax_prog_javac.html`
- `http://www.gnu.org/software/autoconf-archive/ax_prog_javac_works.html`
- `http://www.gnu.org/software/autoconf-archive/ax_prog_java.html`
- `http://www.gnu.org/software/autoconf-archive/ax_prog_java_works.html`
- `http://www.gnu.org/software/autoconf-archive/ax_try_compile_java.html`
- `Fri Mar  4 00:01:29 PST 2011`
- `GPL` (build-time dependency only)

`m4/ax_lib_socket_nsl.m4`
-------------------------

- `http://www.gnu.org/software/autoconf-archive/ax_lib_socket_nsl.html`
- `Fri Oct 31 20:36:10 PDT 2014`
- `MIT-like`

`m4/ax_prog_python_version.m4`
------------------------------
`autoconf m4 macros for python detection`

- `http://www.gnu.org/software/autoconf-archive/ax_prog_python_version.html`
- `Sun Mar  6 13:31:11 PST 2011`
- `MIT-like`

`m4/ax_prog_ruby_version.m4, m4/ax_compare_version.m4`
------------------------------------------------------
`autoconf m4 macros for ruby detection`

- `http://www.gnu.org/software/autoconf-archive/ax_prog_ruby_version.html`
- `http://www.gnu.org/software/autoconf-archive/ax_compare_version.html`
- `Fri Mar  4 00:19:03 PST 2011`
- `MIT-like`

`msgpack`
---------
`MessagePack in Erlang`

- `https://github.com/msgpack/msgpack-erlang`
- `commit c666a5f6568ac2e15687bb155442025dbb8b74ff (0.3.3)`
- `Mon May 25 11:14:25 PDT 2015`
- `Apache License 2.0`

`nodefinder`
------------
`Strategies for automatic node discovery in Erlang`

- `https://github.com/okeuday/nodefinder`
- `commit 82766e3790b8235eaedf045cbd6ebfc4e332d40c (v1.5.1)`
- `Wed Dec 23 18:41:56 PST 2015`
- `BSD`

`parse_trans`
-------------
`Erlang parse transforms`

- `https://github.com/uwiger/parse_trans`
- `commit 82cc00264aa1bad8fc5c0739b7541feb4a843432 (2.9)`
- `Fri Dec 11 15:51:53 PST 2015`
- `Erlang Public License`

`pgsql`
-------
`Erlang PostgreSQL (native) driver (semiocast branch)`

- `https://github.com/semiocast/pgsql`
- `commit e40786b5c3e76dcfe502cbad702ed1b5e50b616e`
- `Tue Sep  1 21:59:06 PDT 2015`
- `BSD`

`poolboy`
---------
`Poolboy - A hunky Erlang worker pool factory`

- `https://github.com/devinus/poolboy`
- `1.0.0`
- `Tue Apr 16 13:47:17 EDT 2013`
- `Apache License 2.0`

`proper`
--------
`PropEr (PROPerty-based testing tool for ERlang)`

- `https://github.com/manopapad/proper`
- `commit 87e4a56e8f97f1972cde44ed30500daa4641595f (v1.1)`
- `Tue Jan 14 11:33:13 PST 2014`
- `GPLv3` (build/test-time dependency only)

`protobuffs`
------------
`Erlang Protocol Buffers`

- `https://github.com/basho/erlang_protobuffs`
- `commit ec7e99f57f5bbcd91c56242091485d41bc78c3d4 (0.8.1p1)`
- `Fri Jun 20 12:11:49 PDT 2014`
- `MIT`

`ranch`
-------
`Erlang Socket acceptor pool for TCP protocols`

- `https://github.com/ninenines/ranch`
- `commit adf1822defc2b7cfdc7aca112adabfa1d614043c (1.0.0)`
- `Mon Aug 11 10:57:20 PDT 2014`
- `MIT`

`rebar`
-------
`Erlang OTP-compliant build tool`

- `https://github.com/rebar/rebar`
- `commit ebb474b7593527969a8ce7d43b42f00e4498c391 (2.1.0-pre)`
- `Tue Feb 12 12:32:53 PST 2013`
- `Apache License 2.0`

`riak_pb`
---------
`Erlang Riak Protocol Buffers`

- `https://github.com/basho/riak_pb`
- `commit 0353be3e04b971ac802acbd6b78d74589db2f6df (1.4.4.0)`
- `Fri Jun 20 12:11:49 PDT 2014`
- `Apache License 2.0`

`riakc`
-------
`Erlang Riak driver`

- `https://github.com/basho/riak-erlang-client`
- `commit 8d33c020f4ca392200b2d0d973c77dd48164b263 (1.4.2)`
- `Fri Jun 20 12:11:49 PDT 2014`
- `Apache License 2.0`

`setup`
-------
`Erlang Setup Application`

- `https://github.com/uwiger/setup`
- `commit 51ee7c9f64d2bbe9dcbb58c278e8fbfd4d0ca5e2 (1.4)`
- `Fri Dec 11 15:51:53 PST 2015`
- `Mozilla Public License 2.0`

`snappy`
--------
`An Erlang NIF wrapper for Google's snappy compressor/decompressor`

- `https://github.com/rpt/snappy`
- `commit 95ce8e8074ca4a514ba8396d7f14e5ec572a8e45`
- `Fri Jun 20 12:11:49 PDT 2014`
- `Apache License 2.0`

`syslog`
--------
`Erlang syslog port driver`

- `https://github.com/okeuday/erlang-syslog`
- `commit e24c9ee8f7bb3f066ec152c210af10c2c712759a (1.0.3)`
- `Sat Aug  8 12:52:40 PDT 2015`
- `BSD`

`thrift`
--------
`Erlang Thrift software library`

- `https://github.com/dieswaytoofast/thrift`
- `commit 1e04570a702b782df600c31537105f46ce0813f2 (0.9.3)`
- `Sat May 31 13:06:15 PDT 2014`
- `Apache License 2.0`

`zeromq/v?/erlzmq`
------------------
`ZeroMQ (http://www.zeromq.org/) Erlang Driver`

- `https://github.com/okeuday/erlzmq2 (branch master and 3.x)`
- `commit 9171c69d94b2af72dbe74cfce8f4b61266026e33` (master)
- `commit e75f3c8cdf050fc08c00a184d91c4bcbace7c683` (3.x)
- `Mon Mar 11 12:52:17 PDT 2013`
- `BSD`

`zeromq/v?/zeromq`
------------------
`ZeroMQ 2.2.0 and 3.2.4`

- `http://download.zeromq.org/zeromq-2.2.0.tar.gz`
- `http://download.zeromq.org/zeromq-3.2.4.tar.gz`
- `Fri Jan 17 11:48:27 PST 2014`
- `LGPL` (requires explicit enable from configure)

