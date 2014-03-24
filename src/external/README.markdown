External Dependencies
=====================

All CloudI Erlang dependencies are now moved to exist with a `"cloudi_x_"`
prefix to avoid any conflicts when CloudI shares the Erlang VM with other
applications that may share common dependencies (erlzmq2 and syslog are
exceptions due to their C integration).

`backward-cpp`
--------------
`for creating a backtraces in C++`
- `https://github.com/bombela/backward-cpp`
- `commit c486368c543f10127431098a57d04c88a442296f`
- `Wed Nov 27 12:15:07 PST 2013`
- `MIT`

`booster (only booster/backtrace.h)`
------------------------------------
`booster/backtrace.h, from booster, from CppCMS,
 for creating a backtraces in C++`
- `https://svn.code.sf.net/p/cppcms/code/framework/trunk/booster/`
- `revision r2237 2013-01-06 12:34:05 -0800 (CppCMS 1.0.4)`
- `Wed Nov 27 12:15:07 PST 2013`
- `Boost Software License v1`

`cowboy`
--------
`Erlang HTTP Server`
- `https://github.com/extend/cowboy`
- `commit df818625545d565b8b76fbc445063a4822114b93 (0.9.0)`
- `Fri Nov 15 13:24:03 PST 2013`
- `MIT`

`cowlib`
--------
`cowboy Protocols`
- `https://github.com/extend/cowlib`
- `commit 63298e8e160031a70efff86a1acde7e7db1fcda6 (0.4.0)`
- `Fri Nov 15 13:24:03 PST 2013`
- `MIT`

`dynamic_compile`
-----------------
`Dynamic compilation of Erlang modules`
- `https://github.com/okeuday/dynamic_compile`
- `commit 993bb0eba609fc80bd7481365febfc05f4eb51e0`
- `Thu Jan  2 13:08:39 PST 2014`
- `BSD`

`ecouchdb (erlang_couchdb)`
---------------------------
`Erlang CouchDB (native) Driver`

- `https://github.com/okeuday/ecouchdb`
- `commit dd8bf6d694327feea202c6552cdb09c650831ba5`
- `Tue Oct 22 11:58:41 PDT 2013`
- `BSD`

`elli`
--------
`Erlang HTTP Server`
- `https://github.com/knutin/elli/`
- `commit e95fa69d9ad3819b59c707a2d44cee81fed26893 (0.4.1)`
- `Thu Jun 27 09:13:14 PDT 2013`
- `MIT`

`ememcached (previously mcerlang)`
----------------------------------
`Erlang memcached Driver`

- `https://github.com/okeuday/ememcached`
- `commit a44340caa9b14cefb590ba47c3baa2ac83270dcc`
- `Tue Oct 22 11:58:41 PDT 2013`
- `BSD`

`emysql (erlang_mysql)`
-----------------------
 `Erlang MySQL (native) Driver`
 
- `https://github.com/okeuday/emysql`
- `alternative: https://github.com/Eonblast/Emysql`
- `commit 13b3957b82f3647cb0255ac7eaeaba49e3ef9579`
- `Tue Oct 22 11:58:41 PDT 2013`

`epgsql`
--------
`Erlang PostgreSQL (native) Driver`

- `https://github.com/okeuday/epgsql`
- `commit b1118003b70c9c71ca8f24a246275b0ce9a9b0a0`
- `Mon Jul  8 11:10:51 PDT 2013`
- `BSD`

`erlasticsearch`
----------------
`Erlang ElasticSearch (thrift) Driver`

- `https://github.com/dieswaytoofast/erlasticsearch`
- `tag v1.4.4`
- `Sat Oct 26 09:57:55 2013 -0400`
- `BSD`

`erlang_cassandra`
----------------
`Erlang Cassandra (thrift) Driver`

- `https://github.com/dieswaytoofast/erlang_cassandra`
- `commit 6d026bfb06f8efd53e63a32f9b887a01bcc550ad`
- `Tue Dec 17 09:29:27 PST 2013`
- `BSD`

`erlcloud`
----------
`Cloud Service Integration APIs`

- `https://github.com/gleber/erlcloud`
- `commit 2c47abb3ac8f03d8a3c65778e8957f677da82fd3`
- `Fri Mar 14 09:50:36 PDT 2014`
- `MIT`

`etokyotyrant (medici)`
-----------------------
`Erlang TokyoTyrant (native) Driver`

- `https://github.com/okeuday/etokyotyrant`
- `commit 469ed7086157e06f74174d32bfb83fd9152384fb`
- `Tue Oct 22 11:58:41 PDT 2013`
- `Erlang Public License`

`goldrush`
----------
`Small, Fast Event Processing for Erlang`

- `https://github.com/DeadZen/goldrush`
- `commit 1d883423ac360b3536ca52dc733a0164bacf3109 (0.1.5)`
- `Tue Nov 19 09:19:54 PST 2013`
- `MIT`

`jinterface`
------------
`Binary Erlang Term Encoding Java Source Code`

- `http://www.erlang.org/download/otp_src_R16B02.tar.gz`
- `(in otp_src_R16B02/lib/jinterface/java_src/com/ericsson/otp/erlang/)`
- `Tue Oct 22 11:58:41 PDT 2013`
- `Erlang Public License`

`jsonrpclib`
------------
`Python JSON-RPC library`

- `https://github.com/joshmarshall/jsonrpclib`
- `commit efcf5eee0e12ef8a148c`
- `Tue Apr 12 18:07:44 PDT 2011`
- `Apache License`

`jsx`
-----
`Erlang JSON Parsing`

- `https://github.com/talentdeficit/jsx`
- `commit d011411c23e9f8c29d98aa1b7a50ddf9709b6a60 (master, v1.4.5)`
- `Mon Dec 30 13:31:51 PST 2013`
- `MIT`

`lager`
-------
`Erlang Logging`

- `https://github.com/basho/lager`
- `commit ad400896af5b1ad8b4f7a4d34e609b5a990640bb (2.0.1)`
- `Tue Nov 19 09:19:54 PST 2013`
- `Apache License 2.0`

`m4/ax_boost_base.m4, m4/ax_boost_thread.m4, m4/ax_boost_system.m4`
-------------------------------------------------------------------
`autoconf m4 macros for boost detection`

- `http://www.gnu.org/software/autoconf-archive/ax_boost_base.html`
- `http://www.gnu.org/software/autoconf-archive/ax_boost_thread.html`
- `http://www.gnu.org/software/autoconf-archive/ax_boost_system.html`
- `Mon Oct 15 23:13:23 PDT 2012`
- `BSD-like`

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

`m4/ax_prog_python_version.m4`
------------------------------
`autoconf m4 macros for python detection`

- `http://www.gnu.org/software/autoconf-archive/ax_prog_python_version.html`
- `Sun Mar  6 13:31:11 PST 2011`
- `BSD-like`

`m4/ax_prog_ruby_version.m4, m4/ax_compare_version.m4`
------------------------------------------------------
`autoconf m4 macros for ruby detection`

- `http://www.gnu.org/software/autoconf-archive/ax_prog_ruby_version.html`
- `http://www.gnu.org/software/autoconf-archive/ax_compare_version.html`
- `Fri Mar  4 00:19:03 PST 2011`
- `BSD-like`

`nodefinder`
------------
`Strategies for automatic node discovery in Erlang`

- `https://github.com/okeuday/nodefinder`
- `commit ba36f27de8c9c47fd4ca3fbfa4d33bd635896160`
- `Mon Mar 24 11:42:10 PDT 2014`
- `BSD`

`poolboy`
---------
`Poolboy - A hunky Erlang worker pool factory`

- `https://github.com/devinus/poolboy`
- `1.0.0`
- `Tue Apr 16 13:47:17 EDT 2013`
- `Apache License`

`proper`
--------
`PropEr (PROPerty-based testing tool for ERlang)`

- `https://github.com/manopapad/proper`
- `commit 87e4a56e8f97f1972cde44ed30500daa4641595f (v1.1)`
- `Tue Jan 14 11:33:13 PST 2014`
- `GPLv3` (build/test-time dependency only)

`ranch`
-------
`Erlang Socket acceptor pool for TCP protocols`

- `https://github.com/extend/ranch`
- `commit 33e1b4fcb0954f61588cb1baff905d107f74b467 (0.9.0)`
- `Fri Nov 15 13:24:03 PST 2013`
- `MIT`

`rebar`
-------
`Erlang OTP-compliant build tool`

- `https://github.com/rebar/rebar`
- `commit ebb474b7593527969a8ce7d43b42f00e4498c391 (2.1.0-pre)`
- `Tue Feb 12 12:32:53 PST 2013`
- `Apache License`

`syslog`
--------
`Erlang syslog port driver`

- `https://github.com/okeuday/erlang-syslog`
- `commit bce08564f78c997f25f47a0e1426c9c6d990e434`
- `Wed Feb  5 18:44:11 PST 2014`
- `BSD`

`thrift`
--------
`Erlang Thrift software library`

- `https://github.com/dieswaytoofast/thrift`
- `commit 0b8efd728f2ff2106d5e15e3c65c8ef895451c0a`
- `Thu Sep 26 16:19:26 2013 -0400`
- `Apache License`

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

