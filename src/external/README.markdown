External Dependencies
=====================

All CloudI Erlang dependencies are now moved to exist with a `"cloudi_x_"`
prefix to avoid any conflicts when CloudI shares the Erlang VM with other
applications that may share common dependencies (erlzmq2 is an exception due
to its integration and dependencies).

`cowboy`
--------
`Erlang HTTP Server`
- `https://github.com/extend/cowboy`
- `commit 9eaee45c090b9cb170554bc7e09d679bc0598716 (version 0.8.6)`
- `Thu Jun 20 10:33:01 PDT 2013`
- `MIT`

`dynamic_compile`
-----------------
`Dynamic compilation of Erlang modules`
- `https://github.com/JacobVorreuter/dynamic_compile`
- `commit 6a165715178dae8ea1c72e6ef171b9fc3435badc`
- `Mon Mar  4 15:43:22 PST 2013`
- `BSD`

`ecouchdb (erlang_couchdb)`
---------------------------
`Erlang CouchDB (native) Driver`

- `http://github.com/ngerakines/erlang_couchdb`
- `commit e8b1a107d05f5e886de9`
- `October 14, 2009`
- `BSD`

`elli`
--------
`Erlang HTTP Server`
- `https://github.com/knutin/elli/`
- `commit e95fa69d9ad3819b59c707a2d44cee81fed26893 (version 0.4.1)`
- `Thu Jun 27 09:13:14 PDT 2013`
- `MIT`

`ememcached (previously mcerlang)`
----------------------------------
`Erlang memcached Driver`

- `modified from http://github.com/JacobVorreuter/mcerlang`
- `commit (repository was removed)`
- `October 14, 2009`
- `BSD`

`emysql (erlang_mysql)`
-----------------------
 `Erlang MySQL (native) Driver`
 
- `http://github.com/ngerakines/erlang_mysql`
- `alternative: https://github.com/Eonblast/Emysql`
- `commit 712f8ead7585e6d3736a`
- `October 14, 2009`

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
- `tag v1.3`
- `Thu Sep 26 15:50:56 2013 -0400`
- `BSD`

`erlang_cassandra`
----------------
`Erlang Cassandra (thrift) Driver`

- `https://github.com/dieswaytoofast/erlang_cassandra`
- `tag v1.0`
- `Mon Jun 10 08:01:52 EST 2013`
- `BSD`

`etokyotyrant (medici)`
-----------------------
`Erlang TokyoTyrant (native) Driver`

- `https://github.com/mccoy/medici`
- `commit d4eafd6dd402d0c3bc42`
- `October 14, 2009`
- `Erlang Public License`

`jinterface`
------------
`Binary Erlang Term Encoding Java Source Code`

- `http://www.erlang.org/download/otp_src_R16A_RELEASE_CANDIDATE.tar.gz`
- `Mon Feb  4 13:03:02 PST 2013`
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
- `commit f0f43c4abab4d4e889bff79e5d5aab675f9e8d4d (master, v1.4.1)`
- `Mon Jun  3 08:57:57 PDT 2013`
- `MIT`

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
- `GPL`

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

- `http://code.google.com/p/nodefinder/`
- `r84`
- `2012-06-16 23:10:00 PST`
- `New BSD`

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
- `commit aa6b088607a08b82ef765dfdfcb6f20f0735915f (version 1.1)`
- `Tue Jul 16 11:26:20 PDT 2013`
- `GPLv3`

`ranch`
-------
`Erlang Socket acceptor pool for TCP protocols`

- `https://github.com/extend/ranch`
- `commit 4f70a16bbe08e446e8fec248ca345d5f8e7df038 (version 0.8.4)`
- `Thu Jun 20 10:33:01 PDT 2013`
- `MIT`

`rebar`
-------
`Erlang OTP-compliant build tool`

- `https://github.com/rebar/rebar`
- `commit ebb474b7593527969a8ce7d43b42f00e4498c391 (2.1.0-pre)`
- `Tue Feb 12 12:32:53 PST 2013`
- `Apache License`

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
`ZeroMQ 2.2.0 and 3.2.2`

- `http://download.zeromq.org/zeromq-2.2.0.tar.gz`
- `http://download.zeromq.org/zeromq-3.2.2.tar.gz`
- `Thu Feb  7 14:18:10 PST 2013`
- `LGPL`

