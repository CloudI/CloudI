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
- `commit cc507789bf1d9ea27e9fb53d06dc8a464672b7da (version 0.8.2)`
- `Sun Mar 10 13:07:51 PDT 2013`
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
- `alternatives are: https://github.com/joearms/elib1/blob/master/lib/src/elib1_mysql.erl, https://github.com/dizzyd/erlang-mysql-driver`
- `commit 712f8ead7585e6d3736a`
- `October 14, 2009`
- `BSD`

`epgsql`
--------
`Erlang PostgreSQL (native) Driver`

- `https://github.com/wg/epgsql`
- `commit 3318bd5d646cad0623ae9dcc6df015bb85258a63`
- `Fri Jul  6 19:45:00 PDT 2012`
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
- `commit cb14b92a228e51f7bdcaa7eb17f035e4a5c0b47d (master, v1.3.3)`
- `Mon Feb  4 12:57:40 PST 2013`
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

`ranch`
-------
`Erlang Socket acceptor pool for TCP protocols`

- `https://github.com/extend/ranch`
- `commit 809a12fdbe5ce355b06bcf197008e1b4e8ea9e21 (version 0.6.2)`
- `Sun Mar 10 13:07:51 PDT 2013`
- `MIT`

`rebar`
-------
`Erlang OTP-compliant build tool`

- `https://github.com/rebar/rebar`
- `commit ebb474b7593527969a8ce7d43b42f00e4498c391 (2.1.0-pre)`
- `Tue Feb 12 12:32:53 PST 2013`
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

