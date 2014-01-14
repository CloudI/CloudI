#-*-Mode:autoconf;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=config fenc=utf-8 sts=4 ts=4 sw=4 et:

AC_INIT([CloudI], [1.3.0], [mjtruog at gmail dot com],
    [cloudi], [http://cloudi.org/])

# paths to check for installations that are not standard but typically used
# (for any libraries that don't depend on executables for paths)
PATHS_NONSYSTEM_INC=""
AC_CHECK_FILE("/opt/local/include",
              [PATHS_NONSYSTEM_INC="/opt/local/include $PATHS_NONSYSTEM_INC"])
AC_CHECK_FILE("/usr/local/include",
              [PATHS_NONSYSTEM_INC="/usr/local/include $PATHS_NONSYSTEM_INC"])
PATHS_NONSYSTEM_LIB=""
AC_CHECK_FILE("/opt/local/lib",
              [PATHS_NONSYSTEM_LIB="/opt/local/lib $PATHS_NONSYSTEM_LIB"])
AC_CHECK_FILE("/usr/local/lib",
              [PATHS_NONSYSTEM_LIB="/usr/local/lib $PATHS_NONSYSTEM_LIB"])

AC_PREREQ([2.59])
AC_CONFIG_AUX_DIR(config)
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([1.10 no-define no-dependencies])
AC_PROG_MKDIR_P
AC_PROG_CC
AC_PROG_CXX
AC_PROG_SED
AC_PROG_LIBTOOL
AM_PROG_CC_C_O
AX_CLOCK_GETTIME

# now in autoconf 2.69
AC_SUBST(abs_top_srcdir, [`cd $srcdir; pwd`])
AC_SUBST(abs_top_builddir, [`pwd`])

AC_ARG_WITH([version],
    [AS_HELP_STRING([--with-version@<:@=ARG@:>@],
        [put the CloudI version within the installation path prefix (ARG=yes),
         or use a constant CloudI installation path prefix (ARG=no)
         @<:@ARG=yes@:>@ ])],
    [
    if test "x$withval" = "xno"; then
        cloudi_prefix="$libdir/cloudi"
    elif test "x$withval" = "xyes"; then
        cloudi_prefix="$libdir/cloudi-$VERSION"
    else
        cloudi_prefix="$libdir/cloudi-$VERSION"
    fi
    ],
    [cloudi_prefix="$libdir/cloudi-$VERSION"])
AC_SUBST(cloudi_prefix)
AC_SUBST(cloudi_logdir, ["$localstatedir/log/cloudi"])
AC_SUBST(cloudi_confdir, ["$sysconfdir/cloudi"])
AC_SUBST(cloudi_bindir, ["$cloudi_prefix/bin"])

AC_SUBST(CLOUDI_TMP_BIN_SCRIPT,
         m4_maketemp(/tmp/cloudi_tmp_bin_script_XXXXXX))

# c++ boost checks
AX_BOOST_BASE([1.40], ,
    [AC_MSG_ERROR([Boost >= 1.40 not found, see http://boost.org/])])
AX_BOOST_THREAD
AX_BOOST_SYSTEM
AX_BOOST_CHECK_HEADER(boost/exception/all.hpp, ,
    [AC_MSG_ERROR([boost::exception not found])], ,
    $PATHS_NONSYSTEM_INC)
AX_BOOST_CHECK_HEADER(boost/preprocessor.hpp, ,
    [AC_MSG_ERROR([boost::preprocessor not found])], ,
    $PATHS_NONSYSTEM_INC)
AX_BOOST_CHECK_HEADER(boost/unordered_map.hpp, ,
    [AC_MSG_ERROR([boost::unordered_map not found])], ,
    $PATHS_NONSYSTEM_INC)
# c++ backtrace check for adding exception information
AC_ARG_WITH([cxx-backtrace],
    [AS_HELP_STRING([--with-cxx-backtrace@<:@=ARG@:>@],
        [the C++ CloudI API will provide the backtrace from
         CloudI::API::backtrace() (ARG=yes),
         or will not include backtrace support (ARG=no)
         @<:@ARG=no@:>@ ])],
    [
    if test "x$withval" = "xyes"; then
        AX_BACKTRACE()
    fi
    ])

# java checks
AC_SUBST(javadir, ["$datarootdir/java"])
AC_ARG_VAR([JAVAC],[javac executable])
AC_ARG_VAR([JAVA],[java executable])
AC_ARG_VAR([JAR],[jar executable])
AC_PATH_PROG([JAVAC],[javac])
AX_PROG_JAVAC
AC_PATH_PROG([JAVA],[java])
AX_PROG_JAVA
AC_PATH_PROG([JAR],[jar])
AX_PROG_JAR
AX_CHECK_RQRD_CLASS(java.nio.ByteBuffer, ,
    [AC_MSG_ERROR([java nio not found])])
AX_CHECK_RQRD_CLASS(java.lang.reflect.Constructor, ,
    [AC_MSG_ERROR([java reflection not found])])
# new in jdk 5.0
AX_CHECK_RQRD_CLASS(java.util.concurrent.ExecutorService, ,
    [AC_MSG_ERROR([java concurrent not found])])

# python check
AC_ARG_WITH([python-debug],
    [AS_HELP_STRING([--with-python-debug@<:@=ARG@:>@],
        [use the debug version of python libraries and the
         python executable (ARG=yes), or use the normal version (ARG=no)
         @<:@ARG=no@:>@ ])],
    [
    if test "x$withval" = "xno"; then
        PYTHON_DEBUG="no"
    elif test "x$withval" = "xyes"; then
        PYTHON_DEBUG="yes"
    else
        PYTHON_DEBUG="yes"
    fi
    ],
    [PYTHON_DEBUG="no"])
AC_ARG_VAR([PYTHON],[python executable])
AS_IF([test "x$PYTHON_DEBUG" == "xno"],
    [AC_PATH_PROGS([PYTHON],[python2 python2.7 python])],
    [AC_PATH_PROGS([PYTHON],[python2-dbg python2.7-dbg python-dbg])])
AX_PROG_PYTHON_VERSION([2.7.0],
    [AX_PYTHON_C],
    [AC_MSG_ERROR([Python >= 2.7.0 not found])])

# ruby checks
AC_ARG_VAR([RUBY],[ruby executable])
AC_PATH_PROGS([RUBY],
              [ruby1.9.4 ruby1.9.3 ruby1.9.2 ruby1.9.1 ruby1.9.0 ruby1.9 ruby])
AX_PROG_RUBY_VERSION([1.9.0], ,
    [AC_MSG_ERROR([Ruby >= 1.9.0 not found])])

# erlang checks
AC_ERLANG_NEED_ERLC()
AC_ERLANG_NEED_ERL()
AX_ERLANG_REQUIRE_OTP_VER([R16A00], ,
    [AC_MSG_ERROR([Erlang >= R16 not found])])
AC_ERLANG_SUBST_ERTS_VER
AC_ERLANG_SUBST_ROOT_DIR
AX_ERLANG_SUBST_OTP_VER
AC_ERLANG_SUBST_LIB_DIR
AC_ERLANG_CHECK_LIB([erl_interface])
AC_ERLANG_CHECK_LIB([ssl], , [AC_MSG_ERROR([Erlang ssl required for epgsql])])
AC_PATH_PROG([ESCRIPT],[escript], ,[`AS_DIRNAME([$ERLC])`])
AC_PATH_PROG([DIALYZER],[dialyzer], ,[`AS_DIRNAME([$ERLC])`])

# rebar dependency
AC_ARG_VAR([REBAR],[rebar executable])
AC_ARG_WITH([rebar],
    [AS_HELP_STRING([--with-rebar@<:@=ARG@:>@],
        [compile rebar executable for erlang dependency compilation (ARG=yes),
         or use a preexisting rebar executable (ARG=no) @<:@ARG=yes@:>@ ])],
    [want_rebar="$withval"], [want_rebar="yes"])
if test "x$want_rebar" = "xyes"; then
    AC_CONFIG_COMMANDS([rebar],
        [(cd $SRCDIR/external/rebar/ && \
          $ESCRIPT bootstrap && \
          mv ./rebar $BUILDDIR && \
          echo "Rebar built" || exit 1)],
        [ESCRIPT=$ESCRIPT
         SRCDIR=$abs_top_srcdir
         BUILDDIR=$abs_top_builddir])
    REBAR="$abs_top_builddir/rebar"
else
    AC_PATH_PROG([REBAR], [rebar], ,
                 ["$abs_top_builddir" "$abs_top_builddir/../.." $PATH])
    if test -z "$REBAR"; then
        AC_MSG_ERROR([rebar not found (use --with-rebar)])
    fi
fi
AC_SUBST(REBAR)

# hexpi test
AX_GMP($PATHS_NONSYSTEM_INC, $PATHS_NONSYSTEM_LIB)

# ZeroMQ dependency
AX_ZEROMQ($PATHS_NONSYSTEM_INC, $PATHS_NONSYSTEM_LIB)
AX_ZEROMQ_ERLZMQ

AC_CONFIG_FILES([
    cloudi.conf
    rebar_src.config
    rebar_src_test.config
    reltool.config
    lib/cloudi_core/src/cloudi_core.app.src
    lib/cloudi_service_api_requests/src/cloudi_service_api_requests.app.src
    lib/cloudi_service_db/src/cloudi_service_db.app.src
    lib/cloudi_service_db_cassandra/src/cloudi_service_db_cassandra.app.src
    lib/cloudi_service_db_couchdb/src/cloudi_service_db_couchdb.app.src
    lib/cloudi_service_db_elasticsearch/src/cloudi_service_db_elasticsearch.app.src
    lib/cloudi_service_db_memcached/src/cloudi_service_db_memcached.app.src
    lib/cloudi_service_db_mysql/src/cloudi_service_db_mysql.app.src
    lib/cloudi_service_db_pgsql/src/cloudi_service_db_pgsql.app.src
    lib/cloudi_service_db_tokyotyrant/src/cloudi_service_db_tokyotyrant.app.src
    lib/cloudi_service_filesystem/src/cloudi_service_filesystem.app.src
    lib/cloudi_service_http_cowboy/src/cloudi_service_http_cowboy.app.src
    lib/cloudi_service_http_elli/src/cloudi_service_http_elli.app.src
    lib/cloudi_service_map_reduce/src/cloudi_service_map_reduce.app.src
    lib/cloudi_service_quorum/src/cloudi_service_quorum.app.src
    lib/cloudi_service_router/src/cloudi_service_router.app.src
    lib/cloudi_service_tcp/src/cloudi_service_tcp.app.src
    lib/cloudi_service_timers/src/cloudi_service_timers.app.src
    lib/cloudi_service_udp/src/cloudi_service_udp.app.src
    lib/cloudi_service_work_manager/src/cloudi_service_work_manager.app.src
    lib/cloudi_service_zeromq/src/cloudi_service_zeromq.app.src
    Makefile
    api/Makefile
    api/c/Makefile
    api/java/Makefile
    api/java/org/Makefile
    api/java/org/cloudi/Makefile
    api/java/com/Makefile
    api/java/com/ericsson/Makefile
    api/java/com/ericsson/otp/Makefile
    api/java/com/ericsson/otp/erlang/Makefile
    api/java/jar/Makefile
    api/python/Makefile
    lib/Makefile
    lib/cloudi_core/Makefile
    lib/cloudi_core/cxx_src/Makefile
    tests/Makefile
    tests/flood/Makefile
    tests/flood/src/Makefile
    tests/flood/service/Makefile
    tests/flood/service/jar/Makefile
    tests/flood/service/org/Makefile
    tests/flood/service/org/cloudi/Makefile
    tests/flood/service/org/cloudi/tests/Makefile
    tests/flood/service/org/cloudi/tests/flood/Makefile
    tests/hexpi/Makefile
    tests/hexpi/src/Makefile
    tests/hexpi/cxx_src/Makefile
    tests/http/Makefile
    tests/http/service/Makefile
    tests/http/service/jar/Makefile
    tests/http/service/org/Makefile
    tests/http/service/org/cloudi/Makefile
    tests/http/service/org/cloudi/tests/Makefile
    tests/http/service/org/cloudi/tests/http/Makefile
    tests/http_req/Makefile
    tests/http_req/c_src/Makefile
    tests/http_req/jar/Makefile
    tests/http_req/org/Makefile
    tests/http_req/org/cloudi/Makefile
    tests/http_req/org/cloudi/tests/Makefile
    tests/http_req/org/cloudi/tests/http_req/Makefile
    tests/http_req/src/Makefile
    tests/messaging/Makefile
    tests/messaging/jar/Makefile
    tests/messaging/org/Makefile
    tests/messaging/org/cloudi/Makefile
    tests/messaging/org/cloudi/tests/Makefile
    tests/messaging/org/cloudi/tests/messaging/Makefile
    tests/messaging/src/Makefile
    tests/messaging/cxx_src/Makefile
    tests/msg_size/Makefile
    tests/msg_size/jar/Makefile
    tests/msg_size/org/Makefile
    tests/msg_size/org/cloudi/Makefile
    tests/msg_size/org/cloudi/tests/Makefile
    tests/msg_size/org/cloudi/tests/msg_size/Makefile
    tests/msg_size/src/Makefile
    tests/msg_size/cxx_src/Makefile
    ])
AC_OUTPUT

