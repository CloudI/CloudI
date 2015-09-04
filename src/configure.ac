#-*-Mode:autoconf;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=config fenc=utf-8 sts=4 ts=4 sw=4 et nomod:

AC_INIT([CloudI], [1.5.0], [mjtruog at gmail dot com],
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
AM_INIT_AUTOMAKE([1.10 no-define no-dependencies foreign])
AC_PROG_MKDIR_P
AC_PROG_CC
AC_PROG_CXX
AC_PROG_SED
AC_PROG_LIBTOOL
AM_PROG_CC_C_O

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
        AC_MSG_ERROR([--with-version needs yes or no, not $withval])
    fi
    ],
    [cloudi_prefix="$libdir/cloudi-$VERSION"])
AC_SUBST(cloudi_prefix)
AC_SUBST(cloudi_logdir, ["$localstatedir/log/cloudi"])
AC_SUBST(cloudi_confdir, ["$sysconfdir/cloudi"])
AC_SUBST(cloudi_bindir, ["$cloudi_prefix/bin"])

AC_SUBST(CLOUDI_TMP_BIN_SCRIPT,
         m4_maketemp(/tmp/cloudi_tmp_bin_script_XXXXXX))

AC_MSG_CHECKING([if the C          CloudI API is enabled])
AC_ARG_ENABLE([c-support],
    [AS_HELP_STRING([--enable-c-support],
                    [Enable the C CloudI API])],
    [
    if test "x$enableval" = "xyes"; then
        c_support="true"
    elif test "x$enableval" = "xno"; then
        c_support="false"
    else
        AC_MSG_ERROR([bad value "$enableval" for --enable-c-support])
    fi
    ], [c_support="true"])
AC_MSG_RESULT([$c_support])
AC_MSG_CHECKING([if the C++        CloudI API is enabled])
AC_ARG_ENABLE([cxx-support],
    [AS_HELP_STRING([--enable-cxx-support],
                    [Enable the C++ CloudI API])],
    [
    if test "x$enableval" = "xyes"; then
        cxx_support="true"
    elif test "x$enableval" = "xno"; then
        cxx_support="false"
    else
        AC_MSG_ERROR([bad value "$enableval" for --enable-cxx-support])
    fi
    ], [cxx_support="true"])
if test "x$cxx_support" = "xtrue" ; then
    if test "x$c_support" = "xfalse" ; then
        AC_MSG_RESULT([failed])
        AC_MSG_ERROR(
            [--disable-c-support requires --disable-cxx-support])
    fi
fi
AC_MSG_RESULT([$cxx_support])
AC_MSG_CHECKING([if the Java       CloudI API is enabled])
AC_ARG_ENABLE([java-support],
    [AS_HELP_STRING([--enable-java-support],
                    [Enable the Java CloudI API])],
    [
    if test "x$enableval" = "xyes"; then
        java_support="true"
    elif test "x$enableval" = "xno"; then
        java_support="false"
    else
        AC_MSG_ERROR([bad value "$enableval" for --enable-java-support])
    fi
    ], [java_support="true"])
AC_MSG_RESULT([$java_support])
AC_MSG_CHECKING([if the JavaScript CloudI API is enabled])
AC_ARG_ENABLE([javascript-support],
    [AS_HELP_STRING([--enable-javascript-support],
                    [Enable the JavaScript/node.js CloudI API])],
    [
    if test "x$enableval" = "xyes"; then
        javascript_support="true"
    elif test "x$enableval" = "xno"; then
        javascript_support="false"
    else
        AC_MSG_ERROR([bad value "$enableval" for --enable-javascript-support])
    fi
    ], [javascript_support="true"])
AC_MSG_RESULT([$javascript_support])
AC_MSG_CHECKING([if the Perl       CloudI API is enabled])
AC_ARG_ENABLE([perl-support],
    [AS_HELP_STRING([--enable-perl-support],
                    [Enable the Perl CloudI API])],
    [
    if test "x$enableval" = "xyes"; then
        perl_support="true"
    elif test "x$enableval" = "xno"; then
        perl_support="false"
    else
        AC_MSG_ERROR([bad value "$enableval" for --enable-perl-support])
    fi
    ], [perl_support="true"])
AC_MSG_RESULT([$perl_support])
AC_MSG_CHECKING([if the PHP        CloudI API is enabled])
AC_ARG_ENABLE([php-support],
    [AS_HELP_STRING([--enable-php-support],
                    [Enable the PHP CloudI API])],
    [
    if test "x$enableval" = "xyes"; then
        php_support="true"
    elif test "x$enableval" = "xno"; then
        php_support="false"
    else
        AC_MSG_ERROR([bad value "$enableval" for --enable-php-support])
    fi
    ], [php_support="true"])
AC_MSG_RESULT([$php_support])
AC_MSG_CHECKING([if the Python     CloudI API is enabled])
AC_ARG_ENABLE([python-support],
    [AS_HELP_STRING([--enable-python-support],
                    [Enable the Python CloudI API])],
    [
    if test "x$enableval" = "xyes"; then
        python_support="true"
    elif test "x$enableval" = "xno"; then
        python_support="false"
    else
        AC_MSG_ERROR([bad value "$enableval" for --enable-python-support])
    fi
    ], [python_support="true"])
AC_MSG_RESULT([$python_support])
AC_MSG_CHECKING([if the Python/C   CloudI API is enabled])
AC_ARG_ENABLE([python-c-support],
    [AS_HELP_STRING([--enable-python-c-support],
                    [Enable the Python CloudI API with C integration])],
    [
    if test "x$enableval" = "xyes"; then
        python_c_support="true"
    elif test "x$enableval" = "xno"; then
        python_c_support="false"
    else
        AC_MSG_ERROR([bad value "$enableval" for --enable-python-c-support])
    fi
    ], [python_c_support="true"])
if test "x$python_c_support" = "xtrue" ; then
    if test "x$c_support" = "xfalse" ; then
        AC_MSG_RESULT([failed])
        AC_MSG_ERROR(
            [--disable-c-support requires --disable-python-c-support])
    elif test "x$cxx_support" = "xfalse" ; then
        AC_MSG_RESULT([failed])
        AC_MSG_ERROR(
            [--disable-cxx-support requires --disable-python-c-support])
    fi
fi
AC_MSG_RESULT([$python_c_support])
AC_MSG_CHECKING([if the Ruby       CloudI API is enabled])
AC_ARG_ENABLE([ruby-support],
    [AS_HELP_STRING([--enable-ruby-support],
                    [Enable the Ruby CloudI API])],
    [
    if test "x$enableval" = "xyes"; then
        ruby_support="true"
    elif test "x$enableval" = "xno"; then
        ruby_support="false"
    else
        AC_MSG_ERROR([bad value "$enableval" for --enable-ruby-support])
    fi
    ], [ruby_support="true"])
AC_MSG_RESULT([$ruby_support])
AC_MSG_CHECKING([if the CloudI integration tests should be included])
AC_ARG_WITH([integration-tests],
    [AS_HELP_STRING([--with-integration-tests@<:@=ARG@:>@],
        [include the CloudI integration tests (ARG=yes),
         or exclude the CloudI integration tests from
         the installation (ARG=no)
         @<:@ARG=yes@:>@ ])],
    [
    if test "x$withval" = "xno"; then
        want_integration_tests="no"
    elif test "x$withval" = "xyes"; then
        want_integration_tests="yes"
    else
        AC_MSG_ERROR([--with-integration-tests needs yes or no, not $withval])
    fi
    ],
    [want_integration_tests="yes"])
if test "x$want_integration_tests" = "xyes"; then
    integration_tests_included="true"
else
    integration_tests_included="false"
fi
AC_MSG_RESULT([$integration_tests_included])
AM_CONDITIONAL([INTEGRATION_TESTS], [$integration_tests_included])
AC_MSG_CHECKING([if the CloudI integration tests should be ran])
AC_ARG_WITH([integration-tests-ran],
    [AS_HELP_STRING([--with-integration-tests-ran@<:@=ARG@:>@],
        [use the default CloudI configuration to run all the integration tests
         (ARG=yes), or use a blank CloudI configuration so nothing is ran by
         default within the installed CloudI configuration (ARG=no)
         @<:@ARG=yes@:>@ ])],
    [
    if test "x$withval" = "xno"; then
        want_integration_tests_ran="no"
    elif test "x$withval" = "xyes"; then
        if test "x$want_integration_tests" = "xno"; then
            AC_MSG_ERROR([--with-integration-tests-ran=yes
                          requires --with-integration-tests=yes])
        fi
        want_integration_tests_ran="yes"
    else
        AC_MSG_ERROR([--with-integration-tests-ran needs yes or no,
                      not $withval])
    fi
    ],
    [want_integration_tests_ran="$want_integration_tests"])
if test "x$want_integration_tests_ran" = "xyes" -a \
        "x$c_support" = "xtrue" -a "x$cxx_support" = "xtrue" -a \
        "x$java_support" = "xtrue" -a "x$javascript_support" = "xtrue" -a \
        "x$perl_support" = "xtrue" -a "x$php_support" = "xtrue" -a \
        "x$python_support" = "xtrue" -a "x$python_c_support" = "xtrue" -a \
        "x$ruby_support" = "xtrue" ; then
    integration_tests_ran="true"
else
    if test "x$want_integration_tests_ran" = "xyes" ; then
        AC_MSG_ERROR([--with-integration-tests-ran=yes
                      requires all programming languages be enabled])
    fi
    integration_tests_ran="false"
fi
AC_MSG_RESULT([$integration_tests_ran])

# C requirements
# setrlimit resource values:
# IEEE Std 1003.1-2008 (“POSIX.1”)
AX_CHECK_HEADER_MACRO(sys/resource.h, RLIMIT_CORE)
AX_CHECK_HEADER_MACRO(sys/resource.h, RLIMIT_CPU)
AX_CHECK_HEADER_MACRO(sys/resource.h, RLIMIT_DATA)
AX_CHECK_HEADER_MACRO(sys/resource.h, RLIMIT_FSIZE)
AX_CHECK_HEADER_MACRO(sys/resource.h, RLIMIT_NOFILE)
AX_CHECK_HEADER_MACRO(sys/resource.h, RLIMIT_STACK)
# Common, but not standard (both BSD/SYSV)
AX_CHECK_HEADER_MACRO(sys/resource.h, RLIMIT_MEMLOCK)
AX_CHECK_HEADER_MACRO(sys/resource.h, RLIMIT_NPROC)
AX_CHECK_HEADER_MACRO(sys/resource.h, RLIMIT_RSS)
# Linux variations
# (Ubuntu)
AX_CHECK_HEADER_MACRO(sys/resource.h, RLIMIT_AS)
AX_CHECK_HEADER_MACRO(sys/resource.h, RLIMIT_MSGQUEUE)
AX_CHECK_HEADER_MACRO(sys/resource.h, RLIMIT_NICE)
AX_CHECK_HEADER_MACRO(sys/resource.h, RLIMIT_RTPRIO)
AX_CHECK_HEADER_MACRO(sys/resource.h, RLIMIT_RTTIME)
AX_CHECK_HEADER_MACRO(sys/resource.h, RLIMIT_SIGPENDING)
# (SmartOS)
AX_CHECK_HEADER_MACRO(sys/resource.h, RLIMIT_VMEM)

# C++ requirements
AX_LIB_SOCKET_NSL
AX_BOOST_BASE([1.40], ,
    [AC_MSG_ERROR([Boost >= 1.40 not found, see http://boost.org/])])
AX_BOOST_SYSTEM
AX_BOOST_CHECK_HEADER(boost/preprocessor.hpp, ,
    [AC_MSG_ERROR([boost::preprocessor not found])], ,
    $PATHS_NONSYSTEM_INC)
# C/C++ CloudI API support
if test "x$c_support" = "xtrue" -o "x$cxx_support" = "xtrue" -o \
        "x$python_c_support" = "xtrue" ; then
AX_BOOST_THREAD
AX_CLOCK_GETTIME
AX_BOOST_CHECK_HEADER(boost/exception/all.hpp, ,
    [AC_MSG_ERROR([boost::exception not found])], ,
    $PATHS_NONSYSTEM_INC)
AX_BOOST_CHECK_HEADER(boost/unordered_map.hpp, ,
    [AC_MSG_ERROR([boost::unordered_map not found])], ,
    $PATHS_NONSYSTEM_INC)
# C++ CloudI API backtrace support
AC_ARG_WITH([cxx-backtrace],
    [AS_HELP_STRING([--with-cxx-backtrace@<:@=ARG@:>@],
        [the C++ CloudI API will provide the backtrace from
         CloudI::API::backtrace() (ARG=yes),
         or will not include backtrace support (ARG=no)
         @<:@ARG=no@:>@ ])],
    [
    if test "x$withval" = "xyes"; then
        AX_BACKTRACE()
    elif test "x$withval" != "xno"; then
        AC_MSG_ERROR([--with-cxx-backtrace needs yes or no, not $withval])
    fi
    ])
AM_CONDITIONAL([C_SUPPORT], [true])
else
AM_CONDITIONAL([C_SUPPORT], [false])
fi
AM_CONDITIONAL([CXX_SUPPORT], [test "x$cxx_support" = "xtrue"])

# Java CloudI API support
if test "x$java_support" = "xtrue" ; then
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
fi
AM_CONDITIONAL([JAVA_SUPPORT], [test "x$java_support" = "xtrue"])

# Javascript/node.js CloudI API support
if test "x$javascript_support" = "xtrue" ; then
AC_ARG_VAR([NODE],[node executable])
AC_PATH_PROGS([NODE],[nodejs node])
if test "x$NODE" = "x" ; then
    AC_MSG_ERROR([node.js not found])
fi
fi
AM_CONDITIONAL([JAVASCRIPT_SUPPORT], [test "x$javascript_support" = "xtrue"])

# Perl CloudI API support
if test "x$perl_support" = "xtrue" ; then
AC_ARG_VAR([PERL],[perl executable])
AC_PATH_PROGS([PERL],[perl])
if test "x$PERL" = "x" ; then
    AC_MSG_ERROR([perl not found])
fi
fi
AM_CONDITIONAL([PERL_SUPPORT], [test "x$perl_support" = "xtrue"])

# PHP CloudI API support
if test "x$php_support" = "xtrue" ; then
AC_ARG_VAR([PHP],[php executable])
AC_PATH_PROGS([PHP],[php])
if test "x$PHP" = "x" ; then
    AC_MSG_ERROR([php not found])
fi
fi
AM_CONDITIONAL([PHP_SUPPORT], [test "x$php_support" = "xtrue"])

# Python CloudI API support
if test "x$python_support" = "xtrue" -o "x$python_c_support" = "xtrue" ; then
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
        AC_MSG_ERROR([--with-python-debug needs yes or no, not $withval])
    fi
    ],
    [PYTHON_DEBUG="no"])
AC_ARG_VAR([PYTHON],[python executable])
AC_ARG_WITH([python_version],
    [AS_HELP_STRING([--with-python-version@<:@=ARG@:>@],
        [specify the version of Python (ARG=2|3)
         @<:@ARG=2@:>@ ])],
        [
        if test "$withval" = "2"; then
            PYTHON_VERSION_REQUIRED="2"
        elif test "$withval" = "3"; then
            PYTHON_VERSION_REQUIRED="3"
        else
            AC_MSG_ERROR([Python version 2 or 3, not $withval])
        fi
        ],
        [PYTHON_VERSION_REQUIRED="2"])
if test "x$PYTHON_VERSION_REQUIRED" = "x2"; then
    AS_IF([test "x$PYTHON_DEBUG" == "xno"],
        [AC_PATH_PROGS([PYTHON],[python2 python2.7 python])],
        [AC_PATH_PROGS([PYTHON],[python2-dbg python2.7-dbg python-dbg])])
    AX_PROG_PYTHON_VERSION([2.7.0],
        [AX_PYTHON_C],
        [AC_MSG_ERROR([Python >= 2.7.0 not found])])
elif test "x$PYTHON_VERSION_REQUIRED" = "x3"; then
    AS_IF([test "x$PYTHON_DEBUG" == "xno"],
        [AC_PATH_PROGS([PYTHON],[python3 python3.2 python])],
        [AC_PATH_PROGS([PYTHON],[python3-dbg python3.2-dbg python-dbg])])
    AX_PROG_PYTHON_VERSION([3.0.0],
        [AX_PYTHON_C],
        [AC_MSG_ERROR([Python >= 3.0.0 not found])])
fi
AM_CONDITIONAL([PYTHON_SUPPORT], [true])
else
AM_CONDITIONAL([PYTHON_SUPPORT], [false])
fi
AM_CONDITIONAL([PYTHON_C_SUPPORT], [test "x$python_c_support" = "xtrue"])

# Ruby CloudI API support
if test "x$ruby_support" = "xtrue" ; then
AC_ARG_VAR([RUBY],[ruby executable])
AC_PATH_PROGS([RUBY],
              [ruby1.9.4 ruby1.9.3 ruby1.9.2 ruby1.9.1 ruby1.9.0 ruby1.9 ruby])
AX_PROG_RUBY_VERSION([1.9.0], ,
    [AC_MSG_ERROR([Ruby >= 1.9.0 not found])])
fi
AM_CONDITIONAL([RUBY_SUPPORT], [test "x$ruby_support" = "xtrue"])

# erlang checks
AC_ERLANG_NEED_ERLC()
AC_ERLANG_NEED_ERL()
AX_ERLANG_REQUIRE_OTP_VER([R16], ,
    [AC_MSG_ERROR([Erlang >= R16 not found])])
AC_ERLANG_SUBST_ERTS_VER
AC_ERLANG_SUBST_ROOT_DIR
AX_ERLANG_SUBST_OTP_VER
AC_ERLANG_SUBST_LIB_DIR
AC_ERLANG_CHECK_LIB([erl_interface])
AC_ERLANG_CHECK_LIB([ssl], , [AC_MSG_ERROR([Erlang ssl required for epgsql])])
AC_PATH_PROG([ESCRIPT],[escript], ,[`AS_DIRNAME([$ERLC])`])
AC_PATH_PROG([DIALYZER],[dialyzer], ,[`AS_DIRNAME([$ERLC])`])
AC_SUBST([ERLC_OPTS],
         ["-DERLANG_OTP_VER_${ERLANG_OTP_VER} -DERLANG_OTP_VER_${ERLANG_OTP_VER_MAJOR} -I $abs_top_srcdir/lib +debug_info +strict_validation +warn_bif_clash +warn_deprecated_function +warn_export_all +warn_export_vars +warn_exported_vars +warn_obsolete_guard +warn_shadow_vars +warn_unused_import +warn_unused_function +warn_unused_record +warn_unused_vars"])
if test "$ERLANG_OTP_VER_MAJOR" -ge 18 ; then
ERL_OPTS="+C multi_time_warp +c true"
else
ERL_OPTS=""
fi
AC_SUBST(ERL_OPTS)

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
if test "x$want_integration_tests" = "xyes" -a \
        "x$cxx_support" = "xtrue"; then
AX_GMP($PATHS_NONSYSTEM_INC, $PATHS_NONSYSTEM_LIB)
fi

# ZeroMQ dependency
AX_ZEROMQ($PATHS_NONSYSTEM_INC, $PATHS_NONSYSTEM_LIB)
AX_ZEROMQ_ERLZMQ($PATHS_NONSYSTEM_INC, $PATHS_NONSYSTEM_LIB)

# service Erlang application
AC_ARG_WITH([service],
    [AS_HELP_STRING([--with-service@<:@=ARG@:>@],
        [include the service Erlang application (ARG=yes),
         or exclude the service Erlang application to avoid module name
         conflicts with external modules that use the service prefix (ARG=no)
         @<:@ARG=yes@:>@ ])],
    [
    if test "x$withval" = "xno"; then
        want_service="no"
    elif test "x$withval" = "xyes"; then
        want_service="yes"
    else
        AC_MSG_ERROR([--with-service needs yes or no, not $withval])
    fi
    ],
    [want_service="yes"])
if test "x$want_service" = "xyes"; then
    SERVICE_RELTOOL="{app, service, @<:@{incl_cond, include}, {mod_cond, all}, {app_file, keep}@:>@},"
    SERVICE_APPCONF="service,"
    SERVICE_PATH=',"lib/service"'
    AC_SUBST(SERVICE_RELTOOL)
    AC_SUBST(SERVICE_APPCONF)
    AC_SUBST(SERVICE_PATH)
fi

if test "x$integration_tests_ran" = "xtrue" ; then
    CLOUDI_CONF="cloudi_tests.conf"
else
    CLOUDI_CONF="cloudi_minimal.conf"
fi
AC_SUBST(CLOUDI_CONF)
AC_CONFIG_FILES([
    cloudi_tests.conf
    cloudi_minimal.conf
    rebar_src.config
    rebar_src_test.config
    reltool.config
    rel/files/vm.args
    Makefile
    lib/Makefile
    lib/cloudi_core/Makefile
    lib/cloudi_core/cxx_src/Makefile
    lib/cloudi_core/src/cloudi_core.app.src
    lib/cloudi_service_api_requests/src/cloudi_service_api_requests.app.src
    lib/cloudi_service_db/src/cloudi_service_db.app.src
    lib/cloudi_service_db_cassandra/src/cloudi_service_db_cassandra.app.src
    lib/cloudi_service_db_cassandra_cql/src/cloudi_service_db_cassandra_cql.app.src
    lib/cloudi_service_db_couchdb/src/cloudi_service_db_couchdb.app.src
    lib/cloudi_service_db_elasticsearch/src/cloudi_service_db_elasticsearch.app.src
    lib/cloudi_service_db_memcached/src/cloudi_service_db_memcached.app.src
    lib/cloudi_service_db_mysql/src/cloudi_service_db_mysql.app.src
    lib/cloudi_service_db_pgsql/src/cloudi_service_db_pgsql.app.src
    lib/cloudi_service_db_riak/src/cloudi_service_db_riak.app.src
    lib/cloudi_service_filesystem/src/cloudi_service_filesystem.app.src
    lib/cloudi_service_http_client/src/cloudi_service_http_client.app.src
    lib/cloudi_service_http_cowboy/src/cloudi_service_http_cowboy.app.src
    lib/cloudi_service_http_elli/src/cloudi_service_http_elli.app.src
    lib/cloudi_service_http_rest/src/cloudi_service_http_rest.app.src
    lib/cloudi_service_map_reduce/src/cloudi_service_map_reduce.app.src
    lib/cloudi_service_oauth1/src/cloudi_service_oauth1.app.src
    lib/cloudi_service_queue/src/cloudi_service_queue.app.src
    lib/cloudi_service_quorum/src/cloudi_service_quorum.app.src
    lib/cloudi_service_router/src/cloudi_service_router.app.src
    lib/cloudi_service_tcp/src/cloudi_service_tcp.app.src
    lib/cloudi_service_timers/src/cloudi_service_timers.app.src
    lib/cloudi_service_udp/src/cloudi_service_udp.app.src
    lib/cloudi_service_validate/src/cloudi_service_validate.app.src
    lib/cloudi_service_zeromq/src/cloudi_service_zeromq.app.src
    lib/service/src/service.app.src
    api/Makefile
    api/c/Makefile
    api/erlang/Makefile
    api/java/Makefile
    api/java/org/Makefile
    api/java/org/cloudi/Makefile
    api/java/com/Makefile
    api/java/com/ericsson/Makefile
    api/java/com/ericsson/otp/Makefile
    api/java/com/ericsson/otp/erlang/Makefile
    api/java/jar/Makefile
    api/javascript/Makefile
    api/perl/Makefile
    api/php/Makefile
    api/python/Makefile
    api/ruby/Makefile
    service_api/Makefile
    service_api/dashboard/Makefile
    service_api/dashboard/css/Makefile
    service_api/dashboard/css/images/Makefile
    service_api/dashboard/js/Makefile
    service_api/python/Makefile
    service_api/python/jsonrpclib/Makefile
    tests/Makefile
    tests/echo/Makefile
    tests/environment/Makefile
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
    tests/request_rate/Makefile
    tests/request_rate/src/Makefile
    tests/service_api/Makefile
    tests/websockets/Makefile
    tests/zeromq/Makefile
    ])
AC_OUTPUT

