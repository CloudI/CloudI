AC_INIT([CloudI], [1.2.2], [mjtruog at gmail dot com],
    [cloudi], [http://cloudi.org/])

# paths to check for installations that are not standard but typically used
# (for any libraries that don't depend on executables for paths)
PATHS_NONSYSTEM_INC="/opt/local/include /usr/local/include"
PATHS_NONSYSTEM_LIB="/opt/local/lib /usr/local/lib"

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
AX_CHECK_PRIVATE_HEADER(gmp.h, ,
    [AC_MSG_ERROR([GNU MP not found, see http://gmplib.org/])], ,
    $PATHS_NONSYSTEM_INC)
AX_CHECK_PRIVATE_LIB(gmp, __gmpz_init, ,
    [AC_MSG_ERROR([GNU MP not found, see http://gmplib.org/])], ,
    $PATHS_NONSYSTEM_LIB)
AX_CLOCK_GETTIME

AC_SUBST(abs_top_srcdir, [`cd $srcdir; pwd`])
AC_SUBST(abs_top_builddir, [`pwd`])

AC_SUBST(cloudi_prefix, ["$libdir/cloudi-$VERSION"])
AC_SUBST(cloudi_logdir, ["$localstatedir/log/cloudi"])
AC_SUBST(cloudi_confdir, ["$sysconfdir/cloudi"])
AC_SUBST(cloudi_bindir, ["$cloudi_prefix/bin"])

# c++ boost checks
AX_BOOST_BASE([1.36], ,
    [AC_MSG_ERROR([Boost >= 1.36 not found, see http://boost.org/])])
AX_BOOST_THREAD
AX_BOOST_SYSTEM
AX_BOOST_CHECK_HEADER(boost/preprocessor.hpp, ,
    [AC_MSG_ERROR([boost::preprocessor not found])], ,
    $PATHS_NONSYSTEM_INC)
AX_BOOST_CHECK_HEADER(boost/unordered_map.hpp, ,
    [AC_MSG_ERROR([boost::unordered_map not found])], ,
    $PATHS_NONSYSTEM_INC)

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
AC_PATH_PROGS([RUBY],[ruby1.9.4 ruby1.9.3 ruby1.9.2 ruby1.9.1 ruby1.9.0 ruby1.9 ruby])
AX_PROG_RUBY_VERSION([1.9.0], ,
    [AC_MSG_ERROR([Ruby >= 1.9.0 not found])])

# erlang checks
AC_ERLANG_NEED_ERLC()
AC_ERLANG_NEED_ERL()
AX_ERLANG_REQUIRE_OTP_VER([R15B01], ,
    [AC_MSG_ERROR([Erlang >= R15B01 not found])])
AC_ERLANG_SUBST_ERTS_VER
AC_ERLANG_SUBST_ROOT_DIR
AX_ERLANG_SUBST_OTP_VER
AC_ERLANG_SUBST_LIB_DIR
AC_ERLANG_CHECK_LIB([erl_interface])
AC_ERLANG_CHECK_LIB([ssl], , [AC_MSG_ERROR([Erlang ssl required for epgsql])])
AC_PATH_PROG([ESCRIPT],[escript], ,[`AS_DIRNAME([$ERLC])`])
AC_PATH_PROG([DIALYZER],[dialyzer], ,[`AS_DIRNAME([$ERLC])`])

# rebar dependency
AC_CONFIG_COMMANDS([rebar],
    [(cd $SRCDIR/external/rebar/ && \
      $ESCRIPT bootstrap && \
      mv ./rebar $BUILDDIR && \
      echo "Rebar built" || exit 1)],
    [ESCRIPT=$ESCRIPT
     SRCDIR=$abs_top_srcdir
     BUILDDIR=$abs_top_builddir])

# ZeroMQ dependency
AX_ZEROMQ($PATHS_NONSYSTEM_INC, $PATHS_NONSYSTEM_LIB)
AX_ZEROMQ_ERLZMQ

AC_CONFIG_FILES([
    cloudi.conf
    rebar.config
    reltool.config
    lib/cloudi_core/src/cloudi_core.app.src
    lib/cloudi_services_internal/src/cloudi_services_internal.app.src
    lib/cloudi_services_databases/src/cloudi_services_databases.app.src
    lib/cloudi_services_messaging/src/cloudi_services_messaging.app.src
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

