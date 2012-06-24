AC_INIT([CloudI], [1.0.0],
        [mjtruog at gmail dot com], [cloudi], [http://cloudi.org/])
AC_PREREQ([2.59])
AC_CONFIG_AUX_DIR(config)
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([1.10 no-define])
AC_PROG_CC
AC_PROG_CXX
AC_PROG_SED
AC_PROG_RANLIB
AC_PROG_LIBTOOL
AM_PROG_CC_C_O
AX_CHECK_PRIVATE_LIB(gmp, __gmpz_init, ,
                     [AC_MSG_ERROR([GNU MP not found, see http://gmplib.org/])])
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
AX_CXX_CHECK_HEADER(boost/preprocessor.hpp, ,
                    [AC_MSG_ERROR([boost::preprocessor not found])], )
AX_CXX_CHECK_HEADER(boost/unordered_map.hpp, ,
                    [AC_MSG_ERROR([boost::unordered_map not found])], )

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

# python check
AC_ARG_VAR([PYTHON],[python executable])
AC_PATH_PROG([PYTHON],[python])
AX_PROG_PYTHON_VERSION([2.5.0], ,
                       [AC_MSG_ERROR([Python >= 2.5.0 not found])])

# ruby checks
AC_ARG_VAR([RUBY],[ruby executable])
AC_PATH_PROGS([RUBY],[ruby1.9 ruby])
AX_PROG_RUBY_VERSION([1.9.0], ,
                     [AC_MSG_ERROR([Ruby >= 1.9.0 not found])])

# erlang checks
AC_ERLANG_NEED_ERLC()
AC_ERLANG_NEED_ERL()
AX_ERLANG_REQUIRE_OTP_VER([R14B01], ,
                          [AC_MSG_ERROR([Erlang >= R14B01 not found])])
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
AX_ZEROMQ
AX_ZEROMQ_ERLZMQ

AC_CONFIG_FILES([cloudi.conf
                 rebar.config
                 reltool.config
                 lib/cloudi/src/cloudi.app.src
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
                 lib/Makefile
                 lib/cloudi/Makefile
                 lib/cloudi/cxx_src/Makefile
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

