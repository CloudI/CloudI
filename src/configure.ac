AC_INIT([CloudI], [0.1.0],
        [mjtruog at gmail dot com], [cloudi], [http://cloudi.org/])
AC_PREREQ([2.59])
AC_CONFIG_AUX_DIR(config)
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_FILES([lib/cloudi/src/cloudi.app.src])
AM_INIT_AUTOMAKE([1.10 no-define])
AC_PROG_CC
AC_PROG_CXX
AC_PROG_RANLIB
AC_PROG_LIBTOOL
AC_CHECK_LIB(gmp, __gmpz_init, ,
             [AC_MSG_ERROR([GNU MP not found, see http://gmplib.org/])])
AX_CLOCK_GETTIME

# c++ boost checks
AX_BOOST_BASE([1.36], ,
              [AC_MSG_ERROR([Boost >= 1.36 not found, see http://boost.org/])])
AX_BOOST_THREAD
AX_CXX_CHECK_HEADER(boost/preprocessor.hpp, ,
                    [AC_MSG_ERROR([boost::preprocessor not found])], )
AX_CXX_CHECK_HEADER(boost/unordered_map.hpp, ,
                    [AC_MSG_ERROR([boost::unordered_map not found])], )

# java checks
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

# ruby checks
AC_PATH_PROG([RUBY],[ruby])
AX_PROG_RUBY_VERSION([1.8.0], ,
                     [AC_MSG_ERROR([Ruby >= 1.8.0 not found])])
AC_PATH_PROG([RUBY_RAKE],[rake])
if test "x$RUBY_RAKE" = "x"; then
    AC_MSG_ERROR([ruby rake not found])
fi
AC_PATH_PROG([RUBY_GEM],[gem])
if test "x$RUBY_GEM" = "x"; then
    AC_MSG_ERROR([ruby gem not found])
fi

# erlang checks
AC_ERLANG_NEED_ERLC()
AC_ERLANG_NEED_ERL()
AX_ERLANG_REQUIRE_OTP_VER([R14B01], ,
                          [AC_MSG_ERROR([Erlang >= R14B01 not found])])
AC_ERLANG_SUBST_ROOT_DIR
AC_ERLANG_SUBST_LIB_DIR
AC_ERLANG_CHECK_LIB([erl_interface])
AC_ERLANG_CHECK_LIB([ssl], , [AC_MSG_ERROR([Erlang ssl required for epgsql])])
AC_PATH_PROG([ESCRIPT],[escript], ,[`AS_DIRNAME([$ERLC])`])
AC_CONFIG_COMMANDS([rebar], [(cd external/rebar/; 
                              $ESCRIPT bootstrap &&
                              mv ./rebar ../../ || exit 1)],
                   [ESCRIPT=$ESCRIPT])

AC_CONFIG_FILES([Makefile
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
                 api/ruby/Makefile
                 api/ruby/gems/Makefile
                 lib/Makefile
                 lib/cloudi/Makefile
                 lib/cloudi/cxx_src/Makefile
                 tests/Makefile
                 tests/hexpi/Makefile
                 tests/hexpi/src/Makefile
                 tests/hexpi/cxx_src/Makefile
                 tests/http/Makefile
                 tests/http/service/Makefile
                 tests/http/service/jar/Makefile
                 tests/http/service/org/Makefile
                 tests/http/service/org/cloudi/Makefile
                 tests/http/service/org/cloudi/tests/Makefile
                 tests/http/service/org/cloudi/tests/http/Makefile])
AC_OUTPUT
