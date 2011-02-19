AC_INIT([CloudI], [0.1.0], [mjtruog at gmail dot com], [cloudi],
        [http://cloudi.org/])
AC_PREREQ([2.59])
AC_CONFIG_AUX_DIR(config)
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([1.10 no-define])
AC_PROG_CC
AC_PROG_CXX
AC_PROG_RANLIB
AC_PROG_LIBTOOL
AC_CHECK_LIB(gmp, __gmpz_init, ,
             [AC_MSG_ERROR([GNU MP not found, see http://gmplib.org/])])
AX_CLOCK_GETTIME

AX_BOOST_BASE([1.36], ,
              [AC_MSG_ERROR([Boost >= 1.36 not found, see http://boost.org/])])
AX_BOOST_THREAD
AX_CXX_CHECK_HEADER(boost/preprocessor.hpp, ,
                    [AC_MSG_ERROR([boost::preprocessor not found])], )
AX_CXX_CHECK_HEADER(boost/unordered_map.hpp, ,
                    [AC_MSG_ERROR([boost::unordered_map not found])], )

AC_ERLANG_NEED_ERLC()
AC_ERLANG_NEED_ERL()
AC_ERLANG_SUBST_ERTS_VER
AC_ERLANG_SUBST_ROOT_DIR
AC_ERLANG_SUBST_LIB_DIR
AC_ERLANG_CHECK_LIB([erl_interface])
AC_CONFIG_COMMANDS([rebar], [(cd external/rebar/; 
                              ./bootstrap && mv ./rebar ../../ || exit 1)])
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
                 lib/Makefile
                 lib/cloudi/Makefile
                 lib/cloudi/cxx_src/Makefile
                 tests/Makefile
                 tests/hexpi/Makefile
                 tests/hexpi/src/Makefile
                 tests/hexpi/cxx_src/Makefile])
AC_OUTPUT
