AC_INIT([CloudI], [0.1.0], [mjtruog.at.gmail...], [cloudi], [http://cloudi.org/])
AC_PREREQ([2.59])
AC_CONFIG_AUX_DIR(config)
AC_CONFIG_HEADERS([config.h])
#AC_CONFIG_SRCDIR(src/foobar.cpp)
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([1.10 no-define])
AC_PROG_CC
AC_PROG_CXX
AC_PROG_RANLIB
AC_PROG_LIBTOOL
BOOST_REQUIRE([1.33])
BOOST_FIND_HEADER([boost/preprocessor.hpp])
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
                 lib/cloudi/cxx_src/Makefile])
AC_OUTPUT
