AC_INIT([CloudI], [0.1.0], [mjtruog.at.gmail...], [cloudi], [http://cloudi.org/])
AC_PREREQ([2.59])
AC_CONFIG_AUX_DIR(config)
AM_INIT_AUTOMAKE([1.10 no-define])
AC_CONFIG_HEADERS([config.h])
#AC_CONFIG_SRCDIR(src/foobar.cpp)
AC_PROG_CC
AC_PROG_CXX
BOOST_REQUIRE([1.33])
BOOST_FIND_HEADER([boost/preprocessor.hpp])
AC_ERLANG_SUBST_ERTS_VER
AC_ERLANG_SUBST_ROOT_DIR
AC_ERLANG_SUBST_LIB_DIR
AC_ERLANG_CHECK_LIB([erl_interface])
AC_CONFIG_COMMANDS([rebar], [(cd external/rebar/; 
                              ./bootstrap && mv ./rebar ../../ || exit 1)])
AC_CONFIG_FILES([Makefile
                 lib/Makefile
                 lib/cloudi/Makefile
                 lib/cloudi/cxx_src/Makefile])
AC_OUTPUT
