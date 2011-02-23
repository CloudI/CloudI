dnl Note that the `version' file needs to be created/deleted externally from vsn.mk
AC_INIT([zmq], m4_normalize(m4_include(m4/version)), [http://github.com/saleyn/erlzmq/issues])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([foreign no-dist -Wall -Werror])

AC_PROG_SED
AC_PROG_CXX
AC_CONFIG_HEADERS([config.h])

dnl ------------------------------------------------------------------
dnl Libtool.
dnl ------------------------------------------------------------------

dnl LT_INIT(disable-static)

dnl AM_DISABLE_STATIC
dnl AM_ENABLE_SHARED

dnl AC_PROG_LIBTOOL
dnl AC_PATH_PROG(PKG_CONFIG, pkg-config, [AC_MSG_ERROR([pkg-config was not found!])])

AC_CANONICAL_HOST

dnl ------------------------------------------------------------------
dnl Erlang environment.
dnl ------------------------------------------------------------------

AC_MSG_NOTICE([Erlang environment])

dnl Check for erl_interface (used by port drivers).

AC_ERLANG_CHECK_LIB([kernel],, 
                    [AC_MSG_ERROR([kernel was not found!])])
AC_ERLANG_CHECK_LIB([stdlib],, 
                    [AC_MSG_ERROR([stdlib was not found!])])
AC_ERLANG_CHECK_LIB([erl_interface],, 
                    [AC_MSG_ERROR([erl_interface was not found!])])

dnl Available flags.
AC_ARG_WITH([erlang],
	AC_HELP_STRING([--with-erlang=PATH],
	    [PATH to Erlang installation (optional)]),
	with_erlang=${withval%/},
	with_erlang="")

dnl erl(1) is used to compile Erlang modules.
if test "x${with_erlang}" = "x"; then
	AC_ERLANG_PATH_ERL
	AC_ERLANG_PATH_ERLC
    AC_ERLANG_SUBST_ROOT_DIR
else
	erl_path="${with_erlang}/bin"
	AC_ERLANG_PATH_ERL(,  [$erl_path$PATH_SEPARATOR$PATH])
	AC_ERLANG_PATH_ERLC(, [$erl_path$PATH_SEPARATOR$PATH])
    AC_ERLANG_SUBST_ROOT_DIR
fi

if test "x${ERL}" = "x"; then
	AC_MSG_ERROR([
Erlang not found. Fill the ERL variable with erl(1) path or provide
Erlang prefix with --with-erlang.])
fi

dnl Get ERTS version.
ERLANG_CHECK_ERTS
ERLANG_CHECK_RELEASE
AC_ERLANG_SUBST_ERTS_VER

dnl Determine directories for installation.
if test "x${prefix}" = "xNONE"; then
	dnl Inside Erlang lib directory.
    AC_SUBST([PACKAGE_INSTALL_DIR], [${ERLANG_ERTS_DIR%/*}/lib/${PACKAGE}-${VERSION}])
else
	dnl Under $prefix
    AC_SUBST([PACKAGE_INSTALL_DIR], [${prefix}/${PACKAGE}-${VERSION}])
fi

AC_CHECK_HEADER([${ERLANG_DRV_INCLUDE}/erl_driver.h], [], [
    AC_MSG_ERROR([Could not find the 'erl_driver.h' header.
Are the Erlang headers installed? Use the '--with-erlang' option to specify the
path to the Erlang include directory.])])


dnl ------------------------------------------------------------------
dnl Options.
dnl ------------------------------------------------------------------

dnl Debugging option.
AC_ARG_ENABLE([debug],
	AC_HELP_STRING([--enable-debug],
	    [turn on debugging [[default=yes]]]),,
	enable_debug=no)
if test "x${enable_debug}" = "xyes"; then
	AC_DEFINE([DEBUG], [], [Enable debug])
    AC_SUBST([DEBUG], [1])
	AC_SUBST([ERL_FLAGS], [+debug_info -Ddebug])
	CXXFLAGS="-O0 -DDEBUG $CXXFLAGS"
else
    AC_SUBST([ERL_FLAGS], [])
	CXXFLAGS="-O3 -DNDEBUG $CXXFLAGS"
fi

dnl Treat warnings as errors.
AC_ARG_ENABLE([warnings],
	AC_HELP_STRING([--enable-warnings],
	    [treat warnings as errors [[default=yes]]]),,
	enable_warnings="yes")
if test "x${enable_warnings}" = "xyes" -a "x${GCC}" = "xyes"; then
	CXXFLAGS="${CXXFLAGS% } -Werror"
fi

CXXFLAGS="-g ${CXXFLAGS% } -I${ERLANG_DRV_INCLUDE} -I${ERLANG_LIB_DIR_erl_interface}/include"

dnl ------------------------------------------------------------------
dnl Documentation.
dnl ------------------------------------------------------------------

AC_ARG_ENABLE([documentation],
	AC_HELP_STRING([--enable-docs],
	    [build documentation [[default=yes]]]),,
	enable_docs="yes")
AM_CONDITIONAL(ENABLE_DOCUMENTATION, test "x${enable_docs}" = "xyes")

dnl ------------------------------------------------------------------
dnl Examples.
dnl ------------------------------------------------------------------

AC_ARG_ENABLE([examples],
	AC_HELP_STRING([--enable-examples],
	    [build examples [[default=yes]]]),,
	enable_examples="yes")
AM_CONDITIONAL(ENABLE_EXAMPLES, test "x${enable_examples}" = "xyes")

dnl ------------------------------------------------------------------
dnl Static
dnl ------------------------------------------------------------------

AC_ARG_ENABLE([static],
	AC_HELP_STRING([--enable-static],
	    [link with zeromq static library [[default=no]]]),,
	enable_static="no")
AM_CONDITIONAL(ENABLE_STATIC, test "x${enable_static}" = "xyes")

dnl ------------------------------------------------------------------
dnl ZeroMQ
dnl ------------------------------------------------------------------

AC_ARG_WITH([zeromq], 
    AC_HELP_STRING([--with-zeromq=PATH],
    [set PATH to the installation of zeromq library [[default=/usr]]]),
    [ZEROMQ_LIB=${with_zeromq}],
    [ZEROMQ_LIB="/usr"])

AC_CHECK_HEADER([${ZEROMQ_LIB}/include/zmq.h], [], [
    AC_MSG_ERROR([Could not find the 'zmq.h' header.
Use the '--with-zeromq=PATH' option to specify location of ZeroMQ])])

CXXFLAGS="${CXXFLAGS# } -I${with_zeromq}/include"

case "${enable_static}" in
    *yes*)
    LDFLAGS="${LDFLAGS# } ${with_zeromq}/lib/libzmq.a"
    ;;
    *)
    case "${host_os}" in
        *darwin*)
            LDFLAGS="${LDFLAGS# } -L${with_zeromq}/lib -lzmq"
        ;;
        *)
            LDFLAGS="${LDFLAGS# } -L${with_zeromq}/lib -lzmq -Wl,-R${with_zeromq}/lib"
        ;;
    esac
    ;;
esac

dnl ------------------------------------------------------------------
dnl OS specific tests.
dnl ------------------------------------------------------------------

case "${host_os}" in
    *darwin*)
	DARWIN_CXXFLAGS="-dynamiclib -m64 -Wl,-undefined -Wl,dynamic_lookup"
        CXXFLAGS="-D_DARWIN_C_SOURCE $DARWIN_CXXFLAGS $CXXFLAGS"
	AC_SUBST([LIB_EXT], [.so])
	;;
    *)
	AC_SUBST([LIB_EXT], [.so]);;
esac

case $(uname -p) in
    i386*)
        CXXFLAGS="-m32 $CXXFLAGS";;
    x86_64*)
        CXXFLAGS="-m64 $CXXFLAGS";;
esac

dnl ------------------------------------------------------------------
dnl Output.
dnl ------------------------------------------------------------------

AC_MSG_NOTICE([Erlang environment])

dnl Check for erl_interface (used by port drivers).
AC_CONFIG_FILES([Makefile c_src/Makefile src/Makefile 
                 ebin/zmq.app:src/zmq.app.in])
AC_OUTPUT

dnl --------------------------------------------------
dnl Configuration report
dnl --------------------------------------------------

echo
AC_MSG_NOTICE([    == Package: ${PACKAGE}-${VERSION} ==])
echo
AC_MSG_NOTICE([    Host OS:            ${host_os}])
AC_MSG_NOTICE([    Install dir:        ${PACKAGE_INSTALL_DIR}])
echo
AC_MSG_NOTICE([    C++ compiler:       ${CXX}])
AC_MSG_NOTICE([    CXXFLAGS:           ${CXXFLAGS}])
AC_MSG_NOTICE([    LDFLAGS:            ${LDFLAGS}])
AC_MSG_NOTICE([    Erlang compiler:    ${ERLC}])
AC_MSG_NOTICE([    Erlang erts dir:    ${ERLANG_ERTS_DIR}])
AC_MSG_NOTICE([    Erlang erts ver:    ${ERLANG_ERTS_VER}])
AC_MSG_NOTICE([    erl_interface:      ${ERLANG_LIB_DIR_erl_interface}])
AC_MSG_NOTICE([    ZeroMQ:             ${ZEROMQ_LIB}])

echo
AC_MSG_NOTICE([    Debug/warnings:     ${enable_debug}/${enable_warnings}])
AC_MSG_NOTICE([    Documentation:      ${enable_docs}])
AC_MSG_NOTICE([    Examples:           ${enable_examples}])

echo
AC_MSG_NOTICE([    Static:              ${enable_static}])

echo
