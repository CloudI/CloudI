#-*-Mode:autoconf;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=config fenc=utf-8 sts=4 ts=4 sw=4 et nomod:

AC_INIT([CloudI], [2.0.1], [mjtruog at protonmail dot com],
        [cloudi], [https://cloudi.org/])
AC_SUBST(CLOUDI_CORE_RELEASE_VERSION, [1])

# build-time information
AC_SUBST(BUILD_MACHINE, [`uname -m`])
AC_SUBST(BUILD_KERNEL_VERSION, [`uname -srv`])
AC_SUBST(BUILD_TIME, [`date`])
BUILD_OPERATING_SYSTEM=`uname -o`
if test $? -ne 0; then
    BUILD_OPERATING_SYSTEM=`uname -s`
fi
AC_SUBST(BUILD_OPERATING_SYSTEM)

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
LT_PREREQ([2.4.2])
LT_INIT()
LT_LIB_M
AC_PROG_MKDIR_P
AC_PROG_CC
AC_PROG_CXX
AC_PROG_CXXCPP
AC_PROG_SED
AM_PROG_CC_C_O

# C++ compiler checks (including vendor and version information for code_status)
# (C++ compiler compilation checked here)
AX_CXX_EXCEPTIONS
if test "x$ax_cv_cxx_exceptions" = "xno"; then
    AC_MSG_ERROR([$CXX does not support exceptions])
fi
AC_LANG_PUSH(C++)
AX_COMPILER_VENDOR
AX_COMPILER_VERSION
AC_LANG_POP([C++])
AC_SUBST(CXX_COMPILER_VENDOR_VERSION,
         ["$ax_cv_cxx_compiler_vendor $ax_cv_cxx_compiler_version"])
AM_CONDITIONAL([CXX_CLANG], [test "x$ax_cv_cxx_compiler_vendor" = "xclang"])

# now in autoconf 2.69
AC_SUBST(abs_top_srcdir, [`cd $srcdir; pwd`])
AC_SUBST(abs_top_builddir, [`pwd`])

AX_RECURSIVE_EVAL("$libdir/cloudi-$VERSION", cloudi_prefix)
AX_RECURSIVE_EVAL("$localstatedir/log/cloudi", cloudi_logdir)
AX_RECURSIVE_EVAL("$sysconfdir/cloudi", cloudi_confdir)
AX_RECURSIVE_EVAL("$cloudi_prefix/bin", cloudi_bindir)
AC_SUBST(cloudi_prefix)
AC_SUBST(cloudi_logdir)
AC_SUBST(cloudi_confdir)
AC_SUBST(cloudi_bindir)

AC_MSG_CHECKING([if the C          CloudI API is enabled])
AC_ARG_ENABLE([c-support],
    [AS_HELP_STRING([--enable-c-support],
                    [Enable the C CloudI API
                     @<:@default=yes@:>@ ])],
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
                    [Enable the C++ CloudI API
                     @<:@default=yes@:>@ ])],
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
AC_MSG_CHECKING([if the Go         CloudI API is enabled])
AC_ARG_ENABLE([go-support],
    [AS_HELP_STRING([--enable-go-support],
                    [Enable the Go CloudI API
                     @<:@default=no@:>@ ])],
    [
    if test "x$enableval" = "xyes"; then
        go_support="true"
    elif test "x$enableval" = "xno"; then
        go_support="false"
    else
        AC_MSG_ERROR([bad value "$enableval" for --enable-go-support])
    fi
    ], [go_support="false"])
AC_MSG_RESULT([$go_support])
AC_MSG_CHECKING([if the Haskell    CloudI API is enabled])
AC_ARG_ENABLE([haskell-support],
    [AS_HELP_STRING([--enable-haskell-support],
                    [Enable the Haskell CloudI API
                     @<:@default=no@:>@ ])],
    [
    if test "x$enableval" = "xyes"; then
        haskell_support="true"
    elif test "x$enableval" = "xno"; then
        haskell_support="false"
    else
        AC_MSG_ERROR([bad value "$enableval" for --enable-haskell-support])
    fi
    ], [haskell_support="false"])
AC_MSG_RESULT([$haskell_support])
AC_MSG_CHECKING([if the Java       CloudI API is enabled])
AC_ARG_ENABLE([java-support],
    [AS_HELP_STRING([--enable-java-support],
                    [Enable the Java CloudI API
                     @<:@default=yes@:>@ ])],
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
                    [Enable the JavaScript/node.js CloudI API
                     @<:@default=yes@:>@ ])],
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
AC_MSG_CHECKING([if the OCaml      CloudI API is enabled])
AC_ARG_ENABLE([ocaml-support],
    [AS_HELP_STRING([--enable-ocaml-support],
                    [Enable the OCaml CloudI API
                     @<:@default=no@:>@ ])],
    [
    if test "x$enableval" = "xyes"; then
        ocaml_support="true"
    elif test "x$enableval" = "xno"; then
        ocaml_support="false"
    else
        AC_MSG_ERROR([bad value "$enableval" for --enable-ocaml-support])
    fi
    ], [ocaml_support="false"])
AC_MSG_RESULT([$ocaml_support])
AC_MSG_CHECKING([if the Perl       CloudI API is enabled])
AC_ARG_ENABLE([perl-support],
    [AS_HELP_STRING([--enable-perl-support],
                    [Enable the Perl CloudI API
                     @<:@default=yes@:>@ ])],
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
                    [Enable the PHP CloudI API
                     @<:@default=yes@:>@ ])],
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
                    [Enable the Python CloudI API
                     @<:@default=yes@:>@ ])],
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
                    [Enable the Python CloudI API with C integration
                     @<:@default=yes@:>@ ])],
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
                    [Enable the Ruby CloudI API
                     @<:@default=yes@:>@ ])],
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
         @<:@ARG=no@:>@ ])],
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
    [want_integration_tests_ran="no"])
if test "x$want_integration_tests_ran" = "xyes" -a \
        "x$c_support" = "xtrue" -a "x$cxx_support" = "xtrue" -a \
        "x$java_support" = "xtrue" -a "x$javascript_support" = "xtrue" -a \
        "x$perl_support" = "xtrue" -a "x$php_support" = "xtrue" -a \
        "x$python_support" = "xtrue" -a "x$python_c_support" = "xtrue" -a \
        "x$ruby_support" = "xtrue" ; then
    integration_tests_ran="true"
else
    if test "x$want_integration_tests_ran" = "xyes" ; then
        AC_MSG_ERROR([--with-integration-tests-ran=yes requires
                      C/C++/Java/Javascript/Perl/PHP/Python/Python-C/Ruby
                      programming languages are enabled])
    fi
    integration_tests_ran="false"
fi
AC_MSG_RESULT([$integration_tests_ran])

CLOUDI_TEST_VMARGS=""
AC_MSG_CHECKING([the CloudI Erlang test timeout])
AC_ARG_WITH([test-timeout],
    [AS_HELP_STRING([--with-test-timeout@<:@=ARG@:>@],
        [The maximum amount of time a CloudI test will wait for completion
         in seconds.  The default of 10 seconds is for bare-metal use
         (ARG=dev).  Execution in a vm should use ARG=vm to get 1 minute (60).
         Unreliable container execution should use ARG=container to get
         10 minutes (600) with a single scheduler (use ARG=container_cpu_quota
         to use the default schedulers).  Otherwise, the integer timeout
         can be provided as the argument (in seconds).
         @<:@ARG=dev@:>@ ])],
    [
    if test "x$withval" = "xdev"; then
        CLOUDI_TEST_TIMEOUT="10"
    elif test "x$withval" = "xvm"; then
        CLOUDI_TEST_TIMEOUT="60"
    elif test "x$withval" = "xcontainer"; then
        CLOUDI_TEST_TIMEOUT="600"
        CLOUDI_TEST_VMARGS="+S 1"
    elif test "x$withval" = "xcontainer_cpu_quota"; then
        CLOUDI_TEST_TIMEOUT="600"
    elif test "$withval" -ge 10; then
        CLOUDI_TEST_TIMEOUT="$withval"
    else
        AC_MSG_ERROR([--with-test-timeout needs dev, vm, container,
                      container_cpu_quota or an integer
                      (greater than or equal to 10), not $withval])
    fi
    ],
    [CLOUDI_TEST_TIMEOUT="10"])
AC_MSG_RESULT([$CLOUDI_TEST_TIMEOUT])
AC_SUBST(CLOUDI_TEST_TIMEOUT)

AC_MSG_CHECKING([the CloudI Erlang test arguments])
AC_MSG_RESULT([$CLOUDI_TEST_VMARGS])
AC_SUBST(CLOUDI_TEST_VMARGS)

AC_MSG_CHECKING([the CloudI Erlang long test timeout])
AC_ARG_WITH([long-test-timeout],
    [AS_HELP_STRING([--with-long-test-timeout@<:@=ARG@:>@],
        [The maximum amount of time a CloudI long test will wait for completion
         in minutes.  The default long test timeout is 60 minutes.
         @<:@ARG=no@:>@ ])],
    [
    if test "x$withval" = "xyes"; then
        CLOUDI_LONG_TEST_TIMEOUT="60"
    elif test "x$withval" = "xno"; then
        CLOUDI_LONG_TEST_TIMEOUT="0"
    elif test "$withval" -ge 1; then
        CLOUDI_LONG_TEST_TIMEOUT="$withval"
    else
        AC_MSG_ERROR([--with-long-test-timeout needs yes or no or an integer,
                      not $withval])
    fi
    ],
    [CLOUDI_LONG_TEST_TIMEOUT="0"])
AC_MSG_RESULT([$CLOUDI_LONG_TEST_TIMEOUT])
AC_SUBST(CLOUDI_LONG_TEST_TIMEOUT)

AC_MSG_CHECKING([the CloudI UUID v1 variant])
AC_ARG_WITH([uuid-variant],
    [AS_HELP_STRING([--with-uuid-variant@<:@=ARG@:>@],
        [The UUID v1 variant used for service ids and transaction ids
         can use the format defined in RFC4122 (ARG=rfc4122),
         or use the future definition variant
         to get lexicographical ordering among UUIDs
         (for database locality and performance when UUIDs
          are used as primary keys, ARG=ordered)
         @<:@ARG=rfc4122@:>@ ])],
    [
    if test "x$withval" = "xrfc4122"; then
        CLOUDI_UUID_V1_VARIANT="rfc4122"
    elif test "x$withval" = "xordered"; then
        CLOUDI_UUID_V1_VARIANT="ordered"
    else
        AC_MSG_ERROR([--with-uuid-variant needs rfc4122 or ordered,
                      not $withval])
    fi
    ],
    [CLOUDI_UUID_V1_VARIANT="rfc4122"])
AC_MSG_RESULT([$CLOUDI_UUID_V1_VARIANT])
AC_SUBST(CLOUDI_UUID_V1_VARIANT)

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

AC_CHECK_FUNC([chroot],
    [AC_DEFINE([HAVE_CHROOT], [1],
        [Define if chroot is available])])
AX_SYSCALL_LOCK()

# C++ requirements
AX_LIB_SOCKET_NSL
AX_BOOST_BASE([1.40], ,
    [AC_MSG_ERROR([Boost >= 1.40 not found, see http://boost.org/])])
AX_BOOST_SYSTEM
AX_BOOST_CHECK_HEADER(boost/preprocessor.hpp, ,
    [AC_MSG_ERROR([boost::preprocessor not found])], ,
    $PATHS_NONSYSTEM_INC)
AX_BOOST_VERSION()
AC_SUBST(CXX_DEPENDENCIES_VERSIONS,
         ["boost_$ax_cv_cxx_boost_version"])

# C/C++ CloudI API support
if test "x$c_support" = "xtrue" -o "x$cxx_support" = "xtrue" -o \
        "x$python_c_support" = "xtrue" ; then
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
         @<:@ARG=yes@:>@ ])],
    [
    if test "x$withval" = "xyes"; then
        AX_BACKTRACE()
    elif test "x$withval" != "xno"; then
        AC_MSG_ERROR([--with-cxx-backtrace needs yes or no, not $withval])
    fi
    ],
    [AX_BACKTRACE()])
AM_CONDITIONAL([C_SUPPORT], [true])
else
AM_CONDITIONAL([C_SUPPORT], [false])
fi
AM_CONDITIONAL([CXX_SUPPORT], [test "x$cxx_support" = "xtrue"])

# Go CloudI API support
if test "x$go_support" = "xtrue" ; then
AC_ARG_VAR([GO],[go gc executable (not gccgo)])
AC_PATH_PROGS([GO],[go])
if test "x$GO" = "x"; then
    AC_MSG_ERROR([go not found])
fi
AX_PROG_GO_VERSION([1.11], ,
    [AC_MSG_ERROR([go >= 1.11 not found])])
AC_SUBST(CONFIG_TESTS_GO_COUNT, ["[[{prefix, \"/tests/count/\"}, {file_path, \"$cloudi_prefix/tests/count/count_go\"}, {dest_refresh, none}, {count_thread, 4}, {env, [{\"GOMAXPROCS\", \"4\"}]}, {options, [{nice, 10}]}]],"])
AC_SUBST(CONFIG_TESTS_GO_HTTP_REQ, ["[[{prefix, \"/tests/http_req/\"}, {file_path, \"$cloudi_prefix/tests/http_req/http_req_go\"}, {dest_refresh, none}, {options, [{request_timeout_adjustment, true}, {nice, 10}]}]],"])
AC_SUBST(CONFIG_TESTS_GO_MESSAGING, ["[[{prefix, \"/tests/messaging/go/\"}, {file_path, \"$cloudi_prefix/tests/messaging/messaging_go\"}, {dest_refresh, immediate_local}, {timeout_async, 10000}, {timeout_sync, 10000}, {count_thread, 4}, {env, [{\"GOMAXPROCS\", \"4\"}]}, {options, [{dispatcher_pid_options, [{priority, low}]}, {nice, 15}, {scope, cloudi_service_test_messaging_go}]}]],"])
AC_SUBST(CONFIG_TESTS_GO_MSG_SIZE, ["[[{prefix, \"/tests/msg_size/\"}, {file_path, \"$cloudi_prefix/tests/msg_size/msg_size_go\"}, {options, [{request_timeout_adjustment, true}, {aspects_init_after, [{cloudi_service_test_msg_size, aspect_init}]}, {aspects_request_before, [{cloudi_service_test_msg_size, aspect_request}]}, {aspects_terminate_before, [{cloudi_service_test_msg_size, aspect_terminate}]}, {scope, cloudi_service_test_msg_size}, {nice, 15}]}]],"])
AC_SUBST(CONFIG_TESTS_GO_NULL, ["[[{prefix, \"/tests/null/response/\"}, {file_path, \"$cloudi_prefix/tests/null/null_go\"}, {dest_refresh, none}, {options, [{response_timeout_immediate_max, limit_min}, {nice, 10}]}]],[[{prefix, \"/tests/null/timeout/\"}, {file_path, \"$cloudi_prefix/tests/null/null_go\"}, {dest_refresh, none}, {options, [{response_timeout_immediate_max, limit_max}, {nice, 10}]}]],"])
fi
AM_CONDITIONAL([GO_SUPPORT], [test "x$go_support" = "xtrue"])

# Haskell CloudI API support
cabal_install_v2="false"
if test "x$haskell_support" = "xtrue" ; then
AC_ARG_VAR([GHC],[Glasgow Haskell Compiler executable])
AC_PATH_PROGS([GHC],[ghc])
if test "x$GHC" = "x"; then
    AC_MSG_ERROR([Glasgow Haskell Compiler (GHC) not found])
fi
AC_ARG_VAR([GHC_PKG],[GHC package manager])
AC_PATH_PROGS([GHC_PKG],[ghc-pkg])
if test "x$GHC_PKG" = "x"; then
    AC_MSG_ERROR([GHC package manager not found])
fi
AC_ARG_VAR([CABAL],[Haskell Cabal (cabal-install) executable])
AC_PATH_PROGS([CABAL],[cabal])
if test "x$CABAL" = "x"; then
    AC_MSG_ERROR([Haskell Cabal (cabal-install) not found])
fi
# Cabal >= 3.0.0.0 is included with GHC >= 8.8.1
AX_CABAL_VERSION([3.0.0.0],[cabal_install_v2="true"])
AC_SUBST([CABAL_SANDBOX], ["$abs_top_builddir/api/haskell/sandbox"])
AC_SUBST(CONFIG_TESTS_HASKELL_COUNT, ["[[{prefix, \"/tests/count/\"}, {file_path, \"$cloudi_prefix/tests/count/count_haskell\"}, {dest_refresh, none}, {count_thread, 4}, {options, [{nice, 10}]}]],"])
AC_SUBST(CONFIG_TESTS_HASKELL_HTTP_REQ, ["[[{prefix, \"/tests/http_req/\"}, {file_path, \"$cloudi_prefix/tests/http_req/http_req_haskell\"}, {dest_refresh, none}, {options, [{request_timeout_adjustment, true}, {nice, 10}]}]],"])
AC_SUBST(CONFIG_TESTS_HASKELL_MESSAGING, ["[[{prefix, \"/tests/messaging/haskell/\"}, {file_path, \"$cloudi_prefix/tests/messaging/messaging_haskell\"}, {dest_refresh, immediate_local}, {timeout_async, 10000}, {timeout_sync, 10000}, {count_thread, 4}, {options, [{dispatcher_pid_options, [{priority, low}]}, {nice, 15}, {scope, cloudi_service_test_messaging_haskell}]}]],"])
AC_SUBST(CONFIG_TESTS_HASKELL_MSG_SIZE, ["[[{prefix, \"/tests/msg_size/\"}, {file_path, \"$cloudi_prefix/tests/msg_size/msg_size_haskell\"}, {options, [{request_timeout_adjustment, true}, {aspects_init_after, [{cloudi_service_test_msg_size, aspect_init}]}, {aspects_request_before, [{cloudi_service_test_msg_size, aspect_request}]}, {aspects_terminate_before, [{cloudi_service_test_msg_size, aspect_terminate}]}, {scope, cloudi_service_test_msg_size}, {nice, 15}]}]],"])
AC_SUBST(CONFIG_TESTS_HASKELL_NULL, ["[[{prefix, \"/tests/null/response/\"}, {file_path, \"$cloudi_prefix/tests/null/null_haskell\"}, {dest_refresh, none}, {options, [{response_timeout_immediate_max, limit_min}, {nice, 10}]}]],[[{prefix, \"/tests/null/timeout/\"}, {file_path, \"$cloudi_prefix/tests/null/null_haskell\"}, {dest_refresh, none}, {options, [{response_timeout_immediate_max, limit_max}, {nice, 10}]}]],"])
fi
AM_CONDITIONAL([CABAL_INSTALL_V2], [test "x$cabal_install_v2" = "xtrue"])
AM_CONDITIONAL([HASKELL_SUPPORT], [test "x$haskell_support" = "xtrue"])

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

# OCaml CloudI API support
if test "x$ocaml_support" = "xtrue" ; then
AC_ARG_VAR([OCAMLC],[ocamlc executable])
AC_PATH_PROGS([OCAMLC],[ocamlc])
AC_ARG_VAR([OCAMLOPT],[ocamlopt executable])
AC_PATH_PROGS([OCAMLOPT],[ocamlopt])
AC_ARG_VAR([OCAMLDEP],[ocamldep executable])
AC_PATH_PROGS([OCAMLDEP],[ocamldep])
AC_ARG_VAR([OCAMLMKLIB],[ocamlmklib executable])
AC_PATH_PROGS([OCAMLMKLIB],[ocamlmklib])
if test "x$OCAMLC" = "x"; then
    AC_MSG_ERROR([ocamlc not found])
fi
if test "x$OCAMLOPT" = "x"; then
    AC_MSG_ERROR([ocamlopt not found])
fi
if test "x$OCAMLDEP" = "x"; then
    AC_MSG_ERROR([ocamldep not found])
fi
if test "x$OCAMLMKLIB" = "x"; then
    AC_MSG_ERROR([ocamlmklib not found])
fi
AC_SUBST([OCAMLFLAGS], ["-safe-string -w @A"])
AC_SUBST(CONFIG_TESTS_OCAML_COUNT, ["[[{prefix, \"/tests/count/\"}, {file_path, \"$cloudi_prefix/tests/count/count_ocaml\"}, {dest_refresh, none}, {count_thread, 4}, {options, [{nice, 10}]}]],"])
AC_SUBST(CONFIG_TESTS_OCAML_HTTP_REQ, ["[[{prefix, \"/tests/http_req/\"}, {file_path, \"$cloudi_prefix/tests/http_req/http_req_ocaml\"}, {dest_refresh, none}, {options, [{request_timeout_adjustment, true}, {nice, 10}]}]],"])
AC_SUBST(CONFIG_TESTS_OCAML_MESSAGING, ["[[{prefix, \"/tests/messaging/ocaml/\"}, {file_path, \"$cloudi_prefix/tests/messaging/messaging_ocaml\"}, {dest_refresh, immediate_local}, {timeout_async, 10000}, {timeout_sync, 10000}, {count_thread, 4}, {options, [{dispatcher_pid_options, [{priority, low}]}, {nice, 15}, {scope, cloudi_service_test_messaging_ocaml}]}]],"])
AC_SUBST(CONFIG_TESTS_OCAML_MSG_SIZE, ["[[{prefix, \"/tests/msg_size/\"}, {file_path, \"$cloudi_prefix/tests/msg_size/msg_size_ocaml\"}, {options, [{request_timeout_adjustment, true}, {aspects_init_after, [{cloudi_service_test_msg_size, aspect_init}]}, {aspects_request_before, [{cloudi_service_test_msg_size, aspect_request}]}, {aspects_terminate_before, [{cloudi_service_test_msg_size, aspect_terminate}]}, {scope, cloudi_service_test_msg_size}, {nice, 15}]}]],"])
AC_SUBST(CONFIG_TESTS_OCAML_NULL, ["[[{prefix, \"/tests/null/response/\"}, {file_path, \"$cloudi_prefix/tests/null/null_ocaml\"}, {dest_refresh, none}, {options, [{response_timeout_immediate_max, limit_min}, {nice, 10}]}]],[[{prefix, \"/tests/null/timeout/\"}, {file_path, \"$cloudi_prefix/tests/null/null_ocaml\"}, {dest_refresh, none}, {options, [{response_timeout_immediate_max, limit_max}, {nice, 10}]}]],"])
fi
AM_CONDITIONAL([OCAML_SUPPORT], [test "x$ocaml_support" = "xtrue"])

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
         @<:@ARG=3@:>@ ])],
        [
        if test "$withval" = "2"; then
            PYTHON_VERSION_REQUIRED="2"
        elif test "$withval" = "3"; then
            PYTHON_VERSION_REQUIRED="3"
        else
            AC_MSG_ERROR([Python version 2 or 3, not $withval])
        fi
        ],
        [PYTHON_VERSION_REQUIRED="3"])
if test "x$PYTHON_VERSION_REQUIRED" = "x2"; then
    AS_IF([test "x$PYTHON_DEBUG" == "xno"],
        [AC_PATH_PROGS([PYTHON],[python2 python2.7 python])],
        [AC_PATH_PROGS([PYTHON],[python2-dbg python2.7-dbg python-dbg])])
    AX_PROG_PYTHON_VERSION([2.7.0],
        [AX_PYTHON_C],
        [AC_MSG_ERROR([Python >= 2.7.0 not found])])
    AC_PATH_PROGS([PYLINT],[pylint2 pylint])
elif test "x$PYTHON_VERSION_REQUIRED" = "x3"; then
    AS_IF([test "x$PYTHON_DEBUG" == "xno"],
        [AC_PATH_PROGS([PYTHON],[python3 python3.2 python])],
        [AC_PATH_PROGS([PYTHON],[python3-dbg python3.2-dbg python-dbg])])
    AX_PROG_PYTHON_VERSION([3.0.0],
        [AX_PYTHON_C],
        [AC_MSG_ERROR([Python >= 3.0.0 not found])])
    AC_PATH_PROGS([PYLINT],[pylint3 pylint])
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
              [ruby1.9.3 ruby1.9.2 ruby1.9.1 ruby1.9.0 ruby1.9 ruby])
AX_PROG_RUBY_VERSION([1.9.0], ,
    [AC_MSG_ERROR([Ruby >= 1.9.0 not found])])
fi
AM_CONDITIONAL([RUBY_SUPPORT], [test "x$ruby_support" = "xtrue"])

# erlang checks
AC_ERLANG_NEED_ERLC()
AC_ERLANG_NEED_ERL()
AX_ERLANG_REQUIRE_OTP_VER([20.0], ,
    [AC_MSG_ERROR([Erlang >= 20.0 not found])])
AC_ERLANG_SUBST_ERTS_VER
AC_ERLANG_SUBST_ROOT_DIR
AX_ERLANG_SUBST_OTP_VER
AC_ERLANG_SUBST_LIB_DIR
AC_ERLANG_CHECK_LIB([erl_interface])
AC_ERLANG_CHECK_LIB([ssl], , [AC_MSG_ERROR([Erlang ssl required for epgsql])])
AC_ERLANG_CHECK_LIB([snmp])
AC_ERLANG_CHECK_LIB([eunit])
AC_PATH_PROG([ESCRIPT],[escript], ,[`AS_DIRNAME([$ERLC])`])
AC_PATH_PROG([DIALYZER],[dialyzer], ,[`AS_DIRNAME([$ERLC])`])
ERLC_OPTS="-DERLANG_OTP_VERSION_${ERLANG_OTP_VER_MAJOR} -I $abs_top_srcdir/lib +debug_info +strict_validation +warn_bif_clash +warn_deprecated_function +warn_export_all +warn_export_vars +warn_exported_vars +warn_obsolete_guard +warn_shadow_vars +warn_unused_import +warn_unused_function +warn_unused_record +warn_unused_vars"
ERL_OPTS=""
CLOUDI_CORE_RELEASE_OPTIONS=""
ERL_ARGS_FILE_PARSE="space_merge"
if test "$ERLANG_OTP_VER_MAJOR" -ge 20 ; then
ERLC_OPTS="${ERLC_OPTS} +deterministic"
fi
if test "$ERLANG_OTP_VER_MAJOR" -ge 22 ; then
CLOUDI_CORE_RELEASE_OPTIONS=",[[{load_dot_erlang,false}]]"
fi
if test "$ERLANG_OTP_VER_MAJOR" -ge 23 ; then
ERL_ARGS_FILE_PARSE="exact"
fi
AC_SUBST(ERLC_OPTS)
AC_SUBST(ERL_OPTS)
AC_SUBST(CLOUDI_CORE_RELEASE_OPTIONS)
AC_SUBST(ERL_ARGS_FILE_PARSE)

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
    AC_PATH_PROG([REBAR], [rebar], , ["$abs_top_builddir" $PATH])
    if test -z "$REBAR"; then
        AC_MSG_ERROR([rebar not found (use --with-rebar)])
    fi
fi
AC_SUBST(REBAR)
AC_SUBST(REBAR_ARGS, ["--verbose 3"])

# hexpi test
if test "x$want_integration_tests" = "xyes" -a \
        "x$cxx_support" = "xtrue"; then
AX_BOOST_THREAD
AX_GMP($PATHS_NONSYSTEM_INC, $PATHS_NONSYSTEM_LIB)
fi

CLOUDI_NODE_ARGS="default.args"
if test "x$integration_tests_ran" = "xtrue" ; then
    CLOUDI_CONF="cloudi_tests.conf"
else
    CLOUDI_CONF="cloudi_minimal.conf"
fi
AC_SUBST(CLOUDI_CONF)
AC_SUBST(CLOUDI_NODE_ARGS)

# Set the umask used for file creation (e.g., log files)
AC_ARG_VAR([CLOUDI_UMASK],
           [CloudI execution umask
            @<:@default=0027@:>@])

# Set a specific pid file path to be used
AC_ARG_VAR([CLOUDI_PID_FILE],
           [CloudI pid file path
            @<:@default='$localstatedir/log/cloudi/cloudi.pid'@:>@])
# Set a specific Distributed Erlang node name
AC_ARG_VAR([CLOUDI_ARG_NODE_NAME_LONG],
           [CloudI distributed Erlang fully qualified node name])
AC_ARG_VAR([CLOUDI_ARG_NODE_NAME_SHORT],
           [CloudI distributed Erlang short node name
            @<:@default='cloudi'@:>@])
AS_IF([test -n "$CLOUDI_ARG_NODE_NAME_LONG"],
      [CLOUDI_ARG_NODE_NAME="-name $CLOUDI_ARG_NODE_NAME_LONG"
       CLOUDI_NODE_NAME="$CLOUDI_ARG_NODE_NAME_LONG"],
      [test -n "$CLOUDI_ARG_NODE_NAME_SHORT"],
      [CLOUDI_ARG_NODE_NAME="-sname $CLOUDI_ARG_NODE_NAME_SHORT"
       CLOUDI_NODE_NAME="$CLOUDI_ARG_NODE_NAME_SHORT"],
      [CLOUDI_ARG_NODE_NAME="-sname cloudi"
       CLOUDI_NODE_NAME="cloudi"])
AC_SUBST(CLOUDI_ARG_NODE_NAME)
AC_SUBST(CLOUDI_NODE_NAME)
# Set a specific Distributed Erlang cookie
AC_ARG_VAR([CLOUDI_ARG_NODE_COOKIE],
           [CloudI distributed Erlang cookie
            @<:@default='cloudi'@:>@])
AS_IF([test -n "$CLOUDI_ARG_NODE_COOKIE"],
      [CLOUDI_ARG_NODE_COOKIE="-setcookie '$CLOUDI_ARG_NODE_COOKIE'"],
      [CLOUDI_ARG_NODE_COOKIE="-setcookie cloudi"])
AC_SUBST(CLOUDI_ARG_NODE_COOKIE)

AC_CONFIG_FILES([
    cloudi_tests.conf
    cloudi_minimal.conf
    rebar_src.config
    rebar_src_test.config
    reltool.config
    rel/files/bin_script
    rel/files/cloudi
    rel/files/default.args
    rel/files/erl
    rel/files/nodetool
    rel/files/vm.args
    rel/files/vm.config
    Makefile
    lib/Makefile
    lib/cloudi_core/Makefile
    lib/cloudi_core/rebar.config
    lib/cloudi_core/cxx_src/Makefile
    lib/cloudi_core/src/cloudi_core.app.src
    lib/cloudi_core/src/cloudi_environment.hrl
    lib/cloudi_service_api_batch/rebar.config
    lib/cloudi_service_api_batch/src/cloudi_service_api_batch.app.src
    lib/cloudi_service_api_requests/src/cloudi_service_api_requests.app.src
    lib/cloudi_service_cron/src/cloudi_service_cron.app.src
    lib/cloudi_service_db_mysql/rebar.config
    lib/cloudi_service_db_mysql/src/cloudi_service_db_mysql.app.src
    lib/cloudi_service_db_pgsql/rebar.config
    lib/cloudi_service_db_pgsql/src/cloudi_service_db_pgsql.app.src
    lib/cloudi_service_filesystem/rebar.config
    lib/cloudi_service_filesystem/src/cloudi_service_filesystem.app.src
    lib/cloudi_service_funnel/src/cloudi_service_funnel.app.src
    lib/cloudi_service_http_client/rebar.config
    lib/cloudi_service_http_client/src/cloudi_service_http_client.app.src
    lib/cloudi_service_http_cowboy/src/cloudi_service_http_cowboy.app.src
    lib/cloudi_service_http_cowboy1/src/cloudi_service_http_cowboy1.app.src
    lib/cloudi_service_http_elli/src/cloudi_service_http_elli.app.src
    lib/cloudi_service_http_rest/src/cloudi_service_http_rest.app.src
    lib/cloudi_service_map_reduce/src/cloudi_service_map_reduce.app.src
    lib/cloudi_service_monitoring/src/cloudi_service_monitoring.app.src
    lib/cloudi_service_null/src/cloudi_service_null.app.src
    lib/cloudi_service_oauth1/rebar.config
    lib/cloudi_service_oauth1/src/cloudi_service_oauth1.app.src
    lib/cloudi_service_queue/rebar.config
    lib/cloudi_service_queue/src/cloudi_service_queue.app.src
    lib/cloudi_service_quorum/rebar.config
    lib/cloudi_service_quorum/src/cloudi_service_quorum.app.src
    lib/cloudi_service_request_rate/src/cloudi_service_request_rate.app.src
    lib/cloudi_service_router/src/cloudi_service_router.app.src
    lib/cloudi_service_shell/src/cloudi_service_shell.app.src
    lib/cloudi_service_tcp/src/cloudi_service_tcp.app.src
    lib/cloudi_service_udp/src/cloudi_service_udp.app.src
    lib/cloudi_service_validate/rebar.config
    lib/cloudi_service_validate/src/cloudi_service_validate.app.src
    lib/syslog_socket/rebar.config
    api/Makefile
    api/c/Makefile
    api/erlang/Makefile
    api/go/Makefile
    api/haskell/Makefile
    api/haskell/cloudi.cabal
    api/java/Makefile
    api/java/org/Makefile
    api/java/org/cloudi/Makefile
    api/java/com/Makefile
    api/java/com/ericsson/Makefile
    api/java/com/ericsson/otp/Makefile
    api/java/com/ericsson/otp/erlang/Makefile
    api/java/jar/Makefile
    api/javascript/Makefile
    api/ocaml/Makefile
    api/ocaml/external/Makefile
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
    tests/count/Makefile
    tests/count/c/Makefile
    tests/count/erlang/Makefile
    tests/count/erlang/src/Makefile
    tests/count/gopath/Makefile
    tests/count/gopath/go.mod
    tests/count/haskell/Makefile
    tests/count/haskell/count_haskell.cabal
    tests/count/java/Makefile
    tests/count/java/jar/Makefile
    tests/count/java/org/Makefile
    tests/count/java/org/cloudi/Makefile
    tests/count/java/org/cloudi/tests/Makefile
    tests/count/java/org/cloudi/tests/count/Makefile
    tests/count/ocaml/Makefile
    tests/echo/Makefile
    tests/environment/Makefile
    tests/hexpi/Makefile
    tests/hexpi/cxx/Makefile
    tests/hexpi/erlang/Makefile
    tests/hexpi/erlang/src/Makefile
    tests/http/Makefile
    tests/http/java/Makefile
    tests/http/java/jar/Makefile
    tests/http/java/org/Makefile
    tests/http/java/org/cloudi/Makefile
    tests/http/java/org/cloudi/tests/Makefile
    tests/http/java/org/cloudi/tests/http/Makefile
    tests/http_req/Makefile
    tests/http_req/c/Makefile
    tests/http_req/erlang/Makefile
    tests/http_req/erlang/src/Makefile
    tests/http_req/gopath/Makefile
    tests/http_req/gopath/go.mod
    tests/http_req/haskell/Makefile
    tests/http_req/haskell/http_req_haskell.cabal
    tests/http_req/java/Makefile
    tests/http_req/java/jar/Makefile
    tests/http_req/java/org/Makefile
    tests/http_req/java/org/cloudi/Makefile
    tests/http_req/java/org/cloudi/tests/Makefile
    tests/http_req/java/org/cloudi/tests/http_req/Makefile
    tests/http_req/ocaml/Makefile
    tests/messaging/Makefile
    tests/messaging/cxx/Makefile
    tests/messaging/erlang/Makefile
    tests/messaging/erlang/src/Makefile
    tests/messaging/gopath/Makefile
    tests/messaging/gopath/go.mod
    tests/messaging/haskell/Makefile
    tests/messaging/haskell/messaging_haskell.cabal
    tests/messaging/java/Makefile
    tests/messaging/java/jar/Makefile
    tests/messaging/java/org/Makefile
    tests/messaging/java/org/cloudi/Makefile
    tests/messaging/java/org/cloudi/tests/Makefile
    tests/messaging/java/org/cloudi/tests/messaging/Makefile
    tests/messaging/ocaml/Makefile
    tests/msg_size/Makefile
    tests/msg_size/cxx/Makefile
    tests/msg_size/erlang/Makefile
    tests/msg_size/erlang/src/Makefile
    tests/msg_size/gopath/Makefile
    tests/msg_size/gopath/go.mod
    tests/msg_size/haskell/Makefile
    tests/msg_size/haskell/msg_size_haskell.cabal
    tests/msg_size/java/Makefile
    tests/msg_size/java/jar/Makefile
    tests/msg_size/java/org/Makefile
    tests/msg_size/java/org/cloudi/Makefile
    tests/msg_size/java/org/cloudi/tests/Makefile
    tests/msg_size/java/org/cloudi/tests/msg_size/Makefile
    tests/msg_size/ocaml/Makefile
    tests/null/Makefile
    tests/null/c/Makefile
    tests/null/erlang/Makefile
    tests/null/erlang/src/Makefile
    tests/null/gopath/Makefile
    tests/null/gopath/go.mod
    tests/null/haskell/Makefile
    tests/null/haskell/null_haskell.cabal
    tests/null/java/Makefile
    tests/null/java/jar/Makefile
    tests/null/java/org/Makefile
    tests/null/java/org/cloudi/Makefile
    tests/null/java/org/cloudi/tests/Makefile
    tests/null/java/org/cloudi/tests/null_/Makefile
    tests/null/ocaml/Makefile
    tests/service_api/Makefile
    tests/websockets/Makefile
    ])
AC_OUTPUT

