#-*-Mode:m4;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=m4 fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
#
# SYNOPSIS
#
#   AX_BACKTRACE()
#
# DESCRIPTION
#
#   Determine if backward-cpp, boost::stacktrace or booster dependencies are
#   available to create a backtrace for the C++ CloudI API.
#
# MIT License
#
# Copyright (c) 2013-2023 Michael Truog <mjtruog at protonmail dot com>
#
# Permission is hereby granted, free of charge, to any person obtaining a
# copy of this software and associated documentation files (the "Software"),
# to deal in the Software without restriction, including without limitation
# the rights to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
# DEALINGS IN THE SOFTWARE.
#

AC_DEFUN([AX_BACKTRACE],
[
    dnl checks below will select the backtrace approach that provides
    dnl the most information
    AS_CASE([$host_os],
            [linux*], [backtrace="backward"],
            [*], [backtrace="booster"])

    AC_LANG_PUSH([C++])

    dnl backward assumes this is present on linux
    has_cxxabi="no"
    AC_MSG_CHECKING([for cxxabi.h])
    AC_LINK_IFELSE(
        [AC_LANG_PROGRAM([[
#include <cxxabi.h>
         ]], [[
abi::__cxa_demangle(0, 0, 0, 0);
         ]])],
        [has_cxxabi="yes"],
        [backtrace="booster"])
    AC_MSG_RESULT($has_cxxabi)

    dnl Many backtrace libraries depend on dladdr
    has_dladdr="no"
    AX_CHECK_PRIVATE_LIB(dl, dladdr,
        [AC_LANG_PROGRAM([[
#include <dlfcn.h>
         ]], [[
dladdr(0, 0);
         ]])],
        [has_dladdr="yes"])

    dnl boost::stacktrace major changes stopped before 1.71.0 (added in 1.65.0)
    dnl use instead of booster, if possible
    if test "x$backtrace" = "xbooster" -a "x$has_dladdr" = "xyes"; then
        AC_MSG_CHECKING([for boost::stacktrace])
        CXXFLAGS_SAVED="$CXXFLAGS"
        CXXFLAGS="$CXXFLAGS $BOOST_CXXFLAGS"
        LDFLAGS_SAVED="$LDFLAGS"
        LDFLAGS="$LDFLAGS $BOOST_LDFLAGS"
        LIBS_SAVED="$LIBS"
        LIBS="-lboost_stacktrace_basic $DL_LIB $LIBS"
        has_boost_stacktrace="no"
        AC_LINK_IFELSE(
            [AC_LANG_PROGRAM([[
#include <iostream>
#include <boost/version.hpp>
#if (BOOST_VERSION / 100000 > 1) ||                                          \
    ((BOOST_VERSION / 100000 == 1) && (BOOST_VERSION / 100 % 1000 >= 71))
#include <boost/stacktrace.hpp>
#endif
             ]], [[
std::cout << boost::stacktrace::stacktrace();
             ]])],
            [has_boost_stacktrace="yes"
             backtrace="boost"])
        AC_MSG_RESULT($has_boost_stacktrace)
        CXXFLAGS="$CXXFLAGS_SAVED"
        LDFLAGS="$LDFLAGS_SAVED"
        LIBS="$LIBS_SAVED"
    fi

    if test "x$backtrace" = "xbackward" -o "x$backtrace" = "xbooster"; then
        dnl preferred, rather than execinfo.h
        has_unwind="no"
        AC_MSG_CHECKING([for _Unwind_GetIP])
        AC_LINK_IFELSE(
            [AC_LANG_PROGRAM([[
#include <unwind.h>
             ]], [[
_Unwind_GetIP(0);
             ]])],
            [has_unwind="yes"])
        AC_MSG_RESULT($has_unwind)

        dnl in case unwind can not be used
        dnl (currently a requirement for having a backtrace)
        has_execinfo="no"
        AC_MSG_CHECKING(for execinfo.h)
        AC_LINK_IFELSE(
            [AC_LANG_PROGRAM([[
#include <execinfo.h>
             ]], [[
backtrace(0, 0);
             ]])],
            [has_execinfo="yes"])
        AC_MSG_RESULT($has_execinfo)
        if test "x$has_execinfo" = "xno"; then
            dnl libexecinfo may be required at link-time
            AX_CHECK_PRIVATE_LIB(execinfo, backtrace,
                [AC_LANG_PROGRAM([[
#include <execinfo.h>
                 ]], [[
backtrace(0, 0);
                 ]])],
                [has_execinfo="yes"])
        else
            EXECINFO_LDFLAGS=""
            EXECINFO_LIB=""
        fi

        dnl backtrace data is required to print backtrace information
        if test "x$has_unwind" = "xno" -a "x$has_execinfo" = "xno"; then
            backtrace=""
        fi
    fi

    if test "x$backtrace" = "xbackward"; then
        dnl nongnu-libunwind improved stacktraces in the past
        dnl for x86*|arm* architectures though it has also caused
        dnl memory corruption (unreliable execution) when combined with
        dnl the libgcc unwinder (release gcc/g++ builds with 1.2.1).
        dnl To avoid potential problems in the future, it is best
        dnl to not use nongnu-libunwind.
        has_libunwind="no"
        UNWIND_LDFLAGS=""
        UNWIND_LIB=""

        want_dwarf="no"
        want_bfd="no"
        if test "x$has_dladdr" = "xyes"; then
            dnl libdwarf version < 20210528
            AX_CHECK_PRIVATE_LIB(dwarf, dwarf_elf_init,
                [AC_LANG_PROGRAM([[
#include <libdwarf/libdwarf.h>
#include <libdwarf/dwarf.h>
                 ]], [[
dwarf_elf_init(0, 0, 0, 0, 0, 0);
                 ]])],
                [AX_CHECK_PRIVATE_LIB(elf, elf_version,
                    [AC_LANG_PROGRAM([[
#include <libelf.h>
                     ]], [[
elf_version(EV_CURRENT);
                     ]])],
                    [want_dwarf="yes"])
                 ])
            AX_CHECK_PRIVATE_LIB(bfd, bfd_init,
                [AC_LANG_PROGRAM([[
#include <bfd.h>
                 ]], [[
bfd_init();
                 ]])],
                [want_bfd="yes"])
        fi
        AX_CHECK_PRIVATE_LIB(dw, dwfl_begin,
            [AC_LANG_PROGRAM([[
#include <elfutils/libdw.h>
#include <elfutils/libdwfl.h>
#include <dwarf.h>
             ]], [[
dwfl_begin(0);
             ]])],
            [want_dw="yes"],
            [want_dw="no"])
    fi

    BACKTRACE_CXXFLAGS=""
    BACKTRACE_LDFLAGS=""
    BACKTRACE_LIB=""
    AC_MSG_CHECKING([for C/C++ backtrace])
    if test "x$backtrace" = "xbackward"; then
        BACKTRACE_CXXFLAGS="-I\$(top_srcdir)/external/backward-cpp/"
        if test "x$has_libunwind" = "xyes"; then
            AC_DEFINE([BACKWARD_HAS_UNWIND], [0],
                      [Define if libgcc has _Unwind_GetIP().])
            AC_DEFINE([BACKWARD_HAS_LIBUNWIND], [1],
                      [Define if libunwind is usable.])
            AC_DEFINE([BACKWARD_HAS_BACKTRACE], [0],
                      [Define if execinfo.h is usable.])
            unwind_status="libunwind"
        elif test "x$has_unwind" = "xyes"; then
            AC_DEFINE([BACKWARD_HAS_UNWIND], [1],
                      [Define if libgcc has _Unwind_GetIP().])
            AC_DEFINE([BACKWARD_HAS_LIBUNWIND], [0],
                      [Define if libunwind is usable.])
            AC_DEFINE([BACKWARD_HAS_BACKTRACE], [0],
                      [Define if execinfo.h is usable.])
            UNWIND_LDFLAGS=""
            UNWIND_LIB=""
            unwind_status="unwind"
        else
            AC_DEFINE([BACKWARD_HAS_UNWIND], [0],
                      [Define if libgcc has _Unwind_GetIP().])
            AC_DEFINE([BACKWARD_HAS_LIBUNWIND], [0],
                      [Define if libunwind is usable.])
            AC_DEFINE([BACKWARD_HAS_BACKTRACE], [1],
                      [Define if execinfo.h is usable.])
            UNWIND_LDFLAGS="$EXECINFO_LDFLAGS"
            UNWIND_LIB="$EXECINFO_LIB"
            unwind_status="execinfo"
        fi
        if test "x$want_bfd" = "xyes"; then
            BACKTRACE_LDFLAGS="$BFD_LDFLAGS $DL_LDFLAGS $UNWIND_LDFLAGS"
            BACKTRACE_LIB="$BFD_LIB $DL_LIB $UNWIND_LIB"
            AC_DEFINE([BACKWARD_HAS_BFD], [1],
                      [Define if libbfd is usable.])
            AC_MSG_RESULT([backward-cpp bfd $unwind_status])
        elif test "x$want_dw" = "xyes"; then
            BACKTRACE_LDFLAGS="$DW_LDFLAGS $UNWIND_LDFLAGS"
            BACKTRACE_LIB="$DW_LIB $UNWIND_LIB"
            AC_DEFINE([BACKWARD_HAS_DW], [1],
                      [Define if libdw is usable.])
            AC_MSG_RESULT([backward-cpp dw $unwind_status])
        elif test "x$want_dwarf" = "xyes"; then
            BACKTRACE_LDFLAGS="$DWARF_LDFLAGS $ELF_LDFLAGS $DL_LDFLAGS $UNWIND_LDFLAGS"
            BACKTRACE_LIB="$DWARF_LIB $ELF_LIB $DL_LIB $UNWIND_LIB"
            AC_DEFINE([BACKWARD_HAS_DWARF], [1],
                      [Define if libdwarf is usable.])
            AC_MSG_RESULT([backward-cpp dwarf $unwind_status])
        else
            BACKTRACE_LDFLAGS="$UNWIND_LDFLAGS"
            BACKTRACE_LIB="$UNWIND_LIB"
            AC_DEFINE([BACKWARD_HAS_BACKTRACE_SYMBOL], [1],
                      [Define if execinfo.h is usable.])
            AC_MSG_RESULT([backward-cpp execinfo $unwind_status])
        fi
        AC_DEFINE([BACKWARD_SYSTEM_LINUX], [1],
                  [Specify linux support.])
        AC_DEFINE([BACKTRACE_USE_BACKWARD], [1],
                  [Provide C++ backtraces with backward code.])
    elif test "x$backtrace" = "xboost"; then
        BACKTRACE_CXXFLAGS=""
        BACKTRACE_LDFLAGS=""
        BACKTRACE_LIB="-lboost_stacktrace_basic $DL_LIB"
        AC_DEFINE([BACKTRACE_USE_BOOST], [1],
                  [Provide C++ backtraces with boost::stacktrace.])
        AC_MSG_RESULT([boost])
    elif test "x$backtrace" = "xbooster"; then
        BACKTRACE_CXXFLAGS="-I\$(top_srcdir)/external/booster"
        BACKTRACE_LDFLAGS="$DL_LDFLAGS $EXECINFO_LDFLAGS"
        BACKTRACE_LIB="$DL_LIB $EXECINFO_LIB"
        if test "x$has_unwind" = "xyes"; then
            AC_DEFINE([BOOSTER_HAVE_UNWIND_BACKTRACE], [1],
                      [Define if libgcc has _Unwind_GetIP().])
            unwind_status="unwind"
        else
            unwind_status="execinfo"
        fi
        if test "x$has_dladdr" = "xyes"; then
            AC_DEFINE([BOOSTER_HAVE_DLADDR], [1],
                      [Define if dladdr() is usable.])
        fi
        if test "x$has_cxxabi" = "xyes"; then
            AC_DEFINE([BOOSTER_HAVE_ABI_CXA_DEMANGLE], [1],
                      [Define if cxxabi.h is usable.])
        fi
        AC_DEFINE([BOOSTER_HAVE_EXECINFO], [1],
                  [Define if execinfo.h is usable.])
        AC_DEFINE([BACKTRACE_USE_BOOSTER], [1],
                  [Provide C++ backtraces with booster code.])
        AC_MSG_RESULT([booster $unwind_status])
    else
        AC_MSG_RESULT([none])
    fi
    AC_LANG_POP([C++])
    AC_SUBST(BACKTRACE_CXXFLAGS)
    AC_SUBST(BACKTRACE_LDFLAGS)
    AC_SUBST(BACKTRACE_LIB)
])
