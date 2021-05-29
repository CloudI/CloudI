#-*-Mode:m4;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=m4 fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
#
# SYNOPSIS
#
#   AX_BACKTRACE()
#
# DESCRIPTION
#
#   Determine if backward-cpp or booster dependencies are available
#   to create a backtrace for the C++ CloudI API.
#
# MIT License
#
# Copyright (c) 2013-2021 Michael Truog <mjtruog at protonmail dot com>
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
    AS_CASE([$host_os],
            [linux*], [AS_CASE([$host_cpu],
                               [x86*], [backtrace="backward"],
                               [*], [backtrace="booster"])],
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

    dnl in case unwind (or dladdr) can not be used
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
            [$has_execinfo="yes"])
    else
        EXECINFO_LIB=""
    fi

    dnl backtrace data is required to print backtrace information
    if test "x$has_unwind" = "xno" -a "x$has_execinfo" = "xno"; then
        backtrace=""
    fi

    want_dladdr="no"
    if test "x$backtrace" = "xbackward"; then
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
        AX_CHECK_PRIVATE_LIB(bfd, bfd_init,
            [AC_LANG_PROGRAM([[
#include <bfd.h>
             ]], [[
bfd_init();
             ]])],
            [want_bfd="yes"],
            [want_bfd="no"])
        if test "x$want_dw" = "xyes"; then
            want_bfd="no"
            want_dladdr="no"
        elif test "x$want_bfd" = "xyes"; then
            want_dw="no"
            want_dladdr="yes"
        else
            want_dladdr="no"
        fi
    elif test "x$backtrace" = "xbooster"; then
        want_dladdr="yes"
    fi
    has_dladdr="no"
    if test "x$want_dladdr" = "xyes"; then
        AX_CHECK_PRIVATE_LIB(dl, dladdr,
            [AC_LANG_PROGRAM([[
#include <dlfcn.h>
             ]], [[
dladdr(0, 0);
             ]])],
            [has_dladdr="yes"],
            [want_bfd="no"])
    fi
    BACKTRACE_CPPFLAGS=""
    BACKTRACE_LDFLAGS=""
    BACKTRACE_LIB=""
    AC_MSG_CHECKING([for backtrace])
    if test "x$backtrace" = "xbackward"; then
        BACKTRACE_CPPFLAGS="-I\$(top_srcdir)/external/backward-cpp/"
        if test "x$has_unwind" = "xyes"; then
            AC_DEFINE([BACKWARD_HAS_UNWIND], [1],
                      [Define if libgcc has _Unwind_GetIP().])
            AC_DEFINE([BACKWARD_HAS_BACKTRACE], [0],
                      [Define if execinfo.h is usable.])
            unwind_status="unwind"
        elif test "x$has_unwind" = "xno"; then
            AC_DEFINE([BACKWARD_HAS_UNWIND], [0],
                      [Define if libgcc has _Unwind_GetIP().])
            AC_DEFINE([BACKWARD_HAS_BACKTRACE], [1],
                      [Define if execinfo.h is usable.])
            unwind_status="execinfo"
        fi
        if test "x$want_dw" = "xyes"; then
            BACKTRACE_LDFLAGS=$DW_LDFLAGS
            BACKTRACE_LIB="$DW_LIB $EXECINFO_LIB"
            AC_DEFINE([BACKWARD_HAS_DW], [1],
                      [Define if libdw is usable.])
            AC_MSG_RESULT([backward-cpp dw $unwind_status])
        elif test "x$want_bfd" = "xyes"; then
            BACKTRACE_LDFLAGS=$BFD_LDFLAGS
            BACKTRACE_LIB="$BFD_LIB $EXECINFO_LIB"
            AC_DEFINE([BACKWARD_HAS_BFD], [1],
                      [Define if libbfd is usable.])
            AC_MSG_RESULT([backward-cpp bfd $unwind_status])
        else
            AC_DEFINE([BACKWARD_HAS_BACKTRACE_SYMBOL], [1],
                      [Define if execinfo.h is usable.])
            AC_MSG_RESULT([backward-cpp execinfo $unwind_status])
        fi
        AC_DEFINE([BACKWARD_SYSTEM_LINUX], [1],
                  [Specify linux support.])
        AC_DEFINE([BACKTRACE_USE_BACKWARD], [1],
                  [Provide C++ backtraces with backward code.])
    elif test "x$backtrace" = "xbooster"; then
        BACKTRACE_CPPFLAGS="-I\$(top_srcdir)/external/booster"
        BACKTRACE_LDFLAGS=$DL_LDFLAGS
        BACKTRACE_LIB="$DL_LIB $EXECINFO_LIB"
        if test "x$has_unwind" = "xyes"; then
            AC_DEFINE([BOOSTER_HAVE_UNWIND_BACKTRACE], [1],
                      [Define if libgcc has _Unwind_GetIP().])
            unwind_status="unwind"
        elif test "x$has_unwind" = "xno"; then
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
    AC_SUBST(BACKTRACE_CPPFLAGS)
    AC_SUBST(BACKTRACE_LDFLAGS)
    AC_SUBST(BACKTRACE_LIB)
])
