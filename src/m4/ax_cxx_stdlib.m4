#-*-Mode:m4;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=m4 fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
#
# SYNOPSIS
#
#   AX_CXX_STDLIB()
#
# DESCRIPTION
#
#   Determine the C++ stdlib used and how it should be linked.
#
# MIT License
#
# Copyright (c) 2023 Michael Truog <mjtruog at protonmail dot com>
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

AC_DEFUN([AX_CXX_STDLIB],
[
    stdlib=""
    AC_ARG_WITH([cxx-stdlib],
        [AS_HELP_STRING([--with-cxx-stdlib=ARG],
            [explicitly provide the C++ stdlib as
             the GCC stdlib (ARG=libstdc++),
             or the LLVM stdlib (ARG=libc++)])],
        [
        if test "x$withval" = "xlibstdc++"; then
            stdlib="stdc++"
        elif test "x$withval" = "xlibc++"; then
            stdlib="c++"
        else
            AC_MSG_ERROR([--with-cxx-stdlib needs libstdc++ or libc++, not $withval])
        fi
        CXXFLAGS="$CXXFLAGS --stdlib=lib$stdlib"
        ])
    if test "x$stdlib" = "x"; then
        AC_LANG_PUSH([C++])
        dnl GCC libstdc++ is common, check if LLVM libc++ is used instead
        dnl as the platform default
        stdlib="stdc++"
        AC_MSG_CHECKING([for C++ stdlib default])
        AC_COMPILE_IFELSE(
            [AC_LANG_PROGRAM([[
#if __cplusplus >= 202002L
#define CXX20
#endif
#ifdef CXX20
#include <version>
#else
#include <ciso646>
#endif
#ifndef _LIBCPP_VERSION
#error "LLVM libc++ is not present"
#endif
             ]], [])],
            [stdlib="c++"])
        AC_LANG_POP([C++])
    else
        AC_MSG_CHECKING([for C++ stdlib configured])
    fi
    AS_CASE([$stdlib],
            [c++], [stdlib_source="LLVM"],
            [stdc++], [stdlib_source="GCC"])
    AC_MSG_RESULT($stdlib_source lib$stdlib)
    CXX_STDLIB_LIB="-l$stdlib"
    AC_SUBST(CXX_STDLIB_LIB)
])
