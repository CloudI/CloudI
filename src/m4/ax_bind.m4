#-*-Mode:m4;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=m4 fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
#
# SYNOPSIS
#
#   AX_BIND()
#
# DESCRIPTION
#
#   Determine if it is possible to set logical processor affinity
#   in the calling execution thread on this operating system.
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

AC_DEFUN([AX_BIND],
[
    AC_REQUIRE([AX_PTHREAD])
    AS_CASE([$host_os],
            [linux*], [bind="linux"],
            [freebsd*], [bind="freebsd"],
            [netbsd*], [bind="pthread"],
            [dragonfly*], [bind="pthread"],
            [*], [bind=""])

    AC_LANG_PUSH([C++])

    if test "x$bind" = "xlinux"; then
        AC_LINK_IFELSE(
            [AC_LANG_PROGRAM([[
#define _GNU_SOURCE
#include <sched.h>
             ]], [[
cpu_set_t cpuset;
CPU_ZERO(&cpuset);
sched_setaffinity(0, sizeof(cpuset), &cpuset);
             ]])],
            [],
            [bind=""])
    elif test "x$bind" = "xfreebsd"; then
        AC_LINK_IFELSE(
            [AC_LANG_PROGRAM([[
#include <sys/param.h>
#include <sys/cpuset.h>
             ]], [[
cpuset_t cpuset;
CPU_ZERO(&cpuset);
cpuset_setaffinity(CPU_LEVEL_WHICH, CPU_WHICH_TID, -1, sizeof(cpuset), &cpuset);
             ]])],
            [],
            [bind=""])
    elif test "x$bind" = "xpthread"; then
        CPPFLAGS_SAVED="$CPPFLAGS"
        CPPFLAGS="$CPPFLAGS $PTHREAD_CFLAGS"
        LIBS_SAVED="$LIBS"
        LIBS="$PTHREAD_LIBS $LIBS"
        AC_LINK_IFELSE(
            [AC_LANG_PROGRAM([[
#if defined(__DragonFly__)
#include <pthread.h>
#include <pthread_np.h>
#elif defined(__NetBSD__)
#include <pthread.h>
#include <sched.h>
#define CPU_ZERO(p) cpuset_zero(p)
#else
#error "unsupported operating system!"
#endif

             ]], [[
cpuset_t cpuset;
CPU_ZERO(&cpuset);
pthread_setaffinity_np(pthread_self(), sizeof(cpuset), &cpuset);
             ]])],
            [],
            [bind=""])
        CPPFLAGS="$CPPFLAGS_SAVED"
        LIBS="$LIBS_SAVED"
    fi
    BIND_CPPFLAGS=""
    BIND_LIBS=""
    AC_MSG_CHECKING([for bind])
    if test "x$bind" = "xlinux"; then
        AC_DEFINE([BIND_USE_LINUX], [1],
                  [Provide bind option with sched_setaffinity.])
        AC_MSG_RESULT([linux])
    elif test "x$bind" = "xfreebsd"; then
        AC_DEFINE([BIND_USE_FREEBSD], [1],
                  [Provide bind option with cpuset_setaffinity.])
        AC_MSG_RESULT([freebsd])
    elif test "x$bind" = "xpthread"; then
        BIND_CPPFLAGS="$PTHREAD_CFLAGS"
        BIND_LIBS="$PTHREAD_LIBS"
        AC_DEFINE([BIND_USE_PTHREAD], [1],
                  [Provide bind option with pthread_setaffinity_np.])
        AC_MSG_RESULT([pthread])
    else
        AC_MSG_RESULT([none])
    fi
    AC_LANG_POP([C++])
    AC_SUBST(BIND_CPPFLAGS)
    AC_SUBST(BIND_LIBS)
])
