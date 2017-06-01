#-*-Mode:m4;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=m4 fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
#
# SYNOPSIS
#
#   AX_CLOCK_GETTIME
#
# DESCRIPTION
#
#   Determine if clock_gettime exists within librt and if clock_gettime
#   supports the argument CLOCK_MONOTONIC.
#
#   This macro sets:
#
#     HAVE_CLOCK_GETTIME_RT (or) HAVE_CLOCK_GETTIME_NONRT
#     HAVE_CLOCK_GETTIME_MONOTONIC
#
# MIT License
#
# Copyright (c) 2011-2017 Michael Truog <mjtruog at gmail dot com>
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

AC_DEFUN([AX_CLOCK_GETTIME],
[
    RT_LIB=""
    clock_gettime=no
    AC_CHECK_LIB(rt, clock_gettime,
        [AC_DEFINE([HAVE_CLOCK_GETTIME_RT], [1],
            [Define if librt clock_gettime exists])
         RT_LIB="-lrt"
         clock_gettime=yes],
        [AC_CHECK_FUNC([clock_gettime],
            [AC_DEFINE([HAVE_CLOCK_GETTIME_NONRT], [1],
                [Define if non-librt clock_gettime exists])
             clock_gettime=yes])])
    AC_SUBST(RT_LIB)
    if test $clock_gettime != no; then
        AC_MSG_CHECKING(clock_gettime CLOCK_MONOTONIC usability)
        AC_PREPROC_IFELSE([AC_LANG_SOURCE([[
#include <unistd.h>
#include <time.h>
#if !defined(CLOCK_MONOTONIC) || !defined(_POSIX_MONOTONIC_CLOCK) || (_POSIX_MONOTONIC_CLOCK < 0)
#error
#endif
             ]])],
            [AC_DEFINE([HAVE_CLOCK_GETTIME_MONOTONIC], [1],
                [Define if clock_gettime supports CLOCK_MONOTONIC])
             AC_MSG_RESULT(yes)],
            [AC_MSG_RESULT(no)])
    fi
])
