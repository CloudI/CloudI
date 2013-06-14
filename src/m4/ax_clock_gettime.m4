#-*-Mode:m4;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=m4 fenc=utf-8 sts=4 ts=4 sw=4 et:
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
# BSD LICENSE
# 
# Copyright (c) 2011-2012, Michael Truog
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
# 
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in
#       the documentation and/or other materials provided with the
#       distribution.
#     * All advertising materials mentioning features or use of this
#       software must display the following acknowledgment:
#         This product includes software developed by Michael Truog
#     * The name of the author may not be used to endorse or promote
#       products derived from this software without specific prior
#       written permission
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
# CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
# OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
# CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
# WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
# DAMAGE.
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
        AC_PREPROC_IFELSE([AC_LANG_PROGRAM([[
#include <unistd.h>
#include <time.h>
#if !defined(CLOCK_MONOTONIC) || !defined(_POSIX_MONOTONIC_CLOCK) || (_POSIX_MONOTONIC_CLOCK < 0)
#error
#endif
             ]], [[]])],
            [AC_DEFINE([HAVE_CLOCK_GETTIME_MONOTONIC], [1],
                [Define if clock_gettime supports CLOCK_MONOTONIC])
             AC_MSG_RESULT(yes)],
            [AC_MSG_RESULT(no)])
    fi
])
