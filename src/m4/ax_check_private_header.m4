#-*-Mode:m4;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=m4 fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
#
# SYNOPSIS
#
#   AX_CHECK_PRIVATE_HEADER([HEADER],
#                           [ACTION-IF-FOUND],[ACTION-IF-NOT-FOUND],
#                           [INC],[OTHER-PATHS])
#
# DESCRIPTION
#
#   Provide a way of checking for C header files that checks additional
#   include directories.
#
# MIT License
#
# Copyright (c) 2012-2017 Michael Truog <mjtruog at protonmail dot com>
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

AC_DEFUN([AX_CHECK_PRIVATE_HEADER],
[
    cflags_prefix=""
    include_path=""
    AS_VAR_PUSHDEF([ac_Header], [ac_cv_header_$1])
    AC_CACHE_CHECK([for $1], ac_Header,
        [m4_ifval([$4],
            [AC_COMPILE_IFELSE([AC_LANG_SOURCE([$4
                                                @%:@include <$1>])],
                [AS_VAR_SET(ac_Header, yes)],
                [AS_VAR_SET(ac_Header, no)
                 ac_check_header_save_CFLAGS=$CFLAGS
                 for path in $5 ; do
                     cflags_prefix="-I$path"
                     include_path=$path
                     CFLAGS="$ac_check_header_save_CFLAGS $cflags_prefix"
                     AC_COMPILE_IFELSE([AC_LANG_SOURCE([$4
                                                        @%:@include <$1>])],
                         [AS_VAR_SET(ac_Header, yes)
                          break],
                         [])
                 done
                 CFLAGS=$ac_check_header_save_CFLAGS])],
            [AC_PREPROC_IFELSE([AC_LANG_SOURCE([@%:@include <$1>])],
                [AS_VAR_SET(ac_Header, yes)],
                [AS_VAR_SET(ac_Header, no)
                 ac_check_header_save_CFLAGS=$CFLAGS
                 for path in $5 ; do
                     cflags_prefix="-I$path"
                     include_path=$path
                     CFLAGS="$ac_check_header_save_CFLAGS $cflags_prefix"
                     AC_COMPILE_IFELSE([AC_LANG_SOURCE([@%:@include <$1>])],
                         [AS_VAR_SET(ac_Header, yes)
                          break],
                         [])
                 done
                 CFLAGS=$ac_check_header_save_CFLAGS])])
        ])
    AS_IF([test AS_VAR_GET(ac_Header) = yes],
        [m4_toupper(AS_TR_SH($1_CFLAGS))=$cflags_prefix
         AC_SUBST(m4_toupper(AS_TR_SH($1_CFLAGS)))
         dnl only set if searched for
         m4_toupper(AS_TR_SH($1_PATH))=$include_path
         AC_SUBST(m4_toupper(AS_TR_SH($1_PATH)))
         $2],
        [$3])
    AS_VAR_POPDEF([ac_Header])
])

