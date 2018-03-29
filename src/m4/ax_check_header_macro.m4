#-*-Mode:m4;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=m4 fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
#
# SYNOPSIS
#
#   AX_CHECK_HEADER_MACRO
#
# DESCRIPTION
#
#   AX_CHECK_HEADER_MACRO([HEADER], [MACRO])
#
# MIT License
#
# Copyright (c) 2015-2017 Michael Truog <mjtruog at protonmail dot com>
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

AC_DEFUN([AX_CHECK_HEADER_MACRO],
[
    AS_IF([eval test x$]AS_TR_SH([ac_cv_header_$1])[ = xyes], [],
          [AC_CHECK_HEADER($1,
                           [AC_DEFINE(m4_toupper(AS_TR_SH(HAVE_$1)), 1,
                                      [Define to 1 if you have
                                       the <$1> header file.])],
                           [AC_MSG_ERROR([$1 not found])])])

    AC_MSG_CHECKING(for $2 in $1)
    AC_PREPROC_IFELSE([AC_LANG_SOURCE([
@%:@include <$1>
@%:@if ! defined($2)
@%:@error
@%:@endif
        ])],
        [AC_MSG_RESULT(yes)
         AC_DEFINE(m4_toupper(AS_TR_SH(HAVE_$1_$2)), 1,
                   [Define to 1 if $2 is defined within <$1>])],
        [AC_MSG_RESULT(no)])
])
