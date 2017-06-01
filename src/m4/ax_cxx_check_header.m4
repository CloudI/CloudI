#-*-Mode:m4;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=m4 fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
#
# SYNOPSIS
#
#   AX_CXX_CHECK_HEADER([HEADER],
#                       [ACTION-IF-FOUND], [ACTION-IF-NOT-FOUND],
#                       [INC], [OTHER-PATHS])
#
# DESCRIPTION
#
#   Provide a way of checking for C++ header files to avoid problems with
#   AC_CHECK_HEADER using a C compiler.
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

AC_DEFUN([AX_CXX_CHECK_HEADER],
[
    AC_LANG_SAVE
    AC_LANG_CPLUSPLUS
    AX_CHECK_PRIVATE_HEADER($1, $2, $3, $4, $5)
    AC_LANG_RESTORE
])

