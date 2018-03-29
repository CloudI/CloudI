#-*-Mode:m4;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=m4 fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
#
# SYNOPSIS
#
#   AX_GMP([OTHER-INC_PATHS], [OTHER-LIB_PATHS])
#
# DESCRIPTION
#
#   Determine if GMP can be found on the system
#
#   This macro sets:
#
#     GMP_H_CFLAGS
#     GMP_LDFLAGS
#     GMP_LIB
#     GMP_PATH
#
# MIT License
#
# Copyright (c) 2013-2017 Michael Truog <mjtruog at protonmail dot com>
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
AC_DEFUN([AX_GMP],
[
    AC_LANG_PUSH([C])
    AX_CHECK_PRIVATE_HEADER(gmp.h, ,
        [AC_MSG_ERROR([GNU MP not found, see http://gmplib.org/])], ,
        $1)
    CFLAGS_SAVED="$CFLAGS"
    CFLAGS="$GMP_H_CFLAGS $CFLAGS"
    export CFLAGS
    AX_CHECK_PRIVATE_LIB(gmp, gmpz_init,
        [AC_LANG_PROGRAM([[
#include <gmp.h>
         ]], [[
mpz_t a;
mpz_init_set_str(a, "12345678901234567890", 10);
mpz_t b;
mpz_init_set_str(b,  "9876543210987654321", 10);
mpz_add(a, a, b);
         ]])], ,
        [AC_MSG_ERROR([GNU MP not found, see http://gmplib.org/])], ,
        $2)
    CFLAGS="$CFLAGS_SAVED"
    export CFLAGS
    AC_LANG_POP([C])
])
