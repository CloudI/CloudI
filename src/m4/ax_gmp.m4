#-*-Mode:m4;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=m4 fenc=utf-8 sts=4 ts=4 sw=4 et:
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
# BSD LICENSE
#
# Copyright (c) 2013, Michael Truog
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
