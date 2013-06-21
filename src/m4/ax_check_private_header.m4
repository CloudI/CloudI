#-*-Mode:m4;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=m4 fenc=utf-8 sts=4 ts=4 sw=4 et:
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
# BSD LICENSE
# 
# Copyright (c) 2012, Michael Truog
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

AC_DEFUN([AX_CHECK_PRIVATE_HEADER],
[
    cflags_prefix=""
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
         $2],
        [$3])
    AS_VAR_POPDEF([ac_Header])
])

