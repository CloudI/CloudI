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
# BSD LICENSE
# 
# Copyright (c) 2015, Michael Truog
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
