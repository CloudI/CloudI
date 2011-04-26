# -*- coding: utf-8; Mode: m4; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
# ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
#
# SYNOPSIS
#
# AX_CHECK_PRIVATE_LIB(LIBRARY, FUNCTION,
#                      [ACTION-IF-FOUND], [ACTION-IF-NOT-FOUND],
#                      [OTHER-LIBRARIES])
#
# DESCRIPTION
#
#   AC_CHECK_LIB functionality that does not modify the LIBS variable
#
# BSD LICENSE
# 
# Copyright (c) 2011, Michael Truog
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

AC_DEFUN([AX_CHECK_PRIVATE_LIB],
[
    m4_ifval([$3], , [AH_CHECK_LIB([$1])])
    AS_LITERAL_IF([$1],
                  [AS_VAR_PUSHDEF([ac_Lib], [ac_cv_lib_$1_$2])],
                  [AS_VAR_PUSHDEF([ac_Lib], [ac_cv_lib_$1''_$2])])
    AC_CACHE_CHECK([for $2 in -l$1], ac_Lib,
                   [ac_check_lib_save_LIBS=$LIBS
                    LIBS="-l$1 $5 $LIBS"
                    AC_LINK_IFELSE([AC_LANG_CALL([], [$2])],
                                   [AS_VAR_SET(ac_Lib, yes)],
                                   [AS_VAR_SET(ac_Lib, no)])
                    LIBS=$ac_check_lib_save_LIBS])
    AS_IF([test AS_VAR_GET(ac_Lib) = yes],
          [m4_default([$3], [AC_DEFINE_UNQUOTED(AS_TR_CPP(HAVE_LIB$1))])],
          [$4])
    AS_VAR_POPDEF([ac_Lib])
])


