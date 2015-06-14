#-*-Mode:m4;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=m4 fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
#
# SYNOPSIS
#
# AX_CHECK_PRIVATE_LIB(LIBRARY, FUNCTION, SOURCE,
#                      [ACTION-IF-FOUND], [ACTION-IF-NOT-FOUND],
#                      [OTHER-LIBRARIES], [OTHER-PATHS])
#
# DESCRIPTION
#
#   AC_CHECK_LIB functionality that does not modify the LIBS variable and
#   can utilize additional system paths
#
# BSD LICENSE
# 
# Copyright (c) 2011-2013, Michael Truog
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
    m4_ifval([$4], , [AH_CHECK_LIB([$1])])
    AS_LITERAL_IF([$1],
                  [AS_VAR_PUSHDEF([ac_Lib], [ac_cv_lib_$1_$2])],
                  [AS_VAR_PUSHDEF([ac_Lib], [ac_cv_lib_$1''_$2])])
    AC_CACHE_CHECK(
        [for $2 in -l$1], ac_Lib,
        [ac_check_lib_save_LDFLAGS=$LDFLAGS
         ac_check_lib_save_LIBS=$LIBS
         ldflags_prefix=""
         lib_path=""
         LIBS="-l$1 $6 $LIBS"
         AC_LINK_IFELSE(
             [$3],
             [AS_VAR_SET(ac_Lib, yes)],
             [AS_VAR_SET(ac_Lib, no)
              for path in $7 ; do
                  ldflags_prefix="-L$path"
                  lib_path=$path
                  LDFLAGS="$ac_check_lib_save_LDFLAGS $ldflags_prefix"
                  AC_LINK_IFELSE([$3],
                                 [AS_VAR_SET(ac_Lib, yes)
                                  break],
                                 [])
              done])
         LIBS=$ac_check_lib_save_LIBS
         LDFLAGS=$ac_check_lib_save_LDFLAGS])
    AS_IF([test AS_VAR_GET(ac_Lib) = yes],
          [m4_toupper($1_LDFLAGS)=$ldflags_prefix
           AC_SUBST(m4_toupper($1_LDFLAGS))
           m4_toupper($1_LIB)="-l$1"
           AC_SUBST(m4_toupper($1_LIB))
           dnl only set if searched for
           m4_toupper($1_PATH)=$lib_path
           AC_SUBST(m4_toupper($1_PATH))
           m4_default([$4], [AC_DEFINE_UNQUOTED(AS_TR_CPP(HAVE_LIB$1))])],
          [$5])
    AS_VAR_POPDEF([ac_Lib])
])

