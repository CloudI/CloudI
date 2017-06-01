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

