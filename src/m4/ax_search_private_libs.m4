#-*-Mode:m4;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=m4 fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
#
# SYNOPSIS
#
#   AX_SEARCH_PRIVATE_LIBS(FUNCTION, SOURCE, SEARCH-LIBS,
#                          [ACTION-IF-FOUND], [ACTION-IF-NOT-FOUND],
#                          [OTHER-LIBRARIES])
#
# DESCRIPTION
#
#   AC_SEARCH_LIBS functionality that does not modify the LIBS variable
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

AC_DEFUN([AX_SEARCH_PRIVATE_LIBS],
[
    AC_CACHE_CHECK([for library containing $1], [ac_cv_search_$1],
                   [ac_func_search_save_LIBS=$LIBS
                    ac_cv_search_$1=no
                    AC_LINK_IFELSE([$2],
                                   [ac_cv_search_$1="none required"])
                    if test "$ac_cv_search_$1" = no; then
                        for ac_lib in $3; do
                            LIBS="-l$ac_lib $6 $ac_func_search_save_LIBS"
                            AC_LINK_IFELSE([$2],
                                           [ac_cv_search_$1="-l$ac_lib"
                                            break])
                        done
                    fi
                    LIBS=$ac_func_search_save_LIBS])
    AS_IF([test "$ac_cv_search_$1" != no],
          [test "$ac_cv_search_$1" = "none required" || $4],
          [$5])
])

