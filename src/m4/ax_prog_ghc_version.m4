#-*-Mode:m4;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=m4 fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
#
# SYNOPSIS
#
#   AX_PROG_GHC_VERSION([VERSION],[ACTION-IF-TRUE],[ACTION-IF-FALSE])
#
# DESCRIPTION
#
#   Check if the installed ghc version is greater than or equal to
#   the provided version.
#
# MIT License
#
# Copyright (c) 2022 Michael Truog <mjtruog at protonmail dot com>
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

AC_DEFUN([AX_PROG_GHC_VERSION],[
    AS_IF([test "x$GHC" = "x"],[
        AC_MSG_WARN([could not find the ghc executable])
        $3
    ],[
        AC_CACHE_CHECK([for ghc version],[ax_cv_ghc_version],[
            ax_cv_ghc_version=`$GHC --numeric-version`
        ])
        AS_IF([test "x$ax_cv_ghc_version" = "x"],[
            $3
        ],[
            ghc_version_required="$1"

            ghc_version_major=`expr $ax_cv_ghc_version : '\([[0-9]]*\)'`
            AC_SUBST([GHC_VERSION],[$ax_cv_ghc_version])
            AC_SUBST([GHC_VERSION_MAJOR],[$ghc_version_major])

            AX_COMPARE_VERSION(
                [$ghc_version_required],[le],[$ax_cv_ghc_version],
                [$2],
                [$3])
        ])
    ])
])
