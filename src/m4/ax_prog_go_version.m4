#-*-Mode:m4;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=m4 fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
#
# SYNOPSIS
#
#   AX_PROG_GO_VERSION([VERSION],[ACTION-IF-TRUE],[ACTION-IF-FALSE])
#
# DESCRIPTION
#
#   Check the installed go version in the shell path.
#
#   Example:
#
#     AC_PATH_PROG([GO],[go])
#     AX_PROG_GO_VERSION([1.11],[ ... ],[ ... ])
#
#   This will check to ensure at least go version 1.11 is present
#   for go module support.
#
# MIT License
#
# Copyright (c) 2021 Michael Truog <mjtruog at protonmail dot com>
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

AC_DEFUN([AX_PROG_GO_VERSION],[
    AC_REQUIRE([AC_PROG_SED])

    AS_IF([test -n "$GO"],[
        ax_go_version="$1"

        AC_MSG_CHECKING([for go version])
        GO_VERSION=`$GO version | $SED -e 's/^.* go\([[0-9]]*\.[[0-9]]*\.[[0-9]]*\) .*$/\1/'`
        AC_MSG_RESULT($GO_VERSION)
        AC_SUBST(GO_VERSION)

        AX_COMPARE_VERSION([$ax_go_version],[le],[$GO_VERSION],
            [$2],
            [$3])
    ],[
        AC_MSG_WARN([could not find the go executable])
        $3
    ])
])
