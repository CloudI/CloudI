#-*-Mode:m4;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=m4 fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
#
# SYNOPSIS
#
#   AX_PYTHON_C
#
# DESCRIPTION
#
#   Determine the location of the Python C API.
#
#   This macro sets:
#
#     PYTHON_CFLAGS
#     PYTHON_LDFLAGS
#
# MIT License
#
# Copyright (c) 2012-2017 Michael Truog <mjtruog at protonmail dot com>
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

AC_DEFUN([AX_PYTHON_C],[
    if test "x$PYTHON_VERSION_REQUIRED" = "x2"; then
        executable="python"
    else
        executable="python$PYTHON_VERSION_REQUIRED"
    fi
    if test "x$PYTHON_DEBUG" == "xno"; then
        executable="$executable-config"
    else
        executable="$executable-dbg-config"
    fi
    AC_ARG_VAR([PYTHON_CONFIG],[python-config executable])
    AC_PATH_PROGS([PYTHON_CONFIG],
        [$executable],
        [AC_MSG_ERROR([python-config not found])],
        [`AS_DIRNAME("$PYTHON")`])
    PYTHON_CFLAGS=`$PYTHON_CONFIG --cflags`
    AC_SUBST(PYTHON_CFLAGS)
    PYTHON_LDFLAGS=`$PYTHON_CONFIG --ldflags`
    AC_SUBST(PYTHON_LDFLAGS)
])

