#-*-Mode:m4;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=m4 fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
#
# SYNOPSIS
#
#   AX_ZEROMQ_ERLZMQ([OTHER-INC_PATHS], [OTHER-LIB_PATHS])
#
# DESCRIPTION
#
#   Build the ZeroMQ Erlang bindings
#
#   This macro sets:
#
#     ZEROMQ_ERLZMQ_RELTOOL
#     ZEROMQ_ERLZMQ_APPCONF
#     ZEROMQ_ERLZMQ_PATH
#
# MIT License
#
# Copyright (c) 2011-2017 Michael Truog <mjtruog at protonmail dot com>
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

AC_DEFUN([AX_ZEROMQ_ERLZMQ],
[
    AC_REQUIRE([AC_ERLANG_SUBST_ROOT_DIR])
    AC_REQUIRE([AX_ERLANG_REQUIRE_OTP_VER])
    AC_REQUIRE([AX_ZEROMQ])

    AC_MSG_CHECKING(for ZeroMQ)
    if test "x$ZEROMQ_VERSION_MAJOR" = "x"; then
        AC_MSG_RESULT(no)
        ZEROMQ_ERLZMQ_RELTOOL=""
        ZEROMQ_ERLZMQ_APPCONF=""
        ZEROMQ_ERLZMQ_PATH=""
    else
        AC_MSG_RESULT(yes)
        AX_ERLANG_REQUIRE_OTP_VER([R14B02], ,
            [AC_MSG_ERROR([Erlang >= R14B02 required for erlzmq usage in cloudi_service_zeromq])])
        abs_top_srcdir=`cd $srcdir; pwd`
        if test -z "$REBAR"; then
            AC_MSG_ERROR([rebar not found!])
        fi
        AX_UUID($1, $2)
        AC_CONFIG_COMMANDS([zeromq_erlzmq],
            [(cd $SRCDIR/external/zeromq/v$ZEROMQ_VERSION_MAJOR/erlzmq/ && \
              ZEROMQ_CFLAGS=$ZEROMQ_CFLAGS \
              ZEROMQ_LDFLAGS=$ZEROMQ_LDFLAGS \
              ZEROMQ_LIB_PATH=$ZEROMQ_LIB_PATH \
              $REBAR compile && \
              echo "erlzmq compiled" || exit 1)],
            [ZEROMQ_CFLAGS="$ZEROMQ_CFLAGS $UUID_UUID_H_CFLAGS"
             ZEROMQ_LDFLAGS="$ZEROMQ_LDFLAGS $UUID_LDFLAGS"
             ZEROMQ_LIB_PATH=$ZEROMQ_LIB_PATH
             ZEROMQ_VERSION_MAJOR=$ZEROMQ_VERSION_MAJOR
             ERLANG_ROOT_DIR=$ERLANG_ROOT_DIR
             SRCDIR=$abs_top_srcdir
             REBAR=$REBAR])
        ZEROMQ_ERLZMQ_RELTOOL="{app, erlzmq, @<:@{incl_cond, include}, {mod_cond, all}, {app_file, keep}@:>@},"
        ZEROMQ_ERLZMQ_APPCONF="erlzmq,"
        ZEROMQ_ERLZMQ_PATH=',"external/zeromq/v'$ZEROMQ_VERSION_MAJOR'"'
    fi
    AC_SUBST(ZEROMQ_ERLZMQ_RELTOOL)
    AC_SUBST(ZEROMQ_ERLZMQ_APPCONF)
    AC_SUBST(ZEROMQ_ERLZMQ_PATH)
])
