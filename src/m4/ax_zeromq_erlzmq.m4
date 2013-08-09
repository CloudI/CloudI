#-*-Mode:m4;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=m4 fenc=utf-8 sts=4 ts=4 sw=4 et:
#
# SYNOPSIS
#
#   AX_ZEROMQ_ERLZMQ
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
        AC_CONFIG_COMMANDS([zeromq_erlzmq],
            [(cd $SRCDIR/external/zeromq/v$ZEROMQ_VERSION_MAJOR/erlzmq/ && \
              ZEROMQ_CFLAGS=$ZEROMQ_CFLAGS \
              ZEROMQ_LDFLAGS=$ZEROMQ_LDFLAGS \
              ZEROMQ_LIB_PATH=$ZEROMQ_LIB_PATH \
              $BUILDDIR/rebar compile && \
              echo "erlzmq compiled" || exit 1)],
            [ZEROMQ_CFLAGS=$ZEROMQ_CFLAGS
             ZEROMQ_LDFLAGS=$ZEROMQ_LDFLAGS
             ZEROMQ_LIB_PATH=$ZEROMQ_LIB_PATH
             ZEROMQ_VERSION_MAJOR=$ZEROMQ_VERSION_MAJOR
             ERLANG_ROOT_DIR=$ERLANG_ROOT_DIR
             SRCDIR=$abs_top_srcdir
             BUILDDIR=$abs_top_builddir])
        ZEROMQ_ERLZMQ_RELTOOL="{app, erlzmq, @<:@{incl_cond, include}, {mod_cond, all}, {app_file, keep}@:>@},"
        ZEROMQ_ERLZMQ_APPCONF="erlzmq,"
        ZEROMQ_ERLZMQ_PATH=',"external/zeromq/v'$ZEROMQ_VERSION_MAJOR'"'
    fi
    AC_SUBST(ZEROMQ_ERLZMQ_RELTOOL)
    AC_SUBST(ZEROMQ_ERLZMQ_APPCONF)
    AC_SUBST(ZEROMQ_ERLZMQ_PATH)
])
