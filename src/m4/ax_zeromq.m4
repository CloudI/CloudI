# -*- coding: utf-8; Mode: m4; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
# ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
#
# SYNOPSIS
#
#   AX_ZEROMQ
#
# DESCRIPTION
#
#   Determine if ZeroMQ can be found on the system and whether ZeroMQ
#   support should be built.
#
#   This macro sets:
#
#     ZEROMQ_CFLAGS
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

AC_DEFUN([AX_ZEROMQ],
[
    AC_ARG_WITH([zeromq],
        [AS_HELP_STRING([--with-zeromq@<:@=ARG@:>@],
            [use ZeroMQ library from a standard location (ARG=yes),
             from the specified location (ARG=<path>),
             or disable it (ARG=no)
             @<:@ARG=no@:>@ ])],
            [
            if test "$withval" = "no"; then
                want_zeromq="no"
            elif test "$withval" = "yes"; then
                want_zeromq="yes"
                ac_zeromq_path=""
            else
                want_zeromq="yes"
                ac_zeromq_path="$withval"
            fi
            ],
            [want_zeromq="no"])

    if test "x$want_zeromq" = "xyes"; then
        local_zeromq_path=`(cd $srcdir; pwd)`"/../install/zeromq"
        if test "x$ac_zeromq_path" != "x"; then
            ZEROMQ_CFLAGS="-I$ac_zeromq_path/include"
        elif test -f "$local_zeromq_path/include/zmq.h"; then
            ac_zeromq_path="$local_zeromq_path"
            ZEROMQ_CFLAGS="-I$local_zeromq_path/include"
        else
            AC_PATH_PROG([zeromq_path_bin_queue], [zmq_queue], , )
            if test "x$zeromq_path_bin_queue" != "x"; then
                zeromq_path_bin=`AS_DIRNAME([$zeromq_path_bin_queue])`
                ac_zeromq_path=`AS_DIRNAME([$zeromq_path_bin])`
                ZEROMQ_CFLAGS="-I$ac_zeromq_path/include"
            else
                ZEROMQ_CFLAGS=""
            fi
        fi
        AC_MSG_CHECKING(for ZeroMQ)

        CFLAGS_SAVED="$CFLAGS"
        CFLAGS="$CFLAGS $ZEROMQ_CFLAGS"
        export CFLAGS

        AC_LANG_PUSH(C)
        AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
            @%:@include <zmq.h>]], [[
            #if ZMQ_VERSION_MAJOR >= 2
            /* ok */
            #else
            #error ZeroMQ version is too old
            #endif
            ]])],[
            AC_MSG_RESULT($ac_zeromq_path)
            build_zeromq="no"],[
            build_zeromq="yes"])
        AC_LANG_POP([C])
        if test "x$build_zeromq" = "xyes"; then
            AC_CONFIG_COMMANDS([zeromq],
                [(cd external/zeromq/ && \
                  ./configure --prefix=$PREFIX --enable-static \
                              --with-pic && \
                  make && make install && \
                  echo "ZeroMQ locally installed" || exit 1)],
                [PREFIX=$local_zeromq_path])

            ZEROMQ_CFLAGS="-I$local_zeromq_path/include"
            ac_zeromq_path="$local_zeromq_path"
            AC_MSG_RESULT(building)
        fi
        AC_SUBST(ZEROMQ_CFLAGS)
        CFLAGS="$CFLAGS_SAVED"

        ZEROMQ_ROOT_DIR="$ac_zeromq_path"
        AC_SUBST(ZEROMQ_ROOT_DIR)
    fi
])
