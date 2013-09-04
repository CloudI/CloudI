#-*-Mode:m4;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=m4 fenc=utf-8 sts=4 ts=4 sw=4 et:
#
# SYNOPSIS
#
#   AX_ZEROMQ([OTHER-INC_PATHS], [OTHER-LIB_PATHS])
#
# DESCRIPTION
#
#   Determine if ZeroMQ can be found on the system and whether ZeroMQ
#   support should be built.
#
#   This macro sets:
#
#     ZEROMQ_CFLAGS
#     ZEROMQ_LDFLAGS
#     ZEROMQ_VERSION_MAJOR
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

AC_DEFUN([AX_ZEROMQ],
[
    default_zeromq_version="3"
    AC_ARG_WITH([zeromq],
        [AS_HELP_STRING([--with-zeromq@<:@=ARG@:>@],
            [use ZeroMQ library from a standard location (ARG=yes),
             from a local installation (ARG=build),
             from the specified location (ARG=<path>),
             or disable it (ARG=no)
             @<:@ARG=no@:>@ ])],
            [
            if test "$withval" = "no"; then
                want_zeromq="no"
                build_zeromq=""
            elif test "$withval" = "yes"; then
                want_zeromq="yes"
                ac_zeromq_path=""
                build_zeromq=""
            elif test "$withval" = "build"; then
                want_zeromq="yes"
                ac_zeromq_path=""
                build_zeromq="yes"
            else
                want_zeromq="yes"
                ac_zeromq_path="$withval"
                build_zeromq=""
            fi
            ],
            [want_zeromq="no"])
    AC_ARG_WITH([zeromq_version],
        [AS_HELP_STRING([--with-zeromq-version@<:@=ARG@:>@],
            [specify the version of the ZeroMQ library (ARG=2|3)
             @<:@ARG=3@:>@ ])],
            [
            if test "$withval" = "2"; then
                want_zeromq_version="2"
            elif test "$withval" = "3"; then
                want_zeromq_version="3"
            else
                AC_MSG_ERROR([ZeroMQ version 2 or 3, not $withval])
            fi
            ],
            [want_zeromq_version=$default_zeromq_version])
    if test "x$want_zeromq" = "xyes"; then
        local_zeromq_path=`(cd $srcdir; pwd)`"/../install/zeromq/v$want_zeromq_version"
        if test "x$build_zeromq" != "xyes"; then
            AC_LANG_PUSH([C])
            if test "x$ac_zeromq_path" != "x"; then
                ac_zeromq_path_include="$ac_zeromq_path/include"
                ac_zeromq_path_lib="$ac_zeromq_path/lib"
                ZEROMQ_CFLAGS="-I$ac_zeromq_path_include"
                ZEROMQ_LDFLAGS="-L$ac_zeromq_path_lib"
            elif test -f "$local_zeromq_path/include/zmq.h"; then
                ac_zeromq_path="$local_zeromq_path"
                ac_zeromq_path_include="$ac_zeromq_path/include"
                ac_zeromq_path_lib="$ac_zeromq_path/lib"
                ZEROMQ_CFLAGS="-I$ac_zeromq_path_include"
                ZEROMQ_LDFLAGS="-L$ac_zeromq_path_lib"
            else
                AC_PATH_PROG([zeromq_path_bin_queue], [zmq_queue])
                if test "x$zeromq_path_bin_queue" != "x"; then
                    dnl old v2 installation
                    zeromq_path_bin=`AS_DIRNAME([$zeromq_path_bin_queue])`
                    ac_zeromq_path=`AS_DIRNAME([$zeromq_path_bin])`
                    ac_zeromq_path_include="$ac_zeromq_path/include"
                    ac_zeromq_path_lib="$ac_zeromq_path/lib"
                    ZEROMQ_CFLAGS="-I$ac_zeromq_path_include"
                    ZEROMQ_LDFLAGS="-L$ac_zeromq_path_lib"
                else
                    AX_CHECK_PRIVATE_HEADER(zmq.h,
                        [ac_zeromq_path=`AS_DIRNAME([$ZMQ_H_PATH])`
                         ac_zeromq_path_include=$ZMQ_H_PATH
                         ac_zeromq_path_lib="$ac_zeromq_path/lib"
                         ZEROMQ_CFLAGS=$ZMQ_H_CFLAGS
                         ZEROMQ_LDFLAGS="-L$ac_zeromq_path_lib"
                        ],
                        [ac_zeromq_path=""
                         ac_zeromq_path_include=""
                         ac_zeromq_path_lib=""
                         ZEROMQ_CFLAGS=""
                         ZEROMQ_LDFLAGS=""], , $1)
                fi
            fi
    
            dnl check ZeroMQ installation
            CFLAGS_SAVED="$CFLAGS"
            CFLAGS="$CFLAGS $ZEROMQ_CFLAGS"
            export CFLAGS
            CPPFLAGS_SAVED="$CPPFLAGS"
            CPPFLAGS="$CFLAGS $ZEROMQ_CFLAGS"
            export CPPFLAGS
            AC_DEFINE_UNQUOTED([VERSION_MAJOR_MIN], $want_zeromq_version,
                               [Requested ZeroMQ major version])
            AC_PREPROC_IFELSE([AC_LANG_PROGRAM([[
#include <zmq.h>
#if ZMQ_VERSION_MAJOR < VERSION_MAJOR_MIN
#error ZeroMQ version is too old
#endif
                 ]], [[]])],
                [build_zeromq="no"],
                [build_zeromq="yes"])
            if test "x$build_zeromq" = "xno"; then
                LDFLAGS_SAVED="$LDFLAGS"
                LDFLAGS="$LDFLAGS $ZEROMQ_LDFLAGS"
                export LDFLAGS
                AX_CHECK_PRIVATE_LIB(zmq, zmq_init, [AC_LANG_PROGRAM([[
#include <zmq.h>
                                      ]], [[
void * ctx = zmq_init(1);
void * socket = zmq_socket(ctx, ZMQ_REQ);
zmq_close(socket);
zmq_term(ctx);
                                      ]])], ,
                                     [build_zeromq="yes"], , $2)
                LDFLAGS="$LDFLAGS_SAVED"
                export LDFLAGS
            fi
            CFLAGS="$CFLAGS_SAVED"
            export CFLAGS
            CPPFLAGS="$CPPFLAGS_SAVED"
            export CPPFLAGS
            AC_LANG_POP([C])
        fi

        if test "x$build_zeromq" = "xyes"; then
            AC_MSG_CHECKING(for ZeroMQ v$want_zeromq_version)
            AC_MSG_RESULT(building)
            abs_top_srcdir=`cd $srcdir; pwd`
            AC_CONFIG_COMMANDS([zeromq],
                [(cd $SRCDIR/external/zeromq/v$ZEROMQ_VERSION_MAJOR/zeromq/ && \
                  ./configure --prefix=$PREFIX --enable-static \
                              --with-pic && \
                  make && make install && \
                  echo "ZeroMQ locally installed" || exit 1)],
                [PREFIX=$local_zeromq_path
                 SRCDIR=$abs_top_srcdir
                 ZEROMQ_VERSION_MAJOR=$want_zeromq_version])
            ac_zeromq_path="$local_zeromq_path"
            ac_zeromq_path_include="$ac_zeromq_path/include"
            ac_zeromq_path_lib="$ac_zeromq_path/lib"
            ZEROMQ_CFLAGS="-I$ac_zeromq_path_include"
            ZEROMQ_LDFLAGS="-L$ac_zeromq_path_lib"
            ZEROMQ_LIB_PATH="$ac_zeromq_path_lib"
        else
            AC_MSG_CHECKING(for ZeroMQ v$want_zeromq_version)
            AC_MSG_RESULT(found)
            dnl ZMQ_LDFLAGS and ZMQ_PATH empty if not found in $2
            if test "x$ZMQ_PATH" = "x"; then
                ZEROMQ_LIB_PATH="$ac_zeromq_path_lib"
            else
                ac_zeromq_path_lib=$ZMQ_PATH
                ZEROMQ_LIB_PATH=$ZMQ_PATH
            fi
            if test "x$ZMQ_LDFLAGS" = "x"; then
                ZEROMQ_LDFLAGS="-L$ac_zeromq_path_lib"
            else
                ZEROMQ_LDFLAGS=$ZMQ_LDFLAGS
            fi
        fi
        AC_SUBST(ZEROMQ_LDFLAGS)
        AC_SUBST(ZEROMQ_CFLAGS)
        AC_SUBST(ZEROMQ_LIB_PATH)

        ZEROMQ_VERSION_MAJOR="$want_zeromq_version"
        AC_SUBST(ZEROMQ_VERSION_MAJOR)
    fi
])
