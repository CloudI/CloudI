#-*-Mode:m4;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=m4 fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
#
# SYNOPSIS
#
#   AX_SYSCALL_LOCK()
#
# DESCRIPTION
#
#   Determine if it is possible to lock access to syscalls on this
#   operating system (pledge on OpenBSD, libseccomp-dev on Linux).
#
# MIT License
#
# Copyright (c) 2020 Michael Truog <mjtruog at protonmail dot com>
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

AC_DEFUN([AX_SYSCALL_LOCK],
[
    AS_CASE([$host_os],
            [openbsd*], [syscall_lock="pledge"],
            [linux*], [syscall_lock="libseccomp"],
            [*], [syscall_lock=""])

    AC_LANG_PUSH([C])

    if test "x$syscall_lock" = "xpledge"; then
        AC_LINK_IFELSE(
            [AC_LANG_PROGRAM([[
#include <unistd.h>
             ]], [[
pledge(0,"stdio");
             ]])],
            [],
            [syscall_lock=""])
    elif test "x$syscall_lock" = "xlibseccomp"; then
        AX_CHECK_PRIVATE_LIB(seccomp, seccomp_init,
            [AC_LANG_PROGRAM([[
#include <seccomp.h>
             ]], [[
seccomp_init(SCMP_ACT_KILL_PROCESS);
             ]])],
            [AC_LINK_IFELSE(
                 [AC_LANG_PROGRAM([[
#include <sys/prctl.h>
                  ]], [[
prctl(PR_SET_NO_NEW_PRIVS, 1);
                  ]])],
                 [],
                 [syscall_lock=""])],
            [syscall_lock=""])
    fi
    SYSCALL_LOCK_TYPE="undefined"
    SYSCALL_LOCK_LDFLAGS=""
    SYSCALL_LOCK_LIB=""
    AC_MSG_CHECKING([for syscall_lock])
    if test "x$syscall_lock" = "xpledge"; then
        SYSCALL_LOCK_TYPE="pledge"
        AC_DEFINE([SYSCALL_LOCK_USE_PLEDGE], [1],
                  [Provide syscall_lock option with pledge.])
        AC_MSG_RESULT([pledge])
    elif test "x$syscall_lock" = "xlibseccomp"; then
        SYSCALL_LOCK_TYPE="function"
        SYSCALL_LOCK_LDFLAGS=$SECCOMP_LDFLAGS
        SYSCALL_LOCK_LIB=$SECCOMP_LIB
        AC_DEFINE([SYSCALL_LOCK_USE_SECCOMP], [1],
                  [Provide syscall_lock option with seccomp.])
        AC_MSG_RESULT([seccomp])
    else
        AC_MSG_RESULT([none])
    fi
    AC_LANG_POP([C])
    AC_SUBST(SYSCALL_LOCK_TYPE)
    AC_SUBST(SYSCALL_LOCK_LDFLAGS)
    AC_SUBST(SYSCALL_LOCK_LIB)
])
