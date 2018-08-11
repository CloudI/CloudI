#-*-Mode:m4;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=m4 fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
#
# SYNOPSIS
#
#   AX_BOOST_VERSION()
#
# DESCRIPTION
#
#   The boost version is returned in the cache variable
#   $ax_cv_cxx_boost_version .
#
# MIT License
#
# Copyright (c) 2018 Michael Truog <mjtruog at protonmail dot com>
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

AC_DEFUN([AX_BOOST_VERSION],
[
    AC_CACHE_CHECK([for boost compilation version], ax_cv_cxx_boost_version, [
    CPPFLAGS_SAVED="$CPPFLAGS"
    CPPFLAGS="$CPPFLAGS $BOOST_CPPFLAGS"
    export CPPFLAGS
    LDFLAGS_SAVED="$LDFLAGS"
    LDFLAGS="$LDFLAGS $BOOST_LDFLAGS"
    export LDFLAGS

    AC_LANG_PUSH(C++)
    AC_RUN_IFELSE(
        [AC_LANG_PROGRAM([
             @%:@include <boost/version.hpp>
             @%:@include <fstream>], [
             std::ofstream output("conftest.out", std::ofstream::binary);
             output << BOOST_LIB_VERSION << std::endl;
             output.close();
             ])],
        [ax_cv_cxx_boost_version=`cat conftest.out`
         rm -f conftest.out],
        [rm -f conftest.out
         AC_MSG_FAILURE([C++ boost version info execution failed])])
    AC_LANG_POP(C++)
    CPPFLAGS="$CPPFLAGS_SAVED"
    LDFLAGS="$LDFLAGS_SAVED"
    ])
])

