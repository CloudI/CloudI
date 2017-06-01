//-*-Mode:C++;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=cpp fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
//
// MIT License
//
// Copyright (c) 2015-2017 Michael Truog <mjtruog at gmail dot com>
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.
//

#include <sys/types.h>
#include <pwd.h>
#include <grp.h>
#include <unistd.h>
#include <cstdio>
#include "cloudi_os_owner.hpp"

int owner_get(uint64_t & user_i,
              char * user_str, uint32_t user_str_len,
              uint64_t & group_i,
              char * group_str, uint32_t group_str_len)
{
    uid_t uid = static_cast<uid_t>(user_i);
    gid_t gid = static_cast<gid_t>(group_i);
    if (uid == 0 && user_str_len > 1)
    {
        struct passwd * user_p = ::getpwnam(user_str);
        if (user_p == 0)
        {
            ::perror("getpwnam: ");
            return -1;
        }
        uid = user_p->pw_uid;
        if (gid == 0 && group_str_len == 1)
        {
            gid = user_p->pw_gid;
        }
    }
    if (gid == 0 && group_str_len > 1)
    {
        struct group * group_p = ::getgrnam(group_str);
        if (group_p == 0)
        {
            ::perror("getgrnam: ");
            return -1;
        }
        gid = group_p->gr_gid;
    }
    user_i = static_cast<uint64_t>(uid);
    group_i = static_cast<uint64_t>(gid);
    return 0;
}

int owner_set(uint64_t const user_i,
              uint64_t const group_i)
{
    uid_t const uid = static_cast<uid_t>(user_i);
    gid_t const gid = static_cast<gid_t>(group_i);
    if (gid > 0)
    {
        if (::setgid(gid) == -1)
        {
            ::perror("setgid: ");
            return -1;
        }
    }
    if (uid > 0)
    {
        if (::setuid(uid) == -1)
        {
            ::perror("setuid: ");
            return -1;
        }
    }
    return 0;
}

