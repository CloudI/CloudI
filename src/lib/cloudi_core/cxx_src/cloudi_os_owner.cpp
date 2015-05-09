//-*-Mode:C++;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=cpp fenc=utf-8 sts=4 ts=4 sw=4 et:
//
// BSD LICENSE
// 
// Copyright (c) 2015, Michael Truog <mjtruog at gmail dot com>
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
// 
//     * Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above copyright
//       notice, this list of conditions and the following disclaimer in
//       the documentation and/or other materials provided with the
//       distribution.
//     * All advertising materials mentioning features or use of this
//       software must display the following acknowledgment:
//         This product includes software developed by Michael Truog
//     * The name of the author may not be used to endorse or promote
//       products derived from this software without specific prior
//       written permission
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
// CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
// INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
// OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
// BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
// WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
// NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
// DAMAGE.
//

#include <sys/types.h>
#include <pwd.h>
#include <grp.h>
#include <unistd.h>
#include <cstdio>
#include "cloudi_os_owner.hpp"

int owner(uint64_t user_i,
          char * user_str, uint32_t user_str_len,
          uint64_t group_i,
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

