//-*-Mode:C++;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=cpp fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
//
// MIT License
//
// Copyright (c) 2020 Michael Truog <mjtruog at protonmail dot com>
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

#include <sstream>
#include <iostream>
#include "config.h"
#include "cloudi_os_syscall_lock.hpp"

#if defined(SYSCALL_LOCK_USE_PLEDGE)
#include <unistd.h>
int syscall_lock_set(char const * const * const syscall_names)
{
    std::ostringstream exec_promises;
    bool space = false;
    char const * const * current = syscall_names;
    for (char const * name; (name = *current); ++current)
    {
        if (space)
        {
            exec_promises << " " << name;
        }
        else
        {
            exec_promises << name;
            space = true;
        }
    }
    std::string const & exec_promises_str = exec_promises.str();
    if (::pledge(0, exec_promises_str.c_str()) == -1)
        return -1;
    return 0;
}
#elif defined(SYSCALL_LOCK_USE_SECCOMP)
#include <sys/prctl.h>
#include <seccomp.h>
int syscall_lock_set(char const * const * const syscall_names)
{
    uint32_t const lock_action = SCMP_ACT_KILL_PROCESS;
    scmp_filter_ctx ctx = ::seccomp_init(lock_action);
    int status = 0;
    char const * const * current = syscall_names;
    for (char const * name; (name = *current); ++current)
    {
        int syscall_index = ::seccomp_syscall_resolve_name(name);
        if (syscall_index < 0)
        {
            std::cerr << "syscall_lock: " << name <<
                " does not exist!" << std::endl;
            status = -1;
            break;
        }
        if (::seccomp_rule_add(ctx, SCMP_ACT_ALLOW, syscall_index, 0))
        {
            status = -1;
            break;
        }
    }
    if (status == 0)
    {
        if (::prctl(PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0))
            status = -1;
    }
    if (status == 0)
        ::seccomp_load(ctx);
    else
        ::seccomp_release(ctx);
    return status;
}
#else
int syscall_lock_set(char const * const * const)
{
    return -1;
}
#endif
