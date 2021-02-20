//-*-Mode:C++;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=cpp fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
//
// MIT License
//
// Copyright (c) 2020-2021 Michael Truog <mjtruog at protonmail dot com>
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
#include <errno.h>
#include <signal.h>
#include "port.hpp"
#include "cloudi_os_command.hpp"
#include "assert.hpp"

namespace
{
    char const * errno_kill_string()
    {
        switch (errno)
        {
            case EINVAL:
                return "EINVAL";
            case EPERM:
                return "EPERM";
            case ESRCH:
                return "ESRCH";
            default:
                return "unknown";
        }
    }
}

bool terminate_now()
{
    return true;
}

char const * kill_pids(uint32_t signal, bool group,
                       uint32_t * pids, uint32_t pids_len)
{
    char const * result = "";
    for (size_t i = 0; i < pids_len; ++i)
    {
        pid_t pid = static_cast<pid_t>(pids[i]);
        if (group)
            pid *= -1;
        int const status = ::kill(pid, static_cast<int>(signal));
        if (status == -1)
        {
            result = errno_kill_string();
            break;
        }
    }
    return result;
}

int main()
{
    assert_initialize();
    return GEPD::default_main();
}

