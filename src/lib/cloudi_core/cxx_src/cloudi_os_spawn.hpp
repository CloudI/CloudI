//-*-Mode:C++;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=cpp fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
//
// MIT License
//
// Copyright (c) 2011-2017 Michael Truog <mjtruog at protonmail dot com>
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
#ifndef CLOUDI_OS_SPAWN_HPP
#define CLOUDI_OS_SPAWN_HPP

#include <stdint.h>
int32_t spawn(char protocol,
              char * socket_path, uint32_t socket_path_len,
              uint32_t * ports, uint32_t ports_len,
              char * rlimits, uint32_t rlimits_len,
              uint64_t user_i,
              char * user_str, uint32_t user_str_len,
              uint64_t group_i,
              char * group_str, uint32_t group_str_len,
              int32_t nice,
              char * chroot_directory, uint32_t chroot_directory_len,
              char * directory, uint32_t directory_len,
              char * filename, uint32_t filename_len,
              char * argv, uint32_t argv_len,
              char * env, uint32_t env_len);

#endif // CLOUDI_OS_SPAWN_HPP
