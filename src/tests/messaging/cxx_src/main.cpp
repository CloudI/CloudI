/* -*-Mode:C++;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
 * ex: set ft=cpp fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
 *
 * BSD LICENSE
 * 
 * Copyright (c) 2012-2015, Michael Truog <mjtruog at gmail dot com>
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * 
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in
 *       the documentation and/or other materials provided with the
 *       distribution.
 *     * All advertising materials mentioning features or use of this
 *       software must display the following acknowledgment:
 *         This product includes software developed by Michael Truog
 *     * The name of the author may not be used to endorse or promote
 *       products derived from this software without specific prior
 *       written permission
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
 * CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */
#include "cloudi.hpp"
#include "thread_pool.hpp"
#include <unistd.h>
#include <iostream>
#include <cstring>
#include <sstream>
#include "assert.hpp"

class ThreadData
{
};

class OutputData
{
    public:
        OutputData & setError(int value)
        {
            m_error = value;
            return *this;
        }
        int error() const { return m_error; }

    private:
        int m_error;
};

class Input
{
    public:
        Input(unsigned int const thread_index) :
            m_stop_default(false),
            m_stop(m_stop_default),
            m_api(thread_index),
            m_thread_index(thread_index)
        {
        }

        uint32_t timeout_terminate() const
        {
            return m_api.timeout_terminate();
        }

        OutputData process(bool const & stop, ThreadData & /*data*/)
        {
            int result;
            result = m_api.subscribe("a/b/c/d", *this,
                                     &Input::sequence1_abcd);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("a/b/c/*", *this,
                                     &Input::sequence1_abc_);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("a/b/*/d", *this,
                                     &Input::sequence1_ab_d);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("a/*/c/d", *this,
                                     &Input::sequence1_a_cd);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("*/b/c/d", *this,
                                     &Input::sequence1__bcd);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("a/b/*", *this,
                                     &Input::sequence1_ab__);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("a/*/d", *this,
                                     &Input::sequence1_a__d);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("*/c/d", *this,
                                     &Input::sequence1___cd);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("a/*", *this,
                                     &Input::sequence1_a___);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("*/d", *this,
                                     &Input::sequence1____d);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("*", *this,
                                     &Input::sequence1_____);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("sequence1", *this,
                                     &Input::sequence1);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("e", *this,
                                     &Input::sequence2_e1);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("e", *this,
                                     &Input::sequence2_e2);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("e", *this,
                                     &Input::sequence2_e3);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("e", *this,
                                     &Input::sequence2_e4);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("e", *this,
                                     &Input::sequence2_e5);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("e", *this,
                                     &Input::sequence2_e6);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("e", *this,
                                     &Input::sequence2_e7);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("e", *this,
                                     &Input::sequence2_e8);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("sequence2", *this,
                                     &Input::sequence2);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("f1", *this,
                                     &Input::sequence3_f1);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("f2", *this,
                                     &Input::sequence3_f2);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("g1", *this,
                                     &Input::sequence3_g1);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("sequence3", *this,
                                     &Input::sequence3);
            assert(result == CloudI::API::return_value::success);
            if (m_thread_index == 0)
            {
                result = m_api.send_async(std::string(m_api.prefix()) +
                                          "sequence1", "start", 6);
                assert(result == CloudI::API::return_value::success);
            }

            OutputData resultObject;
            int value;
            m_stop = stop;
            while (CloudI::API::return_value::timeout ==
                   (value = m_api.poll(1000)))
            {
                if (stop)
                    return resultObject.setError(
                        CloudI::API::return_value::success);
            }
            return resultObject.setError(value);
        }

    private:
        void sequence1_abcd(CloudI::API const & api,
                            int const command,
                            std::string const & name,
                            std::string const & pattern,
                            void const * const /*request_info*/,
                            uint32_t const /*request_info_size*/,
                            void const * const request,
                            uint32_t const request_size,
                            uint32_t timeout,
                            int8_t /*priority*/,
                            char const * const trans_id,
                            char const * const pid,
                            uint32_t const pid_size)
        {
            assert(pattern == (std::string(api.prefix()) + "a/b/c/d"));
            assert(request_size == 6);
            assert(::memcmp(request, "test1", 6) == 0);
            api.return_(command, name, pattern, "", 0, request, request_size,
                        timeout, trans_id, pid, pid_size);
        }

        void sequence1_abc_(CloudI::API const & api,
                            int const command,
                            std::string const & name,
                            std::string const & pattern,
                            void const * const /*request_info*/,
                            uint32_t const /*request_info_size*/,
                            void const * const request,
                            uint32_t const request_size,
                            uint32_t timeout,
                            int8_t /*priority*/,
                            char const * const trans_id,
                            char const * const pid,
                            uint32_t const pid_size)
        {
            assert(pattern == (std::string(api.prefix()) + "a/b/c/*"));
            assert(request_size == 6);
            assert(::memcmp(request, "test2", 6) == 0 ||
                   ::memcmp(request, "test3", 6) == 0);
            api.return_(command, name, pattern, "", 0, request, request_size,
                        timeout, trans_id, pid, pid_size);
        }

        void sequence1_ab_d(CloudI::API const & api,
                            int const command,
                            std::string const & name,
                            std::string const & pattern,
                            void const * const /*request_info*/,
                            uint32_t const /*request_info_size*/,
                            void const * const request,
                            uint32_t const request_size,
                            uint32_t timeout,
                            int8_t /*priority*/,
                            char const * const trans_id,
                            char const * const pid,
                            uint32_t const pid_size)
        {
            assert(pattern == (std::string(api.prefix()) + "a/b/*/d"));
            assert(request_size == 6);
            assert(::memcmp(request, "test4", 6) == 0 ||
                   ::memcmp(request, "test5", 6) == 0);
            api.return_(command, name, pattern, "", 0, request, request_size,
                        timeout, trans_id, pid, pid_size);
        }

        void sequence1_a_cd(CloudI::API const & api,
                            int const command,
                            std::string const & name,
                            std::string const & pattern,
                            void const * const /*request_info*/,
                            uint32_t const /*request_info_size*/,
                            void const * const request,
                            uint32_t const request_size,
                            uint32_t timeout,
                            int8_t /*priority*/,
                            char const * const trans_id,
                            char const * const pid,
                            uint32_t const pid_size)
        {
            assert(pattern == (std::string(api.prefix()) + "a/*/c/d"));
            assert(request_size == 6);
            assert(::memcmp(request, "test6", 6) == 0 ||
                   ::memcmp(request, "test7", 6) == 0);
            api.return_(command, name, pattern, "", 0, request, request_size,
                        timeout, trans_id, pid, pid_size);
        }

        void sequence1__bcd(CloudI::API const & api,
                            int const command,
                            std::string const & name,
                            std::string const & pattern,
                            void const * const /*request_info*/,
                            uint32_t const /*request_info_size*/,
                            void const * const request,
                            uint32_t const request_size,
                            uint32_t timeout,
                            int8_t /*priority*/,
                            char const * const trans_id,
                            char const * const pid,
                            uint32_t const pid_size)
        {
            assert(pattern == (std::string(api.prefix()) + "*/b/c/d"));
            assert(request_size == 6);
            assert(::memcmp(request, "test8", 6) == 0 ||
                   ::memcmp(request, "test9", 6) == 0);
            api.return_(command, name, pattern, "", 0, request, request_size,
                        timeout, trans_id, pid, pid_size);
        }

        void sequence1_ab__(CloudI::API const & api,
                            int const command,
                            std::string const & name,
                            std::string const & pattern,
                            void const * const /*request_info*/,
                            uint32_t const /*request_info_size*/,
                            void const * const request,
                            uint32_t const request_size,
                            uint32_t timeout,
                            int8_t /*priority*/,
                            char const * const trans_id,
                            char const * const pid,
                            uint32_t const pid_size)
        {
            assert(pattern == (std::string(api.prefix()) + "a/b/*"));
            assert(request_size == 7);
            assert(::memcmp(request, "test10", 7) == 0);
            api.return_(command, name, pattern, "", 0, request, request_size,
                        timeout, trans_id, pid, pid_size);
        }

        void sequence1_a__d(CloudI::API const & api,
                            int const command,
                            std::string const & name,
                            std::string const & pattern,
                            void const * const /*request_info*/,
                            uint32_t const /*request_info_size*/,
                            void const * const request,
                            uint32_t const request_size,
                            uint32_t timeout,
                            int8_t /*priority*/,
                            char const * const trans_id,
                            char const * const pid,
                            uint32_t const pid_size)
        {
            assert(pattern == (std::string(api.prefix()) + "a/*/d"));
            assert(request_size == 7);
            assert(::memcmp(request, "test11", 7) == 0);
            api.return_(command, name, pattern, "", 0, request, request_size,
                        timeout, trans_id, pid, pid_size);
        }

        void sequence1___cd(CloudI::API const & api,
                            int const command,
                            std::string const & name,
                            std::string const & pattern,
                            void const * const /*request_info*/,
                            uint32_t const /*request_info_size*/,
                            void const * const request,
                            uint32_t const request_size,
                            uint32_t timeout,
                            int8_t /*priority*/,
                            char const * const trans_id,
                            char const * const pid,
                            uint32_t const pid_size)
        {
            assert(pattern == (std::string(api.prefix()) + "*/c/d"));
            assert(request_size == 7);
            assert(::memcmp(request, "test12", 7) == 0);
            api.return_(command, name, pattern, "", 0, request, request_size,
                        timeout, trans_id, pid, pid_size);
        }

        void sequence1_a___(CloudI::API const & api,
                            int const command,
                            std::string const & name,
                            std::string const & pattern,
                            void const * const /*request_info*/,
                            uint32_t const /*request_info_size*/,
                            void const * const request,
                            uint32_t const request_size,
                            uint32_t timeout,
                            int8_t /*priority*/,
                            char const * const trans_id,
                            char const * const pid,
                            uint32_t const pid_size)
        {
            assert(pattern == (std::string(api.prefix()) + "a/*"));
            assert(request_size == 7);
            assert(::memcmp(request, "test13", 7) == 0);
            api.return_(command, name, pattern, "", 0, request, request_size,
                        timeout, trans_id, pid, pid_size);
        }

        void sequence1____d(CloudI::API const & api,
                            int const command,
                            std::string const & name,
                            std::string const & pattern,
                            void const * const /*request_info*/,
                            uint32_t const /*request_info_size*/,
                            void const * const request,
                            uint32_t const request_size,
                            uint32_t timeout,
                            int8_t /*priority*/,
                            char const * const trans_id,
                            char const * const pid,
                            uint32_t const pid_size)
        {
            assert(pattern == (std::string(api.prefix()) + "*/d"));
            assert(request_size == 7);
            assert(::memcmp(request, "test14", 7) == 0);
            api.return_(command, name, pattern, "", 0, request, request_size,
                        timeout, trans_id, pid, pid_size);
        }

        void sequence1_____(CloudI::API const & api,
                            int const command,
                            std::string const & name,
                            std::string const & pattern,
                            void const * const /*request_info*/,
                            uint32_t const /*request_info_size*/,
                            void const * const request,
                            uint32_t const request_size,
                            uint32_t timeout,
                            int8_t /*priority*/,
                            char const * const trans_id,
                            char const * const pid,
                            uint32_t const pid_size)
        {
            assert(pattern == (std::string(api.prefix()) + "*"));
            assert(request_size == 7);
            assert(::memcmp(request, "test15", 7) == 0);
            api.return_(command, name, pattern, "", 0, request, request_size,
                        timeout, trans_id, pid, pid_size);
        }

        void sequence1(CloudI::API const & api,
                       int const command,
                       std::string const & name,
                       std::string const & pattern,
                       void const * const /*request_info*/,
                       uint32_t const /*request_info_size*/,
                       void const * const request,
                       uint32_t const request_size,
                       uint32_t timeout,
                       int8_t /*priority*/,
                       char const * const trans_id,
                       char const * const pid,
                       uint32_t const pid_size)
        {
            int result;
            result = api.recv_async(1000);
            assert(result == CloudI::API::return_value::success);
            while (::memcmp(api.get_response(), "end", 4) == 0)
            {
                // consume "end" and sleep
                result = api.recv_async(1000);
                assert(result == CloudI::API::return_value::success);
            }
            std::cout << "messaging sequence1 start c++" << std::endl;
            assert(request_size == 6);
            assert(::memcmp(request, "start", 6) == 0);
            // n.b., depends on cloudi_constants.hrl having
            // SERVICE_NAME_PATTERN_MATCHING defined
            result = api.send_async(std::string(api.prefix()) + "a/b/c/d",
                                    "test1", 6);
            assert(result == CloudI::API::return_value::success);
            char test1_id[16];
            ::memcpy(test1_id, api.get_trans_id(), 16);
            result = api.send_async(std::string(api.prefix()) + "a/b/c/z",
                                    "test2", 6);
            assert(result == CloudI::API::return_value::success);
            char test2_id[16];
            ::memcpy(test2_id, api.get_trans_id(), 16);
            result = api.send_async(std::string(api.prefix()) + "a/b/c/dd",
                                    "test3", 6);
            assert(result == CloudI::API::return_value::success);
            char test3_id[16];
            ::memcpy(test3_id, api.get_trans_id(), 16);
            result = api.send_async(std::string(api.prefix()) + "a/b/z/d",
                                    "test4", 6);
            assert(result == CloudI::API::return_value::success);
            char test4_id[16];
            ::memcpy(test4_id, api.get_trans_id(), 16);
            result = api.send_async(std::string(api.prefix()) + "a/b/cc/d",
                                    "test5", 6);
            assert(result == CloudI::API::return_value::success);
            char test5_id[16];
            ::memcpy(test5_id, api.get_trans_id(), 16);
            result = api.send_async(std::string(api.prefix()) + "a/z/c/d",
                                    "test6", 6);
            assert(result == CloudI::API::return_value::success);
            char test6_id[16];
            ::memcpy(test6_id, api.get_trans_id(), 16);
            result = api.send_async(std::string(api.prefix()) + "a/bb/c/d",
                                    "test7", 6);
            assert(result == CloudI::API::return_value::success);
            char test7_id[16];
            ::memcpy(test7_id, api.get_trans_id(), 16);
            result = api.send_async(std::string(api.prefix()) + "z/b/c/d",
                                    "test8", 6);
            assert(result == CloudI::API::return_value::success);
            char test8_id[16];
            ::memcpy(test8_id, api.get_trans_id(), 16);
            result = api.send_async(std::string(api.prefix()) + "aa/b/c/d",
                                    "test9", 6);
            assert(result == CloudI::API::return_value::success);
            char test9_id[16];
            ::memcpy(test9_id, api.get_trans_id(), 16);
            result = api.send_async(std::string(api.prefix()) + "a/b/czd",
                                    "test10", 7);
            assert(result == CloudI::API::return_value::success);
            char test10_id[16];
            ::memcpy(test10_id, api.get_trans_id(), 16);
            result = api.send_async(std::string(api.prefix()) + "a/bzc/d",
                                    "test11", 7);
            assert(result == CloudI::API::return_value::success);
            char test11_id[16];
            ::memcpy(test11_id, api.get_trans_id(), 16);
            result = api.send_async(std::string(api.prefix()) + "azb/c/d",
                                    "test12", 7);
            assert(result == CloudI::API::return_value::success);
            char test12_id[16];
            ::memcpy(test12_id, api.get_trans_id(), 16);
            result = api.send_async(std::string(api.prefix()) + "a/bzczd",
                                    "test13", 7);
            assert(result == CloudI::API::return_value::success);
            char test13_id[16];
            ::memcpy(test13_id, api.get_trans_id(), 16);
            result = api.send_async(std::string(api.prefix()) + "azbzc/d",
                                    "test14", 7);
            assert(result == CloudI::API::return_value::success);
            char test14_id[16];
            ::memcpy(test14_id, api.get_trans_id(), 16);
            result = api.send_async(std::string(api.prefix()) + "azbzczd",
                                    "test15", 7);
            assert(result == CloudI::API::return_value::success);
            char test15_id[16];
            ::memcpy(test15_id, api.get_trans_id(), 16);

            // n.b., depends on cloudi_constants.hrl having
            // RECV_ASYNC_STRATEGY == recv_async_select_oldest
            result = api.recv_async(test1_id, false);
            assert(result == CloudI::API::return_value::success);
            result = api.recv_async();
            assert(result == CloudI::API::return_value::success);
            char test1_check[6];
            assert(api.get_response_size() == 6);
            ::memcpy(test1_check, api.get_response(), 6);
            char test1_id_check[16];
            ::memcpy(test1_id_check, api.get_trans_id(), 16);
            assert(::memcmp(test1_check, "test1", 6) == 0);
            assert(::memcmp(test1_id_check, test1_id, 16) == 0);
            result = api.recv_async(test2_id, false);
            assert(result == CloudI::API::return_value::success);
            result = api.recv_async();
            assert(result == CloudI::API::return_value::success);
            char test2_check[6];
            assert(api.get_response_size() == 6);
            ::memcpy(test2_check, api.get_response(), 6);
            char test2_id_check[16];
            ::memcpy(test2_id_check, api.get_trans_id(), 16);
            assert(::memcmp(test2_check, "test2", 6) == 0);
            assert(::memcmp(test2_id_check, test2_id, 16) == 0);
            result = api.recv_async(test3_id, false);
            assert(result == CloudI::API::return_value::success);
            result = api.recv_async();
            assert(result == CloudI::API::return_value::success);
            char test3_check[6];
            assert(api.get_response_size() == 6);
            ::memcpy(test3_check, api.get_response(), 6);
            char test3_id_check[16];
            ::memcpy(test3_id_check, api.get_trans_id(), 16);
            assert(::memcmp(test3_check, "test3", 6) == 0);
            assert(::memcmp(test3_id_check, test3_id, 16) == 0);
            result = api.recv_async(test4_id, false);
            assert(result == CloudI::API::return_value::success);
            result = api.recv_async();
            assert(result == CloudI::API::return_value::success);
            char test4_check[6];
            assert(api.get_response_size() == 6);
            ::memcpy(test4_check, api.get_response(), 6);
            char test4_id_check[16];
            ::memcpy(test4_id_check, api.get_trans_id(), 16);
            assert(::memcmp(test4_check, "test4", 6) == 0);
            assert(::memcmp(test4_id_check, test4_id, 16) == 0);
            result = api.recv_async(test5_id, false);
            assert(result == CloudI::API::return_value::success);
            result = api.recv_async();
            assert(result == CloudI::API::return_value::success);
            char test5_check[6];
            assert(api.get_response_size() == 6);
            ::memcpy(test5_check, api.get_response(), 6);
            char test5_id_check[16];
            ::memcpy(test5_id_check, api.get_trans_id(), 16);
            assert(::memcmp(test5_check, "test5", 6) == 0);
            assert(::memcmp(test5_id_check, test5_id, 16) == 0);
            result = api.recv_async(test6_id, false);
            assert(result == CloudI::API::return_value::success);
            result = api.recv_async();
            assert(result == CloudI::API::return_value::success);
            char test6_check[6];
            assert(api.get_response_size() == 6);
            ::memcpy(test6_check, api.get_response(), 6);
            char test6_id_check[16];
            ::memcpy(test6_id_check, api.get_trans_id(), 16);
            assert(::memcmp(test6_check, "test6", 6) == 0);
            assert(::memcmp(test6_id_check, test6_id, 16) == 0);
            result = api.recv_async(test7_id, false);
            assert(result == CloudI::API::return_value::success);
            result = api.recv_async();
            assert(result == CloudI::API::return_value::success);
            char test7_check[6];
            assert(api.get_response_size() == 6);
            ::memcpy(test7_check, api.get_response(), 6);
            char test7_id_check[16];
            ::memcpy(test7_id_check, api.get_trans_id(), 16);
            assert(::memcmp(test7_check, "test7", 6) == 0);
            assert(::memcmp(test7_id_check, test7_id, 16) == 0);
            result = api.recv_async(test8_id, false);
            assert(result == CloudI::API::return_value::success);
            result = api.recv_async();
            assert(result == CloudI::API::return_value::success);
            char test8_check[6];
            assert(api.get_response_size() == 6);
            ::memcpy(test8_check, api.get_response(), 6);
            char test8_id_check[16];
            ::memcpy(test8_id_check, api.get_trans_id(), 16);
            assert(::memcmp(test8_check, "test8", 6) == 0);
            assert(::memcmp(test8_id_check, test8_id, 16) == 0);
            result = api.recv_async(test9_id, false);
            assert(result == CloudI::API::return_value::success);
            result = api.recv_async();
            assert(result == CloudI::API::return_value::success);
            char test9_check[6];
            assert(api.get_response_size() == 6);
            ::memcpy(test9_check, api.get_response(), 6);
            char test9_id_check[16];
            ::memcpy(test9_id_check, api.get_trans_id(), 16);
            assert(::memcmp(test9_check, "test9", 6) == 0);
            assert(::memcmp(test9_id_check, test9_id, 16) == 0);
            result = api.recv_async(test10_id, false);
            assert(result == CloudI::API::return_value::success);
            result = api.recv_async();
            assert(result == CloudI::API::return_value::success);
            char test10_check[7];
            assert(api.get_response_size() == 7);
            ::memcpy(test10_check, api.get_response(), 7);
            char test10_id_check[16];
            ::memcpy(test10_id_check, api.get_trans_id(), 16);
            assert(::memcmp(test10_check, "test10", 7) == 0);
            assert(::memcmp(test10_id_check, test10_id, 16) == 0);
            result = api.recv_async(test11_id, false);
            assert(result == CloudI::API::return_value::success);
            result = api.recv_async();
            assert(result == CloudI::API::return_value::success);
            char test11_check[7];
            assert(api.get_response_size() == 7);
            ::memcpy(test11_check, api.get_response(), 7);
            char test11_id_check[16];
            ::memcpy(test11_id_check, api.get_trans_id(), 16);
            assert(::memcmp(test11_check, "test11", 7) == 0);
            assert(::memcmp(test11_id_check, test11_id, 16) == 0);
            result = api.recv_async(test12_id, false);
            assert(result == CloudI::API::return_value::success);
            result = api.recv_async();
            assert(result == CloudI::API::return_value::success);
            char test12_check[7];
            assert(api.get_response_size() == 7);
            ::memcpy(test12_check, api.get_response(), 7);
            char test12_id_check[16];
            ::memcpy(test12_id_check, api.get_trans_id(), 16);
            assert(::memcmp(test12_check, "test12", 7) == 0);
            assert(::memcmp(test12_id_check, test12_id, 16) == 0);
            result = api.recv_async(test13_id, false);
            assert(result == CloudI::API::return_value::success);
            result = api.recv_async();
            assert(result == CloudI::API::return_value::success);
            char test13_check[7];
            assert(api.get_response_size() == 7);
            ::memcpy(test13_check, api.get_response(), 7);
            char test13_id_check[16];
            ::memcpy(test13_id_check, api.get_trans_id(), 16);
            assert(::memcmp(test13_check, "test13", 7) == 0);
            assert(::memcmp(test13_id_check, test13_id, 16) == 0);
            result = api.recv_async(test14_id, false);
            assert(result == CloudI::API::return_value::success);
            result = api.recv_async();
            assert(result == CloudI::API::return_value::success);
            char test14_check[7];
            assert(api.get_response_size() == 7);
            ::memcpy(test14_check, api.get_response(), 7);
            char test14_id_check[16];
            ::memcpy(test14_id_check, api.get_trans_id(), 16);
            assert(::memcmp(test14_check, "test14", 7) == 0);
            assert(::memcmp(test14_id_check, test14_id, 16) == 0);
            result = api.recv_async(test15_id, false);
            assert(result == CloudI::API::return_value::success);
            result = api.recv_async();
            assert(result == CloudI::API::return_value::success);
            char test15_check[7];
            assert(api.get_response_size() == 7);
            ::memcpy(test15_check, api.get_response(), 7);
            char test15_id_check[16];
            ::memcpy(test15_id_check, api.get_trans_id(), 16);
            assert(::memcmp(test15_check, "test15", 7) == 0);
            assert(::memcmp(test15_id_check, test15_id, 16) == 0);
            std::cout << "messaging sequence1 end c++" << std::endl;
            // start sequence2
            result = api.send_async(std::string(api.prefix()) + "sequence2",
                                    "start", 6);
            assert(result == CloudI::API::return_value::success);
            api.return_(command, name, pattern, "", 0, "end", 4,
                        timeout, trans_id, pid, pid_size);
        }

        void sequence2_e1(CloudI::API const & api,
                          int const command,
                          std::string const & name,
                          std::string const & pattern,
                          void const * const /*request_info*/,
                          uint32_t const /*request_info_size*/,
                          void const * const /*request*/,
                          uint32_t const /*request_size*/,
                          uint32_t timeout,
                          int8_t /*priority*/,
                          char const * const trans_id,
                          char const * const pid,
                          uint32_t const pid_size)
        {
            api.return_(command, name, pattern, "", 0, "1", 2,
                        timeout, trans_id, pid, pid_size);
        }

        void sequence2_e2(CloudI::API const & api,
                          int const command,
                          std::string const & name,
                          std::string const & pattern,
                          void const * const /*request_info*/,
                          uint32_t const /*request_info_size*/,
                          void const * const /*request*/,
                          uint32_t const /*request_size*/,
                          uint32_t timeout,
                          int8_t /*priority*/,
                          char const * const trans_id,
                          char const * const pid,
                          uint32_t const pid_size)
        {
            api.return_(command, name, pattern, "", 0, "2", 2,
                        timeout, trans_id, pid, pid_size);
        }

        void sequence2_e3(CloudI::API const & api,
                          int const command,
                          std::string const & name,
                          std::string const & pattern,
                          void const * const /*request_info*/,
                          uint32_t const /*request_info_size*/,
                          void const * const /*request*/,
                          uint32_t const /*request_size*/,
                          uint32_t timeout,
                          int8_t /*priority*/,
                          char const * const trans_id,
                          char const * const pid,
                          uint32_t const pid_size)
        {
            api.return_(command, name, pattern, "", 0, "3", 2,
                        timeout, trans_id, pid, pid_size);
        }

        void sequence2_e4(CloudI::API const & api,
                          int const command,
                          std::string const & name,
                          std::string const & pattern,
                          void const * const /*request_info*/,
                          uint32_t const /*request_info_size*/,
                          void const * const /*request*/,
                          uint32_t const /*request_size*/,
                          uint32_t timeout,
                          int8_t /*priority*/,
                          char const * const trans_id,
                          char const * const pid,
                          uint32_t const pid_size)
        {
            api.return_(command, name, pattern, "", 0, "4", 2,
                        timeout, trans_id, pid, pid_size);
        }

        void sequence2_e5(CloudI::API const & api,
                          int const command,
                          std::string const & name,
                          std::string const & pattern,
                          void const * const /*request_info*/,
                          uint32_t const /*request_info_size*/,
                          void const * const /*request*/,
                          uint32_t const /*request_size*/,
                          uint32_t timeout,
                          int8_t /*priority*/,
                          char const * const trans_id,
                          char const * const pid,
                          uint32_t const pid_size)
        {
            api.return_(command, name, pattern, "", 0, "5", 2,
                        timeout, trans_id, pid, pid_size);
        }

        void sequence2_e6(CloudI::API const & api,
                          int const command,
                          std::string const & name,
                          std::string const & pattern,
                          void const * const /*request_info*/,
                          uint32_t const /*request_info_size*/,
                          void const * const /*request*/,
                          uint32_t const /*request_size*/,
                          uint32_t timeout,
                          int8_t /*priority*/,
                          char const * const trans_id,
                          char const * const pid,
                          uint32_t const pid_size)
        {
            api.return_(command, name, pattern, "", 0, "6", 2,
                        timeout, trans_id, pid, pid_size);
        }

        void sequence2_e7(CloudI::API const & api,
                          int const command,
                          std::string const & name,
                          std::string const & pattern,
                          void const * const /*request_info*/,
                          uint32_t const /*request_info_size*/,
                          void const * const /*request*/,
                          uint32_t const /*request_size*/,
                          uint32_t timeout,
                          int8_t /*priority*/,
                          char const * const trans_id,
                          char const * const pid,
                          uint32_t const pid_size)
        {
            api.return_(command, name, pattern, "", 0, "7", 2,
                        timeout, trans_id, pid, pid_size);
        }

        void sequence2_e8(CloudI::API const & api,
                          int const command,
                          std::string const & name,
                          std::string const & pattern,
                          void const * const /*request_info*/,
                          uint32_t const /*request_info_size*/,
                          void const * const /*request*/,
                          uint32_t const /*request_size*/,
                          uint32_t timeout,
                          int8_t /*priority*/,
                          char const * const trans_id,
                          char const * const pid,
                          uint32_t const pid_size)
        {
            api.return_(command, name, pattern, "", 0, "8", 2,
                        timeout, trans_id, pid, pid_size);
        }

        void sequence2(CloudI::API const & api,
                       int const command,
                       std::string const & name,
                       std::string const & pattern,
                       void const * const /*request_info*/,
                       uint32_t const /*request_info_size*/,
                       void const * const request,
                       uint32_t const request_size,
                       uint32_t timeout,
                       int8_t /*priority*/,
                       char const * const trans_id,
                       char const * const pid,
                       uint32_t const pid_size)
        {
            int result;
            std::cout << "messaging sequence2 start c++" << std::endl;
            assert(request_size == 6);
            assert(::memcmp(request, "start", 6) == 0);
            // the sending process is excluded from the services that receive
            // the asynchronous message, so in this case, the receiving thread
            // will not be called, despite the fact it has subscribed to 'e',
            // to prevent a process (in this case thread) from deadlocking
            // with itself.
            result = api.mcast_async(std::string(api.prefix()) + "e", " ", 2);
            assert(result == CloudI::API::return_value::success);
            // 4 * 8 == 32, but only 3 out of 4 threads can receive messages,
            // since 1 thread is sending the mcast_async, so 3 * 8 == 24
            assert(api.get_trans_id_count() == 24);
            char e_str_check[25] = "                        ";
            char e_ids[24][16];
            ::memcpy(reinterpret_cast<char *>(e_ids),
                     api.get_trans_id(), 24 * 16);
            for (unsigned long i = 0; i < 24; ++i)
            {
                result = api.recv_async(e_ids[i]);
                assert(result == CloudI::API::return_value::success);
                assert(::memcmp(e_ids[i], api.get_trans_id(), 16) == 0);
                assert(api.get_response_size() == 2);
                char const integer = api.get_response()[0];
                int const index = (integer - '1') * 3;
                for (int offset = 0; offset < 3; ++offset)
                {
                    if (e_str_check[index + offset] == ' ')
                    {
                        e_str_check[index + offset] = integer;
                        break;
                    }
                }
            }
            assert(::memcmp(e_str_check,
                            "111222333444555666777888999", 24) == 0);
            std::cout << "messaging sequence2 end c++" << std::endl;
            // start sequence3
            result = api.send_async(std::string(api.prefix()) + "sequence3",
                                    "start", 6);
            assert(result == CloudI::API::return_value::success);
            api.return_(command, name, pattern, "", 0, "end", 4,
                        timeout, trans_id, pid, pid_size);
        }

        void sequence3_f1(CloudI::API const & api,
                          int const command,
                          std::string const & name,
                          std::string const & pattern,
                          void const * const /*request_info*/,
                          uint32_t const /*request_info_size*/,
                          void const * const request,
                          uint32_t const request_size,
                          uint32_t timeout,
                          int8_t priority,
                          char const * const trans_id,
                          char const * const pid,
                          uint32_t const pid_size)
        {
            assert(request_size == 2);
            long const request_i =
                ::strtol(reinterpret_cast<char const *>(request), 0, 10);
            assert(request_i >= 0 && request_i <= 4);
            if (request_i == 4)
            {
                api.return_(command, name, pattern, "", 0, "done", 5,
                            timeout, trans_id, pid, pid_size);
                assert(false);
                return;
            }
            std::stringstream request_new;
            request_new << (request_i + 2);
            std::string const & s = request_new.str();
            api.forward_(command, std::string(api.prefix()) + "f2",
                         "", 0, s.c_str(), s.size() + 1,
                         timeout, priority, trans_id, pid, pid_size);
        }

        void sequence3_f2(CloudI::API const & api,
                          int const command,
                          std::string const & /*name*/,
                          std::string const & /*pattern*/,
                          void const * const /*request_info*/,
                          uint32_t const /*request_info_size*/,
                          void const * const request,
                          uint32_t const request_size,
                          uint32_t timeout,
                          int8_t priority,
                          char const * const trans_id,
                          char const * const pid,
                          uint32_t const pid_size)
        {
            assert(request_size == 2);
            long const request_i =
                ::strtol(reinterpret_cast<char const *>(request), 0, 10);
            assert(request_i >= 2 && request_i <= 5);
            std::stringstream request_new;
            request_new << (request_i + 2);
            std::string const & s = request_new.str();
            api.forward_(command, std::string(api.prefix()) + "f1",
                         "", 0, s.c_str(), s.size() + 1,
                         timeout, priority, trans_id, pid, pid_size);
        }

        void sequence3_g1(CloudI::API const & api,
                          int const command,
                          std::string const & name,
                          std::string const & pattern,
                          void const * const /*request_info*/,
                          uint32_t const /*request_info_size*/,
                          void const * const request,
                          uint32_t const /*request_size*/,
                          uint32_t timeout,
                          int8_t /*priority*/,
                          char const * const trans_id,
                          char const * const pid,
                          uint32_t const pid_size)
        {
            std::string s(reinterpret_cast<char const *>(request));
            s += "suffix";
            api.return_(command, name, pattern, "", 0, s.c_str(), s.size() + 1,
                        timeout, trans_id, pid, pid_size);
        }

        void sequence3(CloudI::API const & api,
                       int const command,
                       std::string const & name,
                       std::string const & pattern,
                       void const * const /*request_info*/,
                       uint32_t const /*request_info_size*/,
                       void const * const request,
                       uint32_t const request_size,
                       uint32_t timeout,
                       int8_t /*priority*/,
                       char const * const trans_id,
                       char const * const pid,
                       uint32_t const pid_size)
        {
            int result;
            std::cout << "messaging sequence3 start c++" << std::endl;
            assert(request_size == 6);
            assert(::memcmp(request, "start", 6) == 0);
            result = api.send_async(std::string(api.prefix()) + "f1",
                                    "0", 2);
            assert(result == CloudI::API::return_value::success);
            char test1_id[16];
            ::memcpy(test1_id, api.get_trans_id(), 16);
            result = api.recv_async(test1_id);
            assert(result == CloudI::API::return_value::success);
            assert(::memcmp(test1_id, api.get_trans_id(), 16) == 0);
            assert(api.get_response_size() == 5);
            assert(::memcmp(api.get_response(), "done", 5) == 0);
            result = api.send_sync(std::string(api.prefix()) + "g1",
                                   "prefix_", 8);
            assert(result == CloudI::API::return_value::success);
            assert(api.get_response_size() == 14);
            assert(::memcmp(api.get_response(), "prefix_suffix", 14) == 0);
            std::cout << "messaging sequence3 end c++" << std::endl;
            // loop to find any infrequent problems, restart sequence1
            result = api.send_async(std::string(api.prefix()) +
                                    "sequence1", "start", 6);
            assert(result == CloudI::API::return_value::success);
            api.return_(command, name, pattern, "", 0, "end", 4,
                        timeout, trans_id, pid, pid_size);
        }

        bool m_stop_default;
        bool & m_stop;
        CloudI::API m_api;
        unsigned int const m_thread_index;

};

class Output
{
    public:
        Output() : m_got_output(false) {}

        void output(OutputData & data)
        {
            if (data.error() &&
                data.error() != CloudI::API::return_value::terminate)
            {
                std::cerr << "CloudI error " << data.error() << std::endl;
            }
            m_got_output = true;
        }

        bool got_output() const { return m_got_output; }

    private:
        bool m_got_output;
};

int main(int, char **)
{
    unsigned int const thread_count = CloudI::API::thread_count();

    Output outputObject;
    ThreadPool<Input, ThreadData, Output, OutputData>
        threadPool(thread_count, thread_count, outputObject);

    uint32_t timeout_terminate = 0;
    for (unsigned int i = 0; i < thread_count; ++i)
    {
        Input inputObject(i);
        if (timeout_terminate == 0)
        {
            timeout_terminate = inputObject.timeout_terminate();
            assert(timeout_terminate >= 1000);
        }
        bool const result = threadPool.input(inputObject);
        assert(result);
    }

    while (outputObject.got_output() == false)
        ::sleep(1);
    std::cout << "terminate messaging c++" << std::endl;
    threadPool.exit(timeout_terminate - 100);
    return 0;
}

