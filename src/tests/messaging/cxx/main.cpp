/* -*-Mode:C++;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
 * ex: set ft=cpp fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
 *
 * MIT License
 *
 * Copyright (c) 2012-2020 Michael Truog <mjtruog at protonmail dot com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */
#include "cloudi.hpp"
#include "thread_pool.hpp"
#include <boost/thread/mutex.hpp>
#include <boost/thread/locks.hpp>
#include <boost/thread/condition_variable.hpp>
#include <iostream>
#include <cstring>
#include <sstream>
#include <limits>
#include "assert.hpp"

class thread_data : public thread_pool_thread_data
{
    public:
        virtual ~thread_data() throw() {}
};

class task_output_data : public thread_pool_output_data
{
    public:
        task_output_data(CloudI::API::return_value_type const error) :
            m_error(error)
        {
        }

        virtual ~task_output_data() throw() {}

        void output_error() const
        {
            if (m_error && m_error != CloudI::API::return_value::terminate)
                std::cerr << "C/C++ CloudI API error " << m_error << std::endl;
        }

    private:
        CloudI::API::return_value_type const m_error;
};

class task : public thread_pool_input<thread_data, task_output_data>
{
    public:
        task(unsigned int const thread_index,
             uint32_t & timeout_terminate) :
            m_stop_default(false),
            m_stop(m_stop_default),
            m_api(thread_index, false),
            m_thread_index(thread_index)
        {
            timeout_terminate = m_api.timeout_terminate();
        }

        virtual ~task() throw() {}

        task_output_data process(bool const & stop, thread_data & /*data*/)
        {
            int result;
            result = m_api.subscribe("a/b/c/d", *this,
                                     &task::sequence1_abcd);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("a/b/c/*", *this,
                                     &task::sequence1_abc_);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("a/b/*/d", *this,
                                     &task::sequence1_ab_d);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("a/*/c/d", *this,
                                     &task::sequence1_a_cd);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("*/b/c/d", *this,
                                     &task::sequence1__bcd);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("a/b/*", *this,
                                     &task::sequence1_ab__);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("a/*/d", *this,
                                     &task::sequence1_a__d);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("*/c/d", *this,
                                     &task::sequence1___cd);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("a/*", *this,
                                     &task::sequence1_a___);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("*/d", *this,
                                     &task::sequence1____d);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("*", *this,
                                     &task::sequence1_____);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("sequence1", *this,
                                     &task::sequence1);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("e", *this,
                                     &task::sequence2_e1);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("e", *this,
                                     &task::sequence2_e2);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("e", *this,
                                     &task::sequence2_e3);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("e", *this,
                                     &task::sequence2_e4);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("e", *this,
                                     &task::sequence2_e5);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("e", *this,
                                     &task::sequence2_e6);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("e", *this,
                                     &task::sequence2_e7);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("e", *this,
                                     &task::sequence2_e8);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("sequence2", *this,
                                     &task::sequence2);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("f1", *this,
                                     &task::sequence3_f1);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("f2", *this,
                                     &task::sequence3_f2);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("g1", *this,
                                     &task::sequence3_g1);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe("sequence3", *this,
                                     &task::sequence3);
            assert(result == CloudI::API::return_value::success);
            if (m_thread_index == 0)
            {
                result = m_api.send_async(std::string(m_api.prefix()) +
                                          "sequence1", "1", 2);
                assert(result == CloudI::API::return_value::success);
            }

            m_stop = stop;
            while (CloudI::API::return_value::timeout ==
                   (result = m_api.poll(1000)))
            {
                if (stop)
                {
                    result = CloudI::API::return_value::success;
                    break;
                }
            }
            return task_output_data(result);
        }

    private:
        void sequence1_abcd(CloudI::API const & api,
                            int const request_type,
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
            api.return_(request_type, name, pattern,
                        "", 0, request, request_size,
                        timeout, trans_id, pid, pid_size);
        }

        void sequence1_abc_(CloudI::API const & api,
                            int const request_type,
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
            api.return_(request_type, name, pattern,
                        "", 0, request, request_size,
                        timeout, trans_id, pid, pid_size);
        }

        void sequence1_ab_d(CloudI::API const & api,
                            int const request_type,
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
            api.return_(request_type, name, pattern,
                        "", 0, request, request_size,
                        timeout, trans_id, pid, pid_size);
        }

        void sequence1_a_cd(CloudI::API const & api,
                            int const request_type,
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
            api.return_(request_type, name, pattern,
                        "", 0, request, request_size,
                        timeout, trans_id, pid, pid_size);
        }

        void sequence1__bcd(CloudI::API const & api,
                            int const request_type,
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
            api.return_(request_type, name, pattern,
                        "", 0, request, request_size,
                        timeout, trans_id, pid, pid_size);
        }

        void sequence1_ab__(CloudI::API const & api,
                            int const request_type,
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
            api.return_(request_type, name, pattern,
                        "", 0, request, request_size,
                        timeout, trans_id, pid, pid_size);
        }

        void sequence1_a__d(CloudI::API const & api,
                            int const request_type,
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
            api.return_(request_type, name, pattern,
                        "", 0, request, request_size,
                        timeout, trans_id, pid, pid_size);
        }

        void sequence1___cd(CloudI::API const & api,
                            int const request_type,
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
            api.return_(request_type, name, pattern,
                        "", 0, request, request_size,
                        timeout, trans_id, pid, pid_size);
        }

        void sequence1_a___(CloudI::API const & api,
                            int const request_type,
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
            api.return_(request_type, name, pattern,
                        "", 0, request, request_size,
                        timeout, trans_id, pid, pid_size);
        }

        void sequence1____d(CloudI::API const & api,
                            int const request_type,
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
            api.return_(request_type, name, pattern,
                        "", 0, request, request_size,
                        timeout, trans_id, pid, pid_size);
        }

        void sequence1_____(CloudI::API const & api,
                            int const request_type,
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
            api.return_(request_type, name, pattern,
                        "", 0, request, request_size,
                        timeout, trans_id, pid, pid_size);
        }

        void sequence1(CloudI::API const & api,
                       int const request_type,
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
            std::cout << "messaging sequence1 start c++ (" <<
                std::string(reinterpret_cast<char const *>(request),
                            request_size - 1) << ")" << std::endl;

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

            // n.b., depends on cloudi_core_i_constants.hrl having
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
            std::cout << "messaging sequence1 end c++ (" <<
                std::string(reinterpret_cast<char const *>(request),
                            request_size - 1) << ")" << std::endl;
            // start sequence2
            result = api.send_async(std::string(api.prefix()) + "sequence2",
                                    request, request_size);
            assert(result == CloudI::API::return_value::success);
            api.return_(request_type, name, pattern, "", 0, "end", 4,
                        timeout, trans_id, pid, pid_size);
        }

        void sequence2_e1(CloudI::API const & api,
                          int const request_type,
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
            api.return_(request_type, name, pattern, "", 0, "1", 2,
                        timeout, trans_id, pid, pid_size);
        }

        void sequence2_e2(CloudI::API const & api,
                          int const request_type,
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
            api.return_(request_type, name, pattern, "", 0, "2", 2,
                        timeout, trans_id, pid, pid_size);
        }

        void sequence2_e3(CloudI::API const & api,
                          int const request_type,
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
            api.return_(request_type, name, pattern, "", 0, "3", 2,
                        timeout, trans_id, pid, pid_size);
        }

        void sequence2_e4(CloudI::API const & api,
                          int const request_type,
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
            api.return_(request_type, name, pattern, "", 0, "4", 2,
                        timeout, trans_id, pid, pid_size);
        }

        void sequence2_e5(CloudI::API const & api,
                          int const request_type,
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
            api.return_(request_type, name, pattern, "", 0, "5", 2,
                        timeout, trans_id, pid, pid_size);
        }

        void sequence2_e6(CloudI::API const & api,
                          int const request_type,
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
            api.return_(request_type, name, pattern, "", 0, "6", 2,
                        timeout, trans_id, pid, pid_size);
        }

        void sequence2_e7(CloudI::API const & api,
                          int const request_type,
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
            api.return_(request_type, name, pattern, "", 0, "7", 2,
                        timeout, trans_id, pid, pid_size);
        }

        void sequence2_e8(CloudI::API const & api,
                          int const request_type,
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
            api.return_(request_type, name, pattern, "", 0, "8", 2,
                        timeout, trans_id, pid, pid_size);
        }

        void sequence2(CloudI::API const & api,
                       int const request_type,
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
            std::cout << "messaging sequence2 start c++ (" <<
                std::string(reinterpret_cast<char const *>(request),
                            request_size - 1) << ")" << std::endl;
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
            std::cout << "messaging sequence2 end c++ (" <<
                std::string(reinterpret_cast<char const *>(request),
                            request_size - 1) << ")" << std::endl;
            // start sequence3
            result = api.send_async(std::string(api.prefix()) + "sequence3",
                                    request, request_size);
            assert(result == CloudI::API::return_value::success);
            api.return_(request_type, name, pattern, "", 0, "end", 4,
                        timeout, trans_id, pid, pid_size);
        }

        void sequence3_f1(CloudI::API const & api,
                          int const request_type,
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
                api.return_(request_type, name, pattern, "", 0, "done", 5,
                            timeout, trans_id, pid, pid_size);
                assert(false);
                return;
            }
            std::stringstream request_new;
            request_new << (request_i + 2);
            std::string const & s = request_new.str();
            api.forward_(request_type, std::string(api.prefix()) + "f2",
                         "", 0, s.c_str(), s.size() + 1,
                         timeout, priority, trans_id, pid, pid_size);
        }

        void sequence3_f2(CloudI::API const & api,
                          int const request_type,
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
            api.forward_(request_type, std::string(api.prefix()) + "f1",
                         "", 0, s.c_str(), s.size() + 1,
                         timeout, priority, trans_id, pid, pid_size);
        }

        void sequence3_g1(CloudI::API const & api,
                          int const request_type,
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
            api.return_(request_type, name, pattern,
                        "", 0, s.c_str(), s.size() + 1,
                        timeout, trans_id, pid, pid_size);
        }

        void sequence3(CloudI::API const & api,
                       int const request_type,
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
            std::cout << "messaging sequence3 start c++ (" <<
                std::string(reinterpret_cast<char const *>(request),
                            request_size - 1) << ")" << std::endl;
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
            std::cout << "messaging sequence3 end c++ (" <<
                std::string(reinterpret_cast<char const *>(request),
                            request_size - 1) << ")" << std::endl;
            // loop to find any infrequent problems, restart sequence1
            long iteration =
                ::strtol(reinterpret_cast<char const *>(request), 0, 10) + 1;
            if (iteration == std::numeric_limits<long>::max())
            {
                iteration = 0;
            }
            std::stringstream request_new;
            request_new << iteration;
            std::string const & s = request_new.str();
            result = api.send_async(std::string(api.prefix()) +
                                    "sequence1", s.c_str(), s.size() + 1);
            assert(result == CloudI::API::return_value::success);
            api.return_(request_type, name, pattern, "", 0, "end", 4,
                        timeout, trans_id, pid, pid_size);
        }

        bool m_stop_default;
        bool & m_stop;
        CloudI::API m_api;
        unsigned int const m_thread_index;

};

class task_output : public thread_pool_output<task_output_data>
{
    public:
        task_output() :
            m_terminate(false)
        {
        }

        virtual ~task_output() throw() {}

        void output(task_output_data & data)
        {
            bool terminated;
            {
                boost::lock_guard<boost::mutex> lock(m_terminate_mutex);
                data.output_error();
                terminated = m_terminate;
                m_terminate = true;
            }
            if (terminated == false)
                m_terminate_conditional.notify_all();
        }

        void wait_on_terminate()
        {
            if (m_terminate == false)
            {
                boost::unique_lock<boost::mutex> lock(m_terminate_mutex);
                m_terminate_conditional.wait(lock);
            }
        }

    private:
        bool m_terminate;
        boost::mutex m_terminate_mutex;
        boost::condition_variable m_terminate_conditional;
};

int main(int, char **)
{
    try
    {
        unsigned int const thread_count = CloudI::API::thread_count();

        task_output output_object;
        thread_pool<task, thread_data, task_output, task_output_data>
            thread_pool(thread_count, thread_count, output_object);

        uint32_t timeout_terminate = 0;
        for (unsigned int i = 0; i < thread_count; ++i)
        {
            task task_input(i, timeout_terminate);
            bool const result = thread_pool.input(task_input);
            assert(result);
        }

        assert(timeout_terminate >= 1000);
        output_object.wait_on_terminate();
        thread_pool.exit(timeout_terminate - 100);
    }
    catch (CloudI::API::invalid_input_exception const & e)
    {
        std::cerr << e.what() << std::endl;
    }
    catch (CloudI::API::terminate_exception const &)
    {
    }

    std::cout << "terminate messaging c++" << std::endl;

    return 0;
}

