//-*-Mode:C++;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=cpp fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
//
// MIT License
//
// Copyright (c) 2011-2017 Michael Truog <mjtruog at gmail dot com>
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
#include "cloudi.hpp"
#include "timer.hpp"
#include "thread_pool.hpp"
#include "piqpr8_gmp.hpp"
#include "piqpr8_gmp_verify.hpp"
#include <unistd.h>
#include <sstream>
#include <iostream>
#include <string>
#include <cstring>
#include "assert.hpp"

class ThreadData
{
};

class OutputData
{
    public:
        OutputData() : m_error(-1) {}
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
            m_api(thread_index)
        {
            int result = 0;
            result = m_api.subscribe_count("hexpi");
            assert(result == CloudI::API::return_value::success);
            assert(m_api.get_subscribe_count() == 0);
            result = m_api.subscribe("hexpi", *this, &Input::hexpi);
            assert(result == CloudI::API::return_value::success);
            result = m_api.subscribe_count("hexpi");
            assert(result == CloudI::API::return_value::success);
            assert(m_api.get_subscribe_count() == 1);
        }

        uint32_t timeout_terminate() const
        {
            return m_api.timeout_terminate();
        }

        void hexpi(CloudI::API const & api,
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
            uint32_t const * const parameters = 
                reinterpret_cast<uint32_t const * const>(request);
            assert(request_size > (sizeof(uint32_t) * 2));
            uint32_t const & iteration_count = parameters[0];
            uint32_t const & digit_step = parameters[1];
            char const * digit_index =
                reinterpret_cast<char const * const>(&parameters[2]);

            // perform the work
            timer t;
            std::ostringstream result;
            for (uint32_t i = 0; i < iteration_count; ++i)
            {
                if (m_stop)
                {
                    return;
                }
                else
                {
                    std::string pi_sequence;
                    if (! bbp_pi(m_stop, digit_index,
                                 digit_step * i, pi_sequence))
                        return;
                    result << pi_sequence;
                }
            }
            std::string pi_result = result.str();
            if (! bbp_pi_verify(digit_index, pi_result))
            {
                std::cerr << "index: " << digit_index << ", \"" <<
                    pi_result << "\" failed validation" << std::endl;
                return;
            }

            // insert the elapsed time as a 32 bit float
            pi_result.insert(0, "    ");
            float * elapsed_hours = reinterpret_cast<float *>(
                const_cast<char *>(pi_result.c_str()));
            *elapsed_hours = static_cast<float>(t.elapsed() / 3600.0);

            api.return_(request_type, name, pattern, "", 0,
                        pi_result.c_str(), pi_result.size(),
                        timeout, trans_id, pid, pid_size);
            std::cout << "execution never gets here" << std::endl;
        }

        OutputData process(bool const & stop, ThreadData & /*data*/)
        {
            OutputData resultObject;
            int value;
            m_stop = stop;
            // a return from this Input object function, provides
            // a single OutputData object (with the error value) to the
            // single shared Output object, which will then cause the
            // thread pool to use the exit() function
            // (m_stop is set to true in all threads,
            //  so all threads are asked to abort their processing)
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
        bool m_stop_default;
        bool & m_stop;

        CloudI::API m_api;

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
    std::cout << "terminate hexpi c++" << std::endl;
    threadPool.exit(timeout_terminate - 100);
    return 0;
}

