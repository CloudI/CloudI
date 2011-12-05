// -*- coding: utf-8; Mode: C++; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
// ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
//
// BSD LICENSE
// 
// Copyright (c) 2011, Michael Truog <mjtruog at gmail dot com>
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
#include "cloudi.h"
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
        Input(int const thread_index)
        {
            int result = cloudi_initialize(&m_api, thread_index);
            assert(result == cloudi_success);
            
            result = cloudi_subscribe(&m_api, "hexpi", &Input::hexpi);
            assert(result == cloudi_success);
        }

        ~Input()
        {
            cloudi_destroy(&m_api);
        }

        Input(Input const & object)
        {
            m_api = object.m_api;
            ::memset(const_cast<cloudi_instance_t *>(&object.m_api), 0,
                     sizeof(cloudi_instance_t));
        }

        static void hexpi(cloudi_instance_t * api,
                          int const command,
                          char const * const name,
                          void const * const request_info,
                          uint32_t const request_info_size,
                          void const * const request,
                          uint32_t const request_size,
                          uint32_t timeout,
                          int8_t priority,
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
                return;

            // insert the elapsed time as a 32 bit float
            pi_result.insert(0, "    ");
            float * elapsed_hours = reinterpret_cast<float *>(
                const_cast<char *>(pi_result.c_str()));
            *elapsed_hours = static_cast<float>(t.elapsed() / 3600.0);

            cloudi_return(api, command, name, "", 0,
                          pi_result.c_str(), pi_result.size(),
                          timeout, trans_id, pid, pid_size);
            std::cout << "execution never gets here" << std::endl;
        }

        OutputData process(bool const & stop, ThreadData & /*data*/)
        {
            OutputData resultObject;
            int value;
            while (cloudi_timeout == (value = cloudi_poll(&m_api, 1000)))
            {
                if (stop)
                    return resultObject.setError(cloudi_success);
            }
            return resultObject.setError(value);
        }

        static void setStop(bool const & stop)
        {
            // store stop for the hexpi static function callback
            // (keep the variable external to the function pointer,
            //  and keep the type as 'bool const &' to prevent modification)
            const_cast<bool &>(m_stop) = stop;
        }

    private:
        static bool m_stop_default;
        static bool const & m_stop;
        cloudi_instance_t m_api;

};

bool Input::m_stop_default = false;
bool const & Input::m_stop = Input::m_stop_default;

class Output
{
    public:
        Output() : m_got_output(false) {}

        void output(OutputData & data)
        {
            if (data.error())
                std::cerr << "CloudI error " << data.error() << std::endl;
            m_got_output = true;
        }

        bool got_output() const { return m_got_output; }

    private:
        bool m_got_output;
};

int main(int, char **)
{
    int thread_count;
    int result = cloudi_initialize_thread_count(&thread_count);
    assert(result == cloudi_success);

    Output outputObject;
    ThreadPool<Input, ThreadData, Output, OutputData>
        threadPool(thread_count, thread_count, outputObject);

    Input::setStop(threadPool.stop());

    for (int i = 0; i < thread_count; ++i)
    {
        Input inputObject(i);
        threadPool.input(inputObject);
    }

    while (outputObject.got_output() == false)
        ::sleep(1);
    threadPool.exit(3000);
    return 0;
}

