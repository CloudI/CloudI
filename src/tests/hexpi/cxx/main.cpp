//-*-Mode:C++;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=cpp fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
//
// MIT License
//
// Copyright (c) 2011-2022 Michael Truog <mjtruog at protonmail dot com>
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
#include <boost/thread/mutex.hpp>
#include <boost/thread/locks.hpp>
#include <boost/thread/condition_variable.hpp>
#include <sstream>
#include <iostream>
#include <string>
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

        int error() const
        {
            int error_value = 0;
            if (m_error && m_error != CloudI::API::return_value::terminate)
                error_value = m_error;
            return error_value;
        }

    private:
        CloudI::API::return_value_type const m_error;
};

class task : public thread_pool_input<thread_data, task_output_data>
{
    public:
        task(unsigned int const thread_index) :
            m_stop_default(false),
            m_stop(m_stop_default),
            m_api(thread_index, true)
        {
            task::m_timeout_terminate = m_api.timeout_terminate();
        }

        virtual ~task() throw() {}

        static uint32_t timeout_terminate()
        {
            return task::m_timeout_terminate;
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
                   char const * const source,
                   uint32_t const source_size)
        {
            uint32_t const * parameters =
                reinterpret_cast<uint32_t const *>(request);
            assert(request_size > (sizeof(uint32_t) * 2));
            uint32_t const & iteration_count = parameters[0];
            uint32_t const & digit_step = parameters[1];
            char const * digit_index =
                reinterpret_cast<char const *>(&parameters[2]);

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
                        timeout, trans_id, source, source_size);
            std::cout << "execution never gets here" << std::endl;
        }

        task_output_data process(bool const & stop, thread_data & /*data*/)
        {
            // a return from this task object function, provides
            // a single task_output_data object (with the error value) to the
            // single shared task_output object, which will then cause the
            // thread pool to use the exit() function
            // (m_stop is set to true in all threads,
            //  so all threads are asked to abort their processing)
            m_stop = stop;
            // it is best to do CloudI API subscribe calls here due to
            // ownership of *this in the thread_pool
            // when the process function is executed
            // (makes it easier to use the thread_data variable,
            //  if necessary,  because *this isn't getting copied)
            int error = m_api.subscribe_count("hexpi");
            if (! error)
            {
                assert(m_api.get_subscribe_count() == 0);
                error = m_api.subscribe("hexpi", *this, &task::hexpi);
            }
            if (! error)
            {
                error = m_api.subscribe_count("hexpi");
            }
            if (! error)
            {
                assert(m_api.get_subscribe_count() == 1);
                while (CloudI::API::return_value::timeout ==
                       (error = m_api.poll(1000)))
                {
                    if (stop)
                    {
                        error = CloudI::API::return_value::success;
                        break;
                    }
                }
            }
            return task_output_data(error);
        }

    private:
        bool m_stop_default;
        bool & m_stop;

        CloudI::API m_api;
        static uint32_t m_timeout_terminate;
};
uint32_t task::m_timeout_terminate = 0;

class task_output : public thread_pool_output<task_output_data>
{
    public:
        task_output() :
            m_status(0),
            m_terminate(false)
        {
        }

        virtual ~task_output() throw() {}

        void output(task_output_data & data)
        {
            bool terminated;
            {
                boost::lock_guard<boost::mutex> lock(m_terminate_mutex);
                int error = data.error();
                if (error)
                {
                    std::cerr << "C/C++ CloudI API error " <<
                        error << std::endl;
                    m_status = error;
                }
                terminated = m_terminate;
                m_terminate = true;
            }
            if (terminated == false)
                m_terminate_conditional.notify_all();
        }

        int wait_on_terminate()
        {   
            if (m_terminate == false)
            {
                boost::unique_lock<boost::mutex> lock(m_terminate_mutex);
                m_terminate_conditional.wait(lock);
            }
            return m_status;
        }

    private:
        int m_status;
        bool m_terminate;
        boost::mutex m_terminate_mutex;
        boost::condition_variable m_terminate_conditional;
};

int main(int, char **)
{
    int status = 0;

    assert_initialize();
    try
    {
        unsigned int const thread_count = CloudI::API::thread_count();

        task_output output_object;
        thread_pool<task, thread_data, task_output, task_output_data>
            thread_pool(thread_count, thread_count, output_object);

        for (unsigned int i = 0; i < thread_count; ++i)
        {
            task task_input(i);
            bool const added = thread_pool.input(task_input);
            assert(added);
        }

        assert(task::timeout_terminate() >= 1000);
        status = output_object.wait_on_terminate();
        thread_pool.exit(task::timeout_terminate() - 100);
    }
    catch (CloudI::API::invalid_input_exception const & e)
    {
        std::cerr << e.what() << std::endl;
    }
    catch (CloudI::API::terminate_exception const &)
    {
    }

    std::cout << "terminate hexpi c++" << std::endl;

    return status;
}

