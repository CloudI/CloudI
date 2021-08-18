//-*-Mode:C++;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=cpp fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
//
// MIT License
//
// Copyright (c) 2009-2020 Michael Truog <mjtruog at protonmail dot com>
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
#ifndef THREAD_POOL_HPP
#define THREAD_POOL_HPP

#include "copy_ptr.hpp"
#include "timer.hpp"
#include <boost/thread/thread.hpp>
#include <boost/thread/mutex.hpp>
#include <boost/thread/locks.hpp>
#include <boost/thread/condition_variable.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <algorithm>
#include <vector>
#include "assert.hpp"

/// abstract base class for thread_pool THREAD_DATA
class thread_pool_thread_data
{
    public:
        virtual ~thread_pool_thread_data() throw() {}
};

/// abstract base class for thread_pool OUTPUT_DATA
class thread_pool_output_data
{
    public:
        virtual ~thread_pool_output_data() throw() {}
};

/// abstract base class for thread_pool INPUT
template <typename THREAD_DATA, typename OUTPUT_DATA>
class thread_pool_input
{
    public:
        virtual ~thread_pool_input() throw() {}

        virtual OUTPUT_DATA process(bool const & stop, THREAD_DATA & data) = 0;
};

/// abstract base class for thread_pool OUTPUT
template <typename OUTPUT_DATA>
class thread_pool_output
{
    public:
        virtual ~thread_pool_output() throw() {}

        virtual void output(OUTPUT_DATA & data) = 0;
};

/// thread_pool object
/// All methods are meant to be used by a single thread
/// except for the output() method (which is meant to be used by other threads).
template <typename INPUT, typename THREAD_DATA,
          typename OUTPUT, typename OUTPUT_DATA>
class thread_pool
{
    private:
        class thread_function_object
        {
            public:
                thread_function_object(INPUT & task, thread_pool & pool) :
                    m_task(task),
                    m_thread_pool(pool)
                {
                }
        
                void operator () (THREAD_DATA & data)
                {
                    bool const & stop = m_thread_pool.stop();
                    OUTPUT_DATA result = m_task.process(stop, data);
                    if (stop)
                        return;
                    m_thread_pool.output(result);
                }

            private:
                INPUT m_task;
                thread_pool & m_thread_pool;
        };
        friend class thread_function_object;

        /// Active Object design pattern
        /// with round-robin scheduling in the thread_pool
        /// and a method (thread_pool::output()) callback for the result
        class thread_object_data
        {
            private:
                typedef std::list<thread_function_object> queue_type;

            public:
                thread_object_data() :
                    m_running(false),
                    m_running_lock(m_running_mutex),
                    m_exited(true)
                {
                }

                thread_object_data(thread_object_data const & object) :
                    m_data(object.m_data),
                    m_running(object.m_running),
                    m_running_lock(m_running_mutex),
                    m_exited(object.m_exited)
                {
                    // can not be called after the thread is started
                    assert(object.m_running == false);
                }

                /// wait for the thread to start
                void wait_on_start()
                {
                    boost::lock_guard<boost::mutex> lock(m_running_mutex);
                }

                /// make the executing thread exit this function object
                void exit()
                {
                    if (m_running == false)
                        return;
                    {
                        boost::lock_guard<boost::mutex> lock(m_queue_mutex);
                        m_running = false;
                    }
                    m_queue_conditional.notify_one();
                }

                /// wait for the thread to exit, if it has not exited already
                void wait_on_exit(boost::posix_time::time_duration & t)
                {
                    timer exit_timer;
                    {
                        boost::unique_lock<boost::mutex> lock(m_queue_mutex);
                        if (m_exited == false)
                            m_destruction_conditional.timed_wait(lock, t);
                    }

                    boost::posix_time::milliseconds elapsed(
                        static_cast<long>(ceil(exit_timer.elapsed() * 1000.0)));
                    if (elapsed < t)
                        t = t - elapsed;
                    else
                        t = boost::posix_time::milliseconds(0);
                }
        
                /// add a function for the thread to execute to the queue
                bool push_back(thread_function_object const & input)
                {
                    if (m_running == false)
                        return false;
                    bool was_empty;
                    {
                        boost::lock_guard<boost::mutex> lock(m_queue_mutex);
                        was_empty = m_queue.empty();
                        m_queue.push_back(input);
                    }
                    if (was_empty)
                        m_queue_conditional.notify_one();
                    return true;
                }
        
                /// the thread's execution function
                void operator () ()
                {
                    m_running = true;
                    m_exited = false;
                    m_running_lock.unlock();
                    boost::unique_lock<boost::mutex> lock(m_queue_mutex);
                    while (m_running && m_queue.empty())
                        m_queue_conditional.wait(lock);
                    while (m_running)
                    {
                        thread_function_object & function = m_queue.front();
                        lock.unlock();
        
                        function(m_data);
        
                        lock.lock();
                        m_queue.pop_front();
                        while (m_running && m_queue.empty())
                            m_queue_conditional.wait(lock);
                    }
                    m_exited = true;
                    m_destruction_conditional.notify_one();
                }

            private:
                THREAD_DATA m_data;
                bool m_running;  /// thread is running
                boost::mutex m_running_mutex;
                boost::unique_lock<boost::mutex> m_running_lock;
                bool m_exited;   /// thread was terminated
                queue_type m_queue;
                boost::mutex m_queue_mutex;
                boost::condition_variable m_queue_conditional;
                boost::condition_variable m_destruction_conditional;

        };

        /// boost::thread parameter container that handles encapsulation of the
        /// thread's data so that object copy semantics do not interfere
        /// with functionality.
        class thread_object
        {
            public:
                thread_object(thread_object_data & data) : m_data(data) {}
                void operator () () { m_data(); }
            private:
                thread_object_data & m_data;
        };

    public:
        thread_pool(size_t initial_size, size_t max_size,
                    OUTPUT & output_object) :
            m_threads(max_size),
            m_objects(max_size),
            m_current_thread(0),
            m_total_configured(0),
            m_total_active(0),
            m_stop(false),
            m_output_object(output_object)
        {
            configure(initial_size);
        }

        ~thread_pool() throw()
        {
            thread_pool::exit(0);
        }

        /// cause all threads to exit and unconfigure the thread pool
        void exit(size_t const timeout)
        {
            m_stop = true;

            for (size_t i = 0; i < m_total_configured; ++i)
                m_objects[i].exit();

            if (timeout > 0)
            {
                boost::posix_time::milliseconds const zero(0);
                boost::posix_time::milliseconds t(timeout);
                for (size_t i = 0; i < m_total_configured; ++i)
                {
                    m_objects[i].wait_on_exit(t);
                    if (t == zero)
                        break;
                }
            }
            m_current_thread = 0;
            m_total_configured = 0;
            m_total_active = 0;
        }

        /// put a function into the thread pool for execution
        bool input(INPUT & task)
        {
            {
                boost::lock_guard<boost::mutex> lock(m_task_input_mutex);
                if (m_objects[m_current_thread].push_back(
                        thread_function_object(task, *this)) == false)
                    return false;
            }
            if (++m_current_thread == m_total_active)
                m_current_thread = 0;
            return true;
        }

        /// make the thread pool grow by an increment
        void grow(size_t increment)
        {
            configure(m_total_active + increment);
        }

        /// make the thread pool shrink by a decrement
        void shrink(size_t decrement)
        {
            assert(decrement <= m_total_active);
            configure(m_total_active - decrement);
        }

        /// return the current count of active threads
        size_t count() const
        {
            return m_total_active;
        }

        /// stop boolean reference for checking if an exit should occur
        /// (should be stored as a 'bool const &' to prevent
        //   external modifications)
        bool const & stop() const
        {
            return m_stop;
        }

    private:
        typedef thread_pool<INPUT, THREAD_DATA, OUTPUT, OUTPUT_DATA>
            thread_pool_type;
        thread_pool(thread_pool_type const &);
        thread_pool_type & operator=(thread_pool_type const &);

        /// verify that there is a sufficient number of threads
        /// configured and prespawned
        void configure(size_t count)
        {
            if (count > m_threads.size())
            {
                count = m_threads.size();
                std::cerr << count << " max threads configured" << std::endl;
            }

            // configure more threads if necessary
            if (count > m_total_configured)
            {
                for (size_t i = m_total_configured; i < count; ++i)
                    m_threads[i].reset(
                        new boost::thread(thread_object(m_objects[i])));
                for (size_t i = m_total_configured; i < count; ++i)
                    m_objects[i].wait_on_start();
                m_total_configured = count;
            }
            
            // make sure only active threads will be referenced in the future
            if (count < m_total_active)
            {
                if (m_current_thread >= count)
                    m_current_thread = 0;
            }
            m_total_active = count;
        }

        /// get a result object from a thread that is almost done executing
        void output(OUTPUT_DATA & result)
        {
            boost::mutex::scoped_try_lock lock(m_task_output_mutex);
            while (lock.owns_lock() == false)
            {
                if (m_stop)
                    return;
                boost::this_thread::yield();
                lock.try_lock();
            }
            if (m_stop)
                return;
            m_output_object.output(result);
        }

        std::vector< copy_ptr<boost::thread> > m_threads;
        std::vector<thread_object_data> m_objects;
        size_t m_current_thread;
        size_t m_total_configured;
        size_t m_total_active;
        boost::mutex m_task_input_mutex;
        boost::mutex m_task_output_mutex;
        bool m_stop;
        OUTPUT & m_output_object;
};

#endif // THREAD_POOL_HPP

