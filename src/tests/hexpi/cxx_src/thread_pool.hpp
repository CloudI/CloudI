// -*- coding: utf-8; Mode: C++; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
// ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
//
// BSD LICENSE
// 
// Copyright (c) 2009-2011, Michael Truog <mjtruog at gmail dot com>
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

/// ThreadPool object
/// All methods are meant to be used by a single thread
/// except for the output() method (which is meant to be used by other threads).
template <typename INPUT, typename THREAD_DATA,
          typename OUTPUT, typename OUTPUT_DATA>
class ThreadPool
{
    private:
        class ThreadFunctionObject
        {
            public:
                ThreadFunctionObject(INPUT & task, ThreadPool & threadPool) :
                        m_task(task),
                        m_threadPool(threadPool)
                {
                }
        
                void operator () (THREAD_DATA & data)
                {
                    bool const & stop = m_threadPool.stop();
                    OUTPUT_DATA result = m_task.process(stop, data);
                    if (stop)
                        return;
                    m_threadPool.output(result);
                }

            private:
                INPUT m_task;
                ThreadPool & m_threadPool;
        };
        friend class ThreadFunctionObject;

        /// Active Object design pattern
        /// with round-robin scheduling in the ThreadPool
        /// and a method (ThreadPool::output()) callback for the result
        class ThreadObjectData
        {
            private:
                typedef std::list<ThreadFunctionObject> QueueType;

            public:
                ThreadObjectData() :
                    m_running(false),
                    m_exited(true)
                {
                }

                ThreadObjectData(ThreadObjectData const & object) :
                    m_data(object.m_data),
                    m_running(object.m_running),
                    m_exited(object.m_exited)
                {
                    // can not be called after the thread is started
                    assert(object.m_running == false);
                }

                /// make the executing thread exit this function object
                void exit()
                {
                    if (! m_running)
                        return;
                    {
                        boost::unique_lock<boost::mutex> lock(m_queueMutex);
                        m_running = false;
                    }
                    m_queueConditional.notify_one();
                }

                /// wait for the thread to exit, if it has not exited already
                void wait_on_exit(boost::posix_time::time_duration & t)
                {
                    if (m_exited)
                        return;
                    timer exitTimer;
                    {
                        boost::unique_lock<boost::mutex> lock(m_queueMutex);
                        m_destructionConditional.timed_wait(lock, t);
                    }

                    boost::posix_time::milliseconds elapsed(
                        static_cast<long>(ceil(exitTimer.elapsed() * 1000.0)));
                    if (elapsed < t)
                        t = t - elapsed;
                    else
                        t = boost::posix_time::milliseconds(0);
                }
        
                /// add a function for the thread to execute to the queue
                bool push_back(ThreadFunctionObject const & input)
                {
                    if (! m_running)
                        return false;
                    bool wasEmpty;
                    {
                        boost::lock_guard<boost::mutex> lock(m_queueMutex);
                        wasEmpty = m_queue.empty();
                        m_queue.push_back(input);
                    }
                    if (wasEmpty)
                        m_queueConditional.notify_one();
                    return true;
                }
        
                /// the thread's execution function
                void operator () ()
                {
                    m_running = true;
                    m_exited = false;
                    boost::unique_lock<boost::mutex> lock(m_queueMutex);
                    while (m_running && m_queue.empty())
                        m_queueConditional.wait(lock);
                    while (m_running)
                    {
                        ThreadFunctionObject & function = m_queue.front();
                        lock.unlock();
        
                        function(m_data);
        
                        lock.lock();
                        m_queue.pop_front();
                        while (m_running && m_queue.empty())
                            m_queueConditional.wait(lock);
                    }
                    m_exited = true;
                    m_destructionConditional.notify_one();
                }

            private:
                THREAD_DATA m_data;
                bool m_running;  /// thread is running
                bool m_exited;   /// thread was terminated
                QueueType m_queue;
                boost::mutex m_queueMutex;
                boost::condition_variable m_queueConditional;
                boost::condition_variable m_destructionConditional;

        };

        /// boost::thread parameter container that handles encapsulation of the
        /// thread's data so that object copy semantics do not interfere
        /// with functionality.
        class ThreadObject
        {
            public:
                ThreadObject(ThreadObjectData & data) : m_data(data) {}
                void operator () () { m_data(); }
            private:
                ThreadObjectData & m_data;
        };

    public:
        ThreadPool(size_t initialSize, size_t maxSize, OUTPUT & outputObject) :
            m_threads(maxSize),
            m_objects(maxSize),
            m_currentThread(0),
            m_totalConfigured(0),
            m_totalActive(0),
            m_stop(false),
            m_outputObject(outputObject)
        {
            configure(initialSize);
            // configure pre-spawns the threads but the pool size
            // still starts at 0
            m_totalActive = initialSize;
        }

        /// cause all threads to exit and unconfigure the thread pool
        void exit(size_t const timeout)
        {
            m_stop = true;
            for (size_t i = 0; i < m_totalConfigured; ++i)
                m_objects[i].exit();
            boost::posix_time::milliseconds const zero(0);
            boost::posix_time::milliseconds t(timeout);
            for (size_t i = 0; i < m_totalConfigured; ++i)
            {
                m_objects[i].wait_on_exit(t);
                if (t == zero)
                    break;
            }
            m_currentThread = 0;
            m_totalConfigured = 0;
            m_totalActive = 0;
        }

        /// put a function into the thread pool for execution
        bool input(INPUT & task)
        {
            {
                boost::lock_guard<boost::mutex> lock(m_taskInputMutex);
                if (! m_objects[m_currentThread].push_back(
                          ThreadFunctionObject(task, *this)))
                    return false;
            }
            if (++m_currentThread == m_totalActive)
                m_currentThread = 0;
            return true;
        }

        /// make the thread pool grow by an increment
        void grow(size_t increment)
        {
            configure(m_totalActive + increment);
        }

        /// make the thread pool shrink by a decrement
        void shrink(size_t decrement)
        {
            assert(decrement <= m_totalActive);
            configure(m_totalActive - decrement);
        }

        /// stop boolean reference for checking if an exit should occur
        /// (should be stored as a 'boost const &' to
        ///  prevent external modifications)
        bool const & stop() const
        {
            return m_stop;
        }

    private:
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
            if (count > m_totalConfigured)
            {
                for (size_t i = m_totalConfigured; i < count; ++i)
                    m_threads[i].reset(
                        new boost::thread(ThreadObject(m_objects[i])));
                m_totalConfigured = count;
            }
            
            // make sure only active threads will be referenced in the future
            if (count < m_totalActive)
            {
                if (m_currentThread >= count)
                    m_currentThread = 0;
            }
            m_totalActive = count;
        }

        /// get a result object from a thread that is almost done executing
        void output(OUTPUT_DATA & result)
        {
            boost::mutex::scoped_try_lock lock(m_taskOutputMutex);
            while (! lock.owns_lock())
            {
                if (m_stop)
                    return;
                boost::this_thread::yield();
                lock.try_lock();
            }
            if (m_stop)
                return;
            m_outputObject.output(result);
        }

        std::vector< copy_ptr<boost::thread> > m_threads;
        std::vector<ThreadObjectData> m_objects;
        size_t m_currentThread;
        size_t m_totalConfigured;
        size_t m_totalActive;
        boost::mutex m_taskInputMutex;
        boost::mutex m_taskOutputMutex;
        bool m_stop;
        OUTPUT & m_outputObject;
};

#endif // THREAD_POOL_HPP

