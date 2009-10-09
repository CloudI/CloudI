// -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
// ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
//
// BSD LICENSE
// 
// Copyright (c) 2009, Michael Truog
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

#include "worker_controller.hpp"
#include "cloud_worker_common.hpp"
#include "worker_protocol.hpp"
#include "node_connections.hpp"
#include "timer.hpp"
#include "copy_ptr.hpp"
#include "library.hpp"
#include "boost_thread.hpp"
#include <boost/date_time/posix_time/posix_time.hpp>
#include <algorithm>
#include <vector>
#include <cassert>

size_t const maximumThreadPoolSize = 512;
size_t const initialThreadPoolSize = 0;

/// ThreadPool object
/// All methods are meant to be used by a single thread
/// except for the output() method (which is meant to be used by other threads).
class WorkerController::WorkerExecution::ThreadPool
{
    private:
        class ThreadFunctionObject
        {
            public:
                typedef PushJobTaskResultRequestType
                    ResultType;
                typedef PushJobTaskResultRequestType::pool_type
                    AllocatorType;
                typedef PushJobTaskResultRequestType::pool_ptr_type
                    AllocatorPtrType;
        
                ThreadFunctionObject(
                    PullJobTaskResponseType & pTask,
                    safe_shared_ptr<library> const & workLibrary,
                    WorkerExecution::ThreadPool & threadPool) :
                        m_pTask(pTask),
                        m_workLibrary(workLibrary),
                        m_threadPool(threadPool),
                        m_id(m_pTask->workTitle(), m_pTask->id())
                {
                }
        
                WorkId const & getWorkId() const { return m_id; }
        
                void operator () (bool & stopped, AllocatorPtrType & allocator);
        
            private:
                PullJobTaskResponseType m_pTask;
                safe_shared_ptr<library> m_workLibrary;
                ThreadPool & m_threadPool;
                WorkId m_id;
        };

        /// Active Object design pattern
        /// with round-robin scheduling in the ThreadPool
        /// and a method (ThreadPool::output()) callback for the result
        class ThreadObjectData
        {
            private:
                typedef std::list<ThreadFunctionObject> QueueType;
            public:
                ThreadObjectData() :
                    m_allocator(new ThreadFunctionObject::AllocatorType),
                    m_running(false)
                {
                }

                ThreadObjectData(ThreadObjectData const & object) :
                    m_allocator(object.m_allocator),
                    m_running(object.m_running),
                    m_stopped(object.m_stopped)
                {
                    // can not be called after the thread is started
                }
        
                /// make the executing thread exit this function object
                void exit()
                {
                    if (! m_running)
                        return;
                    {
                        boost::lock_guard<boost::mutex> lock(m_queueMutex);
                        m_running = false;
                        m_stopped = true;
                    }
                    m_queueConditional.notify_one();
                }
        
                /// stop a worker that is expected to exist in this thread
                bool stop(WorkId const & id)
                {
                    boost::lock_guard<boost::mutex> lock(m_queueMutex);
                    if (m_queue.empty())
                        return false;
                    QueueType::iterator itr = m_queue.begin();
                    if (itr->getWorkId() == id)
                    {
                        if (m_stopped == false)
                        {
                            // stop the task while it is running
                            m_stopped = true;
                            return true;
                        }
                        else
                        {
                            return false;
                        }
                    }
                    for (++itr; itr != m_queue.end(); ++itr)
                    {
                        if (itr->getWorkId() == id)
                        {
                            m_queue.erase(itr);
                            return true;
                        }
                    }
                    return false;
                }
        
                /// check if a worker thread instance exists
                bool find(WorkId const & id)
                {
                    boost::lock_guard<boost::mutex> lock(m_queueMutex);
                    QueueType::const_iterator itr;
                    for (itr = m_queue.begin(); itr != m_queue.end(); ++itr)
                    {
                        if (id == itr->getWorkId())
                            return true;
                    }
                    return false;
                }
        
                /// add a function for the thread to execute to the queue
                bool push_back(ThreadFunctionObject const & input)
                {
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
                    m_stopped = true;
                    boost::unique_lock<boost::mutex> queueLock(m_queueMutex);
                    while (m_running && m_queue.empty())
                        m_queueConditional.wait(queueLock);
                    while (m_running)
                    {
                        ThreadFunctionObject function = m_queue.front();
                        m_stopped = false;
                        queueLock.unlock();
        
                        function(m_stopped, m_allocator);
                        m_stopped = true;
        
                        queueLock.lock();
                        m_queue.pop_front();
                        while (m_running && m_queue.empty())
                            m_queueConditional.wait(queueLock);
                    }
                    m_running = false;
                }
            private:
                ThreadFunctionObject::AllocatorPtrType m_allocator;
                bool m_running; /// thread is running
                bool m_stopped; /// queued function is stopped
                QueueType m_queue;
                boost::mutex m_queueMutex;
                boost::condition_variable m_queueConditional;
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

        typedef std::map<WorkId, size_t> WorkerLookupType;
    public:
        ThreadPool(size_t initialSize, size_t maxSize,
                   WorkerController::WorkerExecution & executionObject) :
            m_threads(maxSize),
            m_objects(maxSize),
            m_currentThread(0),
            m_totalConfigured(0),
            m_totalActive(0),
            m_executionObject(executionObject)
        {
            configure(initialSize);
        }

        /// cause all threads to exit and unconfigure the thread pool
        void exit()
        {
            for (size_t i = 0; i < m_totalConfigured; ++i)
                m_objects[i].exit();
            m_currentThread = 0;
            m_totalConfigured = 0;
            m_totalActive = 0;
            m_workerLookup.clear();
        }

        /// stop a worker that was added to a thread's queue
        bool stop(WorkId const & id)
        {
            boost::lock_guard<boost::mutex> lock(m_taskOutputMutex);
            size_t index;
            {
                boost::lock_guard<boost::mutex> lock(m_taskInputMutex);
                WorkerLookupType::iterator itr = m_workerLookup.find(id);
                if (itr == m_workerLookup.end())
                    return false;
                index = itr->second;
            }
            return m_objects[index].stop(id);
        }

        /// put a function into the thread pool for execution
        bool input(WorkerController::PullJobTaskResponseType & pTask,
                   safe_shared_ptr<library> const & workLibrary)
        {
            {
                boost::lock_guard<boost::mutex> lock(m_taskInputMutex);
                WorkId const id(pTask->workTitle(), pTask->id());
                if (! m_objects[m_currentThread].push_back(
                          ThreadFunctionObject(pTask, workLibrary, *this)))
                    return false;
                m_workerLookup[id] = m_currentThread;
            }
            if (++m_currentThread == m_totalActive)
                m_currentThread = 0;
            return true;
        }

        /// get a result object from a thread that is almost done executing
        void output(bool & stopped, ThreadFunctionObject::ResultType & result)
        {
            boost::mutex::scoped_try_lock lock(m_taskOutputMutex);
            while (! lock.owns_lock())
            {
                if (stopped)
                    return;
                boost::this_thread::yield();
                lock.try_lock();
            }
            if (stopped)
                return;
            stopped = true; // unable to stop now
            m_executionObject.output(result);
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

        /// count all worker thread instances, to verify the work assignments
        size_t count(std::vector<WorkId> const & taskArray,
            PendingRequestsLookup & requests,
            PendingResultsLookup & results)
        {
            size_t count = 0;
            boost::lock_guard<boost::mutex> lockOutput(m_taskOutputMutex);
            boost::lock_guard<boost::mutex> lockInput(m_taskInputMutex);
            std::vector<WorkId>::const_iterator itr;
            for (itr = taskArray.begin(); itr != taskArray.end(); ++itr)
            {
                PendingRequestsLookup::iterator requestItr =
                    requests.find(*itr);
                if (requestItr != requests.end())
                {
                    ++count;
                }
                else
                {
                    PendingResultsLookup::iterator resultsItr =
                        results.find(*itr);
                    if (resultsItr != results.end())
                    {
                        ++count;
                    }
                    else
                    {
                        WorkerLookupType::iterator workerItr =
                            m_workerLookup.find(*itr);
                        if (workerItr != m_workerLookup.end())
                        {
                            if (m_objects[workerItr->second].find(*itr))
                                ++count;
                        }
                    }
                }
            }
            return count;
        }

        /// erase the worker thread data to eliminate further processing
        size_t erase(WorkId const & id,
            PendingRequestsLookup & requests,
            PendingResultsLookup & results)
        {
            boost::lock_guard<boost::mutex> lockOutput(m_taskOutputMutex);
            boost::lock_guard<boost::mutex> lockInput(m_taskInputMutex);
            size_t count = 0;
            std::pair<WorkerLookupType::iterator, WorkerLookupType::iterator>
                workers = m_workerLookup.equal_range(id);
            WorkerLookupType::iterator workerItr = workers.first;
            for (; workerItr != workers.second; ++workerItr)
            {
                if (m_objects[workerItr->second].stop(id))
                    ++count;
            }
            m_workerLookup.erase(workers.first, workers.second);
            return (requests.erase(id) + results.erase(id) + count);
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

        std::vector< copy_ptr<boost::thread> > m_threads;
        std::vector<ThreadObjectData> m_objects;
        size_t m_currentThread;
        size_t m_totalConfigured;
        size_t m_totalActive;
        boost::mutex m_taskInputMutex;
        WorkerLookupType m_workerLookup;
        boost::mutex m_taskOutputMutex;
        WorkerController::WorkerExecution & m_executionObject;
};

void WorkerController::WorkerExecution::ThreadPool::ThreadFunctionObject::
    operator () (bool & stopped, AllocatorPtrType & allocator)
{
    if (stopped)
        return;
    timer workTimer;
    DatabaseQueryVector queriesOut;
    queriesOut.reserve(16);
    bool returnValue = false;
    try
    {
        typedef bool (*WorkFunctionType)(
            bool const &,
            uint32_t const,
            uint32_t const,
            uint32_t const,
            boost::scoped_array<uint8_t> const &,
            size_t const,
            DatabaseQueryVector const &,
            DatabaseQueryVector &);
        WorkFunctionType workFunction = 
            m_workLibrary->f<WorkFunctionType>("do_work");
        returnValue = workFunction(
                stopped,
                m_pTask->failureCount(),
                m_pTask->id(),
                m_pTask->totalIds(),
                m_pTask->taskData(),
                m_pTask->taskDataSize(),
                m_pTask->queries(),
                queriesOut
            );
    }
    catch (boost::thread_interrupted const & te)
    {
        throw te;
    }
    catch (std::exception const & e)
    {
        std::cerr << m_pTask->workTitle() << ": " <<
            m_pTask->id() << " failed: " << e.what() << std::endl;
    }
    catch (...)
    {
        std::cerr << m_pTask->workTitle() << ": " <<
            m_pTask->id() << " failed: unknown exception" << std::endl;
    }
    double const elapsedTimeInHours = (workTimer.elapsed() / 3600.0);
    if (stopped)
        return;
    ResultType pResult(allocator);
    pResult(
        NodeConnections::nodeName(),
        m_pTask->workTitle(),
        m_pTask->id(),
        m_pTask->sequence(),
        m_pTask->taskSize(),
        m_pTask->taskData().get(),
        m_pTask->taskDataSize(),
        elapsedTimeInHours,
        returnValue,
        queriesOut,
        m_pTask->failureCount());
    m_threadPool.output(stopped, pResult);
}

WorkerController::WorkerExecution::WorkerExecution(
    WorkerController & controller) : m_controller(controller)
{
    m_pThreadPool.reset(
        new ThreadPool(initialThreadPoolSize, maximumThreadPoolSize, *this));
}

WorkerController::WorkerExecution::~WorkerExecution()
{
    m_pThreadPool->exit();
}

void WorkerController::WorkerExecution::input(
    PullJobTaskResponseType & pTask)
{
    LibraryId libraryId(LibraryId::create(pTask->workTitle()));

    // find the library
    WorkLibraryLookup::iterator libraryItr = m_libraries.find(libraryId);
    if (libraryItr == m_libraries.end())
    {
        typedef std::pair<LibraryId, safe_shared_ptr<library> > LibraryPair;

        // determine the library path
        boost::filesystem::path libraryPath = m_controller.getCurrentPath();
        std::string const libraryPrefix("work_types/lib");
        std::string const librarySuffix(".so");
        libraryPath /= 
            libraryPrefix + libraryId.workLibrary() + librarySuffix;

        // store the library reference
        // (libraries are reference counted internally, so
        //  separate work types (work titles) will get separate
        //  library instances pointing to the same work library)
        std::pair<WorkLibraryLookup::iterator, bool> libraryResult = 
            m_libraries.insert(LibraryPair(
                libraryId, 
                safe_shared_ptr<library>(new library(libraryPath.string()))
            ));
        assert(libraryResult.second);
        libraryItr = libraryResult.first;
        if (! libraryItr->second->loaded())
        {
            // the request message was deleted
            // previous to this function call
            // so no further work of this type will be attempted.
            // (for this response message at least, others should be
            //  ignored in the same way)
            std::cerr << "invalid work library \"" <<
                libraryPath.string() << "\": " <<
                libraryItr->second->error() << std::endl;
            m_libraries.erase(libraryItr);
            return;
        }
    }

    if (! m_pThreadPool->input(pTask, libraryItr->second))
    {
        std::cerr << "unable to execute task for \"" <<
            libraryId.workLibrary() << "\" work library" << std::endl;
    }
}

void WorkerController::WorkerExecution::increaseCapacity(size_t count)
{
    m_pThreadPool->grow(count);
}

size_t WorkerController::WorkerExecution::count(
    std::vector<WorkId> const & taskArray,
    PendingRequestsLookup & requests,
    PendingResultsLookup & results)
{
    return m_pThreadPool->count(taskArray, requests, results);
}

size_t WorkerController::WorkerExecution::erase(WorkId const & id,
    PendingRequestsLookup & requests,
    PendingResultsLookup & results)
{
    size_t const count = m_pThreadPool->erase(id, requests, results);
    m_pThreadPool->shrink(count);
    return count;
}

void WorkerController::WorkerExecution::clear()
{
    m_pThreadPool->exit();
    m_pThreadPool.reset(
        new ThreadPool(initialThreadPoolSize, maximumThreadPoolSize, *this));
}

