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
#include "library.hpp"
#include "boost_thread.hpp"
#include <boost/date_time/posix_time/posix_time.hpp>
#include <algorithm>
#include <vector>
#include <cassert>

namespace
{

class ThreadPool
{
    public:
        class ThreadObject
        {
            public:
                typedef safe_object_pool<
                    WorkerProtocol::PushJobTaskResultRequest> PoolType;

                ThreadObject() :
                    m_running(false) {}
                ThreadObject(ThreadObject const &) :
                    m_running(false) {}
                ThreadObject & operator = (ThreadObject const &)
                {
                    m_running = false;
                    return *this;
                }

                // run a ThreadFunctionObject
                template <typename F>
                void run(F const & object)
                {
                    F & objectRef = const_cast<F &>(object);
                    objectRef.attach(this);
                    m_thread = boost::thread(objectRef);
                }

                // force the thread to stop if it is running
                void stop(size_t const timeoutLimit)
                {
                    boost::lock_guard<boost::mutex> lock(m_stopMutex);
                    // if the thread is not running,
                    // then do not wait for it to stop
                    if (! m_running)
                        return;

                    // stop the running thread
                    m_stopProcessing = true;

                    // detach the thread
                    m_thread = boost::thread();

                    // If a work type is not paying attention to the
                    // stopProcessing state, it is possible it will
                    // get stuck processing work for some indefinite
                    // period of time.  It is also possible that
                    // the Erlang VM will terminate before the
                    // threads have stopped, which will leave the main thread
                    // waiting for threads that have already terminated
                    // (at least that seems to be the case,
                    //  more testing is needed here).
                    // The problem develops when a timeout occurs for
                    // cloud_worker_port_sup:stop_port/1 when nodes are
                    // being halted.  So, a timeoutLimit should be supplied
                    // that respects the Erlang function call timeout.
                    size_t const timeout =
                        std::max(timeoutLimit, static_cast<size_t>(1));

                    // wait for the thread to stop
                    size_t const timeoutIncrement = 25;
                    size_t timeoutElapsed = 0;
                    boost::posix_time::milliseconds delay(timeoutIncrement);
                    while (m_stopProcessing && timeoutElapsed < timeout)
                    {
                        boost::this_thread::sleep(delay);
                        timeoutElapsed += timeoutIncrement;
                    }
                }

                PoolType & getResultPool()
                {
                    return m_resultPool;
                }

                bool const & stopProcessing() const
                {
                    return m_stopProcessing;
                }

                // processing in the thread function has started
                void startedProcessing()
                {
                    boost::lock_guard<boost::mutex> lock(m_stopMutex);
                    m_running = true;
                    m_stopProcessing = false;
                }

                // processing in the thread function has finished
                void finishedProcessing()
                {
                    boost::lock_guard<boost::mutex> lock(m_stopMutex);
                    m_running = false;
                    m_stopProcessing = false;
                }
            private:
                boost::thread m_thread;
                boost::mutex m_stopMutex;
                bool m_running;
                bool m_stopProcessing;
                PoolType m_resultPool;
        };

        ThreadPool(size_t initialSize) :
            m_currentThread(0), m_threadCount(0), 
            m_threads(initialSize) {}
        void reserve(size_t threadCount)
        {
            m_threadCount += threadCount;
            if (m_threadCount >= m_threads.size())
            {
                // force a resize to a power of 2 that is greater than i
                int bits = 1;
                for (size_t div2 = m_threadCount; div2 > 1; div2 >>= 1)
                    ++bits;
                m_threads.resize(1 << bits);
                assert(m_threadCount < m_threads.size());
            }
        }
        void shrink(size_t threadCount)
        {
            m_threadCount -= threadCount;
            if (m_currentThread >= m_threadCount)
                m_currentThread = 0;
        }
        void stop(size_t const index, size_t const timeoutLimit)
        {
            m_threads[index].stop(timeoutLimit);
        }
        template <typename F>
        size_t run(F const & object)
        {
            size_t const index = m_currentThread;
            m_threads[m_currentThread++].run(object);
            if (m_currentThread == m_threadCount)
                m_currentThread = 0;
            return index;
        }
    private:
        size_t m_currentThread;
        size_t m_threadCount;
        std::vector<ThreadObject> m_threads;
};

class ThreadFunctionObject
{
    public:
        ThreadFunctionObject() : m_pThreadObject(0) {}
        virtual ~ThreadFunctionObject() {}

        // attach a ThreadPool::ThreadObject
        void attach(ThreadPool::ThreadObject * pThreadObject)
        {
            m_pThreadObject = pThreadObject;
        }

        // get the result memory pool from the ThreadPool::ThreadObject
        ThreadPool::ThreadObject::PoolType & getResultPool() const
        {
            assert(m_pThreadObject);
            return m_pThreadObject->getResultPool();
        }

        // get the stop processing flag from the ThreadPool::ThreadObject
        bool const & stopProcessing() const
        {
            assert(m_pThreadObject);
            return m_pThreadObject->stopProcessing();
        }

        // manage ThreadPool::ThreadObject state changes
        // within the ThreadFunctionObject::operator () function
        class ScopedProcessing
        {
            public:
                friend class ThreadFunctionObject;

                ~ScopedProcessing()
                {
                    m_obj.finishedProcessing();
                }
            private:
                ScopedProcessing(ThreadPool::ThreadObject & obj) : m_obj(obj)
                {
                    m_obj.startedProcessing();
                }
                ThreadPool::ThreadObject & m_obj;
        };
        
        ScopedProcessing getScopedProcessing() const
        {
            assert(m_pThreadObject);
            return ScopedProcessing(*m_pThreadObject);
        }

        // thread function
        virtual void operator () () = 0;

    private:
        ThreadPool::ThreadObject * m_pThreadObject; // ThreadPool::ThreadObject
};

class TaskContainer : public ThreadFunctionObject
{
    public:
        TaskContainer(
            WorkerController::PullJobTaskResponseType & pTask,
            safe_shared_ptr<library> const & taskLibrary,
            WorkerController::WorkerExecution & execution) :
            m_execution(execution),
            m_taskLibrary(taskLibrary),
            m_pTask(pTask) {}

        void operator () ()
        {
            ScopedProcessing cleanupObject(getScopedProcessing());
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
                    m_taskLibrary->f<WorkFunctionType>("do_work");
                returnValue = workFunction(
                        stopProcessing(),
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
            if (stopProcessing())
                return;
            WorkerController::PushJobTaskResultRequestType
                pResult(getResultPool());
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
            m_execution.discard(stopProcessing(), pResult);
        }

    private:
        WorkerController::WorkerExecution & m_execution;
        safe_shared_ptr<library> m_taskLibrary;
        WorkerController::PullJobTaskResponseType m_pTask;
};

// the always true predicate function object
class always_true
{
    public:
        typedef
            WorkerController::WorkerExecution::WorkerExecutionLookup::value_type
            ArgumentType;
        bool operator () (ArgumentType &) { return true; }
};

// WorkerExecution thread pool decoupled
// from the WorkerExecution implementation
static ThreadPool executionPool(1);

// respect the timeout of the
// cloud_worker_port_sup:stop_port/1 Erlang function call (5000 milliseconds)
static size_t const totalStopTimeoutLimit = 4000; // milliseconds

} // anonymous namespace end

WorkerController::WorkerExecution::~WorkerExecution()
{
    boost::lock_guard<boost::mutex> lock(m_threadsMutex);
    size_t const totalThreads = m_threads.size();
    if (totalThreads > 0)
    {
        size_t const timeoutLimit = totalStopTimeoutLimit / totalThreads;
        // make sure all the current threads die and the results are ignored
        WorkerExecutionLookup::iterator itr;
        for (itr = m_threads.begin(); itr != m_threads.end(); ++itr)
            executionPool.stop(itr->second, timeoutLimit);
        m_threads.clear();
    }
}

void WorkerController::WorkerExecution::reserveCapacity(size_t count)
{
    executionPool.reserve(count);
}

void WorkerController::WorkerExecution::add(PullJobTaskResponseType & pTask)
{
    LibraryId libraryId(LibraryId::create(pTask->workTitle()));

    // lock access to m_libraries and m_threads
    boost::lock_guard<boost::mutex> lock(m_threadsMutex);

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

    // create the execution thread
    typedef std::pair<WorkId, size_t> ThreadsPair;
    WorkId workId(pTask->workTitle(), pTask->id());
    std::pair<WorkerExecutionLookup::iterator, bool> threadResult = 
        m_threads.insert(ThreadsPair(workId,
            executionPool.run(
                TaskContainer(pTask, libraryItr->second, *this)
            )
        ));
    assert(threadResult.second);
}

void WorkerController::WorkerExecution::discard(bool const & ignore,
    PushJobTaskResultRequestType & pTask)
{
    boost::mutex::scoped_try_lock lock(m_threadsMutex);
    if (! lock.owns_lock())
    {
        do
        {
            // ignore the results if possible
            // but don't get stuck waiting for a lock
            if (ignore)
                return;
            boost::this_thread::yield();
        }
        while (! lock.try_lock());
    }
    WorkId workId(pTask->workTitle(), pTask->id());
    m_controller.add_result(ignore, pTask);
    m_threads.erase(workId);
}

uint32_t WorkerController::WorkerExecution::count(
    std::vector<WorkerController::WorkId> const & taskArray)
{
    boost::lock_guard<boost::mutex> lock(m_threadsMutex);
    uint32_t tasksFound = 0;
    std::vector<WorkerController::WorkId>::const_iterator itr;
    for (itr = taskArray.begin(); itr != taskArray.end(); ++itr)
    {
        WorkerExecutionLookup::const_iterator threadItr = m_threads.find(*itr);
        if (threadItr != m_threads.end())
            ++tasksFound;
    }
    return tasksFound;
}

size_t WorkerController::WorkerExecution::remove(
    WorkerController::WorkId const & workId,
    PendingRequestsLookup & pendingRequests,
    PendingResultsLookup & pendingResults)
{
    boost::lock_guard<boost::mutex> lock(m_threadsMutex);
    size_t count = (pendingRequests.erase(workId) +
        pendingResults.erase(workId));
    std::pair<WorkerExecutionLookup::iterator,
        WorkerExecutionLookup::iterator> threadToErase =
            m_threads.equal_range(workId);
    if (threadToErase.first != threadToErase.second)
    {
        size_t const totalThreads =
            std::count_if(threadToErase.first,
                          threadToErase.second, always_true());
        size_t const timeoutLimit = totalStopTimeoutLimit / totalThreads;
        WorkerExecutionLookup::iterator itr;
        for (itr = threadToErase.first; itr != threadToErase.second; ++itr)
        {
            executionPool.stop(itr->second, timeoutLimit);
            ++count;
        }
        m_threads.erase(threadToErase.first, threadToErase.second);
    }
    m_libraries.erase(LibraryId::create(workId.workTitle()));
    executionPool.shrink(count);
    return count;
}

bool WorkerController::WorkerExecution::clear(
    PendingRequestsLookup & pendingRequests,
    PendingResultsLookup & pendingResults)
{
    boost::lock_guard<boost::mutex> lock(m_threadsMutex);
    size_t count = (pendingRequests.size() + pendingResults.size());
    pendingRequests.clear();
    pendingResults.clear();
    size_t const totalThreads = m_threads.size();
    if (totalThreads > 0)
    {
        size_t const timeoutLimit = totalStopTimeoutLimit / totalThreads;
        WorkerExecutionLookup::iterator itr;
        for (itr = m_threads.begin(); itr != m_threads.end(); ++itr)
        {
            executionPool.stop(itr->second, timeoutLimit);
            ++count;
        }
        m_threads.clear();
    }
    m_libraries.clear();
    executionPool.shrink(count);
    return (count > 0);
}

