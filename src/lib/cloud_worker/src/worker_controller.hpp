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
#ifndef WORKER_CONTROLLER_HPP
#define WORKER_CONTROLLER_HPP

#include "port_main.h"
#include "const_iterator_map_merge.hpp"
#include "timer.hpp"
#include "library.hpp"
#include "safe_shared_ptr.hpp"
#include "realloc_ptr.hpp"
#include "ei_x_buff_ptr.hpp"
#include "worker_protocol.hpp"
#include "boost_thread.hpp"
#include <boost/scoped_ptr.hpp>
#include <boost/scoped_array.hpp>
#include <boost/filesystem.hpp>
#include <map>
#include <string>
#include <cstring>
#include <stdint.h>
#include <cassert>

struct pollfd;
typedef struct ei_cnode_s ei_cnode;

// high-level controller of work so that resources are allocated for the work
// to take place
class WorkerController
{
    public:
        /// created in
        /// add_work() and handle_message_PushJobTaskResultResponse()
        typedef
            pool_copy_ptr<WorkerProtocol::PullJobTaskRequest>
            PullJobTaskRequestType;
        /// created in WorkerProtocol::WorkerInboundMessage
        typedef 
            WorkerProtocol::WorkerInboundMessage::PullJobTaskResponseType
            PullJobTaskResponseType;
        /// created in WorkerExecution thread
        typedef pool_copy_ptr< WorkerProtocol::PushJobTaskResultRequest,
            safe_object_pool<WorkerProtocol::PushJobTaskResultRequest> >
            PushJobTaskResultRequestType;
        /// created in WorkerProtocol::WorkerInboundMessage
        typedef 
            WorkerProtocol::WorkerInboundMessage::PushJobTaskResultResponseType
            PushJobTaskResultResponseType;

        WorkerController();
        ~WorkerController();

        inline bool add_work(std::string const & workTitle,
                             uint32_t idOffset, uint32_t numberOfThreads)
        {
            return m_workerQueries.add(workTitle, idOffset, numberOfThreads);
        }

        inline bool has_work(std::string const & workTitle,
                             uint32_t idOffset, uint32_t numberOfThreads)
        {
            return m_workerQueries.has(workTitle, idOffset, numberOfThreads);
        }

        inline bool remove_work(std::string const & workTitle)
        {
            return m_workerQueries.remove(workTitle);
        }

        inline bool clear_work()
        {
            return m_workerQueries.clear();
        }

        inline bool add_result(bool const & ignore,
            PushJobTaskResultRequestType & ptr)
        {
            return m_workerQueries.add(ignore, ptr);
        }

        // return ExitStatus
        int receive(int fd);

        /// send data after an event was triggered
        int send(ei_cnode * ec, struct pollfd const * pfds, int nfds,
                 bool retransmit);
        /// send stderr data to the "standard_error" registered process
        int sendStderr(ei_cnode * ec, struct pollfd const & pfd,
                       char const * const buffer, size_t size);

        inline int get_event_fd() const
        {
            return m_workerQueries.get_event_fd();
        }

        inline int timeout_value()
        {
            return m_workerQueries.timeout_value();
        }

        // thus spake the bearded ones (7.3.1/4):
        // "Every namespace-definition shall appear in the
        //  global scope or in a namespace scope (3.3.5)."
        class ExitStatus
        {
            public:enum
            {
                server_socket_ERR = GEPD::ExitStatus::errors_max,
                server_socket_HUP,
                server_socket_NVAL,
                server_socket_accept,
                epmd_socket_ERR,
                epmd_socket_HUP,
                epmd_socket_NVAL,
                event_pipe_ERR,
                event_pipe_HUP,
                event_pipe_NVAL,
                stderr_ERR,
                stderr_HUP,
                stderr_NVAL,
                controller_alloc,
                node_receive_EIO,
                node_receive_ETIMEDOUT,
                node_receive_unknown,
                node_send_EIO,
                node_send_unknown,
                protocol_deserialize_erlang_failed,
                protocol_deserialize_asn1_failed,
                protocol_deserialize_asn1_incomplete,
                protocol_serialize_erlang_failed,
                protocol_serialize_asn1_copy,
                protocol_serialize_asn1_failed,
                protocol_unknown_message
            };
        };

    private:
        void handle_message_PullJobTaskResponse(
            PullJobTaskResponseType & ptr);
        void handle_message_PushJobTaskResultResponse(
            PushJobTaskResultResponseType & ptr);

        // receive buffer
        realloc_ptr<unsigned char> m_asn1ReceiveBuffer;
        ei_x_buff_ptr m_erlangReceiveBuffer;
        static size_t const m_asn1InitialReceiveBufferSize = 1024;
        static size_t const m_asn1MaxReceiveBufferSize = 4194304;    // 4MB

        // send buffer
        realloc_ptr<unsigned char> m_asn1SendBuffer;
        realloc_ptr<unsigned char> m_erlangSendBuffer;
        static size_t const m_asn1InitialSendBufferSize = 1024;
        static size_t const m_asn1MaxSendBufferSize = 268435456;    // 256MB
        static size_t const m_erlangOverheadSize = 64;

        // worker thread id
        class WorkId;

        // task result id
        class TaskId;

        // WorkerQuery request storage
        typedef std::map<WorkId, PullJobTaskRequestType>
            PendingRequestsLookup;

        // WorkerQuery response storage
        typedef std::map<TaskId, PushJobTaskResultRequestType>
            PendingResultsLookup;

    public:
        void setCurrentPath(char const * const pPath);
        boost::filesystem::path const & getCurrentPath() const
        {
            return m_currentPath;
        }

        // handle work execution with the current thread pool
        class WorkerExecution
        {
            public:
                WorkerExecution(WorkerController & controller) :
                    m_controller(controller) {}
                ~WorkerExecution();
                void reserveCapacity(size_t count);
                void add(PullJobTaskResponseType & pTask);
                void discard(bool const & ignore, 
                    PushJobTaskResultRequestType & pTask);
                uint32_t count(std::vector<WorkId> const & taskArray);
                size_t remove(WorkId const & workId,
                    PendingRequestsLookup & pendingRequests,
                    PendingResultsLookup & pendingResults);
                bool clear(
                    PendingRequestsLookup & pendingRequests,
                    PendingResultsLookup & pendingResults);

                typedef std::map<WorkId, size_t> WorkerExecutionLookup;
            private:
                WorkerExecution(WorkerExecution const &);

                // id for a work title instance of a work library
                class LibraryId;

                typedef std::map<LibraryId, safe_shared_ptr<library> >
                    WorkLibraryLookup;

                WorkerController & m_controller;
                WorkerExecutionLookup m_threads;
                WorkLibraryLookup m_libraries;
                // protect access to m_threads and m_libraries
                boost::mutex m_threadsMutex;
        };

    private:
        boost::filesystem::path m_currentPath;

        // WorkerQuery is a Message Dispatcher acting as an
        // Event Triggered Producer of messages that are Request-Reply,
        // with the reply given to Competing Consumers
        // (managed by the WorkerExecution), whose output eventually
        // is sent to the Aggregator of completed work (Erlang code)
        // for fault-tolerant storage management
        class WorkerQuery
        {
            public:
                WorkerQuery(WorkerExecution & executingTasks);
                ~WorkerQuery();

                // add work to request when idle
                bool add(std::string const & workTitle,
                         uint32_t idOffset, uint32_t numberOfThreads);

                // check if there is work as specified
                bool has(std::string const & workTitle,
                         uint32_t idOffset, uint32_t numberOfThreads);

                // remove work from the queue
                bool remove(std::string const & workTitle);

                // clear all work from the queue
                bool clear();

                // add the result of work, to store in a fault-tolerant way
                bool add(bool const & ignore, 
                    PushJobTaskResultRequestType & ptr);

                void received(PullJobTaskResponseType & ptr);
                void received(PushJobTaskResultResponseType & ptr);

                inline void flush_events() const
                {
                    // thread-safe
                    char buffer[512];
                    while (read(m_eventPipe[0],
                                reinterpret_cast<void *>(buffer),
                                sizeof(buffer)) == sizeof(buffer)) {}
                }
                inline bool trigger_event() const
                {
                    // thread-safe
                    char const value = 1;
                    if (write(m_eventPipe[1],
                              static_cast<void const *>(&value),
                              sizeof(char)) == sizeof(char))
                        return true;
                    else
                        return false;
                }
                inline int get_event_fd() const { return m_eventPipe[0]; }
                int timeout_value();

                typedef const_iterator_map_merge<
                    WorkerProtocol::WorkerOutboundMessage *,
                    PendingRequestsLookup, PendingResultsLookup> const_iterator;
                inline const_iterator begin() const
                {
                    return const_iterator(m_pendingRequests, m_pendingResults);
                }
                inline const_iterator end() const
                {
                    const_iterator e(m_pendingRequests, m_pendingResults);
                    e.set_end();
                    return e;
                }
                inline size_t size()
                {
                    // make sure the m_resultsMutex has been locked
                    assert(m_resultsMutex.try_lock() == false);
                    return (
                        m_pendingRequests.size() + m_pendingResults.size());
                }
                inline size_t empty()
                {
                    // make sure the m_resultsMutex has been locked
                    assert(m_resultsMutex.try_lock() == false);
                    return (
                        m_pendingRequests.empty() && m_pendingResults.empty());
                }
                boost::mutex & resultsMutex() { return m_resultsMutex; }

            private:
                // how long to wait before resending request messages
                int m_receiveTimeout;
                static int const m_maxReceiveTimeout = 1000; // milliseconds
                timer m_receiveTimeoutTimer;

                volatile int m_eventPipe[2];
                PendingRequestsLookup m_pendingRequests;
                boost::object_pool<
                    WorkerProtocol::PullJobTaskRequest> m_requestPool;

                PendingResultsLookup m_pendingResults;
                boost::mutex m_resultsMutex; // protect m_pendingResults

                WorkerExecution & m_executingTasks;
        };

        WorkerExecution m_executingTasks;
        WorkerQuery m_workerQueries;
};

class WorkerController::WorkId
{
    private:
        static uint32_t const maxId;
    public:
        // matches specific work with any worker id
        WorkId(std::string const & workTitle) :
            m_workTitle(workTitle), m_id(maxId) {}
        // matches a specific worker's work
        WorkId(std::string const & workTitle, uint32_t id) :
            m_workTitle(workTitle), m_id(id) {}
        inline friend bool operator <(WorkId const & lhs, WorkId const & rhs)
        {
            int const check1 = lhs.m_workTitle.compare(rhs.m_workTitle);
            if (check1 < 0)
            {
                return true;
            }
            else if (check1 > 0)
            {
                return false;
            }
            else
            {
                if (lhs.m_id == maxId || rhs.m_id == maxId)
                    return false;
                else
                    return (lhs.m_id < rhs.m_id);
            }
        }
        inline friend bool operator ==(WorkId const & lhs, WorkId const & rhs)
        {
            bool const check1 = (lhs.m_workTitle.compare(rhs.m_workTitle) == 0);
            if (lhs.m_id == maxId || rhs.m_id == maxId)
                return check1;
            else
                return (check1 && lhs.m_id == rhs.m_id);
        }
        inline friend bool operator !=(WorkId const & lhs, WorkId const & rhs)
        {
            return ! (lhs == rhs);
        }

        std::string const & workTitle() const { return m_workTitle; }
        uint32_t id() const { return m_id; }
    private:
        std::string m_workTitle;
        uint32_t m_id;
};

class WorkerController::TaskId
{
    private:
        static size_t const maxTaskDataSize;
    public:
        TaskId(WorkId const & workId) :
            m_workId(workId),
            m_taskDataSize(maxTaskDataSize)
        {
        }
        TaskId(std::string const & workTitle, uint32_t id,
               boost::scoped_array<uint8_t> const & taskData,
               size_t taskDataSize) :
            m_workId(workTitle, id),
            m_taskData(new uint8_t[taskDataSize]),
            m_taskDataSize(taskDataSize)
        {
            memcpy(m_taskData.get(), taskData.get(), taskDataSize);
        }
        TaskId(TaskId const & o) :
            m_workId(o.m_workId),
            m_taskDataSize(o.m_taskDataSize)
        {
            m_taskData.swap(const_cast<TaskId &>(o).m_taskData);
        }
        friend bool operator <(TaskId const & lhs, TaskId const & rhs)
        {
            if (lhs.m_workId != rhs.m_workId)
                return (lhs.m_workId < rhs.m_workId);
            if (lhs.m_taskDataSize == maxTaskDataSize ||
                rhs.m_taskDataSize == maxTaskDataSize)
                return false;
            size_t const size = 
                std::min(lhs.m_taskDataSize, rhs.m_taskDataSize);
            int const check1 = 
                memcmp(static_cast<void *>(lhs.m_taskData.get()),
                       static_cast<void *>(rhs.m_taskData.get()), size);
            if (check1 < 0)
                return true;
            else if (check1 > 0)
                return false;
            else
                return (
                    lhs.m_taskDataSize < rhs.m_taskDataSize);
        }
        friend bool operator ==(TaskId const & lhs, TaskId const & rhs)
        {
            if (lhs.m_workId != rhs.m_workId)
                return false;
            if (lhs.m_taskDataSize == maxTaskDataSize ||
                rhs.m_taskDataSize == maxTaskDataSize)
                return true;
            if (lhs.m_taskDataSize != rhs.m_taskDataSize)
                return false;
            return (memcmp(static_cast<void *>(
                           lhs.m_taskData.get()),
                           static_cast<void *>(
                           rhs.m_taskData.get()),
                           lhs.m_taskDataSize) == 0);
        }
    private:
        WorkId m_workId;
        boost::scoped_array<uint8_t> m_taskData;
        size_t m_taskDataSize;
};

class WorkerController::WorkerExecution::LibraryId
{
    private:
        static char const invalidInstanceChar = 'x';
    public:
        static LibraryId create(std::string const & workTitle)
        {
            size_t const tagIndex = workTitle.find_first_of('.');
            std::string workLibrary(workTitle.substr(0, tagIndex));
            std::string workInstance(workTitle.substr(tagIndex));
            return LibraryId(workLibrary, workInstance);
        }

        LibraryId(std::string const & workLibrary) :
            m_workLibrary(workLibrary),
            m_workInstance(1, invalidInstanceChar) {}
        LibraryId(std::string const & workLibrary,
                  std::string const & workInstance) :
            m_workLibrary(workLibrary),
            m_workInstance(workInstance)
        {
            // valid work instances always have a '.' prefix
            assert(m_workInstance[0] == '.');
        }
        inline friend bool operator <(LibraryId const & lhs,
                                      LibraryId const & rhs)
        {
            int const check1 = lhs.m_workLibrary.compare(rhs.m_workLibrary);
            if (check1 < 0)
            {
                return true;
            }
            else if (check1 > 0)
            {
                return false;
            }
            else
            {
                if (lhs.m_workInstance[0] == invalidInstanceChar ||
                    rhs.m_workInstance[0] == invalidInstanceChar)
                {
                    return check1;
                }
                return (lhs.m_workInstance.compare(rhs.m_workInstance) < 0);
            }
        }
        inline friend bool operator ==(LibraryId const & lhs,
                                       LibraryId const & rhs)
        {
            
            bool const check1 = 
                (lhs.m_workLibrary.compare(rhs.m_workLibrary) == 0);
            if (lhs.m_workInstance[0] == invalidInstanceChar ||
                rhs.m_workInstance[0] == invalidInstanceChar)
                return check1;
            else
                return (check1 && lhs.m_workInstance == rhs.m_workInstance);
        }
        inline friend bool operator !=(LibraryId const & lhs,
                                       LibraryId const & rhs)
        {
            return ! (lhs == rhs);
        }

        std::string const & workLibrary() const { return m_workLibrary; }
    private:
        std::string m_workLibrary;
        std::string m_workInstance;
};

#endif // WORKER_CONTROLLER_HPP

