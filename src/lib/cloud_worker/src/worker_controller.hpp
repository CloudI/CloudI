// -*- coding: utf-8; Mode: C++; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
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
#include "event_pipe.hpp"
#include "library.hpp"
#include "safe_shared_ptr.hpp"
#include "realloc_ptr.hpp"
#include "ei_x_buff_ptr.hpp"
#include "worker_protocol.hpp"
#include <boost/thread/mutex.hpp>
#include <boost/scoped_ptr.hpp>
#include <boost/scoped_array.hpp>
#include <boost/filesystem.hpp>
#include <map>
#include <list>
#include <string>
#include <cstring>
#include <stdint.h>

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
            safe_shared_ptr<
                safe_object_pool<WorkerProtocol::PushJobTaskResultRequest>
            > >
            PushJobTaskResultRequestType;
        /// created in WorkerProtocol::WorkerInboundMessage
        typedef 
            WorkerProtocol::WorkerInboundMessage::PushJobTaskResultResponseType
            PushJobTaskResultResponseType;

        WorkerController();
        ~WorkerController();

        /// receive work data if it is available
        /// @return ExitStatus 0 if no error occurred
        int receive_work(int fd);

        /// send work data after an event was triggered
        int send_work(ei_cnode * ec, struct pollfd const * pfds, int nfds,
                      bool retransmit);
        /// send stderr data to the "standard_error" registered process
        int send_stderr(ei_cnode * ec, struct pollfd const & pfd,
                        char const * const buffer, size_t size);

        inline bool cloud_worker_add_work(
            std::string const & workTitle,
            uint32_t idOffset, uint32_t numberOfThreads)
        {
            return m_workerQueries.add(workTitle, idOffset, numberOfThreads);
        }

        inline bool cloud_worker_has_work(
            std::string const & workTitle,
            uint32_t idOffset, uint32_t numberOfThreads)
        {
            return m_workerQueries.has(workTitle, idOffset, numberOfThreads);
        }

        inline bool cloud_worker_remove_work(
            std::string const & workTitle)
        {
            return m_workerQueries.remove(workTitle);
        }

        inline void cloud_worker_clear_work()
        {
            m_workerQueries.clear();
        }

        inline void set_current_path(char const * const pPath)
        {
            m_currentPath = std::string(pPath);
            m_currentPath.remove_filename();
        }
        inline boost::filesystem::path const & get_current_path() const
        {
            return m_currentPath;
        }

        inline int get_send_event_fd() const
        {
            return m_workerQueries.sendEvent().fd();
        }

        inline int timeout_value()
        {
            return m_workerQueries.timeout_value();
        }

        inline void store(PushJobTaskResultRequestType & ptr)
        {
            m_workerQueries.store(ptr);
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
        inline void store(PullJobTaskResponseType & ptr)
        {
            if (ptr->empty())
                return;
            m_workerQueries.store(ptr);
        }
        inline void store(PushJobTaskResultResponseType & ptr)
        {
            m_workerQueries.store(ptr);
        }

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

        // handle concurrent work execution with an internal thread pool
        class WorkerExecution
        {
            public:
                class ThreadPool;
                WorkerExecution(WorkerController & controller);
                ~WorkerExecution();
                void input(PullJobTaskResponseType & pTask);
                void output(PushJobTaskResultRequestType & pTask)
                {
                    m_controller.store(pTask);
                }
                void increase_capacity(size_t count);
                size_t count(std::vector<WorkId> const & taskArray,
                    PendingRequestsLookup & requests,
                    PendingResultsLookup & results);
                size_t erase(WorkId const & id,
                    PendingRequestsLookup & requests,
                    PendingResultsLookup & results);
                void clear();
            private:
                WorkerExecution(WorkerExecution const &);

                WorkerController & m_controller;
                // id for a work title instance of a work library
                class LibraryId;
                typedef std::map<LibraryId, safe_shared_ptr<library> >
                    WorkLibraryLookup;
                WorkLibraryLookup m_libraries;
                boost::scoped_ptr<ThreadPool> m_pThreadPool;
        };

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
                void clear();

                // add the result of a work task
                void store(PushJobTaskResultRequestType & ptr);

                // add the received response from a task request
                void store(PullJobTaskResponseType & ptr);

                // add the received response from a task result
                void store(PushJobTaskResultResponseType & ptr);

                int timeout_value();

                typedef const_iterator_map_merge<
                    WorkerProtocol::WorkerOutboundMessage *,
                    PendingRequestsLookup, PendingResultsLookup> const_iterator;
                const_iterator begin();
                inline const_iterator end() const
                {
                    const_iterator e(m_pendingRequests, m_pendingResults);
                    e.set_end();
                    return e;
                }
                bool empty();
                event_pipe const & sendEvent() const { return m_sendEvent; }

            private:
                event_pipe m_sendEvent;
                PendingRequestsLookup m_pendingRequests;
                PullJobTaskRequestType::pool_ptr_type m_pRequestAllocator;
                
                PendingResultsLookup m_pendingResults;

                std::list<PushJobTaskResultRequestType> m_newResults;
                boost::mutex m_newResultsMutex;

                WorkerExecution & m_executingTasks;
        };

        // receive buffer
        realloc_ptr<unsigned char> m_asn1ReceiveBuffer;
        ei_x_buff_ptr m_erlangReceiveBuffer;

        // send buffer
        realloc_ptr<unsigned char> m_asn1SendBuffer;
        realloc_ptr<unsigned char> m_erlangSendBuffer;

        boost::filesystem::path m_currentPath;

        WorkerExecution m_executingTasks;
        WorkerQuery m_workerQueries;
};

#include "work_id.hpp"
#include "task_id.hpp"
#include "library_id.hpp"

#endif // WORKER_CONTROLLER_HPP

