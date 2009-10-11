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
#ifndef WORKER_PROTOCOL_HPP
#define WORKER_PROTOCOL_HPP

#include <vector>
#include <string>
#include <stdint.h>
#include <boost/scoped_array.hpp>
#include "cloud_work_interface_data.hpp"
#include "worker_protocol_defs.hpp"
#include "pool_copy_ptr.hpp"

typedef struct WorkerMessage WorkerMessage_t;
template <typename T> class realloc_ptr;

namespace WorkerProtocol
{

class WorkerOutboundMessage
{
    public:
        WorkerOutboundMessage();
        ~WorkerOutboundMessage();
        /// keep track of whether the message was sent
        void sent() const { m_sent = true; }
        /// @return bool if message was sent then true, else false
        bool wasSent() const { return m_sent; }
        /// confirm the data was valid after loading into asn1 structures
        bool valid() const { return m_valid; }
        /// serialize the asn1 structures to a dynamic buffer
        /// @return size_t bytes serialized, 0 if an error occurred
        size_t serialize(realloc_ptr<unsigned char> &buffer) const;
        /// @return char* name of globally registered process to send to
        char const * destination() const { return m_pProcessName; }
    protected:
        WorkerMessage_t & m_message;
        bool m_valid;
    private:
        WorkerOutboundMessage(WorkerOutboundMessage const &);
        char const * const m_pProcessName;
        mutable bool m_sent; // state unrelated to message contents
        uint8_t m_pMessageData[SIZEOF_WORKERMESSAGE_T];
};

class PullJobTaskResponse;
class PushJobTaskResultResponse;

class WorkerInboundMessage
{
    public:
        WorkerInboundMessage();

        bool deserialize(void const * buffer, size_t bufferSize);
        bool deserializeIncomplete() const
        {
            return m_incomplete;
        }

        typedef pool_copy_ptr< PullJobTaskResponse,
            safe_shared_ptr< safe_object_pool<PullJobTaskResponse> > >
            PullJobTaskResponseType;
        typedef pool_copy_ptr< PushJobTaskResultResponse >
            PushJobTaskResultResponseType;
        
        PullJobTaskResponseType & getPullJobTaskResponse()
        {
            return m_pPullJobTaskResponse;
        }
        PushJobTaskResultResponseType & getPushJobTaskResultResponse()
        {
            return m_pPushJobTaskResultResponse;
        }
    private:
        bool m_incomplete;

        PullJobTaskResponseType m_pPullJobTaskResponse;
        PushJobTaskResultResponseType m_pPushJobTaskResultResponse;

        // request responses need to be thread-safe
        static safe_shared_ptr<
            safe_object_pool<PullJobTaskResponse> > m_pRequestsPool;
        // result responses don't need to be thread-safe
        static safe_shared_ptr<
            boost::object_pool<PushJobTaskResultResponse> > m_pResultsPool;
        
};

class PullJobTaskRequest : public WorkerOutboundMessage
{
    public:
        /// @param workerName node name where request originated from
        /// @param workTitle work name that identifies the type of work
        /// @param id worker id identifying the worker's thread of execution
        PullJobTaskRequest(std::string const & workerName,
                           std::string const & workTitle,
                           uint32_t id);
};

class PullJobTaskResponse
{
    public:
        PullJobTaskResponse(WorkerMessage_t & message);

        /// @return bool if work exists within the response, true else false
        bool empty() const
        {
            return (m_taskDataSize == 0);
        }
    
        /// @return std::string work name that identifies the type of work
        std::string const & workTitle() const
        {
            return m_workTitle;
        }
        /// @return uint32_t id identifying the thread of work type execution
        ///                  (specific to the type of work, but a global index)
        uint32_t id() const
        {
            return m_id;
        }
        /// @return uint32_t total ids executing on this type of work
        ///                  with the current configuration
        uint32_t totalIds() const
        {
            return m_totalIds;
        }
        /// @return uint32_t sequence number to track output
        uint32_t sequence() const
        {
            return m_sequence;
        }
        /// @return double task size defined by the work module
        double taskSize() const
        {
            return m_taskSize;
        }
        /// @return uint8_t[] work task input that defines the unit of work
        ///                   (unique to this task for the work)
        boost::scoped_array<uint8_t> const & taskData() const
        {
            return m_taskData;
        }
        /// @return size_t size of the task input data
        size_t taskDataSize() const
        {
            return m_taskDataSize;
        }
        /// @return std::vector work task queries to be performed
        DatabaseQueryVector const & queries() const
        {
            return m_queries;
        }
        /// @return uint32_t count of how many failures this task encountered
        uint32_t failureCount() const
        {
            return m_failureCount;
        }
    private:
        PullJobTaskResponse(PullJobTaskResponse const &);
        std::string m_workTitle;
        uint32_t m_id;
        uint32_t m_totalIds;
        uint32_t m_sequence;
        double m_taskSize;
        boost::scoped_array<uint8_t> m_taskData;
        size_t m_taskDataSize;
        DatabaseQueryVector m_queries;
        uint32_t m_failureCount;
};

class PushJobTaskResultRequest : public WorkerOutboundMessage
{
    public:
        PushJobTaskResultRequest(std::string const & workerName,
                                 std::string const & workTitle,
                                 uint32_t id,
                                 uint32_t sequence,
                                 double taskSize,
                                 uint8_t const * taskData, size_t taskDataSize,
                                 double elapsedTime,
                                 bool returnValue,
                                 DatabaseQueryVector const & queries,
                                 uint32_t failureCount);
        PushJobTaskResultRequest(std::string const & workerName,
                                 std::string const & workTitle,
                                 uint32_t id,
                                 uint32_t sequence,
                                 double taskSize,
                                 uint8_t const * taskData, size_t taskDataSize,
                                 double elapsedTime,
                                 bool returnValue,
                                 DatabaseQuery const & query,
                                 uint32_t failureCount);
        std::string const & workTitle() const
        {
            return m_workTitle;
        }
        uint32_t id() const
        {
            return m_id;
        }
        boost::scoped_array<uint8_t> const & taskData() const
        {
            return m_taskData;
        }
        size_t taskDataSize() const
        {
            return m_taskDataSize;
        }

    private:
        std::string m_workTitle;
        uint32_t m_id;
        boost::scoped_array<uint8_t> m_taskData;
        size_t m_taskDataSize;
};

class PushJobTaskResultResponse
{
    public:
        PushJobTaskResultResponse(WorkerMessage_t & message);

        // access reponse data

        std::string const & workTitle() const
        {
            return m_workTitle;
        }
        uint32_t id() const
        {
            return m_id;
        }
        boost::scoped_array<uint8_t> const & taskData() const
        {
            return m_taskData;
        }
        size_t taskDataSize() const
        {
            return m_taskDataSize;
        }
    private:
        PushJobTaskResultResponse(PushJobTaskResultResponse const &);
        std::string m_workTitle;
        uint32_t m_id;
        boost::scoped_array<uint8_t> m_taskData;
        size_t m_taskDataSize;
};

} // namespace WorkerProtocol

#endif // WORKER_PROTOCOL_HPP

