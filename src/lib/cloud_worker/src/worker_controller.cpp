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
#include <ei.h>
#include <limits>
#include <cmath>
#include <fcntl.h>
#include <poll.h>
#include <unistd.h>
#include <assert.h>
#include <iostream>
#include "worker_controller.hpp"
#include "cloud_worker_common.hpp"
#include "node_connections.hpp"

namespace
{

int Erlang2ASN1(ei_x_buff_ptr & pErlangData,
                realloc_ptr<unsigned char> & pAsn1Data, size_t & asn1Size)
{
    // parse: {'asn1', pid, <<asn1_binary_data>>}
    // in the ei erlang binary format.
    // use separate buffers to avoid making assumptions about
    // how the ei interface will work.
    int index = 0;
    int erlangVersion;
    if (ei_decode_version(pErlangData.buff(), &index, &erlangVersion))
        return WorkerController::ExitStatus::protocol_deserialize_erlang_failed;
    int tupleArity;
    if (ei_decode_tuple_header(pErlangData.buff(), &index, &tupleArity) ||
        tupleArity != 3)
        return WorkerController::ExitStatus::protocol_deserialize_erlang_failed;
    char asn1Atom[MAXATOMLEN];
    if (ei_decode_atom(pErlangData.buff(), &index, asn1Atom) ||
        strcmp(asn1Atom, "asn1") != 0)
        return WorkerController::ExitStatus::protocol_deserialize_erlang_failed;
    erlang_pid srcPid;
    if (ei_decode_pid(pErlangData.buff(), &index, &srcPid))
        return WorkerController::ExitStatus::protocol_deserialize_erlang_failed;
    int erlangType, binarySize;
    if (ei_get_type(pErlangData.buff(), &index, &erlangType, &binarySize))
        return WorkerController::ExitStatus::protocol_deserialize_erlang_failed;
    if (! pAsn1Data.reserve(binarySize))
        return WorkerController::ExitStatus::controller_alloc;
    long tmpAsn1Size;
    if (ei_decode_binary(pErlangData.buff(), &index,
                         pAsn1Data.get<char>(), &tmpAsn1Size))
        return WorkerController::ExitStatus::protocol_deserialize_erlang_failed;
    asn1Size = static_cast<size_t>(tmpAsn1Size);
    return 0;
}

int ASN12ErlangHeader(realloc_ptr<unsigned char> & pErlangData, int & offset)
{
    // create tuple: {'asn1', _, _}
    offset = 0;
    if (ei_encode_version(pErlangData.get<char>(), &offset))
        return WorkerController::ExitStatus::protocol_serialize_erlang_failed;
    if (ei_encode_tuple_header(pErlangData.get<char>(), &offset, 3))
        return WorkerController::ExitStatus::protocol_serialize_erlang_failed;
    if (ei_encode_atom(pErlangData.get<char>(), &offset, "asn1"))
        return WorkerController::ExitStatus::protocol_serialize_erlang_failed;
    return 0;
}

int ASN12ErlangData(realloc_ptr<unsigned char> & pErlangData, int offset,
                    size_t & erlangSize,
                    realloc_ptr<unsigned char> & pAsn1Data, size_t asn1Size,
                    size_t erlangOverheadSize, ei_cnode * ec, int fd)
{
    // complete tuple: {_, pid, <<asn1_binary_data>>}
    erlang_pid * const pErlangPid = ei_self(ec);
    pErlangPid->num = fd;
    if (ei_encode_pid(pErlangData.get<char>(), &offset, pErlangPid))
        return WorkerController::ExitStatus::protocol_serialize_erlang_failed;
    assert(static_cast<unsigned>(offset) < erlangOverheadSize);
    if (! pErlangData.reserve(erlangOverheadSize + asn1Size))
        return WorkerController::ExitStatus::controller_alloc;
    if (ei_encode_binary(pErlangData.get<char>(), &offset,
                         pAsn1Data.get<char>(), asn1Size))
        return WorkerController::ExitStatus::protocol_serialize_erlang_failed;
    erlangSize = static_cast<size_t>(offset);
    return 0;
}

int stderr2Erlang(realloc_ptr<unsigned char> & pErlangData, int & offset,
                  char const * const buffer, size_t size,
                  size_t erlangOverheadSize, ei_cnode * ec, int fd)
{
    // create tuple: {'stderr', pid, "stderr text"}
    offset = 0;
    if (ei_encode_version(pErlangData.get<char>(), &offset))
        return WorkerController::ExitStatus::protocol_serialize_erlang_failed;
    if (ei_encode_tuple_header(pErlangData.get<char>(), &offset, 3))
        return WorkerController::ExitStatus::protocol_serialize_erlang_failed;
    if (ei_encode_atom(pErlangData.get<char>(), &offset, "stderr"))
        return WorkerController::ExitStatus::protocol_serialize_erlang_failed;
    erlang_pid * const pErlangPid = ei_self(ec);
    pErlangPid->num = fd;
    if (ei_encode_pid(pErlangData.get<char>(), &offset, pErlangPid))
        return WorkerController::ExitStatus::protocol_serialize_erlang_failed;
    assert(static_cast<unsigned>(offset) < erlangOverheadSize);
    if (! pErlangData.reserve(erlangOverheadSize + size))
        return WorkerController::ExitStatus::controller_alloc;
    if (ei_encode_string_len(pErlangData.get<char>(), &offset, buffer, size))
        return WorkerController::ExitStatus::protocol_serialize_erlang_failed;
    return 0;
}

} // namespace (anonymous)

uint32_t const WorkerController::WorkId::maxId =
    std::numeric_limits<uint32_t>::max();
size_t const WorkerController::TaskId::maxTaskDataSize =
    std::numeric_limits<size_t>::max();

WorkerController::WorkerController() :
    m_asn1ReceiveBuffer(
        m_asn1InitialReceiveBufferSize, m_asn1MaxReceiveBufferSize),
    m_asn1SendBuffer(
        m_asn1InitialSendBufferSize, m_asn1MaxSendBufferSize),
    m_erlangSendBuffer(
        m_asn1InitialSendBufferSize + m_erlangOverheadSize,
        m_asn1MaxSendBufferSize),
    m_executingTasks(*this),
    m_workerQueries(m_executingTasks)
{
}

WorkerController::~WorkerController()
{
}

int WorkerController::receive(int fd)
{
    erlang_msg msg;
    while (ei_xreceive_msg(fd, &msg, m_erlangReceiveBuffer.get()) != ERL_MSG)
    {
        switch (erl_errno)
        {
            case EAGAIN:
                // Temporary error: Try again. 
                //
                // tick messages get an automatic reply.
                // a tick reply is required within the 
                // net_kernel:set_net_ticktime/1 that
                // is configured. when a tick message is handled
                // EAGAIN is used with erl_errno.
                return 0;
            case EMSGSIZE:
                // Buffer too small. 
                return ExitStatus::controller_alloc;
            case EIO:
                // I/O error.
                // This occurs when the node connected to the cnode dies
                return ExitStatus::node_receive_EIO;
            case ETIMEDOUT:
                // Timed out (should be with ei_receive_tmo() only though)
                return ExitStatus::node_receive_ETIMEDOUT;
            default:
                // unknown badness
                return ExitStatus::node_receive_unknown;
        }
    }
    switch (msg.msgtype)
    {
        case ERL_SEND:
        case ERL_REG_SEND:
            break;
        case ERL_LINK:
            std::cerr << "ei_xreceive_msg got ERL_LINK" << std::endl;
            return 0;
        case ERL_UNLINK:
            std::cerr << "ei_xreceive_msg got ERL_UNLINK" << std::endl;
            return 0;
        case ERL_EXIT:
            std::cerr << "ei_xreceive_msg got ERL_EXIT" << std::endl;
            return 0;
        default:
            std::cerr << "ei_xreceive_msg got msgtype " <<
                msg.msgtype << std::endl;
            return 0;
    };

    size_t asn1Size;
    int status;
    if ((status = Erlang2ASN1(m_erlangReceiveBuffer,
                              m_asn1ReceiveBuffer, asn1Size)))
        return status;

    // deserialize the asn1 message
    WorkerProtocol::WorkerInboundMessage message;
    if (! message.deserialize(m_asn1ReceiveBuffer.get(), asn1Size))
    {
        if (message.deserializeIncomplete())
            return ExitStatus::protocol_deserialize_asn1_incomplete;
        else
            return ExitStatus::protocol_deserialize_asn1_failed;
    }
    if (message.getPullJobTaskResponse())
        handle_message_PullJobTaskResponse(
            message.getPullJobTaskResponse());
    else if (message.getPushJobTaskResultResponse())
        handle_message_PushJobTaskResultResponse(
            message.getPushJobTaskResultResponse());
    else
        return ExitStatus::protocol_unknown_message;
    return 0;
}

// send all pending messages
// (it is possible that messages may be sent more than once
//  due to delays, but that is handled when the response is received)
int WorkerController::send(ei_cnode * ec, struct pollfd const * pfds, int nfds,
                           bool retransmit)
{
    boost::lock_guard<boost::mutex> lock(m_workerQueries.resultsMutex());
    int status = 0; // first error encountered
    if (nfds > 0 && ! m_workerQueries.empty())
    {
        int erlangSendBufferIndex;
        status = ASN12ErlangHeader(m_erlangSendBuffer, erlangSendBufferIndex);

        WorkerQuery::const_iterator itr = m_workerQueries.begin();
        WorkerQuery::const_iterator const end = m_workerQueries.end();
        for (int i = 0; itr != end && status == 0; ++itr)
        {
            WorkerProtocol::WorkerOutboundMessage const * p = *itr;
            if (! retransmit && p->wasSent())
                continue;
            size_t asn1Size, erlangSize;
            if (! p->valid())
                status = ExitStatus::protocol_serialize_asn1_copy;
            else if ((asn1Size = p->serialize(m_asn1SendBuffer)) == 0)
                status = ExitStatus::protocol_serialize_asn1_failed;
            else
                status = ASN12ErlangData(
                    m_erlangSendBuffer, erlangSendBufferIndex, erlangSize,
                    m_asn1SendBuffer, asn1Size, 
                    m_erlangOverheadSize, ec, pfds[i].fd);
            if (status == 0)
            {
                if (ei_reg_send(ec, pfds[i].fd,
                                const_cast<char *>(p->destination()),
                                m_erlangSendBuffer.get<char>(),
                                erlangSize) != 0)
                {
                    if (erl_errno == EIO)
                        // I/O error.
                        status = ExitStatus::node_send_EIO;
                    else
                        status = ExitStatus::node_send_unknown;
                }
                else
                {
                    p->sent();
                }
            }
            if (++i == nfds)
                i = 0;
        }
    }
    m_workerQueries.flush_events();
    return status;
}

int WorkerController::sendStderr(ei_cnode * ec, struct pollfd const & pfd,
                                 char const * const buffer, size_t size)
{
    int totalSize;
    int status = stderr2Erlang(m_erlangSendBuffer, totalSize, buffer, size,
                               m_erlangOverheadSize, ec, pfd.fd);
    if (status)
        return status;
    char * stderrProcess = 
        const_cast<char *>(NodeConnections::parentProcessName().c_str());
    if (ei_reg_send(ec, pfd.fd, stderrProcess,
                    m_erlangSendBuffer.get<char>(), totalSize) != 0)
    {
        if (erl_errno == EIO)
            // I/O error.
            return ExitStatus::node_send_EIO;
        else
            return ExitStatus::node_send_unknown;
    }
    return 0;
}

void WorkerController::handle_message_PullJobTaskResponse(
    PullJobTaskResponseType & ptr)
{
    if (ptr->empty())
        return;
    m_workerQueries.received(ptr);
}

void WorkerController::handle_message_PushJobTaskResultResponse(
    PushJobTaskResultResponseType & ptr)
{
    m_workerQueries.received(ptr);
}

void WorkerController::setCurrentPath(char const * const pPath)
{
    m_currentPath = std::string(pPath);
    m_currentPath.remove_filename();
}

WorkerController::WorkerQuery::WorkerQuery(WorkerExecution & executingTasks) :
    m_receiveTimeout(-1),
    m_pRequestAllocator(new PullJobTaskRequestType::pool_type),
    m_executingTasks(executingTasks)
{
    int pipeData[2];
    bool success = (pipe(pipeData) == 0);
    int flags;
    if (success)
    {
        flags = fcntl(pipeData[0], F_GETFL, 0);
        success = (flags != -1);
    }
    if (success)
    {
        success = (fcntl(pipeData[0], F_SETFL, flags | O_NONBLOCK) == 0);
    }
    if (success)
    {
        m_eventPipe[0] = pipeData[0];
        m_eventPipe[1] = pipeData[1];
    }
    else
    {
        m_eventPipe[0] = -1;
        m_eventPipe[1] = -1;
    }
}

WorkerController::WorkerQuery::~WorkerQuery()
{
    boost::lock_guard<boost::mutex> lock(m_resultsMutex);
    m_pendingRequests.clear();
    m_pendingResults.clear();
}

bool WorkerController::WorkerQuery::add(
    std::string const & workTitle, uint32_t idOffset, uint32_t numberOfThreads)
{
    // add tasks to request
    bool success = true;
    for (uint32_t i = 0; i < numberOfThreads && success; ++i)
    {
        // allow the addition of all worker task requests before
        // declaring success or failure.  it is up to the controlling
        // erlang node to not replicate worker task requests
        // (e.g., not add a worker task request that is already executing)
        typedef std::pair<WorkId, PullJobTaskRequestType> RequestPair;
        uint32_t const id = idOffset + i;
        PullJobTaskRequestType pRequest(m_pRequestAllocator);
        pRequest(NodeConnections::nodeName(), workTitle, id);
        std::pair<PendingRequestsLookup::iterator, bool> requestResult =
            m_pendingRequests.insert(RequestPair(
                WorkId(workTitle, id), pRequest
            ));
        if (! requestResult.second)
            success = false;
    }
    if (success)
    {
        m_executingTasks.increaseCapacity(numberOfThreads);
        trigger_event();
    }
    else
    {
        m_pendingRequests.erase(WorkId(workTitle));
    }
    return success;
}

// check if there is work as specified
bool WorkerController::WorkerQuery::has(
    std::string const & workTitle, uint32_t idOffset, uint32_t numberOfThreads)
{
    std::vector<WorkId> taskArray;
    taskArray.reserve(numberOfThreads + 2);
    for (uint32_t i = 0; i < numberOfThreads; ++i)
        taskArray.push_back(WorkId(workTitle, idOffset + i));
    if (idOffset > 0)
        taskArray.push_back(WorkId(workTitle, idOffset - 1));
    taskArray.push_back(WorkId(workTitle, idOffset + numberOfThreads));

    boost::lock_guard<boost::mutex> lock(m_resultsMutex);
    uint32_t tasksFound =
        m_executingTasks.count(taskArray, m_pendingRequests, m_pendingResults);
    return (tasksFound == numberOfThreads);
}

// remove all tasks related to a type of work
// return true if any work was removed, else false
bool WorkerController::WorkerQuery::remove(
    std::string const & workTitle)
{
    boost::lock_guard<boost::mutex> lock(m_resultsMutex);
    return (m_executingTasks.erase(
        WorkId(workTitle), m_pendingRequests, m_pendingResults) > 0);
}

void WorkerController::WorkerQuery::clear()
{
    boost::lock_guard<boost::mutex> lock(m_resultsMutex);
    m_executingTasks.clear();
    m_pendingRequests.clear();
    m_pendingResults.clear();
}

bool WorkerController::WorkerQuery::add(
    PushJobTaskResultRequestType & ptr)
{
    boost::lock_guard<boost::mutex> lock(m_resultsMutex);
    // save the result
    typedef std::pair<TaskId, PushJobTaskResultRequestType> RequestPair;
    std::pair<PendingResultsLookup::iterator, bool> requestResult =
        m_pendingResults.insert(RequestPair(
            TaskId(ptr->workTitle(), ptr->id(),
                   ptr->taskData(), ptr->taskDataSize()),
            ptr
        ));
    if (requestResult.second)
    {
        trigger_event();
        return true;
    }
    else
    {
        return false;
    }
}

void WorkerController::WorkerQuery::received(
    PullJobTaskResponseType & ptr)
{
    // remove pointer from pending map
    size_t const removedCount = m_pendingRequests.erase(
        WorkId(ptr->workTitle(), ptr->id()));
    if (removedCount > 0)
    {
        // use response information to put task in executing tasks
        // (only if the request was found should the
        //  response then be added as a task to execute)
        assert(! ptr->empty());
        m_executingTasks.input(ptr);
    }
}

void WorkerController::WorkerQuery::received(
    PushJobTaskResultResponseType & ptr)
{
    // remove task from m_pendingResults
    size_t removedCount;
    {
        boost::lock_guard<boost::mutex> lock(m_resultsMutex);
        removedCount = m_pendingResults.erase(
            TaskId(ptr->workTitle(), ptr->id(),
                   ptr->taskData(), ptr->taskDataSize()));
    }
    if (removedCount > 0)
    {
        // use response information to put work in a queue 
        // to query for another task
        // (only if the request was found should the
        //  response then be added as work to request)
        typedef std::pair<WorkId, PullJobTaskRequestType> RequestPair;
        PullJobTaskRequestType pRequest(m_pRequestAllocator);
        pRequest(NodeConnections::nodeName(), ptr->workTitle(), ptr->id());
        std::pair<PendingRequestsLookup::iterator, bool> requestResult =
            m_pendingRequests.insert(RequestPair(
                WorkId(ptr->workTitle(), ptr->id()), pRequest
            ));
        assert(requestResult.second);
        trigger_event();
    }
}

// determine the timeout value for each execution of the poll() system call
int WorkerController::WorkerQuery::timeout_value()
{
    if (! NodeConnections::is_initialized())
    {
        m_receiveTimeout = -1;
        return m_receiveTimeout;
    }
    else if (m_receiveTimeout == -1)
    {
        boost::lock_guard<boost::mutex> lock(m_resultsMutex);
        if (empty())
        {
            // make sure events are flushed
            flush_events();
            // wait for items to send or receive
            return m_receiveTimeout;
        }
        else
        {
            // wait for message resends
            m_receiveTimeout = m_maxReceiveTimeout;
            m_receiveTimeoutTimer.restart();
            return m_receiveTimeout;
        }
    }
    int const elapsed = static_cast<int>(
        round(m_receiveTimeoutTimer.elapsed() * 1e3));
    if (elapsed >= m_receiveTimeout)
    {
        // trigger resend of all messages
        // (m_maxReceiveTimeout milliseconds elapsed)
        trigger_event();
        m_receiveTimeout = -1;
    }
    else
    {
        m_receiveTimeout -= elapsed;
    }
    return m_receiveTimeout;
}

