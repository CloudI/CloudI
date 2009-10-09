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
#include "worker_protocol.hpp"
#include "realloc_ptr.hpp"
#include "WorkerMessage.h"
#include <cstdlib>
#include <iostream>
#include <asn_application.h>
#include <ber_decoder.h>
#include <der_encoder.h>

namespace WorkerProtocol
{

// helper functions for serialization/deserialization

namespace
{

// ASN.1 REAL type is not worth using, since
// asn1c supports 2, 8, 16 bases
// erlang supports 2, 10 bases
// which limits the precision by depending only on base 2
// so, make sure to use doubles converted to strings in scientific notation

size_t double2string(double x, char * p, size_t size)
{
    assert(size >= 64);
    return static_cast<size_t>(snprintf(p, size, "%.35e", x));
}

double string2double(char const * p)
{
    return strtod(p, 0);
}

typedef struct der_encode_to_realloc_buffer_arguments
{
    // track the current buffer position
    void * p;
    size_t left;
    // buffer memory to realloc if necessary
    realloc_ptr<unsigned char> &buffer;

} der_encode_to_realloc_buffer_arguments;

int der_encode_to_realloc_buffer_callback(
    const void *buffer, size_t size, void *key)
{
    der_encode_to_realloc_buffer_arguments *args = 
        reinterpret_cast<der_encode_to_realloc_buffer_arguments *>(key);

    if (args->left < size)
    {
        size_t const offset =
            reinterpret_cast<size_t>(args->p) -
            reinterpret_cast<size_t>(args->buffer.get());
        if (! args->buffer.reserve(offset + size))
            return -1;
        args->p = &(args->buffer.get()[offset]);
        args->left = args->buffer.size() - offset;
    }

    memcpy(args->p, buffer, size);
    args->p = static_cast<char *>(args->p) + size;
    args->left -= size;
    return 0;
}

ssize_t der_serialize(WorkerMessage_t const *message,
    realloc_ptr<unsigned char> &buffer)
{
    der_encode_to_realloc_buffer_arguments args = {
        buffer.get(), buffer.size(), buffer};
    asn_enc_rval_t ec = asn_DEF_WorkerMessage.der_encoder(
        &asn_DEF_WorkerMessage,
        reinterpret_cast<void *>(const_cast<WorkerMessage_t *>(message)),
        0, 0, der_encode_to_realloc_buffer_callback, &args);
    assert(ec.encoded == -1 ||
           ec.encoded == static_cast<ssize_t>(buffer.size() - args.left));
    return ec.encoded; // -1 or bytes encoded
}

bool ber_deserialize(WorkerMessage_t * pMessage,
    bool & incomplete, void const * buffer, size_t buffer_size)
{
    asn_dec_rval_t rval = ber_decode(0, &asn_DEF_WorkerMessage,
        reinterpret_cast<void **>(&pMessage), buffer, buffer_size);
    if (rval.code == RC_OK)
    {
        incomplete = false;
        return true;
    }
    else
    {
        incomplete = (rval.code == RC_WMORE);
        return false;
    }
}

} // namespace (anonymous)

// wrapper classes for external protocol interface
// (not automatically generated)

WorkerOutboundMessage::WorkerOutboundMessage() :
    m_message(*(reinterpret_cast<WorkerMessage_t *>(m_pMessageData))),
    m_valid(true), m_pProcessName("cloud_work_manager"), m_sent(false)
{
    assert(SIZEOF_WORKERMESSAGE_T == sizeof(WorkerMessage_t));
    assert(sizeof(m_pMessageData) == sizeof(WorkerMessage_t));
    memset(&m_message, 0, sizeof(WorkerMessage_t));
}

WorkerOutboundMessage::~WorkerOutboundMessage()
{
    int const free_contents_only = 1;
    asn_DEF_WorkerMessage.free_struct(
        &asn_DEF_WorkerMessage, &m_message, free_contents_only);
}

size_t WorkerOutboundMessage::serialize(
    realloc_ptr<unsigned char> &buffer) const
{
    ssize_t const bytes = der_serialize(&m_message, buffer);
    if (bytes < 0)
        return 0;
    else
        return static_cast<size_t>(bytes);
}

safe_shared_ptr< safe_object_pool<PullJobTaskResponse> >
    WorkerInboundMessage::m_pRequestsPool(
        new safe_object_pool<PullJobTaskResponse>());
safe_shared_ptr< boost::object_pool<PushJobTaskResultResponse> >
    WorkerInboundMessage::m_pResultsPool(
        new boost::object_pool<PushJobTaskResultResponse>());

WorkerInboundMessage::WorkerInboundMessage() :
    m_incomplete(false),
    m_pPullJobTaskResponse(m_pRequestsPool),
    m_pPushJobTaskResultResponse(m_pResultsPool)
{
}

bool WorkerInboundMessage::deserialize(void const * buffer, size_t bufferSize)
{
    bool returnValue = true;
    WorkerMessage_t message;
    memset(&message, 0, sizeof(WorkerMessage_t));
    if (ber_deserialize(&message, m_incomplete, buffer, bufferSize))
    {
        switch (message.present)
        {
            case WorkerMessage_PR_pullJobTaskResponse:
                m_pPullJobTaskResponse(message);
                break;
            case WorkerMessage_PR_pushJobTaskResultResponse:
                m_pPushJobTaskResultResponse(message);
                break;
            default:
                std::cerr << "destroyed asn1 WorkerMessage id " << 
                    static_cast<int>(message.present) << std::endl;
                returnValue = false;
                break;
        }
    }
    else
    {
        returnValue = false;
    }
    int const free_contents_only = 1;
    asn_DEF_WorkerMessage.free_struct(
        &asn_DEF_WorkerMessage, &message, free_contents_only);
    return returnValue;
}

#define CHECK_VALID(STATEMENT) if (STATEMENT < 0) { m_valid = false; return; }
#define RETURN_VALID(STATEMENT) if (STATEMENT < 0) { return -1; }

PullJobTaskRequest::PullJobTaskRequest(
    std::string const & workerName,
    std::string const & workTitle,
    uint32_t id)
{
    m_message.present = WorkerMessage_PR_pullJobTaskRequest;
    CHECK_VALID(OCTET_STRING_fromBuf(
        &(m_message.choice.pullJobTaskRequest.workerName),
        workerName.c_str(), workerName.size()))
    CHECK_VALID(OCTET_STRING_fromBuf(
        &(m_message.choice.pullJobTaskRequest.workTitle),
        workTitle.c_str(), workTitle.size()))
    CHECK_VALID(asn_long2INTEGER(
        &(m_message.choice.pullJobTaskRequest.id), id))
}


PullJobTaskResponse::PullJobTaskResponse(WorkerMessage_t & message) :
    m_taskDataSize(0)
{
    m_workTitle.assign(reinterpret_cast<char *>(
        message.choice.pullJobTaskResponse.workTitle.buf),
        message.choice.pullJobTaskResponse.workTitle.size);
    long id = -1;
    asn_INTEGER2long(&(message.choice.pullJobTaskResponse.id), &id);
    m_id = static_cast<uint32_t>(id & 0xffffffff);
    long totalIds = -1;
    asn_INTEGER2long(&(message.choice.pullJobTaskResponse.totalIds), &totalIds);
    m_totalIds = static_cast<uint32_t>(totalIds & 0xffffffff);
    long sequence = -1;
    asn_INTEGER2long(&(message.choice.pullJobTaskResponse.sequence), &sequence);
    m_sequence = static_cast<uint32_t>(sequence & 0xffffffff);
    m_taskSize = string2double(reinterpret_cast<char *>(
        message.choice.pullJobTaskResponse.taskSize.buf));
    if (message.choice.pullJobTaskResponse.taskData.size > 0)
    {
        m_taskDataSize = static_cast<size_t>(
            message.choice.pullJobTaskResponse.taskData.size);
        m_taskData.reset(new uint8_t[m_taskDataSize]);
        memcpy(reinterpret_cast<void *>(m_taskData.get()),
            reinterpret_cast<void *>(
            message.choice.pullJobTaskResponse.taskData.buf), m_taskDataSize);
    }
    int const queriesCount =
        message.choice.pullJobTaskResponse.queries.list.count;
    for (int i = 0; i < queriesCount; ++i)
    {
        Query_t const * pQuery = 
            message.choice.pullJobTaskResponse.queries.list.array[i];
        m_queries.push_back(DatabaseQuery(
            std::string(reinterpret_cast<char const *>(pQuery->type.buf),
                pQuery->type.size),
            std::string(reinterpret_cast<char const *>(pQuery->query.buf),
                pQuery->query.size)
            ));
    }
    long failureCount = -1;
    asn_INTEGER2long(
        &(message.choice.pullJobTaskResponse.failureCount), &failureCount);
    m_failureCount = static_cast<uint32_t>(failureCount & 0xffffffff);
}

static int PushJobTaskResultRequestInitialization(WorkerMessage_t & message,
    std::string const & workerName,
    std::string const & workTitle,
    uint32_t id,
    uint32_t sequence,
    double taskSize,
    uint8_t const * taskData, size_t taskDataSize,
    double elapsedTime,
    bool returnValue,
    uint32_t failureCount)
{
    message.present = WorkerMessage_PR_pushJobTaskResultRequest;
    RETURN_VALID(OCTET_STRING_fromBuf(
        &(message.choice.pushJobTaskResultRequest.workerName),
        workerName.c_str(), workerName.size()))
    RETURN_VALID(OCTET_STRING_fromBuf(
        &(message.choice.pushJobTaskResultRequest.workTitle),
        workTitle.c_str(), workTitle.size()))
    RETURN_VALID(asn_long2INTEGER(
        &(message.choice.pushJobTaskResultRequest.id), id))
    RETURN_VALID(asn_long2INTEGER(
        &(message.choice.pushJobTaskResultRequest.sequence), sequence))
    char strTaskSize[64];
    RETURN_VALID(OCTET_STRING_fromBuf(
        &(message.choice.pushJobTaskResultRequest.taskSize), strTaskSize,
        double2string(taskSize, strTaskSize, sizeof(strTaskSize))));
    RETURN_VALID(OCTET_STRING_fromBuf(
        &(message.choice.pushJobTaskResultRequest.taskData),
        reinterpret_cast<char const *>(taskData), taskDataSize))
    char strElapsedTime[64];
    RETURN_VALID(OCTET_STRING_fromBuf(
        &(message.choice.pushJobTaskResultRequest.elapsedTime), strElapsedTime,
        double2string(elapsedTime, strElapsedTime, sizeof(strElapsedTime))));
    message.choice.pushJobTaskResultRequest.returnValue = 
        (returnValue ? 0xff : 0);
    RETURN_VALID(asn_long2INTEGER(
        &(message.choice.pushJobTaskResultRequest.failureCount), failureCount))
    return 0;
}

PushJobTaskResultRequest::PushJobTaskResultRequest(
    std::string const & workerName,
    std::string const & workTitle,
    uint32_t id,
    uint32_t sequence,
    double taskSize,
    uint8_t const * taskData, size_t taskDataSize,
    double elapsedTime,
    bool returnValue,
    DatabaseQueryVector const & queries,
    uint32_t failureCount) :
        m_workTitle(workTitle), m_id(id), m_taskDataSize(taskDataSize)
{
    // keep some input parameters for internal processing
    if (m_taskDataSize > 0)
    {
        m_taskData.reset(new uint8_t[m_taskDataSize]);
        memcpy(reinterpret_cast<void *>(m_taskData.get()),
            reinterpret_cast<void const *>(taskData), m_taskDataSize);
    }
    // create request
    CHECK_VALID(PushJobTaskResultRequestInitialization(m_message,
        workerName, workTitle, id, sequence, taskSize, taskData, 
        taskDataSize, elapsedTime, returnValue, failureCount))
    DatabaseQueryVector::const_iterator itr;
    for (itr = queries.begin(); itr != queries.end(); ++itr)
    {
        Query_t * pQuery = reinterpret_cast<Query_t *>(
            calloc(1, sizeof(Query_t)));
        CHECK_VALID(OCTET_STRING_fromBuf(
            &(pQuery->type), itr->type().c_str(), itr->type().size()))
        CHECK_VALID(OCTET_STRING_fromBuf(
            &(pQuery->query), itr->query().c_str(), itr->query().size()))
        CHECK_VALID(ASN_SEQUENCE_ADD(
            &(m_message.choice.pushJobTaskResultRequest.queries.list), pQuery))
    }
}

PushJobTaskResultRequest::PushJobTaskResultRequest(
    std::string const & workerName,
    std::string const & workTitle,
    uint32_t id,
    uint32_t sequence,
    double taskSize,
    uint8_t const * taskData, size_t taskDataSize,
    double elapsedTime,
    bool returnValue,
    DatabaseQuery const & query,
    uint32_t failureCount) :
        m_workTitle(workTitle), m_id(id), m_taskDataSize(taskDataSize)
{
    // keep some input parameters for internal processing
    if (m_taskDataSize > 0)
    {
        m_taskData.reset(new uint8_t[m_taskDataSize]);
        memcpy(reinterpret_cast<void *>(m_taskData.get()),
            reinterpret_cast<void const *>(taskData), m_taskDataSize);
    }
    // create request
    CHECK_VALID(PushJobTaskResultRequestInitialization(m_message,
        workerName, workTitle, id, sequence, taskSize, taskData, 
        taskDataSize, elapsedTime, returnValue, failureCount))
    Query_t * pQuery = reinterpret_cast<Query_t *>(calloc(1, sizeof(Query_t)));
    CHECK_VALID(OCTET_STRING_fromBuf(
        &(pQuery->type), query.type().c_str(), query.type().size()))
    CHECK_VALID(OCTET_STRING_fromBuf(
        &(pQuery->query), query.query().c_str(), query.query().size()))
    CHECK_VALID(ASN_SEQUENCE_ADD(
        &(m_message.choice.pushJobTaskResultRequest.queries.list), pQuery))
}

PushJobTaskResultResponse::PushJobTaskResultResponse(
    WorkerMessage_t & message) : m_taskDataSize(0)
{
    m_workTitle.assign(reinterpret_cast<char *>(
        message.choice.pushJobTaskResultResponse.workTitle.buf),
        message.choice.pushJobTaskResultResponse.workTitle.size);
    long id = -1;
    asn_INTEGER2long(&(message.choice.pushJobTaskResultResponse.id), &id);
    m_id = static_cast<uint32_t>(id & 0xffffffff);
    if (message.choice.pushJobTaskResultResponse.taskData.size > 0)
    {
        m_taskDataSize = static_cast<size_t>(
            message.choice.pushJobTaskResultResponse.taskData.size);
        m_taskData.reset(new uint8_t[m_taskDataSize]);
        memcpy(reinterpret_cast<void *>(m_taskData.get()),
            reinterpret_cast<void *>(
            message.choice.pushJobTaskResultResponse.taskData.buf),
            m_taskDataSize);
    }
}

} // namespace WorkerProtocol

