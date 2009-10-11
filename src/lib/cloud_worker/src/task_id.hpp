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
#ifndef TASK_ID_HPP
#define TASK_ID_HPP

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

#endif // TASK_ID_HPP

