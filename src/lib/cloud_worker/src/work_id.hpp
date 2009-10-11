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
#ifndef WORK_ID_HPP
#define WORK_ID_HPP

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

#endif // WORK_ID_HPP

