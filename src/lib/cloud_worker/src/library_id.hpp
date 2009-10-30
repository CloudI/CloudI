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
#ifndef LIBRARY_ID_HPP
#define LIBRARY_ID_HPP

class WorkerController::WorkerExecution::LibraryId
{
    private:
        static char const invalidInstanceChar = 'x';
    public:
        static LibraryId create(std::string const & workTitle)
        {
            size_t const tagIndex = workTitle.find_first_of('.');
            if (tagIndex == std::string::npos)
            {
                return LibraryId(workTitle);
            }
            else
            {
                std::string workLibrary(workTitle.substr(0, tagIndex));
                std::string workInstance(workTitle.substr(tagIndex));
                return LibraryId(workLibrary, workInstance);
            }
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
        std::string const & workInstance() const { return m_workInstance; }
    private:
        std::string m_workLibrary;
        std::string m_workInstance;
};

#endif // LIBRARY_ID_HPP

