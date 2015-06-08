//-*-Mode:C++;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=cpp fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
//
// BSD LICENSE
// 
// Copyright (c) 2011-2012, Michael Truog <mjtruog at gmail dot com>
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

#include <sstream>
#include <exception>
#include "assert.hpp"

namespace boost
{
    void assertion_failed_msg(char const * expr, char const * function,        
                              char const * file, char const * mm, long line)
    {  
        class assert_exception_msg : public std::exception
        {
            public:
                assert_exception_msg(std::string const & message) throw () :
                    m_message(message)
                {
                }
                virtual ~assert_exception_msg() throw ()
                {
                }
                virtual char const * what() const throw ()
                {
                    return m_message.c_str();
                }
            private:
                std::string m_message;
        };
        std::ostringstream stream;
        stream << file << ":" << line <<
            " (" << function << ") failure: " << expr << ": " << mm;
        throw assert_exception_msg(stream.str());
    }                                                                          
   

    void assertion_failed(char const * expr,
                          char const * function,
                          char const * file,
                          long line)
    {
        class assert_exception : public std::exception
        {
            public:
                assert_exception(std::string const & message) throw () :
                    m_message(message)
                {
                }
                virtual ~assert_exception() throw ()
                {
                }
                virtual char const * what() const throw ()
                {
                    return m_message.c_str();
                }
            private:
                std::string m_message;
        };
        std::ostringstream stream;
        stream << file << ":" << line <<
            " (" << function << ") failure: " << expr;
        throw assert_exception(stream.str());
    }
}

