//-*-Mode:C++;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=cpp fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
//
// MIT License
//
// Copyright (c) 2011-2020 Michael Truog <mjtruog at protonmail dot com>
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.
//

#include <sstream>
#include <exception>
#include "cloudi.hpp"
#include "assert.hpp"

namespace boost
{
    void assertion_failed_msg(char const * expr, char const * function,        
                              char const * file, char const * mm, long line)
    {  
        class assert_exception_msg : public CloudI::API::fatal_error
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
        class assert_exception : public CloudI::API::fatal_error
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

