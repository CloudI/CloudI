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

#include "library.hpp"
#include "copy_ptr.hpp"
#include <dlfcn.h>
#include <iostream>
#include <sstream>

class library_dlsym_failed : public std::exception
{
    public:
        library_dlsym_failed(char const * const pError)
        {
            std::ostringstream s;
            s << "dlsym() failed: " << pError;
            m_pMessage.reset(new std::string(s.str()));
        }
        virtual ~library_dlsym_failed() throw () {}
        char const * what() const throw () { return m_pMessage->c_str(); }
    private:
        copy_ptr<std::string> m_pMessage;

};

// take symbols from the library and avoid symbols in the
// cloud_worker_port Erlang port binary
// (available since glibc 2.3.4, but not POSIX)
#ifndef RTLD_DEEPBIND
#define RTLD_DEEPBIND 0
#endif

library::library(std::string const & name) :
    m_handle(dlopen(name.c_str(), RTLD_NOW | RTLD_LOCAL | RTLD_DEEPBIND))
{
}

library::~library()
{
    if (m_handle)
        dlclose(m_handle);
}

char * library::error() const
{
    return dlerror();
}

void * library::get_function(char const * const pName)
{
    dlerror();
    void * pFunction = dlsym(m_handle, pName);
    char const * const pError = dlerror();
    if (pError)
        throw library_dlsym_failed(pError);
    return pFunction;
}

