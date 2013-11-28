//-*-Mode:C++;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=cpp fenc=utf-8 sts=4 ts=4 sw=4 et:
//
// BSD LICENSE
// 
// Copyright (c) 2011-2013, Michael Truog <mjtruog at gmail dot com>
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

#ifdef CLOUDI_H
#warning "Only need to include cloudi.hpp (C++), not cloudi.h (C)"
#endif
#ifndef CLOUDI_HPP
#define CLOUDI_HPP

#include <stdint.h>
#include <string>

#define CLOUDI_MAX_BUFFERSIZE 2147483648U /* 2GB */

typedef struct cloudi_instance_t cloudi_instance_t;

namespace CloudI
{

class API
{
    public:
        class function_object_cxx_const
        {
            public:
                virtual ~function_object_cxx_const() throw() {}
                virtual void operator() (API const &,
                                         int const,
                                         std::string const &,
                                         std::string const &,
                                         void const * const,
                                         uint32_t const,
                                         void const * const,
                                         uint32_t const,
                                         uint32_t,
                                         int8_t,
                                         char const * const,
                                         char const * const,
                                         uint32_t const) const = 0;
        };

        class function_object_c_const
        {
            public:
                virtual ~function_object_c_const() throw() {}
                virtual void operator() (API const &,
                                         int const,
                                         char const * const,
                                         char const * const,
                                         void const * const,
                                         uint32_t const,
                                         void const * const,
                                         uint32_t const,
                                         uint32_t,
                                         int8_t,
                                         char const * const,
                                         char const * const,
                                         uint32_t const) const = 0;
        };

        class function_object_cxx
        {
            public:
                virtual ~function_object_cxx() throw() {}
                virtual void operator() (API const &,
                                         int const,
                                         std::string const &,
                                         std::string const &,
                                         void const * const,
                                         uint32_t const,
                                         void const * const,
                                         uint32_t const,
                                         uint32_t,
                                         int8_t,
                                         char const * const,
                                         char const * const,
                                         uint32_t const) = 0;
        };

        class function_object_c
        {
            public:
                virtual ~function_object_c() throw() {}
                virtual void operator() (API const &,
                                         int const,
                                         char const * const,
                                         char const * const,
                                         void const * const,
                                         uint32_t const,
                                         void const * const,
                                         uint32_t const,
                                         uint32_t,
                                         int8_t,
                                         char const * const,
                                         char const * const,
                                         uint32_t const) = 0;
        };

    public:
        class callback_function_generic
        {
            public:
                virtual ~callback_function_generic() throw() {}
                virtual void operator () (int const,
                                          char const * const,
                                          char const * const,
                                          void const * const,
                                          uint32_t const,
                                          void const * const,
                                          uint32_t const,
                                          uint32_t,
                                          int8_t,
                                          char const * const,
                                          char const * const,
                                          uint32_t const) = 0;
        };

    private:
        class callback_function_cxx_or1 : public callback_function_generic
        {
            public:
                callback_function_cxx_or1(function_object_cxx_const const &
                                          object,
                                          API const * api) :
                    m_object(object), m_api(api) {}
                virtual ~callback_function_cxx_or1() throw()
                {
                    delete m_api;
                }

                virtual void operator () (int const command,
                                          char const * const name,
                                          char const * const pattern,
                                          void const * const request_info,
                                          uint32_t const request_info_size,
                                          void const * const request,
                                          uint32_t const request_size,
                                          uint32_t timeout,
                                          int8_t priority,
                                          char const * const trans_id,
                                          char const * const pid,
                                          uint32_t const pid_size)
                {
                    m_object(*m_api,
                             command,
                             std::string(name),
                             std::string(pattern),
                             request_info,
                             request_info_size,
                             request,
                             request_size,
                             timeout,
                             priority,
                             trans_id,
                             pid,
                             pid_size);
                }
            private:
                function_object_cxx_const const & m_object;
                API const * m_api;
        };

        class callback_function_cxx_or2 : public callback_function_generic
        {
            public:
                callback_function_cxx_or2(function_object_c_const const &
                                          object,
                                          API const * api) :
                    m_object(object), m_api(api) {}
                virtual ~callback_function_cxx_or2() throw()
                {
                    delete m_api;
                }

                virtual void operator () (int const command,
                                          char const * const name,
                                          char const * const pattern,
                                          void const * const request_info,
                                          uint32_t const request_info_size,
                                          void const * const request,
                                          uint32_t const request_size,
                                          uint32_t timeout,
                                          int8_t priority,
                                          char const * const trans_id,
                                          char const * const pid,
                                          uint32_t const pid_size)
                {
                    m_object(*m_api,
                             command,
                             name,
                             pattern,
                             request_info,
                             request_info_size,
                             request,
                             request_size,
                             timeout,
                             priority,
                             trans_id,
                             pid,
                             pid_size);
                }
            private:
                function_object_c_const const & m_object;
                API const * m_api;
        };

        class callback_function_cxx_op1 : public callback_function_generic
        {
            public:
                callback_function_cxx_op1(function_object_cxx * object,
                                          API const * api) :
                    m_object(object), m_api(api) {}
                virtual ~callback_function_cxx_op1() throw()
                {
                    delete m_object;
                    delete m_api;
                }

                virtual void operator () (int const command,
                                          char const * const name,
                                          char const * const pattern,
                                          void const * const request_info,
                                          uint32_t const request_info_size,
                                          void const * const request,
                                          uint32_t const request_size,
                                          uint32_t timeout,
                                          int8_t priority,
                                          char const * const trans_id,
                                          char const * const pid,
                                          uint32_t const pid_size)
                {
                    (*m_object)(*m_api,
                                command,
                                std::string(name),
                                std::string(pattern),
                                request_info,
                                request_info_size,
                                request,
                                request_size,
                                timeout,
                                priority,
                                trans_id,
                                pid,
                                pid_size);
                }
            private:
                function_object_cxx * const m_object;
                API const * m_api;
        };

        class callback_function_cxx_op2 : public callback_function_generic
        {
            public:
                callback_function_cxx_op2(function_object_c * object,
                                          API const * api) :
                    m_object(object), m_api(api) {}
                virtual ~callback_function_cxx_op2() throw()
                {
                    delete m_object;
                    delete m_api;
                }

                virtual void operator () (int const command,
                                          char const * const name,
                                          char const * const pattern,
                                          void const * const request_info,
                                          uint32_t const request_info_size,
                                          void const * const request,
                                          uint32_t const request_size,
                                          uint32_t timeout,
                                          int8_t priority,
                                          char const * const trans_id,
                                          char const * const pid,
                                          uint32_t const pid_size)
                {
                    (*m_object)(*m_api,
                                command,
                                name,
                                pattern,
                                request_info,
                                request_info_size,
                                request,
                                request_size,
                                timeout,
                                priority,
                                trans_id,
                                pid,
                                pid_size);
                }
            private:
                function_object_c * const m_object;
                API const * m_api;
        };

        template <typename T>
        class callback_function_cxx_m1 : public callback_function_generic
        {
            public:
                callback_function_cxx_m1(T & object,
                                         API const * api,
                                         void (T::*f) (API const &,
                                                       int const,
                                                       std::string const &,
                                                       std::string const &,
                                                       void const * const,
                                                       uint32_t const,
                                                       void const * const,
                                                       uint32_t const,
                                                       uint32_t,
                                                       int8_t,
                                                       char const * const,
                                                       char const * const,
                                                       uint32_t const)) :
                    m_object(object), m_api(api), m_f(f) {}
                virtual ~callback_function_cxx_m1() throw()
                {
                    delete m_api;
                }

                virtual void operator () (int const command,
                                          char const * const name,
                                          char const * const pattern,
                                          void const * const request_info,
                                          uint32_t const request_info_size,
                                          void const * const request,
                                          uint32_t const request_size,
                                          uint32_t timeout,
                                          int8_t priority,
                                          char const * const trans_id,
                                          char const * const pid,
                                          uint32_t const pid_size)
                {
                    (m_object.*m_f)(*m_api,
                                    command,
                                    std::string(name),
                                    std::string(pattern),
                                    request_info,
                                    request_info_size,
                                    request,
                                    request_size,
                                    timeout,
                                    priority,
                                    trans_id,
                                    pid,
                                    pid_size);
                }
            private:
                T & m_object;
                API const * m_api;
                void (T::*m_f) (API const &,
                                int const,
                                std::string const &,
                                std::string const &,
                                void const * const,
                                uint32_t const,
                                void const * const,
                                uint32_t const,
                                uint32_t,
                                int8_t,
                                char const * const,
                                char const * const,
                                uint32_t const);
        };

        template <typename T>
        class callback_function_cxx_m2 : public callback_function_generic
        {
            public:
                callback_function_cxx_m2(T & object,
                                         API const * api,
                                         void (T::*f) (API const &,
                                                       int const,
                                                       char const * const,
                                                       char const * const,
                                                       void const * const,
                                                       uint32_t const,
                                                       void const * const,
                                                       uint32_t const,
                                                       uint32_t,
                                                       int8_t,
                                                       char const * const,
                                                       char const * const,
                                                       uint32_t const)) :
                    m_object(object), m_api(api), m_f(f) {}
                virtual ~callback_function_cxx_m2() throw()
                {
                    delete m_api;
                }

                virtual void operator () (int const command,
                                          char const * const name,
                                          char const * const pattern,
                                          void const * const request_info,
                                          uint32_t const request_info_size,
                                          void const * const request,
                                          uint32_t const request_size,
                                          uint32_t timeout,
                                          int8_t priority,
                                          char const * const trans_id,
                                          char const * const pid,
                                          uint32_t const pid_size)
                {
                    (m_object.*m_f)(*m_api,
                                    command,
                                    name,
                                    pattern,
                                    request_info,
                                    request_info_size,
                                    request,
                                    request_size,
                                    timeout,
                                    priority,
                                    trans_id,
                                    pid,
                                    pid_size);
                }
            private:
                T & m_object;
                API const * m_api;
                void (T::*m_f) (API const &,
                                int const,
                                char const * const,
                                char const * const,
                                void const * const,
                                uint32_t const,
                                void const * const,
                                uint32_t const,
                                uint32_t,
                                int8_t,
                                char const * const,
                                char const * const,
                                uint32_t const);
        };

        class callback_function_cxx_s1 : public callback_function_generic
        {
            public:
                callback_function_cxx_s1(API const * api,
                                         void (*f) (API const &,
                                                    int const,
                                                    std::string const &,
                                                    std::string const &,
                                                    void const * const,
                                                    uint32_t const,
                                                    void const * const,
                                                    uint32_t const,
                                                    uint32_t,
                                                    int8_t,
                                                    char const * const,
                                                    char const * const,
                                                    uint32_t const)) :
                    m_api(api), m_f(f) {}
                virtual ~callback_function_cxx_s1() throw()
                {
                    delete m_api;
                }

                virtual void operator () (int const command,
                                          char const * const name,
                                          char const * const pattern,
                                          void const * const request_info,
                                          uint32_t const request_info_size,
                                          void const * const request,
                                          uint32_t const request_size,
                                          uint32_t timeout,
                                          int8_t priority,
                                          char const * const trans_id,
                                          char const * const pid,
                                          uint32_t const pid_size)
                {
                    (*m_f)(*m_api,
                           command,
                           std::string(name),
                           std::string(pattern),
                           request_info,
                           request_info_size,
                           request,
                           request_size,
                           timeout,
                           priority,
                           trans_id,
                           pid,
                           pid_size);
                }
            private:
                API const * m_api;
                void (*m_f) (API const &,
                             int const,
                             std::string const &,
                             std::string const &,
                             void const * const,
                             uint32_t const,
                             void const * const,
                             uint32_t const,
                             uint32_t,
                             int8_t,
                             char const * const,
                             char const * const,
                             uint32_t const);
        };

        class callback_function_cxx_s2 : public callback_function_generic
        {
            public:
                callback_function_cxx_s2(API const * api,
                                         void (*f) (API const &,
                                                    int const,
                                                    char const * const,
                                                    char const * const,
                                                    void const * const,
                                                    uint32_t const,
                                                    void const * const,
                                                    uint32_t const,
                                                    uint32_t,
                                                    int8_t,
                                                    char const * const,
                                                    char const * const,
                                                    uint32_t const)) :
                    m_api(api), m_f(f) {}
                virtual ~callback_function_cxx_s2() throw()
                {
                    delete m_api;
                }

                virtual void operator () (int const command,
                                          char const * const name,
                                          char const * const pattern,
                                          void const * const request_info,
                                          uint32_t const request_info_size,
                                          void const * const request,
                                          uint32_t const request_size,
                                          uint32_t timeout,
                                          int8_t priority,
                                          char const * const trans_id,
                                          char const * const pid,
                                          uint32_t const pid_size)
                {
                    (*m_f)(*m_api,
                           command,
                           name,
                           pattern,
                           request_info,
                           request_info_size,
                           request,
                           request_size,
                           timeout,
                           priority,
                           trans_id,
                           pid,
                           pid_size);
                }
            private:
                API const * m_api;
                void (*m_f) (API const &,
                             int const,
                             char const * const,
                             char const * const,
                             void const * const,
                             uint32_t const,
                             void const * const,
                             uint32_t const,
                             uint32_t,
                             int8_t,
                             char const * const,
                             char const * const,
                             uint32_t const);
        };

    public:
        // command values
        static int const ASYNC = 1;
        static int const SYNC = -1;

        API(unsigned int const thread_index);
        ~API();
        API(API const & object);

        static unsigned int thread_count();

        int subscribe(std::string const & pattern,
                      function_object_cxx_const const & object) const
        {
            return subscribe(pattern.c_str(),
                             new callback_function_cxx_or1(object,
                                                           new API(*this)));
        }

        int subscribe(char const * const pattern,
                      function_object_c_const const & object) const
        {
            return subscribe(pattern,
                             new callback_function_cxx_or2(object,
                                                           new API(*this)));
        }

        int subscribe(std::string const & pattern,
                      function_object_cxx * object) const
        {
            return subscribe(pattern.c_str(),
                             new callback_function_cxx_op1(object,
                                                           new API(*this)));
        }

        int subscribe(char const * const pattern,
                      function_object_c * object) const
        {
            return subscribe(pattern,
                             new callback_function_cxx_op2(object,
                                                           new API(*this)));
        }

        template <typename T>
        int subscribe(std::string const & pattern,
                      T & object,
                      void (T::*f) (API const &,
                                    int const,
                                    std::string const &,
                                    std::string const &,
                                    void const * const,
                                    uint32_t const,
                                    void const * const,
                                    uint32_t const,
                                    uint32_t,
                                    int8_t,
                                    char const * const,
                                    char const * const,
                                    uint32_t const)) const
        {
            return subscribe(pattern.c_str(),
                             new callback_function_cxx_m1<T>(object,
                                                             new API(*this),
                                                             f));
        }

        template <typename T>
        int subscribe(char const * const pattern,
                      T & object,
                      void (T::*f) (API const &,
                                    int const,
                                    std::string const &,
                                    std::string const &,
                                    void const * const,
                                    uint32_t const,
                                    void const * const,
                                    uint32_t const,
                                    uint32_t,
                                    int8_t,
                                    char const * const,
                                    char const * const,
                                    uint32_t const)) const
        {
            return subscribe(pattern,
                             new callback_function_cxx_m1<T>(object,
                                                             new API(*this),
                                                             f));
        }

        template <typename T>
        int subscribe(std::string const & pattern,
                      T & object,
                      void (T::*f) (API const &,
                                    int const,
                                    char const * const,
                                    char const * const,
                                    void const * const,
                                    uint32_t const,
                                    void const * const,
                                    uint32_t const,
                                    uint32_t,
                                    int8_t,
                                    char const * const,
                                    char const * const,
                                    uint32_t const)) const
        {
            return subscribe(pattern.c_str(),
                             new callback_function_cxx_m2<T>(object,
                                                             new API(*this),
                                                             f));
        }

        template <typename T>
        int subscribe(char const * const pattern,
                      T & object,
                      void (T::*f) (API const &,
                                    int const,
                                    char const * const,
                                    char const * const,
                                    void const * const,
                                    uint32_t const,
                                    void const * const,
                                    uint32_t const,
                                    uint32_t,
                                    int8_t,
                                    char const * const,
                                    char const * const,
                                    uint32_t const)) const
        {
            return subscribe(pattern,
                             new callback_function_cxx_m2<T>(object,
                                                             new API(*this),
                                                             f));
        }

        inline int subscribe(std::string const & pattern,
                             void (*f) (API const &,
                                        int const,
                                        std::string const &,
                                        std::string const &,
                                        void const * const,
                                        uint32_t const,
                                        void const * const,
                                        uint32_t const,
                                        uint32_t,
                                        int8_t,
                                        char const * const,
                                        char const * const,
                                        uint32_t const)) const
        {
            return subscribe(pattern.c_str(),
                             new callback_function_cxx_s1(new API(*this), f));
        }

        inline int subscribe(char const * const pattern,
                             void (*f) (API const &,
                                        int const,
                                        std::string const &,
                                        std::string const &,
                                        void const * const,
                                        uint32_t const,
                                        void const * const,
                                        uint32_t const,
                                        uint32_t,
                                        int8_t,
                                        char const * const,
                                        char const * const,
                                        uint32_t const)) const
        {
            return subscribe(pattern,
                             new callback_function_cxx_s1(new API(*this), f));
        }

        inline int subscribe(std::string const & pattern,
                             void (*f) (API const &,
                                        int const,
                                        char const * const,
                                        char const * const,
                                        void const * const,
                                        uint32_t const,
                                        void const * const,
                                        uint32_t const,
                                        uint32_t,
                                        int8_t,
                                        char const * const,
                                        char const * const,
                                        uint32_t const)) const
        {
            return subscribe(pattern.c_str(),
                             new callback_function_cxx_s2(new API(*this), f));
        }

        inline int subscribe(char const * const pattern,
                             void (*f) (API const &,
                                        int const,
                                        char const * const,
                                        char const * const,
                                        void const * const,
                                        uint32_t const,
                                        void const * const,
                                        uint32_t const,
                                        uint32_t,
                                        int8_t,
                                        char const * const,
                                        char const * const,
                                        uint32_t const)) const
        {
            return subscribe(pattern,
                             new callback_function_cxx_s2(new API(*this), f));
        }

    private:
        int subscribe(char const * const pattern,
                      callback_function_generic * p) const;

    public:
        int unsubscribe(char const * const pattern) const;

        inline int unsubscribe(std::string const & pattern) const
        {
            return unsubscribe(pattern.c_str());
        }

        int send_async(char const * const name,
                       void const * const request,
                       uint32_t const request_size) const;

        inline int send_async(std::string const & name,
                              void const * const request,
                              uint32_t const request_size) const
        {
            return send_async(name.c_str(),
                              request,
                              request_size);
        }

        int send_async(char const * const name,
                       void const * const request_info,
                       uint32_t const request_info_size,
                       void const * const request,
                       uint32_t const request_size,
                       uint32_t timeout,
                       int8_t const priority) const;

        inline int send_async(std::string const & name,
                              void const * const request_info,
                              uint32_t const request_info_size,
                              void const * const request,
                              uint32_t const request_size,
                              uint32_t timeout,
                              int8_t const priority) const
        {
            return send_async(name.c_str(),
                              request_info,
                              request_info_size,
                              request,
                              request_size,
                              timeout,
                              priority);
        }

        int send_sync(char const * const name,
                      void const * const request,
                      uint32_t const request_size) const;

        inline int send_sync(std::string const & name,
                             void const * const request,
                             uint32_t const request_size) const
        {
            return send_sync(name.c_str(),
                             request,
                             request_size);
        }

        int send_sync(char const * const name,
                      void const * const request_info,
                      uint32_t const request_info_size,
                      void const * const request,
                      uint32_t const request_size,
                      uint32_t timeout,
                      int8_t const priority) const;

        inline int send_sync(std::string const & name,
                             void const * const request_info,
                             uint32_t const request_info_size,
                             void const * const request,
                             uint32_t const request_size,
                             uint32_t timeout,
                             int8_t const priority) const
        {
            return send_sync(name.c_str(),
                             request_info,
                             request_info_size,
                             request,
                             request_size,
                             timeout,
                             priority);
        }

        int mcast_async(char const * const name,
                        void const * const request,
                        uint32_t const request_size) const;

        inline int mcast_async(std::string const & name,
                               void const * const request,
                               uint32_t const request_size) const
        {
            return mcast_async(name.c_str(),
                               request,
                               request_size);
        }

        int mcast_async(char const * const name,
                        void const * const request_info,
                        uint32_t const request_info_size,
                        void const * const request,
                        uint32_t const request_size,
                        uint32_t timeout,
                        int8_t const priority) const;

        inline int mcast_async(std::string const & name,
                               void const * const request_info,
                               uint32_t const request_info_size,
                               void const * const request,
                               uint32_t const request_size,
                               uint32_t timeout,
                               int8_t const priority) const
        {
            return mcast_async(name.c_str(),
                               request_info,
                               request_info_size,
                               request,
                               request_size,
                               timeout,
                               priority);
        }

        char const * get_response() const;
        uint32_t get_response_size() const;

        char const * get_response_info() const;
        uint32_t get_response_info_size() const;

        uint32_t get_trans_id_count() const;
        char const * get_trans_id(unsigned int const i = 0) const;
        bool get_trans_id_null(unsigned int const i = 0) const;

        int forward_(int const command,
                     char const * const name,
                     void const * const request_info,
                     uint32_t const request_info_size,
                     void const * const request,
                     uint32_t const request_size,
                     uint32_t timeout,
                     int8_t const priority,
                     char const * const trans_id,
                     char const * const pid,
                     uint32_t const pid_size) const;

        inline int forward_(int const command,
                            std::string const & name,
                            void const * const request_info,
                            uint32_t const request_info_size,
                            void const * const request,
                            uint32_t const request_size,
                            uint32_t timeout,
                            int8_t const priority,
                            char const * const trans_id,
                            char const * const pid,
                            uint32_t const pid_size) const
        {
            return forward_(command,
                            name.c_str(),
                            request_info,
                            request_info_size,
                            request,
                            request_size,
                            timeout,
                            priority,
                            trans_id,
                            pid,
                            pid_size);
        }

        int forward_async(char const * const name,
                          void const * const request_info,
                          uint32_t const request_info_size,
                          void const * const request,
                          uint32_t const request_size,
                          uint32_t timeout,
                          int8_t const priority,
                          char const * const trans_id,
                          char const * const pid,
                          uint32_t const pid_size) const;

        inline int forward_async(std::string const & name,
                                 void const * const request_info,
                                 uint32_t const request_info_size,
                                 void const * const request,
                                 uint32_t const request_size,
                                 uint32_t timeout,
                                 int8_t const priority,
                                 char const * const trans_id,
                                 char const * const pid,
                                 uint32_t const pid_size) const
        {
            return forward_async(name.c_str(),
                                 request_info,
                                 request_info_size,
                                 request,
                                 request_size,
                                 timeout,
                                 priority,
                                 trans_id,
                                 pid,
                                 pid_size);
        }

        int forward_sync(char const * const name,
                         void const * const request_info,
                         uint32_t const request_info_size,
                         void const * const request,
                         uint32_t const request_size,
                         uint32_t timeout,
                         int8_t const priority,
                         char const * const trans_id,
                         char const * const pid,
                         uint32_t const pid_size) const;

        inline int forward_sync(std::string const & name,
                                void const * const request_info,
                                uint32_t const request_info_size,
                                void const * const request,
                                uint32_t const request_size,
                                uint32_t timeout,
                                int8_t const priority,
                                char const * const trans_id,
                                char const * const pid,
                                uint32_t const pid_size) const
        {
            return forward_sync(name.c_str(),
                                request_info,
                                request_info_size,
                                request,
                                request_size,
                                timeout,
                                priority,
                                trans_id,
                                pid,
                                pid_size);
        }

        int return_(int const command,
                    char const * const name,
                    char const * const pattern,
                    void const * const response_info,
                    uint32_t const response_info_size,
                    void const * const response,
                    uint32_t const response_size,
                    uint32_t timeout,
                    char const * const trans_id,
                    char const * const pid,
                    uint32_t const pid_size) const;

        inline int return_(int const command,
                           std::string const & name,
                           std::string const & pattern,
                           void const * const response_info,
                           uint32_t const response_info_size,
                           void const * const response,
                           uint32_t const response_size,
                           uint32_t timeout,
                           char const * const trans_id,
                           char const * const pid,
                           uint32_t const pid_size) const
        {
            return return_(command,
                           name.c_str(),
                           pattern.c_str(),
                           response_info,
                           response_info_size,
                           response,
                           response_size,
                           timeout,
                           trans_id,
                           pid,
                           pid_size);
        }

        int return_async(char const * const name,
                         char const * const pattern,
                         void const * const response_info,
                         uint32_t const response_info_size,
                         void const * const response,
                         uint32_t const response_size,
                         uint32_t timeout,
                         char const * const trans_id,
                         char const * const pid,
                         uint32_t const pid_size) const;

        inline int return_async(std::string const & name,
                                std::string const & pattern,
                                void const * const response_info,
                                uint32_t const response_info_size,
                                void const * const response,
                                uint32_t const response_size,
                                uint32_t timeout,
                                char const * const trans_id,
                                char const * const pid,
                                uint32_t const pid_size) const
        {
            return return_async(name.c_str(),
                                pattern.c_str(),
                                response_info,
                                response_info_size,
                                response,
                                response_size,
                                timeout,
                                trans_id,
                                pid,
                                pid_size);
        }

        int return_sync(char const * const name,
                        char const * const pattern,
                        void const * const response_info,
                        uint32_t const response_info_size,
                        void const * const response,
                        uint32_t const response_size,
                        uint32_t timeout,
                        char const * const trans_id,
                        char const * const pid,
                        uint32_t const pid_size) const;

        inline int return_sync(std::string const & name,
                               std::string const & pattern,
                               void const * const response_info,
                               uint32_t const response_info_size,
                               void const * const response,
                               uint32_t const response_size,
                               uint32_t timeout,
                               char const * const trans_id,
                               char const * const pid,
                               uint32_t const pid_size) const
        {
            return return_sync(name.c_str(),
                               pattern.c_str(),
                               response_info,
                               response_info_size,
                               response,
                               response_size,
                               timeout,
                               trans_id,
                               pid,
                               pid_size);
        }

        int recv_async() const;

        int recv_async(uint32_t timeout) const;

        int recv_async(char const * const trans_id) const;

        int recv_async(uint32_t timeout,
                       char const * const trans_id) const;

        inline int recv_async(uint32_t timeout,
                              std::string const & trans_id) const
        {
            return recv_async(timeout,
                              trans_id.c_str());
        }

        int recv_async(uint32_t timeout,
                       bool consume) const;

        int recv_async(char const * const trans_id,
                       bool consume) const;

        inline int recv_async(std::string const & trans_id,
                              bool consume) const
        {
            return recv_async(trans_id.c_str(),
                              consume);
        }

        int recv_async(uint32_t timeout,
                       char const * const trans_id,
                       bool consume) const;

        inline int recv_async(uint32_t timeout,
                              std::string const & trans_id,
                              bool consume) const
        {
            return recv_async(timeout,
                              trans_id.c_str(),
                              consume);
        }

        char const * prefix() const;

        uint32_t timeout_async() const;

        uint32_t timeout_sync() const;

        int8_t priority_default() const;

        int poll(int timeout = -1) const;

        char const ** request_http_qs_parse(void const * const request,
                                            uint32_t const request_size) const;
        void request_http_qs_destroy(char const ** p) const;

        char const ** info_key_value_parse(void const * const message_info,
                                           uint32_t const message_info_size)
                                           const;
        void info_key_value_destroy(char const ** p) const;

    private:
        cloudi_instance_t * const m_api;
        int * m_count; // m_api shared pointer count

    public:
        // create a nested namespace in a way the c++ standard accepts
        class return_value
        {
            public:
            enum
            {
                success                             =   0,
                // programs can use exit status
                // values [1..6] without conflicts
                // with internal cloudi error conditions
            
                // API specific errors
                timeout                             =   7,
                error_function_parameter            =   8,
                error_read_underflow                =   9,
                error_ei_decode                     =  10,
                // reuse some exit status values from os_spawn
                invalid_input                       =  11,
                out_of_memory                       =  12,
                // reuse some exit status values from GEPD
                error_read_EAGAIN                   =  81,
                error_read_EBADF                    =  82,
                error_read_EFAULT                   =  83,
                error_read_EINTR                    =  84,
                error_read_EINVAL                   =  85,
                error_read_EIO                      =  86,
                error_read_EISDIR                   =  87,
                error_read_null                     =  88,
                error_read_overflow                 =  89,
                error_read_unknown                  =  90,
                error_write_EAGAIN                  =  91,
                error_write_EBADF                   =  92,
                error_write_EFAULT                  =  93,
                error_write_EFBIG                   =  94,
                error_write_EINTR                   =  95,
                error_write_EINVAL                  =  96,
                error_write_EIO                     =  97,
                error_write_ENOSPC                  =  98,
                error_write_EPIPE                   =  99,
                error_write_null                    = 100,
                error_write_overflow                = 101,
                error_write_unknown                 = 102,
                error_ei_encode                     = 103,
                error_poll_EBADF                    = 104,
                error_poll_EFAULT                   = 105,
                error_poll_EINTR                    = 106,
                error_poll_EINVAL                   = 107,
                error_poll_ENOMEM                   = 108,
                error_poll_ERR                      = 109,
                error_poll_HUP                      = 110,
                error_poll_NVAL                     = 111,
                error_poll_unknown                  = 112
            };
        };

        // Get the backtrace at the current point of execution
        // (e.g., when throwing an exception):
        // #include <boost/exception/all.hpp>
        // ...
        // typedef boost::error_info<struct stack, std::string> errinfo_stack;
        // ...
        // throw (boost::enable_error_info(e)
        //            << errinfo_stack(CloudI::API::backtrace()));
        //
        // (n.b., requires the --with-cxx-backtrace configuration option)
        static std::string backtrace();

        class invalid_input_exception : public std::exception
        {
            public:
                invalid_input_exception() throw() {}
                virtual ~invalid_input_exception() throw() {}
                virtual char const * what() const throw()
                {
                    return "Invalid Input";
                }
        };

        class return_async_exception : public std::exception
        {
            public:
                return_async_exception() throw() {}
                virtual ~return_async_exception() throw() {}
                virtual char const * what() const throw()
                {
                    return "Asynchronous Call Return Invalid";
                }
        };

        class return_sync_exception : public std::exception
        {
            public:
                return_sync_exception() throw() {}
                virtual ~return_sync_exception() throw() {}
                virtual char const * what() const throw()
                {
                    return "Synchronous Call Return Invalid";
                }
        };

        class forward_async_exception : public std::exception
        {
            public:
                forward_async_exception() throw() {}
                virtual ~forward_async_exception() throw() {}
                virtual char const * what() const throw()
                {
                    return "Asynchronous Call Forward Invalid";
                }
        };

        class forward_sync_exception : public std::exception
        {
            public:
                forward_sync_exception() throw() {}
                virtual ~forward_sync_exception() throw() {}
                virtual char const * what() const throw()
                {
                    return "Synchronous Call Forward Invalid";
                }
        };

};

} // namespace CloudI

#endif // CLOUDI_HPP

