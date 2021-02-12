//-*-Mode:C++;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=cpp fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
//
// MIT License
//
// Copyright (c) 2011-2021 Michael Truog <mjtruog at protonmail dot com>
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
#include <ei.h>
#include <vector>
#include <cstring>
#include <errno.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/un.h>
#include <sys/socket.h>
#include <signal.h>
#include <iostream>
#include "config.h"
#include "port.hpp"
#include "realloc_ptr.hpp"
#include "copy_ptr.hpp"
#include "cloudi_os_spawn.hpp"
#include "cloudi_os_rlimit.hpp"
#include "cloudi_os_owner.hpp"
#include "cloudi_os_syscall_lock.hpp"
#include "assert.hpp"

namespace
{
    namespace spawn_status
    {
        enum
        {
            success                         =  0,
            invalid_input                   = 11,
            out_of_memory,
            pipe_EFAULT,
            pipe_EINVAL,
            pipe_EMFILE,
            pipe_ENFILE,
            pipe_unknown,
            fork_EAGAIN,
            fork_ENOMEM,
            fork_unknown,                  // 20
            socket_EACCES,
            socket_EAFNOSUPPORT,
            socket_EINVAL,
            socket_EMFILE,
            socket_ENFILE,
            socket_ENOBUFS,
            socket_ENOMEM,
            socket_EPROTONOSUPPORT,
            socket_unknown,
            dup_EBADF,                     // 30
            dup_EBUSY,
            dup_EINTR,
            dup_EINVAL,
            dup_EMFILE,
            dup_unknown,
            close_EBADF,
            close_EINTR,
            close_EIO,
            close_unknown,
            waitpid_ECHILD,                // 40
            waitpid_EINTR,
            waitpid_EINVAL,
            waitpid_unknown,
            connect_EACCES,
            connect_EPERM,
            connect_EADDRINUSE,
            connect_ENOENT,
            connect_EAGAIN,
            connect_EALREADY,
            connect_EBADF,                 // 50
            connect_ECONNREFUSED,
            connect_EFAULT,
            connect_EINPROGRESS,
            connect_EINTR,
            connect_EISCONN,
            connect_ENETUNREACH,
            connect_ENOTSOCK,
            connect_ETIMEDOUT,
            connect_unknown,
            exec_E2BIG,                    // 60
            exec_EACCES,
            exec_EFAULT,
            exec_EINVAL,
            exec_EIO,
            exec_EISDIR,
            exec_ELIBBAD,
            exec_ELOOP,
            exec_EMFILE,
            exec_ENAMETOOLONG,
            exec_ENFILE,                   // 70
            exec_ENOENT,
            exec_ENOEXEC,
            exec_ENOMEM,
            exec_ENOTDIR,
            exec_EPERM,
            exec_ETXTBSY,
            exec_unknown,
            last_value                     // 78
        };

        char const * string(int status)
        {
            switch (status)
            {
                // internal spawn_status errors
                case invalid_input:
                    return "invalid_input";
                case out_of_memory:
                    return "out_of_memory";
                case pipe_EFAULT:
                    return "pipe_EFAULT";
                case pipe_EINVAL:
                    return "pipe_EINVAL";
                case pipe_EMFILE:
                    return "pipe_EMFILE";
                case pipe_ENFILE:
                    return "pipe_ENFILE";
                case pipe_unknown:
                    return "pipe_unknown";
                case fork_EAGAIN:
                    return "fork_EAGAIN";
                case fork_ENOMEM:
                    return "fork_ENOMEM";
                case fork_unknown:
                    return "fork_unknown";
                case socket_EACCES:
                    return "socket_EACCES";
                case socket_EAFNOSUPPORT:
                    return "socket_EAFNOSUPPORT";
                case socket_EINVAL:
                    return "socket_EINVAL";
                case socket_EMFILE:
                    return "socket_EMFILE";
                case socket_ENFILE:
                    return "socket_ENFILE";
                case socket_ENOBUFS:
                    return "socket_ENOBUFS";
                case socket_ENOMEM:
                    return "socket_ENOMEM";
                case socket_EPROTONOSUPPORT:
                    return "socket_EPROTONOSUPPORT";
                case socket_unknown:
                    return "socket_unknown";
                case dup_EBADF:
                    return "dup_EBADF";
                case dup_EBUSY:
                    return "dup_EBUSY";
                case dup_EINTR:
                    return "dup_EINTR";
                case dup_EINVAL:
                    return "dup_EINVAL";
                case dup_EMFILE:
                    return "dup_EMFILE";
                case dup_unknown:
                    return "dup_unknown";
                case close_EBADF:
                    return "close_EBADF";
                case close_EINTR:
                    return "close_EINTR";
                case close_EIO:
                    return "close_EIO";
                case close_unknown:
                    return "close_unknown";
                case waitpid_ECHILD:
                    return "waitpid_ECHILD";
                case waitpid_EINTR:
                    return "waitpid_EINTR";
                case waitpid_EINVAL:
                    return "waitpid_EINVAL";
                case waitpid_unknown:
                    return "waitpid_unknown";
                case connect_EACCES:
                    return "connect_EACCES";
                case connect_EPERM:
                    return "connect_EPERM";
                case connect_EADDRINUSE:
                    return "connect_EADDRINUSE";
                case connect_ENOENT:
                    return "connect_ENOENT";
                case connect_EAGAIN:
                    return "connect_EAGAIN";
                case connect_EALREADY:
                    return "connect_EALREADY";
                case connect_EBADF:
                    return "connect_EBADF";
                case connect_ECONNREFUSED:
                    return "connect_ECONNREFUSED";
                case connect_EFAULT:
                    return "connect_EFAULT";
                case connect_EINPROGRESS:
                    return "connect_EINPROGRESS";
                case connect_EINTR:
                    return "connect_EINTR";
                case connect_EISCONN:
                    return "connect_EISCONN";
                case connect_ENETUNREACH:
                    return "connect_ENETUNREACH";
                case connect_ENOTSOCK:
                    return "connect_ENOTSOCK";
                case connect_ETIMEDOUT:
                    return "connect_ETIMEDOUT";
                case connect_unknown:
                    return "connect_unknown";
                case exec_E2BIG:
                    return "exec_E2BIG";
                case exec_EACCES:
                    return "exec_EACCES";
                case exec_EFAULT:
                    return "exec_EFAULT";
                case exec_EINVAL:
                    return "exec_EINVAL";
                case exec_EIO:
                    return "exec_EIO";
                case exec_EISDIR:
                    return "exec_EISDIR";
                case exec_ELIBBAD:
                    return "exec_ELIBBAD";
                case exec_ELOOP:
                    return "exec_ELOOP";
                case exec_EMFILE:
                    return "exec_EMFILE";
                case exec_ENAMETOOLONG:
                    return "exec_ENAMETOOLONG";
                case exec_ENFILE:
                    return "exec_ENFILE";
                case exec_ENOENT:
                    return "exec_ENOENT";
                case exec_ENOEXEC:
                    return "exec_ENOEXEC";
                case exec_ENOMEM:
                    return "exec_ENOMEM";
                case exec_ENOTDIR:
                    return "exec_ENOTDIR";
                case exec_EPERM:
                    return "exec_EPERM";
                case exec_ETXTBSY:
                    return "exec_ETXTBSY";
                case exec_unknown:
                    return "exec_unknown";

                // GEPD::ExitStatus values reused
                case GEPD::ExitStatus::read_EAGAIN:
                    return "read_EAGAIN";
                case GEPD::ExitStatus::read_EBADF:
                    return "read_EBADF";
                case GEPD::ExitStatus::read_EFAULT:
                    return "read_EFAULT";
                case GEPD::ExitStatus::read_EINTR:
                    return "read_EINTR";
                case GEPD::ExitStatus::read_EINVAL:
                    return "read_EINVAL";
                case GEPD::ExitStatus::read_EIO:
                    return "read_EIO";
                case GEPD::ExitStatus::read_EISDIR:
                    return "read_EISDIR";
                case GEPD::ExitStatus::read_null:
                    return "read_null";
                case GEPD::ExitStatus::read_overflow:
                    return "read_overflow";
                case GEPD::ExitStatus::read_unknown:
                    return "read_unknown";
                case GEPD::ExitStatus::write_EAGAIN:
                    return "write_EAGAIN";
                case GEPD::ExitStatus::write_EBADF:
                    return "write_EBADF";
                case GEPD::ExitStatus::write_EFAULT:
                    return "write_EFAULT";
                case GEPD::ExitStatus::write_EFBIG:
                    return "write_EFBIG";
                case GEPD::ExitStatus::write_EINTR:
                    return "write_EINTR";
                case GEPD::ExitStatus::write_EINVAL:
                    return "write_EINVAL";
                case GEPD::ExitStatus::write_EIO:
                    return "write_EIO";
                case GEPD::ExitStatus::write_ENOSPC:
                    return "write_ENOSPC";
                case GEPD::ExitStatus::write_EPIPE:
                    return "write_EPIPE";
                case GEPD::ExitStatus::write_null:
                    return "write_null";
                case GEPD::ExitStatus::write_overflow:
                    return "write_overflow";
                case GEPD::ExitStatus::write_unknown:
                    return "write_unknown";
                case GEPD::ExitStatus::ei_encode_error:
                    return "ei_encode_error";
                case GEPD::ExitStatus::poll_EBADF:
                    return "poll_EBADF";
                case GEPD::ExitStatus::poll_EFAULT:
                    return "poll_EFAULT";
                case GEPD::ExitStatus::poll_EINTR:
                    return "poll_EINTR";
                case GEPD::ExitStatus::poll_EINVAL:
                    return "poll_EINVAL";
                case GEPD::ExitStatus::poll_ENOMEM:
                    return "poll_ENOMEM";
                case GEPD::ExitStatus::poll_ERR:
                    return "poll_ERR";
                case GEPD::ExitStatus::poll_HUP:
                    return "poll_HUP";
                case GEPD::ExitStatus::poll_NVAL:
                    return "poll_NVAL";
                case GEPD::ExitStatus::poll_unknown:
                    return "poll_unknown";
                case GEPD::ExitStatus::pipe_EFAULT:
                    return "pipe_EFAULT";
                case GEPD::ExitStatus::pipe_EINVAL:
                    return "pipe_EINVAL";
                case GEPD::ExitStatus::pipe_EMFILE:
                    return "pipe_EMFILE";
                case GEPD::ExitStatus::pipe_ENFILE:
                    return "pipe_ENFILE";
                case GEPD::ExitStatus::pipe_unknown:
                    return "pipe_unknown";
                case GEPD::ExitStatus::dup_EBADF:
                    return "dup_EBADF";
                case GEPD::ExitStatus::dup_EBUSY:
                    return "dup_EBUSY";
                case GEPD::ExitStatus::dup_EINTR:
                    return "dup_EINTR";
                case GEPD::ExitStatus::dup_EINVAL:
                    return "dup_EINVAL";
                case GEPD::ExitStatus::dup_EMFILE:
                    return "dup_EMFILE";
                case GEPD::ExitStatus::dup_unknown:
                    return "dup_unknown";
                case GEPD::ExitStatus::close_EBADF:
                    return "close_EBADF";
                case GEPD::ExitStatus::close_EINTR:
                    return "close_EINTR";
                case GEPD::ExitStatus::close_EIO:
                    return "close_EIO";
                case GEPD::ExitStatus::close_unknown:
                    return "close_unknown";

                // signals
                case 129:
                    return "SIGHUP";
                case 130:
                    return "SIGINT";
                case 131:
                    return "SIGQUIT";
                case 132:
                    return "SIGILL";
                case 133:
                    return "SIGTRAP";
                case 134:
                    return "SIGABRT";
                case 136:
                    return "SIGFPE";
                case 137:
                    return "SIGKILL";
                case 139:
                    return "SIGSEGV";
                case 141:
                    return "SIGPIPE";
                case 142:
                    return "SIGALRM";
                case 143:
                    return "SIGTERM";
                default:
                    return 0;
            }
        }

        int errno_pipe()
        {
            switch (errno)
            {
                case EFAULT:
                    return pipe_EFAULT;
                case EINVAL:
                    return pipe_EINVAL;
                case EMFILE:
                    return pipe_EMFILE;
                case ENFILE:
                    return pipe_ENFILE;
                default:
                    return pipe_unknown;
            }
        }

        int errno_fork()
        {
            switch (errno)
            {
                case EAGAIN:
                    return fork_EAGAIN;
                case ENOMEM:
                    return fork_ENOMEM;
                default:
                    return fork_unknown;
            }
        }
    
        int errno_socket()
        {
            switch (errno)
            {
                case EACCES:
                    return socket_EACCES;
                case EAFNOSUPPORT:
                    return socket_EAFNOSUPPORT;
                case EINVAL:
                    return socket_EINVAL;
                case EMFILE:
                    return socket_EMFILE;
                case ENFILE:
                    return socket_ENFILE;
                case ENOBUFS:
                    return socket_ENOBUFS;
                case ENOMEM:
                    return socket_ENOMEM;
                case EPROTONOSUPPORT:
                    return socket_EPROTONOSUPPORT;
                default:
                    return socket_unknown;
            }
        }

        int errno_dup()
        {
            switch (errno)
            {
                case EBADF:
                    return dup_EBADF;
                case EBUSY:
                    return dup_EBUSY;
                case EINTR:
                    return dup_EINTR;
                case EINVAL:
                    return dup_EINVAL;
                case EMFILE:
                    return dup_EMFILE;
                default:
                    return dup_unknown;
            }
        }

        int errno_close()
        {
            switch (errno)
            {
                case EBADF:
                    return close_EBADF;
                case EINTR:
                    return close_EINTR;
                case EIO:
                    return close_EIO;
                default:
                    return close_unknown;
            }
        }

        int errno_connect()
        {
            switch (errno)
            {
                case EACCES:
                    return connect_EACCES;
                case EPERM:
                    return connect_EPERM;
                case EADDRINUSE:
                    return connect_EADDRINUSE;
                case ENOENT:
                    return connect_ENOENT;
                case EAGAIN:
                    return connect_EAGAIN;
                case EALREADY:
                    return connect_EALREADY;
                case EBADF:
                    return connect_EBADF;
                case ECONNREFUSED:
                    return connect_ECONNREFUSED;
                case EFAULT:
                    return connect_EFAULT;
                case EINPROGRESS:
                    return connect_EINPROGRESS;
                case EINTR:
                    return connect_EINTR;
                case EISCONN:
                    return connect_EISCONN;
                case ENETUNREACH:
                    return connect_ENETUNREACH;
                case ENOTSOCK:
                    return connect_ENOTSOCK;
                case ETIMEDOUT:
                    return connect_ETIMEDOUT;
                default:
                    return connect_unknown;
            }
        }

        int errno_exec()
        {
            switch (errno)
            {
                case E2BIG:
                    return exec_E2BIG;
                case EACCES:
                    return exec_EACCES;
                case EFAULT:
                    return exec_EFAULT;
                case EINVAL:
                    return exec_EINVAL;
                case EIO:
                    return exec_EIO;
                case EISDIR:
                    return exec_EISDIR;
#ifdef ELIBBAD
                case ELIBBAD:
                    return exec_ELIBBAD;
#endif
                case ELOOP:
                    return exec_ELOOP;
                case EMFILE:
                    return exec_EMFILE;
                case ENAMETOOLONG:
                    return exec_ENAMETOOLONG;
                case ENFILE:
                    return exec_ENFILE;
                case ENOENT:
                    return exec_ENOENT;
                case ENOEXEC:
                    return exec_ENOEXEC;
                case ENOMEM:
                    return exec_ENOMEM;
                case ENOTDIR:
                    return exec_ENOTDIR;
                case EPERM:
                    return exec_EPERM;
                case ETXTBSY:
                    return exec_ETXTBSY;
                default:
                    return exec_unknown;
            }
        }

        int errno_write()
        {
            switch (errno)
            {
                case EAGAIN:
                    return GEPD::ExitStatus::write_EAGAIN;
                case EBADF:
                    return GEPD::ExitStatus::write_EBADF;
                case EFAULT:
                    return GEPD::ExitStatus::write_EFAULT;
                case EFBIG:
                    return GEPD::ExitStatus::write_EFBIG;
                case EINTR:
                    return GEPD::ExitStatus::write_EINTR;
                case EINVAL:
                    return GEPD::ExitStatus::write_EINVAL;
                case EIO:
                    return GEPD::ExitStatus::write_EIO;
                case ENOSPC:
                    return GEPD::ExitStatus::write_ENOSPC;
                case EPIPE:
                    return GEPD::ExitStatus::write_EPIPE;
                default:
                    return GEPD::ExitStatus::write_unknown;
            }
        }

    } // namespace spawn_status

    uint32_t strings_count(char const * const p, uint32_t const len)
    {
        uint32_t count = 1;
        assert(p[len - 1] == '\0');
        if (len > 1)
        {
            ++count;
            for (uint32_t i = 0; i < len - 1; ++i)
            {
                if (p[i] == '\0')
                    ++count;
            }
        }
        return count;
    }

    void strings_set(char ** const strings, uint32_t const count,
                     char * const p, uint32_t const len)
    {
        uint32_t index = 0;
        if (count > 1)
        {
            strings[index++] = p;
            for (uint32_t i = 0; i < len - 1; ++i)
            {
                if (p[i] == '\0')
                    strings[index++] = &(p[i + 1]);
            }
        }
        strings[index++] = 0;
        assert(index == count);
    }

    class process_data
    {
        public:
            process_data(unsigned long const pid,
                         int const index_stdout,
                         int const index_stderr) :
                m_pid(pid),
                m_index_stdout(index_stdout),
                m_index_stderr(index_stderr),
                m_index_stream1(0),
                m_index_stream2(0),
                m_stream1(1, 16384),
                m_stream2(1, 16384),
                m_killed(false)
            {
            }

            ~process_data()
            {
                GEPD::nfds -= 2;

                if (m_killed == false)
                {
                    // after the stdout/stderr pipes have closed,
                    // block execution here waiting for m_pid to terminate
                    // (depend on cloudi_core_i_services_monitor killing
                    //  the pid with SIGKILL after the termination timeout)
                    int status;
                    int const pid = ::waitpid(m_pid, &status, 0);
                    assert(pid == static_cast<signed>(m_pid));

                    if (WIFEXITED(status))
                    {
                        status = WEXITSTATUS(status);
                    }
                    else if (WIFSIGNALED(status))
                    {
                        status = WTERMSIG(status) + 128;
                    }
                    else
                    {
                        assert(false);
                    }
                    if (status != 0)
                    {
                        char const * const error = spawn_status::string(status);
                        std::cerr << "OS pid " << m_pid << " exited with ";
                        if (error)
                            std::cerr << error << std::endl;
                        else if (status > 128)
                            std::cerr << "SIG#" << (status - 128) << std::endl;
                        else
                            std::cerr << status << std::endl;
                    }
                }
            }

            void close()
            {
                ::close(GEPD::fds[m_index_stdout].fd);
                ::close(GEPD::fds[m_index_stderr].fd);
            }

            void kill()
            {
                close();
                assert(SIGKILL == 9);
                ::kill(m_pid, SIGKILL);
                m_killed = true;
            }

            void shift()
            {
                int const old_index_stdout = m_index_stdout;
                int const old_index_stderr = m_index_stderr;
                m_index_stdout -= 2;
                m_index_stderr -= 2;
                GEPD::fds[m_index_stdout] = GEPD::fds[old_index_stdout];
                GEPD::fds[m_index_stderr] = GEPD::fds[old_index_stderr];
            }

            int flush(int & count, realloc_ptr<unsigned char> & send_buffer)
            {
                using namespace GEPD;
                int status;
                if (count > 0 && fds[m_index_stderr].revents != 0)
                {
                    if ((status = flush_stream(fds[m_index_stderr].fd,
                                               fds[m_index_stderr].revents,
                                               "stderr", m_pid, send_buffer,
                                               m_stream2, m_index_stream2)))
                        return status;
                    --count;
                }
                if (count > 0 && fds[m_index_stdout].revents != 0)
                {
                    if ((status = flush_stream(fds[m_index_stdout].fd,
                                               fds[m_index_stdout].revents,
                                               "stdout", m_pid, send_buffer,
                                               m_stream1, m_index_stream1)))
                        return status;
                    --count;
                }
                return 0;
            }

            int check(int & count, realloc_ptr<unsigned char> & send_buffer)
            {
                using namespace GEPD;
                int status;
                if (count > 0 && fds[m_index_stderr].revents != 0)
                {
                    if ((status = consume_stream(fds[m_index_stderr].fd,
                                                 fds[m_index_stderr].revents,
                                                 "stderr", m_pid, send_buffer,
                                                 m_stream2, m_index_stream2)))
                        return status;
                    --count;
                }
                if (count > 0 && fds[m_index_stdout].revents != 0)
                {
                    if ((status = consume_stream(fds[m_index_stdout].fd,
                                                 fds[m_index_stdout].revents,
                                                 "stdout", m_pid, send_buffer,
                                                 m_stream1, m_index_stream1)))
                        return status;
                    --count;
                }
                return 0;
            }
    
        private:
            unsigned long const m_pid;
            int m_index_stdout;
            int m_index_stderr;
            size_t m_index_stream1;
            size_t m_index_stream2;
            realloc_ptr<unsigned char> m_stream1;
            realloc_ptr<unsigned char> m_stream2;
            bool m_killed;
    };

    std::vector< copy_ptr<process_data> > processes;

} // anonymous namespace

bool terminate_now()
{
    return processes.empty();
}

int32_t spawn(char protocol,
              char * socket_path, uint32_t socket_path_len,
              uint32_t * ports, uint32_t ports_len,
              char * rlimits, uint32_t rlimits_len,
              uint64_t user_i,
              char * user_str, uint32_t user_str_len,
              uint64_t group_i,
              char * group_str, uint32_t group_str_len,
              int32_t nice,
              char * chroot_directory, uint32_t chroot_directory_len,
              char * syscall_lock, uint32_t syscall_lock_len,
              char * directory, uint32_t directory_len,
              char * filename, uint32_t /*filename_len*/,
              char * argv, uint32_t argv_len,
              char * env, uint32_t env_len)
{
    int domain;
    int type;
    int use_header;
    if (protocol == 't') // tcp inet
    {
        domain = PF_INET;
        type = SOCK_STREAM;
        use_header = 1;
    }
    else if (protocol == 'u') // udp inet
    {
        domain = PF_INET;
        type = SOCK_DGRAM;
        use_header = 0;
    }
    else if (protocol == 'l') // tcp local
    {
        domain = PF_LOCAL;
        type = SOCK_STREAM;
        use_header = 1;
    }
    else
    {
        return spawn_status::invalid_input;
    }
    int fds_stdout[2] = {-1, -1};
    int fds_stderr[2] = {-1, -1};
    if (::pipe(fds_stdout) == -1)
        return spawn_status::errno_pipe();
    if (::pipe(fds_stderr) == -1)
        return spawn_status::errno_pipe();
    pid_t const pid = fork();
    if (pid == -1)
    {
        return spawn_status::errno_fork();
    }
    else if (pid == 0)
    {
        for (size_t i = 0; i < GEPD::nfds; ++i)
        {
            if (::close(GEPD::fds[i].fd) == -1)
                ::_exit(spawn_status::errno_close());
        }
        if (::dup2(fds_stdout[1], 1) == -1)
            ::_exit(spawn_status::errno_dup());
        if (::close(fds_stdout[0]) == -1 || close(fds_stdout[1]) == -1)
            ::_exit(spawn_status::errno_close());
        if (::dup2(fds_stderr[1], 2) == -1)
            ::_exit(spawn_status::errno_dup());
        if (::close(fds_stderr[0]) == -1 || close(fds_stderr[1]) == -1)
            ::_exit(spawn_status::errno_close());

        char pid_message[1024];
        int pid_message_index = 0;
        if (use_header)
            pid_message_index = 4;
        unsigned long const pid_child = ::getpid();
        if (::ei_encode_version(pid_message, &pid_message_index))
            ::_exit(GEPD::ExitStatus::ei_encode_error);
        if (::ei_encode_tuple_header(pid_message, &pid_message_index, 2))
            ::_exit(GEPD::ExitStatus::ei_encode_error);
        if (::ei_encode_atom(pid_message, &pid_message_index, "pid"))
            ::_exit(GEPD::ExitStatus::ei_encode_error);
        if (::ei_encode_ulong(pid_message, &pid_message_index, pid_child))
            ::_exit(GEPD::ExitStatus::ei_encode_error);
        if (use_header)
        {
            int pid_message_length = pid_message_index - 4;
            pid_message[0] = (pid_message_length & 0xff000000) >> 24;
            pid_message[1] = (pid_message_length & 0x00ff0000) >> 16;
            pid_message[2] = (pid_message_length & 0x0000ff00) >> 8;
            pid_message[3] =  pid_message_length & 0x000000ff;
        }

        for (size_t i = 0; i < ports_len; ++i)
        {
            int sockfd = ::socket(domain, type, 0);
            if (sockfd == -1)
                ::_exit(spawn_status::errno_socket());
            if (domain == PF_INET && type == SOCK_STREAM)
            {
                int const tcp_nodelay_flag = 1;
                // set TCP_NODELAY to turn off Nagle's algorithm
                if (::setsockopt(sockfd, IPPROTO_TCP, TCP_NODELAY,
                                 &tcp_nodelay_flag, sizeof(int)) == -1)
                    ::_exit(spawn_status::socket_unknown);
            }

            if (static_cast<size_t>(sockfd) != i + 3)
            {
                if (::dup2(sockfd, i + 3) == -1)
                    ::_exit(spawn_status::errno_dup());
                if (::close(sockfd) == -1)
                    ::_exit(spawn_status::errno_close());
                sockfd = i + 3;
            }
            
            if (domain == PF_INET)
            {
                struct sockaddr_in localhost;
                localhost.sin_family = domain;
                localhost.sin_port = htons(ports[i]);
                //"127.0.0.1" == htonl(INADDR_LOOPBACK) == 0x0100007f
                // (in network byte order == big endian for PF_INET)
                localhost.sin_addr.s_addr = 0x0100007f;
    
                if (::connect(sockfd,
                              reinterpret_cast<struct sockaddr *>(&localhost),
                              sizeof(localhost)) == -1)
                    ::_exit(spawn_status::errno_connect());
            }
            else if (domain == PF_LOCAL)
            {
                char port_str[16];
                int const port_str_len = ::snprintf(port_str, sizeof(port_str),
                                                    "%d", ports[i]);
                assert(socket_path_len <=
                       static_cast<uint32_t>(104 - port_str_len));
                struct sockaddr_un local;
                ::memset(&local, 0, sizeof(local));
                local.sun_family = domain;
                ::memcpy(local.sun_path, socket_path, socket_path_len - 1);
                ::memcpy(&(local.sun_path[socket_path_len - 1]),
                         port_str, port_str_len);

                if (::connect(sockfd,
                              reinterpret_cast<struct sockaddr *>(&local),
                              sizeof(local)) == -1)
                    ::_exit(spawn_status::errno_connect());
            }
            else
            {
                assert(false);
            }

            if (i == 0)
            {
                // let the first connection get a pid message for attempting
                // to kill the OS process when the Erlang process terminates
                if (::write(sockfd, pid_message, pid_message_index) == -1)
                    ::_exit(spawn_status::errno_write());
            }
        }

        uint32_t const argv_count = 1 + strings_count(argv, argv_len);
        char ** const execve_argv = new char*[argv_count];
        execve_argv[0] = filename;
        strings_set(&execve_argv[1], argv_count - 1, argv, argv_len);

        uint32_t const env_count = strings_count(env, env_len);
        char ** const execve_env = new char*[env_count];
        strings_set(execve_env, env_count, env, env_len);

        if (owner_get(user_i, user_str, user_str_len,
                      group_i, group_str, group_str_len))
            ::_exit(spawn_status::invalid_input);
        if (chroot_directory_len > 1)
        {
#if defined(HAVE_CHROOT)
            if (::chroot(chroot_directory))
                ::_exit(spawn_status::invalid_input);
            if (directory_len == 1)
            {
                if (::chdir("/"))
                    ::_exit(spawn_status::invalid_input);
            }
#else
            assert(chroot_directory);
            ::_exit(spawn_status::invalid_input);
#endif
        }
        if (rlimit(rlimits, rlimits_len))
            ::_exit(spawn_status::invalid_input);
        if (nice != 0)
        {
            errno = 0;
            if (::nice(nice)) {} // ignore invalid -1 result
            if (errno != 0)
                ::_exit(spawn_status::invalid_input);
        }
        if (owner_set(user_i, group_i))
            ::_exit(spawn_status::invalid_input);
        if (directory_len > 1)
        {
            if (::chdir(directory))
                ::_exit(spawn_status::invalid_input);
        }
        if (syscall_lock_len > 1)
        {
            uint32_t const syscall_lock_count = strings_count(syscall_lock,
                                                              syscall_lock_len);
            char ** const syscall_names = new char*[syscall_lock_count];
            strings_set(syscall_names, syscall_lock_count,
                        syscall_lock, syscall_lock_len);
            if (syscall_lock_set(syscall_names))
                ::_exit(spawn_status::invalid_input);
            delete [] syscall_names;
        }

        ::execve(filename, execve_argv, execve_env);
        ::_exit(spawn_status::errno_exec());
    }
    else
    {
        if (::close(fds_stdout[1]) == -1)
            return spawn_status::errno_close();
        if (::close(fds_stderr[1]) == -1)
            return spawn_status::errno_close();

        if (GEPD::fds.reserve(GEPD::nfds + 2) == false)
            ::exit(spawn_status::out_of_memory);
        size_t const index_stdout = GEPD::nfds;
        size_t const index_stderr = GEPD::nfds + 1;
        GEPD::fds[index_stdout].fd = fds_stdout[0];
        GEPD::fds[index_stdout].events = POLLIN | POLLPRI;
        GEPD::fds[index_stdout].revents = 0;
        GEPD::fds[index_stderr].fd = fds_stderr[0];
        GEPD::fds[index_stderr].events = POLLIN | POLLPRI;
        GEPD::fds[index_stderr].revents = 0;
        GEPD::nfds += 2;

        copy_ptr<process_data> P(new process_data(pid,
                                                  index_stdout,
                                                  index_stderr));
        processes.push_back(P);
    }
    return pid;
}

int main()
{
    ::signal(SIGPIPE, SIG_IGN); // write to a broken socket error
    typedef std::vector< copy_ptr<process_data> >::iterator iterator;
    assert(spawn_status::last_value == GEPD::ExitStatus::min);

    realloc_ptr<unsigned char> erlang_buffer(32768, 4194304); // 4MB
    realloc_ptr<unsigned char> stream1(1, 16384);
    realloc_ptr<unsigned char> stream2(1, 16384);
    int status;
    if ((status = GEPD::init()))
        return status;
    int count;
    while ((status = GEPD::wait(count, erlang_buffer,
                                stream1, stream2)) == GEPD::ExitStatus::ready)
    {
        iterator itr = processes.begin();
        while (itr != processes.end() && count > 0)
        {
            if ((status = (*itr)->check(count, erlang_buffer)))
            {
                if (status != GEPD::ExitStatus::error_HUP)
                    return status;
                if ((status = (*itr)->flush(count, erlang_buffer)))
                    return status;
                iterator const dead = itr;
                (*dead)->close();
                for (++itr; itr != processes.end(); ++itr)
                    (*itr)->shift();
                itr = processes.erase(dead);
            }
            else
            {
                ++itr;
            }
        }
    }
    // kill all remaining processes with SIGKILL
    for (iterator itr = processes.begin(); itr != processes.end(); ++itr)
    {
        (*itr)->kill();
    }
    processes.clear();
    return status;
}

