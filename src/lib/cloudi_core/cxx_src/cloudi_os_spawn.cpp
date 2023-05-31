//-*-Mode:C++;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=cpp fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
//
// MIT License
//
// Copyright (c) 2011-2023 Michael Truog <mjtruog at protonmail dot com>
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

        char const * signal_to_string(int signal)
        {
            // only signals consistent among all platforms
            // use a specific string
            switch (signal)
            {
                case 1:
                    return "SIGHUP";
                case 2:
                    return "SIGINT";
                case 3:
                    return "SIGQUIT";
                case 4:
                    return "SIGILL";
                case 5:
                    return "SIGTRAP";
                case 6:
                    return "SIGABRT";
                case 8:
                    return "SIGFPE";
                case 9:
                    return "SIGKILL";
                case 11:
                    return "SIGSEGV";
                case 13:
                    return "SIGPIPE";
                case 14:
                    return "SIGALRM";
                case 15:
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
                        // spawn exit status specific to the forked filename
                        // (the external service's source code)
                        if (status > 128)
                        {
                            int const signal = status - 128;
                            char const * const signal_name =
                                spawn_status::signal_to_string(signal);
                            if (signal_name)
                            {
                                std::cerr << "OS pid " << m_pid <<
                                    " exited with " <<
                                    signal_name << std::endl;
                            }
                            else
                            {
                                std::cerr << "OS pid " << m_pid <<
                                    " exited with " <<
                                    "SIG#" << signal << std::endl;
                            }
                        }
                        else
                        {
                            std::cerr << "OS pid " << m_pid <<
                                " exited with " << status << std::endl;
                        }
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
    typedef std::vector< copy_ptr<process_data> >::iterator iterator;

    int processes_events(realloc_ptr<unsigned char> & erlang_buffer,
                         realloc_ptr<unsigned char> & stream1,
                         realloc_ptr<unsigned char> & stream2)
    {
        int status;
        if ((status = GEPD::init()))
            return status;
        int count;
        while ((status = GEPD::wait(count,
                                    erlang_buffer,
                                    stream1,
                                    stream2)) == GEPD::ExitStatus::ready)
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
        return status;
    }

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
    assert_initialize();
    assert(spawn_status::last_value == GEPD::ExitStatus::min);
    ::signal(SIGPIPE, SIG_IGN); // write to a broken socket error
    realloc_ptr<unsigned char> erlang_buffer(32768, 4194304); // 4MB
    realloc_ptr<unsigned char> stream1(1, 16384);
    realloc_ptr<unsigned char> stream2(1, 16384);
    int const status = processes_events(erlang_buffer, stream1, stream2);

    // kill all remaining processes with SIGKILL
    for (iterator itr = processes.begin(); itr != processes.end(); ++itr)
    {
        (*itr)->kill();
    }
    processes.clear();
    return status;
}

