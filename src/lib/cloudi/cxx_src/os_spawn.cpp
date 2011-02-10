#include "port.hpp"
#include "realloc_ptr.hpp"
#include "copy_ptr.hpp"
#include "os_spawn.hpp"
#include <vector>
#include <errno.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/socket.h>
#include <signal.h>

namespace
{
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
                m_stream2(1, 16384)
            {
            }

            ~process_data()
            {
                GEPD::nfds -= 2;

                // kills the pid if it isn't dead,
                // to avoid blocking on a closed pipe
                kill(m_pid, 9);
                assert(waitpid(m_pid, NULL, 0) == m_pid);
            }

            void close()
            {
                ::close(GEPD::fds[m_index_stdout].fd);
                ::close(GEPD::fds[m_index_stderr].fd);
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
    };

    std::vector< copy_ptr<process_data> > processes;

    namespace spawn_status
    {
        enum
        {
            success = 0,
            invalid_input = -127,
            out_of_memory,
            pipe_EFAULT,
            pipe_EINVAL,
            pipe_EMFILE,
            pipe_ENFILE,
            pipe_unknown,
            fork_EAGAIN,
            fork_ENOMEM,
            fork_unknown,
            socket_EACCES,
            socket_EAFNOSUPPORT,
            socket_EINVAL,
            socket_EMFILE,
            socket_ENFILE,
            socket_ENOBUFS,
            socket_ENOMEM,
            socket_EPROTONOSUPPORT,
            socket_unknown,
            dup_EBADF,
            dup_EBUSY,
            dup_EINTR,
            dup_EINVAL,
            dup_EMFILE,
            dup_unknown,
            close_EBADF,
            close_EINTR,
            close_EIO,
            close_unknown,
            waitpid_ECHILD,
            waitpid_EINTR,
            waitpid_EINVAL,
            waitpid_unknown,
            connect_EACCES,
            connect_EPERM,
            connect_EADDRINUSE,
            connect_EAFNOSUPPORT,
            connect_EAGAIN,
            connect_EALREADY,
            connect_EBADF,
            connect_ECONNREFUSED,
            connect_EFAULT,
            connect_EINPROGRESS,
            connect_EINTR,
            connect_EISCONN,
            connect_ENETUNREACH,
            connect_ENOTSOCK,
            connect_ETIMEDOUT,
            connect_unknown,
            exec_E2BIG,
            exec_EACCES,
            exec_EFAULT,
            exec_EINVAL,
            exec_EIO,
            exec_EISDIR,
            exec_ELIBBAD,
            exec_ELOOP,
            exec_EMFILE,
            exec_ENAMETOOLONG,
            exec_ENFILE,
            exec_ENOENT,
            exec_ENOEXEC,
            exec_ENOMEM,
            exec_ENOTDIR,
            exec_EPERM,
            exec_ETXTBSY,
            exec_unknown
        };

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

        int errno_waitpid()
        {
            switch (errno)
            {
                case ECHILD:
                    return waitpid_ECHILD;
                case EINTR:
                    return waitpid_EINTR;
                case EINVAL:
                    return waitpid_EINVAL;
                default:
                    return waitpid_unknown;
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
                case EAFNOSUPPORT:
                    return connect_EAFNOSUPPORT;
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
                case ELIBBAD:
                    return exec_ELIBBAD;
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
    }
}

int32_t spawn(char protocol, uint32_t * ports, uint32_t ports_len,
              char * filename, uint32_t filename_len,
              char * argv, uint32_t argv_len,
              char * env, uint32_t env_len)
{
    int type;
    if (protocol == 't') // tcp
    {
        type = SOCK_STREAM;
    }
    else if (protocol == 'u') // udp
    {
        type = SOCK_DGRAM;
    }
    else
    {
        return spawn_status::invalid_input;
    }
    int fds_stdout[2] = {-1, -1};
    int fds_stderr[2] = {-1, -1};
    if (pipe(fds_stdout) == -1)
        return spawn_status::errno_pipe();
    if (pipe(fds_stderr) == -1)
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
            if (close(GEPD::fds[i].fd) == -1)
                _exit(spawn_status::errno_close());
        }
        if (dup2(fds_stdout[1], 1) == -1)
            _exit(spawn_status::errno_dup());
        if (close(fds_stdout[0]) == -1 || close(fds_stdout[1]) == -1)
            _exit(spawn_status::errno_close());
        if (dup2(fds_stderr[1], 2) == -1)
            _exit(spawn_status::errno_dup());
        if (close(fds_stderr[0]) == -1 || close(fds_stderr[1]) == -1)
            _exit(spawn_status::errno_close());

        for (size_t i = 0; i < ports_len; ++i)
        {
            int sockfd = socket(AF_INET, type, 0);
            if (sockfd == -1)
                _exit(spawn_status::errno_socket());
            assert(sockfd == i + 3);
            
            struct sockaddr_in localhost;
            localhost.sin_family = AF_INET;
            localhost.sin_port = htons(ports[i]);
            localhost.sin_addr.s_addr = htonl(INADDR_LOOPBACK);

            if (connect(sockfd,
                        reinterpret_cast<struct sockaddr *>(&localhost),
                        sizeof(localhost)) == -1)
                _exit(spawn_status::errno_connect());

        }

        int argv_count = 2;
        {
            assert(argv[argv_len - 1] == '\0');
            for (size_t i = 1; i < argv_len; ++i)
            {
                if (argv[i] == '\0')
                    ++argv_count;
            }
        }
        char * execve_argv[argv_count];
        {
            int index = 0;
            execve_argv[index++] = filename;
            if (argv_count == 2)
            {
                execve_argv[index++] = 0;
            }
            else
            {
                execve_argv[index++] = argv;
                for (size_t i = 0; i < argv_len - 1; ++i)
                {
                    if (argv[i] == '\0')
                        execve_argv[index++] = &(argv[i + 1]);
                }
                execve_argv[index++] = 0;
                assert(index == argv_count);
            }
        }

        int env_count = 1;
        {
            assert(env[env_len - 1] == '\0');
            for (size_t i = 1; i < env_len; ++i)
            {
                if (env[i] == '\0')
                    ++env_count;
            }
        }
        char * execve_env[env_count];
        {
            if (env_count == 1)
            {
                execve_env[0] = 0;
            }
            else
            {
                int index = 0;
                execve_env[index++] = env;
                for (size_t i = 0; i < env_len - 1; ++i)
                {
                    if (env[i] == '\0')
                        execve_env[index++] = &(env[i + 1]);
                }
                execve_env[index++] = 0;
                assert(index == env_count);
            }
        }

        execve(filename, execve_argv, execve_env);
        _exit(spawn_status::errno_exec());
    }
    else
    {
        if (close(fds_stdout[1]) == -1)
            return spawn_status::errno_close();
        if (close(fds_stderr[1]) == -1)
            return spawn_status::errno_close();

        if (GEPD::fds.reserve(GEPD::nfds + 2) == false)
            _exit(spawn_status::out_of_memory);
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
    typedef std::vector< copy_ptr<process_data> >::iterator iterator;

    int const timeout = -1; // milliseconds
    realloc_ptr<unsigned char> erlang_buffer(32768, 4194304); // 4MB
    realloc_ptr<unsigned char> stream1(1, 16384);
    realloc_ptr<unsigned char> stream2(1, 16384);
    int status;
    if ((status = GEPD::init()))
        return status;
    int count;
    while ((status = GEPD::wait(count, timeout, erlang_buffer,
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
                processes.erase(dead);
                itr = processes.begin();
                continue;
            }
            ++itr;
        }
    }
    return status;
}

