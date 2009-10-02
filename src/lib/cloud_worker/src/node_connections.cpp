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
#include "node_connections.hpp"
#include "port_main.h"
#include "worker_controller.hpp"
#include "cloud_worker_common.hpp"
#include <ei.h>
#include <netdb.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <poll.h>
#include <assert.h>
#include <unistd.h>
#include <string>
#include <cstring>
#include <iostream>
#include <sstream>
#include <boost/algorithm/string/replace.hpp>

// relevant documentation:
// http://erlang.org/doc/apps/erl_interface/index.html
// http://erlang.org/doc/tutorial/cnode.html

// older documentation that is also a good guide:
// http://erlang.org/documentation/doc-5.6.2/lib/erl_interface-3.5.6/doc/html/part_erl_interface_frame.html
// http://erlang.org/doc/apps/erl_interface/ref_man_erl_interface_frame.html
// http://erlang.org/doc/apps/erl_interface/part_ei_frame.html

// class static variables
bool NodeConnections::m_initialized = false;
std::string NodeConnections::m_hostName;
std::string NodeConnections::m_parentProcessName;
std::string NodeConnections::m_nodeNamePrefix;
std::string NodeConnections::m_shortName;
std::string NodeConnections::m_longName;
std::string NodeConnections::m_cookie;
int32_t NodeConnections::m_port;
int16_t NodeConnections::m_instance;

// static file-scope variables (avoid header file dependencies)
static int stderrDup[2] = {-1, -1};
static int serverSocket;
static int epmdSocket;
static ei_cnode serverCNode; // ei library data for server socket as a C node
static nfds_t pollDescriptorsCount = 1;
// POLL_DESC_INDEX 0 is stdin and is handled by GEPD
#define POLL_DESC_INDEX_STDERR               1
#define POLL_DESC_INDEX_EVENT_PIPE           2
#define POLL_DESC_INDEX_EPMD_SOCKET          3
#define POLL_DESC_INDEX_LISTEN_SOCKET        4
static nfds_t const pollDescriptorsNode0 = 5;
// only allow 1 Erlang node connection right now
static int const maxCNodeConnections = pollDescriptorsNode0 + 1;
static struct pollfd 
    pollDescriptors[maxCNodeConnections] = {
        {PORT_READ_FILE_DESCRIPTOR, POLLIN | POLLPRI, 0}
    };
static std::string nodeNames[maxCNodeConnections];


static std::string erl_errno_as_string(char const * prefix)
{
    std::ostringstream stream;
    char msg[256];
    strerror_r(erl_errno, msg, sizeof(msg));
    stream << prefix << msg;
    return stream.str();
}

static void send_erl_errno_to_stderr(char const * prefix)
{
    char msg[256];
    strerror_r(erl_errno, msg, sizeof(msg));
    std::cerr << prefix << msg << erl_endl;
}

static void send_errno_to_stderr(char const * prefix)
{
    char msg[256];
    strerror_r(errno, msg, sizeof(msg));
    std::cerr << prefix << msg << erl_endl;
}

static in_addr_t get_ip_address(char const * hostname)
{
    struct hostent *host_entry = gethostbyname(hostname);
    assert(host_entry);
    return reinterpret_cast<struct in_addr *>(
        *(host_entry->h_addr_list))->s_addr;
}

static int listen_on_port(int32_t port)
{
    int listen_fd = socket(AF_INET, SOCK_STREAM, 0);
    if (listen_fd < 0)
    {
        return -1;
    }
  
    int on = 1;
    setsockopt(listen_fd, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on));
  
    struct sockaddr_in address;
    memset(reinterpret_cast<void *>(&address), 0, sizeof(address));
    address.sin_family = AF_INET;
    address.sin_port = htons(port);
    address.sin_addr.s_addr = htonl(INADDR_ANY);
  
    struct sockaddr * paddress = reinterpret_cast<struct sockaddr*>(&address);
    if (bind(listen_fd, paddress, sizeof(address)) < 0)
    {
        return -1;
    }
  
    if (listen(listen_fd, 8) < 0)
    {
        return -1;
    }
    return listen_fd;
}

static std::string retrieve_hostname()
{
    char localHostname[256];
    gethostname(localHostname, sizeof(localHostname));
    return std::string(localHostname);
}

bool NodeConnections::initialize(std::string const & nodeNamePrefix,
                                 std::string const & domainName,
                                 std::string const & cookie,
                                 int32_t port,
                                 int16_t instance,
                                 WorkerController & controller)
{
    // erl_endl used for std::cerr output since the stderr is not dup-ed
    // at this point, so the only chance of getting stderr output is
    // when the worker is on the same node that has an open shell.
    // The Erlang shell prefers "\r\n" over "\n".  All other
    // stderr communication assumes "\n" to be consistent with
    // std::endl on Linux.

    // NodeConnections initialization data
    m_hostName = retrieve_hostname();
    // this process is the cnode so it has the 'c' prefix on worker
    // for the Erlang registered process/node name
    m_parentProcessName = nodeNamePrefix;
    boost::algorithm::replace_first(m_parentProcessName, "cworker", "worker");

    m_nodeNamePrefix = nodeNamePrefix;
    m_shortName = nodeNamePrefix + '@' + m_hostName;
    if (domainName.size() > 0)
        m_longName = nodeNamePrefix + '@' + m_hostName + '.' + domainName;
    m_cookie = cookie;
    m_port = port;
    m_instance = instance;

    // initialization rountines
    struct in_addr localAddress;
    localAddress.s_addr = get_ip_address(m_hostName.c_str());
    if (m_longName.size() > 0)
    {
        // register with a long name
        std::string const fullHostName = m_hostName + '.' + domainName;
        if (ei_connect_xinit(&serverCNode,
                             const_cast<char *>(fullHostName.c_str()),
                             const_cast<char *>(m_nodeNamePrefix.c_str()),
                             const_cast<char *>(m_longName.c_str()),
                             &localAddress,
                             const_cast<char *>(m_cookie.c_str()),
                             m_instance) == -1)
        {
            send_erl_errno_to_stderr("ei_connect_xinit: ");
            return false;
        }
    }
    else
    {
        // register with a short name
        if (ei_connect_xinit(&serverCNode,
                             const_cast<char *>(m_hostName.c_str()),
                             const_cast<char *>(m_nodeNamePrefix.c_str()),
                             const_cast<char *>(m_shortName.c_str()),
                             &localAddress,
                             const_cast<char *>(m_cookie.c_str()),
                             m_instance) == -1)
        {
            send_erl_errno_to_stderr("ei_connect_xinit: ");
            return false;
        }
    }
    if ((serverSocket = listen_on_port(m_port)) <= 0)
    {
        std::cerr << "listen_on_port: "
            "failed to bind to port " << m_port << erl_endl;
        return false;
    }
    // unpublish before publishing in case the previous process was not stopped
    ei_unpublish(&serverCNode);
    // uses ERL_EPMD_PORT to determine which Erlang cluster is being used
    epmdSocket = ei_publish(&serverCNode, m_port);
    if (epmdSocket < 0)
    {
        std::string const errorString(erl_errno_as_string("ei_publish: "));
        // assume some bad state is keeping epmd from publish-ing.
        // attempt to unpublish before publish-ing.
        if (ei_unpublish(&serverCNode) == 0)
        {
            epmdSocket = ei_publish(&serverCNode, m_port);
            if (epmdSocket < 0)
            {
                std::cerr << errorString << erl_endl;
                return false;
            }
        }
        else
        {
            std::cerr << errorString << erl_endl;
            return false;
        }
    }
    // make sure the event pipe is valid
    if (controller.get_event_fd() < 0)
    {
        std::cerr << "get_event_fd() failed" << erl_endl;
        return false;
    }
    // setup stderr so that it can be polled and forwarded
    if (stderrDup[0] < 0)
    {
        if (pipe(stderrDup) != 0)
        {
            std::cerr << "pipe() failed" << erl_endl;
            return false;
        }
        dup2(stderrDup[1], 2);
        close(stderrDup[1]);
    }

    assert(pollDescriptorsCount == 1);
    pollDescriptors[POLL_DESC_INDEX_STDERR].fd = stderrDup[0];
    pollDescriptors[POLL_DESC_INDEX_STDERR].events = POLLIN | POLLPRI;
    pollDescriptors[POLL_DESC_INDEX_STDERR].revents = 0;
    pollDescriptors[POLL_DESC_INDEX_EVENT_PIPE].fd = controller.get_event_fd();
    pollDescriptors[POLL_DESC_INDEX_EVENT_PIPE].events = POLLIN | POLLPRI;
    pollDescriptors[POLL_DESC_INDEX_EVENT_PIPE].revents = 0;
    pollDescriptors[POLL_DESC_INDEX_EPMD_SOCKET].fd = epmdSocket;
    pollDescriptors[POLL_DESC_INDEX_EPMD_SOCKET].events = POLLIN | POLLPRI;
    pollDescriptors[POLL_DESC_INDEX_EPMD_SOCKET].revents = 0;
    pollDescriptors[POLL_DESC_INDEX_LISTEN_SOCKET].fd = serverSocket;
    pollDescriptors[POLL_DESC_INDEX_LISTEN_SOCKET].events = POLLIN | POLLPRI;
    pollDescriptors[POLL_DESC_INDEX_LISTEN_SOCKET].revents = 0;
    pollDescriptorsCount = pollDescriptorsNode0;
    assert(pollDescriptorsCount == 5);
    m_initialized = true;
    return true;
}

bool NodeConnections::initialize(std::string const & nodeNamePrefix,
                                 std::string const & cookie,
                                 int32_t port,
                                 int16_t instance,
                                 WorkerController & controller)
{
    return NodeConnections::initialize(nodeNamePrefix, "",
                                       cookie, port, instance,
                                       controller);
}

bool NodeConnections::deinitialize()
{
    if (! m_initialized)
    {
        return false;
    }
    
    for (nfds_t i = pollDescriptorsNode0; i < pollDescriptorsCount; ++i)
        close(pollDescriptors[i].fd);
    close(serverSocket);
    close(epmdSocket);
    pollDescriptorsCount = 1;
    m_shortName.clear();
    m_longName.clear();
    m_initialized = false;
    return true;
}

static int handle_stderr(WorkerController & controller)
{
    short & revents = pollDescriptors[POLL_DESC_INDEX_STDERR].revents;
    if (! revents)
        return 0;
    else if (revents & POLLERR)
        return WorkerController::ExitStatus::stderr_ERR;
    else if (revents & POLLHUP)
        return WorkerController::ExitStatus::stderr_HUP;
    else if (revents & POLLNVAL)
        return WorkerController::ExitStatus::stderr_NVAL;
    revents = 0;

    // collect stderr to send on the socket back to
    // an Erlang cloud_worker_port process
    static realloc_ptr<char> buffer(1024, 4194304); // 4 MB max
    static size_t i = 0;
    ssize_t left = buffer.size() - i;
    ssize_t readBytes;
    while ((readBytes = read(stderrDup[0], &buffer[i], left)) == left &&
           buffer.grow())
    {
        i += left;
        left = buffer.size() - i;
    }
    if (readBytes <= 0 && i == 0)
        return 0;
    i += readBytes; // i is the next index to read at, always

    // only send stderr output before the last newline character
    bool foundNewline = false;
    size_t iNewline = 0;
    for (ssize_t j = i - 1; ! foundNewline && j >= 0; --j)
    {
        if (buffer[j] == '\n')
        {
            foundNewline = true;
            iNewline = j;
        }
    }

    if ((pollDescriptorsCount - pollDescriptorsNode0) > 0 && foundNewline)
    {
        // if there are many Erlang node connections,
        // round-robin the stderr data among them
        static nfds_t fdIndex = 0;
        nfds_t const fdCount = pollDescriptorsCount - pollDescriptorsNode0;
        if (fdIndex >= fdCount)
            fdIndex = 0;
        int const status = controller.sendStderr(&serverCNode,
            pollDescriptors[pollDescriptorsNode0 + fdIndex],
            buffer.get(), iNewline + 1);
        if (++fdIndex == fdCount)
            fdIndex = 0;
        // keep any stderr data not yet sent (waiting for a newline)
        if (iNewline == i - 1)
        {
            i = 0;
        }
        else
        {
            size_t const remainingBytes = i - iNewline - 1;
            buffer.move(iNewline + 1, remainingBytes, 0);
            i = remainingBytes;
        }
        return status;
    }
    else
    {
        return 0;
    }

}

static int handle_event_pipe(WorkerController & controller)
{
    short & revents = pollDescriptors[POLL_DESC_INDEX_EVENT_PIPE].revents;
    if (! revents)
        return 0;
    else if (revents & POLLERR)
        return WorkerController::ExitStatus::event_pipe_ERR;
    else if (revents & POLLHUP)
        return WorkerController::ExitStatus::event_pipe_HUP;
    else if (revents & POLLNVAL)
        return WorkerController::ExitStatus::event_pipe_NVAL;
    revents = 0;
    return controller.send(&serverCNode,
        &(pollDescriptors[pollDescriptorsNode0]),
        pollDescriptorsCount - pollDescriptorsNode0, false);
}

static int handle_epmd_socket()
{
    short & revents = pollDescriptors[POLL_DESC_INDEX_EPMD_SOCKET].revents;
    if (! revents)
        return 0;
    else if (revents & POLLERR)
        return WorkerController::ExitStatus::epmd_socket_ERR;
    else if (revents & POLLHUP)
        return WorkerController::ExitStatus::epmd_socket_HUP;
    else if (revents & POLLNVAL)
        return WorkerController::ExitStatus::epmd_socket_NVAL;
    revents = 0;

    char buffer[1024];
    read(pollDescriptors[POLL_DESC_INDEX_EPMD_SOCKET].fd,
         buffer, sizeof(buffer));
    // consume the data and ignore it
    std::cerr << "epmd wants to say something, don't know why" << std::endl;
    return 0;
}

// handle c node server socket
// accept new connections from Erlang nodes
static int handle_server_socket()
{
    short & revents = pollDescriptors[POLL_DESC_INDEX_LISTEN_SOCKET].revents;
    if (! revents)
        return 0;
    else if (revents & POLLERR)
        return WorkerController::ExitStatus::server_socket_ERR;
    else if (revents & POLLHUP)
        return WorkerController::ExitStatus::server_socket_HUP;
    else if (revents & POLLNVAL)
        return WorkerController::ExitStatus::server_socket_NVAL;
    revents = 0;

    ErlConnect connection;
    int fd = ei_accept(&serverCNode, serverSocket, &connection);
    if (fd == ERL_ERROR)
    {
        if (pollDescriptorsCount == pollDescriptorsNode0)
            send_erl_errno_to_stderr("ei_accept: ");
        else
            std::cerr << erl_errno_as_string("ei_accept: ") << std::endl;
        return WorkerController::ExitStatus::server_socket_accept;
    }
    if (pollDescriptorsCount == sizeof(pollDescriptors))
    {
        close(fd);
        std::cerr << "too many connections (" << sizeof(pollDescriptors) <<
            ") discarding a new one" << std::endl;
        return 0; // ignore too many connections
    }

    // a new node connection
    nodeNames[pollDescriptorsCount] = connection.nodename;
    pollDescriptors[pollDescriptorsCount].fd = fd;
    pollDescriptors[pollDescriptorsCount].events = POLLIN | POLLPRI;
    pollDescriptors[pollDescriptorsCount].revents = 0;
    pollDescriptorsCount++;
    return 0;
}

// handle Erlang node connections
static int handle_node_connection(int index, WorkerController & controller)
{
    short & revents = pollDescriptors[index].revents;
    if (! revents)
    {
        return 0;
    }
    bool removeNode = revents & (POLLERR | POLLHUP | POLLNVAL);
    revents = 0;
    int status = 0;
    if (! removeNode)
    {
        status = controller.receive(pollDescriptors[index].fd);
        if (status == WorkerController::ExitStatus::node_receive_EIO)
        {
            // the Erlang node has exited
            status = 0;
            removeNode = true;
        }
    }
    if (removeNode)
    {
        // remove the ill-mannered node
        pollDescriptorsCount--;
        int const last = pollDescriptorsCount;
        close(pollDescriptors[last].fd);
        if (index != last)
        {
            pollDescriptors[index] = pollDescriptors[last];
            nodeNames[index] = nodeNames[last];
        }
        return 0;
    }
    return status;
}

// main event loop for the open sockets and pipes
int NodeConnections::worker_loop(unsigned char * buffer,
                                 WorkerController & controller)
{
    int status = 0;
    while ((status = GEPD::polling_main(pollDescriptors,
                                        pollDescriptorsCount,
                                        controller.timeout_value(),
                                        buffer)) > 0 &&
           (status == GEPD::ExitStatus::external_fd_ready ||
            status == GEPD::ExitStatus::poll_timeout))
    {
        if (! m_initialized)
        {
            // timeout expired after the port was deinitialized
            assert(pollDescriptorsCount == 1);
            continue;
        }
        if (status == GEPD::ExitStatus::poll_timeout)
        {
            status = controller.send(&serverCNode,
                &(pollDescriptors[pollDescriptorsNode0]),
                pollDescriptorsCount - pollDescriptorsNode0, true);
            if (status)
                return status;
            continue;
        }
        if ((status = handle_stderr(controller)))
            return status;
        if ((status = handle_event_pipe(controller)))
            return status;
        if ((status = handle_epmd_socket()))
            return status;
        if ((status = handle_server_socket()))
            return status;
        for (nfds_t i = pollDescriptorsNode0; i < pollDescriptorsCount; ++i)
        {
            if ((status = handle_node_connection(i, controller)))
                return status;
        }
    }
    return status;
}

