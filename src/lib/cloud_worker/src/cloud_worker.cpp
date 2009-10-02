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
#include <erl_interface.h>
#include "cloud_worker.hpp"
#include "node_connections.hpp"
#include "worker_controller.hpp"

// control work requests, work task execution, and work results
static WorkerController controller;

bool keep_alive()
{
    return true;
}

bool worker_start(char * nodeNameStr, uint32_t nodeNameLen,
                  char * domainNameStr, uint32_t domainNameLen,
                  char * cookieStr, uint32_t cookieLen,
                  int32_t port, int16_t instance)
{
    std::string const nodeName(nodeNameStr, nodeNameLen); // the "alive"
    std::string const domainName(domainNameStr, domainNameLen);
    std::string const cookie(cookieStr, cookieLen);
    return NodeConnections::initialize(nodeName, domainName, cookie,
                                       port, instance, controller);
}

bool worker_start(char * nodeNameStr, uint32_t nodeNameLen,
                  char * cookieStr, uint32_t cookieLen,
                  int32_t port, int16_t instance)
{
    std::string const nodeName(nodeNameStr, nodeNameLen); // the "alive"
    std::string const cookie(cookieStr, cookieLen);
    return NodeConnections::initialize(nodeName, cookie,
                                       port, instance, controller);
}

bool worker_stop()
{
    bool const success = controller.clear_work();
    return (NodeConnections::deinitialize() && success);
}

bool add_work(char * workTitleStr, uint32_t workTitleLen,
              uint32_t concurrentTaskIdOffset, uint32_t concurrentTasks)
{
    std::string const workTitle(workTitleStr, workTitleLen);
    return controller.add_work(workTitle,
                               concurrentTaskIdOffset, concurrentTasks);
}

bool has_work(char * workTitleStr, uint32_t workTitleLen,
              uint32_t concurrentTaskIdOffset, uint32_t concurrentTasks)
{
    std::string const workTitle(workTitleStr, workTitleLen);
    return controller.has_work(workTitle,
                               concurrentTaskIdOffset, concurrentTasks);
}

bool remove_work(char * workTitleStr, uint32_t workTitleLen)
{
    std::string const workTitle(workTitleStr, workTitleLen);
    return controller.remove_work(workTitle);
}

int main(int, char **argv)
{
    // legacy arguments, the arguments are now unused
    // (the function may now be unnecessary)
    erl_init(0, 0);

    // erlang port communication buffer
    // (using the option {packet, 2} for open_port/2 within GEPD)
    unsigned char buffer[32768];
   
    controller.setCurrentPath(argv[0]);
    int const status = NodeConnections::worker_loop(buffer, controller);
    NodeConnections::deinitialize();
    return status;
}

