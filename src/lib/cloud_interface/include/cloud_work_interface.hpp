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
#ifndef CLOUD_WORK_INTERFACE_HPP
#define CLOUD_WORK_INTERFACE_HPP

#include "cloud_work_interface_data.hpp"
#include <stdint.h>
#include <boost/scoped_array.hpp>

extern "C"
{

/// Called when the work type is loaded (i.e., started)
void initialize() __attribute__ ((constructor));

/// Called when the work type is unloaded (i.e., finished)
void deinitialize() __attribute__ ((destructor));

/// Called to perform a task
/// @param abort check to see if the thread of execution should abort
/// @param failureCount how many times this task has failed
/// @param machineName the hostname (with domain if using long node names)
/// @param id unique thread of execution identifier
///           (unique across all work task instances on all nodes)
/// @param totalIds the total number of concurrent work task instances
///                 on all nodes with the current configuration
/// @param taskData data specific to this task instance
/// @param taskDataSize size of taskData
/// @param queriesIn data query specifications for input data
/// @param queriesOut data query specifications for output data
/// @return bool if the work task was processed successfully, true else false
bool do_work(bool const & abortTask,
             uint32_t const failureCount,
             std::string const & machineName,
             uint32_t const id, uint32_t const totalIds,
             boost::scoped_array<uint8_t> const & taskData,
             size_t const taskDataSize,
             DatabaseQueryVector const & queriesIn,
             DatabaseQueryVector & queriesOut);

}

#endif // CLOUD_WORK_INTERFACE_HPP

