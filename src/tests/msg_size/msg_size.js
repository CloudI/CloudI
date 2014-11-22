//-*-Mode:javascript;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=javascript fenc=utf-8 sts=4 ts=4 sw=4 et:
//
// BSD LICENSE
// 
// Copyright (c) 2014, Michael Truog <mjtruog at gmail dot com>
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

var CloudI = require('../../api/javascript/CloudI.js').CloudI;
var assert = require('assert');

var DESTINATION = '/tests/msg_size/erlang';

Task = function Task (thread_index) {
    var Task = this;
    Task._thread_index = thread_index;
};
Task.prototype.run = function () {
    var Task = this;
    try {
        new CloudI.API(Task._thread_index, function (api) {
        Task._api = api;
        Task._api.subscribe('javascript', Task, Task.request, function () {
        Task._api.poll(function (timeout) {
            assert(timeout == false);
            process.stdout.write('terminate msg_size javascript\n');
        });
        });});
    }
    catch (err) {
        if (typeof err.stack !== 'undefined') {
            process.stderr.write(err.stack + '\n');
        }
        else {
            process.stderr.write(err + '\n');
        }
    }
};
Task.prototype.request = function (command, name, pattern,
                                   request_info, request,
                                   timeout, priority, trans_id, pid) {
    var Task = this;
    var i = (new Uint32Array((new Uint8Array([request[0],
                                              request[1],
                                              request[2],
                                              request[3]])).buffer))[0];
    if (i == 4294967295) {
        i = 0;
    }
    else {
        i += 1;
    }
    var buffer = new Uint8Array((new Uint32Array([i])).buffer);
    request[0] = buffer[0];
    request[1] = buffer[1];
    request[2] = buffer[2];
    request[3] = buffer[3];
    process.stdout.write('forward #' + i + ' javascript to ' + DESTINATION +
                         ' (with timeout ' + timeout + ' ms)\n');
    Task._api.forward_(command, DESTINATION, request_info, request,
                       timeout, priority, trans_id, pid);
};

assert(CloudI.API.thread_count() == 1);
var thread = new Task(0);
thread.run();

