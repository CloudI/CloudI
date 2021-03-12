//-*-Mode:javascript;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=javascript fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
//
// MIT License
//
// Copyright (c) 2014-2021 Michael Truog <mjtruog at protonmail dot com>
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

var CloudI = require('CloudI.js').CloudI;
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
Task.prototype.request = function (request_type, name, pattern,
                                   request_info, request,
                                   timeout, priority, trans_id, pid) {
    var Task = this;
    var i = (new Uint32Array((new Uint8Array([request[0],
                                              request[1],
                                              request[2],
                                              request[3]])).buffer))[0];
    if (i == 1073741823) {
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
    Task._api.forward_(request_type, DESTINATION, request_info, request,
                       timeout, priority, trans_id, pid);
};

assert(CloudI.API.thread_count() == 1);
var thread = new Task(0);
thread.run();

