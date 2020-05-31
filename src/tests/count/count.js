//-*-Mode:javascript;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=javascript fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
//
// MIT License
//
// Copyright (c) 2017-2020 Michael Truog <mjtruog at protonmail dot com>
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

Task = function Task (thread_index) {
    var Task = this;
    Task._thread_index = thread_index;
    Task._count = 0;
};
Task.prototype.run = function () {
    var Task = this;
    try {
        new CloudI.API(Task._thread_index, function (api) {
        Task._api = api;
        Task._api.subscribe('javascript/get', Task, Task.request,
                            function () {
        Task._api.poll(function (timeout) {
            assert(timeout == false);
            process.stdout.write('terminate count javascript\n');
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
    if (Task._count == 4294967295) {
        Task._count = 0;
    }
    else {
        Task._count += 1;
    }
    process.stdout.write('count == ' + Task._count + ' javascript\n');
    var response = '' + Task._count;
    var response_info = CloudI.API.info_key_value_new({});
    Task._api.return_(request_type, name, pattern,
                      response_info, response, timeout, trans_id, pid);
};

assert(CloudI.API.thread_count() == 1);
var thread = new Task(0);
thread.run();

