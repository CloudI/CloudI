//-*-Mode:javascript;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=javascript fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
//
// MIT License
//
// Copyright (c) 2014-2022 Michael Truog <mjtruog at protonmail dot com>
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

var bufferAlloc;
if (CloudI.nodejs_version_after('5.10.0',true)) {
    bufferAlloc = Buffer.alloc;
}
else {
    bufferAlloc = Buffer;
}
Task = function Task (thread_index) {
    var Task = this;
    Task._thread_index = thread_index;
};
Task.prototype.run = function () {
    var Task = this;
    try {
        new CloudI.API(Task._thread_index, function(api) {
        Task._api = api;
        Task._api.subscribe('a/b/c/d', Task, Task.sequence1_abcd, function () {
        Task._api.subscribe('a/b/c/*', Task, Task.sequence1_abc_, function () {
        Task._api.subscribe('a/b/*/d', Task, Task.sequence1_ab_d, function () {
        Task._api.subscribe('a/*/c/d', Task, Task.sequence1_a_cd, function () {
        Task._api.subscribe('*/b/c/d', Task, Task.sequence1__bcd, function () {
        Task._api.subscribe('a/b/*',   Task, Task.sequence1_ab__, function () {
        Task._api.subscribe('a/*/d',   Task, Task.sequence1_a__d, function () {
        Task._api.subscribe('*/c/d',   Task, Task.sequence1___cd, function () {
        Task._api.subscribe('a/*',     Task, Task.sequence1_a___, function () {
        Task._api.subscribe('*/d',     Task, Task.sequence1____d, function () {
        Task._api.subscribe('*',       Task, Task.sequence1_____, function () {
        Task._api.subscribe('sequence1', Task, Task.sequence1, function () {
        Task._api.subscribe('e', Task, Task.sequence2_e1, function () {
        Task._api.subscribe('e', Task, Task.sequence2_e2, function () {
        Task._api.subscribe('e', Task, Task.sequence2_e3, function () {
        Task._api.subscribe('e', Task, Task.sequence2_e4, function () {
        Task._api.subscribe('e', Task, Task.sequence2_e5, function () {
        Task._api.subscribe('e', Task, Task.sequence2_e6, function () {
        Task._api.subscribe('e', Task, Task.sequence2_e7, function () {
        Task._api.subscribe('e', Task, Task.sequence2_e8, function () {
        Task._api.subscribe('sequence2', Task, Task.sequence2, function () {
        Task._api.subscribe('f1', Task, Task.sequence3_f1, function () {
        Task._api.subscribe('f2', Task, Task.sequence3_f2, function () {
        Task._api.subscribe('g1', Task, Task.sequence3_g1, function () {
        Task._api.subscribe('sequence3', Task, Task.sequence3, function () {
        if (Task._api.process_index() == 0) {
            Task._api.send_async(Task._api.prefix() + 'sequence1', '1');
        }
        Task._api.poll(function (timeout) {
            assert(timeout == false);
            process.stdout.write('terminate messaging javascript\n');
        });
        });});});});});});});});});});});});});
        });});});});});});});});});});});});});
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
Task.prototype.sequence1_abcd = function (request_type, name, pattern,
                                          request_info, request,
                                          timeout, priority, trans_id, source) {
    assert(pattern == this._api.prefix() + 'a/b/c/d');
    assert(request.toString('binary') == 'test1');
    this._api.return_(request_type, name, pattern,
                      '', request, timeout, trans_id, source);
};
Task.prototype.sequence1_abc_ = function (request_type, name, pattern,
                                          request_info, request,
                                          timeout, priority, trans_id, source) {
    assert(pattern == this._api.prefix() + 'a/b/c/*');
    assert(request.toString('binary') == 'test2' ||
           request.toString('binary') == 'test3');
    this._api.return_(request_type, name, pattern,
                      '', request, timeout, trans_id, source);
};
Task.prototype.sequence1_ab_d = function (request_type, name, pattern,
                                          request_info, request,
                                          timeout, priority, trans_id, source) {
    assert(pattern == this._api.prefix() + 'a/b/*/d');
    assert(request.toString('binary') == 'test4' ||
           request.toString('binary') == 'test5');
    this._api.return_(request_type, name, pattern,
                      '', request, timeout, trans_id, source);
};
Task.prototype.sequence1_a_cd = function (request_type, name, pattern,
                                          request_info, request,
                                          timeout, priority, trans_id, source) {
    assert(pattern == this._api.prefix() + 'a/*/c/d');
    assert(request.toString('binary') == 'test6' ||
           request.toString('binary') == 'test7');
    this._api.return_(request_type, name, pattern,
                      '', request, timeout, trans_id, source);
};
Task.prototype.sequence1__bcd = function (request_type, name, pattern,
                                          request_info, request,
                                          timeout, priority, trans_id, source) {
    assert(pattern == this._api.prefix() + '*/b/c/d');
    assert(request.toString('binary') == 'test8' ||
           request.toString('binary') == 'test9');
    this._api.return_(request_type, name, pattern,
                      '', request, timeout, trans_id, source);
};
Task.prototype.sequence1_ab__ = function (request_type, name, pattern,
                                          request_info, request,
                                          timeout, priority, trans_id, source) {
    assert(pattern == this._api.prefix() + 'a/b/*');
    assert(request.toString('binary') == 'test10');
    this._api.return_(request_type, name, pattern,
                      '', request, timeout, trans_id, source);
};
Task.prototype.sequence1_a__d = function (request_type, name, pattern,
                                          request_info, request,
                                          timeout, priority, trans_id, source) {
    assert(pattern == this._api.prefix() + 'a/*/d');
    assert(request.toString('binary') == 'test11');
    this._api.return_(request_type, name, pattern,
                      '', request, timeout, trans_id, source);
};
Task.prototype.sequence1___cd = function (request_type, name, pattern,
                                          request_info, request,
                                          timeout, priority, trans_id, source) {
    assert(pattern == this._api.prefix() + '*/c/d');
    assert(request.toString('binary') == 'test12');
    this._api.return_(request_type, name, pattern,
                      '', request, timeout, trans_id, source);
};
Task.prototype.sequence1_a___ = function (request_type, name, pattern,
                                          request_info, request,
                                          timeout, priority, trans_id, source) {
    assert(pattern == this._api.prefix() + 'a/*');
    assert(request.toString('binary') == 'test13');
    this._api.return_(request_type, name, pattern,
                      '', request, timeout, trans_id, source);
};
Task.prototype.sequence1____d = function (request_type, name, pattern,
                                          request_info, request,
                                          timeout, priority, trans_id, source) {
    assert(pattern == this._api.prefix() + '*/d');
    assert(request.toString('binary') == 'test14');
    this._api.return_(request_type, name, pattern,
                      '', request, timeout, trans_id, source);
};
Task.prototype.sequence1_____ = function (request_type, name, pattern,
                                          request_info, request,
                                          timeout, priority, trans_id, source) {
    assert(pattern == this._api.prefix() + '*');
    assert(request.toString('binary') == 'test15');
    this._api.return_(request_type, name, pattern,
                      '', request, timeout, trans_id, source);
};
Task.prototype.sequence1 = function (request_type, name, pattern,
                                     request_info, request,
                                     timeout, priority, trans_id, source) {
    var Task = this;
    var old_request;
    old_request = function (tmp, old_response, tmp) {
        if (old_response == 'end') {
            process.nextTick(function () {
                Task._api.recv_async(old_request, 1000);
            });
            return;
        }
        CloudI.stdout_write('messaging sequence1 start javascript (' +
                            request + ')\n');
        Task._api.send_async(Task._api.prefix() + 'a/b/c/d',  'test1',
                             function (test1_id) {
        Task._api.send_async(Task._api.prefix() + 'a/b/c/z',  'test2',
                             function (test2_id) {
        Task._api.send_async(Task._api.prefix() + 'a/b/c/dd', 'test3',
                             function (test3_id) {
        Task._api.send_async(Task._api.prefix() + 'a/b/z/d',  'test4',
                             function (test4_id) {
        Task._api.send_async(Task._api.prefix() + 'a/b/cc/d', 'test5',
                             function (test5_id) {
        Task._api.send_async(Task._api.prefix() + 'a/z/c/d',  'test6',
                             function (test6_id) {
        Task._api.send_async(Task._api.prefix() + 'a/bb/c/d', 'test7',
                             function (test7_id) {
        Task._api.send_async(Task._api.prefix() + 'z/b/c/d',  'test8',
                             function (test8_id) {
        Task._api.send_async(Task._api.prefix() + 'aa/b/c/d', 'test9',
                             function (test9_id) {
        Task._api.send_async(Task._api.prefix() + 'a/b/czd',  'test10',
                             function (test10_id) {
        Task._api.send_async(Task._api.prefix() + 'a/bzc/d',  'test11',
                             function (test11_id) {
        Task._api.send_async(Task._api.prefix() + 'azb/c/d',  'test12',
                             function (test12_id) {
        Task._api.send_async(Task._api.prefix() + 'a/bzczd',  'test13',
                             function (test13_id) {
        Task._api.send_async(Task._api.prefix() + 'azbzc/d',  'test14',
                             function (test14_id) {
        Task._api.send_async(Task._api.prefix() + 'azbzczd',  'test15',
                             function (test15_id) {
        // n.b., depends on cloudi_core_i_constants.hrl having
        // RECV_ASYNC_STRATEGY == recv_async_select_oldest
        Task._api.recv_async(undefined, undefined, undefined, false);
        Task._api.recv_async(function (tmp, test1_check, test1_id_check) {
            assert(test1_check.toString('binary') == 'test1');
            assert(test1_id_check.toString('binary') ==
                   test1_id.toString('binary'));
        Task._api.recv_async(undefined, undefined, undefined, false);
        Task._api.recv_async(function (tmp, test2_check, test2_id_check) {
            assert(test2_check.toString('binary') == 'test2');
            assert(test2_id_check.toString('binary') ==
                   test2_id.toString('binary'));
        Task._api.recv_async(undefined, undefined, undefined, false);
        Task._api.recv_async(function (tmp, test3_check, test3_id_check) {
            assert(test3_check.toString('binary') == 'test3');
            assert(test3_id_check.toString('binary') ==
                   test3_id.toString('binary'));
        Task._api.recv_async(undefined, undefined, undefined, false);
        Task._api.recv_async(function (tmp, test4_check, test4_id_check) {
            assert(test4_check.toString('binary') == 'test4');
            assert(test4_id_check.toString('binary') ==
                   test4_id.toString('binary'));
        Task._api.recv_async(undefined, undefined, undefined, false);
        Task._api.recv_async(function (tmp, test5_check, test5_id_check) {
            assert(test5_check.toString('binary') == 'test5');
            assert(test5_id_check.toString('binary') ==
                   test5_id.toString('binary'));
        Task._api.recv_async(undefined, undefined, undefined, false);
        Task._api.recv_async(function (tmp, test6_check, test6_id_check) {
            assert(test6_check.toString('binary') == 'test6');
            assert(test6_id_check.toString('binary') ==
                   test6_id.toString('binary'));
        Task._api.recv_async(undefined, undefined, undefined, false);
        Task._api.recv_async(function (tmp, test7_check, test7_id_check) {
            assert(test7_check.toString('binary') == 'test7');
            assert(test7_id_check.toString('binary') ==
                   test7_id.toString('binary'));
        Task._api.recv_async(undefined, undefined, undefined, false);
        Task._api.recv_async(function (tmp, test8_check, test8_id_check) {
            assert(test8_check.toString('binary') == 'test8');
            assert(test8_id_check.toString('binary') ==
                   test8_id.toString('binary'));
        Task._api.recv_async(undefined, undefined, undefined, false);
        Task._api.recv_async(function (tmp, test9_check, test9_id_check) {
            assert(test9_check.toString('binary') == 'test9');
            assert(test9_id_check.toString('binary') ==
                   test9_id.toString('binary'));
        Task._api.recv_async(undefined, undefined, undefined, false);
        Task._api.recv_async(function (tmp, test10_check, test10_id_check) {
            assert(test10_check.toString('binary') == 'test10');
            assert(test10_id_check.toString('binary') ==
                   test10_id.toString('binary'));
        Task._api.recv_async(undefined, undefined, undefined, false);
        Task._api.recv_async(function (tmp, test11_check, test11_id_check) {
            assert(test11_check.toString('binary') == 'test11');
            assert(test11_id_check.toString('binary') ==
                   test11_id.toString('binary'));
        Task._api.recv_async(undefined, undefined, undefined, false);
        Task._api.recv_async(function (tmp, test12_check, test12_id_check) {
            assert(test12_check.toString('binary') == 'test12');
            assert(test12_id_check.toString('binary') ==
                   test12_id.toString('binary'));
        Task._api.recv_async(undefined, undefined, undefined, false);
        Task._api.recv_async(function (tmp, test13_check, test13_id_check) {
            assert(test13_check.toString('binary') == 'test13');
            assert(test13_id_check.toString('binary') ==
                   test13_id.toString('binary'));
        Task._api.recv_async(undefined, undefined, undefined, false);
        Task._api.recv_async(function (tmp, test14_check, test14_id_check) {
            assert(test14_check.toString('binary') == 'test14');
            assert(test14_id_check.toString('binary') ==
                   test14_id.toString('binary'));
        Task._api.recv_async(undefined, undefined, undefined, false);
        Task._api.recv_async(function (tmp, test15_check, test15_id_check) {
            assert(test15_check.toString('binary') == 'test15');
            assert(test15_id_check.toString('binary') ==
                   test15_id.toString('binary'));
        CloudI.stdout_write('messaging sequence1 end javascript (' +
                            request + ')\n');
        // start sequence2
        Task._api.send_async(Task._api.prefix() + 'sequence2', request);
        Task._api.return_(request_type, name, pattern,
                          '', 'end', timeout, trans_id, source);
        });});});});});});});});});});});});});});});
        });});});});});});});});});});});});});});});
    };
    Task._api.recv_async(old_request, 1000);
};
Task.prototype.sequence2_e1 = function (request_type, name, pattern,
                                        request_info, request,
                                        timeout, priority, trans_id, source) {
    this._api.return_(request_type, name, pattern,
                      '', '1', timeout, trans_id, source);
};
Task.prototype.sequence2_e2 = function (request_type, name, pattern,
                                        request_info, request,
                                        timeout, priority, trans_id, source) {
    this._api.return_(request_type, name, pattern,
                      '', '2', timeout, trans_id, source);
};
Task.prototype.sequence2_e3 = function (request_type, name, pattern,
                                        request_info, request,
                                        timeout, priority, trans_id, source) {
    this._api.return_(request_type, name, pattern,
                      '', '3', timeout, trans_id, source);
};
Task.prototype.sequence2_e4 = function (request_type, name, pattern,
                                        request_info, request,
                                        timeout, priority, trans_id, source) {
    this._api.return_(request_type, name, pattern,
                      '', '4', timeout, trans_id, source);
};
Task.prototype.sequence2_e5 = function (request_type, name, pattern,
                                        request_info, request,
                                        timeout, priority, trans_id, source) {
    this._api.return_(request_type, name, pattern,
                      '', '5', timeout, trans_id, source);
};
Task.prototype.sequence2_e6 = function (request_type, name, pattern,
                                        request_info, request,
                                        timeout, priority, trans_id, source) {
    this._api.return_(request_type, name, pattern,
                      '', '6', timeout, trans_id, source);
};
Task.prototype.sequence2_e7 = function (request_type, name, pattern,
                                        request_info, request,
                                        timeout, priority, trans_id, source) {
    this._api.return_(request_type, name, pattern,
                      '', '7', timeout, trans_id, source);
};
Task.prototype.sequence2_e8 = function (request_type, name, pattern,
                                        request_info, request,
                                        timeout, priority, trans_id, source) {
    this._api.return_(request_type, name, pattern,
                      '', '8', timeout, trans_id, source);
};
Task.prototype.sequence2 = function (request_type, name, pattern,
                                     request_info, request,
                                     timeout, priority, trans_id, source) {
    var Task = this;
    CloudI.stdout_write('messaging sequence2 start javascript (' +
                        request + ')\n');
    var loop;
    loop = function () {
        // the sending process is excluded from the services that receive
        // the asynchronous message, so in Task case, the receiving thread
        // will not be called, despite the fact it has subscribed to 'e',
        // to prevent a process (in Task case thread) from deadlocking
        // with itself.
        Task._api.mcast_async(Task._api.prefix() + 'e', ' ',
                              function (e_ids) {
        // 4 * 8 == 32, but only 3 out of 4 threads can receive messages,
        // since 1 thread is sending the mcast_async, so 3 * 8 == 24
        if (e_ids.length == 24) {
            e_check_buffer = new bufferAlloc(24);
            e_check_buffer.fill(0);
            for (var i = 0; i < e_ids.length; i++) {
                var cb = function (i) {
                return function (tmp, e_check, e_id_check) {
                assert(e_ids[i].toString('binary') ==
                       e_id_check.toString('binary'));
                var index = (e_check[0] - '1'.charCodeAt(0)) * 3;
                for (var offset = 0; offset < 3; offset++) {
                    if (e_check_buffer[index + offset] == 0) {
                        e_check_buffer[index + offset] = e_check[0];
                        break;
                    }
                }
                if (i == (e_ids.length - 1)) {
                    assert(e_check_buffer.toString('binary') ==
                           '111222333444555666777888');
                    CloudI.stdout_write('messaging sequence2 end javascript (' +
                                        request + ')\n');
                    // start sequence3
                    Task._api.send_async(Task._api.prefix() + 'sequence3',
                                         request);
                    Task._api.return_(request_type, name, pattern,
                                      '', 'end', timeout, trans_id, source);
                }
                };};
                Task._api.recv_async(cb(i), undefined, e_ids[i]);
            }
        }
        else {
            var waiting = 4 - e_ids.length / 8.0;
            CloudI.stdout_write('Waiting for ' + waiting +
                                'services to initialize\n');
            for (var i = 0; i < e_ids.length; i++) {
                var cb = function (i) {
                return function (tmp, e_check, e_id_check) {
                assert(e_ids[i].toString('binary') ==
                       e_id_check.toString('binary'));
                if (i == (e_ids.length - 1)) {
                    Task._api.recv_async(function (tmp, tmp, null_id) {
                    assert(null_id.toString('binary') ==
                           '\x00\x00\x00\x00\x00\x00\x00\x00' +
                           '\x00\x00\x00\x00\x00\x00\x00\x00');
                    loop();
                    }, 1000);
                }
                };};
                Task._api.recv_async(cb(i), undefined, e_ids[i]);
            }
        }
        });
    }
    loop();
};
Task.prototype.sequence3_f1 = function (request_type, name, pattern,
                                        request_info, request,
                                        timeout, priority, trans_id, source) {
    var request_i = parseInt(request);
    if (request_i == 4) {
        return 'done';
    }
    var request_new = request_i + 2; // two steps forward
    this._api.forward_(request_type, this._api.prefix() + 'f2', request_info,
                       '' + request_new, timeout, priority, trans_id, source);
};
Task.prototype.sequence3_f2 = function (request_type, name, pattern,
                                        request_info, request,
                                        timeout, priority, trans_id, source) {
    var request_i = parseInt(request);
    var request_new = request_i - 1; // one step back
    this._api.forward_(request_type, this._api.prefix() + 'f1', request_info,
                       '' + request_new, timeout, priority, trans_id, source);
};
Task.prototype.sequence3_g1 = function (request_type, name, pattern,
                                        request_info, request,
                                        timeout, priority, trans_id, source) {
    this._api.return_(request_type, name, pattern,
                      '', request + 'suffix', timeout, trans_id, source);
};
Task.prototype.sequence3 = function (request_type, name, pattern,
                                     request_info, request,
                                     timeout, priority, trans_id, source) {
    var Task = this;
    CloudI.stdout_write('messaging sequence3 start javascript (' +
                        request + ')\n');
    Task._api.send_async(Task._api.prefix() + 'f1', '0',
                         function (test1_id) {
    Task._api.recv_async(function (tmp, test1_check, test1_id_check) {
    assert(test1_id_check.toString('binary') == test1_id.toString('binary'));
    assert(test1_check.toString('binary') == 'done');
    Task._api.send_sync(Task._api.prefix() + 'g1', 'prefix_',
                        function (tmp, test2_check, test2_id_check) {
    assert(test2_check.toString('binary') == 'prefix_suffix');
    CloudI.stdout_write('messaging sequence3 end javascript (' +
                        request + ')\n');
    // loop to find any infrequent problems, restart sequence1
    var iteration = parseInt(request) + 1;
    if (iteration == 9007199254740991) {
        iteration = 0;
    }
    Task._api.send_async(Task._api.prefix() + 'sequence1', '' + iteration);
    Task._api.return_(request_type, name, pattern,
                      '', 'end', timeout, trans_id, source);
    });
    }, undefined, test1_id);
    });
};

assert(CloudI.API.thread_count() == 1);
var thread = new Task(0);
thread.run();

