//-*-Mode:javascript;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=javascript fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
//
// MIT License
//
// Copyright (c) 2014-2020 Michael Truog <mjtruog at protonmail dot com>
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

exports.CloudI = new function() {
var CloudI = this; // namespace
'use strict';

var Erlang = require('./Erlang.js').Erlang;
var net = require('net');
var timers = require('timers');
var domain = require('domain');
var fs = require('fs');
var assert = require('assert');

var toNativeString = {}.toString;
CloudI.nodejs_version_after = Erlang.nodejs_version_after;
var bufferFrom;
if (Erlang.nodejs_version_after('5.10.0',true)) {
    bufferFrom = Buffer.from;
}
else {
    bufferFrom = Buffer;
}
var littleEndian = (
    (new Uint16Array((new Uint8Array([0,1])).buffer))[0] === 0x0100);
var unpackUint8 = function unpackUint8 (i, data) {
    return data[i];
};
var unpackInt8 = function unpackInt8 (i, data) {
    var value = data[i];
    if ((0x80 & value) != 0) {
        value = -128 + (value & 0x7f);
    }
    return value;
};
var unpackUint32big = function unpackUint32big (i, data) {
    return (data[i] << 24) |
           (data[i + 1] << 16) |
           (data[i + 2] << 8) |
           data[i + 3];
};
var unpackUint32;
if (littleEndian) {
    unpackUint32 = function unpackUint32 (i, data) {
        return data[i] |
               (data[i + 1] << 8) |
               (data[i + 2] << 16) |
               (data[i + 3] << 24);
    };
}
else {
    unpackUint32 = unpackUint32big;
}
var packUint32big = function packUint32big (value) {
    return new bufferFrom([(value >>> 24) & 0xff,
                           (value >>> 16) & 0xff,
                           (value >>> 8) & 0xff,
                           value & 0xff]);
};
if (Erlang.nodejs_version_after('10.0.0',true)) {
    var originalEmitWarning = process.emitWarning;
    process.emitWarning = function(warning, type, code, ctor) {
        if (code === 'DEP0097') {
            // Ignore the stderr output of the runtime deprecation:
            // "Using a domain property in MakeCallback is deprecated.
            //  Use the async_context variant of MakeCallback or the
            //  AsyncResource class instead."
            //
            // Until the exception handling of the domain module is
            // provided by a different module (like async_hooks) or
            // exceptions (throw/try/catch) are removed from Javascript,
            // the use of the domain module needs to remain
            // (to remain consistent with other CloudI API implementations).
            // The stderr runtime deprecation information is ignored
            // because it provides no useful information and is only an
            // annoyance as a false negative.
            return;
        }
        originalEmitWarning(warning, type, code, ctor);
    };
}

var InvalidInputException = function InvalidInputException () {
    var error = new Error('Invalid Input');
    error.name = 'InvalidInputException';
    this.message = error.message;
    if (error.stack) {
        this.stack = error.stack;
    }
}
InvalidInputException.prototype = Object.create(Error.prototype, {
    name: { value: 'InvalidInputException' }
});
var ReturnSyncException = function ReturnSyncException () {
    var error = new Error('Synchronous Call Return Invalid');
    error.name = 'ReturnSyncException';
    this.message = error.message;
    if (error.stack) {
        this.stack = error.stack;
    }
}
ReturnSyncException.prototype = Object.create(Error.prototype, {
    name: { value: 'ReturnSyncException' }
});
var ReturnAsyncException = function ReturnAsyncException () {
    var error = new Error('Asynchronous Call Return Invalid');
    error.name = 'ReturnAsyncException';
    this.message = error.message;
    if (error.stack) {
        this.stack = error.stack;
    }
}
ReturnAsyncException.prototype = Object.create(Error.prototype, {
    name: { value: 'ReturnAsyncException' }
});
var ForwardSyncException = function ForwardSyncException () {
    var error = new Error('Synchronous Call Forward Invalid');
    error.name = 'ForwardSyncException';
    this.message = error.message;
    if (error.stack) {
        this.stack = error.stack;
    }
}
ForwardSyncException.prototype = Object.create(Error.prototype, {
    name: { value: 'ForwardSyncException' }
});
var ForwardAsyncException = function ForwardAsyncException () {
    var error = new Error('Asynchronous Call Forward Invalid');
    error.name = 'ForwardAsyncException';
    this.message = error.message;
    if (error.stack) {
        this.stack = error.stack;
    }
}
ForwardAsyncException.prototype = Object.create(Error.prototype, {
    name: { value: 'ForwardAsyncException' }
});
var MessageDecodingException = function MessageDecodingException () {
    var error = new Error('Message Decoding Error');
    error.name = 'MessageDecodingException';
    this.message = error.message;
    if (error.stack) {
        this.stack = error.stack;
    }
}
MessageDecodingException.prototype = Object.create(Error.prototype, {
    name: { value: 'MessageDecodingException' }
});
var TerminateException = function TerminateException (timeout) {
    var error = new Error('Terminate');
    error.name = 'TerminateException';
    this.message = error.message;
    if (error.stack) {
        this.stack = error.stack;
    }
    this._timeout = timeout;
}
TerminateException.prototype = Object.create(Error.prototype, {
    name: { value: 'TerminateException' }
});
TerminateException.prototype.timeout = function () {
    return this._timeout;
};

CloudI.stdout_write = function stdout_write (s) {
    fs.writeSync(1, s);
};
CloudI.stderr_write = function stderr_write (s) {
    fs.writeSync(2, s);
};

var MESSAGE_INIT                = 1;
var MESSAGE_SEND_ASYNC          = 2;
var MESSAGE_SEND_SYNC           = 3;
var MESSAGE_RECV_ASYNC          = 4;
var MESSAGE_RETURN_ASYNC        = 5;
var MESSAGE_RETURN_SYNC         = 6;
var MESSAGE_RETURNS_ASYNC       = 7;
var MESSAGE_KEEPALIVE           = 8;
var MESSAGE_REINIT              = 9;
var MESSAGE_SUBSCRIBE_COUNT     = 10;
var MESSAGE_TERM                = 11;

CloudI.API = function API (thread_index, callback) {
    var API = this;
    API.ASYNC = 1;
    API.SYNC = -1;
    if (typeof thread_index !== 'number') {
        throw new InvalidInputException();
    }
    callback = typeof callback !== 'undefined' ?
               callback : function (API_) {};
    var protocol_str = process.env.CLOUDI_API_INIT_PROTOCOL;
    if (protocol_str === undefined) {
        throw new InvalidInputException();
    }
    var buffer_size_str = process.env.CLOUDI_API_INIT_BUFFER_SIZE;
    if (buffer_size_str === undefined) {
        throw new InvalidInputException();
    }
    if (protocol_str == 'tcp') {
        API._use_header = true;
    }
    else if (protocol_str == 'udp') {
        API._use_header = false;
    }
    else if (protocol_str == 'local') {
        API._use_header = true;
    }
    else {
        throw new InvalidInputException();
    }
    API._socket = new net.Socket({fd: (thread_index + 3),
                                  readable: true,
                                  writable: true});
    API._initialization_complete = false;
    API._terminate = false;
    API._terminate_callback = undefined;
    API._size = parseInt(buffer_size_str);
    API._callbacks = {};
    API._timeout_terminate = 10; // TIMEOUT_TERMINATE_MIN
    API._poll_callback = callback;
    API._poll_callbacks_pending = [];
    API._poll_data = undefined;
    API._poll_data_size = undefined;
    API._socket.on('data', function(data) {
        try {
            if (! API._use_header) {
                API._poll_request(data);
                return;
            }
            if (API._poll_data === undefined) {
                API._poll_data_size = 4 + unpackUint32big(0, data);
                API._poll_data = data;
            }
            else {
                data = Buffer.concat([API._poll_data, data]);
            }
            if (data.length == API._poll_data_size) {
                API._poll_data = undefined;
                API._poll_data_size = undefined;
                API._poll_request(data);
            }
            else if (data.length < API._poll_data_size) {
                API._poll_data = data;
            }
            else if (data.length > API._poll_data_size + 4) {
                var next_data = data.slice(API._poll_data_size);
                data = data.slice(0, API._poll_data_size);
                API._poll_data = next_data;
                API._poll_data_size = 4 + unpackUint32big(0, next_data);
                API._poll_request(data);
            }
        }
        catch (err) {
            if (err instanceof InvalidInputException ||
                err instanceof MessageDecodingException) {
                CloudI.stderr_write(err.stack + '\n');
                API._poll_terminate();
            }
            else if (err instanceof TerminateException) {
                API._poll_terminate();
            }
            else {
                API._exception(err);
                API._poll_terminate();
            }
        }
    });
    API._socket.on('error', function(err) {
        API._exception(err);
        API._poll_terminate();
    });
    API._send(new Erlang.OtpErlangAtom('init'));
};

// class method
CloudI.API.thread_count = function thread_count () {
    var count_str = process.env.CLOUDI_API_INIT_THREAD_COUNT;
    if (count_str === undefined) {
        throw new InvalidInputException();
    }
    return parseInt(count_str);
};

CloudI.API.prototype.subscribe = function (pattern, obj, obj_f, callback) {
    if (obj_f.length != 9) {
        throw new InvalidInputException();
    }
    callback = typeof callback !== 'undefined' ?
               callback : function () {};
    this._poll_wait(function (API) {
        var f = {obj: obj, obj_f: obj_f};
        var key = API._prefix + pattern;
        var value = API._callbacks[key];
        if (value === undefined) {
            API._callbacks[key] = [f];
        }
        else {
            value.push(f);
        }
        API._send([new Erlang.OtpErlangAtom('subscribe'), pattern]);
        callback();
    });
};

CloudI.API.prototype.subscribe_count = function (pattern, callback) {
    callback = typeof callback !== 'undefined' ?
               callback : function (count) {};
    this._poll_wait(function (API) {
        API._poll_callback = callback;
        API._send([new Erlang.OtpErlangAtom('subscribe_count'), pattern]);
    });
};

CloudI.API.prototype.unsubscribe = function (pattern, callback) {
    callback = typeof callback !== 'undefined' ?
               callback : function () {};
    this._poll_wait(function (API) {
        var key = API._prefix + pattern;
        var value = API._callbacks[key];
        assert(value !== undefined);
        value.shift();
        if (value.length == 0) {
            delete API._callbacks[key];
        }
        API._send([new Erlang.OtpErlangAtom('unsubscribe'), pattern]);
        callback();
    });
};

CloudI.API.prototype.send_async = function (name, request, callback,
                                            timeout, request_info, priority) {
    callback = typeof callback !== 'undefined' ?
               callback : function (trans_id) {};
    request_info = typeof request_info !== 'undefined' ?
                   request_info : '';
    this._poll_wait(function (API) {
        timeout = typeof timeout !== 'undefined' ?
                  timeout : API._timeout_async;
        priority = typeof priority !== 'undefined' ?
                   priority : API._priority_default;
        API._poll_callback = callback;
        API._send([new Erlang.OtpErlangAtom('send_async'), name,
                   new Erlang.OtpErlangBinary(request_info),
                   new Erlang.OtpErlangBinary(request),
                   timeout, priority]);
    });
};

CloudI.API.prototype.send_sync = function (name, request, callback,
                                           timeout, request_info, priority) {
    callback = typeof callback !== 'undefined' ?
               callback : function (request_info, request, trans_id) {};
    request_info = typeof request_info !== 'undefined' ?
                   request_info : '';
    this._poll_wait(function (API) {
        timeout = typeof timeout !== 'undefined' ?
                  timeout : API._timeout_sync;
        priority = typeof priority !== 'undefined' ?
                   priority : API._priority_default;
        API._poll_callback = callback;
        API._send([new Erlang.OtpErlangAtom('send_sync'), name,
                   new Erlang.OtpErlangBinary(request_info),
                   new Erlang.OtpErlangBinary(request),
                   timeout, priority]);
    });
};

CloudI.API.prototype.mcast_async = function (name, request, callback,
                                             timeout, request_info, priority) {
    callback = typeof callback !== 'undefined' ?
               callback : function (trans_ids) {};
    request_info = typeof request_info !== 'undefined' ?
                   request_info : '';
    this._poll_wait(function (API) {
        timeout = typeof timeout !== 'undefined' ?
                  timeout : API._timeout_sync;
        priority = typeof priority !== 'undefined' ?
                   priority : API._priority_default;
        API._poll_callback = callback;
        API._send([new Erlang.OtpErlangAtom('mcast_async'), name,
                   new Erlang.OtpErlangBinary(request_info),
                   new Erlang.OtpErlangBinary(request),
                   timeout, priority]);
    });
};

CloudI.API.prototype.forward_ = function (request_type, name,
                                          request_info, request,
                                          timeout, priority, trans_id, pid) {
    var API = this;
    switch (request_type) {
        case API.ASYNC:
            API.forward_async(name, request_info, request,
                              timeout, priority, trans_id, pid);
            return;
        case API.SYNC:
            API.forward_sync(name, request_info, request,
                             timeout, priority, trans_id, pid);
            return;
        default:
            throw new InvalidInputException();
    }
};

CloudI.API.prototype.forward_async = function (name, request_info, request,
                                               timeout, priority,
                                               trans_id, pid) {
    this._poll_wait(function (API) {
        API._send([new Erlang.OtpErlangAtom('forward_async'), name,
                   new Erlang.OtpErlangBinary(request_info),
                   new Erlang.OtpErlangBinary(request),
                   timeout, priority,
                   new Erlang.OtpErlangBinary(trans_id), pid]);
        throw new ForwardAsyncException();
    });
};

CloudI.API.prototype.forward_sync = function (name, request_info, request,
                                              timeout, priority,
                                              trans_id, pid) {
    this._poll_wait(function (API) {
        API._send([new Erlang.OtpErlangAtom('forward_sync'), name,
                   new Erlang.OtpErlangBinary(request_info),
                   new Erlang.OtpErlangBinary(request),
                   timeout, priority,
                   new Erlang.OtpErlangBinary(trans_id), pid]);
        throw new ForwardSyncException();
    });
};

CloudI.API.prototype.return_ = function (request_type, name, pattern,
                                         response_info, response,
                                         timeout, trans_id, pid) {
    var API = this;
    switch (request_type) {
        case API.ASYNC:
            API.return_async(name, pattern, response_info, response,
                             timeout, trans_id, pid);
            return;
        case API.SYNC:
            API.return_sync(name, pattern, response_info, response,
                            timeout, trans_id, pid);
            return;
        default:
            throw new InvalidInputException();
    }
};

CloudI.API.prototype.return_async = function (name, pattern,
                                              response_info, response,
                                              timeout, trans_id, pid) {
    this._poll_wait(function (API) {
        API._send([new Erlang.OtpErlangAtom('return_async'), name, pattern,
                   new Erlang.OtpErlangBinary(response_info),
                   new Erlang.OtpErlangBinary(response),
                   timeout, new Erlang.OtpErlangBinary(trans_id), pid]);
        throw new ReturnAsyncException();
    });
};

CloudI.API.prototype.return_sync = function (name, pattern,
                                             response_info, response,
                                             timeout, trans_id, pid) {
    this._poll_wait(function (API) {
        API._send([new Erlang.OtpErlangAtom('return_sync'), name, pattern,
                   new Erlang.OtpErlangBinary(response_info),
                   new Erlang.OtpErlangBinary(response),
                   timeout, new Erlang.OtpErlangBinary(trans_id), pid]);
        throw new ReturnSyncException();
    });
};

CloudI.API.prototype.recv_async = function (callback,
                                            timeout, trans_id, consume) {
    callback = typeof callback !== 'undefined' ?
               callback : function (response_info, response, trans_id_) {};
    trans_id = typeof trans_id !== 'undefined' ?
               trans_id : '\x00\x00\x00\x00\x00\x00\x00\x00' +
                          '\x00\x00\x00\x00\x00\x00\x00\x00';
    consume = typeof consume !== 'undefined' ?
              consume : true;
    this._poll_wait(function (API) {
        timeout = typeof timeout !== 'undefined' ?
                  timeout : API._timeout_sync;
        API._poll_callback = callback;
        API._send([new Erlang.OtpErlangAtom('recv_async'), timeout,
                   new Erlang.OtpErlangBinary(trans_id), consume]);
    });
};

CloudI.API.prototype.process_index = function () {
    return this._process_index;
};

CloudI.API.prototype.process_count = function () {
    return this._process_count;
};

CloudI.API.prototype.process_count_max = function () {
    return this._process_count_max;
};

CloudI.API.prototype.process_count_min = function () {
    return this._process_count_min;
};

CloudI.API.prototype.prefix = function () {
    return this._prefix;
};

CloudI.API.prototype.timeout_initialize = function () {
    return this._timeout_initialize;
};

CloudI.API.prototype.timeout_async = function () {
    return this._timeout_async;
};

CloudI.API.prototype.timeout_sync = function () {
    return this._timeout_sync;
};

CloudI.API.prototype.timeout_terminate = function () {
    return this._timeout_terminate;
};

CloudI.API.prototype._null_response = function (request_type, name, pattern,
                                                request_info, request,
                                                timeout, priority,
                                                trans_id, pid) {
    return '';
};

CloudI.API.prototype._callback = function (command, name, pattern,
                                           request_info, request,
                                           timeout, priority, trans_id, pid) {
    var API = this;
    var function_queue = API._callbacks[pattern];
    var f;
    if (function_queue === undefined) {
        f = {obj: API, obj_f: API._null_response};
    }
    else {
        f = function_queue.shift();
        function_queue.push(f);
    }
    switch (command) {
        case MESSAGE_SEND_ASYNC:
            var domain_async = domain.create();
            domain_async.name = 'CloudI Async';
            domain_async.on('error', function(err) {
                domain_async.exit();
                var return_null_response = false;
                if (err instanceof MessageDecodingException) {
                    API._terminate = true;
                    return_null_response = true;
                }
                else if (err instanceof TerminateException) {
                    return_null_response = true;
                }
                else if (err instanceof ReturnAsyncException) {
                    return;
                }
                else if (err instanceof ReturnSyncException) {
                    API._terminate = true;
                    CloudI.stderr_write(err.stack + '\n');
                }
                else if (err instanceof ForwardAsyncException) {
                    return;
                }
                else if (err instanceof ForwardSyncException) {
                    API._terminate = true;
                    CloudI.stderr_write(err.stack + '\n');
                }
                else {
                    return_null_response = true;
                    API._exception(err);
                }
                if (return_null_response) {
                    var response_info = '';
                    var response = '';
                    try {
                        API.return_async(name, pattern,
                                         response_info, response,
                                         timeout, trans_id, pid);
                    }
                    catch (err_new) {
                        err_new = undefined;
                    }
                }
                if (API._terminate) {
                    API._poll_terminate();
                }
            });
            domain_async.enter();
            process.nextTick(function () {
                var response = f.obj_f.call(f.obj,
                                            API.ASYNC, name, pattern,
                                            request_info, request,
                                            timeout, priority, trans_id, pid);
                if (typeof response === 'undefined') {
                    return;
                }
                domain_async.exit();
                var response_info;
                if (typeof response === 'object' &&
                    toNativeString.call(response) == '[object Array]') {
                    response_info = response[0];
                    response = response[1];
                    if (typeof response_info !== 'string') {
                        response_info = '';
                    }
                }
                else {
                    response_info = '';
                }
                if (typeof response !== 'string') {
                    response = '';
                }
                try {
                    API.return_async(name, pattern,
                                     response_info, response,
                                     timeout, trans_id, pid);
                }
                catch (err_new) {
                    err_new = undefined;
                }
            });
            return;
        case MESSAGE_SEND_SYNC:
            var domain_sync = domain.create();
            domain_sync.name = 'CloudI Sync';
            domain_sync.on('error', function(err) {
                domain_sync.exit();
                var return_null_response = false;
                if (err instanceof MessageDecodingException) {
                    API._terminate = true;
                    return_null_response = true;
                }
                else if (err instanceof TerminateException) {
                    return_null_response = true;
                }
                else if (err instanceof ReturnAsyncException) {
                    API._terminate = true;
                    CloudI.stderr_write(err.stack + '\n');
                }
                else if (err instanceof ReturnSyncException) {
                    return;
                }
                else if (err instanceof ForwardAsyncException) {
                    API._terminate = true;
                    CloudI.stderr_write(err.stack + '\n');
                }
                else if (err instanceof ForwardSyncException) {
                    return;
                }
                else {
                    return_null_response = true;
                    API._exception(err);
                }
                if (return_null_response) {
                    var response_info = '';
                    var response = '';
                    try {
                        API.return_sync(name, pattern,
                                        response_info, response,
                                        timeout, trans_id, pid);
                    }
                    catch (err_new) {
                        err_new = undefined;
                    }
                }
                if (API._terminate) {
                    API._poll_terminate();
                }
            });
            domain_sync.enter();
            process.nextTick(function () {
                var response = f.obj_f.call(f.obj,
                                            API.SYNC, name, pattern,
                                            request_info, request,
                                            timeout, priority, trans_id, pid);
                if (typeof response === 'undefined') {
                    return;
                }
                domain_sync.exit();
                var response_info;
                if (typeof response === 'object' &&
                    toNativeString.call(response) == '[object Array]') {
                    response_info = response[0];
                    response = response[1];
                    if (typeof response_info !== 'string') {
                        response_info = '';
                    }
                }
                else {
                    response_info = '';
                }
                if (typeof response !== 'string') {
                    response = '';
                }
                try {
                    API.return_sync(name, pattern,
                                    response_info, response,
                                    timeout, trans_id, pid);
                }
                catch (err_new) {
                    err_new = undefined;
                }
            });
            return;
        default:
            throw new MessageDecodingException();
    }
};

CloudI.API.prototype._handle_events = function (data, data_size, i, command) {
    var API = this;
    if (typeof command === 'undefined') {
        if (i > data_size) {
            throw new MessageDecodingException();
        }
        command = unpackUint32(i, data);
        i += 4;
    }
    while (true) {
        switch (command) {
            case MESSAGE_TERM:
                API._terminate = true;
                throw new TerminateException(API._timeout_terminate);
            case MESSAGE_REINIT:
                API._process_count = unpackUint32(i, data);
                i += 4;
                API._timeout_async = unpackUint32(i, data);
                i += 4;
                API._timeout_sync = unpackUint32(i, data);
                i += 4;
                API._priority_default = unpackInt8(i, data);
                i += 1;
                break;
            case MESSAGE_KEEPALIVE:
                API._send(new Erlang.OtpErlangAtom('keepalive'));
                break;
            default:
                throw new MessageDecodingException();
        }
        if (i > data_size) {
            throw new MessageDecodingException();
        }
        else if (i == data_size) {
            return;
        }
        command = unpackUint32(i, data);
        i += 4;
    }
};

CloudI.API.prototype._poll_request = function (data) {
    var API = this;
    if (API._terminate) {
        throw new TerminateException(API._timeout_terminate);
    }
    var data_size = data.length;
    var i = 4;
    while (true) {
        var command = unpackUint32(i, data);
        i += 4;
        switch (command) {
            case MESSAGE_INIT:
                API._process_index = unpackUint32(i, data);
                i += 4;
                API._process_count = unpackUint32(i, data);
                i += 4;
                API._process_count_max = unpackUint32(i, data);
                i += 4;
                API._process_count_min = unpackUint32(i, data);
                i += 4;
                var prefix_size = unpackUint32(i, data);
                i += 4;
                var j = i + prefix_size - 1;
                API._prefix = data.slice(i, j).toString('binary');
                i = j + 1;
                API._timeout_initialize = unpackUint32(i, data);
                i += 4;
                API._timeout_async = unpackUint32(i, data);
                i += 4;
                API._timeout_sync = unpackUint32(i, data);
                i += 4;
                API._timeout_terminate = unpackUint32(i, data);
                i += 4;
                API._priority_default = unpackInt8(i, data);
                i += 1;
                if (i != data_size) {
                    API._handle_events(data, data_size, i);
                }
                API._poll_callback(API);
                API._poll_callback = undefined;
                return;
            case MESSAGE_SEND_ASYNC:
            case MESSAGE_SEND_SYNC:
                var name_size = unpackUint32(i, data);
                i += 4;
                var j = i + name_size - 1;
                var name = data.slice(i, j).toString('binary');
                i = j + 1;
                var pattern_size = unpackUint32(i, data);
                i += 4;
                j = i + pattern_size - 1;
                var pattern = data.slice(i, j).toString('binary');
                i = j + 1;
                var request_info_size = unpackUint32(i, data);
                i += 4;
                j = i + request_info_size;
                var request_info = data.slice(i, j);
                i = j + 1;
                var request_size = unpackUint32(i, data);
                i += 4;
                j = i + request_size;
                var request = data.slice(i, j);
                i = j + 1;
                var request_timeout = unpackUint32(i, data);
                i += 4;
                var priority = unpackInt8(i, data);
                i += 1;
                j = i + 16;
                var trans_id = data.slice(i, j);
                i = j;
                var pid_size = unpackUint32(i, data);
                i += 4;
                j = i + pid_size;
                var pid = data.slice(i, j);
                i = j;
                if (i != data_size) {
                    API._handle_events(data, data_size, i);
                }
                Erlang.binary_to_term(pid, function (err, pid_object) {
                    if (err) {
                        API._exception(err);
                        API._poll_terminate();
                    }
                    else {
                        API._callback(command, name, pattern,
                                      request_info, request,
                                      request_timeout, priority,
                                      trans_id, pid_object);
                    }
                });
                return;
            case MESSAGE_RECV_ASYNC:
            case MESSAGE_RETURN_SYNC:
                var response_info_size = unpackUint32(i, data);
                i += 4;
                var j = i + response_info_size;
                var response_info = data.slice(i, j);
                i = j + 1;
                var response_size = unpackUint32(i, data);
                i += 4;
                j = i + response_size;
                var response = data.slice(i, j);
                i = j + 1;
                j = i + 16;
                var trans_id = data.slice(i, j);
                i = j;
                if (i != data_size) {
                    API._handle_events(data, data_size, i);
                }
                API._poll_callback(response_info, response, trans_id);
                API._poll_callback = undefined;
                return;
            case MESSAGE_RETURN_ASYNC:
                var j = i + 16;
                var trans_id = data.slice(i, j);
                i = j;
                if (i != data_size) {
                    API._handle_events(data, data_size, i);
                }
                API._poll_callback(trans_id);
                API._poll_callback = undefined;
                return;
            case MESSAGE_RETURNS_ASYNC:
                var trans_id_count = unpackUint32(i, data);
                i += 4;
                var j;
                var trans_ids = [];
                for (trans_id_index = 0; trans_id_index < trans_id_count;
                     trans_id_index++) {
                    j = i + 16;
                    var trans_id = data.slice(i, j);
                    i = j;
                    trans_ids.push(trans_id);
                }
                if (i != data_size) {
                    API._handle_events(data, data_size, i);
                }
                API._poll_callback(trans_ids);
                API._poll_callback = undefined;
                return;
            case MESSAGE_SUBSCRIBE_COUNT:
                var count = unpackUint32(i, data);
                i += 4;
                if (i != data_size) {
                    API._handle_events(data, data_size, i);
                }
                API._poll_callback(count);
                API._poll_callback = undefined;
                return;
            case MESSAGE_TERM:
                API._handle_events(data, data_size, i, command);
                return;
            case MESSAGE_REINIT:
                API._process_count = unpackUint32(i, data);
                i += 4;
                API._timeout_async = unpackUint32(i, data);
                i += 4;
                API._timeout_sync = unpackUint32(i, data);
                i += 4;
                API._priority_default = unpackInt8(i, data);
                i += 1;
                if (i == data_size) {
                    return;
                }
                else if (i < data_size) {
                    break;
                }
                else {
                    throw new MessageDecodingException();
                }
            case MESSAGE_KEEPALIVE:
                API._send(new Erlang.OtpErlangAtom('keepalive'));
                if (i == data_size) {
                    return;
                }
                else if (i < data_size) {
                    break;
                }
                else {
                    throw new MessageDecodingException();
                }
            default:
                throw new MessageDecodingException();
        }
    }
};

CloudI.API.prototype._poll_wait_retry = function () {
    var API = this;
    if (API._terminate) {
        return;
    }
    else if (API._poll_callback === undefined) {
        var f = API._poll_callbacks_pending.shift();
        f(API);
    }
    else {
        timers.setTimeout(function() {
            API._poll_wait_retry();
        }, 0);
    }
};

CloudI.API.prototype._poll_wait = function (f) {
    var API = this;
    if (API._terminate) {
        return;
    }
    else if (API._poll_callback === undefined &&
             API._poll_callbacks_pending.length == 0) {
        f(API);
    }
    else {
        API._poll_callbacks_pending.push(f);
        timers.setTimeout(function() {
            API._poll_wait_retry();
        }, 0);
    }
};

CloudI.API.prototype._poll_terminate = function () {
    var API = this;
    API._terminate = true;
    API._socket.destroy();
    if (API._terminate_callback !== undefined) {
        API._terminate_callback(false);
    }
    process.exit();
};

CloudI.API.prototype._poll_blocked = function () {
    var API = this;
    if (API._terminate) {
        return;
    }
    timers.setTimeout(function () {
        API._poll_blocked();
    }, 500);
};

CloudI.API.prototype._poll_block = function (callback, timeout) {
    var API = this;
    if (API._terminate) {
        return;
    }
    else if (timeout == 0) {
        API._terminate_callback = undefined;
        callback(true);
    }
    else if (timeout > 0) {
        var timeout_interval = 500;
        API._terminate_callback = callback;
        if (timeout > timeout_interval) {
            timers.setTimeout(function () {
                API._poll_block(callback, timeout - timeout_interval);
            }, timeout_interval);
        }
        else {
            timers.setTimeout(function () {
                API._poll_block(callback, 0);
            }, timeout);
        }
    }
    else {
        API._terminate_callback = callback;
        API._poll_blocked();
    }
};

CloudI.API.prototype.poll = function (callback, timeout) {
    var API = this;
    callback = typeof callback !== 'undefined' ?
               callback : function (timeout_) {};
    timeout = typeof timeout !== 'undefined' ?
              timeout : -1;
    if (API._terminate) {
        callback(false);
        return;
    }
    else if (! API._initialization_complete) {
        API._send(new Erlang.OtpErlangAtom('polling'));
        API._initialization_complete = true;
    }
    timers.setTimeout(function () {
        API._poll_block(callback, timeout);
    }, 0);
};

CloudI.API.prototype.shutdown = function (callback, reason) {
    callback = typeof callback !== 'undefined' ?
               callback : function () {};
    reason = typeof reason !== 'undefined' ?
             reason : "";
    this._poll_wait(function (API) {
        API._send([new Erlang.OtpErlangAtom('shutdown'), reason]);
        callback();
    });
};

// class method
var text_pairs_parse = function text_pairs_parse (text) {
    var pairs = {};
    var text_segments = text.toString('binary').split('\x00');
    for (var i = 0; i < text_segments.length - 1; i += 2) {
        var key = text_segments[i];
        var current = pairs[key];
        if (current === undefined) {
            pairs[key] = text_segments[i + 1];
        }
        else if (typeof current === 'object' && 
                 toNativeString.call(response) == '[object Array]') {
            current.push(text_segments[i + 1]);
        }
        else {
            pairs[key] = [current, text_segments[i + 1]];
        }
    }
    return pairs;
};

// class method
var text_pairs_new = function text_pairs_new (pairs, response) {
    var text_segments = [];
    for (var key in pairs) {
        var values = pairs[key];
        if (typeof values === 'string') {
            text_segments.push(key);
            text_segments.push(values);
        }
        else {
            for (var value in values) {
                text_segments.push(key);
                text_segments.push(value);
            }
        }
    }
    if (response && text_segments.length == 0) {
        return '\x00';
    }
    text_segments.push('');
    return text_segments.join('\x00');
};

CloudI.API.info_key_value_parse = function info_key_value_parse (info) {
    return text_pairs_parse(info);
};

CloudI.API.info_key_value_new = function info_key_value_new (pairs, response) {
    response = typeof response !== 'undefined' ?
               response : true;
    return text_pairs_new(pairs, response);
};

CloudI.API.prototype._exception = function (err) {
    if (typeof err.stack === 'undefined') {
        err = new Error(err.toString());
    }
    CloudI.stderr_write(err.stack + '\n');
};

CloudI.API.prototype._send = function (terms) {
    var API = this;
    Erlang.term_to_binary(terms, function (err, data) {
        if (err) {
            API._exception(err);
            API._poll_terminate();
        }
        if (API._use_header) {
            data = Buffer.concat([packUint32big(data.length), data]);
        }
        API._socket.write(data, 'binary');
    });
};

CloudI.InvalidInputException = InvalidInputException;
CloudI.ReturnSyncException = ReturnSyncException;
CloudI.ReturnAsyncException = ReturnAsyncException;
CloudI.ForwardSyncException = ForwardSyncException;
CloudI.ForwardAsyncException = ForwardAsyncException;
CloudI.MessageDecodingException = MessageDecodingException;
CloudI.TerminateException = TerminateException;

};
