package cloudi

//-*-Mode:Go;coding:utf-8;tab-width:4;c-basic-offset:4-*-
// ex: set ft=go fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:
//
// MIT License
//
// Copyright (c) 2017-2023 Michael Truog <mjtruog at protonmail dot com>
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

import (
	"bytes"
	"container/list"
	"encoding/binary"
	"fmt"
	"github.com/okeuday/erlang_go/v2/erlang"
	"math"
	"net"
	"os"
	"reflect"
	"runtime/debug"
	"strconv"
	"time"
	"unsafe"
)

const (
	messageInit           = 1
	messageSendAsync      = 2
	messageSendSync       = 3
	messageRecvAsync      = 4
	messageReturnAsync    = 5
	messageReturnSync     = 6
	messageReturnsAsync   = 7
	messageKeepalive      = 8
	messageReinit         = 9
	messageSubscribeCount = 10
	messageTerm           = 11
)

const (
	// ASYNC is the requestType provided when handling an asynchronous service request
	ASYNC = 1
	// SYNC is the requestType provided when handling a synchronous service request
	SYNC = -1
)

var nativeEndian binary.ByteOrder

// Instance is an instance of the CloudI API
type Instance struct {
	state                  interface{}
	socket                 net.Conn
	useHeader              bool
	initializationComplete bool
	fatalExceptions        bool
	terminate              bool
	fragmentSize           uint32
	fragmentRecv           []byte
	callbacks              map[string]*list.List
	bufferRecv             *bytes.Buffer
	processIndex           uint32
	processCount           uint32
	processCountMax        uint32
	processCountMin        uint32
	prefix                 string
	timeoutInitialize      uint32
	timeoutAsync           uint32
	timeoutSync            uint32
	timeoutTerminate       uint32
	priorityDefault        int8
	responseInfo           []byte
	response               []byte
	transId                []byte
	transIds               [][]byte
	subscribeCount         uint32
}

// Source is the Erlang pid that is the source of the service request
type Source erlang.OtpErlangPid

// Callback is a function to handle a service request
type Callback func(int, string, string, []byte, []byte, uint32, int8, [16]byte, Source, interface{}, *Instance) ([]byte, []byte, error)

func nullResponse(requestType int, name, pattern string, requestInfo, request []byte, timeout uint32, priority int8, transId [16]byte, source Source, state interface{}, api *Instance) ([]byte, []byte, error) {
	return []byte{}, []byte{}, nil
}

// API creates an instance of the CloudI API
func API(threadIndex uint32, state interface{}) (*Instance, error) {
	protocol := os.Getenv("CLOUDI_API_INIT_PROTOCOL")
	if protocol == "" {
		os.Stderr.WriteString("CloudI service execution must occur in CloudI\n")
		return nil, invalidInputErrorNew()
	}
	bufferSize, err := uintGetenv("CLOUDI_API_INIT_BUFFER_SIZE")
	if err != nil {
		return nil, err
	}
	switch byteOrder := uint16(0x00ff); *(*uint8)(unsafe.Pointer(&byteOrder)) {
	case 0x00:
		nativeEndian = binary.BigEndian
	case 0xff:
		nativeEndian = binary.LittleEndian
	}
	var socket net.Conn
	socket, err = net.FileConn(os.NewFile(uintptr(threadIndex+3), strconv.Itoa(int(threadIndex))))
	if err != nil {
		return nil, err
	}
	err = socket.SetDeadline(time.Time{})
	if err != nil {
		return nil, err
	}
	var useHeader bool
	switch protocol {
	case "tcp":
		useHeader = true
	case "udp":
		useHeader = false
	case "local":
		useHeader = true
	}
	fragmentRecv := make([]byte, bufferSize)
	callbacks := make(map[string]*list.List)
	bufferRecv := new(bytes.Buffer)
	bufferRecv.Grow(int(bufferSize))
	timeoutTerminate := uint32(10) // TIMEOUT_TERMINATE_MIN
	api := &Instance{state: state, socket: socket, useHeader: useHeader, fragmentSize: bufferSize, fragmentRecv: fragmentRecv, callbacks: callbacks, bufferRecv: bufferRecv, timeoutTerminate: timeoutTerminate}
	var init []byte
	init, err = erlang.TermToBinary(erlang.OtpErlangAtom("init"), -1)
	if err != nil {
		return nil, err
	}
	err = api.send(init)
	if err != nil {
		return nil, err
	}
	_, err = api.pollRequest(-1, false)
	if err != nil {
		return nil, err
	}
	return api, nil
}

// ThreadCount returns the thread count from the service configuration
func ThreadCount() (uint32, error) {
	return uintGetenv("CLOUDI_API_INIT_THREAD_COUNT")
}

// Subscribe subscribes to a service name pattern with a callback
func (api *Instance) Subscribe(pattern string, function Callback) error {
	key := api.prefix + pattern
	functionQueue := api.callbacks[key]
	if functionQueue == nil {
		functionQueue = list.New()
		api.callbacks[key] = functionQueue
	}
	_ = functionQueue.PushBack(function)
	subscribe, err := erlang.TermToBinary([]interface{}{erlang.OtpErlangAtom("subscribe"), pattern}, -1)
	if err != nil {
		return err
	}
	err = api.send(subscribe)
	if err != nil {
		return err
	}
	return nil
}

// SubscribeCount returns the number of subscriptions for a single service name pattern
func (api *Instance) SubscribeCount(pattern string) (uint32, error) {
	subscribeCount, err := erlang.TermToBinary([]interface{}{erlang.OtpErlangAtom("subscribe_count"), pattern}, -1)
	if err != nil {
		return 0, err
	}
	err = api.send(subscribeCount)
	if err != nil {
		return 0, err
	}
	_, err = api.pollRequest(-1, false)
	if err != nil {
		return 0, err
	}
	return api.subscribeCount, nil
}

// Unsubscribe unsubscribes from a service name pattern once
func (api *Instance) Unsubscribe(pattern string) error {
	key := api.prefix + pattern
	functionQueue := api.callbacks[key]
	_ = functionQueue.Remove(functionQueue.Front())
	if functionQueue.Len() == 0 {
		api.callbacks[key] = nil
	}
	unsubscribe, err := erlang.TermToBinary([]interface{}{erlang.OtpErlangAtom("unsubscribe"), pattern}, -1)
	if err != nil {
		return err
	}
	err = api.send(unsubscribe)
	if err != nil {
		return err
	}
	return nil
}

// SendAsync sends an asynchronous service request
func (api *Instance) SendAsync(name string, requestInfo, request []byte, timeoutPriority ...interface{}) ([]byte, error) {
	if name == "" {
		return nil, invalidInputErrorNew()
	}
	if requestInfo == nil {
		requestInfo = []byte{}
	}
	if request == nil {
		request = []byte{}
	}
	extraArity := len(timeoutPriority)
	if extraArity > 2 {
		return nil, invalidInputErrorNew()
	}
	var err error
	timeout := api.timeoutAsync
	if extraArity > 0 {
		timeout, err = timeoutCheck(timeoutPriority[0])
		if err != nil {
			return nil, err
		}
	}
	priority := api.priorityDefault
	if extraArity > 1 {
		priority, err = priorityCheck(timeoutPriority[1])
		if err != nil {
			return nil, err
		}
	}
	var sendAsync []byte
	sendAsync, err = erlang.TermToBinary([]interface{}{erlang.OtpErlangAtom("send_async"), name, requestInfo, request, timeout, priority}, -1)
	if err != nil {
		return nil, err
	}
	err = api.send(sendAsync)
	if err != nil {
		return nil, err
	}
	_, err = api.pollRequest(-1, false)
	if err != nil {
		return nil, err
	}
	return api.transId, nil
}

// SendSync sends a synchronous service request
func (api *Instance) SendSync(name string, requestInfo, request []byte, timeoutPriority ...interface{}) ([]byte, []byte, []byte, error) {
	if name == "" {
		return nil, nil, nil, invalidInputErrorNew()
	}
	if requestInfo == nil {
		requestInfo = []byte{}
	}
	if request == nil {
		request = []byte{}
	}
	extraArity := len(timeoutPriority)
	if extraArity > 2 {
		return nil, nil, nil, invalidInputErrorNew()
	}
	var err error
	timeout := api.timeoutSync
	if extraArity > 0 {
		timeout, err = timeoutCheck(timeoutPriority[0])
		if err != nil {
			return nil, nil, nil, err
		}
	}
	priority := api.priorityDefault
	if extraArity > 1 {
		priority, err = priorityCheck(timeoutPriority[1])
		if err != nil {
			return nil, nil, nil, err
		}
	}
	var sendSync []byte
	sendSync, err = erlang.TermToBinary([]interface{}{erlang.OtpErlangAtom("send_sync"), name, requestInfo, request, timeout, priority}, -1)
	if err != nil {
		return nil, nil, nil, err
	}
	err = api.send(sendSync)
	if err != nil {
		return nil, nil, nil, err
	}
	_, err = api.pollRequest(-1, false)
	if err != nil {
		return nil, nil, nil, err
	}
	return api.responseInfo, api.response, api.transId, nil
}

// McastAsync sends asynchronous service requests to all subscribers of the matching service name pattern
func (api *Instance) McastAsync(name string, requestInfo, request []byte, timeoutPriority ...interface{}) ([][]byte, error) {
	if name == "" {
		return nil, invalidInputErrorNew()
	}
	if requestInfo == nil {
		requestInfo = []byte{}
	}
	if request == nil {
		request = []byte{}
	}
	extraArity := len(timeoutPriority)
	if extraArity > 2 {
		return nil, invalidInputErrorNew()
	}
	var err error
	timeout := api.timeoutAsync
	if extraArity > 0 {
		timeout, err = timeoutCheck(timeoutPriority[0])
		if err != nil {
			return nil, err
		}
	}
	priority := api.priorityDefault
	if extraArity > 1 {
		priority, err = priorityCheck(timeoutPriority[1])
		if err != nil {
			return nil, err
		}
	}
	var mcastAsync []byte
	mcastAsync, err = erlang.TermToBinary([]interface{}{erlang.OtpErlangAtom("mcast_async"), name, requestInfo, request, timeout, priority}, -1)
	if err != nil {
		return nil, err
	}
	err = api.send(mcastAsync)
	if err != nil {
		return nil, err
	}
	_, err = api.pollRequest(-1, false)
	if err != nil {
		return nil, err
	}
	return api.transIds, nil
}

func (api *Instance) forwardAsyncI(name string, requestInfo, request []byte, timeout uint32, priority int8, transId [16]byte, source Source) error {
	if requestInfo == nil {
		requestInfo = []byte{}
	}
	if request == nil {
		request = []byte{}
	}
	forwardAsync, err := erlang.TermToBinary([]interface{}{erlang.OtpErlangAtom("forward_async"), name, requestInfo, request, timeout, priority, transId[:], erlang.OtpErlangPid(source)}, -1)
	if err != nil {
		return err
	}
	err = api.send(forwardAsync)
	if err != nil {
		return err
	}
	return nil
}

// ForwardAsync forwards an asynchronous service request to a different service name
func (api *Instance) ForwardAsync(name string, requestInfo, request []byte, timeout uint32, priority int8, transId [16]byte, source Source) {
	err := api.forwardAsyncI(name, requestInfo, request, timeout, priority, transId, source)
	if err == nil {
		err = forwardAsyncErrorNew()
	}
	panic(err)
}

func (api *Instance) forwardSyncI(name string, requestInfo, request []byte, timeout uint32, priority int8, transId [16]byte, source Source) error {
	if requestInfo == nil {
		requestInfo = []byte{}
	}
	if request == nil {
		request = []byte{}
	}
	forwardSync, err := erlang.TermToBinary([]interface{}{erlang.OtpErlangAtom("forward_sync"), name, requestInfo, request, timeout, priority, transId[:], erlang.OtpErlangPid(source)}, -1)
	if err != nil {
		return err
	}
	err = api.send(forwardSync)
	if err != nil {
		return err
	}
	return nil
}

// ForwardSync forwards a synchronous service request to a different service name
func (api *Instance) ForwardSync(name string, requestInfo, request []byte, timeout uint32, priority int8, transId [16]byte, source Source) {
	err := api.forwardSyncI(name, requestInfo, request, timeout, priority, transId, source)
	if err == nil {
		err = forwardSyncErrorNew()
	}
	panic(err)
}

// Forward forwards a service request to a different service name
func (api *Instance) Forward(requestType int, name string, requestInfo, request []byte, timeout uint32, priority int8, transId [16]byte, source Source) {
	switch requestType {
	case ASYNC:
		api.ForwardAsync(name, requestInfo, request, timeout, priority, transId, source)
	case SYNC:
		api.ForwardSync(name, requestInfo, request, timeout, priority, transId, source)
	default:
		panic(invalidInputErrorNew())
	}
}

func (api *Instance) returnAsyncI(name, pattern string, responseInfo, response []byte, timeout uint32, transId [16]byte, source Source) error {
	if responseInfo == nil {
		responseInfo = []byte{}
	}
	if response == nil {
		response = []byte{}
	}
	returnAsync, err := erlang.TermToBinary([]interface{}{erlang.OtpErlangAtom("return_async"), name, pattern, responseInfo, response, timeout, transId[:], erlang.OtpErlangPid(source)}, -1)
	if err != nil {
		return err
	}
	err = api.send(returnAsync)
	if err != nil {
		return err
	}
	return nil
}

// ReturnAsync provides a response to an asynchronous service request
func (api *Instance) ReturnAsync(name, pattern string, responseInfo, response []byte, timeout uint32, transId [16]byte, source Source) {
	err := api.returnAsyncI(name, pattern, responseInfo, response, timeout, transId, source)
	if err == nil {
		err = returnAsyncErrorNew()
	}
	panic(err)
}

func (api *Instance) returnSyncI(name, pattern string, responseInfo, response []byte, timeout uint32, transId [16]byte, source Source) error {
	if responseInfo == nil {
		responseInfo = []byte{}
	}
	if response == nil {
		response = []byte{}
	}
	returnSync, err := erlang.TermToBinary([]interface{}{erlang.OtpErlangAtom("return_sync"), name, pattern, responseInfo, response, timeout, transId[:], erlang.OtpErlangPid(source)}, -1)
	if err != nil {
		return err
	}
	err = api.send(returnSync)
	if err != nil {
		return err
	}
	return nil
}

// ReturnSync provides a response to a synchronous service request
func (api *Instance) ReturnSync(name, pattern string, responseInfo, response []byte, timeout uint32, transId [16]byte, source Source) {
	err := api.returnSyncI(name, pattern, responseInfo, response, timeout, transId, source)
	if err == nil {
		err = returnSyncErrorNew()
	}
	panic(err)
}

// Return provides a response to a service request
func (api *Instance) Return(requestType int, name, pattern string, responseInfo, response []byte, timeout uint32, transId [16]byte, source Source) {
	switch requestType {
	case ASYNC:
		api.ReturnAsync(name, pattern, responseInfo, response, timeout, transId, source)
	case SYNC:
		api.ReturnSync(name, pattern, responseInfo, response, timeout, transId, source)
	default:
		panic(invalidInputErrorNew())
	}
}

// RecvAsync blocks to receive an asynchronous service request response
func (api *Instance) RecvAsync(extra ...interface{}) ([]byte, []byte, []byte, error) {
	extraArity := len(extra)
	if extraArity > 3 {
		return nil, nil, nil, invalidInputErrorNew()
	}
	timeout := api.timeoutSync
	var transId [16]byte
	consume := true
	for _, extraArg := range extra {
		switch arg := extraArg.(type) {
		case uint32:
			timeout = arg
		case uint8:
			timeout = uint32(arg)
		case uint16:
			timeout = uint32(arg)
		case uint64:
			if arg > math.MaxUint32 {
				return nil, nil, nil, invalidInputErrorNew()
			}
			timeout = uint32(arg)
		case int8:
			if arg < 0 {
				return nil, nil, nil, invalidInputErrorNew()
			}
			timeout = uint32(arg)
		case int16:
			if arg < 0 {
				return nil, nil, nil, invalidInputErrorNew()
			}
			timeout = uint32(arg)
		case int32:
			if arg < 0 {
				return nil, nil, nil, invalidInputErrorNew()
			}
			timeout = uint32(arg)
		case int64:
			if arg < 0 || arg > math.MaxUint32 {
				return nil, nil, nil, invalidInputErrorNew()
			}
			timeout = uint32(arg)
		case int:
			if arg < 0 || uint64(arg) > math.MaxUint32 {
				return nil, nil, nil, invalidInputErrorNew()
			}
			timeout = uint32(arg)
		case [16]byte:
			transId = arg
		case []byte:
			if len(arg) != 16 {
				return nil, nil, nil, invalidInputErrorNew()
			}
			copy(transId[:], arg)
		case bool:
			consume = arg
		default:
			return nil, nil, nil, invalidInputErrorNew()
		}
	}
	recvAsync, err := erlang.TermToBinary([]interface{}{erlang.OtpErlangAtom("recv_async"), timeout, transId[:], consume}, -1)
	if err != nil {
		return nil, nil, nil, err
	}
	err = api.send(recvAsync)
	if err != nil {
		return nil, nil, nil, err
	}
	_, err = api.pollRequest(-1, false)
	if err != nil {
		return nil, nil, nil, err
	}
	return api.responseInfo, api.response, api.transId, nil
}

func timeoutCheck(value interface{}) (uint32, error) {
	switch timeout := value.(type) {
	case uint32:
		return timeout, nil
	case uint8:
		return uint32(timeout), nil
	case uint16:
		return uint32(timeout), nil
	case uint64:
		if timeout > math.MaxUint32 {
			return 0, invalidInputErrorNew()
		}
		return uint32(timeout), nil
	case int8:
		if timeout < 0 {
			return 0, invalidInputErrorNew()
		}
		return uint32(timeout), nil
	case int16:
		if timeout < 0 {
			return 0, invalidInputErrorNew()
		}
		return uint32(timeout), nil
	case int32:
		if timeout < 0 {
			return 0, invalidInputErrorNew()
		}
		return uint32(timeout), nil
	case int64:
		if timeout < 0 || timeout > math.MaxUint32 {
			return 0, invalidInputErrorNew()
		}
		return uint32(timeout), nil
	case int:
		if timeout < 0 || uint64(timeout) > math.MaxUint32 {
			return 0, invalidInputErrorNew()
		}
		return uint32(timeout), nil
	default:
		return 0, invalidInputErrorNew()
	}
}

func priorityCheck(value interface{}) (int8, error) {
	switch priority := value.(type) {
	case int8:
		return priority, nil
	case uint8:
		if priority > math.MaxInt8 {
			return 0, invalidInputErrorNew()
		}
		return int8(priority), nil
	case uint16:
		if priority > math.MaxInt8 {
			return 0, invalidInputErrorNew()
		}
		return int8(priority), nil
	case uint32:
		if priority > math.MaxInt8 {
			return 0, invalidInputErrorNew()
		}
		return int8(priority), nil
	case uint64:
		if priority > math.MaxInt8 {
			return 0, invalidInputErrorNew()
		}
		return int8(priority), nil
	case int16:
		if priority < math.MaxInt8 || priority > math.MaxInt8 {
			return 0, invalidInputErrorNew()
		}
		return int8(priority), nil
	case int32:
		if priority < math.MaxInt8 || priority > math.MaxInt8 {
			return 0, invalidInputErrorNew()
		}
		return int8(priority), nil
	case int64:
		if priority < math.MaxInt8 || priority > math.MaxInt8 {
			return 0, invalidInputErrorNew()
		}
		return int8(priority), nil
	case int:
		if priority < math.MaxInt8 || priority > math.MaxInt8 {
			return 0, invalidInputErrorNew()
		}
		return int8(priority), nil
	default:
		return 0, invalidInputErrorNew()
	}
}

// ProcessIndex returns the 0-based index of this process in the service instance
func (api *Instance) ProcessIndex() uint32 {
	return api.processIndex
}

// ProcessCount returns the current process count based on the service configuration
func (api *Instance) ProcessCount() uint32 {
	return api.processCount
}

// ProcessCountMax returns the count_process_dynamic maximum count based on the service configuration
func (api *Instance) ProcessCountMax() uint32 {
	return api.processCountMax
}

// ProcessCountMin returns the count_process_dynamic minimum count based on the service configuration
func (api *Instance) ProcessCountMin() uint32 {
	return api.processCountMin
}

// Prefix returns the service name pattern prefix from the service configuration
func (api *Instance) Prefix() string {
	return api.prefix
}

// TimeoutInitialize returns the service initialization timeout from the service configuration
func (api *Instance) TimeoutInitialize() uint32 {
	return api.timeoutInitialize
}

// TimeoutAsync returns the default asynchronous service request send timeout from the service configuration
func (api *Instance) TimeoutAsync() uint32 {
	return api.timeoutAsync
}

// TimeoutSync returns the default synchronous service request send timeout from the service configuration
func (api *Instance) TimeoutSync() uint32 {
	return api.timeoutSync
}

// TimeoutTerminate returns the service termination timeout based on the service configuration
func (api *Instance) TimeoutTerminate() uint32 {
	return api.timeoutTerminate
}

// PriorityDefault returns the default service request send priority from the service configuration
func (api *Instance) PriorityDefault() int8 {
	return api.priorityDefault
}

func (api *Instance) callback(command uint32, name, pattern string, requestInfo, request []byte, timeout uint32, priority int8, transId [16]byte, source Source) error {
	functionQueue := api.callbacks[pattern]
	var function Callback
	if functionQueue == nil {
		function = nullResponse
	} else {
		function = functionQueue.Remove(functionQueue.Front()).(Callback)
		_ = functionQueue.PushBack(function)
	}
	switch command {
	case messageSendAsync:
		responseInfo, response, err := api.callbackExecute(function, command, name, pattern, requestInfo, request, timeout, priority, transId, source)
		if err != nil {
			switch err.(type) {
			case *MessageDecodingError:
				api.terminate = true
				err = nil
			case *TerminateError:
				err = nil
			case *ReturnAsyncError:
				return nil
			case *ReturnSyncError:
				api.terminate = true
				return err
			case *ForwardAsyncError:
				return nil
			case *ForwardSyncError:
				api.terminate = true
				return err
			case *AssertionError:
				ErrorExit(os.Stderr, err)
			case *FatalError:
				ErrorExit(os.Stderr, err)
			default:
				if api.fatalExceptions {
					ErrorExit(os.Stderr, err)
				}
				os.Stderr.WriteString(err.Error() + "\n")
				err = nil
			}
		}
		return api.returnAsyncI(name, pattern, responseInfo, response, timeout, transId, source)
	case messageSendSync:
		responseInfo, response, err := api.callbackExecute(function, command, name, pattern, requestInfo, request, timeout, priority, transId, source)
		if err != nil {
			switch err.(type) {
			case *MessageDecodingError:
				api.terminate = true
				err = nil
			case *TerminateError:
				err = nil
			case *ReturnAsyncError:
				api.terminate = true
				return err
			case *ReturnSyncError:
				return nil
			case *ForwardAsyncError:
				api.terminate = true
				return err
			case *ForwardSyncError:
				return nil
			case *AssertionError:
				ErrorExit(os.Stderr, err)
			case *FatalError:
				ErrorExit(os.Stderr, err)
			default:
				if api.fatalExceptions {
					ErrorExit(os.Stderr, err)
				}
				os.Stderr.WriteString(err.Error() + "\n")
				err = nil
			}
		}
		return api.returnSyncI(name, pattern, responseInfo, response, timeout, transId, source)
	default:
		return messageDecodingErrorNew()
	}
}

func (api *Instance) callbackExecute(function Callback, command uint32, name, pattern string, requestInfo, request []byte, timeout uint32, priority int8, transId [16]byte, source Source) (responseInfo []byte, response []byte, err error) {
	defer func() {
		if r := recover(); r != nil {
			switch errValue := r.(type) {
			case *InvalidInputError:
				err = errValue
			case *MessageDecodingError:
				err = errValue
			case *TerminateError:
				err = errValue
			case *ReturnAsyncError:
				err = errValue
			case *ReturnSyncError:
				err = errValue
			case *ForwardAsyncError:
				err = errValue
			case *ForwardSyncError:
				err = errValue
			case *AssertionError:
				err = errValue
			case *FatalError:
				err = errValue
			default:
				err = StackErrorWrapNew(errValue)
			}
		}
	}()
	switch command {
	case messageSendAsync:
		responseInfo, response, err = function(ASYNC, name, pattern, requestInfo, request, timeout, priority, transId, source, api.state, api)
	case messageSendSync:
		responseInfo, response, err = function(SYNC, name, pattern, requestInfo, request, timeout, priority, transId, source, api.state, api)
	}
	return
}

func (api *Instance) handleEvents(external bool, reader *bytes.Reader, command uint32) (bool, error) {
	var err error
	if command == 0 {
		err = binary.Read(reader, nativeEndian, &command)
		if err != nil {
			return true, err
		}
	}
	for true {
		switch command {
		case messageTerm:
			api.terminate = true
			if external {
				return false, nil
			}
			return true, terminateErrorNew(api.timeoutTerminate)
		case messageReinit:
			err = binary.Read(reader, nativeEndian, &(api.processCount))
			if err != nil {
				return true, err
			}
			err = binary.Read(reader, nativeEndian, &(api.timeoutAsync))
			if err != nil {
				return false, err
			}
			err = binary.Read(reader, nativeEndian, &(api.timeoutSync))
			if err != nil {
				return false, err
			}
			err = binary.Read(reader, nativeEndian, &(api.priorityDefault))
			if err != nil {
				return false, err
			}
			err = binary.Read(reader, nativeEndian, &(api.fatalExceptions))
			if err != nil {
				return false, err
			}
		case messageKeepalive:
			var keepalive []byte
			keepalive, err = erlang.TermToBinary(erlang.OtpErlangAtom("keepalive"), -1)
			if err != nil {
				return true, err
			}
			err = api.send(keepalive)
			if err != nil {
				return true, err
			}
		default:
			return true, messageDecodingErrorNew()
		}
		if reader.Len() == 0 {
			return true, nil
		}
		err = binary.Read(reader, nativeEndian, &command)
		if err != nil {
			return true, err
		}
	}
	return true, nil
}

func (api *Instance) pollRequest(timeout int32, external bool) (bool, error) {
	var err error
	if api.terminate {
		if external {
			return false, nil
		}
		return true, terminateErrorNew(api.timeoutTerminate)
	} else if external && !api.initializationComplete {
		var polling []byte
		polling, err = erlang.TermToBinary(erlang.OtpErlangAtom("polling"), -1)
		if err != nil {
			return false, err
		}
		err = api.send(polling)
		if err != nil {
			return false, err
		}
		api.initializationComplete = true
	}
	pollTimer := time.Now()
	var pollTimerDeadline time.Time
	if timeout == 0 {
		pollTimerDeadline = pollTimer.Add(time.Duration(500) * time.Microsecond)
	} else if timeout > 0 {
		pollTimerDeadline = pollTimer.Add(time.Duration(timeout) * time.Millisecond)
	}
	for true {
		err = api.socket.SetReadDeadline(pollTimerDeadline)
		if err != nil {
			return false, err
		}
		var data []byte
		data, err = api.recv()
		if err != nil {
			switch errNet := err.(type) {
			case *net.OpError:
				if errNet.Timeout() {
					return true, nil
				}
			default:
				return false, err
			}
		}
		reader := bytes.NewReader(data)
		var command uint32
		err = binary.Read(reader, nativeEndian, &command)
		if err != nil {
			return false, err
		}
		switch command {
		case messageInit:
			err = binary.Read(reader, nativeEndian, &(api.processIndex))
			if err != nil {
				return false, err
			}
			err = binary.Read(reader, nativeEndian, &(api.processCount))
			if err != nil {
				return false, err
			}
			err = binary.Read(reader, nativeEndian, &(api.processCountMax))
			if err != nil {
				return false, err
			}
			err = binary.Read(reader, nativeEndian, &(api.processCountMin))
			if err != nil {
				return false, err
			}
			var prefixSize uint32
			err = binary.Read(reader, nativeEndian, &prefixSize)
			if err != nil {
				return false, err
			}
			prefix := make([]byte, prefixSize-1)
			_, err = reader.Read(prefix)
			if err != nil {
				return false, err
			}
			api.prefix = string(prefix)
			_, err = reader.ReadByte() // null terminator
			if err != nil {
				return false, err
			}
			err = binary.Read(reader, nativeEndian, &(api.timeoutInitialize))
			if err != nil {
				return false, err
			}
			err = binary.Read(reader, nativeEndian, &(api.timeoutAsync))
			if err != nil {
				return false, err
			}
			err = binary.Read(reader, nativeEndian, &(api.timeoutSync))
			if err != nil {
				return false, err
			}
			err = binary.Read(reader, nativeEndian, &(api.timeoutTerminate))
			if err != nil {
				return false, err
			}
			err = binary.Read(reader, nativeEndian, &(api.priorityDefault))
			if err != nil {
				return false, err
			}
			err = binary.Read(reader, nativeEndian, &(api.fatalExceptions))
			if err != nil {
				return false, err
			}
			var bind int32
			err = binary.Read(reader, nativeEndian, &bind)
			if err != nil {
				return false, err
			}
			if bind >= 0 {
				return false, invalidInputErrorNew()
			}
			if reader.Len() > 0 {
				_, err = api.handleEvents(external, reader, 0)
				if err != nil {
					return false, err
				}
			}
			return false, nil
		case messageSendAsync:
			fallthrough
		case messageSendSync:
			var nameSize uint32
			err = binary.Read(reader, nativeEndian, &nameSize)
			if err != nil {
				return false, err
			}
			name := make([]byte, nameSize-1)
			_, err = reader.Read(name)
			if err != nil {
				return false, err
			}
			_, err = reader.ReadByte() // null terminator
			if err != nil {
				return false, err
			}
			var patternSize uint32
			err = binary.Read(reader, nativeEndian, &patternSize)
			if err != nil {
				return false, err
			}
			pattern := make([]byte, patternSize-1)
			_, err = reader.Read(pattern)
			if err != nil {
				return false, err
			}
			_, err = reader.ReadByte() // null terminator
			if err != nil {
				return false, err
			}
			var requestInfoSize uint32
			err = binary.Read(reader, nativeEndian, &requestInfoSize)
			if err != nil {
				return false, err
			}
			requestInfo := make([]byte, requestInfoSize)
			_, err = reader.Read(requestInfo)
			if err != nil {
				return false, err
			}
			_, err = reader.ReadByte() // null terminator
			if err != nil {
				return false, err
			}
			var requestSize uint32
			err = binary.Read(reader, nativeEndian, &requestSize)
			if err != nil {
				return false, err
			}
			request := make([]byte, requestSize)
			_, err = reader.Read(request)
			if err != nil {
				return false, err
			}
			_, err = reader.ReadByte() // null terminator
			if err != nil {
				return false, err
			}
			var requestTimeout uint32
			err = binary.Read(reader, nativeEndian, &requestTimeout)
			if err != nil {
				return false, err
			}
			var priority int8
			err = binary.Read(reader, nativeEndian, &priority)
			if err != nil {
				return false, err
			}
			var transId [16]byte
			_, err = reader.Read(transId[:])
			if err != nil {
				return false, err
			}
			var sourceSize uint32
			err = binary.Read(reader, nativeEndian, &sourceSize)
			if err != nil {
				return false, err
			}
			sourceBinary := make([]byte, sourceSize)
			_, err = reader.Read(sourceBinary)
			if err != nil {
				return false, err
			}
			var source interface{}
			source, err = erlang.BinaryToTerm(sourceBinary)
			if err != nil {
				return false, err
			}
			if reader.Len() > 0 {
				var handled bool
				handled, err = api.handleEvents(external, reader, 0)
				if err != nil {
					return false, err
				}
				if !handled {
					return false, nil
				}
			}
			err = api.callback(command, string(name), string(pattern), requestInfo, request, requestTimeout, priority, transId, Source(source.(erlang.OtpErlangPid)))
			if err != nil {
				return false, err
			}
			if api.terminate {
				return false, nil
			}
		case messageRecvAsync:
			fallthrough
		case messageReturnSync:
			var responseInfoSize uint32
			err = binary.Read(reader, nativeEndian, &responseInfoSize)
			if err != nil {
				return false, err
			}
			responseInfo := make([]byte, responseInfoSize)
			_, err = reader.Read(responseInfo)
			if err != nil {
				return false, err
			}
			_, err = reader.ReadByte() // null terminator
			if err != nil {
				return false, err
			}
			var responseSize uint32
			err = binary.Read(reader, nativeEndian, &responseSize)
			if err != nil {
				return false, err
			}
			response := make([]byte, responseSize)
			_, err = reader.Read(response)
			if err != nil {
				return false, err
			}
			_, err = reader.ReadByte() // null terminator
			if err != nil {
				return false, err
			}
			transId := make([]byte, 16)
			_, err = reader.Read(transId)
			if err != nil {
				return false, err
			}
			if reader.Len() > 0 {
				_, err = api.handleEvents(external, reader, 0)
				if err != nil {
					return false, err
				}
			}
			api.responseInfo = responseInfo
			api.response = response
			api.transId = transId
			return false, nil
		case messageReturnAsync:
			transId := make([]byte, 16)
			_, err = reader.Read(transId)
			if err != nil {
				return false, err
			}
			if reader.Len() > 0 {
				_, err = api.handleEvents(external, reader, 0)
				if err != nil {
					return false, err
				}
			}
			api.transId = transId
			return false, nil
		case messageReturnsAsync:
			var transIdCount uint32
			err = binary.Read(reader, nativeEndian, &transIdCount)
			if err != nil {
				return false, err
			}
			transIds := make([][]byte, transIdCount)
			for i := uint32(0); i < transIdCount; i++ {
				transId := make([]byte, 16)
				_, err = reader.Read(transId)
				if err != nil {
					return false, err
				}
				transIds[i] = transId
			}
			if reader.Len() > 0 {
				_, err = api.handleEvents(external, reader, 0)
				if err != nil {
					return false, err
				}
			}
			api.transIds = transIds
			return false, nil
		case messageSubscribeCount:
			var subscribeCount uint32
			err = binary.Read(reader, nativeEndian, &subscribeCount)
			if err != nil {
				return false, err
			}
			if reader.Len() > 0 {
				_, err = api.handleEvents(external, reader, 0)
				if err != nil {
					return false, err
				}
			}
			api.subscribeCount = subscribeCount
			return false, nil
		case messageTerm:
			_, err = api.handleEvents(external, reader, command)
			if err != nil {
				return false, err
			}
			return false, nil
		case messageReinit:
			err = binary.Read(reader, nativeEndian, &(api.processCount))
			if err != nil {
				return false, err
			}
			err = binary.Read(reader, nativeEndian, &(api.timeoutAsync))
			if err != nil {
				return false, err
			}
			err = binary.Read(reader, nativeEndian, &(api.timeoutSync))
			if err != nil {
				return false, err
			}
			err = binary.Read(reader, nativeEndian, &(api.priorityDefault))
			if err != nil {
				return false, err
			}
			err = binary.Read(reader, nativeEndian, &(api.fatalExceptions))
			if err != nil {
				return false, err
			}
		case messageKeepalive:
			var keepalive []byte
			keepalive, err = erlang.TermToBinary(erlang.OtpErlangAtom("keepalive"), -1)
			if err != nil {
				return false, err
			}
			err = api.send(keepalive)
			if err != nil {
				return false, err
			}
		default:
			return false, messageDecodingErrorNew()
		}

		if timeout == 0 {
			return true, nil
		} else if timeout > 0 {
			if time.Now().Sub(pollTimer) > time.Duration(timeout)*time.Millisecond {
				return true, nil
			}
		}
	}
	return false, nil
}

// Poll blocks to process incoming CloudI service requests
func (api *Instance) Poll(timeout int32) (bool, error) {
	return api.pollRequest(timeout, true)
}

// Shutdown the service successfully
func (api *Instance) Shutdown(extra ...interface{}) error {
	extraArity := len(extra)
	if extraArity > 1 {
		return invalidInputErrorNew()
	}
	reason := ""
	for _, extraArg := range extra {
		switch arg := extraArg.(type) {
		case string:
			reason = arg
		default:
			return invalidInputErrorNew()
		}
	}
	shutdown, err := erlang.TermToBinary([]interface{}{erlang.OtpErlangAtom("shutdown"), reason}, -1)
	if err != nil {
		return err
	}
	err = api.send(shutdown)
	if err != nil {
		return err
	}
	return nil
}

func textPairsParse(text []byte) map[string][]string {
	pairs := map[string][]string{}
	textSegments := bytes.Split(text, []byte{0})
	for i := 0; i < len(textSegments)-1; i += 2 {
		key := string(textSegments[i])
		value := string(textSegments[i+1])
		current := pairs[key]
		if current == nil {
			pairs[key] = []string{value}
		} else {
			pairs[key] = append(current, value)
		}
	}
	return pairs
}

func textPairsNew(pairs map[string][]string, response bool) ([]byte, error) {
	var textBuffer = new(bytes.Buffer)
	var err error
	for key, values := range pairs {
		for i := 0; i < len(values); i++ {
			_, err = textBuffer.WriteString(key)
			if err != nil {
				return nil, err
			}
			err = textBuffer.WriteByte(0)
			if err != nil {
				return nil, err
			}
			_, err = textBuffer.WriteString(values[i])
			if err != nil {
				return nil, err
			}
			err = textBuffer.WriteByte(0)
			if err != nil {
				return nil, err
			}
		}
	}
	if response && textBuffer.Len() == 0 {
		err = textBuffer.WriteByte(0)
		if err != nil {
			return nil, err
		}
	}
	text := textBuffer.Bytes()
	return text, nil
}

// InfoKeyValueParse decodes service request info key/value data
func InfoKeyValueParse(info []byte) map[string][]string {
	return textPairsParse(info)
}

// InfoKeyValueNew encodes service response info key/value data
func InfoKeyValueNew(pairs map[string][]string, extra ...interface{}) ([]byte, error) {
	extraArity := len(extra)
	if extraArity > 1 {
		return nil, invalidInputErrorNew()
	}
	response := true
	for _, extraArg := range extra {
		switch arg := extraArg.(type) {
		case bool:
			response = arg
		default:
			return nil, invalidInputErrorNew()
		}
	}
	return textPairsNew(pairs, response)
}

func (api *Instance) send(data []byte) error {
	var err error
	if api.useHeader {
		var buffer *bytes.Buffer = new(bytes.Buffer)
		length := uint32(len(data))
		buffer.Grow(int(4 + length))
		err = binary.Write(buffer, binary.BigEndian, length)
		if err != nil {
			return err
		}
		_, err = buffer.Write(data)
		if err != nil {
			return err
		}
		data = buffer.Bytes()
	}
	_, err = api.socket.Write(data)
	return err
}

func (api *Instance) recv() ([]byte, error) {
	var err error
	var i int
	var total int
	if api.useHeader {
		for api.bufferRecv.Len() < 4 {
			_, err = api.recvFragment(4 - api.bufferRecv.Len())
			if err != nil {
				return nil, err
			}
		}
		header := make([]byte, 4)
		i, err = api.bufferRecv.Read(header)
		if err != nil && i != 4 {
			return nil, err
		}
		var length uint32
		err = binary.Read(bytes.NewReader(header), binary.BigEndian, &length)
		if err != nil {
			return nil, err
		}
		total = int(length)
		api.bufferRecv.Grow(total)
		for api.bufferRecv.Len() < total {
			_, err = api.recvFragment(total - api.bufferRecv.Len())
			if err != nil {
				return nil, err
			}
		}
	} else {
		ready := true
		nonblocking := false
		for ready {
			i, err = api.recvFragment(0)
			if err != nil {
				switch errNet := err.(type) {
				case *net.OpError:
					if !(errNet.Timeout() && api.bufferRecv.Len() > 0) {
						return nil, err
					}
				default:
					return nil, err
				}
			}
			ready = (i == int(api.fragmentSize))
			if ready && !nonblocking {
				nonblocking = true
				err = api.socket.SetReadDeadline(time.Now().Add(time.Duration(100) * time.Nanosecond))
				if err != nil {
					return nil, err
				}
				defer func() {
					_ = api.socket.SetReadDeadline(time.Time{})
				}()
			}
		}
		total = api.bufferRecv.Len()
	}
	recv := make([]byte, total)
	i, err = api.bufferRecv.Read(recv)
	if err != nil && i != total {
		return nil, err
	}
	return recv, nil
}

func (api *Instance) recvFragment(size int) (int, error) {
	recvSize := int(api.fragmentSize)
	if size != 0 && recvSize > size {
		recvSize = size
	}
	i, err1 := api.socket.Read(api.fragmentRecv[:recvSize])
	var err2 error
	if i > 0 {
		_, err2 = api.bufferRecv.Write(api.fragmentRecv[:i])
	}
	if err1 != nil {
		return i, err1
	}
	if err2 != nil {
		return i, err2
	}
	return i, nil
}

func uintGetenv(key string) (uint32, error) {
	s := os.Getenv(key)
	if s == "" {
		return 0, invalidInputErrorNew()
	}
	i, err := strconv.Atoi(s)
	if err != nil || i < 0 || uint64(i) > math.MaxUint32 {
		return 0, invalidInputErrorNew()
	}
	return uint32(i), nil
}

// StackError provides an interface for error structs that provide a stacktrace
type StackError interface {
	Stack() []byte
}

// StackErrorWrap is used to attach a stacktrace to panic data as an error
type StackErrorWrap struct {
	stack []byte
	Value error
}

// StackErrorWrapNew creates a new error with a stack trace from any data
func StackErrorWrapNew(value interface{}) error {
	stack := debug.Stack()
	if err, ok := value.(error); ok {
		return &StackErrorWrap{stack: stack, Value: err}
	}
	return &StackErrorWrap{stack: stack, Value: fmt.Errorf("%#v", value)}
}

func (e *StackErrorWrap) Error() string {
	output := new(bytes.Buffer)
	errorFormat(output, e.Value)
	stackErrorFormat(output, e.stack)
	return output.String()
}

// Stack return the stack stored when the error was created
func (e *StackErrorWrap) Stack() []byte {
	return e.stack
}
func errorFormat(output *bytes.Buffer, err error) {
	_, _ = output.WriteString(reflect.TypeOf(err).String())
	_, _ = output.WriteString(": ")
	_, _ = output.WriteString(err.Error())
	_, _ = output.WriteString("\n")
}
func stackErrorFormat(output *bytes.Buffer, stack []byte) {
	_, _ = output.WriteString("\t")
	_, _ = output.Write(bytes.TrimSuffix(bytes.Join(bytes.Split(stack, []byte{'\n'}), []byte{'\n', '\t'}), []byte{'\t'}))
}

// InvalidInputError indicates that invalid input was provided
type InvalidInputError struct {
	stack []byte
}

func invalidInputErrorNew() error {
	return &InvalidInputError{stack: debug.Stack()}
}
func (e *InvalidInputError) Error() string {
	return "Invalid Input"
}

// Stack return the stack stored when the error was created
func (e *InvalidInputError) Stack() []byte {
	return e.stack
}

// ReturnSyncError indicates a request was handled with a sync return
type ReturnSyncError struct {
}

func returnSyncErrorNew() error {
	return &ReturnSyncError{}
}
func (e *ReturnSyncError) Error() string {
	return "Synchronous Call Return Invalid"
}

// ReturnAsyncError indicates a request was handled with an async return
type ReturnAsyncError struct {
}

func returnAsyncErrorNew() error {
	return &ReturnAsyncError{}
}
func (e *ReturnAsyncError) Error() string {
	return "Asynchronous Call Return Invalid"
}

// ForwardSyncError indicates a request was handled with a sync forward
type ForwardSyncError struct {
}

func forwardSyncErrorNew() error {
	return &ForwardSyncError{}
}
func (e *ForwardSyncError) Error() string {
	return "Synchronous Call Forward Invalid"
}

// ForwardAsyncError indicates a request was handled with an async forward
type ForwardAsyncError struct {
}

func forwardAsyncErrorNew() error {
	return &ForwardAsyncError{}
}
func (e *ForwardAsyncError) Error() string {
	return "Asynchronous Call Forward Invalid"
}

// MessageDecodingError indicates an error decoding CloudI messages
type MessageDecodingError struct {
	stack []byte
}

func messageDecodingErrorNew() error {
	return &MessageDecodingError{stack: debug.Stack()}
}
func (e *MessageDecodingError) Error() string {
	return "Message Decoding Error"
}

// Stack return the stack stored when the error was created
func (e *MessageDecodingError) Stack() []byte {
	return e.stack
}

// TerminateError indicates that unavoidable termination is occurring
type TerminateError struct {
	timeout uint32
}

func terminateErrorNew(timeout uint32) error {
	return &TerminateError{timeout: timeout}
}
func (e *TerminateError) Error() string {
	return "Terminate"
}

// Timeout provides the termination timeout configured for the service
func (e *TerminateError) Timeout() uint32 {
	return e.timeout
}

// AssertionError indicates an unrecoverable condition in a CloudI service
type AssertionError struct {
	stack []byte
}

// Assert that a value is true
func Assert(value bool) {
	if !value {
		panic(&AssertionError{stack: debug.Stack()})
	}
}
func (e *AssertionError) Error() string {
	return "Assertion failed !"
}

// Stack return the stack stored when the error was created
func (e *AssertionError) Stack() []byte {
	return e.stack
}

// FatalError indicates an unrecoverable error in a CloudI service
type FatalError struct {
	stack []byte
}

// Create a FatalError
func FatalErrorNew() error {
	return &FatalError{stack: debug.Stack()}
}
func (e *FatalError) Error() string {
	return "Fatal Error"
}

// Stack return the stack stored when the error was created
func (e *FatalError) Stack() []byte {
	return e.stack
}

// ErrorWrite outputs error information to the cloudi.log file through stderr
func ErrorWrite(stream *os.File, err error) {
	output := new(bytes.Buffer)
	if wrap, ok := err.(*StackErrorWrap); ok {
		errorFormat(output, wrap.Value)
	} else {
		errorFormat(output, err)
	}
	if serr, ok := err.(StackError); ok {
		stackErrorFormat(output, serr.Stack())
	}
	_, _ = stream.Write(output.Bytes())
}

// ErrorExit outputs error information and exits
func ErrorExit(stream *os.File, err error) {
	ErrorWrite(stream, err)
	os.Exit(1)
}
