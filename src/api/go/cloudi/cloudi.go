package cloudi

//-*-Mode:Go;coding:utf-8;tab-width:4;c-basic-offset:4-*-
// ex: set ft=go fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:
//
// BSD LICENSE
//
// Copyright (c) 2017, Michael Truog <mjtruog at gmail dot com>
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
//   * Redistributions of source code must retain the above copyright
//	 notice, this list of conditions and the following disclaimer.
//   * Redistributions in binary form must reproduce the above copyright
//	 notice, this list of conditions and the following disclaimer in
//	 the documentation and/or other materials provided with the
//	 distribution.
//   * All advertising materials mentioning features or use of this
//	 software must display the following acknowledgment:
//	   This product includes software developed by Michael Truog
//   * The name of the author may not be used to endorse or promote
//	 products derived from this software without specific prior
//	 written permission
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

import (
	"bytes"
	"container/list"
	"encoding/binary"
	"erlang"
	"math"
	"net"
	"os"
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
	// Asynchronous service request
	ASYNC = 1
	// Synchronous service request
	SYNC = -1
)

var nativeEndian binary.ByteOrder

// Instance is an instance of the CloudI API
type Instance struct {
	socket                   net.Conn
	useHeader                bool
	initializationComplete   bool
	terminate                bool
	fragmentSize             uint32
	fragmentRecv             []byte
	callbacks                map[string]*list.List
	bufferRecv               *bytes.Buffer
	processIndex             uint32
	processCount             uint32
	processCountMax          uint32
	processCountMin          uint32
	prefix                   string
	timeoutInitialize        uint32
	timeoutAsync             uint32
	timeoutSync              uint32
	timeoutTerminate         uint32
	priorityDefault          int8
	requestTimeoutAdjustment bool
}

// Callback is a function to handle a service request
type Callback func(*Instance, int, string, string, []byte, []byte, uint32, int8, []byte, []byte) error

// API creates an instance of the CloudI API
func API(threadIndex uint32) (*Instance, error) {
	protocol := os.Getenv("CLOUDI_API_INIT_PROTOCOL")
	if protocol == "" {
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
	bufferRecv := new(bytes.Buffer)
	bufferRecv.Grow(int(bufferSize))
	timeoutTerminate := uint32(1000)
	api := &Instance{socket: socket, useHeader: useHeader, fragmentSize: bufferSize, fragmentRecv: fragmentRecv, bufferRecv: bufferRecv, timeoutTerminate: timeoutTerminate}
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

func ThreadCount() (uint32, error) {
	return uintGetenv("CLOUDI_API_INIT_THREAD_COUNT")
}

//XXX

func (api *Instance) ProcessIndex() uint32 {
	return api.processIndex
}

func (api *Instance) ProcessCount() uint32 {
	return api.processCount
}

func (api *Instance) ProcessCountMax() uint32 {
	return api.processCountMax
}

func (api *Instance) ProcessCountMin() uint32 {
	return api.processCountMin
}

func (api *Instance) Prefix() string {
	return api.prefix
}

func (api *Instance) TimeoutInitialize() uint32 {
	return api.timeoutInitialize
}

func (api *Instance) TimeoutAsync() uint32 {
	return api.timeoutAsync
}

func (api *Instance) TimeoutSync() uint32 {
	return api.timeoutSync
}

func (api *Instance) TimeoutTerminate() uint32 {
	return api.timeoutTerminate
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
		return false, nil
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
			switch err.(type) {
			case *net.OpError:
				if err.(*net.OpError).Timeout() {
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
			var requestTimeoutAdjustment uint8
			err = binary.Read(reader, nativeEndian, &requestTimeoutAdjustment)
			if err != nil {
				return false, err
			}
			api.requestTimeoutAdjustment = (requestTimeoutAdjustment != 0)
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
			//XXX
			return false, messageDecodingErrorNew()
		case messageRecvAsync:
			fallthrough
		case messageReturnSync:
			//XXX
			return false, messageDecodingErrorNew()
		case messageReturnAsync:
			//XXX
			return false, messageDecodingErrorNew()
		case messageReturnsAsync:
			//XXX
			return false, messageDecodingErrorNew()
		case messageSubscribeCount:
			//XXX
			return false, messageDecodingErrorNew()
		case messageReinit:
			//XXX
			return false, messageDecodingErrorNew()
		case messageKeepalive:
			//XXX
			return false, messageDecodingErrorNew()
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
	var total uint32
	if api.useHeader {
		for api.bufferRecv.Len() < 4 {
			_, err = api.recvFragment()
			if err != nil {
				return nil, err
			}
		}
		header := make([]byte, 4)
		copy(header, api.bufferRecv.Bytes()[:4])
		err = binary.Read(bytes.NewReader(header), binary.BigEndian, &total)
		if err != nil {
			return nil, err
		}
		api.bufferRecv.Grow(int(total))
		for api.bufferRecv.Len() < int(total) {
			_, err = api.recvFragment()
			if err != nil {
				return nil, err
			}
		}
		i, err = api.bufferRecv.Read(header)
		if err != nil && i != 4 {
			return nil, err
		}
	} else {
		ready := true
		nonblocking := false
		for ready {
			i, err = api.recvFragment()
			if err != nil {
				switch err.(type) {
				case *net.OpError:
					if !(err.(*net.OpError).Timeout() && api.bufferRecv.Len() > 0) {
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
		total = uint32(api.bufferRecv.Len())
	}
	recv := make([]byte, total)
	i, err = api.bufferRecv.Read(recv)
	if err != nil && i != int(total) {
		return nil, err
	}
	return recv, nil
}

func (api *Instance) recvFragment() (int, error) {
	i, err1 := api.socket.Read(api.fragmentRecv)
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
	if err != nil || i < 0 || i > math.MaxUint32 {
		return 0, invalidInputErrorNew()
	}
	return uint32(i), nil
}

// InvalidInputError indicates that invalid input was provided
type InvalidInputError struct {
}

func invalidInputErrorNew() error {
	return &InvalidInputError{}
}
func (e *InvalidInputError) Error() string {
	return "Invalid Input"
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
}

func messageDecodingErrorNew() error {
	return &MessageDecodingError{}
}
func (e *MessageDecodingError) Error() string {
	return "Message Decoding Error"
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
func (e *TerminateError) Timeout() uint32 {
	return e.timeout
}
