package main

//-*-Mode:Go;coding:utf-8;tab-width:4;c-basic-offset:4-*-
// ex: set ft=go fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:
//
// MIT License
//
// Copyright (c) 2017-2022 Michael Truog <mjtruog at protonmail dot com>
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
	"fmt"
	"github.com/CloudI/cloudi_api_go/v2/cloudi"
	"os"
	"sync"
)

type serviceState struct {
	count uint32
}

func request(requestType int, name, pattern string, requestInfo, request []byte, timeout uint32, priority int8, transId [16]byte, source cloudi.Source, state interface{}, api *cloudi.Instance) ([]byte, []byte, error) {
	stateP := state.(*serviceState)
	if stateP.count == 4294967295 {
		stateP.count = 0
	} else {
		stateP.count += 1
	}
	fmt.Printf("count == %d go\n", stateP.count)
	response := []byte(fmt.Sprintf("%d", stateP.count))
	var responseInfo []byte
	var err error
	responseInfo, err = cloudi.InfoKeyValueNew(map[string][]string{})
	if err != nil {
		return nil, nil, err
	}
	api.Return(requestType, name, pattern, responseInfo, response, timeout, transId, source)
	// execution doesn't reach here
	return nil, nil, nil
}

func task(threadIndex uint32, execution *sync.WaitGroup) {
	defer execution.Done()
	stateP := &serviceState{count: 0}
	api, err := cloudi.API(threadIndex, stateP)
	if err != nil {
		cloudi.ErrorWrite(os.Stderr, err)
		return
	}
	err = api.Subscribe("go/get", request)
	if err != nil {
		cloudi.ErrorWrite(os.Stderr, err)
		return
	}
	_, err = api.Poll(-1)
	if err != nil {
		cloudi.ErrorWrite(os.Stderr, err)
	}
	os.Stdout.WriteString("terminate count go\n")
}

func assert(value interface{}, expected interface{}) {
	if value == expected {
		return
	}
	panic("assert failed!")
}

func main() {
	threadCount, err := cloudi.ThreadCount()
	if err != nil {
		cloudi.ErrorExit(os.Stderr, err)
	}
	var execution sync.WaitGroup
	for threadIndex := uint32(0); threadIndex < threadCount; threadIndex++ {
		execution.Add(1)
		go task(threadIndex, &execution)
	}
	execution.Wait()
}
