package main

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
//   notice, this list of conditions and the following disclaimer.
//   * Redistributions in binary form must reproduce the above copyright
//   notice, this list of conditions and the following disclaimer in
//   the documentation and/or other materials provided with the
//   distribution.
//   * All advertising materials mentioning features or use of this
//   software must display the following acknowledgment:
//     This product includes software developed by Michael Truog
//   * The name of the author may not be used to endorse or promote
//   products derived from this software without specific prior
//   written permission
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
	"cloudi"
	"fmt"
	"os"
	"strconv"
	"sync"
)

func request(api *cloudi.Instance, requestType int, name, pattern string, requestInfo, request []byte, timeout uint32, priority int8, transId [16]byte, pid cloudi.Source) ([]byte, []byte, error) {
	httpQs := api.RequestHttpQsParse(request)
	value := httpQs["value"]
	var valueInt int
	var err error
	if value != nil {
		valueInt, err = strconv.Atoi(value[0])
		if err != nil {
			value = nil
		}
	}
	var response []byte
	if value == nil {
		response = []byte("<http_test><error>no value specified</error></http_test>")
	} else {
		response = []byte(fmt.Sprintf("<http_test><value>%d</value></http_test>", valueInt))
	}
	api.Return(requestType, name, pattern, []byte{}, response, timeout, transId, pid)
	// execution doesn't reach here
	return nil, nil, nil
}

func task(threadIndex uint32, execution *sync.WaitGroup) {
	defer execution.Done()
	api, err := cloudi.API(threadIndex)
	if err != nil {
		cloudi.ErrorWrite(os.Stderr, err)
		return
	}
	var count1 uint32
	count1, err = api.SubscribeCount("go.xml/get")
	if err != nil {
		cloudi.ErrorWrite(os.Stderr, err)
		return
	}
	assert(count1, uint32(0))
	err = api.Subscribe("go.xml/get", request)
	if err != nil {
		cloudi.ErrorWrite(os.Stderr, err)
		return
	}
	var count2 uint32
	count2, err = api.SubscribeCount("go.xml/get")
	if err != nil {
		cloudi.ErrorWrite(os.Stderr, err)
		return
	}
	assert(count2, uint32(1))
	_, err = api.Poll(-1)
	if err != nil {
		cloudi.ErrorWrite(os.Stderr, err)
	}
	os.Stdout.WriteString("terminate http_req go\n")
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
