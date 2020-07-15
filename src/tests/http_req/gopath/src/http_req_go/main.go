package main

//-*-Mode:Go;coding:utf-8;tab-width:4;c-basic-offset:4-*-
// ex: set ft=go fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:
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

import (
	"cloudi"
	"fmt"
	"os"
	"strconv"
	"sync"
)

func request(requestType int, name, pattern string, requestInfo, request []byte, timeout uint32, priority int8, transId [16]byte, pid cloudi.Source, state interface{}, api *cloudi.Instance) ([]byte, []byte, error) {
	httpQs := cloudi.InfoKeyValueParse(request)
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
		cloudi.Assert(valueInt < 20)
		response = []byte(fmt.Sprintf("<http_test><value>%d</value></http_test>", valueInt))
	}
	var responseInfo []byte
	responseInfo, err = cloudi.InfoKeyValueNew(map[string][]string{
		"content-type": {"text/xml; charset=utf-8"},
	})
	if err != nil {
		return nil, nil, err
	}
	api.Return(requestType, name, pattern, responseInfo, response, timeout, transId, pid)
	// execution doesn't reach here
	return nil, nil, nil
}

func task(threadIndex uint32, execution *sync.WaitGroup) {
	defer execution.Done()
	api, err := cloudi.API(threadIndex, nil)
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
