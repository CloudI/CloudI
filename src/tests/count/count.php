#!/usr/bin/env php
<?php //-*-coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=php fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
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

require 'CloudI.php';

class Task //extends \Thread
{
    private $api;

    public function __construct($api)
    {
        $this->api = $api;
        $this->count = 0;
    }

    public function run()
    {
        try
        {
            $this->api->subscribe('php/get', $this, 'request');
            $result = $this->api->poll();
            assert($result === false);
        }
        catch (\CloudI\TerminateException $e)
        {
        }
        catch (Exception $e)
        {
            error_log("{$e->getMessage()}\n{$e}\n");
        }
        echo "terminate count php\n";
    }

    public function request($request_type, $name, $pattern,
                            $request_info, $request,
                            $timeout, $priority, $trans_id, $pid)
    {
        if ($this->count == 4294967295)
        {
            $this->count = 0;
        }
        else
        {
            $this->count += 1;
        }
        echo "count == {$this->count} php\n";
        $response = "{$this->count}";
        $response_info = \CloudI\API::info_key_value_new(array());
        $this->api->return_($request_type, $name, $pattern,
                            $response_info, $response,
                            $timeout, $trans_id, $pid);
    }
}

$thread_count = \CloudI\API::thread_count();
assert($thread_count == 1);
$main_thread = new Task(new \CloudI\API(0));
$main_thread->run();

/*
// commented out due to PHP threads not having
// readily available installation packages
$thread_count = \CloudI\API::thread_count();
assert($thread_count >= 1);
    
$threads = array();
for ($i = 0; $i < $thread_count; $i++)
{
    $threads[$i] = new Task(new \CloudI\API($i));
}
foreach ($threads as $t)
    $t->start();
foreach ($threads as $t)
    $t->join();
*/

?>
