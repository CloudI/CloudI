#!/usr/bin/env php
<?php //-*-coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=php fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
//
// BSD LICENSE
// 
// Copyright (c) 2014, Michael Truog <mjtruog at gmail dot com>
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
// 
//     * Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above copyright
//       notice, this list of conditions and the following disclaimer in
//       the documentation and/or other materials provided with the
//       distribution.
//     * All advertising materials mentioning features or use of this
//       software must display the following acknowledgment:
//         This product includes software developed by Michael Truog
//     * The name of the author may not be used to endorse or promote
//       products derived from this software without specific prior
//       written permission
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

require dirname(__FILE__) . '/../../api/php/CloudI.php';

define('DESTINATION', '/tests/msg_size/erlang');

class Task //extends \Thread
{
    private $api;

    public function __construct($api)
    {
        $this->api = $api;
    }

    public function run()
    {
        try
        {
            $this->api->subscribe('php', $this, 'request');

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
        echo "terminate msg_size php\n";
    }

    public function request($command, $name, $pattern, $request_info, $request,
                            $timeout, $priority, $trans_id, $pid)
    {
        list(, $i) = unpack('L', substr($request, 0, 4));
        if ($i == 4294967295)
            $i = 0;
        else
            $i++;
        $request = pack('L', $i) . substr($request, 4);
        echo "forward #{$i} php to " . DESTINATION .
             " (with timeout {$timeout} ms)\n";
        $this->api->forward_($command, DESTINATION, $request_info, $request,
                             $timeout, $priority, $trans_id, $pid);
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
