#!/usr/bin/env php
<?php //-*-coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=php fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
//
// MIT License
//
// Copyright (c) 2014-2017 Michael Truog <mjtruog at gmail dot com>
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
    }

    public function run()
    {
        try
        {
            $this->api->subscribe('a/b/c/d', $this, 'sequence1_abcd');
            $this->api->subscribe('a/b/c/*', $this, 'sequence1_abc_');
            $this->api->subscribe('a/b/*/d', $this, 'sequence1_ab_d');
            $this->api->subscribe('a/*/c/d', $this, 'sequence1_a_cd');
            $this->api->subscribe('*/b/c/d', $this, 'sequence1__bcd');
            $this->api->subscribe('a/b/*',   $this, 'sequence1_ab__');
            $this->api->subscribe('a/*/d',   $this, 'sequence1_a__d');
            $this->api->subscribe('*/c/d',   $this, 'sequence1___cd');
            $this->api->subscribe('a/*',     $this, 'sequence1_a___');
            $this->api->subscribe('*/d',     $this, 'sequence1____d');
            $this->api->subscribe('*',       $this, 'sequence1_____');
            $this->api->subscribe('sequence1', $this, 'sequence1');
            $this->api->subscribe('e', $this, 'sequence2_e1');
            $this->api->subscribe('e', $this, 'sequence2_e2');
            $this->api->subscribe('e', $this, 'sequence2_e3');
            $this->api->subscribe('e', $this, 'sequence2_e4');
            $this->api->subscribe('e', $this, 'sequence2_e5');
            $this->api->subscribe('e', $this, 'sequence2_e6');
            $this->api->subscribe('e', $this, 'sequence2_e7');
            $this->api->subscribe('e', $this, 'sequence2_e8');
            $this->api->subscribe('sequence2', $this, 'sequence2');
            $this->api->subscribe('f1', $this, 'sequence3_f1');
            $this->api->subscribe('f2', $this, 'sequence3_f2');
            $this->api->subscribe('g1', $this, 'sequence3_g1');
            $this->api->subscribe('sequence3', $this, 'sequence3');
            if ($this->api->process_index() == 0)
            {
                // start sequence1
                $this->api->send_async(
                    $this->api->prefix() . 'sequence1', 'start');
            }
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
        echo "terminate messaging php\n";
    }

    public function sequence1_abcd($request_type, $name, $pattern,
                                   $request_info, $request,
                                   $timeout, $priority, $trans_id, $pid)
    {
        assert($pattern == $this->api->prefix() . 'a/b/c/d');
        assert($request == 'test1');
        $this->api->return_($request_type, $name, $pattern,
                            '', $request, $timeout, $trans_id, $pid);
    }

    public function sequence1_abc_($request_type, $name, $pattern,
                                   $request_info, $request,
                                   $timeout, $priority, $trans_id, $pid)
    {
        assert($pattern == $this->api->prefix() . 'a/b/c/*');
        assert($request == 'test2' || $request == 'test3');
        $this->api->return_($request_type, $name, $pattern,
                            '', $request, $timeout, $trans_id, $pid);
    }

    public function sequence1_ab_d($request_type, $name, $pattern,
                                   $request_info, $request,
                                   $timeout, $priority, $trans_id, $pid)
    {
        assert($pattern == $this->api->prefix() . 'a/b/*/d');
        assert($request == 'test4' || $request == 'test5');
        $this->api->return_($request_type, $name, $pattern,
                            '', $request, $timeout, $trans_id, $pid);
    }

    public function sequence1_a_cd($request_type, $name, $pattern,
                                   $request_info, $request,
                                   $timeout, $priority, $trans_id, $pid)
    {
        assert($pattern == $this->api->prefix() . 'a/*/c/d');
        assert($request == 'test6' || $request == 'test7');
        $this->api->return_($request_type, $name, $pattern,
                            '', $request, $timeout, $trans_id, $pid);
    }

    public function sequence1__bcd($request_type, $name, $pattern,
                                   $request_info, $request,
                                   $timeout, $priority, $trans_id, $pid)
    {
        assert($pattern == $this->api->prefix() . '*/b/c/d');
        assert($request == 'test8' || $request == 'test9');
        $this->api->return_($request_type, $name, $pattern,
                            '', $request, $timeout, $trans_id, $pid);
    }

    public function sequence1_ab__($request_type, $name, $pattern,
                                   $request_info, $request,
                                   $timeout, $priority, $trans_id, $pid)
    {
        assert($pattern == $this->api->prefix() . 'a/b/*');
        assert($request == 'test10');
        $this->api->return_($request_type, $name, $pattern,
                            '', $request, $timeout, $trans_id, $pid);
    }

    public function sequence1_a__d($request_type, $name, $pattern,
                                   $request_info, $request,
                                   $timeout, $priority, $trans_id, $pid)
    {
        assert($pattern == $this->api->prefix() . 'a/*/d');
        assert($request == 'test11');
        $this->api->return_($request_type, $name, $pattern,
                            '', $request, $timeout, $trans_id, $pid);
    }

    public function sequence1___cd($request_type, $name, $pattern,
                                   $request_info, $request,
                                   $timeout, $priority, $trans_id, $pid)
    {
        assert($pattern == $this->api->prefix() . '*/c/d');
        assert($request == 'test12');
        $this->api->return_($request_type, $name, $pattern,
                            '', $request, $timeout, $trans_id, $pid);
    }

    public function sequence1_a___($request_type, $name, $pattern,
                                   $request_info, $request,
                                   $timeout, $priority, $trans_id, $pid)
    {
        assert($pattern == $this->api->prefix() . 'a/*');
        assert($request == 'test13');
        $this->api->return_($request_type, $name, $pattern,
                            '', $request, $timeout, $trans_id, $pid);
    }

    public function sequence1____d($request_type, $name, $pattern,
                                   $request_info, $request,
                                   $timeout, $priority, $trans_id, $pid)
    {
        assert($pattern == $this->api->prefix() . '*/d');
        assert($request == 'test14');
        $this->api->return_($request_type, $name, $pattern,
                            '', $request, $timeout, $trans_id, $pid);
    }

    public function sequence1_____($request_type, $name, $pattern,
                                   $request_info, $request,
                                   $timeout, $priority, $trans_id, $pid)
    {
        assert($pattern == $this->api->prefix() . '*');
        assert($request == 'test15');
        $this->api->return_($request_type, $name, $pattern,
                            '', $request, $timeout, $trans_id, $pid);
    }

    public function sequence1($request_type, $name, $pattern,
                              $request_info, $request,
                              $timeout, $priority, $trans_id, $pid)
    {
        // consume all the 'end' responses from all sequences handled
        // by this service
        list(, $old_request, ) = $this->api->recv_async(1000);
        while ($old_request == 'end')
            list(, $old_request, ) = $this->api->recv_async(1000);
        echo "messaging sequence1 start php\n";
        assert($request == 'start');
        $test1_id = $this->api->send_async(
            $this->api->prefix() . 'a/b/c/d',  'test1');
        $test2_id = $this->api->send_async(
            $this->api->prefix() . 'a/b/c/z',  'test2');
        $test3_id = $this->api->send_async(
            $this->api->prefix() . 'a/b/c/dd', 'test3');
        $test4_id = $this->api->send_async(
            $this->api->prefix() . 'a/b/z/d',  'test4');
        $test5_id = $this->api->send_async(
            $this->api->prefix() . 'a/b/cc/d', 'test5');
        $test6_id = $this->api->send_async(
            $this->api->prefix() . 'a/z/c/d',  'test6');
        $test7_id = $this->api->send_async(
            $this->api->prefix() . 'a/bb/c/d', 'test7');
        $test8_id = $this->api->send_async(
            $this->api->prefix() . 'z/b/c/d',  'test8');
        $test9_id = $this->api->send_async(
            $this->api->prefix() . 'aa/b/c/d', 'test9');
        $test10_id = $this->api->send_async(
            $this->api->prefix() . 'a/b/czd',  'test10');
        $test11_id = $this->api->send_async(
            $this->api->prefix() . 'a/bzc/d',  'test11');
        $test12_id = $this->api->send_async(
            $this->api->prefix() . 'azb/c/d',  'test12');
        $test13_id = $this->api->send_async(
            $this->api->prefix() . 'a/bzczd',  'test13');
        $test14_id = $this->api->send_async(
            $this->api->prefix() . 'azbzc/d',  'test14');
        $test15_id = $this->api->send_async(
            $this->api->prefix() . 'azbzczd',  'test15');
        // n.b., depends on cloudi_core_i_constants.hrl having
        // RECV_ASYNC_STRATEGY == recv_async_select_oldest
        $this->api->recv_async(null, $test1_id, false);
        list(, $test1_check, $test1_id_check) = $this->api->recv_async();
        assert($test1_check == 'test1');
        assert($test1_id_check == $test1_id);
        $this->api->recv_async(null, $test2_id, false);
        list(, $test2_check, $test2_id_check) = $this->api->recv_async();
        assert($test2_check == 'test2');
        assert($test2_id_check == $test2_id);
        $this->api->recv_async(null, $test3_id, false);
        list(, $test3_check, $test3_id_check) = $this->api->recv_async();
        assert($test3_check == 'test3');
        assert($test3_id_check == $test3_id);
        $this->api->recv_async(null, $test4_id, false);
        list(, $test4_check, $test4_id_check) = $this->api->recv_async();
        assert($test4_check == 'test4');
        assert($test4_id_check == $test4_id);
        $this->api->recv_async(null, $test5_id, false);
        list(, $test5_check, $test5_id_check) = $this->api->recv_async();
        assert($test5_check == 'test5');
        assert($test5_id_check == $test5_id);
        $this->api->recv_async(null, $test6_id, false);
        list(, $test6_check, $test6_id_check) = $this->api->recv_async();
        assert($test6_check == 'test6');
        assert($test6_id_check == $test6_id);
        $this->api->recv_async(null, $test7_id, false);
        list(, $test7_check, $test7_id_check) = $this->api->recv_async();
        assert($test7_check == 'test7');
        assert($test7_id_check == $test7_id);
        $this->api->recv_async(null, $test8_id, false);
        list(, $test8_check, $test8_id_check) = $this->api->recv_async();
        assert($test8_check == 'test8');
        assert($test8_id_check == $test8_id);
        $this->api->recv_async(null, $test9_id, false);
        list(, $test9_check, $test9_id_check) = $this->api->recv_async();
        assert($test9_check == 'test9');
        assert($test9_id_check == $test9_id);
        $this->api->recv_async(null, $test10_id, false);
        list(, $test10_check, $test10_id_check) = $this->api->recv_async();
        assert($test10_check == 'test10');
        assert($test10_id_check == $test10_id);
        $this->api->recv_async(null, $test11_id, false);
        list(, $test11_check, $test11_id_check) = $this->api->recv_async();
        assert($test11_check == 'test11');
        assert($test11_id_check == $test11_id);
        $this->api->recv_async(null, $test12_id, false);
        list(, $test12_check, $test12_id_check) = $this->api->recv_async();
        assert($test12_check == 'test12');
        assert($test12_id_check == $test12_id);
        $this->api->recv_async(null, $test13_id, false);
        list(, $test13_check, $test13_id_check) = $this->api->recv_async();
        assert($test13_check == 'test13');
        assert($test13_id_check == $test13_id);
        $this->api->recv_async(null, $test14_id, false);
        list(, $test14_check, $test14_id_check) = $this->api->recv_async();
        assert($test14_check == 'test14');
        assert($test14_id_check == $test14_id);
        $this->api->recv_async(null, $test15_id, false);
        list(, $test15_check, $test15_id_check) = $this->api->recv_async();
        assert($test15_check == 'test15');
        assert($test15_id_check == $test15_id);
        echo "messaging sequence1 end php\n";
        // start sequence2
        $this->api->send_async($this->api->prefix() . 'sequence2', 'start');
        $this->api->return_($request_type, $name, $pattern,
                            '', 'end', $timeout, $trans_id, $pid);
    }

    public function sequence2_e1($request_type, $name, $pattern,
                                 $request_info, $request,
                                 $timeout, $priority, $trans_id, $pid)
    {
        $this->api->return_($request_type, $name, $pattern,
                            '', '1', $timeout, $trans_id, $pid);
    }

    public function sequence2_e2($request_type, $name, $pattern,
                                 $request_info, $request,
                                 $timeout, $priority, $trans_id, $pid)
    {
        $this->api->return_($request_type, $name, $pattern,
                            '', '2', $timeout, $trans_id, $pid);
    }

    public function sequence2_e3($request_type, $name, $pattern,
                                 $request_info, $request,
                                 $timeout, $priority, $trans_id, $pid)
    {
        $this->api->return_($request_type, $name, $pattern,
                            '', '3', $timeout, $trans_id, $pid);
    }

    public function sequence2_e4($request_type, $name, $pattern,
                                 $request_info, $request,
                                 $timeout, $priority, $trans_id, $pid)
    {
        $this->api->return_($request_type, $name, $pattern,
                            '', '4', $timeout, $trans_id, $pid);
    }

    public function sequence2_e5($request_type, $name, $pattern,
                                 $request_info, $request,
                                 $timeout, $priority, $trans_id, $pid)
    {
        $this->api->return_($request_type, $name, $pattern,
                            '', '5', $timeout, $trans_id, $pid);
    }

    public function sequence2_e6($request_type, $name, $pattern,
                                 $request_info, $request,
                                 $timeout, $priority, $trans_id, $pid)
    {
        $this->api->return_($request_type, $name, $pattern,
                            '', '6', $timeout, $trans_id, $pid);
    }

    public function sequence2_e7($request_type, $name, $pattern,
                                 $request_info, $request,
                                 $timeout, $priority, $trans_id, $pid)
    {
        $this->api->return_($request_type, $name, $pattern,
                            '', '7', $timeout, $trans_id, $pid);
    }

    public function sequence2_e8($request_type, $name, $pattern,
                                 $request_info, $request,
                                 $timeout, $priority, $trans_id, $pid)
    {
        $this->api->return_($request_type, $name, $pattern,
                            '', '8', $timeout, $trans_id, $pid);
    }

    public function sequence2($request_type, $name, $pattern,
                              $request_info, $request,
                              $timeout, $priority, $trans_id, $pid)
    {
        echo "messaging sequence2 start php\n";
        assert($request == 'start');
        while (true)
        {
            // the sending process is excluded from the services that receive
            // the asynchronous message, so in this case, the receiving thread
            // will not be called, despite the fact it has subscribed to 'e',
            // to prevent a process (in this case thread) from deadlocking
            // with itself.
            $e_ids = $this->api->mcast_async($this->api->prefix() . 'e',  ' ');
            // 4 * 8 == 32, but only 3 out of 4 threads can receive messages,
            // since 1 thread is sending the mcast_async, so 3 * 8 == 24
            if (count($e_ids) == 24)
            {
                $e_check_list = array();
                foreach ($e_ids as $e_id)
                {
                    list(, $e_check,
                         $e_id_check) = $this->api->recv_async(null, $e_id);
                    assert($e_id == $e_id_check);
                    $e_check_list[] = $e_check;
                }
                sort($e_check_list);
                $e_check = implode($e_check_list);
                assert($e_check == '111222333444555666777888');
                break;
            }
            else
            {
                $waiting = strval(4 - count($e_ids) / 8.0);
                echo "Waiting for {$waiting} services to initialize\n";
                foreach ($e_ids as $e_id)
                {
                    list(, $e_check,
                         $e_id_check) = $this->api->recv_async(null, $e_id);
                    assert($e_id == $e_id_check);
                }
                list(, , $null_id) = $this->api->recv_async(1000);
                assert($null_id == str_repeat("\0", 16));
            }
        }
        echo "messaging sequence2 end php\n";
        # start sequence3
        $this->api->send_async($this->api->prefix() . 'sequence3', 'start');
        $this->api->return_($request_type, $name, $pattern,
                            '', 'end', $timeout, $trans_id, $pid);
    }

    public function sequence3_f1($request_type, $name, $pattern,
                                 $request_info, $request,
                                 $timeout, $priority, $trans_id, $pid)
    {
        $request_i = intval($request);
        if ($request_i == 4)
            return 'done';
        $request_new = $request_i + 2; // two steps forward
        $this->api->forward_($request_type,
                             $this->api->prefix() . 'f2', $request_info,
                             strval($request_new),
                             $timeout, $priority, $trans_id, $pid);
    }

    public function sequence3_f2($request_type, $name, $pattern,
                                 $request_info, $request,
                                 $timeout, $priority, $trans_id, $pid)
    {
        $request_i = intval($request);
        $request_new = $request_i - 1; // one step back
        $this->api->forward_($request_type,
                             $this->api->prefix() . 'f1', $request_info,
                             strval($request_new),
                             $timeout, $priority, $trans_id, $pid);
    }

    public function sequence3_g1($request_type, $name, $pattern,
                                 $request_info, $request,
                                 $timeout, $priority, $trans_id, $pid)
    {
        $this->api->return_($request_type, $name, $pattern,
                            '', $request . 'suffix', $timeout, $trans_id, $pid);
    }

    public function sequence3($request_type, $name, $pattern,
                              $request_info, $request,
                              $timeout, $priority, $trans_id, $pid)
    {
        echo "messaging sequence3 start php\n";
        assert($request == 'start');
        $test1_id = $this->api->send_async(
            $this->api->prefix() . 'f1', '0');
        list(, $test1_check,
             $test1_id_check) = $this->api->recv_async(null, $test1_id);
        assert($test1_id_check == $test1_id);
        assert($test1_check == 'done');
        list(, $test2_check,
             $test2_id_check
             ) = $this->api->send_sync($this->api->prefix() . 'g1', 'prefix_');
        assert($test2_check == 'prefix_suffix');
        echo "messaging sequence3 end php\n";
        $this->api->send_async($this->api->prefix() . 'sequence1', 'start');
        $this->api->return_($request_type, $name, $pattern,
                            '', 'end', $timeout, $trans_id, $pid);
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
    $threads[] = new Task(new \CloudI\API($i));
}
foreach ($threads as $t)
    $t->start();
foreach ($threads as $t)
    $t->join();
*/

?>
