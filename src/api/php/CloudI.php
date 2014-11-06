<?php //-*-coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=php fenc=utf-8 sts=4 ts=4 sw=4 et:
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

namespace CloudI;

// unbuffered stdout
for ($i = 0; $i < ob_get_level(); $i++) { ob_end_flush(); }
ob_implicit_flush(true);
// an assert failure needs to cause php to exit
assert_options(ASSERT_BAIL);

require dirname(__FILE__) . '/Erlang.php';

define(__NAMESPACE__ . '\MESSAGE_INIT',               1);
define(__NAMESPACE__ . '\MESSAGE_SEND_ASYNC',         2);
define(__NAMESPACE__ . '\MESSAGE_SEND_SYNC',          3);
define(__NAMESPACE__ . '\MESSAGE_RECV_ASYNC',         4);
define(__NAMESPACE__ . '\MESSAGE_RETURN_ASYNC',       5);
define(__NAMESPACE__ . '\MESSAGE_RETURN_SYNC',        6);
define(__NAMESPACE__ . '\MESSAGE_RETURNS_ASYNC',      7);
define(__NAMESPACE__ . '\MESSAGE_KEEPALIVE',          8);
define(__NAMESPACE__ . '\MESSAGE_REINIT',             9);
define(__NAMESPACE__ . '\MESSAGE_SUBSCRIBE_COUNT',   10);
define(__NAMESPACE__ . '\MESSAGE_TERM',              11);

class API
{
    private static $ASYNC =  1;
    private static $SYNC  = -1;

    private $use_header;
    private $s;
    private $initialization_complete;
    private $terminate;
    private $size;
    private $callbacks;
    private $process_index;
    private $process_count;
    private $process_count_max;
    private $process_count_min;
    private $prefix;
    private $timeout_initialize;
    private $timeout_async;
    private $timeout_sync;
    private $timeout_terminate;
    private $priority_default;
    private $request_timeout_adjustment;
    private $request_timeout;
    private $request_timer;

    public function __construct($thread_index)
    {
        $protocol_str = getenv('CLOUDI_API_INIT_PROTOCOL');
        if ($protocol_str == false)
            throw new InvalidInputException();
        $buffer_size_str = getenv('CLOUDI_API_INIT_BUFFER_SIZE');
        if ($buffer_size_str == false)
            throw new InvalidInputException();
        $this->s = fopen('php://fd/' . strval($thread_index + 3), 'rwb');
        if ($protocol_str == 'tcp')
            $this->use_header = true;
        elseif ($protocol_str == 'udp')
            $this->use_header = false;
        elseif ($protocol_str == 'local')
            $this->use_header = true;
        else
            throw new InvalidInputException();
        $this->initialization_complete = false;
        $this->terminate = false;
        $this->size = intval($buffer_size_str);
        $this->callbacks = array();
        $this->timeout_terminate = 1000; // TIMEOUT_TERMINATE_MIN
        $this->send(\Erlang\term_to_binary(new \Erlang\OtpErlangAtom('init')));
        list($this->process_index,
             $this->process_count,
             $this->process_count_max,
             $this->process_count_min,
             $this->prefix,
             $this->timeout_initialize,
             $this->timeout_async,
             $this->timeout_sync,
             $this->timeout_terminate,
             $this->priority_default,
             $this->request_timeout_adjustment
             ) = $this->poll_request(null, false);
    }

    public static function thread_count()
    {
        $count_str = getenv('CLOUDI_API_INIT_THREAD_COUNT');
        if ($count_str == false)
            throw new InvalidInputException();
        return intval($count_str);
    }

    public function subscribe($pattern, $object, $method)
    {
        if (! (is_object($object) && is_string($method)))
            throw new InvalidInputException();
        $callable = array($object, $method);
        $key = $this->prefix . $pattern;
        if (array_key_exists($key, $this->callbacks))
            $this->callbacks[$key][] = $callable;
        else
            $this->callbacks[$key] = array($callable);
        $this->send(\Erlang\term_to_binary(
            array(new \Erlang\OtpErlangAtom('subscribe'), $pattern)));
    }

    public function subscribe_count($pattern)
    {
        $this->send(\Erlang\term_to_binary(
            array(new \Erlang\OtpErlangAtom('subscribe_count'), $pattern)));
        return $this->poll_request(null, false);
    }

    public function unsubscribe($pattern)
    {
        $key = $this->prefix . $pattern;
        assert(array_key_exists($key, $this->callbacks));
        $value =& $this->callbacks[$key];
        array_shift($value);
        if (count($value) == 0)
            unset($this->callbacks[$key]);
        $this->send(\Erlang\term_to_binary(
            array(new \Erlang\OtpErlangAtom('unsubscribe'), $pattern)));
    }

    public function send_async($name, $request,
                               $timeout = null, $request_info = null,
                               $priority = null)
    {
        if (is_null($timeout))
            $timeout = $this->timeout_async;
        if (is_null($request_info))
            $request_info = '';
        if (is_null($priority))
            $priority = $this->priority_default;
        $this->send(\Erlang\term_to_binary(
            array(new \Erlang\OtpErlangAtom('send_async'), $name,
                  new \Erlang\OtpErlangBinary($request_info),
                  new \Erlang\OtpErlangBinary($request),
                  $timeout, $priority)));
        return $this->poll_request(null, false);
    }

    public function send_sync($name, $request,
                              $timeout = null, $request_info = null,
                              $priority = null)
    {
        if (is_null($timeout))
            $timeout = $this->timeout_sync;
        if (is_null($request_info))
            $request_info = '';
        if (is_null($priority))
            $priority = $this->priority_default;
        $this->send(\Erlang\term_to_binary(
            array(new \Erlang\OtpErlangAtom('send_sync'), $name,
                  new \Erlang\OtpErlangBinary($request_info),
                  new \Erlang\OtpErlangBinary($request),
                  $timeout, $priority)));
        return $this->poll_request(null, false);
    }

    public function mcast_async($name, $request,
                                $timeout = null, $request_info = null,
                                $priority = null)
    {
        if (is_null($timeout))
            $timeout = $this->timeout_async;
        if (is_null($request_info))
            $request_info = '';
        if (is_null($priority))
            $priority = $this->priority_default;
        $this->send(\Erlang\term_to_binary(
            array(new \Erlang\OtpErlangAtom('mcast_async'), $name,
                  new \Erlang\OtpErlangBinary($request_info),
                  new \Erlang\OtpErlangBinary($request),
                  $timeout, $priority)));
        return $this->poll_request(null, false);
    }

    public function forward_($command, $name, $request_info, $request,
                             $timeout, $priority, $trans_id, $pid)
    {
        switch ($command)
        {
            case API::$ASYNC:
                $this->forward_async($name, $request_info, $request,
                                     $timeout, $priority, $trans_id, $pid);
            case API::$SYNC:
                $this->forward_sync($name, $request_info, $request,
                                    $timeout, $priority, $trans_id, $pid);
        }
    }

    public function forward_async($name, $request_info, $request,
                                  $timeout, $priority, $trans_id, $pid)
    {
        if ($this->request_timeout_adjustment)
        {
            if ($timeout == $this->request_timeout)
            {
                $elapsed = max(0, (integer) floor((microtime(true) -
                                                   $this->request_timer) *
                                                  1000.0));
                if ($elapsed > $timeout)
                    $timeout = 0;
                else
                    $timeout -= $elapsed;
            }
        }
        $this->send(\Erlang\term_to_binary(
            array(new \Erlang\OtpErlangAtom('forward_async'), $name,
                  new \Erlang\OtpErlangBinary($request_info),
                  new \Erlang\OtpErlangBinary($request), $timeout, $priority,
                  new \Erlang\OtpErlangBinary($trans_id), $pid)));
        throw new ForwardAsyncException();
    }

    public function forward_sync($name, $request_info, $request,
                                 $timeout, $priority, $trans_id, $pid)
    {
        if ($this->request_timeout_adjustment)
        {
            if ($timeout == $this->request_timeout)
            {
                $elapsed = max(0, (integer) floor((microtime(true) -
                                                   $this->request_timer) *
                                                  1000.0));
                if ($elapsed > $timeout)
                    $timeout = 0;
                else
                    $timeout -= $elapsed;
            }
        }
        $this->send(\Erlang\term_to_binary(
            array(new \Erlang\OtpErlangAtom('forward_sync'), $name,
                  new \Erlang\OtpErlangBinary($request_info),
                  new \Erlang\OtpErlangBinary($request), $timeout, $priority,
                  new \Erlang\OtpErlangBinary($trans_id), $pid)));
        throw new ForwardSyncException();
    }

    public function return_($command, $name, $pattern,
                            $response_info, $response,
                            $timeout, $trans_id, $pid)
    {
        switch ($command)
        {
            case API::$ASYNC:
                $this->return_async($name, $pattern, $response_info, $response,
                                    $timeout, $trans_id, $pid);
            case API::$SYNC:
                $this->return_sync($name, $pattern, $response_info, $response,
                                   $timeout, $trans_id, $pid);
        }
    }

    public function return_async($name, $pattern, $response_info, $response,
                                 $timeout, $trans_id, $pid)
    {
        if ($this->request_timeout_adjustment)
        {
            if ($timeout == $this->request_timeout)
            {
                $elapsed = max(0, (integer) floor((microtime(true) -
                                                   $this->request_timer) *
                                                  1000.0));
                if ($elapsed > $timeout)
                {
                    $response_info = '';
                    $response = '';
                    $timeout = 0;
                }
                else
                {
                    $timeout -= $elapsed;
                }
            }
        }
        $this->send(\Erlang\term_to_binary(
            array(new \Erlang\OtpErlangAtom('return_async'), $name, $pattern,
                  new \Erlang\OtpErlangBinary($response_info),
                  new \Erlang\OtpErlangBinary($response), $timeout,
                  new \Erlang\OtpErlangBinary($trans_id), $pid)));
        throw new ReturnAsyncException();
    }

    public function return_sync($name, $pattern, $response_info, $response,
                                $timeout, $trans_id, $pid)
    {
        if ($this->request_timeout_adjustment)
        {
            if ($timeout == $this->request_timeout)
            {
                $elapsed = max(0, (integer) floor((microtime(true) -
                                                   $this->request_timer) *
                                                  1000.0));
                if ($elapsed > $timeout)
                {
                    $response_info = '';
                    $response = '';
                    $timeout = 0;
                }
                else
                {
                    $timeout -= $elapsed;
                }
            }
        }
        $this->send(\Erlang\term_to_binary(
            array(new \Erlang\OtpErlangAtom('return_sync'), $name, $pattern,
                  new \Erlang\OtpErlangBinary($response_info),
                  new \Erlang\OtpErlangBinary($response), $timeout,
                  new \Erlang\OtpErlangBinary($trans_id), $pid)));
        throw new ReturnSyncException();
    }

    public function recv_async($timeout = null, $trans_id = null,
                               $consume = true)
    {
        if (is_null($timeout))
            $timeout = $this->timeout_sync;
        if (is_null($trans_id))
            $trans_id = str_repeat("\0", 16);
        $this->send(\Erlang\term_to_binary(
            array(new \Erlang\OtpErlangAtom('recv_async'), $timeout,
                  new \Erlang\OtpErlangBinary($trans_id), $consume)));
        return $this->poll_request(null, false);
    }

    public function process_index()
    {
        return $this->process_index;
    }

    public function process_count()
    {
        return $this->process_count;
    }

    public function process_count_max()
    {
        return $this->process_count_max;
    }

    public function process_count_min()
    {
        return $this->process_count_min;
    }

    public function prefix()
    {
        return $this->prefix;
    }

    public function timeout_initialize()
    {
        return $this->timeout_initialize;
    }

    public function timeout_async()
    {
        return $this->timeout_async;
    }

    public function timeout_sync()
    {
        return $this->timeout_sync;
    }

    public function timeout_terminate()
    {
        return $this->timeout_terminate;
    }

    private function callback($command, $name, $pattern,
                              $request_info, $request,
                              $timeout, $priority, $trans_id, $pid)
    {
        if ($this->request_timeout_adjustment)
        {
            $this->request_timer = microtime(true);
            $this->request_timeout = $timeout;
        }
        assert(isset($this->callbacks[$pattern]));
        $function_queue =& $this->callbacks[$pattern];
        $function = array_shift($function_queue);
        $function_queue[] = $function;
        switch ($command)
        {
            case MESSAGE_SEND_ASYNC:
                try
                {
                    $response = call_user_func($function,
                                               API::$ASYNC, $name, $pattern,
                                               $request_info, $request,
                                               $timeout, $priority,
                                               $trans_id, $pid);
                    if (is_array($response))
                    {
                        assert(count($response) == 2);
                        $response_info = $response[0];
                        $response = $response[1];
                        if (! is_string($response_info))
                            $response_info = '';
                    }
                    else
                    {
                        $response_info = '';
                    }
                    if (! is_string($response))
                        $response = '';
                }
                catch (InvalidInputException $e)
                {
                    throw $e;
                }
                catch (MessageDecodingException $e)
                {
                    throw $e;
                }
                catch (TerminateException $e)
                {
                    throw $e;
                }
                catch (ReturnAsyncException $e)
                {
                    return;
                }
                catch (ForwardAsyncException $e)
                {
                    return;
                }
                catch (ReturnSyncException $e)
                {
                    echo "{$e->getMessage()}\n{$e}\n";
                    assert(false);
                    return;
                }
                catch (ForwardSyncException $e)
                {
                    echo "{$e->getMessage()}\n{$e}\n";
                    assert(false);
                    return;
                }
                catch (\Exception $e)
                {
                    echo "{$e->getMessage()}\n{$e}\n";
                    $response_info = '';
                    $response = '';
                }
                try
                {
                    $this->return_async($name, $pattern,
                                        $response_info, $response,
                                        $timeout, $trans_id, $pid);
                }
                catch (ReturnAsyncException $e)
                {
                }
                return;
            case MESSAGE_SEND_SYNC:
                try
                {
                    $response = call_user_func($function,
                                               API::$SYNC, $name, $pattern,
                                               $request_info, $request,
                                               $timeout, $priority,
                                               $trans_id, $pid);
                    if (is_array($response))
                    {
                        assert(count($response) == 2);
                        $response_info = $response[0];
                        $response = $response[1];
                        if (! is_string($response_info))
                            $response_info = '';
                    }
                    else
                    {
                        $response_info = '';
                    }
                    if (! is_string($response))
                        $response = '';
                }
                catch (InvalidInputException $e)
                {
                    throw $e;
                }
                catch (MessageDecodingException $e)
                {
                    throw $e;
                }
                catch (TerminateException $e)
                {
                    throw $e;
                }
                catch (ReturnSyncException $e)
                {
                    return;
                }
                catch (ForwardSyncException $e)
                {
                    return;
                }
                catch (ReturnAsyncException $e)
                {
                    echo "{$e->getMessage()}\n{$e}\n";
                    assert(false);
                    return;
                }
                catch (ForwardAsyncException $e)
                {
                    echo "{$e->getMessage()}\n{$e}\n";
                    assert(false);
                    return;
                }
                catch (\Exception $e)
                {
                    echo "{$e->getMessage()}\n{$e}\n";
                    $response_info = '';
                    $response = '';
                }
                try
                {
                    $this->return_sync($name, $pattern,
                                       $response_info, $response,
                                       $timeout, $trans_id, $pid);
                }
                catch (ReturnSyncException $e)
                {
                }
                return;
            default:
                throw new MessageDecodingException();
        }
    }

    private function handle_events($external, $data, $data_size, $i,
                                   $command = null)
    {
        if (is_null($command))
        {
            if ($i > $data_size)
                throw new MessageDecodingException();
            $j = 4;
            list(, $command) = unpack('L', substr($data, $i, $j));
        }
        else
        {
            $j = 4;
        }
        while (true)
        {
            switch ($command)
            {
                case MESSAGE_TERM:
                    $this->terminate = true;
                    if ($external)
                        return false;
                    else
                        throw new TerminateException($this->timeout_terminate);
                case MESSAGE_REINIT:
                    $i += $j; $j = 4;
                    list(, $this->process_count) = unpack('L', substr($data,
                                                                      $i, $j));
                    $i += $j;
                    break;
                case MESSAGE_KEEPALIVE:
                    $this->send(\Erlang\term_to_binary(
                        new \Erlang\OtpErlangAtom('keepalive')));
                    $i += $j;
                    break;
                default:
                    throw new MessageDecodingException();
            }
            if ($i > $data_size)
                throw new MessageDecodingException();
            elseif ($i == $data_size)
                return true;
            $j = 4;
            list(, $command) = unpack('L', substr($data, $i, $j));
        }
    }

    private function poll_request($timeout, $external)
    {
        if ($this->terminate)
        {
            return false;
        }
        elseif ($external && ! $this->initialization_complete)
        {
            $this->send(\Erlang\term_to_binary(
                new \Erlang\OtpErlangAtom('polling')));
            $this->initialization_complete = true;
        }

        $poll_timer = null;
        if (is_null($timeout) || $timeout < 0)
        {
            $timeout_value_secs = null;
            $timeout_value_usecs = null;
        }
        elseif ($timeout == 0)
        {
            $timeout_value_secs = 0;
            $timeout_value_usecs = 0;
        }
        elseif ($timeout > 0)
        {
            $poll_timer = microtime(true);
            $timeout_value_secs = intval($timeout / 1000);
            $timeout_value_usecs = intval($timeout -
                                          ($timeout_value_secs *
                                           1000)) * 1000;
        }
        $result_read = array($this->s);
        $result_write = null;
        $result_except = array($this->s);
        $result = stream_select($result_read, $result_write, $result_except,
                                $timeout_value_secs, $timeout_value_usecs);
        if ($result === false || count($result_except) > 0)
            return false;
        if (count($result_read) == 0)
            return true;

        $data = $this->recv('');
        $data_size = strlen($data);
        if ($data_size == 0)
            return false;
        $i = 0; $j = 4;

        while (true)
        {
            list(, $command) = unpack('L', substr($data, $i, $j));
            switch ($command)
            {
                case MESSAGE_INIT:
                    $i += $j; $j = 4 + 4 + 4 + 4 + 4;
                    list(, $process_index,
                         $process_count,
                         $process_count_max,
                         $process_count_min,
                         $prefix_size) = unpack('L5', substr($data, $i, $j));
                    $i += $j; $j = $prefix_size;
                    $prefix = substr($data, $i, $j - 1);
                    $i += $j; $j = 4 + 4 + 4 + 4 + 1 + 1;
                    $tmp = unpack('L4a/cb/Cc', substr($data, $i, $j));
                    $timeout_initialize = $tmp['a1'];
                    $timeout_async = $tmp['a2'];
                    $timeout_sync = $tmp['a3'];
                    $timeout_terminate = $tmp['a4'];
                    $priority_default = $tmp['b'];
                    $request_timeout_adjustment = $tmp['c'];
                    $i += $j;
                    if ($i != $data_size)
                    {
                        assert($external == false);
                        $this->handle_events($external, $data, $data_size, $i);
                    }
                    return array($process_index, $process_count,
                                 $process_count_max, $process_count_min,
                                 $prefix, $timeout_initialize,
                                 $timeout_sync, $timeout_async,
                                 $timeout_terminate, $priority_default,
                                 ($request_timeout_adjustment == 1));
                case MESSAGE_SEND_ASYNC:
                case MESSAGE_SEND_SYNC:
                    $i += $j; $j = 4;
                    list(, $name_size) = unpack('L', substr($data, $i, $j));
                    $i += $j; $j = $name_size;
                    $name = substr($data, $i, $j - 1);
                    $i += $j; $j = 4;
                    list(, $pattern_size) = unpack('L', substr($data, $i, $j));
                    $i += $j; $j = $pattern_size;
                    $pattern = substr($data, $i, $j - 1);
                    $i += $j; $j = 4;
                    list(, $request_info_size) = unpack('L',
                                                        substr($data, $i, $j));
                    $i += $j; $j = $request_info_size;
                    $request_info = substr($data, $i, $j);
                    $i += $j; $j = 4;
                    $i++; // skip null byte
                    list(, $request_size) = unpack('L', substr($data, $i, $j));
                    $i += $j; $j = $request_size;
                    $request = substr($data, $i, $j);
                    $i += $j; $j = 4 + 1;
                    $i++; // skip null byte
                    $tmp = unpack('La/cb', substr($data, $i, $j));
                    $request_timeout = $tmp['a'];
                    $priority = $tmp['b'];
                    $i += $j; $j = 16;
                    $trans_id = substr($data, $i, $j);
                    $i += $j; $j = 4;
                    list(, $pid_size) = unpack('L', substr($data, $i, $j));
                    $i += $j; $j = $pid_size;
                    $pid = substr($data, $i, $j);
                    $i += $j;
                    if ($i != $data_size)
                    {
                        assert($external == true);
                        if (! $this->handle_events($external,
                                                   $data, $data_size, $i))
                        {
                            return false;
                        }
                    }
                    $data = '';
                    $this->callback($command, $name, $pattern,
                                    $request_info, $request,
                                    $request_timeout, $priority, $trans_id,
                                    \Erlang\binary_to_term($pid));
                    break;
                case MESSAGE_RECV_ASYNC:
                case MESSAGE_RETURN_SYNC:
                    $i += $j; $j = 4;
                    list(, $response_info_size) = unpack('L',
                                                         substr($data, $i, $j));
                    $i += $j; $j = $response_info_size;
                    $response_info = substr($data, $i, $j);
                    $i += $j; $j = 4;
                    $i++; // skip null byte
                    list(, $response_size) = unpack('L', substr($data, $i, $j));
                    $i += $j; $j = $response_size;
                    $response = substr($data, $i, $j);
                    $i += $j; $j = 16;
                    $i++; // skip null byte
                    $trans_id = substr($data, $i, $j);
                    $i += $j;
                    if ($i != $data_size)
                    {
                        assert($external == false);
                        $this->handle_events($external, $data, $data_size, $i);
                    }
                    return array($response_info, $response, $trans_id);
                case MESSAGE_RETURN_ASYNC:
                    $i += $j; $j = 16;
                    $trans_id = substr($data, $i, $j);
                    $i += $j;
                    if ($i != $data_size)
                    {
                        assert($external == false);
                        $this->handle_events($external, $data, $data_size, $i);
                    }
                    return $trans_id;
                case MESSAGE_RETURNS_ASYNC:
                    $i += $j; $j = 4;
                    list(, $trans_id_count) = unpack('L',
                                                     substr($data, $i, $j));
                    $i += $j; $j = 16;
                    $trans_ids = array();
                    for ($k = 0; $k < $trans_id_count; $k++)
                    {
                        $trans_ids[] = substr($data, $i, $j);
                        $i += $j;
                    }
                    if ($i != $data_size)
                    {
                        assert($external == false);
                        $this->handle_events($external, $data, $data_size, $i);
                    }
                    return $trans_ids;
                case MESSAGE_SUBSCRIBE_COUNT:
                    $i += $j; $j = 4;
                    list(, $count) = unpack('L', substr($data, $i, $j));
                    $i += $j;
                    if ($i != $data_size)
                    {
                        assert($external == false);
                        $this->handle_events($external, $data, $data_size, $i);
                    }
                    return $count;
                case MESSAGE_TERM:
                    if (! $this->handle_events($external,
                                               $data, $data_size, $i, $command))
                    {
                        return false;
                    }
                    assert(false);
                case MESSAGE_REINIT:
                    $i += $j; $j = 4;
                    list(, $this->process_count
                         ) = unpack('L', substr($data, $i, $j));
                    $i += $j; $j = 4;
                    if ($i == $data_size)
                        break;
                    elseif ($i < $data_size)
                        continue 2;
                    else
                        throw new MessageDecodingException();
                case MESSAGE_KEEPALIVE:
                    $this->send(\Erlang\term_to_binary(
                        new \Erlang\OtpErlangAtom('keepalive')));
                    $i += $j; $j = 4;
                    if ($i == $data_size)
                        break;
                    elseif ($i < $data_size)
                        continue 2;
                    else
                        throw new MessageDecodingException();
                default:
                    throw new MessageDecodingException();
            }

            if (! is_null($poll_timer))
            {
                $poll_timer_new = microtime(true);
                $elapsed = max(0, (integer) floor(($poll_timer_new -
                                                   $poll_timer) * 1000.0));
                $poll_timer = $poll_timer_new;
                if ($elapsed >= $timeout)
                    $timeout = 0;
                else
                    $timeout -= $elapsed;
            }
            if (! is_null($timeout_value_secs))
            {
                if ($timeout == 0)
                {
                    return true;
                }
                elseif ($timeout > 0)
                {
                    $timeout_value_secs = intval($timeout / 1000);
                    $timeout_value_usecs = intval($timeout -
                                                  ($timeout_value_secs *
                                                   1000)) * 1000;
                }
            }
            $result_read = array($this->s);
            $result_write = null;
            $result_except = array($this->s);
            $result = stream_select($result_read, $result_write, $result_except,
                                    $timeout_value_secs, $timeout_value_usecs);
            if ($result === false || count($result_except) > 0)
                return false;
            if (count($result_read) == 0)
                return true;
    
            $data = $this->recv($data);
            $data_size = strlen($data);
            if ($data_size == 0)
                return false;
            $i = 0; $j = 4;
        }
    }

    public function poll($timeout = -1)
    {
        return $this->poll_request($timeout, true);
    }

    private function text_key_value_parse($text)
    {
        $result = array();
        $data = explode("\0", $text);
        $size = count($data);
        if ($size >= 2)
        {
            foreach (range(0, ($size - $size % 2) - 2, 2) as $i)
            {
                $key = $data[$i];
                if (isset($result[$key]))
                {
                    $value = $result[$key];
                    if (is_array($value))
                        $value[] = $data[$i + 1];
                    else
                        $result[$key] = array($value, $data[$i + 1]);
                }
                else
                {
                    $result[$key] = $data[$i + 1];
                }
            }
        }
        return $result;
    }

    public function request_http_qs_parse($request)
    {
        return $this->text_key_value_parse($request);
    }

    public function info_key_value_parse($message_info)
    {
        return $this->text_key_value_parse($message_info);
    }

    private function send($data)
    {
        if ($this->use_header)
            $data = pack('N', strlen($data)) . $data;
        fwrite($this->s, $data);
    }

    private function recv($data)
    {
        if ($this->use_header)
        {
            while (strlen($data) < 4)
            {
                $fragment = fread($this->s, $this->size);
                $data .= $fragment;
            }
            list(, $total) = unpack('N', substr($data, 0, 4));
            $data = substr($data, 4);
            while (strlen($data) < $total)
            {
                $fragment = fread($this->s, $this->size);
                $data .= $fragment;
            }
        }
        else
        {
            $ready = true;
            while ($ready == true)
            {
                $fragment = fread($this->s, $this->size);
                $data .= $fragment;
                $ready = (strlen($fragment) == $this->size);

                if ($ready)
                {
                    $result_read = array($this->s);
                    $result_write = null;
                    $result_except = null;
                    $ready = (stream_select($result_read, $result_write,
                                            $result_except, 0) == 1);
                }
            }
        }
        return $data;
    }
}

class InvalidInputException extends \Exception
{
    public function __construct()
    {
        parent::__construct('Invalid Input', 0, null);
    }
}

class ReturnSyncException extends \Exception
{
    public function __construct()
    {
        parent::__construct('Synchronous Call Return Invalid', 0, null);
    }
}

class ReturnAsyncException extends \Exception
{
    public function __construct()
    {
        parent::__construct('Asynchronous Call Return Invalid', 0, null);
    }
}

class ForwardSyncException extends \Exception
{
    public function __construct()
    {
        parent::__construct('Synchronous Call Forward Invalid', 0, null);
    }
}

class ForwardAsyncException extends \Exception
{
    public function __construct()
    {
        parent::__construct('Asynchronous Call Forward Invalid', 0, null);
    }
}

class MessageDecodingException extends \Exception
{
    public function __construct()
    {
        parent::__construct('Message Decoding Error', 0, null);
    }
}

class TerminateException extends \Exception
{
    private $timeout;
    public function __construct($timeout)
    {
        parent::__construct('Terminate', 0, null);
        $this->timeout = $timeout;
    }

    public function timeout()
    {
        return $this->timeout;
    }
}

?>
