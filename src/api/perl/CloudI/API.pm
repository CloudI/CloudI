#-*-Mode:perl;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=perl fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
#
# MIT License
#
# Copyright (c) 2014-2025 Michael Truog <mjtruog at protonmail dot com>
#
# Permission is hereby granted, free of charge, to any person obtaining a
# copy of this software and associated documentation files (the "Software"),
# to deal in the Software without restriction, including without limitation
# the rights to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
# DEALINGS IN THE SOFTWARE.
#

package CloudI::API;
use strict;
use warnings;
use 5.010;

$CloudI::API::VERSION = '2.07';

use POSIX qw(getenv);
use IO::Handle;
use Time::HiRes qw(gettimeofday);
use Scalar::Util qw(blessed);

use constant MESSAGE_INIT              =>  1;
use constant MESSAGE_SEND_ASYNC        =>  2;
use constant MESSAGE_SEND_SYNC         =>  3;
use constant MESSAGE_RECV_ASYNC        =>  4;
use constant MESSAGE_RETURN_ASYNC      =>  5;
use constant MESSAGE_RETURN_SYNC       =>  6;
use constant MESSAGE_RETURNS_ASYNC     =>  7;
use constant MESSAGE_KEEPALIVE         =>  8;
use constant MESSAGE_REINIT            =>  9;
use constant MESSAGE_SUBSCRIBE_COUNT   => 10;
use constant MESSAGE_TERM              => 11;

require Erlang;
require Erlang::OtpErlangAtom;
require Erlang::OtpErlangBinary;
require Erlang::OtpErlangPid;
require Erlang::Exception;
require CloudI::InvalidInputException;
require CloudI::ReturnSyncException;
require CloudI::ReturnAsyncException;
require CloudI::ForwardSyncException;
require CloudI::ForwardAsyncException;
require CloudI::MessageDecodingException;
require CloudI::TerminateException;
require CloudI::AssertionError;

use constant ASYNC =>  1;
use constant SYNC  => -1;

select STDERR; $| = 1; # make unbuffered
select STDOUT; $| = 1; # make unbuffered

sub new
{
    my $class = shift;
    my ($thread_index) = @_;
    if (! defined($thread_index) or $thread_index !~ /^\d+$/)
    {
        die CloudI::InvalidInputException->new();
    }
    my $protocol = getenv('CLOUDI_API_INIT_PROTOCOL');
    if (! defined($protocol))
    {
        print STDERR "CloudI service execution must occur in CloudI";
        die CloudI::InvalidInputException->new();
    }
    my $buffer_size = _getenv_to_uint('CLOUDI_API_INIT_BUFFER_SIZE');
    my $s = IO::Handle->new_from_fd($thread_index + 1024, 'r+');
    $s->autoflush(1);
    my $use_header;
    if ($protocol eq 'tcp')
    {
        $use_header = 1;
    }
    elsif ($protocol eq 'udp')
    {
        $use_header = 0;
    }
    elsif ($protocol eq 'local')
    {
        $use_header = 1;
    }
    else
    {
        die CloudI::InvalidInputException->new();
    }
    my %callbacks = ();
    my $self = bless {
        _use_header => $use_header,
        _s => $s,
        _initialization_complete => 0,
        _fatal_exceptions => 0,
        _terminate => 0,
        _size => $buffer_size,
        _callbacks => \%callbacks,
        _process_index => undef,
        _process_count => undef,
        _process_count_max => undef,
        _process_count_min => undef,
        _prefix => undef,
        _timeout_initialize => undef,
        _timeout_async => undef,
        _timeout_sync => undef,
        _timeout_terminate => 10, # TIMEOUT_TERMINATE_MIN
        _priority_default => undef,
    }, $class;
    $self->_send(Erlang::term_to_binary(Erlang::OtpErlangAtom->new('init')));
    ($self->{_process_index},
     $self->{_process_count},
     $self->{_process_count_max},
     $self->{_process_count_min},
     $self->{_prefix},
     $self->{_timeout_initialize},
     $self->{_timeout_async},
     $self->{_timeout_sync},
     $self->{_timeout_terminate},
     $self->{_priority_default},
     $self->{_fatal_exceptions}) = $self->_poll_request(undef, 0);
    return $self;
}

sub DESTROY
{
    my $self = shift;
    if (defined($self->{_s}))
    {
        $self->{_s}->close();
    }
}

# callable without an object
sub thread_count
{
    return _getenv_to_uint('CLOUDI_API_INIT_THREAD_COUNT');
}

sub subscribe
{
    my $self = shift;
    # arity 2 or 3 (so using an object is not required)
    my ($pattern, $object, $method) = @_;
    my $function;
    if (! defined($method) and ref($object) eq 'CODE')
    {
        $function = $object;
    }
    elsif (defined(blessed($object)) and
           defined($method) and ref($method) eq '' and
           defined($object->can($method)))
    {
        # object method
        $function = sub { return $object->$method(@_); };
    }
    else
    {
        die CloudI::InvalidInputException->new();
    }
    my $key = $self->{_prefix} . $pattern;
    if (! defined($self->{_callbacks}{$key}))
    {
        $self->{_callbacks}{$key} = [$function];
    }
    else
    {
        my $function_queue_ref = $self->{_callbacks}{$key};
        push(@$function_queue_ref, $function);
    }
    $self->_send(Erlang::term_to_binary([
        Erlang::OtpErlangAtom->new('subscribe'), $pattern]));
}

sub subscribe_count
{
    my $self = shift;
    my ($pattern) = @_;
    $self->_send(Erlang::term_to_binary([
        Erlang::OtpErlangAtom->new('subscribe_count'), $pattern]));
    return $self->_poll_request(undef, 0);
}

sub unsubscribe
{
    my $self = shift;
    my ($pattern) = @_;
    my $key = $self->{_prefix} . $pattern;
    my $function_queue_ref = $self->{_callbacks}{$key};
    assert(scalar(@$function_queue_ref) > 0);
    shift(@$function_queue_ref);
    if (scalar(@$function_queue_ref) == 0)
    {
        delete($self->{_callbacks}{$key});
    }
    $self->_send(Erlang::term_to_binary([
        Erlang::OtpErlangAtom->new('unsubscribe'), $pattern]));
}

sub send_async
{
    my $self = shift;
    my ($name, $request, $timeout, $request_info, $priority) = @_;
    if (! defined($timeout))
    {
        $timeout = $self->{_timeout_async};
    }
    if (! defined($request_info))
    {
        $request_info = '';
    }
    if (! defined($priority))
    {
        $priority = $self->{_priority_default};
    }
    $self->_send(Erlang::term_to_binary([
        Erlang::OtpErlangAtom->new('send_async'), $name,
        Erlang::OtpErlangBinary->new($request_info),
        Erlang::OtpErlangBinary->new($request),
        $timeout, $priority]));
    return $self->_poll_request(undef, 0);
}

sub send_sync
{
    my $self = shift;
    my ($name, $request, $timeout, $request_info, $priority) = @_;
    if (! defined($timeout))
    {
        $timeout = $self->{_timeout_sync};
    }
    if (! defined($request_info))
    {
        $request_info = '';
    }
    if (! defined($priority))
    {
        $priority = $self->{_priority_default};
    }
    $self->_send(Erlang::term_to_binary([
        Erlang::OtpErlangAtom->new('send_sync'), $name,
        Erlang::OtpErlangBinary->new($request_info),
        Erlang::OtpErlangBinary->new($request),
        $timeout, $priority]));
    return $self->_poll_request(undef, 0);
}

sub mcast_async
{
    my $self = shift;
    my ($name, $request, $timeout, $request_info, $priority) = @_;
    if (! defined($timeout))
    {
        $timeout = $self->{_timeout_async};
    }
    if (! defined($request_info))
    {
        $request_info = '';
    }
    if (! defined($priority))
    {
        $priority = $self->{_priority_default};
    }
    $self->_send(Erlang::term_to_binary([
        Erlang::OtpErlangAtom->new('mcast_async'), $name,
        Erlang::OtpErlangBinary->new($request_info),
        Erlang::OtpErlangBinary->new($request),
        $timeout, $priority]));
    return $self->_poll_request(undef, 0);
}

sub forward_
{
    my $self = shift;
    my ($request_type, $name, $request_info, $request,
        $timeout, $priority, $trans_id, $source) = @_;
    if ($request_type == ASYNC)
    {
        $self->forward_async($name, $request_info, $request,
                             $timeout, $priority, $trans_id, $source);
    }
    elsif ($request_type == SYNC)
    {
        $self->forward_sync($name, $request_info, $request,
                            $timeout, $priority, $trans_id, $source);
    }
    else
    {
        die CloudI::InvalidInputException->new();
    }
}

sub forward_async
{
    my $self = shift;
    my ($name, $request_info, $request,
        $timeout, $priority, $trans_id, $source) = @_;
    $self->_send(Erlang::term_to_binary([
        Erlang::OtpErlangAtom->new('forward_async'), $name,
        Erlang::OtpErlangBinary->new($request_info),
        Erlang::OtpErlangBinary->new($request), $timeout, $priority,
        Erlang::OtpErlangBinary->new($trans_id), $source]));
    die CloudI::ForwardAsyncException->new();
}

sub forward_sync
{
    my $self = shift;
    my ($name, $request_info, $request,
        $timeout, $priority, $trans_id, $source) = @_;
    $self->_send(Erlang::term_to_binary([
        Erlang::OtpErlangAtom->new('forward_sync'), $name,
        Erlang::OtpErlangBinary->new($request_info),
        Erlang::OtpErlangBinary->new($request), $timeout, $priority,
        Erlang::OtpErlangBinary->new($trans_id), $source]));
    die CloudI::ForwardSyncException->new();
}

sub return_
{
    my $self = shift;
    my ($request_type, $name, $pattern, $response_info, $response,
        $timeout, $trans_id, $source) = @_;
    if ($request_type == ASYNC)
    {
        $self->return_async($name, $pattern, $response_info, $response,
                            $timeout, $trans_id, $source);
    }
    elsif ($request_type == SYNC)
    {
        $self->return_sync($name, $pattern, $response_info, $response,
                           $timeout, $trans_id, $source);
    }
    else
    {
        die CloudI::InvalidInputException->new();
    }
}

sub return_async
{
    my $self = shift;
    my ($name, $pattern, $response_info, $response,
        $timeout, $trans_id, $source) = @_;
    $self->_send(Erlang::term_to_binary([
        Erlang::OtpErlangAtom->new('return_async'), $name, $pattern,
        Erlang::OtpErlangBinary->new($response_info),
        Erlang::OtpErlangBinary->new($response), $timeout,
        Erlang::OtpErlangBinary->new($trans_id), $source]));
    die CloudI::ReturnAsyncException->new();
}

sub return_sync
{
    my $self = shift;
    my ($name, $pattern, $response_info, $response,
        $timeout, $trans_id, $source) = @_;
    $self->_send(Erlang::term_to_binary([
        Erlang::OtpErlangAtom->new('return_sync'), $name, $pattern,
        Erlang::OtpErlangBinary->new($response_info),
        Erlang::OtpErlangBinary->new($response), $timeout,
        Erlang::OtpErlangBinary->new($trans_id), $source]));
    die CloudI::ReturnSyncException->new();
}

sub recv_async
{
    my $self = shift;
    my ($timeout, $trans_id, $consume) = @_;
    if (! defined($timeout))
    {
        $timeout = $self->{_timeout_sync};
    }
    if (! defined($trans_id))
    {
        $trans_id = "\0" x 16;
    }
    if (! defined($consume))
    {
        $consume = 1;
    }
    $self->_send(Erlang::term_to_binary([
        Erlang::OtpErlangAtom->new('recv_async'), $timeout,
        Erlang::OtpErlangBinary->new($trans_id),
        ($consume ?
         Erlang::OtpErlangAtom->new('true') :
         Erlang::OtpErlangAtom->new('false'))]));
    return $self->_poll_request(undef, 0);
}

sub process_index
{
    my $self = shift;
    return $self->{_process_index};
}

# callable without an object
sub process_index_
{
    return _getenv_to_uint('CLOUDI_API_INIT_PROCESS_INDEX');
}

sub process_count
{
    my $self = shift;
    return $self->{_process_count};
}

sub process_count_max
{
    my $self = shift;
    return $self->{_process_count_max};
}

# callable without an object
sub process_count_max_
{
    return _getenv_to_uint('CLOUDI_API_INIT_PROCESS_COUNT_MAX');
}

sub process_count_min
{
    my $self = shift;
    return $self->{_process_count_min};
}

# callable without an object
sub process_count_min_
{
    return _getenv_to_uint('CLOUDI_API_INIT_PROCESS_COUNT_MIN');
}

sub prefix
{
    my $self = shift;
    return $self->{_prefix};
}

sub timeout_initialize
{
    my $self = shift;
    return $self->{_timeout_initialize};
}

# callable without an object
sub timeout_initialize_
{
    return _getenv_to_uint('CLOUDI_API_INIT_TIMEOUT_INITIALIZE');
}

sub timeout_async
{
    my $self = shift;
    return $self->{_timeout_async};
}

sub timeout_sync
{
    my $self = shift;
    return $self->{_timeout_sync};
}

sub timeout_terminate
{
    my $self = shift;
    return $self->{_timeout_terminate};
}

# callable without an object
sub timeout_terminate_
{
    return _getenv_to_uint('CLOUDI_API_INIT_TIMEOUT_TERMINATE');
}

sub priority_default
{
    my $self = shift;
    return $self->{_priority_default};
}

sub _null_response
{
    my $self = shift;
    my ($request_type, $name, $pattern, $request_info, $request,
        $timeout, $priority, $trans_id, $source) = @_;
    return '';
}

sub _callback
{
    my $self = shift;
    my ($command, $name, $pattern, $request_info, $request,
        $timeout, $priority, $trans_id, $source) = @_;
    my $function;
    if (! defined($self->{_callbacks}{$pattern}))
    {
        $function = sub { return $self->_null_response(@_); };
    }
    else
    {
        my $function_queue_ref = $self->{_callbacks}{$pattern};
        $function = shift(@$function_queue_ref);
        push(@$function_queue_ref, $function);
    }
    my $response_info;
    my $response;
    if ($command == MESSAGE_SEND_ASYNC)
    {
        eval
        {
            ($response_info,
             $response) = &$function(ASYNC, $name, $pattern,
                                     $request_info, $request,
                                     $timeout, $priority, $trans_id, $source);
            if (! defined($response))
            {
                $response = $response_info;
                $response_info = '';
                if (! defined($response))
                {
                    $response = '';
                }
            }
            elsif (ref($response_info) ne '' or
                   ! defined($response_info))
            {
                $response_info = '';
            }
            if (ref($response) ne '')
            {
                $response = '';
            }
        };
        my $e = $@;
        if ($e)
        {
            if ($e->isa('CloudI::MessageDecodingException'))
            {
                $self->{_terminate} = 1;
            }
            elsif ($e->isa('CloudI::TerminateException'))
            {
            }
            elsif ($e->isa('CloudI::ReturnAsyncException') or
                   $e->isa('CloudI::ForwardAsyncException'))
            {
                return;
            }
            elsif ($e->isa('CloudI::ReturnSyncException') or
                   $e->isa('CloudI::ForwardSyncException'))
            {
                $self->{_terminate} = 1;
                print STDERR "$e";
                return;
            }
            elsif ($e->isa('CloudI::AssertionError') or
                   $e->isa('CloudI::FatalError'))
            {
                print STDERR "$e";
                exit(1);
            }
            else
            {
                print STDERR "$e";
                if ($self->{_fatal_exceptions})
                {
                    exit(1);
                }
            }
            $response_info = '';
            $response = '';
        }
        eval
        {
            $self->return_async($name, $pattern, $response_info, $response,
                                $timeout, $trans_id, $source);
        };
        $e = $@;
        if ($e)
        {
            if (! $e->isa('CloudI::ReturnAsyncException'))
            {
                die $e;
            }
        }
        return;
    }
    elsif ($command == MESSAGE_SEND_SYNC)
    {
        eval
        {
            ($response_info,
             $response) = &$function(SYNC, $name, $pattern,
                                     $request_info, $request,
                                     $timeout, $priority, $trans_id, $source);
            if (! defined($response))
            {
                $response = $response_info;
                $response_info = '';
                if (! defined($response))
                {
                    $response = '';
                }
            }
            elsif (ref($response_info) ne '' or
                   ! defined($response_info))
            {
                $response_info = '';
            }
            if (ref($response) ne '')
            {
                $response = '';
            }
        };
        my $e = $@;
        if ($e)
        {
            if ($e->isa('CloudI::MessageDecodingException'))
            {
                $self->{_terminate} = 1;
            }
            elsif ($e->isa('CloudI::TerminateException'))
            {
            }
            elsif ($e->isa('CloudI::ReturnSyncException') or
                   $e->isa('CloudI::ForwardSyncException'))
            {
                return;
            }
            elsif ($e->isa('CloudI::ReturnAsyncException') or
                   $e->isa('CloudI::ForwardAsyncException'))
            {
                $self->{_terminate} = 1;
                print STDERR "$e";
                return;
            }
            elsif ($e->isa('CloudI::AssertionError') or
                   $e->isa('CloudI::FatalError'))
            {
                print STDERR "$e";
                exit(1);
            }
            else
            {
                print STDERR "$e";
                if ($self->{_fatal_exceptions})
                {
                    exit(1);
                }
            }
            $response_info = '';
            $response = '';
        }
        eval
        {
            $self->return_sync($name, $pattern, $response_info, $response,
                               $timeout, $trans_id, $source);
        };
        $e = $@;
        if ($e)
        {
            if (! $e->isa('CloudI::ReturnSyncException'))
            {
                die $e;
            }
        }
        return;
    }
    else
    {
        die CloudI::MessageDecodingException->new();
    }
}

sub _handle_events
{
    my $self = shift;
    my ($external, $data, $data_size, $i, $command) = @_;
    my $j;
    if (! defined($command))
    {
        if ($i > $data_size)
        {
            die CloudI::MessageDecodingException->new();
        }
        $j = 4;
        ($command) = unpack('L', substr($data, $i, $j));
    }
    else
    {
        $j = 4;
    }
    while (1)
    {
        if ($command == MESSAGE_TERM)
        {
            $self->{_terminate} = 1;
            if ($external)
            {
                return 0;
            }
            else
            {
                die CloudI::TerminateException->new(
                    $self->{_timeout_terminate});
            }
        }
        elsif ($command == MESSAGE_REINIT)
        {
            $i += $j; $j = 4 + 4 + 4 + 1 + 1;
            ($self->{_process_count},
             $self->{_timeout_async},
             $self->{_timeout_sync},
             $self->{_priority_default},
             $self->{_fatal_exceptions}) = unpack("L3 c C",
                                                  substr($data, $i, $j));
            $i += $j;
        }
        elsif ($command == MESSAGE_KEEPALIVE)
        {
            $self->_send(Erlang::term_to_binary(
                Erlang::OtpErlangAtom->new('keepalive')));
            $i += $j;
        }
        else
        {
            die CloudI::MessageDecodingException->new();
        }
        if ($i > $data_size)
        {
            die CloudI::MessageDecodingException->new();
        }
        elsif ($i == $data_size)
        {
            return 1;
        }
        $j = 4;
        ($command) = unpack('L', substr($data, $i, $j));
    }
}

sub _poll_request
{
    my $self = shift;
    my ($timeout, $external) = @_;
    if ($self->{_terminate})
    {
        if ($external)
        {
            return 0;
        }
        else
        {
            die CloudI::TerminateException->new($self->{_timeout_terminate});
        }
    }
    elsif ($external and ! $self->{_initialization_complete})
    {
        $self->_send(Erlang::term_to_binary(
            Erlang::OtpErlangAtom->new('polling')));
        $self->{_initialization_complete} = 1;
    }

    my $poll_timer;
    my $timeout_value_secs;
    if (! defined($timeout) or $timeout < 0)
    {
        $timeout_value_secs = undef;
    }
    elsif ($timeout == 0)
    {
        $timeout_value_secs = 0;
    }
    elsif ($timeout > 0)
    {
        $poll_timer = _milliseconds();
        $timeout_value_secs = $timeout / 1000.0;
    }
    my $result_read = '';
    my $result_except = '';
    vec($result_read, fileno($self->{_s}), 1) = 1;
    vec($result_except, fileno($self->{_s}), 1) = 1;
    my $result = select($result_read, undef, $result_except,
                        $timeout_value_secs);
    if ($result == -1 or vec($result_except, fileno($self->{_s}), 1) == 1)
    {
        return 0;
    }
    if ($result == 0)
    {
        return 1;
    }

    my $data = $self->_recv('');
    my $data_size = length($data);
    if ($data_size == 0)
    {
        return 0;
    }
    my $i = 0; my $j = 4;

    while (1)
    {
        my ($command) = unpack('L', substr($data, $i, $j));
        if ($command == MESSAGE_INIT)
        {
            $i += $j; $j = 4 + 4 + 4 + 4 + 4;
            my ($process_index,
                $process_count,
                $process_count_max,
                $process_count_min,
                $prefix_size) = unpack('L5', substr($data, $i, $j));
            $i += $j; $j = $prefix_size + 4 + 4 + 4 + 4 + 1 + 1 + 4;
            my ($prefix,
                $timeout_initialize,
                $timeout_async,
                $timeout_sync,
                $timeout_terminate,
                $priority_default,
                $fatal_exceptions,
                $bind) = unpack("Z$prefix_size L4 c C l",
                                substr($data, $i, $j));
            $i += $j;
            if ($bind >= 0)
            {
                die CloudI::InvalidInputException->new();
            }
            if ($i != $data_size)
            {
                assert($external == 0);
                $self->_handle_events($external, $data, $data_size, $i);
            }
            return ($process_index, $process_count,
                    $process_count_max, $process_count_min,
                    $prefix, $timeout_initialize,
                    $timeout_sync, $timeout_async, $timeout_terminate,
                    $priority_default, $fatal_exceptions);
        }
        elsif ($command == MESSAGE_SEND_ASYNC or
               $command == MESSAGE_SEND_SYNC)
        {
            $i += $j; $j = 4;
            my ($name_size) = unpack('L', substr($data, $i, $j));
            $i += $j; $j = $name_size + 4;
            my ($name,
                $pattern_size) = unpack("Z$name_size L", substr($data, $i, $j));
            $i += $j; $j = $pattern_size + 4;
            my ($pattern,
                $request_info_size) = unpack("Z$pattern_size L",
                                             substr($data, $i, $j));
            $i += $j; $j = $request_info_size + 1 + 4;
            my ($request_info,
                $request_size) = unpack("a$request_info_size x L",
                                        substr($data, $i, $j));
            $i += $j; $j = $request_size + 1 + 4 + 1 + 16 + 4;
            my ($request,
                $request_timeout,
                $priority,
                $trans_id,
                $source_size) = unpack("a$request_size x L c a16 L",
                                    substr($data, $i, $j));
            $i += $j; $j = $source_size;
            my $source = substr($data, $i, $j);
            $i += $j;
            if ($i != $data_size)
            {
                assert($external == 1);
                if (! $self->_handle_events($external, $data, $data_size, $i))
                {
                    return 0;
                }
            }
            $data = '';
            $self->_callback($command, $name, $pattern,
                             $request_info, $request,
                             $request_timeout, $priority, $trans_id,
                             Erlang::binary_to_term($source));
            if ($self->{_terminate})
            {
                return 0;
            }
        }
        elsif ($command == MESSAGE_RECV_ASYNC or
               $command == MESSAGE_RETURN_SYNC)
        {
            $i += $j; $j = 4;
            my ($response_info_size) = unpack('L', substr($data, $i, $j));
            $i += $j; $j = $response_info_size + 1 + 4;
            my ($response_info,
                $response_size) = unpack("a$response_info_size x L",
                                         substr($data, $i, $j));
            $i += $j; $j = $response_size + 1 + 16;
            my ($response,
                $trans_id) = unpack("a$response_size x a16",
                                    substr($data, $i, $j));
            $i += $j;
            if ($i != $data_size)
            {
                assert($external == 0);
                $self->_handle_events($external, $data, $data_size, $i);
            }
            return ($response_info, $response, $trans_id);
        }
        elsif ($command == MESSAGE_RETURN_ASYNC)
        {
            $i += $j; $j = 16;
            my $trans_id = substr($data, $i, $j);
            $i += $j;
            if ($i != $data_size)
            {
                assert($external == 0);
                $self->_handle_events($external, $data, $data_size, $i);
            }
            return $trans_id;
        }
        elsif ($command == MESSAGE_RETURNS_ASYNC)
        {
            $i += $j; $j = 4;
            my ($trans_id_count) = unpack('L', substr($data, $i, $j));
            $i += $j; $j = 16 * $trans_id_count;
            my @trans_ids = unpack('a16' x $trans_id_count,
                                   substr($data, $i, $j));
            $i += $j;
            if ($i != $data_size)
            {
                assert($external == 0);
                $self->_handle_events($external, $data, $data_size, $i);
            }
            return @trans_ids;
        }
        elsif ($command == MESSAGE_SUBSCRIBE_COUNT)
        {
            $i += $j; $j = 4;
            my ($count) = unpack('L', substr($data, $i, $j));
            $i += $j;
            if ($i != $data_size)
            {
                assert($external == 0);
                $self->_handle_events($external, $data, $data_size, $i);
            }
            return $count;
        }
        elsif ($command == MESSAGE_TERM)
        {
            if (! $self->_handle_events($external,
                                        $data, $data_size, $i, $command))
            {
                return 0;
            }
            assert(0);
        }
        elsif ($command == MESSAGE_REINIT)
        {
            $i += $j; $j = 4 + 4 + 4 + 1 + 1;
            ($self->{_process_count},
             $self->{_timeout_async},
             $self->{_timeout_sync},
             $self->{_priority_default},
             $self->{_fatal_exceptions}) = unpack("L3 c C",
                                                  substr($data, $i, $j));
            $i += $j; $j = 4;
            if ($i == $data_size)
            {
                $data = '';
            }
            elsif ($i < $data_size)
            {
                redo;
            }
            else
            {
                die CloudI::MessageDecodingException->new();
            }
        }
        elsif ($command == MESSAGE_KEEPALIVE)
        {
            $self->_send(Erlang::term_to_binary(
                Erlang::OtpErlangAtom->new('keepalive')));
            $i += $j; $j = 4;
            if ($i == $data_size)
            {
                $data = '';
            }
            elsif ($i < $data_size)
            {
                redo;
            }
            else
            {
                die CloudI::MessageDecodingException->new();
            }
        }
        else
        {
            die CloudI::MessageDecodingException->new();
        }

        if (defined($poll_timer))
        {
            my $poll_timer_new = _milliseconds();
            my $elapsed = _max(0, $poll_timer_new - $poll_timer);
            $poll_timer = $poll_timer_new;
            if ($elapsed >= $timeout)
            {
                $timeout = 0;
            }
            else
            {
                $timeout -= $elapsed;
            }
        }
        if (defined($timeout_value_secs))
        {
            if ($timeout == 0)
            {
                return 1;
            }
            elsif ($timeout > 0)
            {
                $timeout_value_secs = $timeout / 1000.0;
            }
        }
        $result_read = '';
        $result_except = '';
        vec($result_read, fileno($self->{_s}), 1) = 1;
        vec($result_except, fileno($self->{_s}), 1) = 1;
        $result = select($result_read, undef, $result_except,
                         $timeout_value_secs);
        if ($result == -1 or vec($result_except, fileno($self->{_s}), 1) == 1)
        {
            return 0;
        }
        if ($result == 0)
        {
            return 1;
        }

        $data = $self->_recv($data);
        $data_size = length($data);
        if ($data_size == 0)
        {
            return 0;
        }
        $i = 0; $j = 4;
    }
}

sub poll
{
    my $self = shift;
    my ($timeout) = @_;
    if (! defined($timeout))
    {
        $timeout = -1;
    }
    return $self->_poll_request($timeout, 1);
}

sub shutdown
{
    my $self = shift;
    my ($reason) = @_;
    if (! defined($reason))
    {
        $reason = '';
    }
    $self->_send(Erlang::term_to_binary([
        Erlang::OtpErlangAtom->new('shutdown'), $reason]));
}

# callable without an object
sub _text_pairs_parse
{
    my ($text) = @_;
    my %pairs = ();
    my @text_segments = split("\0", $text);
    my $size = scalar(@text_segments);
    if ($size >= 2)
    {
        use integer;
        for my $i_step (0 .. (($size / 2) - 1))
        {
            my $i = $i_step * 2;
            my $key = $text_segments[$i];
            my $value = $pairs{$key};
            if (defined($value))
            {
                if (ref($value) eq 'ARRAY')
                {
                    push(@$value, $text_segments[$i + 1]);
                }
                else
                {
                    $pairs{$key} = [$value, $text_segments[$i + 1]];
                }
            }
            else
            {
                $pairs{$key} = $text_segments[$i + 1];
            }
        }
    }
    return %pairs;
}

# callable without an object
sub _text_pairs_new
{
    my ($pairs_ref, $response) = @_;
    my $text = '';
    while (my ($key, $values) = each(%$pairs_ref))
    {
        if (ref($values) eq '' and scalar($values) eq $values)
        {
            # values is a string
            $text .= $key . "\0" . $values . "\0";
        }
        else
        {
            for my $value (@$values)
            {
                $text .= $key . "\0" . $value . "\0";
            }
        }
    }
    if ($response and $text eq '')
    {
        $text = "\0";
    }
    return $text;
}

# callable without an object
sub info_key_value_parse
{
    my ($info) = @_;
    return _text_pairs_parse($info);
}

# callable without an object
sub info_key_value_new
{
    my ($pairs_ref, $response) = @_;
    if (! defined($response))
    {
        $response = 1;
    }
    return _text_pairs_new($pairs_ref, $response);
}

sub _send
{
    my $self = shift;
    my ($data) = @_;
    if ($self->{_use_header})
    {
        $data = pack('N', length($data)) . $data;
    }
    syswrite($self->{_s}, $data);
}

sub _recv
{
    my $self = shift;
    my ($data_old) = @_;
    my $data = '';
    my $fragment = '';
    my $read;
    if ($self->{_use_header})
    {
        my $i = 0;
        while ($i < 4)
        {
            $read = sysread($self->{_s}, $fragment, 4 - $i);
            if (! defined($read) or $read == 0)
            {
                die CloudI::MessageDecodingException->new();
            }
            $i += $read;
            $data .= $fragment;
        }
        my ($total) = unpack('N', $data);
        $i = 0;
        $data = $data_old;
        while ($i < $total)
        {
            $read = sysread($self->{_s}, $fragment,
                            _min($total - $i, $self->{_size}));
            if (! defined($read) or $read == 0)
            {
                die CloudI::MessageDecodingException->new();
            }
            $i += $read;
            $data .= $fragment;
        }
    }
    else
    {
        $data = $data_old;
        my $i = length($data);
        my $ready = 1;
        while ($ready)
        {
            $read = sysread($self->{_s}, $fragment, $self->{_size});
            if (! defined($read) or $read == 0)
            {
                die CloudI::MessageDecodingException->new();
            }
            $data .= $fragment;
            $ready = ($read == $self->{_size});

            if ($ready)
            {
                my $result_read = '';
                vec($result_read, fileno($self->{_s}), 1) = 1;
                $ready = (select($result_read, undef, undef, 0) == 1);
            }
        }
    }
    return $data;
}

sub _getenv_to_uint
{
    my ($name) = @_;
    my $value = getenv($name);
    if (! defined($value) or $value !~ /^\d+$/)
    {
        die CloudI::InvalidInputException->new();
    }
    return $value;
}

sub assert
{
    my ($test) = @_;
    $test or die CloudI::AssertionError->new();
    return $test;
}

sub _milliseconds
{
    use integer;
    my ($seconds, $microseconds) = gettimeofday();
    return $seconds * 1000 + $microseconds / 1000;
}

sub _min
{
    my ($min, @args) = @_;
    for my $v (@args)
    {
        if ($v < $min)
        {
            $min = $v;
        }
    }
    return $min;
}

sub _max
{
    my ($max, @args) = @_;
    for my $v (@args)
    {
        if ($v > $max)
        {
            $max = $v;
        }
    }
    return $max;
}

1;
