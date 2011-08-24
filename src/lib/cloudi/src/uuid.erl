%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Erlang UUID Generation==
%%% http://www.ietf.org/rfc/rfc4122.txt is the reference for official UUIDs.
%%% This implementation provides a version 1 UUID that includes the Erlang pid
%%% identifier (ID, Serial, Creation) within the 48 bit node ID.  To make room
%%% for the Erlang pid identifier, the 48 bits from the MAC address are
%%% bitwise-XORed (i.e., 3 OCI (Organizationally Unique Identifier)
%%% bytes and 3 NIC (Network Interface Controller) specific bytes) down to
%%% 16 bits. The Erlang pid is bitwise-XORed from 72 bits down to 32 bits.
%%% The version 3 (MD5), version 4 (random), and version 5 (SHA)
%%% methods are provided as specified within the RFC.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2011, Michael Truog <mjtruog at gmail dot com>
%%% All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%% 
%%%     * Redistributions of source code must retain the above copyright
%%%       notice, this list of conditions and the following disclaimer.
%%%     * Redistributions in binary form must reproduce the above copyright
%%%       notice, this list of conditions and the following disclaimer in
%%%       the documentation and/or other materials provided with the
%%%       distribution.
%%%     * All advertising materials mentioning features or use of this
%%%       software must display the following acknowledgment:
%%%         This product includes software developed by Michael Truog
%%%     * The name of the author may not be used to endorse or promote
%%%       products derived from this software without specific prior
%%%       written permission
%%% 
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
%%% CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
%%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
%%% OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
%%% CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
%%% DAMAGE.
%%%
%%% @author Michael Truog <mjtruog [at] gmail (dot) com>
%%% @copyright 2011 Michael Truog
%%% @version 0.1.4 {@date} {@time}
%%%------------------------------------------------------------------------

-module(uuid).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([new/1,
         get_v1/1,
         get_v1_time/1,
         get_v3/1,
         get_v4/0,
         get_v5/1,
         uuid_to_string/1,
         increment/1]).

-record(uuid_state,
    {
        node_id,
        clock_seq,
        clock_seq_high,
        clock_seq_low
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

new(Pid) when is_pid(Pid) ->
    {ok, Ifs} = inet:getiflist(),
    If = lists:last(lists:filter(fun(I) ->
        not lists:prefix("lo", I)
    end, Ifs)),
    NodeData = if
        If =:= [] ->
            % include the distributed Erlang node name to be node specific
            erlang:list_to_binary(erlang:atom_to_list(node()));
        true ->
            % 48 bits for MAC address
            {ok,[{hwaddr, MAC}]} = inet:ifget(If, [hwaddr]),
            % include the distributed Erlang node name to be node specific
            erlang:list_to_binary(MAC ++ erlang:atom_to_list(node()))
    end,
    <<NODE01, NODE02, NODE03, NODE04, NODE05,
      NODE06, NODE07, NODE08, NODE09, NODE10,
      NODE11, NODE12, NODE13, NODE14, NODE15,
      NODE16, NODE17, NODE18, NODE19, NODE20>> = crypto:sha(NodeData),
    % reduce the 160 bit checksum to 16 bits
    NodeByte1 = ((((((((NODE01 bxor NODE02)
                       bxor NODE03)
                      bxor NODE04)
                     bxor NODE05)
                    bxor NODE06)
                   bxor NODE07)
                  bxor NODE08)
                 bxor NODE09)
                bxor NODE10,
    NodeByte2 = ((((((((NODE11 bxor NODE12)
                       bxor NODE13)
                      bxor NODE14)
                     bxor NODE15)
                    bxor NODE16)
                   bxor NODE17)
                  bxor NODE18)
                 bxor NODE19)
                bxor NODE20,
    % make the version 1 UUID both node and pid specific
    PidBin = erlang:term_to_binary(Pid),
    % 72 bits for the Erlang pid
    <<ID1:8, ID2:8, ID3:8, ID4:8, % ID (Node index)
      SR1:8, SR2:8, SR3:8, SR4:8, % Serial (Process index)
      CR1:8                       % Node Creation Count
      >> = binary:part(PidBin, erlang:byte_size(PidBin), -9),
    % reduce the Erlang pid to 32 bits
    PidByte1 = ID1 bxor SR4,
    PidByte2 = ID2 bxor SR3,
    PidByte3 = ID3 bxor SR2,
    PidByte4 = (ID4 bxor SR1) bxor CR1,
    ClockSeq = random:uniform(16384) - 1,
    <<ClockSeqHigh:6, ClockSeqLow:8>> = <<ClockSeq:14>>,
    #uuid_state{node_id = <<NodeByte1:8, NodeByte2:8,
                            PidByte1:8, PidByte2:8,
                            PidByte3:8, PidByte4:8>>,
                clock_seq = ClockSeq,
                clock_seq_high = ClockSeqHigh,
                clock_seq_low = ClockSeqLow}.

get_v1(#uuid_state{node_id = NodeId,
                   clock_seq_high = ClockSeqHigh,
                   clock_seq_low = ClockSeqLow}) ->
    {MegaSeconds, Seconds, MicroSeconds} = erlang:now(),
    Time = (MegaSeconds * 1000000 + Seconds) * 1000000 + MicroSeconds,
    <<TimeHigh:12/little, TimeMid:16/little, TimeLow:32/little>> = <<Time:60>>,
    <<TimeLow:32, TimeMid:16, TimeHigh:12,
      0:1, 0:1, 0:1, 1:1,  % version 1 bits
      ClockSeqHigh:6,
      0:1, 1:1,            % reserved bits
      ClockSeqLow:8,
      NodeId/binary>>.

get_v1_time(#uuid_state{}) ->
    {MegaSeconds, Seconds, MicroSeconds} = erlang:now(),
    (MegaSeconds * 1000000 + Seconds) * 1000000 + MicroSeconds;

get_v1_time(Value)
    when is_binary(Value), byte_size(Value) == 16 ->
    <<TimeLow:32, TimeMid:16, TimeHigh:12,
      0:1, 0:1, 0:1, 1:1,  % version 1 bits
      _:6,
      0:1, 1:1,            % reserved bits
      _:8, _:48>> = Value,
    <<Time:60>> = <<TimeHigh:12/little, TimeMid:16/little, TimeLow:32/little>>,
    Time. % microseconds since epoch

get_v3([I | _] = Name)
    when is_integer(I) ->
    <<B1:60, B2a:6, B2b:6, B3:56>> = crypto:md5(Name),
    B2 = B2a bxor B2b,
    <<B1:60,
      0:1, 0:1, 1:1, 1:1,  % version 3 bits
      B2:6,
      0:1, 1:1,            % reserved bits
      B3:56>>.

% crypto:rand_bytes/1 repeats in the same way as
% RAND_pseudo_bytes within OpenSSL.
% if OpenSSL is configured to use the MD PRNG (default) with SHA1
% (in openssl/crypto/rand/md_rand.c),
% the collisions are between 2^80 and 2^51
% (http://eprint.iacr.org/2008/469.pdf).  So, that means this would
% repeat ideally every 1.21e24 and at worst every 2.25e15.
% if OpenSSL was compiled in FIPS mode, it uses ANSI X9.31 RNG
% and would have collisions based on 3DES (which is a black-box algorithm,
% i.e., the DES S-boxes used within the cipher were never published).
get_v4() ->
    <<Rand1:60, _:4, Rand2:6, _:2, Rand3:56>> = crypto:rand_bytes(16),
    <<Rand1:60,
      0:1, 1:1, 0:1, 0:1,  % version 4 bits
      Rand2:6,
      0:1, 1:1,            % reserved bits
      Rand3:56>>.

get_v5([I | _] = Name)
    when is_integer(I) ->
    <<B1:60, B2:6, B3a:56, B3b:38>> = crypto:sha(Name),
    B3 = B3a bxor B3b,
    <<B1:60,
      0:1, 1:1, 0:1, 1:1,  % version 5 bits
      B2:6,
      0:1, 1:1,            % reserved bits
      B3:56>>.

uuid_to_string(Value)
    when is_binary(Value), byte_size(Value) == 16 ->
    <<B1:32/unsigned-integer,
      B2:16/unsigned-integer,
      B3:16/unsigned-integer,
      B4:16/unsigned-integer,
      B5:48/unsigned-integer>> = Value,
    lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b",
                                [B1, B2, B3, B4, B5])).

% The RFC said to increment the clock sequence counter
% if the system clock was set backwards.  However, erlang:now/0 always
% provides increasing time values, so this function is not necessary
% when the system clock changes.  Since the version 1 node id contains the
% Erlang PID ID, Serial, and Creation numbers in a (non-destructive)
% bitwise-xor operation, the node id is specific to both the Erlang node
% and the Erlang node lifetime (the PID Creation is different after a node 
% crash). Therefore, it is unclear why this function would be necessary
% within this Erlang implementation of v1 UUID generation (if the system
% is always running).
increment(#uuid_state{clock_seq = ClockSeq} = State) ->
    NextClockSeq = ClockSeq + 1,
    NewClockSeq = if
        NextClockSeq == 16384 ->
            0;
        true ->
            NextClockSeq
    end,
    <<ClockSeqHigh:6, ClockSeqLow:8>> = <<NewClockSeq:14>>,
    State#uuid_state{clock_seq = NewClockSeq,
                     clock_seq_high = ClockSeqHigh,
                     clock_seq_low = ClockSeqLow}.

