%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Erlang UUID Generation==
%%% http://www.ietf.org/rfc/rfc4122.txt is the reference for official UUIDs.
%%% This implementation provides a version 1 UUID that includes both the
%%% Erlang pid identifier (ID, Serial, Creation) and the distributed Erlang
%%% node name within the 48 bit node ID.  To make room for the Erlang pid
%%% identifier, the 48 bits from the MAC address
%%% (i.e., 3 OCI (Organizationally Unique Identifier) bytes and
%%% 3 NIC (Network Interface Controller) specific bytes) and
%%% the distributed Erlang node name are bitwise-XORed down to 16 bits.
%%% The Erlang pid is bitwise-XORed from 72 bits down to 32 bits.
%%% The version 3 (MD5), version 4 (random), and version 5 (SHA)
%%% methods are provided as specified within the RFC.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2011-2012, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2011-2012 Michael Truog
%%% @version 0.2.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(uuid).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([new/1,
         new/2,
         get_v1/1,
         get_v1_time/0,
         get_v1_time/1,
         get_v3/1,
         get_v3/2,
         get_v4/0,
         get_v4_urandom_bigint/0,
         get_v4_urandom_native/0,
         get_v5/1,
         get_v5/2,
         uuid_to_string/1,
         string_to_uuid/1,
         increment/1]).

-record(uuid_state,
    {
        node_id,
        clock_seq,
        clock_seq_high,
        clock_seq_low,
        timestamp_type
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

new(Pid) when is_pid(Pid) ->
    new(Pid, erlang).

new(Pid, TimestampType)
    when is_pid(Pid), TimestampType =:= erlang;
         is_pid(Pid), TimestampType =:= os ->
    % make the version 1 UUID specific to the Erlang node and pid

    % 48 bits for the first MAC address found is included with the
    % distributed Erlang node name
    <<NodeD01, NodeD02, NodeD03, NodeD04, NodeD05,
      NodeD06, NodeD07, NodeD08, NodeD09, NodeD10,
      NodeD11, NodeD12, NodeD13, NodeD14, NodeD15,
      NodeD16, NodeD17, NodeD18, NodeD19, NodeD20>> =
      crypto:sha(erlang:list_to_binary(mac_address() ++
                                       erlang:atom_to_list(node()))),
    PidBin = erlang:term_to_binary(Pid),
    % 72 bits for the Erlang pid
    <<PidID1:8, PidID2:8, PidID3:8, PidID4:8, % ID (Node specific, 15 bits)
      PidSR1:8, PidSR2:8, PidSR3:8, PidSR4:8, % Serial (extra uniqueness)
      PidCR1:8                       % Node Creation Count
      >> = binary:part(PidBin, erlang:byte_size(PidBin), -9),
    % reduce the 160 bit NodeData checksum to 16 bits
    NodeByte1 = ((((((((NodeD01 bxor NodeD02)
                       bxor NodeD03)
                      bxor NodeD04)
                     bxor NodeD05)
                    bxor NodeD06)
                   bxor NodeD07)
                  bxor NodeD08)
                 bxor NodeD09)
                bxor NodeD10,
    NodeByte2 = (((((((((NodeD11 bxor NodeD12)
                        bxor NodeD13)
                       bxor NodeD14)
                      bxor NodeD15)
                     bxor NodeD16)
                    bxor NodeD17)
                   bxor NodeD18)
                  bxor NodeD19)
                 bxor NodeD20)
                bxor PidCR1,
    % reduce the Erlang pid to 32 bits
    PidByte1 = PidID1 bxor PidSR4,
    PidByte2 = PidID2 bxor PidSR3,
    PidByte3 = PidID3 bxor PidSR2,
    PidByte4 = PidID4 bxor PidSR1,
    ClockSeq = random:uniform(16384) - 1,
    <<ClockSeqHigh:6, ClockSeqLow:8>> = <<ClockSeq:14>>,
    #uuid_state{node_id = <<NodeByte1:8, NodeByte2:8,
                            PidByte1:8, PidByte2:8,
                            PidByte3:8, PidByte4:8>>,
                clock_seq = ClockSeq,
                clock_seq_high = ClockSeqHigh,
                clock_seq_low = ClockSeqLow,
                timestamp_type = TimestampType}.

get_v1(#uuid_state{node_id = NodeId,
                   clock_seq_high = ClockSeqHigh,
                   clock_seq_low = ClockSeqLow,
                   timestamp_type = TimestampType}) ->
    {MegaSeconds, Seconds, MicroSeconds} = if
        TimestampType =:= erlang ->
            erlang:now();
        TimestampType =:= os ->
            os:timestamp()
    end,
    Time = (MegaSeconds * 1000000 + Seconds) * 1000000 + MicroSeconds,
    <<TimeHigh:12/little, TimeMid:16/little, TimeLow:32/little>> = <<Time:60>>,
    <<TimeLow:32, TimeMid:16, TimeHigh:12,
      0:1, 0:1, 0:1, 1:1,  % version 1 bits
      ClockSeqHigh:6,
      0:1, 1:1,            % reserved bits
      ClockSeqLow:8,
      NodeId/binary>>.

get_v1_time() ->
    get_v1_time(erlang).

get_v1_time(erlang) ->
    {MegaSeconds, Seconds, MicroSeconds} = erlang:now(),
    (MegaSeconds * 1000000 + Seconds) * 1000000 + MicroSeconds;

get_v1_time(os) ->
    {MegaSeconds, Seconds, MicroSeconds} = os:timestamp(),
    (MegaSeconds * 1000000 + Seconds) * 1000000 + MicroSeconds;

get_v1_time(#uuid_state{timestamp_type = TimestampType}) ->
    get_v1_time(TimestampType);

get_v1_time(Value)
    when is_binary(Value), byte_size(Value) == 16 ->
    <<TimeLow:32, TimeMid:16, TimeHigh:12,
      0:1, 0:1, 0:1, 1:1,  % version 1 bits
      _:6,
      0:1, 1:1,            % reserved bits
      _:8, _:48>> = Value,
    <<Time:60>> = <<TimeHigh:12/little, TimeMid:16/little, TimeLow:32/little>>,
    Time. % microseconds since epoch

get_v3(Name) ->
    <<B1:60, B2a:6, B2b:6, B3:56>> = crypto:md5(Name),
    B2 = B2a bxor B2b,
    <<B1:60,
      0:1, 0:1, 1:1, 1:1,  % version 3 bits
      B2:6,
      0:1, 1:1,            % reserved bits
      B3:56>>.

get_v3(Namespace, Name) when is_binary(Namespace) ->
    NameBin = if
        is_binary(Name) ->
            Name;
        is_list(Name) ->
            erlang:list_to_binary(Name)
    end,
    get_v3(<<Namespace/binary, NameBin/binary>>).

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

% random:uniform/1 repeats every 2.78e13
% (see B.A. Wichmann and I.D.Hill, in 
%  'An efficient and portable pseudo-random number generator',
%  Journal of Applied Statistics. AS183. 1982, or Byte March 1987)
% a single random:uniform/1 call can provide a maximum of 45 bits
% (currently this is not significantly faster
%  because multiple function calls are necessary)

get_v4_urandom_bigint() ->
    Rand1 = random:uniform(2199023255552) - 1, % random 41 bits
    Rand2 = random:uniform(2199023255552) - 1, % random 41 bits
    Rand3 = random:uniform(1099511627776) - 1, % random 40 bits
    <<Rand2a:19, Rand2b:6, Rand2c:16>> = <<Rand2:41>>,
    <<Rand1:41, Rand2a:19,
      0:1, 1:1, 0:1, 0:1, % version 4 bits
      Rand2b:6,
      0:1, 1:1, % reserved bits
      Rand2c:16, Rand3:40>>.

% Erlang only allows 27 bits to be used for a native integer

get_v4_urandom_native() ->
    Rand1 = random:uniform(134217727) - 1, % random 27 bits
    Rand2 = random:uniform(134217727) - 1, % random 27 bits
    Rand3 = random:uniform(16383) - 1, % random 14 bits
    Rand4 = random:uniform(134217727) - 1, % random 27 bits
    Rand5 = random:uniform(134217727) - 1, % random 27 bits
    <<Rand3a:2, Rand3b:6, Rand3c:6>> = <<Rand3:14>>,
    <<Rand1:27, Rand2:27, Rand3a:2, 
      0:1, 1:1, 0:1, 0:1, % version 4 bits
      Rand3b:6,
      0:1, 1:1, % reserved bits
      Rand3c:6, Rand4:27, Rand5:27>>.

get_v5(Name) ->
    <<B1:60, B2:6, B3a:56, B3b:38>> = crypto:sha(Name),
    B3 = B3a bxor B3b,
    <<B1:60,
      0:1, 1:1, 0:1, 1:1,  % version 5 bits
      B2:6,
      0:1, 1:1,            % reserved bits
      B3:56>>.

get_v5(Namespace, Name) when is_binary(Namespace) ->
    NameBin = if
        is_binary(Name) ->
            Name;
        is_list(Name) ->
            erlang:list_to_binary(Name)
    end,
    get_v5(<<Namespace/binary, NameBin/binary>>).

uuid_to_string(Value)
    when is_binary(Value), byte_size(Value) == 16 ->
    <<B1:32/unsigned-integer,
      B2:16/unsigned-integer,
      B3:16/unsigned-integer,
      B4:16/unsigned-integer,
      B5:48/unsigned-integer>> = Value,
    lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b",
                                [B1, B2, B3, B4, B5])).

string_to_uuid([N01, N02, N03, N04, N05, N06, N07, N08, $-,
                N09, N10, N11, N12, $-,
                N13, N14, N15, N16, $-,
                N17, N18, N19, N20, $-,
                N21, N22, N23, N24, N25, N26,
                N27, N28, N29, N30, N31, N32]) ->
    string_to_uuid(N01, N02, N03, N04, N05, N06, N07, N08,
                   N09, N10, N11, N12,
                   N13, N14, N15, N16,
                   N17, N18, N19, N20,
                   N21, N22, N23, N24, N25, N26,
                   N27, N28, N29, N30, N31, N32);

string_to_uuid([N01, N02, N03, N04, N05, N06, N07, N08,
                N09, N10, N11, N12,
                N13, N14, N15, N16,
                N17, N18, N19, N20,
                N21, N22, N23, N24, N25, N26,
                N27, N28, N29, N30, N31, N32]) ->
    string_to_uuid(N01, N02, N03, N04, N05, N06, N07, N08,
                   N09, N10, N11, N12,
                   N13, N14, N15, N16,
                   N17, N18, N19, N20,
                   N21, N22, N23, N24, N25, N26,
                   N27, N28, N29, N30, N31, N32);

string_to_uuid(<<N01, N02, N03, N04, N05, N06, N07, N08, $-,
                 N09, N10, N11, N12, $-,
                 N13, N14, N15, N16, $-,
                 N17, N18, N19, N20, $-,
                 N21, N22, N23, N24, N25, N26,
                 N27, N28, N29, N30, N31, N32>>) ->
    string_to_uuid(N01, N02, N03, N04, N05, N06, N07, N08,
                   N09, N10, N11, N12,
                   N13, N14, N15, N16,
                   N17, N18, N19, N20,
                   N21, N22, N23, N24, N25, N26,
                   N27, N28, N29, N30, N31, N32);

string_to_uuid(<<N01, N02, N03, N04, N05, N06, N07, N08,
                 N09, N10, N11, N12,
                 N13, N14, N15, N16,
                 N17, N18, N19, N20,
                 N21, N22, N23, N24, N25, N26,
                 N27, N28, N29, N30, N31, N32>>) ->
    string_to_uuid(N01, N02, N03, N04, N05, N06, N07, N08,
                   N09, N10, N11, N12,
                   N13, N14, N15, N16,
                   N17, N18, N19, N20,
                   N21, N22, N23, N24, N25, N26,
                   N27, N28, N29, N30, N31, N32).

string_to_uuid(N01, N02, N03, N04, N05, N06, N07, N08,
               N09, N10, N11, N12,
               N13, N14, N15, N16,
               N17, N18, N19, N20,
               N21, N22, N23, N24, N25, N26,
               N27, N28, N29, N30, N31, N32) ->
    B01 = hex_to_int(N01, N02),
    B02 = hex_to_int(N03, N04),
    B03 = hex_to_int(N05, N06),
    B04 = hex_to_int(N07, N08),
    B05 = hex_to_int(N09, N10),
    B06 = hex_to_int(N11, N12),
    B07 = hex_to_int(N13, N14),
    B08 = hex_to_int(N15, N16),
    B09 = hex_to_int(N17, N18),
    B10 = hex_to_int(N19, N20),
    B11 = hex_to_int(N21, N22),
    B12 = hex_to_int(N23, N24),
    B13 = hex_to_int(N25, N26),
    B14 = hex_to_int(N27, N28),
    B15 = hex_to_int(N29, N30),
    B16 = hex_to_int(N31, N32),
    <<B01, B02, B03, B04, B05, B06, B07, B08,
      B09, B10, B11, B12, B13, B14, B15, B16>>.

% The RFC said to increment the clock sequence counter
% if the system clock was set backwards.  However, erlang:now/0 always
% provides increasing time values, so this function is not necessary
% when the system clock changes.  Since the version 1 node id contains the
% Erlang PID ID, Serial, and Creation numbers in a (non-destructive)
% bitwise-xor operation, the node id is specific to both the Erlang node
% and the Erlang node lifetime (the PID Creation is different after a node 
% crash). Therefore, it is unclear why this function would be necessary
% within this Erlang implementation of v1 UUID generation (if the system
% is always running). The only event that seems to require this function's
% usage is if the v1 UUID has been stored and retrieved where both actions
% occurred at a point with a system clock change inbetween or possibly
% on different machines with a large difference in system clocks
% (i.e., in some situation that isn't handled by the Erlang VM, so
%  possibly if an external distribution mechanism was used between
%  Erlang VMs, not connected with distributed Erlang).
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

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

-compile({inline, [{hex_to_int,1}]}).

hex_to_int(C1, C2) ->
    hex_to_int(C1) * 16 + hex_to_int(C2).

hex_to_int(C) when $0 =< C, C =< $9 ->
    C - $0;
hex_to_int(C) when $A =< C, C =< $F ->
    C - $A + 10;
hex_to_int(C) when $a =< C, C =< $f ->
    C - $a + 10.

mac_address() ->
    {ok, Ifs} = inet:getifaddrs(),
    mac_address(lists:keysort(1, Ifs)).

mac_address([]) ->
    [0, 0, 0, 0, 0, 0];

mac_address([{_, L} | Rest]) ->
    case lists:keyfind(hwaddr, 1, L) of
        false ->
            mac_address(Rest);
        {hwaddr, [0, 0, 0, 0, 0, 0]} ->
            mac_address(Rest);
        {hwaddr, [_, N2, N3, N4, N5, N6] = MAC}
            when N2 /= 0, N3 /= 0, N4 /= 0, N5 /= 0, N6 /= 0 ->
            MAC;
        {hwaddr, _} ->
            mac_address(Rest)
    end.
