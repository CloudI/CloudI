%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Erlang UUID Generation==
%%% [http://www.ietf.org/rfc/rfc4122.txt] is the reference for official UUIDs.
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
%%% Copyright (c) 2011-2013, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2011-2013 Michael Truog
%%% @version 1.3.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(uuid).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([new/1,
         new/2,
         get_v1/1,
         get_v1_time/0,
         get_v1_time/1,
         is_v1/1,
         get_v3/1,
         get_v3/2,
         get_v3_compat/1,
         get_v3_compat/2,
         is_v3/1,
         get_v4/0,
         get_v4/1,
         get_v4_urandom/0,
         get_v4_urandom_bigint/0,
         get_v4_urandom_native/0,
         is_v4/1,
         get_v5/1,
         get_v5/2,
         get_v5_compat/1,
         get_v5_compat/2,
         is_v5/1,
         uuid_to_list/1,
         uuid_to_string/1,
         uuid_to_string/2,
         string_to_uuid/1,
         is_uuid/1,
         increment/1,
         mac_address/0,
         test/0]).

-record(uuid_state,
    {
        node_id :: <<_:48>>,
        clock_seq :: 0..16383,
        timestamp_type :: 'os' | 'erlang'
    }).

-type state() :: #uuid_state{}.
-export_type([state/0]).

-include("uuid.hrl").

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Create new UUID state for v1 UUID generation.===
%% @end
%%-------------------------------------------------------------------------

-spec new(Pid :: pid()) ->
    #uuid_state{}.

new(Pid) when is_pid(Pid) ->
    new(Pid, [{timestamp_type, erlang}]).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create new UUID state for v1 UUID generation using a specific type of timestamp.===
%% The timestamp can either be based on erlang:now/0 with erlang or
%% os:timestamp/0 with os.  erlang:now/0 will make sure all time values are
%% increasing, even if the system clock changes.  os:timestamp/0 will get the
%% system clock quickly without modifying the result.
%% @end
%%-------------------------------------------------------------------------

-spec new(Pid :: pid(),
          Options :: 'os' | 'erlang' |
                     list({timestamp_type, 'os' | 'erlang'} |
                          {mac_address, list(non_neg_integer())})) ->
    #uuid_state{}.

new(Pid, TimestampType)
    when is_pid(Pid),
         ((TimestampType =:= erlang) orelse (TimestampType =:= os)) ->
    new(Pid, [{timestamp_type, TimestampType}]);
new(Pid, Options)
    when is_pid(Pid), is_list(Options) ->
    TimestampType = case lists:keyfind(timestamp_type, 1, Options) of
        {timestamp_type, Value1} ->
            Value1;
        false ->
            erlang
    end,
    MacAddress = case lists:keyfind(mac_address, 1, Options) of
        {mac_address, Value2} ->
            Value2;
        false ->
            mac_address()
    end,

    % make the version 1 UUID specific to the Erlang node and pid

    % 48 bits for the first MAC address found is included with the
    % distributed Erlang node name
    <<NodeD01, NodeD02, NodeD03, NodeD04, NodeD05,
      NodeD06, NodeD07, NodeD08, NodeD09, NodeD10,
      NodeD11, NodeD12, NodeD13, NodeD14, NodeD15,
      NodeD16, NodeD17, NodeD18, NodeD19, NodeD20>> =
      crypto:hash(sha, erlang:list_to_binary(MacAddress ++
                                             erlang:atom_to_list(node()))),
    % later, when the pid format changes, handle the different format
    ExternalTermFormatVersion = 131,
    PidExtType = 103,
    <<ExternalTermFormatVersion:8,
      PidExtType:8,
      PidBin/binary>> = erlang:term_to_binary(Pid),
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
    #uuid_state{node_id = <<NodeByte1:8, NodeByte2:8,
                            PidByte1:8, PidByte2:8,
                            PidByte3:8, PidByte4:8>>,
                clock_seq = ClockSeq,
                timestamp_type = TimestampType}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a v1 UUID.===
%% @end
%%-------------------------------------------------------------------------

-spec get_v1(#uuid_state{}) ->
    <<_:128>>.

get_v1(#uuid_state{node_id = NodeId,
                   clock_seq = ClockSeq,
                   timestamp_type = TimestampType}) ->
    {MegaSeconds, Seconds, MicroSeconds} = if
        TimestampType =:= erlang ->
            erlang:now();
        TimestampType =:= os ->
            os:timestamp()
    end,
    % 16#01b21dd213814000 is the number of 100-ns intervals between the
    % UUID epoch 1582-10-15 00:00:00 and the Unix epoch 1970-01-01 00:00:00.
    Time = ((MegaSeconds * 1000000 + Seconds) * 1000000 + MicroSeconds) * 10 +
           16#01b21dd213814000,
    <<TimeHigh:12, TimeMid:16, TimeLow:32>> = <<Time:60>>,
    <<TimeLow:32, TimeMid:16,
      0:1, 0:1, 0:1, 1:1,  % version 1 bits
      TimeHigh:12,
      1:1, 0:1,            % RFC 4122 variant bits
      ClockSeq:14,
      NodeId/binary>>.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the current time value in a manner consistent with the v1 UUID.===
%% The result is an integer in microseconds.
%% @end
%%-------------------------------------------------------------------------

-spec get_v1_time() ->
    non_neg_integer().

get_v1_time() ->
    get_v1_time(erlang).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the current time value in a manner consistent with the v1 UUID.===
%% The result is an integer in microseconds.
%% @end
%%-------------------------------------------------------------------------

-spec get_v1_time('os' | 'erlang' | #uuid_state{} | <<_:128>>) ->
    non_neg_integer().

get_v1_time(erlang) ->
    {MegaSeconds, Seconds, MicroSeconds} = erlang:now(),
    (MegaSeconds * 1000000 + Seconds) * 1000000 + MicroSeconds;

get_v1_time(os) ->
    {MegaSeconds, Seconds, MicroSeconds} = os:timestamp(),
    (MegaSeconds * 1000000 + Seconds) * 1000000 + MicroSeconds;

get_v1_time(#uuid_state{timestamp_type = TimestampType}) ->
    get_v1_time(TimestampType);

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the time value from a v1 UUID.===
%% The result is an integer in microseconds.
%% @end
%%-------------------------------------------------------------------------

get_v1_time(Value)
    when is_binary(Value), byte_size(Value) == 16 ->
    <<TimeLow:32, TimeMid:16,
      0:1, 0:1, 0:1, 1:1,  % version 1 bits
      TimeHigh:12,
      1:1, 0:1,            % RFC 4122 variant bits
      _:14,
      _:48>> = Value,
    <<Time:60>> = <<TimeHigh:12, TimeMid:16, TimeLow:32>>,
    erlang:trunc((Time - 16#01b21dd213814000) / 10). % microseconds since epoch

%%-------------------------------------------------------------------------
%% @doc
%% ===Is the binary a v1 UUID?===
%% @end
%%-------------------------------------------------------------------------

-spec is_v1(Value :: any()) ->
    boolean().

is_v1(<<_TimeLow:32, _TimeMid:16,
        0:1, 0:1, 0:1, 1:1,  % version 1 bits
        _TimeHigh:12,
        1:1, 0:1,            % RFC 4122 variant bits
        _ClockSeq:14,
        _NodeId:48>>) ->
    true;
is_v1(_) ->
    false.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a v3 UUID.===
%% @end
%%-------------------------------------------------------------------------

-spec get_v3(Data :: binary()) ->
    <<_:128>>.

get_v3(Data) when is_binary(Data) ->
    <<B1:48, B4a:4, B2:12, B4b:2, B3:14, B4c:48>> =
        crypto:hash(md5, Data),
    B4 = (B4a bxor B4b) bxor B4c,
    <<B1:48,
      0:1, 0:1, 1:1, 1:1,  % version 3 bits
      B2:12,
      1:1, 0:1,            % RFC 4122 variant bits
      B3:14,
      B4:48>>.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a v3 UUID in a particular namespace.===
%% @end
%%-------------------------------------------------------------------------

-spec get_v3(Namespace :: dns | url | oid | x500 | binary(),
             Data :: binary() | iolist()) ->
    <<_:128>>.

get_v3(dns, Data) ->
    get_v3(?UUID_NAMESPACE_DNS, Data);
get_v3(url, Data) ->
    get_v3(?UUID_NAMESPACE_URL, Data);
get_v3(oid, Data) ->
    get_v3(?UUID_NAMESPACE_OID, Data);
get_v3(x500, Data) ->
    get_v3(?UUID_NAMESPACE_X500, Data);
get_v3(Namespace, Data) when is_binary(Namespace) ->
    DataBin = if
        is_binary(Data) ->
            Data;
        is_list(Data) ->
            erlang:iolist_to_binary(Data)
    end,
    get_v3(<<Namespace/binary, DataBin/binary>>).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a compatible v3 UUID.===
%% Do not use all bits from the checksum so that the UUID matches external
%% implementations.
%% @end
%%-------------------------------------------------------------------------

-spec get_v3_compat(Data :: binary()) ->
    <<_:128>>.

get_v3_compat(Data) when is_binary(Data) ->
    <<B1:48, _:4, B2:12, _:2, B3:14, B4:48>> =
        crypto:hash(md5, Data),
    <<B1:48,
      0:1, 0:1, 1:1, 1:1,  % version 3 bits
      B2:12,
      1:1, 0:1,            % RFC 4122 variant bits
      B3:14,
      B4:48>>.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a compatible v3 UUID in a particular namespace.===
%% Do not use all bits from the checksum so that the UUID matches external
%% implementations.
%% @end
%%-------------------------------------------------------------------------

-spec get_v3_compat(Namespace :: dns | url | oid | x500 | binary(),
                    Data :: binary() | iolist()) ->
    <<_:128>>.

get_v3_compat(dns, Data) ->
    get_v3_compat(?UUID_NAMESPACE_DNS, Data);
get_v3_compat(url, Data) ->
    get_v3_compat(?UUID_NAMESPACE_URL, Data);
get_v3_compat(oid, Data) ->
    get_v3_compat(?UUID_NAMESPACE_OID, Data);
get_v3_compat(x500, Data) ->
    get_v3_compat(?UUID_NAMESPACE_X500, Data);
get_v3_compat(Namespace, Data) when is_binary(Namespace) ->
    DataBin = if
        is_binary(Data) ->
            Data;
        is_list(Data) ->
            erlang:iolist_to_binary(Data)
    end,
    get_v3_compat(<<Namespace/binary, DataBin/binary>>).

%%-------------------------------------------------------------------------
%% @doc
%% ===Is the binary a v3 UUID?===
%% @end
%%-------------------------------------------------------------------------

-spec is_v3(Value :: any()) ->
    boolean().

is_v3(<<_:48,
        0:1, 0:1, 1:1, 1:1,  % version 3 bits
        _:12,
        1:1, 0:1,            % RFC 4122 variant bits
        _:62>>) ->
    true;
is_v3(_) ->
    false.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a v4 UUID (using crypto/openssl).===
%% crypto:strong_rand_bytes/1 repeats in the same way as
%% RAND_bytes within OpenSSL.
%% crypto:rand_bytes/1 repeats in the same way as
%% RAND_pseudo_bytes within OpenSSL.
%% if OpenSSL is configured to use the MD PRNG (default) with SHA1
%% (in openssl/crypto/rand/md_rand.c),
%% the collisions are between 2^80 and 2^51
%% ([http://eprint.iacr.org/2008/469.pdf]).  So, that means "weak" would
%% repeat ideally every 1.21e24 and at worst every 2.25e15.
%% if OpenSSL was compiled in FIPS mode, it uses ANSI X9.31 RNG
%% and would have collisions based on 3DES.
%% @end
%%-------------------------------------------------------------------------

-spec get_v4() ->
    <<_:128>>.

get_v4() ->
    get_v4(strong).

-spec get_v4('strong' | 'weak') ->
    <<_:128>>.

get_v4(strong) ->
    <<Rand1:48, _:4, Rand2:12, _:2, Rand3:62>> = crypto:strong_rand_bytes(16),
    <<Rand1:48,
      0:1, 1:1, 0:1, 0:1,  % version 4 bits
      Rand2:12,
      1:1, 0:1,            % RFC 4122 variant bits
      Rand3:62>>;

get_v4(weak) ->
    <<Rand1:48, _:4, Rand2:12, _:2, Rand3:62>> = crypto:rand_bytes(16),
    <<Rand1:48,
      0:1, 1:1, 0:1, 0:1,  % version 4 bits
      Rand2:12,
      1:1, 0:1,            % RFC 4122 variant bits
      Rand3:62>>.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a v4 UUID (using Wichmann-Hill 2006).===
%% random_wh06_int:uniform/1 repeats every 2.66e36 (2^121) approx.
%% (see B.A. Wichmann and I.D.Hill, in
%%  'Generating good pseudo-random numbers',
%%  Computational Statistics and Data Analysis 51 (2006) 1614-1622)
%% a single random_wh06_int:uniform/1 call can provide a maximum of 124 bits
%% (see random_wh06_int.erl for details)
%% @end
%%-------------------------------------------------------------------------

-spec get_v4_urandom() ->
    <<_:128>>.

get_v4_urandom() ->
    % random 122 bits
    Rand = random_wh06_int:uniform(5316911983139663491615228241121378304) - 1,
    <<Rand1:48, Rand2:12, Rand3:62>> = <<Rand:122>>,
    <<Rand1:48,
      0:1, 1:1, 0:1, 0:1,  % version 4 bits
      Rand2:12,
      1:1, 0:1,            % RFC 4122 variant bits
      Rand3:62>>.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a v4 UUID (using Wichmann-Hill 1982).===
%% random:uniform/1 repeats every 2.78e13
%% (see B.A. Wichmann and I.D.Hill, in
%%  'An efficient and portable pseudo-random number generator',
%%  Journal of Applied Statistics. AS183. 1982, or Byte March 1987)
%% a single random:uniform/1 call can provide a maximum of 45 bits
%% (currently this is not significantly faster
%%  because multiple function calls are necessary)
%%
%% explain the 45 bits of randomness:
%%  random:uniform/0 code:
%%   B1 = (A1*171) rem 30269,
%%   B2 = (A2*172) rem 30307,
%%   B3 = (A3*170) rem 30323,
%%   put(random_seed, {B1,B2,B3}),
%%   R = A1/30269 + A2/30307 + A3/30323,
%%   R - trunc(R).
%%
%%  {B1, B2, B3} becomes the next seed value {A1, A2, A3}, so:
%%    R = (918999161 * A1 + 917846887 * A2 + 917362583 * A3) / 27817185604309
%%  Whatever the values for A1, A2, and A3,
%%  (918999161 * A1 + 917846887 * A2 + 917362583 * A3) can not exceed
%%  27817185604309 (30269 * 30307 * 30323) because of the previous modulus.
%%  So, random:uniform/1 is unable to uniformly sample numbers beyond
%%  a N of 27817185604309. The bits required to represent 27817185604309:
%%   1> (math:log(27817185604309) / math:log(2)) + 1.
%%   45.6610416965467
%%
%% @end
%%-------------------------------------------------------------------------

-spec get_v4_urandom_bigint() ->
    <<_:128>>.

get_v4_urandom_bigint() ->
    Rand1 = random:uniform(2199023255552) - 1, % random 41 bits
    Rand2 = random:uniform(2199023255552) - 1, % random 41 bits
    Rand3 = random:uniform(1099511627776) - 1, % random 40 bits
    <<Rand2a:7, Rand2b:12, Rand2c:22>> = <<Rand2:41>>,
    <<Rand1:41, Rand2a:7,
      0:1, 1:1, 0:1, 0:1,  % version 4 bits
      Rand2b:12,
      1:1, 0:1,            % RFC 4122 variant bits
      Rand2c:22, Rand3:40>>.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a v4 UUID (using Wichmann-Hill 1982).===
%% Attempt to only use native integers (Erlang limits integers to 27 bits
%% before using bigints) to investigate the speed when using HiPE.
%% @end
%%-------------------------------------------------------------------------

-spec get_v4_urandom_native() ->
    <<_:128>>.

get_v4_urandom_native() ->
    Rand1 = random:uniform(134217728) - 1, % random 27 bits
    Rand2 = random:uniform(2097152) - 1,   % random 21 bits
    Rand3 = random:uniform(1048576) - 1,   % random 20 bits
    Rand4 = random:uniform(134217728) - 1, % random 27 bits
    Rand5 = random:uniform(134217728) - 1, % random 27 bits
    <<Rand3a:12, Rand3b:8>> = <<Rand3:20>>,
    <<Rand1:27, Rand2:21,
      0:1, 1:1, 0:1, 0:1,  % version 4 bits
      Rand3a:12,
      1:1, 0:1,            % RFC 4122 variant bits
      Rand3b:8, Rand4:27, Rand5:27>>.

%%-------------------------------------------------------------------------
%% @doc
%% ===Is the binary a v4 UUID?===
%% @end
%%-------------------------------------------------------------------------

-spec is_v4(Value :: any()) ->
    boolean().

is_v4(<<_:48,
        0:1, 1:1, 0:1, 0:1,  % version 4 bits
        _:12,
        1:1, 0:1,            % RFC 4122 variant bits
        _:62>>) ->
    true;
is_v4(_) ->
    false.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a v5 UUID.===
%% @end
%%-------------------------------------------------------------------------

-spec get_v5(Data :: binary()) ->
    <<_:128>>.

get_v5(Data) when is_binary(Data) ->
    <<B1:48, B4a:4, B2:12, B4b:2, B3:14, B4c:32, B4d:48>> =
        crypto:hash(sha, Data),
    B4 = ((B4a bxor B4b) bxor B4c) bxor B4d,
    <<B1:48,
      0:1, 1:1, 0:1, 1:1,  % version 5 bits
      B2:12,
      1:1, 0:1,            % RFC 4122 variant bits
      B3:14,
      B4:48>>.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a v5 UUID in a particular namespace.===
%% @end
%%-------------------------------------------------------------------------

-spec get_v5(Namespace :: dns | url | oid | x500 | binary(),
             Data :: binary() | iolist()) ->
    <<_:128>>.

get_v5(dns, Data) ->
    get_v5(?UUID_NAMESPACE_DNS, Data);
get_v5(url, Data) ->
    get_v5(?UUID_NAMESPACE_URL, Data);
get_v5(oid, Data) ->
    get_v5(?UUID_NAMESPACE_OID, Data);
get_v5(x500, Data) ->
    get_v5(?UUID_NAMESPACE_X500, Data);
get_v5(Namespace, Data) when is_binary(Namespace) ->
    DataBin = if
        is_binary(Data) ->
            Data;
        is_list(Data) ->
            erlang:iolist_to_binary(Data)
    end,
    get_v5(<<Namespace/binary, DataBin/binary>>).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a compatible v5 UUID.===
%% Do not use all bits from the checksum so that the UUID matches external
%% implementations.
%% @end
%%-------------------------------------------------------------------------

-spec get_v5_compat(Data :: binary()) ->
    <<_:128>>.

get_v5_compat(Data) when is_binary(Data) ->
    <<B1:48, _:4, B2:12, _:2, B3:14, B4:48, _:32>> =
        crypto:hash(sha, Data),
    <<B1:48,
      0:1, 1:1, 0:1, 1:1,  % version 5 bits
      B2:12,
      1:1, 0:1,            % RFC 4122 variant bits
      B3:14,
      B4:48>>.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a compatible v5 UUID in a particular namespace.===
%% Do not use all bits from the checksum so that the UUID matches external
%% implementations.
%% @end
%%-------------------------------------------------------------------------

-spec get_v5_compat(Namespace :: dns | url | oid | x500 | binary(),
                    Data :: binary() | iolist()) ->
    <<_:128>>.

get_v5_compat(dns, Data) ->
    get_v5_compat(?UUID_NAMESPACE_DNS, Data);
get_v5_compat(url, Data) ->
    get_v5_compat(?UUID_NAMESPACE_URL, Data);
get_v5_compat(oid, Data) ->
    get_v5_compat(?UUID_NAMESPACE_OID, Data);
get_v5_compat(x500, Data) ->
    get_v5_compat(?UUID_NAMESPACE_X500, Data);
get_v5_compat(Namespace, Data) when is_binary(Namespace) ->
    DataBin = if
        is_binary(Data) ->
            Data;
        is_list(Data) ->
            erlang:iolist_to_binary(Data)
    end,
    get_v5_compat(<<Namespace/binary, DataBin/binary>>).

%%-------------------------------------------------------------------------
%% @doc
%% ===Is the binary a v5 UUID?===
%% @end
%%-------------------------------------------------------------------------

-spec is_v5(Value :: any()) ->
    boolean().

is_v5(<<_:48,
        0:1, 1:1, 0:1, 1:1,  % version 5 bits
        _:12,
        1:1, 0:1,            % RFC 4122 variant bits
        _:62>>) ->
    true;
is_v5(_) ->
    false.

%%-------------------------------------------------------------------------
%% @doc
%% ===Convert a UUID to a list.===
%% @end
%%-------------------------------------------------------------------------

-spec uuid_to_list(Value :: <<_:128>>) ->
    iolist().

uuid_to_list(Value)
    when is_binary(Value), byte_size(Value) == 16 ->
    <<B1:32/unsigned-integer,
      B2:16/unsigned-integer,
      B3:16/unsigned-integer,
      B4:16/unsigned-integer,
      B5:48/unsigned-integer>> = Value,
    [B1, B2, B3, B4, B5].

%%-------------------------------------------------------------------------
%% @doc
%% ===Convert a UUID to a string representation.===
%% @end
%%-------------------------------------------------------------------------

-spec uuid_to_string(Value :: <<_:128>>) ->
    string().

uuid_to_string(Value) ->
    uuid_to_string(Value, standard).

%%-------------------------------------------------------------------------
%% @doc
%% ===Convert a UUID to a string representation based on an option.===
%% @end
%%-------------------------------------------------------------------------

-spec uuid_to_string(Value :: <<_:128>>,
                     Option :: standard | nodash |
                               list_standard | list_nodash |
                               binary_standard | binary_nodash) ->
    string() | binary().

uuid_to_string(<<Value:128/unsigned-integer>>, standard) ->
    [N01, N02, N03, N04, N05, N06, N07, N08,
     N09, N10, N11, N12,
     N13, N14, N15, N16,
     N17, N18, N19, N20,
     N21, N22, N23, N24, N25, N26, N27, N28, N29, N30, N31, N32] =
        int_to_hex_list(Value, 32),
    [N01, N02, N03, N04, N05, N06, N07, N08, $-,
     N09, N10, N11, N12, $-,
     N13, N14, N15, N16, $-,
     N17, N18, N19, N20, $-,
     N21, N22, N23, N24, N25, N26, N27, N28, N29, N30, N31, N32];

uuid_to_string(<<Value:128/unsigned-integer>>, nodash) ->
    int_to_hex_list(Value, 32);

uuid_to_string(Value, list_standard) ->
    uuid_to_string(Value, standard);

uuid_to_string(Value, list_nodash) ->
    uuid_to_string(Value, nodash);

uuid_to_string(<<Value:128/unsigned-integer>>, binary_standard) ->
    [N01, N02, N03, N04, N05, N06, N07, N08,
     N09, N10, N11, N12,
     N13, N14, N15, N16,
     N17, N18, N19, N20,
     N21, N22, N23, N24, N25, N26, N27, N28, N29, N30, N31, N32] =
        int_to_hex_list(Value, 32),
    <<N01, N02, N03, N04, N05, N06, N07, N08, $-,
      N09, N10, N11, N12, $-,
      N13, N14, N15, N16, $-,
      N17, N18, N19, N20, $-,
      N21, N22, N23, N24, N25, N26, N27, N28, N29, N30, N31, N32>>;

uuid_to_string(<<Value:128/unsigned-integer>>, binary_nodash) ->
    [N01, N02, N03, N04, N05, N06, N07, N08,
     N09, N10, N11, N12,
     N13, N14, N15, N16,
     N17, N18, N19, N20,
     N21, N22, N23, N24, N25, N26, N27, N28, N29, N30, N31, N32] =
        int_to_hex_list(Value, 32),
    <<N01, N02, N03, N04, N05, N06, N07, N08,
      N09, N10, N11, N12,
      N13, N14, N15, N16,
      N17, N18, N19, N20,
      N21, N22, N23, N24, N25, N26, N27, N28, N29, N30, N31, N32>>.

%%-------------------------------------------------------------------------
%% @doc
%% ===Convert a string representation to a UUID.===
%% @end
%%-------------------------------------------------------------------------

-spec string_to_uuid(string() | binary()) ->
    <<_:128>>.

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
                   N27, N28, N29, N30, N31, N32);

string_to_uuid(_) ->
    erlang:exit(badarg).

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

%%-------------------------------------------------------------------------
%% @doc
%% ===Is the binary a UUID?===
%% @end
%%-------------------------------------------------------------------------

-spec is_uuid(Value :: any()) ->
    boolean().

is_uuid(Value)
    when is_binary(Value), byte_size(Value) == 16 ->
    is_v1(Value) orelse is_v3(Value) orelse is_v4(Value) orelse is_v5(Value);
is_uuid(_) ->
    false.

%%-------------------------------------------------------------------------
%% @doc
%% ===Increment the clock sequence of v1 UUID state.===
%% The RFC said to increment the clock sequence counter
%% if the system clock was set backwards.  However, erlang:now/0 always
%% provides increasing time values, so this function is not necessary
%% when the system clock changes.  Since the version 1 node id contains the
%% Erlang PID ID, Serial, and Creation numbers in a (non-destructive)
%% bitwise-xor operation, the node id is specific to both the Erlang node
%% and the Erlang node lifetime (the PID Creation is different after a node
%% crash). Therefore, it is unclear why this function would be necessary
%% within this Erlang implementation of v1 UUID generation (if the system
%% is always running). The only event that seems to require this function's
%% usage is if the v1 UUID has been stored and retrieved where both actions
%% occurred at a point with a system clock change inbetween or possibly
%% on different machines with a large difference in system clocks
%% (i.e., in some situation that isn't handled by the Erlang VM, so
%%  possibly if an external distribution mechanism was used between
%%  Erlang VMs, not connected with distributed Erlang).
%% @end
%%-------------------------------------------------------------------------

-spec increment(State :: #uuid_state{}) ->
    #uuid_state{}.

increment(#uuid_state{clock_seq = ClockSeq} = State) ->
    NextClockSeq = ClockSeq + 1,
    NewClockSeq = if
        NextClockSeq == 16384 ->
            0;
        true ->
            NextClockSeq
    end,
    State#uuid_state{clock_seq = NewClockSeq}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Provide a usable network interface MAC address.===
%% @end
%%-------------------------------------------------------------------------

-spec mac_address() ->
    list(non_neg_integer()).

mac_address() ->
    {ok, Ifs} = inet:getifaddrs(),
    mac_address(lists:keysort(1, Ifs)).

%%-------------------------------------------------------------------------
%% @doc
%% ===Regression test.===
%% @end
%%-------------------------------------------------------------------------

-spec test() ->
    ok.

test() ->
    % version 1 tests
    % uuidgen -t ; date
    % "Fri Dec  7 19:13:58 PST 2012"
    % (Sat Dec  8 03:13:58 UTC 2012)
    V1uuid1 = uuid:string_to_uuid("4ea4b020-40e5-11e2-ac70-001fd0a5484e"),
    "4ea4b020-40e5-11e2-ac70-001fd0a5484e" =
        uuid:uuid_to_string(V1uuid1, standard),
    <<V1TimeLow1:32, V1TimeMid1:16,
      0:1, 0:1, 0:1, 1:1,  % version 1 bits
      V1TimeHigh1:12,
      1:1, 0:1,            % RFC 4122 variant bits
      V1ClockSeq1:14,
      V1NodeId1/binary>> = V1uuid1,
    true = uuid:is_uuid(V1uuid1),
    true = uuid:is_v1(V1uuid1),
    <<V1Time1:60>> = <<V1TimeHigh1:12, V1TimeMid1:16, V1TimeLow1:32>>,
    V1Time1total = erlang:trunc((V1Time1 - 16#01b21dd213814000) / 10),
    V1Time1mega = erlang:trunc(V1Time1total / 1000000000000),
    V1Time1sec = erlang:trunc(V1Time1total / 1000000 - V1Time1mega * 1000000),
    V1Time1micro = erlang:trunc(V1Time1total -
        (V1Time1mega * 1000000 + V1Time1sec) * 1000000),
    {{2012, 12, 8}, {3, 13, 58}} =
        calendar:now_to_datetime({V1Time1mega, V1Time1sec, V1Time1micro}),
    % $ python
    % >>> import uuid
    % >>> import time
    % >>> uuid.uuid1().hex;time.time()
    % '50d15f5c40e911e2a0eb001fd0a5484e'
    % 1354938160.1998589
    V1uuid2 = uuid:string_to_uuid("50d15f5c40e911e2a0eb001fd0a5484e"),
    "50d15f5c40e911e2a0eb001fd0a5484e" =
        uuid:uuid_to_string(V1uuid2, nodash),
    <<V1TimeLow2:32, V1TimeMid2:16,
      0:1, 0:1, 0:1, 1:1,  % version 1 bits
      V1TimeHigh2:12,
      1:1, 0:1,            % RFC 4122 variant bits
      V1ClockSeq2:14,
      V1NodeId2/binary>> = V1uuid2,
    true = uuid:is_v1(V1uuid2),
    <<V1Time2:60>> = <<V1TimeHigh2:12, V1TimeMid2:16, V1TimeLow2:32>>,
    V1Time2total = erlang:trunc((V1Time2 - 16#01b21dd213814000) / 10),
    V1Time2Amega = erlang:trunc(V1Time2total / 1000000000000),
    V1Time2Asec = erlang:trunc(V1Time2total / 1000000 - V1Time2Amega * 1000000),
    V1Time2Amicro = erlang:trunc(V1Time2total -
        (V1Time2Amega * 1000000 + V1Time2Asec) * 1000000),
    V1Time2B = 1354938160.1998589,
    V1Time2Bmega = erlang:trunc(V1Time2B / 1000000),
    V1Time2Bsec = erlang:trunc(V1Time2B - V1Time2Bmega * 1000000),
    V1Time2Bmicro = erlang:trunc(V1Time2B * 1000000 -
        (V1Time2Bmega * 1000000 + V1Time2Bsec) * 1000000),
    true = (V1Time2Amega == V1Time2Bmega),
    true = (V1Time2Asec == V1Time2Bsec),
    true = (V1Time2Amicro < V1Time2Bmicro) and
           ((V1Time2Amicro + 605) == V1Time2Bmicro),
    true = V1ClockSeq1 /= V1ClockSeq2,
    true = V1NodeId1 == V1NodeId2,
    V1uuid3 = uuid:get_v1(uuid:new(self(), erlang)),
    V1uuid3timeB = uuid:get_v1_time(erlang),
    V1uuid3timeA = uuid:get_v1_time(V1uuid3),
    true = (V1uuid3timeA < V1uuid3timeB) and
           ((V1uuid3timeA + 1000) > V1uuid3timeB),
    true = is_number(uuid:get_v1_time(
        uuid:string_to_uuid("3ff0fc1e-c23b-11e2-b8a0-38607751fca5"))),
    true = is_number(uuid:get_v1_time(
        uuid:string_to_uuid("67ff79a6-c23b-11e2-b374-38607751fca5"))),
    true = is_number(uuid:get_v1_time(
        uuid:string_to_uuid("7134eede-c23b-11e2-a4a7-38607751fca5"))),
    true = is_number(uuid:get_v1_time(
        uuid:string_to_uuid("717003c0-c23b-11e2-83a4-38607751fca5"))),
    V1uuid4 = uuid:get_v1(uuid:new(self(), os)),
    V1uuid4timeB = uuid:get_v1_time(os),
    V1uuid4timeA = uuid:get_v1_time(V1uuid4),
    true = (V1uuid4timeA < V1uuid4timeB) and
           ((V1uuid4timeA + 1000) > V1uuid4timeB),

    % version 3 tests
    % $ python
    % >>> import uuid
    % >>> uuid.uuid3(uuid.NAMESPACE_DNS, 'test').hex
    % '45a113acc7f230b090a5a399ab912716'
    V3uuid1 = uuid:string_to_uuid("45a113acc7f230b090a5a399ab912716"),
    "45a113acc7f230b090a5a399ab912716" =
        uuid:uuid_to_string(V3uuid1, nodash),
    <<V3uuid1A:48,
      0:1, 0:1, 1:1, 1:1,  % version 3 bits
      V3uuid1B:12,
      1:1, 0:1,            % RFC 4122 variant bits
      V3uuid1C:14,
      V3uuid1D:48>> = V3uuid1,
    true = uuid:is_v3(V3uuid1),
    V3uuid2 = uuid:get_v3(?UUID_NAMESPACE_DNS, <<"test">>),
    true = (V3uuid2 == uuid:get_v3(dns, <<"test">>)),
    <<V3uuid2A:48,
      0:1, 0:1, 1:1, 1:1,  % version 3 bits
      V3uuid2B:12,
      1:1, 0:1,            % RFC 4122 variant bits
      V3uuid2C:14,
      V3uuid2D:48>> = V3uuid2,
    true = uuid:is_v3(V3uuid2),
    true = ((V3uuid1A == V3uuid2A) and
            (V3uuid1B == V3uuid2B) and
            (V3uuid1C == V3uuid2C)),
    % check fails:
    % since the python uuid discards bits from MD5 while this module
    % bitwise xor the middle bits to utilize the whole checksum
    false = (V3uuid1D == V3uuid2D),

    % replicate the same UUID value used in other implementations
    % where bits are discarded from the checksum
    V3uuid1 = uuid:get_v3_compat(?UUID_NAMESPACE_DNS, <<"test">>),
    true = (uuid:get_v3_compat(dns, <<"test1">>) ==
            uuid:string_to_uuid("c501822b22a837ff91a99545f4689a3d")),
    true = (uuid:get_v3_compat(dns, <<"test2">>) ==
            uuid:string_to_uuid("f191764306b23e6dab770a5044067d0a")),
    true = (uuid:get_v3_compat(dns, <<"test3">>) ==
            uuid:string_to_uuid("bf7f1e5a6b28310c8f9ef815dbd56fb7")),
    true = (uuid:get_v3_compat(dns, <<"test4">>) ==
            uuid:string_to_uuid("fe584e2496c83d2d8b39f1cc6a877f72")),

    % version 4 tests
    % uuidgen -r
    V4uuid1 = uuid:string_to_uuid("ffe8b758-a5dc-4bf4-9eb0-878e010e8df7"),
    "ffe8b758-a5dc-4bf4-9eb0-878e010e8df7" =
        uuid:uuid_to_string(V4uuid1, standard),
    <<V4Rand1A:48,
      0:1, 1:1, 0:1, 0:1,  % version 4 bits
      V4Rand1B:12,
      1:1, 0:1,            % RFC 4122 variant bits
      V4Rand1C:14,
      V4Rand1D:48>> = V4uuid1,
    true = uuid:is_v4(V4uuid1),
    % $ python
    % >>> import uuid
    % >>> uuid.uuid4().hex
    % 'cc9769818fe747398e2422e99fee2753'
    V4uuid2 = uuid:string_to_uuid("cc9769818fe747398e2422e99fee2753"),
    "cc9769818fe747398e2422e99fee2753" =
        uuid:uuid_to_string(V4uuid2, nodash),
    <<V4Rand2A:48,
      0:1, 1:1, 0:1, 0:1,  % version 4 bits
      V4Rand2B:12,
      1:1, 0:1,            % RFC 4122 variant bits
      V4Rand2C:14,
      V4Rand2D:48>> = V4uuid2,
    true = uuid:is_v4(V4uuid2),
    V4uuid3 = uuid:get_v4(strong),
    <<_:48,
      0:1, 1:1, 0:1, 0:1,  % version 4 bits
      _:12,
      1:1, 0:1,            % RFC 4122 variant bits
      _:14,
      _:48>> = V4uuid3,
    true = uuid:is_v4(V4uuid3),
    true = (V4Rand1A /= V4Rand2A),
    true = (V4Rand1B /= V4Rand2B),
    true = (V4Rand1C /= V4Rand2C),
    true = (V4Rand1D /= V4Rand2D),

    % version 5 tests
    % $ python
    % >>> import uuid
    % >>> uuid.uuid5(uuid.NAMESPACE_DNS, 'test').hex
    % '4be0643f1d98573b97cdca98a65347dd'
    V5uuid1 = uuid:string_to_uuid("4be0643f1d98573b97cdca98a65347dd"),
    "4be0643f1d98573b97cdca98a65347dd" =
        uuid:uuid_to_string(V5uuid1, nodash),
    <<V5uuid1A:48,
      0:1, 1:1, 0:1, 1:1,  % version 5 bits
      V5uuid1B:12,
      1:1, 0:1,            % RFC 4122 variant bits
      V5uuid1C:14,
      V5uuid1D:48>> = V5uuid1,
    true = uuid:is_v5(V5uuid1),
    V5uuid2 = uuid:get_v5(?UUID_NAMESPACE_DNS, <<"test">>),
    true = (V5uuid2 == uuid:get_v5(dns, <<"test">>)),
    <<V5uuid2A:48,
      0:1, 1:1, 0:1, 1:1,  % version 5 bits
      V5uuid2B:12,
      1:1, 0:1,            % RFC 4122 variant bits
      V5uuid2C:14,
      V5uuid2D:48>> = V5uuid2,
    true = uuid:is_v5(V5uuid2),
    true = ((V5uuid1A == V5uuid2A) and
            (V5uuid1B == V5uuid2B) and
            (V5uuid1C == V5uuid2C)),
    % check fails:
    % since the python uuid discards bits from SHA while this module
    % bitwise xor the remaining bits to utilize the whole checksum
    false = (V5uuid1D == V5uuid2D),

    % replicate the same UUID value used in other implementations
    % where bits are discarded from the checksum
    V5uuid1 = uuid:get_v5_compat(?UUID_NAMESPACE_DNS, <<"test">>),
    true = (uuid:get_v5_compat(dns, <<"test1">>) ==
            uuid:string_to_uuid("86e3aed315535d238d612286215e65f1")),
    true = (uuid:get_v5_compat(dns, <<"test2">>) ==
            uuid:string_to_uuid("6eabff02c9685cbcbc7f3b672928a761")),
    true = (uuid:get_v5_compat(dns, <<"test3">>) ==
            uuid:string_to_uuid("20ca53afd04c58a2a8b3d02b9e414e80")),
    true = (uuid:get_v5_compat(dns, <<"test4">>) ==
            uuid:string_to_uuid("3e673fdc1a4f5b168890dbe7e763f7b5")),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

int_to_hex_list(I, N) when is_integer(I), I >= 0 ->
    int_to_hex_list([], I, 1, N).

int_to_hex_list_pad(L, 0) ->
    L;
int_to_hex_list_pad(L, Count) ->
    int_to_hex_list_pad([$0 | L], Count - 1).

int_to_hex_list(L, I, Count, N)
    when I < 16 ->
    int_to_hex_list_pad([int_to_hex(I) | L], N - Count);
int_to_hex_list(L, I, Count, N) ->
    int_to_hex_list([int_to_hex(I rem 16) | L], I div 16, Count + 1, N).

-compile({inline, [{int_to_hex,1}, {hex_to_int,1}]}).

int_to_hex(I) when 0 =< I, I =< 9 ->
    I + $0;
int_to_hex(I) when 10 =< I, I =< 15 ->
    (I - 10) + $a.

hex_to_int(C1, C2) ->
    hex_to_int(C1) * 16 + hex_to_int(C2).

hex_to_int(C) when $0 =< C, C =< $9 ->
    C - $0;
hex_to_int(C) when $A =< C, C =< $F ->
    C - $A + 10;
hex_to_int(C) when $a =< C, C =< $f ->
    C - $a + 10.

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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

internal_test_() ->
    [
        {"internal tests", ?_assertEqual(ok, test())}
    ].

-endif.

