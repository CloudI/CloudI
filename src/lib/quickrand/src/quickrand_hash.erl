%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Random Number Generation With Hash Functions==
%%% The random numbers created by the functions in this module are
%%% not meant for cryptographic purposes.
%%%
%%% Any functions that have a jenkins prefix use Bob Jenkins' lookup3 hashing
%%% (lookup3, May 2006).  In my testing, the function jenkins_32 is 4 times
%%% slower than erlang:phash2/1 because jenkins_32 is implemented in Erlang.
%%% Both the jenkins_32 and jenkins_64 functions execute at a speed similar to
%%% crypto:hash(ripemd160,_) with crypto:hash(sha256,_) slightly faster and
%%% crypto:hash(sha512,_) slightly slower.
%%%
%%% Any functions that have a jenkins64 prefix use Bob Jenkins' SpookyHash
%%% (SpookyV2, August 5 2012).  In my testing, the function jenkins64_128 is
%%% 4.5 times slower than crypto:hash(md5,_) which provides the same number
%%% of bits, because the jenkins64 functions are implemented in Erlang.
%%%
%%% The jenkins prefix functions are faster than the jenkins64 prefix functions
%%% due to avoiding Erlang bignums and both provide the same quality.
%%%
%%% All the functions have been checked with the C++ implementations to ensure
%%% the same hash value is obtained, though this implementation forces numbers
%%% to be interpreted as big-endian.
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2017-2022 Michael Truog <mjtruog at protonmail dot com>
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a
%%% copy of this software and associated documentation files (the "Software"),
%%% to deal in the Software without restriction, including without limitation
%%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%%% and/or sell copies of the Software, and to permit persons to whom the
%%% Software is furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%%% DEALINGS IN THE SOFTWARE.
%%%
%%% @author Michael Truog <mjtruog at protonmail dot com>
%%% @copyright 2017-2022 Michael Truog
%%% @version 2.0.5 {@date} {@time}
%%%------------------------------------------------------------------------

-module(quickrand_hash).
-author('mjtruog at protonmail dot com').

%% external interface
-export([jenkins_32/1,
         jenkins_32/2,
         jenkins_64/1,
         jenkins_64/2,
         jenkins64_128/1,
         jenkins64_128/2,
         jenkins64_64/1,
         jenkins64_64/2,
         jenkins64_32/1,
         jenkins64_32/2]).

-ifdef(OTP_RELEASE).
% able to use -if/-elif here
-if(?OTP_RELEASE >= 24).
-define(ERLANG_OTP_VERSION_24_FEATURES, true).
-endif.
-endif.

-ifdef(ERLANG_OTP_VERSION_24_FEATURES).
% iodata() recursive type is not infinitely recursive
% like within the function iodata_to_list/1
% (unable to only use no_underspecs on iodata_to_list/1)
-dialyzer({no_underspecs,
           [jenkins_32/1,
            jenkins_32/2,
            jenkins_64/1,
            jenkins_64/2,
            jenkins64_128/1,
            jenkins64_128/2,
            jenkins64_64/1,
            jenkins64_64/2,
            jenkins64_32/1,
            jenkins64_32/2]}).
-endif.

% a constant which:
%  * is not zero
%  * is odd
%  * is a not-very-regular mix of 1's and 0's
%  * does not need any other special mathematical properties
-define(JENKINS64_CONST, 16#DEADBEEFDEADBEEF).
-define(JENKINS_CONST, 16#DEADBEEF).

-include("quickrand_constants.hrl").

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Bob Jenkins lookup3 hashword for iodata.===
%% @end
%%-------------------------------------------------------------------------

-spec jenkins_32(MessageRaw :: iodata()) ->
    non_neg_integer().

jenkins_32(MessageRaw) ->
    jenkins_32(MessageRaw, 0).

%%-------------------------------------------------------------------------
%% @doc
%% ===Bob Jenkins lookup3 hashword for iodata.===
%% @end
%%-------------------------------------------------------------------------

-spec jenkins_32(MessageRaw :: iodata(),
                 Seed :: non_neg_integer()) ->
    non_neg_integer().

jenkins_32(MessageRaw, Seed)
    when is_integer(Seed), Seed >= 0 ->
    {Message, Size} = iodata_to_list(MessageRaw),
    A = B = C = add_32(?JENKINS_CONST, ((Size + 3) div 4) bsl 2, Seed),
    {HashA, _} = jenkins_32(Message, Size, A, B, C),
    HashA.

%%-------------------------------------------------------------------------
%% @doc
%% ===Bob Jenkins lookup3 hashword2 for iodata.===
%% @end
%%-------------------------------------------------------------------------

-spec jenkins_64(MessageRaw :: iodata()) ->
    non_neg_integer().

jenkins_64(MessageRaw) ->
    jenkins_64(MessageRaw, 0).

%%-------------------------------------------------------------------------
%% @doc
%% ===Bob Jenkins lookup3 hashword2 for iodata.===
%% @end
%%-------------------------------------------------------------------------

-spec jenkins_64(MessageRaw :: iodata(),
                 Seed :: non_neg_integer()) ->
    non_neg_integer().

jenkins_64(MessageRaw, Seed)
    when is_integer(Seed), Seed >= 0 ->
    {Message, Size} = iodata_to_list(MessageRaw),
    A = B = C0 = add_32(?JENKINS_CONST, ((Size + 3) div 4) bsl 2, Seed),
    CN = add_32(C0, Seed bsr 32),
    {HashA, HashB} = jenkins_32(Message, Size, A, B, CN),
    (HashB bsl 32) + HashA.

%%-------------------------------------------------------------------------
%% @doc
%% ===Bob Jenkins SpookyHashV2 Hash128.===
%% @end
%%-------------------------------------------------------------------------

-spec jenkins64_128(MessageRaw :: iodata()) ->
    non_neg_integer().

jenkins64_128(MessageRaw) ->
    jenkins64_128(MessageRaw, 0).

%%-------------------------------------------------------------------------
%% @doc
%% ===Bob Jenkins SpookyHashV2 Hash128.===
%% @end
%%-------------------------------------------------------------------------

-spec jenkins64_128(MessageRaw :: iodata(),
                    Seed :: non_neg_integer()) ->
    non_neg_integer().

jenkins64_128(MessageRaw, Seed)
    when is_integer(Seed), Seed >= 0 ->
    {Message, Size} = iodata_to_list(MessageRaw),
    SeedA = Seed band ?BITMASK_64,
    SeedB = (Seed bsr 64) band ?BITMASK_64,
    {HashA, HashB} = jenkins64_128(Message, Size, SeedA, SeedB),
    (HashB bsl 64) + HashA.

%%-------------------------------------------------------------------------
%% @doc
%% ===Bob Jenkins SpookyHashV2 Hash64.===
%% @end
%%-------------------------------------------------------------------------

-spec jenkins64_64(MessageRaw :: iodata()) ->
    non_neg_integer().

jenkins64_64(MessageRaw) ->
    jenkins64_64(MessageRaw, 0).

%%-------------------------------------------------------------------------
%% @doc
%% ===Bob Jenkins SpookyHashV2 Hash64.===
%% @end
%%-------------------------------------------------------------------------

-spec jenkins64_64(MessageRaw :: iodata(),
                   Seed :: non_neg_integer()) ->
    non_neg_integer().

jenkins64_64(MessageRaw, Seed)
    when is_integer(Seed), Seed >= 0 ->
    {Message, Size} = iodata_to_list(MessageRaw),
    SeedA = Seed band ?BITMASK_64,
    {HashA, _} = jenkins64_128(Message, Size, SeedA, SeedA),
    HashA.

%%-------------------------------------------------------------------------
%% @doc
%% ===Bob Jenkins SpookyHashV2 Hash32.===
%% @end
%%-------------------------------------------------------------------------

-spec jenkins64_32(MessageRaw :: iodata()) ->
    non_neg_integer().

jenkins64_32(MessageRaw) ->
    jenkins64_32(MessageRaw, 0).

%%-------------------------------------------------------------------------
%% @doc
%% ===Bob Jenkins SpookyHashV2 Hash32.===
%% @end
%%-------------------------------------------------------------------------

-spec jenkins64_32(MessageRaw :: iodata(),
                   Seed :: non_neg_integer()) ->
    non_neg_integer().

jenkins64_32(MessageRaw, Seed)
    when is_integer(Seed), Seed >= 0 ->
    {Message, Size} = iodata_to_list(MessageRaw),
    SeedA = Seed band ?BITMASK_32,
    {HashA, _} = jenkins64_128(Message, Size, SeedA, SeedA),
    HashA band ?BITMASK_32.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

jenkins_32(Message0, Size0, A0, B0, C0) when Size0 >= 12 ->
    {IncrA, Message1, Size1} = consume_32(Message0, Size0),
    {IncrB, Message2, Size2} = consume_32(Message1, Size1),
    {IncrC, MessageN, SizeN} = consume_32(Message2, Size2),
    {AN,
     BN,
     CN} = jenkins_mix(add_32(A0, IncrA), add_32(B0, IncrB), add_32(C0, IncrC)),
    jenkins_32(MessageN, SizeN, AN, BN, CN);
jenkins_32(Message0, Size0, A, B, C) when Size0 >= 1 ->
    {IncrA, Message1, Size1} = if
        Size0 >= 4 ->
            consume_32(Message0, Size0);
        true ->
            consume_32_part(Message0, Size0)
    end,
    {IncrB, MessageN, SizeN} = if
        Size1 >= 4 ->
            consume_32(Message1, Size1);
        true ->
            consume_32_part(Message1, Size1)
    end,
    {IncrC, [], 0} = consume_32_part(MessageN, SizeN),
    jenkins_final(add_32(A, IncrA), add_32(B, IncrB), add_32(C, IncrC));
jenkins_32([], 0, _, B, C) ->
    {C, B}.

% mix -- mix 3 32-bit values reversibly.
%
% This is reversible, so any information in (a,b,c) before mix() is
% still in (a,b,c) after mix().
%
% If four pairs of (a,b,c) inputs are run through mix(), or through
% mix() in reverse, there are at least 32 bits of the output that
% are sometimes the same for one pair and different for another pair.
% This was tested for:
% * pairs that differed by one bit, by two bits, in any combination
%   of top bits of (a,b,c), or in any combination of bottom bits of
%   (a,b,c).
% * "differ" is defined as +, -, ^, or ~^.  For + and -, the output
%   delta transformed to a Gray code (a^(a>>1)) so a string of 1's (as
%   is commonly produced by subtraction) look like a single 1-bit
%   difference.
% * the base values were pseudorandom, all zero but one bit set, or
%   all zero plus a counter that starts at zero.
%
% Some k values for my "a-=c; a^=rot(c,k); c+=b;" arrangement that
% satisfy this are
%     4  6  8 16 19  4
%     9 15  3 18 27 15
%    14  9  3  7 17  3
% Well, "9 15 3 18 27 15" didn't quite get 32 bits diffing
% for "differ" defined as + with a one-bit base and a two-bit delta.
% Data at http://burtleburtle.net/bob/hash/avalanche.html was used to
% choose the operations, constants, and arrangements of the variables.
%
% This does not achieve avalanche.  There are input bits of (a,b,c)
% that fail to affect some output bits of (a,b,c), especially of a.  The
% most thoroughly mixed value is c, but it doesn't really even achieve
% avalanche in c.
%
jenkins_mix(A0, B0, C0) ->
    A1 = subtract_32(A0, C0),A2 = A1 bxor rotate_32(C0,  4),C1 = add_32(C0, B0),
    B1 = subtract_32(B0, A2),B2 = B1 bxor rotate_32(A2,  6),A3 = add_32(A2, C1),
    C2 = subtract_32(C1, B2),C3 = C2 bxor rotate_32(B2,  8),B3 = add_32(B2, A3),
    A4 = subtract_32(A3, C3),A5 = A4 bxor rotate_32(C3, 16),C4 = add_32(C3, B3),
    B4 = subtract_32(B3, A5),B5 = B4 bxor rotate_32(A5, 19),AN = add_32(A5, C4),
    C5 = subtract_32(C4, B5),CN = C5 bxor rotate_32(B5,  4),BN = add_32(B5, AN),
    {AN, BN, CN}.

% final -- final mixing of 3 32-bit values (a,b,c) into c
%
% Pairs of (a,b,c) values differing in only a few bits will usually
% produce values of c that look totally different.  This was tested for
% * pairs that differed by one bit, by two bits, in any combination
%   of top bits of (a,b,c), or in any combination of bottom bits of
%   (a,b,c).
% * "differ" is defined as +, -, ^, or ~^.  For + and -, the output
%   delta transformed to a Gray code (a^(a>>1)) so a string of 1's (as
%   is commonly produced by subtraction) look like a single 1-bit
%   difference.
% * the base values were pseudorandom, all zero but one bit set, or
%   all zero plus a counter that starts at zero.
%
% These constants passed:
%  14 11 25 16 4 14 24
%  12 14 25 16 4 14 24
% and these came close:
%   4  8 15 26 3 22 24
%  10  8 15 26 3 22 24
%  11  8 15 26 3 22 24
%
jenkins_final(A0, B0, C0) ->
    C1 = C0 bxor B0, C2 = subtract_32(C1, rotate_32(B0, 14)),
    A1 = A0 bxor C2, A2 = subtract_32(A1, rotate_32(C2, 11)),
    B1 = B0 bxor A2, B2 = subtract_32(B1, rotate_32(A2, 25)),
    C3 = C2 bxor B2, C4 = subtract_32(C3, rotate_32(B2, 16)),
    A3 = A2 bxor C4, A4 = subtract_32(A3, rotate_32(C4,  4)),
    B3 = B2 bxor A4, BN = subtract_32(B3, rotate_32(A4, 14)),
    C5 = C4 bxor BN, CN = subtract_32(C5, rotate_32(BN, 24)),
    {CN, BN}.

jenkins64_128(Message, Size, SeedA, SeedB) ->
    HashC = ?JENKINS64_CONST,
    HashD = ?JENKINS64_CONST,
    jenkins64_128(Message, Size, SeedA, SeedB, HashC, HashD, Size).

jenkins64_128(Message0, Size0, A0, B0, C0, D0, TotalSize) when Size0 >= 32 ->
    {IncrC, Message1, Size1} = consume_64(Message0, Size0),
    {IncrD, Message2, Size2} = consume_64(Message1, Size1),
    {AN,
     BN,
     CN,
     DN} = jenkins64_short_mix(A0, B0, add_64(C0, IncrC), add_64(D0, IncrD)),
    {IncrA, Message3, Size3} = consume_64(Message2, Size2),
    {IncrB, MessageN, SizeN} = consume_64(Message3, Size3),
    jenkins64_128(MessageN, SizeN,
                  add_64(AN, IncrA), add_64(BN, IncrB), CN, DN, TotalSize);
jenkins64_128(Message0, Size0, A0, B0, C0, D0, TotalSize) when Size0 >= 16 ->
    {IncrC, Message1, Size1} = consume_64(Message0, Size0),
    {IncrD, MessageN, SizeN} = consume_64(Message1, Size1),
    {AN,
     BN,
     CN,
     DN} = jenkins64_short_mix(A0, B0, add_64(C0, IncrC), add_64(D0, IncrD)),
    jenkins64_128(MessageN, SizeN, AN, BN, CN, DN, TotalSize);
jenkins64_128(Message0, Size, AN, BN, C0, D0, TotalSize) when Size >= 12 ->
    [Byte00, Byte01, Byte02, Byte03, Byte04, Byte05, Byte06, Byte07,
     Byte08, Byte09, Byte10, Byte11 | Message1] = Message0,
    <<Value64:64/big-unsigned-integer,
      Value32:32/big-unsigned-integer>> =
        <<Byte00, Byte01, Byte02, Byte03, Byte04, Byte05, Byte06, Byte07,
          Byte08, Byte09, Byte10, Byte11>>,
    {ValueByte12, Message3} = if
        Size >= 13 ->
            [Byte12 | Message2] = Message1,
            {Byte12 bsl 32, Message2};
        true ->
            {0, Message1}
    end,
    {ValueByte13, Message5} = if
        Size >= 14 ->
            [Byte13 | Message4] = Message3,
            {Byte13 bsl 40, Message4};
        true ->
            {0, Message3}
    end,
    ValueByte14 = if
        Size == 15 ->
            [Byte14] = Message5,
            Byte14 bsl 48;
        true ->
            [] = Message5,
            0
    end,
    CN = add_64(C0, Value64),
    DN = add_64(D0, TotalSize bsl 56,
                ValueByte14, ValueByte13, ValueByte12, Value32),
    jenkins64_short_end(AN, BN, CN, DN);
jenkins64_128(Message0, Size, AN, BN, C0, D0, TotalSize) when Size >=  8 ->
    [Byte00, Byte01, Byte02, Byte03, Byte04, Byte05, Byte06, Byte07 |
     Message1] = Message0,
    <<Value64:64/big-unsigned-integer>> =
        <<Byte00, Byte01, Byte02, Byte03, Byte04, Byte05, Byte06, Byte07>>,
    {ValueByte08, Message3} = if
        Size >= 9 ->
            [Byte08 | Message2] = Message1,
            {Byte08, Message2};
        true ->
            {0, Message1}
    end,
    {ValueByte09, Message5} = if
        Size >= 10 ->
            [Byte09 | Message4] = Message3,
            {Byte09 bsl 8, Message4};
        true ->
            {0, Message3}
    end,
    ValueByte10 = if
        Size == 11 ->
            [Byte10] = Message5,
            Byte10 bsl 16;
        true ->
            [] = Message5,
            0
    end,
    CN = add_64(C0, Value64),
    DN = add_64(D0, TotalSize bsl 56, ValueByte10, ValueByte09, ValueByte08),
    jenkins64_short_end(AN, BN, CN, DN);
jenkins64_128(Message0, Size, AN, BN, C0, D0, TotalSize) when Size >=  4 ->
    [Byte00, Byte01, Byte02, Byte03 | Message1] = Message0,
    <<Value32:32/big-unsigned-integer>> = <<Byte00, Byte01, Byte02, Byte03>>,
    {ValueByte04, Message3} = if
        Size >= 5 ->
            [Byte04 | Message2] = Message1,
            {Byte04 bsl 32, Message2};
        true ->
            {0, Message1}
    end,
    {ValueByte05, Message5} = if
        Size >= 6 ->
            [Byte05 | Message4] = Message3,
            {Byte05 bsl 40, Message4};
        true ->
            {0, Message3}
    end,
    ValueByte06 = if
        Size == 7 ->
            [Byte06] = Message5,
            Byte06 bsl 48;
        true ->
            [] = Message5,
            0
    end,
    CN = add_64(C0, ValueByte06, ValueByte05, ValueByte04, Value32),
    DN = add_64(D0, TotalSize bsl 56),
    jenkins64_short_end(AN, BN, CN, DN);
jenkins64_128(Message0, Size, AN, BN, C0, D0, TotalSize) when Size >=  1 ->
    [Byte00 | Message1] = Message0,
    {ValueByte01, Message3} = if
        Size >= 2 ->
            [Byte01 | Message2] = Message1,
            {Byte01 bsl 8, Message2};
        true ->
            {0, Message1}
    end,
    ValueByte02 = if
        Size == 3 ->
            [Byte02] = Message3,
            Byte02 bsl 16;
        true ->
            [] = Message3,
            0
    end,
    CN = add_64(C0, ValueByte02, ValueByte01, Byte00),
    DN = add_64(D0, TotalSize bsl 56),
    jenkins64_short_end(AN, BN, CN, DN);
jenkins64_128([], 0, AN, BN, C0, D0, TotalSize) ->
    CN = add_64(C0, ?JENKINS64_CONST),
    DN = add_64(D0, TotalSize bsl 56, ?JENKINS64_CONST),
    jenkins64_short_end(AN, BN, CN, DN).

%
% The goal is for each bit of the input to expand into 128 bits of
%   apparent entropy before it is fully overwritten.
% n trials both set and cleared at least m bits of h0 h1 h2 h3
%   n: 2   m: 29
%   n: 3   m: 46
%   n: 4   m: 57
%   n: 5   m: 107
%   n: 6   m: 146
%   n: 7   m: 152
% when run forwards or backwards
% for all 1-bit and 2-bit diffs
% with diffs defined by either xor or subtraction
% with a base of all zeros plus a counter, or plus another bit, or random
%
jenkins64_short_mix(H0_0, H1_0, H2_0, H3_0)
    when is_integer(H0_0), is_integer(H1_0),
         is_integer(H2_0), is_integer(H3_0) ->
    H2_1 = add_64(rotate_64(H2_0, 50), H3_0),H0_1 = H0_0 bxor H2_1,
    H3_1 = add_64(rotate_64(H3_0, 52), H0_1),H1_1 = H1_0 bxor H3_1,
    H0_2 = add_64(rotate_64(H0_1, 30), H1_1),H2_2 = H2_1 bxor H0_2,
    H1_2 = add_64(rotate_64(H1_1, 41), H2_2),H3_2 = H3_1 bxor H1_2,
    H2_3 = add_64(rotate_64(H2_2, 54), H3_2),H0_3 = H0_2 bxor H2_3,
    H3_3 = add_64(rotate_64(H3_2, 48), H0_3),H1_3 = H1_2 bxor H3_3,
    H0_4 = add_64(rotate_64(H0_3, 38), H1_3),H2_4 = H2_3 bxor H0_4,
    H1_4 = add_64(rotate_64(H1_3, 37), H2_4),H3_4 = H3_3 bxor H1_4,
    H2_5 = add_64(rotate_64(H2_4, 62), H3_4),H0_5 = H0_4 bxor H2_5,
    H3_5 = add_64(rotate_64(H3_4, 34), H0_5),H1_5 = H1_4 bxor H3_5,
    H0_N = add_64(rotate_64(H0_5,  5), H1_5),H2_N = H2_5 bxor H0_N,
    H1_N = add_64(rotate_64(H1_5, 36), H2_N),H3_N = H3_5 bxor H1_N,
    {H0_N, H1_N, H2_N, H3_N}.

%
% Mix all 4 inputs together so that h0, h1 are a hash of them all.
%
% For two inputs differing in just the input bits
% Where "differ" means xor or subtraction
% And the base value is random, or a counting value starting at that bit
% The final result will have each bit of h0, h1 flip
% For every input bit,
% with probability 50 +- .3% (it is probably better than that)
% For every pair of input bits,
% with probability 50 +- .75% (the worst case is approximately that)
%
jenkins64_short_end(H0_0, H1_0, H2_0, H3_0)
    when is_integer(H0_0), is_integer(H1_0),
         is_integer(H2_0), is_integer(H3_0) ->
    H3_1 = H3_0 bxor H2_0,H2_1 = rotate_64(H2_0, 15),H3_2 = add_64(H3_1, H2_1),
    H0_1 = H0_0 bxor H3_2,H3_3 = rotate_64(H3_2, 52),H0_2 = add_64(H0_1, H3_3),
    H1_1 = H1_0 bxor H0_2,H0_3 = rotate_64(H0_2, 26),H1_2 = add_64(H1_1, H0_3),
    H2_2 = H2_1 bxor H1_2,H1_3 = rotate_64(H1_2, 51),H2_3 = add_64(H2_2, H1_3),
    H3_4 = H3_3 bxor H2_3,H2_4 = rotate_64(H2_3, 28),H3_5 = add_64(H3_4, H2_4),
    H0_4 = H0_3 bxor H3_5,H3_6 = rotate_64(H3_5,  9),H0_5 = add_64(H0_4, H3_6),
    H1_4 = H1_3 bxor H0_5,H0_6 = rotate_64(H0_5, 47),H1_5 = add_64(H1_4, H0_6),
    H2_5 = H2_4 bxor H1_5,H1_6 = rotate_64(H1_5, 54),H2_6 = add_64(H2_5, H1_6),
    H3_7 = H3_6 bxor H2_6,H2_N = rotate_64(H2_6, 32),H3_8 = add_64(H3_7, H2_N),
    H0_7 = H0_6 bxor H3_8,H3_N = rotate_64(H3_8, 25),H0_8 = add_64(H0_7, H3_N),
    H1_7 = H1_6 bxor H0_8,H0_N = rotate_64(H0_8, 63),H1_N = add_64(H1_7, H0_N),
    {H0_N, H1_N}.

-compile({inline,
          [{rotate_32,2},
           {rotate_64,2},
           {consume_32,2},
           {consume_32_part,2},
           {consume_64,2},
           {add_32,2},
           {add_32,3},
           {add_64,2},
           {add_64,3},
           {add_64,4},
           {add_64,5},
           {add_64,6},
           {subtract_32,2}]}).

% left rotate a 32-bit value by k bits
rotate_32(X, Bits)
    when is_integer(X), is_integer(Bits), Bits >= 0, Bits =< 32 ->
    ((X bsl Bits) band ?BITMASK_32) bor (X bsr (32 - Bits)).

% left rotate a 64-bit value by k bits
rotate_64(X, Bits)
    when is_integer(X), is_integer(Bits), Bits >= 0, Bits =< 64 ->
    ((X bsl Bits) band ?BITMASK_64) bor (X bsr (64 - Bits)).

consume_32([Byte00, Byte01, Byte02, Byte03 | Message], Size) ->
    <<Value:32/big-unsigned-integer>> = <<Byte00, Byte01, Byte02, Byte03>>,
    {Value, Message, Size - 4}.

consume_32_part([], 0) ->
    {0, [], 0};
consume_32_part([Byte00], 1) ->
    <<Value:32/big-unsigned-integer>> = <<Byte00, 0, 0, 0>>,
    {Value, [], 0};
consume_32_part([Byte00, Byte01], 2) ->
    <<Value:32/big-unsigned-integer>> = <<Byte00, Byte01, 0, 0>>,
    {Value, [], 0};
consume_32_part([Byte00, Byte01, Byte02], 3) ->
    <<Value:32/big-unsigned-integer>> = <<Byte00, Byte01, Byte02, 0>>,
    {Value, [], 0}.

consume_64([Byte00, Byte01, Byte02, Byte03, Byte04, Byte05, Byte06, Byte07 |
            Message], Size) ->
    <<Value:64/big-unsigned-integer>> =
        <<Byte00, Byte01, Byte02, Byte03, Byte04, Byte05, Byte06, Byte07>>,
    {Value, Message, Size - 8}.

add_32(X0, X1)
    when is_integer(X0), is_integer(X1) ->
    (X0 + X1) band ?BITMASK_32.

add_32(X0, X1, X2)
    when is_integer(X0), is_integer(X1), is_integer(X2) ->
    (X0 + X1 + X2) band ?BITMASK_32.

add_64(X0, X1)
    when is_integer(X0), is_integer(X1) ->
    (X0 + X1) band ?BITMASK_64.

add_64(X0, X1, X2)
    when is_integer(X0), is_integer(X1), is_integer(X2) ->
    (X0 + X1 + X2) band ?BITMASK_64.

add_64(X0, X1, X2, X3)
    when is_integer(X0), is_integer(X1), is_integer(X2), is_integer(X3) ->
    (X0 + X1 + X2 + X3) band ?BITMASK_64.

add_64(X0, X1, X2, X3, X4)
    when is_integer(X0), is_integer(X1), is_integer(X2),
         is_integer(X3), is_integer(X4) ->
    (X0 + X1 + X2 + X3 + X4) band ?BITMASK_64.

add_64(X0, X1, X2, X3, X4, X5)
    when is_integer(X0), is_integer(X1), is_integer(X2),
         is_integer(X3), is_integer(X4), is_integer(X5) ->
    (X0 + X1 + X2 + X3 + X4 + X5) band ?BITMASK_64.

subtract_32(X0, X1)
    when is_integer(X0), is_integer(X1) ->
    (X0 - X1) band ?BITMASK_32.

-spec iodata_to_list(IOData :: iodata()) ->
    {list(byte()), non_neg_integer()}.

iodata_to_list(IOData)
    when is_binary(IOData) ->
    {erlang:binary_to_list(IOData), byte_size(IOData)};
iodata_to_list(IOData)
    when is_list(IOData) ->
    iodata_to_list([], IOData, 0).

iodata_to_list(ListOut, [], Size) ->
    {lists:reverse(ListOut), Size};
iodata_to_list(ListOut, Binary, Size)
    when is_binary(Binary) ->
    iodata_to_list(lists:reverse(erlang:binary_to_list(Binary), ListOut),
                   [], Size + byte_size(Binary));
iodata_to_list(ListOut, [Binary | IODataIn], Size)
    when is_binary(Binary) ->
    iodata_to_list(lists:reverse(erlang:binary_to_list(Binary), ListOut),
                   IODataIn, Size + byte_size(Binary));
iodata_to_list(ListOut0, [List | IODataIn], Size0)
    when is_list(List) ->
    {ListOutN, SizeN} = iodata_to_list(ListOut0, List, Size0),
    iodata_to_list(lists:reverse(ListOutN), IODataIn, SizeN);
iodata_to_list(ListOut, [Byte | IOData], Size)
    when is_integer(Byte), Byte >= 0, Byte =< 255 ->
    iodata_to_list([Byte | ListOut], IOData, Size + 1).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-include("quickrand_test.hrl").

module_test_() ->
    {timeout, ?TEST_TIMEOUT, [
        {"jenkins tests", ?_assertOk(t_jenkins())},
        {"jenkins64 tests", ?_assertOk(t_jenkins64())}
    ]}.

t_jenkins() ->
    Message1List = "The quick brown fox jumps over the lazy dog",
    Message1Binary = erlang:list_to_binary(Message1List),
    Hash1_32 = 1995770187,
    Hash1_64 = 10406847816247085387,
    Hash1_32 = quickrand_hash:jenkins_32(Message1List),
    Hash1_32 = quickrand_hash:jenkins_32(Message1Binary),
    Hash1_64 = quickrand_hash:jenkins_64(Message1List),
    Hash1_64 = quickrand_hash:jenkins_64(Message1Binary),
    ok.

t_jenkins64() ->
    Message1List = "The quick brown fox jumps over the lazy dog",
    Message1Binary = erlang:list_to_binary(Message1List),
    Hash1_32 = 564871382,
    Hash1_64 = 15135784821221703894,
    Hash1_128 = 93844563773912622153168398518001025238,
    Hash1_32 = quickrand_hash:jenkins64_32(Message1List),
    Hash1_32 = quickrand_hash:jenkins64_32(Message1Binary),
    Hash1_64 = quickrand_hash:jenkins64_64(Message1List),
    Hash1_64 = quickrand_hash:jenkins64_64(Message1Binary),
    Hash1_128 = quickrand_hash:jenkins64_128(Message1List),
    Hash1_128 = quickrand_hash:jenkins64_128(Message1Binary),
    ok.

-endif.
