%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI IP Address Parsing==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2013-2021 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2013-2021 Michael Truog
%%% @version 2.0.3 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_ip_address).
-author('mjtruog at protonmail dot com').

%% external interface
-export([from_binary/1,
         from_string/1,
         to_binary/1,
         to_string/1]).

-type format_binary() ::
    <<_:8, _:_*8>>.
% IPv4 dotted decimal address (no octal or hex)
% IPv6 lowercase hex with colons
-type format_string() ::
    nonempty_list($0..$9 | $. | $a..$f | $: | $%).
-export_type([format_binary/0,
              format_string/0]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Parse a binary string representation.===
%% @end
%%-------------------------------------------------------------------------

-spec from_binary(format_binary()) ->
    inet:ip_address().

from_binary(BinaryIP) ->
    from_string(erlang:binary_to_list(BinaryIP)).

%%-------------------------------------------------------------------------
%% @doc
%% ===Parse a binary string representation.===
%% @end
%%-------------------------------------------------------------------------

-spec from_string(format_string()) ->
    inet:ip_address().

from_string(StringIP) ->
    {ok, IP} = inet:parse_strict_address(StringIP),
    IP.

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a binary string representation.===
%% IPv6 doesn't shorten a group of zeroes so more exact pattern matches
%% are possible in service names.
%% @end
%%-------------------------------------------------------------------------

-spec to_binary(inet:ip_address()) ->
    format_binary().

to_binary(IP) ->
    erlang:list_to_binary(to_string(IP)).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a list string representation.===
%% IPv6 doesn't shorten a group of zeroes so more exact pattern matches
%% are possible in service names.
%% @end
%%-------------------------------------------------------------------------

-spec to_string(inet:ip_address()) ->
    format_string().

to_string({I0, I1, I2, I3})
    when ((I0 bor I1 bor I2 bor I3) band (bnot 16#ff)) =:= 0 ->
    Delimiter = $.,
    S0 = int_to_dec_list([], I3),
    S1 = int_to_dec_list([Delimiter | S0], I2),
    SN = int_to_dec_list([Delimiter | S1], I1),
    int_to_dec_list([Delimiter | SN], I0);
to_string({I0, I1, I2, I3, I4, I5, I6, I7})
    when ((I0 bor I1 bor I2 bor I3 bor
           I4 bor I5 bor I6 bor I7) band (bnot 16#ffff)) =:= 0 ->
    Delimiter = $:,
    S0 = int_to_hex_list([], I7),
    S1 = int_to_hex_list([Delimiter | S0], I6),
    S2 = int_to_hex_list([Delimiter | S1], I5),
    S3 = int_to_hex_list([Delimiter | S2], I4),
    S4 = int_to_hex_list([Delimiter | S3], I3),
    S5 = int_to_hex_list([Delimiter | S4], I2),
    SN = int_to_hex_list([Delimiter | S5], I1),
    int_to_hex_list([Delimiter | SN], I0).

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

int_to_dec_list(L, I)
    when I < 10 ->
    [int_to_dec(I) | L];
int_to_dec_list(L, I) ->
    int_to_dec_list([int_to_dec(I rem 10) | L], I div 10).

int_to_hex_list(L, I)
    when I < 16 ->
    [int_to_hex(I) | L];
int_to_hex_list(L, I) ->
    int_to_hex_list([int_to_hex(I rem 16) | L], I div 16).

-compile({inline,
          [{int_to_dec,1},
           {int_to_hex,1}]}).

int_to_dec(I) when 0 =< I, I =< 9 ->
    I + $0.

int_to_hex(I) when 0 =< I, I =< 9 ->
    I + $0;
int_to_hex(I) when 10 =< I, I =< 15 ->
    (I - 10) + $a.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-include("cloudi_core_i_test.hrl").

module_test_() ->
    {timeout, ?TEST_TIMEOUT, [
        {"from_binary tests", ?_assertOk(t_from_binary())},
        {"from_string tests", ?_assertOk(t_from_string())},
        {"to_binary tests", ?_assertOk(t_to_binary())},
        {"to_string tests", ?_assertOk(t_to_string())}
    ]}.

t_from_binary() ->
    {0,0,0,0,0,0,0,0} = from_binary(<<"::">>), % any
    {0,0,0,0,0,0,0,1} = from_binary(<<"::1">>), % localhost
    {8194,0,0,4660,43981,
     65535,49320,257} = from_binary(<<"2002::1234:abcd:ffff:c0a8:101">>),
    {8194,0,0,4660,43981,
     65535,49320,257} = from_binary(<<"2002:0:0:1234:abcd:ffff:c0a8:101">>),
    ok.

t_from_string() ->
    {0,0,0,0,0,0,0,0} = from_string("::"), % any
    {0,0,0,0,0,0,0,1} = from_string("::1"), % localhost
    {8194,0,0,4660,43981,
     65535,49320,257} = from_string("2002::1234:abcd:ffff:c0a8:101"),
    {8194,0,0,4660,43981,
     65535,49320,257} = from_string("2002:0:0:1234:abcd:ffff:c0a8:101"),
    ok.

t_to_binary() ->
    <<"0:0:0:0:0:0:0:0">> = to_binary({0,0,0,0,0,0,0,0}),
    <<"0:0:0:0:0:0:0:1">> = to_binary({0,0,0,0,0,0,0,1}),
    <<"2002:0:0:1234:abcd:ffff:c0a8:101">> = to_binary({8194,0,0,4660,43981,
                                                        65535,49320,257}),
    ok.

t_to_string() ->
    "0:0:0:0:0:0:0:0" = to_string({0,0,0,0,0,0,0,0}),
    "0:0:0:0:0:0:0:1" = to_string({0,0,0,0,0,0,0,1}),
    "2002:0:0:1234:abcd:ffff:c0a8:101" = to_string({8194,0,0,4660,43981,
                                                    65535,49320,257}),
    ok.

-endif.
