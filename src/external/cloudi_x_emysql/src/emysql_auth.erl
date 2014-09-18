%% Copyright (c) 2009-2014
%% Bill Warnecke <bill@rupture.com>
%% Jacob Vorreuter <jacob.vorreuter@gmail.com>
%% Henning Diedrich <hd2010@eonblast.com>
%% Jesper Louis Andersen <jesper.louis.andersen@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
%% 
%% Handle authentication in the protocol
%%
%% @private
-module(emysql_auth).

-export([handshake/3]).

-include("emysql.hrl").
-include("crypto_compat.hrl").

%% API
%% -----------------------------------------------------------------

%% handshake/3 runs the low-level handshake code upon connection initiation
%% @private
handshake(Sock, User, Password) ->
    {Packet, Unparsed} = emysql_tcp:recv_packet(Sock, emysql_app:default_timeout(), <<>>),
    case parse_greeting(Packet) of
        {ok, #greeting { seq_num = SeqNum } = Greeting} ->
            Auth = auth(Sock, User, Password, Greeting#greeting { seq_num = SeqNum + 1 }),
            check_handshake_auth(Auth, Greeting);
        {error, wrong_parse} -> 
            {#error_packet{ code = Code, msg = Msg},_, _Rest} =
                emysql_tcp:response(Sock, emysql_app:default_timeout(), Packet, Unparsed),
            {error, {Code, Msg}};
        {greeting_failed, What} ->
            {error, {greeting_failed, What}}
    end.

%% Internal functions
%% -----------------------------------------------------------------

check_handshake_auth(#ok_packet{}, Greeting) -> {ok, Greeting};
check_handshake_auth(#error_packet{} = Err, _Greeting) -> {error, {auth_fail, Err}}.

%% build_greeting/3 eats through the greeting string systematically
%%   We build the greeting by traversing several stages. Each stage decodes yet
%%   another crappy 0-terminated ascii string.
build_greeting(stage1, D, G) ->
    {SV, <<ThreadId:32/little, Rest/binary>>} = asciiz(D),
    build_greeting(stage2, Rest, G#greeting { server_version = SV, thread_id = ThreadId });
build_greeting(stage2, D, G) ->
    {Salt, <<ServerCaps:16/little,
             ServerLanguage:8/little,
             ServerStatus:16/little,
             ServerCapsHigh:16/little,
             ScrambleLength:8/little,_:10/binary-unit:8,
             Rest/binary>>} = asciiz(D),
    Salt2Length = case ScrambleLength of
                      0 -> 13;
                      K -> K - 8
    end,
    build_greeting({stage3, Salt2Length}, Rest,
                   G#greeting {
                       salt1 = Salt,
                       caps = ServerCaps,
                       caps_high = ServerCapsHigh,
                       language = ServerLanguage,
                       status = ServerStatus });
build_greeting({stage3, SaltLength}, D, G) ->
    <<Salt2_0:SaltLength/binary-unit:8, Plugin/binary>> = D,
    {Salt2, <<>>} = asciiz(Salt2_0),
    G#greeting{
        salt2 = Salt2,
        plugin = Plugin
    }.

%% parse_greeting/1 figures out what the greeting packet is all about
parse_greeting(#packet { data = <<255, _/binary>> }) ->
    {error, wrong_parse};
parse_greeting(#packet { data = <<ProtocolVersion:8/integer, Rest1/binary>>, seq_num = SeqNo }) ->
    G = build_greeting(stage1, Rest1, #greeting { protocol_version = ProtocolVersion,
                                                  seq_num = SeqNo }),
    {ok, G};
parse_greeting(#packet { data = What }) ->
    {greeting_failed, What}.
   

%% password_type/2 discriminates the kind of password we want
password_type(Password, ?MYSQL_OLD_PASSWORD) when is_list(Password); is_binary(Password) -> old;
password_type(Password, _) when is_list(Password); is_binary(Password) -> new;
password_type(_, _) -> empty.

%% capabilities/0 formats a list of capabilities for the wire
capabilities(Cs) ->
    lists:foldl(fun erlang:'bor'/2, 0, Cs).

%% auth_packet/3 constructs a normal authentication packet
auth_packet(User, Password,
            #greeting { salt1 = Salt1,
                        salt2 = Salt2,
                        plugin = Plugin }) ->
    ScrambleBuff = case password_type(Password, Plugin) of
    	old -> password_old(Password, <<Salt1/binary, Salt2/binary>>);
    	new -> password_new(Password, <<Salt1/binary, Salt2/binary>>);
    	empty -> <<>>
    end,

    Caps = capabilities([
    	?LONG_PASSWORD , ?CLIENT_LOCAL_FILE, ?LONG_FLAG, ?TRANSACTIONS,
	    ?CLIENT_MULTI_STATEMENTS, ?CLIENT_MULTI_RESULTS, ?PROTOCOL_41, ?SECURE_CONNECTION
    ]),
    Maxsize = ?MAXPACKETBYTES,
    UserB = unicode:characters_to_binary(User),
    PasswordL = size(ScrambleBuff),
    <<Caps:32/little, Maxsize:32/little, 8:8, 0:23/integer-unit:8, UserB/binary, 0:8,
      PasswordL:8, ScrambleBuff/binary>>.

%% auth_packet_old/2 constructs an old-style auth-packet
auth_packet_old(Password, #greeting { salt1 = Salt1 }) ->
    AuthOld = password_old(Password, Salt1),
    <<AuthOld/binary, 0:8>>.

%% auth/4 handles authentication inside the system.
auth(Sock, User, Password, #greeting { seq_num = SeqNum } = Greeting) ->
    Packet = auth_packet(User, Password, Greeting),
    case emysql_tcp:send_and_recv_packet(Sock, Packet, SeqNum) of
        #eof_packet{seq_num = EofSeqNum} ->
            PacketOld = auth_packet_old(Password, Greeting),
            emysql_tcp:send_and_recv_packet(Sock, PacketOld, EofSeqNum+1);
        Result ->
            Result
    end.

password_new([], _Salt) -> <<>>;
password_new(Password, Salt) ->
    Stage1 = ?HASH_SHA(Password),
    Stage2 = ?HASH_SHA(Stage1),
    Res = ?HASH_FINAL(
        ?HASH_UPDATE(
            ?HASH_UPDATE(?HASH_INIT(), Salt),
            Stage2
        )
    ),
    bxor_binary(Res, Stage1).

password_old(Password, Salt) ->
    {P1, P2} = hash(Password),
    {S1, S2} = hash(Salt),
    Seed1 = P1 bxor S1,
    Seed2 = P2 bxor S2,
    List = rnd(9, Seed1, Seed2),
    {L, [Extra]} = lists:split(8, List),
    list_to_binary(lists:map(fun (E) -> E bxor (Extra - 64) end, L)).

bxor_binary(B1, B2) ->
    list_to_binary([E1 bxor E2 || {E1, E2} <- lists:zip(binary_to_list(B1), binary_to_list(B2))]).

rnd(N, Seed1, Seed2) ->
    Mod = (1 bsl 30) - 1,
    rnd(N, [], Seed1 rem Mod, Seed2 rem Mod).
rnd(0, List, _, _) ->
    lists:reverse(List);
rnd(N, List, Seed1, Seed2) ->
    Mod = (1 bsl 30) - 1,
    NSeed1 = (Seed1 * 3 + Seed2) rem Mod,
    NSeed2 = (NSeed1 + Seed2 + 33) rem Mod,
    Float = (float(NSeed1) / float(Mod))*31,
    Val = trunc(Float)+64,
    rnd(N - 1, [Val | List], NSeed1, NSeed2).

hash(B) when is_binary(B) -> hash(binary_to_list(B));
hash(S) -> hash(S, 1345345333, 305419889, 7).
hash([C | S], N1, N2, Add) ->
    N1_1 = N1 bxor (((N1 band 63) + Add) * C + N1 * 256),
    N2_1 = N2 + ((N2 * 256) bxor N1_1),
    Add_1 = Add + C,
    hash(S, N1_1, N2_1, Add_1);
hash([], N1, N2, _Add) ->
    Mask = (1 bsl 31) - 1,
    {N1 band Mask , N2 band Mask}.

asciiz(Data) when is_binary(Data) ->
    [S, R] = binary:split(Data, <<0>>),
    {S, R}.
