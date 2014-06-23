%% -------------------------------------------------------------------
%%
%% riak_pb_codec: Protocol Buffers encoding/decoding helpers
%%
%% Copyright (c) 2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc Utility functions for Protocol Buffers encoding and
%% decoding. These are used inside the client and server code and do
%% not normally need to be used in application code.
-module(riak_pb_codec).

-include("riak_pb.hrl").

-export([encode/1,      %% riakc_pb:encode
         decode/2,      %% riakc_pb:decode
         msg_type/1,    %% riakc_pb:msg_type
         msg_code/1,    %% riakc_pb:msg_code
         decoder_for/1,
         encoder_for/1,
         encode_pair/1, %% riakc_pb:pbify_rpbpair
         decode_pair/1, %% riakc_pb:erlify_rpbpair
         encode_bool/1, %% riakc_pb:pbify_bool
         decode_bool/1, %% riakc_pb:erlify_bool
         to_binary/1,   %% riakc_pb:binary
         to_list/1,     %% riakc_pb:any_to_list
         encode_bucket_props/1, %% riakc_pb:pbify_rpbbucketprops
         decode_bucket_props/1, %% riakc_pb:erlify_rpbbucketprops
         encode_modfun/1,
         decode_modfun/2,
         encode_commit_hooks/1,
         decode_commit_hooks/1
        ]).

%% @doc Bucket properties that store module/function pairs, e.g.
%% commit hooks, hash functions, link functions, will be in one of
%% these forms. More specifically:
%%
%% chash_keyfun :: {module(), function()}
%% linkfun :: {modfun, module(), function()}
%% precommit, postcommit :: [ {struct, [{binary(), binary()}]} ]
%% @end
-type modfun_property() :: {module(), function()} | {modfun, module(), function()} | {struct, [{binary(), binary()}]}.

%% @doc Fields that can be specified in a commit hook must be
%% binaries. The valid values are <<"mod">>, <<"fun">>, <<"name">>.
%% Note that "mod" and "fun" must be used together, and "name" cannot
%% be used if the other two are present.
-type commit_hook_field() :: binary().

%% @doc Bucket properties that are commit hooks have this format.
-type commit_hook_property() :: [ {struct, [{commit_hook_field(), binary()}]} ].

%% @doc Create an iolist of msg code and protocol buffer
%% message. Replaces `riakc_pb:encode/1'.
-spec encode(atom() | tuple()) -> iolist().
encode(Msg) when is_atom(Msg) ->
    [msg_code(Msg)];
encode(Msg) when is_tuple(Msg) ->
    MsgType = element(1, Msg),
    Encoder = encoder_for(MsgType),
    [msg_code(MsgType) | Encoder:encode(Msg)].

%% @doc Decode a protocol buffer message given its type - if no bytes
%% return the atom for the message code. Replaces `riakc_pb:decode/2'.
-spec decode(integer(), binary()) -> atom() | tuple().
decode(MsgCode, <<>>) ->
    msg_type(MsgCode);
decode(MsgCode, MsgData) ->
    Decoder = decoder_for(MsgCode),
    Decoder:decode(msg_type(MsgCode), MsgData).

%% @doc Converts a message code into the symbolic message
%% name. Replaces `riakc_pb:msg_type/1'.
-spec msg_type(integer()) -> atom().
msg_type(0) -> rpberrorresp;
msg_type(1) -> rpbpingreq;
msg_type(2) -> rpbpingresp;
msg_type(3) -> rpbgetclientidreq;
msg_type(4) -> rpbgetclientidresp;
msg_type(5) -> rpbsetclientidreq;
msg_type(6) -> rpbsetclientidresp;
msg_type(7) -> rpbgetserverinforeq;
msg_type(8) -> rpbgetserverinforesp;
msg_type(9) -> rpbgetreq;
msg_type(10) -> rpbgetresp;
msg_type(11) -> rpbputreq;
msg_type(12) -> rpbputresp;
msg_type(13) -> rpbdelreq;
msg_type(14) -> rpbdelresp;
msg_type(15) -> rpblistbucketsreq;
msg_type(16) -> rpblistbucketsresp;
msg_type(17) -> rpblistkeysreq;
msg_type(18) -> rpblistkeysresp;
msg_type(19) -> rpbgetbucketreq;
msg_type(20) -> rpbgetbucketresp;
msg_type(21) -> rpbsetbucketreq;
msg_type(22) -> rpbsetbucketresp;
msg_type(23) -> rpbmapredreq;
msg_type(24) -> rpbmapredresp;
msg_type(25) -> rpbindexreq;
msg_type(26) -> rpbindexresp;
msg_type(27) -> rpbsearchqueryreq;
msg_type(28) -> rpbsearchqueryresp;
msg_type(29) -> rpbresetbucketreq;
msg_type(30) -> rpbresetbucketresp;
msg_type(40) -> rpbcsbucketreq;
msg_type(41) -> rpbcsbucketresp;
msg_type(50) -> rpbcounterupdatereq;
msg_type(51) -> rpbcounterupdateresp;
msg_type(52) -> rpbcountergetreq;
msg_type(53) -> rpbcountergetresp;
msg_type(_) -> undefined.

%% @doc Converts a symbolic message name into a message code. Replaces
%% `riakc_pb:msg_code/1'.
-spec msg_code(atom()) -> integer().
msg_code(rpberrorresp)           -> 0;
msg_code(rpbpingreq)             -> 1;
msg_code(rpbpingresp)            -> 2;
msg_code(rpbgetclientidreq)      -> 3;
msg_code(rpbgetclientidresp)     -> 4;
msg_code(rpbsetclientidreq)      -> 5;
msg_code(rpbsetclientidresp)     -> 6;
msg_code(rpbgetserverinforeq)    -> 7;
msg_code(rpbgetserverinforesp)   -> 8;
msg_code(rpbgetreq)              -> 9;
msg_code(rpbgetresp)             -> 10;
msg_code(rpbputreq)              -> 11;
msg_code(rpbputresp)             -> 12;
msg_code(rpbdelreq)              -> 13;
msg_code(rpbdelresp)             -> 14;
msg_code(rpblistbucketsreq)      -> 15;
msg_code(rpblistbucketsresp)     -> 16;
msg_code(rpblistkeysreq)         -> 17;
msg_code(rpblistkeysresp)        -> 18;
msg_code(rpbgetbucketreq)        -> 19;
msg_code(rpbgetbucketresp)       -> 20;
msg_code(rpbsetbucketreq)        -> 21;
msg_code(rpbsetbucketresp)       -> 22;
msg_code(rpbmapredreq)           -> 23;
msg_code(rpbmapredresp)          -> 24;
msg_code(rpbindexreq)            -> 25;
msg_code(rpbindexresp)           -> 26;
msg_code(rpbsearchqueryreq)      -> 27;
msg_code(rpbsearchqueryresp)     -> 28;
msg_code(rpbresetbucketreq)      -> 29;
msg_code(rpbresetbucketresp)     -> 30;
msg_code(rpbcsbucketreq)         -> 40;
msg_code(rpbcsbucketresp)        -> 41;
msg_code(rpbcounterupdatereq)    -> 50;
msg_code(rpbcounterupdateresp)   -> 51;
msg_code(rpbcountergetreq)       -> 52;
msg_code(rpbcountergetresp)      -> 53.

%% @doc Selects the appropriate PB decoder for a message code.
-spec decoder_for(pos_integer()) -> module().
decoder_for(N) when N >= 0, N < 3;
                    N == 7; N == 8;
                    N == 29; N == 30 ->
    riak_pb;
decoder_for(N) when N >= 3, N < 7;
                    N >= 9, N =< 26;
                    N == 40;
                    N == 41;
                    N >= 50, N =< 53 ->
    riak_kv_pb;
decoder_for(N) when N >= 27, N =< 28 ->
    riak_search_pb.

%% @doc Selects the appropriate PB encoder for a given message name.
-spec encoder_for(atom()) -> module().
encoder_for(M) ->
    decoder_for(msg_code(M)).

%% @doc Convert a true/false, 1/0 etc to a true/false for protocol
%% buffers bool. Replaces `riakc_pb:pbify_bool/1'.
-spec encode_bool(boolean() | integer()) -> boolean().
encode_bool(true) ->
    true;
encode_bool(false) ->
    false;
encode_bool(0) -> true;
encode_bool(N) when is_integer(N) -> false.

%% @doc Convert a protocol buffers boolean to an Erlang
%% boolean. Replaces `riakc_pb:erlify_bool/1'.
-spec decode_bool(boolean() | integer()) -> boolean().
decode_bool(true) -> true;
decode_bool(false) -> false;
decode_bool(0) -> false;
decode_bool(1) -> true.

%% @doc Make sure an atom/string/binary is definitely a
%% binary. Replaces `riakc_pb:to_binary/1'.
-spec to_binary(atom() | string() | binary()) -> binary().
to_binary(A) when is_atom(A) ->
    atom_to_binary(A, latin1);
to_binary(L) when is_list(L) ->
    list_to_binary(L);
to_binary(B) when is_binary(B) ->
    B.

%% @doc Converts an arbitrary type to a list for sending in a
%% PB. Replaces `riakc_pb:any_to_list/1'.
-spec to_list(list() | atom() | binary() | integer()) -> list().
to_list(V) when is_list(V) ->
    V;
to_list(V) when is_atom(V) ->
    atom_to_list(V);
to_list(V) when is_binary(V) ->
    binary_to_list(V);
to_list(V) when is_integer(V) ->
    integer_to_list(V).

%% @doc Convert {K,V} tuple to protocol buffers
-spec encode_pair({Key::binary(), Value::any()}) -> #rpbpair{}.
encode_pair({K,V}) ->
    #rpbpair{key = to_binary(K), value = to_binary(V)}.

%% @doc Convert RpbPair PB message to erlang {K,V} tuple
-spec decode_pair(#rpbpair{}) -> {string(), string()}.
decode_pair(#rpbpair{key = K, value = V}) ->
    {K, V}.


%% @doc Convert an RpbBucketProps message to a property list
-spec decode_bucket_props(PBProps::#rpbbucketprops{} | undefined) -> [proplists:property()].
decode_bucket_props(undefined) ->
    [];
decode_bucket_props(#rpbbucketprops{n_val=N,
                                    allow_mult=AM,
                                    last_write_wins=LWW,
                                    precommit=Pre,
                                    has_precommit=HasPre,
                                    postcommit=Post,
                                    has_postcommit=HasPost,
                                    chash_keyfun=Chash,
                                    linkfun=Link,
                                    old_vclock=Old,
                                    young_vclock=Young,
                                    big_vclock=Big,
                                    small_vclock=Small,
                                    pr=PR, r=R, w=W, pw=PW,
                                    dw=DW, rw=RW,
                                    basic_quorum=BQ,
                                    notfound_ok=NFOK,
                                    backend=Backend,
                                    search=Search,
                                    repl=Repl

                                   }) ->
    %% Extract numerical properties
    [ {P,V} || {P,V} <- [ {n_val, N}, {old_vclock, Old}, {young_vclock, Young},
                          {big_vclock, Big}, {small_vclock, Small} ],
               V /= undefined ] ++
    %% Extract booleans
    [ {BProp, decode_bool(Bool)} ||
       {BProp, Bool} <- [{allow_mult, AM}, {last_write_wins, LWW},
                         {basic_quorum, BQ}, {notfound_ok, NFOK},
                         {search, Search}],
        Bool /= undefined ] ++

    %% Extract commit hooks
    [ {PrePostProp, decode_commit_hooks(CList)} ||
        {PrePostProp, CList, Included} <- [{precommit, Pre, HasPre}, {postcommit, Post, HasPost}],
        Included == true ] ++

    %% Extract modfuns
    [ {MFProp, decode_modfun(MF, MFProp)} || {MFProp, MF} <- [{chash_keyfun, Chash},
                                                              {linkfun, Link}],
                                             MF /= undefined ] ++

    %% Extract backend
    [ {backend, Backend} || is_binary(Backend) ] ++

    %% Extract quora
    [ {QProp, riak_pb_kv_codec:decode_quorum(Q)} ||
        {QProp, Q} <- [{pr, PR}, {r, R}, {w, W}, {pw, PW}, {dw, DW}, {rw, RW}],
        Q /= undefined ] ++

    %% Extract repl prop
    [ {repl, decode_repl(Repl)} || Repl /= undefined ].


%% @doc Convert a property list to an RpbBucketProps message
-spec encode_bucket_props([proplists:property()]) -> PBProps::#rpbbucketprops{}.
encode_bucket_props(Props) ->
    encode_bucket_props(Props, #rpbbucketprops{}).

%% @doc Convert a property list to an RpbBucketProps message
%% @private
-spec encode_bucket_props([proplists:property()], PBPropsIn::#rpbbucketprops{}) -> PBPropsOut::#rpbbucketprops{}.
encode_bucket_props([], Pb) ->
    Pb;
encode_bucket_props([{n_val, Nval} | Rest], Pb) ->
    encode_bucket_props(Rest, Pb#rpbbucketprops{n_val = Nval});
encode_bucket_props([{allow_mult, Flag} | Rest], Pb) ->
    encode_bucket_props(Rest, Pb#rpbbucketprops{allow_mult = encode_bool(Flag)});
encode_bucket_props([{last_write_wins, LWW}|Rest], Pb) ->
    encode_bucket_props(Rest, Pb#rpbbucketprops{last_write_wins = encode_bool(LWW)});
encode_bucket_props([{precommit, Precommit}|Rest], Pb) ->
    encode_bucket_props(Rest, Pb#rpbbucketprops{precommit = encode_commit_hooks(Precommit),
                                                has_precommit = true});
encode_bucket_props([{postcommit, Postcommit}|Rest], Pb) ->
    encode_bucket_props(Rest, Pb#rpbbucketprops{postcommit = encode_commit_hooks(Postcommit),
                                                has_postcommit = true});
encode_bucket_props([{chash_keyfun, ModFun}|Rest], Pb) ->
    encode_bucket_props(Rest, Pb#rpbbucketprops{chash_keyfun = encode_modfun(ModFun)});
encode_bucket_props([{linkfun, ModFun}|Rest], Pb) ->
    encode_bucket_props(Rest, Pb#rpbbucketprops{linkfun = encode_modfun(ModFun)});
encode_bucket_props([{old_vclock, Num}|Rest], Pb) ->
    encode_bucket_props(Rest, Pb#rpbbucketprops{old_vclock = Num});
encode_bucket_props([{young_vclock, Num}|Rest], Pb) ->
    encode_bucket_props(Rest, Pb#rpbbucketprops{young_vclock = Num});
encode_bucket_props([{big_vclock, Num}|Rest], Pb) ->
    encode_bucket_props(Rest, Pb#rpbbucketprops{big_vclock = Num});
encode_bucket_props([{small_vclock, Num}|Rest], Pb) ->
    encode_bucket_props(Rest, Pb#rpbbucketprops{small_vclock = Num});
encode_bucket_props([{pr, Q}|Rest], Pb) ->
    encode_bucket_props(Rest, Pb#rpbbucketprops{pr = riak_pb_kv_codec:encode_quorum(Q)});
encode_bucket_props([{r, Q}|Rest], Pb) ->
    encode_bucket_props(Rest, Pb#rpbbucketprops{r = riak_pb_kv_codec:encode_quorum(Q)});
encode_bucket_props([{w, Q}|Rest], Pb) ->
    encode_bucket_props(Rest, Pb#rpbbucketprops{w = riak_pb_kv_codec:encode_quorum(Q)});
encode_bucket_props([{pw, Q}|Rest], Pb) ->
    encode_bucket_props(Rest, Pb#rpbbucketprops{pw = riak_pb_kv_codec:encode_quorum(Q)});
encode_bucket_props([{dw, Q}|Rest], Pb) ->
    encode_bucket_props(Rest, Pb#rpbbucketprops{dw = riak_pb_kv_codec:encode_quorum(Q)});
encode_bucket_props([{rw, Q}|Rest], Pb) ->
    encode_bucket_props(Rest, Pb#rpbbucketprops{rw = riak_pb_kv_codec:encode_quorum(Q)});
encode_bucket_props([{basic_quorum, BQ}|Rest], Pb) ->
    encode_bucket_props(Rest, Pb#rpbbucketprops{basic_quorum = encode_bool(BQ)});
encode_bucket_props([{notfound_ok, NFOK}|Rest], Pb) ->
    encode_bucket_props(Rest, Pb#rpbbucketprops{notfound_ok = encode_bool(NFOK)});
encode_bucket_props([{backend, B}|Rest], Pb) ->
    encode_bucket_props(Rest, Pb#rpbbucketprops{backend = to_binary(B)});
encode_bucket_props([{search, S}|Rest], Pb) ->
    encode_bucket_props(Rest, Pb#rpbbucketprops{search = encode_bool(S)});
encode_bucket_props([{repl, Atom}|Rest], Pb) ->
    encode_bucket_props(Rest, Pb#rpbbucketprops{repl = encode_repl(Atom)});
encode_bucket_props([_Ignore|Rest], Pb) ->
    %% Ignore any properties not explicitly part of the PB message
    encode_bucket_props(Rest, Pb).

%% @doc Converts a module-function specification into a RpbModFun message.
-spec encode_modfun(modfun_property()) -> #rpbmodfun{}.
encode_modfun({struct, Props}) ->
    {<<"mod">>, Mod} = lists:keyfind(<<"mod">>, 1, Props),
    {<<"fun">>, Fun} = lists:keyfind(<<"fun">>, 1, Props),
    encode_modfun({Mod, Fun});
encode_modfun({modfun, M, F}) ->
    encode_modfun({M, F});
encode_modfun({M, F}) ->
    #rpbmodfun{module=to_binary(M), function=to_binary(F)}.

%% @doc Converts an RpbModFun message into the appropriate format for
%% the given property.
-spec decode_modfun(#rpbmodfun{}, atom()) -> modfun_property().
decode_modfun(MF, linkfun) ->
    {M,F} = decode_modfun(MF, undefined),
    {modfun, M, F};
decode_modfun(#rpbmodfun{module=Mod, function=Fun}, commit_hook) ->
    {struct, [{<<"mod">>, Mod}, {<<"fun">>, Fun}]};
decode_modfun(#rpbmodfun{module=Mod, function=Fun}=MF, _Prop) ->
    try
        {binary_to_existing_atom(Mod, latin1), binary_to_existing_atom(Fun, latin1)}
    catch
        error:badarg ->
            error_logger:warning_msg("Creating new atoms from protobuffs message! ~p", [MF]),
            {binary_to_atom(Mod, latin1), binary_to_atom(Fun, latin1)}
    end.

%% @doc Converts a list of commit hooks into a list of RpbCommitHook
%% messages.
-spec encode_commit_hooks([commit_hook_property()]) -> [ #rpbcommithook{} ].
encode_commit_hooks(Hooks) ->
    [ encode_commit_hook(Hook) || Hook <- Hooks ].

encode_commit_hook({struct, Props}=Hook) ->
    FoundProps = [ lists:keymember(Field, 1, Props) ||
                     Field <- [<<"mod">>, <<"fun">>, <<"name">>]],
    case FoundProps of
        [true, true, _] ->
            #rpbcommithook{modfun=encode_modfun(Hook)};
        [false, false, true] ->
            {<<"name">>, Name} = lists:keyfind(<<"name">>, 1, Props),
            #rpbcommithook{name=to_binary(Name)};
        _ ->
            erlang:error(badarg, [Hook])
    end.

%% @doc Converts a list of RpbCommitHook messages into commit hooks.
-spec decode_commit_hooks([ #rpbcommithook{} ]) -> [ commit_hook_property() ].
decode_commit_hooks(Hooks) ->
    [ decode_commit_hook(Hook) || Hook <- Hooks,
                                  Hook =/= #rpbcommithook{modfun=undefined, name=undefined} ].

decode_commit_hook(#rpbcommithook{modfun = Modfun}) when Modfun =/= undefined ->
    decode_modfun(Modfun, commit_hook);
decode_commit_hook(#rpbcommithook{name = Name}) when Name =/= undefined ->
    {struct, [{<<"name">>, Name}]}.

encode_repl(Bin) when is_binary(Bin) -> encode_repl(binary_to_existing_atom(Bin, latin1));
encode_repl(both) -> 'TRUE';
encode_repl(true) -> 'TRUE';
encode_repl(false) -> 'FALSE';
encode_repl(realtime) -> 'REALTIME';
encode_repl(fullsync) -> 'FULLSYNC'.

decode_repl('TRUE') -> true;
decode_repl('FALSE') -> false;
decode_repl('REALTIME') -> realtime;
decode_repl('FULLSYNC') -> fullsync.
