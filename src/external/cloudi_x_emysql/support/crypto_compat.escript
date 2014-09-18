#!/usr/bin/env escript
%% vim: ts=4 sw=4 et ft=erlang

%% The purpose of this script is to analyze the capabilities of the ERTS to
%% determine if it supports crypto:hash and its sibling functions. Erlang from
%% R15B to R16B01 underwent a handful of non-backwards compatible changes to
%% the crypto module, and in R16B, started emitting errors about the use of
%% crypto:sha. This script will adjust accordingly to support older versions of
%% Erlang without headache.
%%
%% Note: This should be run *before* compiling!

main([]) ->
    crypto:start(),

	Filename = "include/crypto_compat.hrl",
	io:format("Generating ~p ...~n", [Filename]),

	case erlang:function_exported(crypto, hash, 2) of
        true ->
            HASH_SHA = "crypto:hash(sha, Data)",
            HASH_FINAL = "crypto:hash_final(Data)",
            HASH_UPDATE = "crypto:hash_update(Data, Salt)",
            HASH_INIT = "crypto:hash_init(sha)",
            io:format("...supports cryto:hash/2~n");
        false ->
            HASH_SHA = "crypto:sha(Data)",
            HASH_FINAL = "crypto:sha_final(Data)",
            HASH_UPDATE = "crypto:sha_update(Data, Salt)",
            HASH_INIT = "crypto:sha_init()",
            io:format("...does not support crypto:hash/2. Using crypto:sha/1~n")
    end,

	Contents = [
        "%% Note: This file was automatically generated. Do not include it in source control\n",
		"-define(HASH_SHA(Data), ",         HASH_SHA,").\n",
		"-define(HASH_FINAL(Data), ",       HASH_FINAL,").\n",
		"-define(HASH_UPDATE(Data, Salt), ",HASH_UPDATE,").\n",
        "-define(HASH_INIT(), ",            HASH_INIT,").\n"
	],

    ContentsBin = iolist_to_binary(Contents),
    case file:read_file(Filename) of
        {ok, ContentsBin} -> 
            io:format("...no changes needed to ~p. Skipping writing new file~n", [Filename]);
        _ -> 
            io:format("...writing ~p~n", [Filename]),
            file:write_file(Filename, Contents)
    end.
