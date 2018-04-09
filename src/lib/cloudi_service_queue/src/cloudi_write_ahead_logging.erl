%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Write Ahead Logging (WAL)==
%%% File storage for transaction logging done by cloudi_service_queue.
%%% No disk index is maintained, but an in-memory index is kept.
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2014-2018 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2014-2018 Michael Truog
%%% @version 1.7.4 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_write_ahead_logging).
-author('mjtruog at protonmail dot com').

%% external interface
-export([erase/2,
         erase_retry/4,
         fetch_keys/1,
         size/1,
         size_free/1,
         store_end/3,
         store_fail/2,
         store_start/2,
         new/5,
         update/3]).

% overhead: chunk_size, chunk_size_used, checksum_size
-define(CHUNK_OVERHEAD(ChecksumSize), 8 + 8 + ChecksumSize).
% use 64 bit offsets/sizes
-define(MAX_64BITS, 18446744073709551615).
-type non_neg_integer_64bit() :: 0..?MAX_64BITS.
-type pos_integer_64bit() :: 1..?MAX_64BITS.
-type checksum_algorithms() :: crc32 |
                               % crypto hash_algorithms() type
                               md5 | ripemd160 |
                               sha | sha224 | sha256 | sha384 | sha512.

-record(chunk,
    {
        size :: pos_integer_64bit(),         % bytes (wo/overhead)
        position :: non_neg_integer_64bit(), % file position (bof) in bytes
        request :: cloudi_service_queue:request() | undefined,
        retries = 0 :: non_neg_integer()
    }).

-record(state,
    {
        file :: string(),
        file_size_limit :: pos_integer_64bit(), % bytes
        compression :: 0..9, % zlib compression level
        checksum :: undefined | checksum_algorithms(),
        checksum_size :: pos_integer_64bit(), % bytes
        position :: non_neg_integer(),
        chunks :: #{cloudi_service:trans_id() := #chunk{}},
        chunks_free :: list(#chunk{}) % ordered
    }).

-type state() :: #state{}.
-type retry_function() ::
    fun((Chunk :: cloudi_service_queue:request(),
         Retry :: boolean()) ->
        {ok, cloudi_service:trans_id()} |
        {error, timeout}).
-type update_function() ::
    fun((cloudi_service_queue:request()) ->
        {cloudi_service:trans_id(),
         cloudi_service_queue:request()} | undefined).
-export_type([state/0,
              retry_function/0,
              update_function/0]).

-define(FILE_EXTENSION_TMP, ".tmp").

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

-spec erase(ChunkId :: cloudi_service:trans_id(),
            State :: #state{}) ->
    {cloudi_service_queue:request(), #state{}}.

erase(ChunkId,
      #state{file = FilePath,
             chunks = Chunks} = State) ->
    {Chunk, NewChunks} = maps:take(ChunkId, Chunks),
    #chunk{request = ChunkRequest} = Chunk,
    {ok, Fd} = file_open_tmp(FilePath),
    NewState = erase_chunk(Chunk, Fd, State),
    ok = file_close_tmp(FilePath, Fd),
    {ChunkRequest, NewState#state{chunks = NewChunks}}.

-spec erase_retry(ChunkId :: cloudi_service:trans_id(),
                  RetryMax :: non_neg_integer(),
                  RetryF :: retry_function(),
                  State :: #state{}) ->
    #state{}.

erase_retry(ChunkId, RetryMax, RetryF,
            #state{file = FilePath,
                   chunks = Chunks} = State) ->
    {Chunk, NewChunks} = maps:take(ChunkId, Chunks),
    #chunk{request = ChunkRequest,
           retries = Retries} = Chunk,
    NewChunkId = case RetryF(ChunkRequest, Retries < RetryMax) of
        {error, timeout} ->
            undefined;
        {ok, TransId} ->
            TransId
    end,
    if
        NewChunkId =:= undefined ->
            {ok, Fd} = file_open_tmp(FilePath),
            NewState = erase_chunk(Chunk, Fd, State),
            ok = file_close_tmp(FilePath, Fd),
            NewState#state{chunks = NewChunks};
        is_binary(NewChunkId) ->
            NewChunk = Chunk#chunk{retries = Retries + 1},
            State#state{chunks = maps:put(NewChunkId, NewChunk, NewChunks)}
    end.

-spec fetch_keys(State :: #state{}) ->
    list(cloudi_service:trans_id()).

fetch_keys(#state{chunks = Chunks}) ->
    lists:sort(maps:keys(Chunks)). % oldest -> newest

-spec size(State :: #state{}) ->
    non_neg_integer().

size(#state{chunks = Chunks}) ->
    maps:size(Chunks).

-spec size_free(State :: #state{}) ->
    non_neg_integer().

size_free(#state{chunks_free = ChunksFree}) ->
    erlang:length(ChunksFree).

-spec store_end(ChunkId :: cloudi_service:trans_id(),
                Chunk :: #chunk{},
                State :: #state{}) ->
    #state{}.

store_end(ChunkId, Chunk,
          #state{chunks = Chunks} = State) ->
    State#state{chunks = maps:put(ChunkId, Chunk, Chunks)}.

-spec store_fail(Chunk :: #chunk{},
                 State :: #state{}) ->
    #state{}.

store_fail(Chunk, #state{file = FilePath} = State) ->
    {ok, Fd} = file_open_tmp(FilePath),
    NewState = erase_chunk(Chunk, Fd, State),
    ok = file_close_tmp(FilePath, Fd),
    NewState.

-spec store_start(ChunkRequest :: cloudi_service_queue:request(),
                  State :: #state{}) ->
    {#chunk{}, #state{}} | full.

store_start(ChunkRequest,
            #state{file = FilePath,
                   file_size_limit = FileSizeLimit,
                   compression = Compression,
                   checksum = Checksum,
                   checksum_size = ChecksumSize,
                   position = Position,
                   chunks_free = ChunksFree} = State) ->
    {ok, Fd} = file_open_tmp(FilePath),
    ChunkData = erlang:term_to_binary(ChunkRequest,
                                      [{compressed, Compression}]),
    ChunkSizeUsed = erlang:byte_size(ChunkData),
    case chunk_free_check(ChunksFree, ChunkSizeUsed) of
        false
            when Position + ?CHUNK_OVERHEAD(ChecksumSize) + ChunkSizeUsed >
                 FileSizeLimit ->
            full;
        false ->
            ChunkSize = ChunkSizeUsed,
            NewPosition = chunk_write(ChunkSize, ChunkSizeUsed, ChunkData,
                                      Checksum, ChecksumSize, Position, Fd),
            ok = file_close_tmp(FilePath, Fd),
            NewChunk = #chunk{size = ChunkSize,
                              position = Position,
                              request = ChunkRequest},
            {NewChunk, State#state{position = NewPosition}};
        {#chunk{size = ChunkSize,
                position = ChunkPosition} = ChunkFree, NewChunksFree} ->
            chunk_write(ChunkSize, ChunkSizeUsed, ChunkData,
                        Checksum, ChecksumSize, ChunkPosition, Fd),
            ok = file_close_tmp(FilePath, Fd),
            NewChunk = ChunkFree#chunk{request = ChunkRequest},
            {NewChunk, State#state{chunks_free = NewChunksFree}}
    end.

-spec new(FilePath :: string(),
          FileSizeLimit :: 1024..?MAX_64BITS,
          Compression :: 0..9,
          Checksum :: undefined | checksum_algorithms(),
          RetryF :: retry_function()) ->
    #state{}.

new(FilePath, FileSizeLimit, Compression, Checksum, RetryF)
    when is_integer(FileSizeLimit),
         FileSizeLimit >= 1024, FileSizeLimit =< ?MAX_64BITS,
         is_integer(Compression), Compression >= 0, Compression =< 9,
         is_function(RetryF, 2) ->
    {ok, Fd} = file_open_copy(FilePath),
    ChecksumSize = checksum_size(Checksum),
    {ok,
     Position,
     Chunks,
     ChunksFree} = chunks_recover(Checksum, ChecksumSize, #{}, [], Fd, RetryF),
    ok = file_close_tmp(FilePath, Fd),
    #state{file = FilePath,
           file_size_limit = FileSizeLimit,
           compression = Compression,
           checksum = Checksum,
           checksum_size = ChecksumSize,
           position = Position,
           chunks = Chunks,
           chunks_free = ChunksFree}.

-spec update(ChunkId :: cloudi_service:trans_id(),
             UpdateF :: update_function(),
             State :: #state{}) ->
    {cloudi_service_queue:request() | undefined, #state{}}.

update(ChunkId, UpdateF,
       #state{file = FilePath,
              compression = Compression,
              checksum = Checksum,
              checksum_size = ChecksumSize,
              chunks = Chunks} = State) ->
    Chunk = maps:get(ChunkId, Chunks),
    #chunk{request = ChunkRequest} = Chunk,
    case UpdateF(ChunkRequest) of
        undefined ->
            {_, NewState} = erase(ChunkId, State),
            {undefined, NewState};
        {NewChunkId, NewChunkRequest} ->
            {ok, Fd} = file_open_tmp(FilePath),
            % erase previous entry
            NextState = erase_chunk(Chunk, Fd, State),
            #state{position = Position,
                   chunks_free = ChunksFree} = NextState,
            % store update
            NewChunkData = erlang:term_to_binary(NewChunkRequest,
                                                 [{compressed, Compression}]),
            NewChunkSizeUsed = erlang:byte_size(NewChunkData),
            NewState = case chunk_free_check(ChunksFree, NewChunkSizeUsed) of
                false ->
                    % not checking NewChunkSizeUsed with file_size_limit
                    % due to the operation being an update
                    % (so it is possible to have the file size exceed
                    %  file_size_limit here)
                    NewChunkSize = NewChunkSizeUsed,
                    NewPosition = chunk_write(NewChunkSize, NewChunkSizeUsed,
                                              NewChunkData,
                                              Checksum, ChecksumSize,
                                              Position, Fd),
                    NewChunk = #chunk{size = NewChunkSize,
                                      position = Position,
                                      request = NewChunkRequest},
                    NewChunks = maps:put(NewChunkId, NewChunk, Chunks),
                    NextState#state{chunks = NewChunks,
                                    position = NewPosition};
                {#chunk{size = ChunkSize,
                        position = ChunkPosition} = ChunkFree, NewChunksFree} ->
                    chunk_write(ChunkSize, NewChunkSizeUsed,
                                NewChunkData, Checksum, ChecksumSize,
                                ChunkPosition, Fd),
                    NewChunk = ChunkFree#chunk{request = NewChunkRequest},
                    NewChunks = maps:put(NewChunkId, NewChunk, Chunks),
                    NextState#state{chunks = NewChunks,
                                    chunks_free = NewChunksFree}
            end,
            ok = file_close_tmp(FilePath, Fd),
            {NewChunkRequest, NewState}
    end.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

file_open_copy(FilePath) ->
    case file:copy({FilePath, [raw]},
                   {FilePath ++ ?FILE_EXTENSION_TMP, [raw]}) of
        {ok, _} ->
            ok;
        {error, enoent} ->
            case file:delete(FilePath ++ ?FILE_EXTENSION_TMP) of
                ok ->
                    ok;
                {error, enoent} ->
                    ok
            end
    end,
    file:open(FilePath ++ ?FILE_EXTENSION_TMP, [raw, write, read, binary]).

file_open_tmp(FilePath) ->
    file:open(FilePath ++ ?FILE_EXTENSION_TMP, [raw, write, read, binary]).

file_close_tmp(FilePath, Fd) ->
    ok = file:close(Fd),
    ok = file:rename(FilePath ++ ?FILE_EXTENSION_TMP, FilePath),
    {ok, _} = file:copy({FilePath, [raw]},
                        {FilePath ++ ?FILE_EXTENSION_TMP, [raw]}),
    ok.

chunk_write(ChunkSize, ChunkSizeUsed, ChunkData,
            Checksum, ChecksumSize, Position, Fd) ->
    ChunkChecksum = checksum(Checksum, ChunkData),
    ChunkSizeZero = (ChunkSize - ChunkSizeUsed),
    ok = file:pwrite(Fd, Position,
                     <<ChunkSize:64/unsigned-integer-big,
                       ChunkSizeUsed:64/unsigned-integer-big,
                       ChunkChecksum/binary,
                       ChunkData/binary,
                       0:(ChunkSizeZero * 8)>>),
    Position + ?CHUNK_OVERHEAD(ChecksumSize) + ChunkSize.

chunk_erase_last(ChunkSize, ChecksumSize, Position, Fd) ->
    ok = file:pwrite(Fd, Position,
                     <<0:64,
                       0:64,
                       0:(ChecksumSize * 8),
                       0:(ChunkSize * 8)>>),
    ok.

chunk_free(ChunkSize, ChecksumSize, Position, Fd) ->
    ok = file:pwrite(Fd, Position,
                     <<ChunkSize:64/unsigned-integer-big,
                       0:64,
                       0:(ChecksumSize * 8),
                       0:(ChunkSize * 8)>>),
    ok.

erase_chunk(#chunk{size = ChunkSize,
                   position = ChunkPosition} = Chunk,
            Fd,
            #state{checksum_size = ChecksumSize,
                   position = Position,
                   chunks_free = ChunksFree} = State) ->
    if
        ChunkPosition + ?CHUNK_OVERHEAD(ChecksumSize) + ChunkSize == Position ->
            chunk_erase_last(ChunkSize, ChecksumSize, ChunkPosition, Fd),
            State#state{position = ChunkPosition};
        true ->
            chunk_free(ChunkSize, ChecksumSize, ChunkPosition, Fd),
            ChunkFree = Chunk#chunk{request = undefined,
                                    retries = 0},
            State#state{chunks_free = lists:umerge(ChunksFree, [ChunkFree])}
    end.

chunk_free_check(L, Size) ->
    chunk_free_check(L, [], Size).

chunk_free_check([], _, _) ->
    false;
chunk_free_check([#chunk{size = ChunkSize} = Chunk | ChunksFree], L, Size)
    when ChunkSize >= Size ->
    {Chunk, lists:reverse(L) ++ ChunksFree};
chunk_free_check([Chunk | ChunksFree], L, Size) ->
    chunk_free_check(ChunksFree, [Chunk | L], Size).

chunk_recover_free(Position, ChunkSize, Checksum, ChecksumSize,
                   Chunks, ChunksFree, Fd, RetryF) ->
    NewPosition = Position + ?CHUNK_OVERHEAD(ChecksumSize) + ChunkSize,
    ChunkFree = #chunk{size = ChunkSize,
                       position = Position,
                       request = undefined},
    chunks_recover(NewPosition, Checksum, ChecksumSize, Chunks,
                   lists:umerge(ChunksFree, [ChunkFree]),
                   Fd, RetryF).

chunk_recover_used(Position, ChunkSize, ChunkSizeUsed, Checksum, ChecksumSize,
                   Chunks, ChunksFree, Fd, RetryF) ->
    case file:read(Fd, ChecksumSize + ChunkSizeUsed) of
        {ok, <<ChunkChecksum:ChecksumSize/binary,
               ChunkData/binary>>} ->
            ChecksumValid = ChunkChecksum == checksum(Checksum, ChunkData),
            if
                ChecksumValid =:= true ->
                    ChunkRequest = erlang:binary_to_term(ChunkData),
                    case RetryF(ChunkRequest, true) of
                        {error, timeout} ->
                            ok = chunk_free(ChunkSize, ChecksumSize,
                                            Position, Fd),
                            chunk_recover_free(Position, ChunkSize,
                                               Checksum, ChecksumSize,
                                               Chunks, ChunksFree, Fd, RetryF);
                        {ok, ChunkId} when ChunkSize == ChunkSizeUsed ->
                            NewPosition = Position +
                                          ?CHUNK_OVERHEAD(ChecksumSize) +
                                          ChunkSize,
                            Chunk = #chunk{size = ChunkSize,
                                           position = Position,
                                           request = ChunkRequest},
                            chunks_recover(NewPosition,
                                           Checksum, ChecksumSize,
                                           maps:put(ChunkId, Chunk, Chunks),
                                           ChunksFree, Fd, RetryF);
                        {ok, ChunkId} ->
                            ChunkEnd = (ChunkSize - ChunkSizeUsed),
                            case file:position(Fd, {cur, ChunkEnd}) of
                                {ok, NewPosition} ->
                                    Chunk = #chunk{size = ChunkSize,
                                                   position = Position,
                                                   request = ChunkRequest},
                                    chunks_recover(NewPosition,
                                                   Checksum, ChecksumSize,
                                                   maps:put(ChunkId, Chunk,
                                                            Chunks),
                                                   ChunksFree, Fd, RetryF);
                                {error, Reason} ->
                                    {error, {chunk_corrupt, Reason}}
                            end
                    end;
                ChecksumValid =:= false ->
                    {error, chunk_checksum_failed}
            end;
        {error, Reason} ->
            {error, {chunk_size_used_invalid, Reason}}
    end.

chunk_recover(Position, ChunkSize, Checksum, ChecksumSize,
              Chunks, ChunksFree, Fd, RetryF) ->
    case file:read(Fd, 8) of
        {error, Reason} ->
            {error, {chunk_used_size_invalid, Reason}};
        eof ->
            {error, {chunk_used_size_missing, eof}};
        {ok, <<0:64>>} ->
            case file:position(Fd, {cur, ChecksumSize + ChunkSize}) of
                {ok, _} ->
                    chunk_recover_free(Position, ChunkSize,
                                       Checksum, ChecksumSize,
                                       Chunks, ChunksFree, Fd, RetryF);
                {error, Reason} ->
                    {error, {chunk_corrupt, Reason}}
            end;
        {ok, <<ChunkSizeUsed:64/unsigned-integer-big>>} ->
            if
                ChunkSize >= ChunkSizeUsed ->
                    chunk_recover_used(Position, ChunkSize, ChunkSizeUsed,
                                       Checksum, ChecksumSize,
                                       Chunks, ChunksFree, Fd, RetryF);
                true ->
                    {error, chunk_size_corrupt}
            end
    end.

chunks_recover(Checksum, ChecksumSize, Chunks, ChunksFree, Fd, RetryF) ->
    chunks_recover(0, Checksum, ChecksumSize, Chunks, ChunksFree, Fd, RetryF).

chunks_recover(Position, Checksum, ChecksumSize,
               Chunks, ChunksFree, Fd, RetryF) ->
    case file:read(Fd, 8) of
        {error, Reason} ->
            {error, {chunk_size_missing, Reason}};
        eof ->
            {ok, Position, Chunks, ChunksFree};
        {ok, <<0:64>>} ->
            {ok, Position, Chunks, ChunksFree};
        {ok, <<ChunkSize:64/unsigned-integer-big>>} ->
            chunk_recover(Position, ChunkSize, Checksum, ChecksumSize,
                          Chunks, ChunksFree, Fd, RetryF)
    end.

checksum_size(undefined) ->
    0;
checksum_size(crc32) ->
    4;
checksum_size(md5) ->
    16;
checksum_size(ripemd160) ->
    20;
checksum_size(sha) ->
    20;
checksum_size(sha224) ->
    28;
checksum_size(sha256) ->
    32;
checksum_size(sha384) ->
    48;
checksum_size(sha512) ->
    64.

checksum(undefined, _) ->
    <<>>;
checksum(crc32, Data) ->
    I = erlang:crc32(Data), % CRC-32 IEEE 802.3 style
    <<I:32/big-unsigned-integer>>;
checksum(ChecksumType, Data) ->
    crypto:hash(ChecksumType, Data).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

checksum_size_test() ->
    Data = <<"The quick brown fox jumps over the lazy dog">>,
    Checksums = [undefined, crc32, md5, ripemd160,
                 sha, sha224, sha256, sha384, sha512],
    true = lists:all(fun(Checksum) ->
        checksum_size(Checksum) == byte_size(checksum(Checksum, Data))
    end, Checksums),
    ok.

-endif.

