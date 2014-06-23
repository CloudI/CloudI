-module(lz4).

-export([compress/1, compress/2, uncompress/2,
    pack/1, pack/2, unpack/1]).

-on_load(init/0).

-type option() :: high.
%% Compressor option.
%%
%% <dt>`high'</dt>
%% <dd>Compresses with high ratio.</dd>

-type pack() :: binary().
%% Binary included compressed data and original size stored
%% at first 4 bytes in little endian.

-define(nif_stub, nif_stub_error(?LINE)).
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, bad_name} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    erlang:load_nif(filename:join(PrivDir, lz4_nif), 0).

%% @doc Equals `compress(Binary, [])'.
%% @see compress/2
-spec compress(binary()) -> {ok, binary()} | {error, term()}.
compress(Binary) ->
    compress(Binary, []).

%% @doc Returns an compressed binary. Note that the compressed binary
%%      does not include original size to be needed at uncompressing.
%% @see uncompress/2
%% @see pack/2
-spec compress(binary(), [option()]) -> {ok, binary()} | {error, term()}.
compress(_Binary, _Options) ->
    ?nif_stub.

%% @doc Returns an uncompressed binary.
%%      You need to specify original size as `OrigSize'.
%% @see compress/2
-spec uncompress(binary(), integer()) -> {ok, binary()} | {error, term()}.
uncompress(_Binary, _OrigSize) ->
    ?nif_stub.

%% @doc Equals `pack(Binary, [])'.
%% @see pack/2
-spec pack(binary()) -> {ok, pack()} | {error, term()}.
pack(Binary) ->
    pack(Binary, []).

%% @doc Returns a binary included compressed data and original size.
%%      Use `unpack/1' to uncompress the returned binary.
%% @see compress/2
%% @see unpack/1
-spec pack(binary(), [option()]) -> {ok, pack()} | {error, term()}.
pack(Binary, Options) ->
    case compress(Binary, Options) of
        {ok, Compressed} ->
            OrigSize = byte_size(Binary),
            {ok, <<OrigSize:4/little-unsigned-integer-unit:8,
                Compressed/binary>>};
        Error ->
            Error
    end.

%% @doc Return a uncompressed binary compressed with `pack/2'.
%% @see pack/2
-spec unpack(pack()) -> {ok, binary()} | {error, term()}.
unpack(<<OrigSize:4/little-unsigned-integer-unit:8, Binary/binary>>=_Binary) ->
    uncompress(Binary, OrigSize).

