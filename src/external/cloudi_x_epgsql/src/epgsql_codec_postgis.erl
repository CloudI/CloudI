%%% @doc
%%% Codec for `geometry' PostGIS umbrella datatype.
%%% http://postgis.net/docs/manual-2.4/geometry.html
%%% $POSTGIS$/postgis/lwgeom_inout.c
%%% @end
%%% Created : 14 Oct 2017 by Sergey Prokhorov <me@seriyps.ru>

-module(epgsql_codec_postgis).
-behaviour(epgsql_codec).

-export([init/2, names/0, encode/3, decode/3, decode_text/3]).

-export_type([data/0]).

-type data() :: epgsql_ewkb:geometry().

init(_, _) -> [].

names() ->
    [geometry].

encode(Geo, geometry, _) ->
    epgsql_ewkb:encode_geometry(Geo).

decode(Bin, geometry, _) ->
    epgsql_ewkb:decode_geometry(Bin).

decode_text(V, _, _) -> V.
