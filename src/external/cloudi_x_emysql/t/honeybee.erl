-module(honeybee).
-behavior(emysql_worker).

-export([init/1, process/1]).
-export([get_by_bid/1, get_by_type/1, get_all/0, create/1, update_type/2]).
-export([cajole_bee/1]).

-record(bee, {name, type, size}).

init(_) ->
    erlang:register(honeybee1, self()),
    Prepares = [
        {bees_by_bid, <<"select * from hive where name = ?">>},
        {bees_by_type, <<"select * from hive where type = ?">>},
        {bees_all, <<"select * from hive">>},
        {bees_create, <<"INSERT INTO hive SET name = ?, type = ?, size = ?">>},
        {bees_update_type, <<"UPDATE hive SET type = ? WHERE name = ?">>}
    ],
    {ok, Prepares, master}.

process({get_by_bid, Args}) ->
    {bees_by_bid, Args, {bee, record_info(fields, bee), fun cajole_bee/1}};

process({get_by_type, _}) ->
    {bees_by_type};

process({get_all, Args}) ->
    {bees_all, Args, {bee, record_info(fields, bee)}};

process({update, [BeeID, Type]}) ->
    {bees_update_type, [Type, BeeID]};

process({create, Bee}) ->
    {bees_create, [Bee#bee.name, Bee#bee.type, Bee#bee.size]}.

get_by_bid(BeeId) ->
    emysql_worker:execute(honeybee1, {get_by_bid, [BeeId]}).

get_by_type(Type) ->
    emysql_worker:execute(honeybee1, {get_by_type, [Type]}).

get_all() ->
    emysql_worker:execute(honeybee1, {get_all}).

create(Bee) ->
    emysql_worker:execute(honeybee1, {create, Bee}).

update_type(BeeID, Type) ->
    emysql_worker:execute(honeybee1, {update, [BeeID, Type]}).

cajole_bee(X) -> X.
