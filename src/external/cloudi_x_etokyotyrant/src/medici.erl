%%% The contents of this file are subject to the Erlang Public License,
%%% Version 1.1, (the "License"); you may not use this file except in
%%% compliance with the License. You should have received a copy of the
%%% Erlang Public License along with this software. If not, it can be
%%% retrieved via the world wide web at http://www.erlang.org/.
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%-------------------------------------------------------------------
%%% File:      medici.erl
%%% @author    Jim McCoy <mccoy@mad-scientist.com>
%%% @copyright Copyright (c) 2009, Jim McCoy.  All Rights Reserved.
%%%
%%% @doc
%%% This module provides the primary API for interfacing with the
%%% medici application. These functions assume you are using the default 
%%% registered name for the service and that you know what sort of remote database
%%% you are talking to (e.g. don't make table-specific calls to a hash
%%% database.) If you need byte-order specific ops, want to register the
%%% controller with a different name, or want to run medici interfaces
%%% to multiple remote databases within the same erlang VM the you should
%%% update these functions or just run gen_server:call() directly.
%%% @end

-module(medici).

%% Connecting and Disconnecting
-export([connect/1, close/1]).

%% Basic API exports
-export([addint/3, addint/4,
         adddouble/3, adddouble/4,
         adddouble_parts/4, adddouble_parts/5,
         copy/2, copy/3,
         fwmkeys/3, fwmkeys/4,
         get/2, get/3, 
         iterinit/1, iterinit/2,
         iternext/1, iternext/2,
         mget/2, mget/3,
         optimize/2, optimize/3,
         out/2, out/3,
         put/3, put/4,
         putcat/3, putcat/4,
         putkeep/3, putkeep/4,
         putnr/3, putnr/4,
         putshl/4, putshl/5,
         restore/3, restore/4,
         rnum/1, rnum/2,
         setmst/3, setmst/4,
         size/1, size/2, 
         stat/1, stat/2,
         sync/1, sync/2,
         vanish/1, vanish/2,
         vsiz/2, vsiz/3]).

%% Table API exports
-export([genuid/1, genuid/2,
         query_add_condition/5, query_add_condition/6,
         query_limit/3, query_limit/4,
         query_limit_skip/4, query_limit_skip/5,
         query_order/4, query_order/5,
         search/2, search/3,
         searchcount/2, searchcount/3,
         searchout/2, searchout/3,
         setindex/3, setindex/4,
         update/3, update/4]).

-include("medici.hrl").

%% @spec connect(Options::proplist()) -> {ok, Connection} | {error, Reason}
%%
%% @doc 
%% Start the medici tokyo tyrant connections, using the provided proplist.
%% @end
connect(Options) when is_list(Options) ->
    case medici_sup:start_link(Options) of
        {ok, SupervisorPid} when is_pid(SupervisorPid) ->
            case lists:keyfind(controller, 1,
                               supervisor:which_children(SupervisorPid)) of
                false ->
                    close({undefined, SupervisorPid}),
                    {error, "controller does not exist"};
                {controller, undefined, _, _} ->
                    close({undefined, SupervisorPid}),
                    {error, "controller was not started"};
                {controller, ControllerPid, _, _} when is_pid(ControllerPid) ->
                    {ok, {ControllerPid, SupervisorPid}}
            end;
        {error, _} = Error ->
            Error
    end.

close({_, SupervisorPid} = Connection) when is_tuple(Connection) ->
    unlink(SupervisorPid),
    erlang:exit(SupervisorPid, normal),
    ok.

put(Connection, Key, Value) ->
    put(Connection, Key, Value, ?TIMEOUT).
put({ConnectionPid, _}, Key, Value, Timeout)
    when is_integer(Timeout) ->
    gen_server:call(ConnectionPid,
                    {put, Key, Value, Timeout},
                    Timeout + 5000).
 
putcat(Connection, Key, Value) ->
    putcat(Connection, Key, Value, ?TIMEOUT).
putcat({ConnectionPid, _}, Key, Value, Timeout)
    when is_integer(Timeout) ->
    gen_server:call(ConnectionPid,
                    {putcat, Key, Value, Timeout},
                    Timeout + 5000).

putkeep(Connection, Key, Value) ->
    putkeep(Connection, Key, Value, ?TIMEOUT).
putkeep({ConnectionPid, _}, Key, Value, Timeout)
    when is_integer(Timeout) ->
    gen_server:call(ConnectionPid,
                    {putkeep, Key, Value, Timeout},
                    Timeout + 5000).

putshl(Connection, Key, Value, Width) ->
    putshl(Connection, Key, Value, Width, ?TIMEOUT).
putshl({ConnectionPid, _}, Key, Value, Width, Timeout)
    when is_integer(Timeout) ->
    gen_server:call(ConnectionPid,
                    {putshl, Key, Value, Width, Timeout},
                    Timeout + 5000).

putnr(Connection, Key, Value) ->
    putnr(Connection, Key, Value, ?TIMEOUT).
putnr({ConnectionPid, _}, Key, Value, Timeout)
    when is_integer(Timeout) ->
    gen_server:cast(ConnectionPid,
                    {putnr, Key, Value, Timeout}).

out(Connection, Key) ->
    out(Connection, Key, ?TIMEOUT).
out({ConnectionPid, _}, Key, Timeout)
    when is_integer(Timeout) ->
    gen_server:call(ConnectionPid,
                    {out, Key, Timeout},
                    Timeout + 5000).

get(Connection, Key) ->
    get(Connection, Key, ?TIMEOUT).
get({ConnectionPid, _}, Key, Timeout)
    when is_integer(Timeout) ->
    gen_server:call(ConnectionPid,
                    {get, Key, Timeout},
                    Timeout + 5000).

mget(Connection, KeyList) ->
    mget(Connection, KeyList, ?TIMEOUT).
mget({ConnectionPid, _}, KeyList, Timeout)
    when is_integer(Timeout) ->
    gen_server:call(ConnectionPid,
                    {mget, KeyList, Timeout},
                    Timeout + 5000).

vsiz(Connection, Key) ->
    vsiz(Connection, Key, ?TIMEOUT).
vsiz({ConnectionPid, _}, Key, Timeout)
    when is_integer(Timeout) ->
    gen_server:call(ConnectionPid,
                    {vsiz, Key, Timeout},
                    Timeout + 5000).

iterinit(Connection) ->
    iterinit(Connection, ?TIMEOUT).
iterinit({ConnectionPid, _}, Timeout)
    when is_integer(Timeout) ->
    gen_server:call(ConnectionPid,
                    {iterinit, Timeout},
                    Timeout + 5000).

iternext(Connection) ->
    iternext(Connection, ?TIMEOUT).
iternext({ConnectionPid, _}, Timeout)
    when is_integer(Timeout) ->
    gen_server:call(ConnectionPid,
                    {iternext, Timeout},
                    Timeout + 5000).

fwmkeys(Connection, Prefix, MaxKeys) ->
    fwmkeys(Connection, Prefix, MaxKeys, ?TIMEOUT).
fwmkeys({ConnectionPid, _}, Prefix, MaxKeys, Timeout)
    when is_integer(Timeout) ->
    gen_server:call(ConnectionPid,
                    {fwmkeys, Prefix, MaxKeys, Timeout},
                    Timeout + 5000).

addint(Connection, Key, Int) ->
    addint(Connection, Key, Int, ?TIMEOUT).
addint({ConnectionPid, _}, Key, Int, Timeout)
    when is_integer(Timeout) ->
    gen_server:call(ConnectionPid,
                    {addint, Key, Int, Timeout},
                    Timeout + 5000).

adddouble(Connection, Key, Double) ->
    adddouble(Connection, Key, Double, ?TIMEOUT).
adddouble({ConnectionPid, _}, Key, Double, Timeout)
    when is_integer(Timeout) ->
    gen_server:call(ConnectionPid,
                    {adddouble, Key, Double, Timeout},
                    Timeout + 5000).

adddouble_parts(Connection, Key, IntPart, FracPart) ->
    adddouble_parts(Connection, Key, IntPart, FracPart, ?TIMEOUT).
adddouble_parts({ConnectionPid, _}, Key, IntPart, FracPart, Timeout)
    when is_integer(Timeout) ->
    gen_server:call(ConnectionPid,
                    {adddouble, Key, IntPart, FracPart, Timeout},
                    Timeout + 5000).

sync(Connection) ->
    sync(Connection, ?TIMEOUT).
sync({ConnectionPid, _}, Timeout)
    when is_integer(Timeout) ->
    gen_server:call(ConnectionPid,
                    {sync, Timeout},
                    Timeout + 5000).

vanish(Connection) ->
    vanish(Connection, ?TIMEOUT).
vanish({ConnectionPid, _}, Timeout)
    when is_integer(Timeout) ->
    gen_server:call(ConnectionPid,
                    {vanish, Timeout},
                    Timeout + 5000).

optimize(Connection, TuningOptions) ->
    optimize(Connection, TuningOptions, ?TIMEOUT).
optimize({ConnectionPid, _}, TuningOptions, Timeout)
    when is_integer(Timeout) ->
    gen_server:call(ConnectionPid,
                    {optimize, TuningOptions, Timeout},
                    Timeout + 5000).

rnum(Connection) ->
    rnum(Connection, ?TIMEOUT).
rnum({ConnectionPid, _}, Timeout)
    when is_integer(Timeout) ->
    gen_server:call(ConnectionPid,
                    {rnum, Timeout},
                    Timeout + 5000).

size(Connection) ->
    size(Connection, ?TIMEOUT).
size({ConnectionPid, _}, Timeout)
    when is_integer(Timeout) ->
    gen_server:call(ConnectionPid,
                    {size, Timeout},
                    Timeout + 5000).

stat(Connection) ->
    stat(Connection, ?TIMEOUT).
stat({ConnectionPid, _}, Timeout)
    when is_integer(Timeout) ->
    gen_server:call(ConnectionPid,
                    {stat, Timeout},
                    Timeout + 5000).

copy(Connection, PathName) ->
    copy(Connection, PathName, ?TIMEOUT).
copy({ConnectionPid, _}, PathName, Timeout)
    when is_integer(Timeout) ->
    gen_server:call(ConnectionPid,
                    {copy, PathName, Timeout},
                    Timeout + 5000).

restore(Connection, PathName, TimeStamp) ->
    restore(Connection, PathName, TimeStamp, ?TIMEOUT).
restore({ConnectionPid, _}, PathName, TimeStamp, Timeout)
    when is_integer(Timeout) ->
    gen_server:call(ConnectionPid,
                    {restore, PathName, TimeStamp, Timeout},
                    Timeout + 5000).

setmst(Connection, HostName, Port) ->
    setmst(Connection, HostName, Port, ?TIMEOUT).
setmst({ConnectionPid, _}, HostName, Port, Timeout)
    when is_integer(Timeout) ->
    gen_server:call(ConnectionPid,
                    {setmst, HostName, Port, Timeout},
                    Timeout + 5000).

%% Additional table functions
update(Connection, Key, NewCols) ->
    update(Connection, Key, NewCols, ?TIMEOUT).
update({ConnectionPid, _}, Key, NewCols, Timeout)
    when is_integer(Timeout) ->
    gen_server:call(ConnectionPid,
                    {update, Key, NewCols, Timeout},
                    Timeout + 5000).

setindex(Connection, Column, Type) ->
    setindex(Connection, Column, Type, ?TIMEOUT).
setindex({ConnectionPid, _}, Column, Type, Timeout)
    when is_integer(Timeout) ->
    gen_server:call(ConnectionPid,
                    {setindex, Column, Type, Timeout},
                    Timeout + 5000).

genuid(Connection) ->
    genuid(Connection, ?TIMEOUT).
genuid({ConnectionPid, _}, Timeout)
    when is_integer(Timeout) ->
    gen_server:call(ConnectionPid,
                    {genuid, Timeout},
                    Timeout + 5000).

query_limit(Connection, OldQuery, Max) ->
    query_limit(Connection, OldQuery, Max, ?TIMEOUT).
query_limit({ConnectionPid, _}, OldQuery, Max, Timeout)
    when is_integer(Timeout) ->
    gen_server:call(ConnectionPid,
                    {query_limit, OldQuery, Max, Timeout},
                    Timeout + 5000).

query_limit_skip(Connection, OldQuery, Max, Skip) ->
    query_limit_skip(Connection, OldQuery, Max, Skip, ?TIMEOUT).
query_limit_skip({ConnectionPid, _}, OldQuery, Max, Skip, Timeout)
    when is_integer(Timeout) ->
    gen_server:call(ConnectionPid,
                    {query_limit, OldQuery, Max, Skip, Timeout},
                    Timeout + 5000).

query_add_condition(Connection, OldQuery, Column, Op, ExprList) ->
    query_add_condition(Connection, OldQuery, Column, Op, ExprList, ?TIMEOUT).
query_add_condition({ConnectionPid, _}, OldQuery, Column, Op, ExprList, Timeout)
    when is_integer(Timeout) ->
    gen_server:call(ConnectionPid,
                    {query_add_condition, OldQuery, Column, Op, ExprList, Timeout},
                    Timeout + 5000).

query_order(Connection, OldQuery, Column, Type) ->
    query_order(Connection, OldQuery, Column, Type, ?TIMEOUT).
query_order({ConnectionPid, _}, OldQuery, Column, Type, Timeout)
    when is_integer(Timeout) ->
    gen_server:call(ConnectionPid,
                    {query_order, OldQuery, Column, Type, Timeout},
                    Timeout + 5000).

search(Connection, Query) ->
    search(Connection, Query, ?TIMEOUT).
search({ConnectionPid, _}, Query, Timeout)
    when is_integer(Timeout) ->
    gen_server:call(ConnectionPid,
                    {search, Query, Timeout},
                    Timeout + 5000).

searchcount(Connection, Query) ->
    searchcount(Connection, Query, ?TIMEOUT).
searchcount({ConnectionPid, _}, Query, Timeout)
    when is_integer(Timeout) ->
    gen_server:call(ConnectionPid,
                    {searchcount, Query, Timeout},
                    Timeout + 5000).

searchout(Connection, Query) ->
    searchout(Connection, Query, ?TIMEOUT).
searchout({ConnectionPid, _}, Query, Timeout)
    when is_integer(Timeout) ->
    gen_server:call(ConnectionPid,
                    {searchout, Query, Timeout},
                    Timeout + 5000).

%% EUnit tests
%%

%% TODO: for completeness, set up two internal suites within this
%% test suite, one for hash and another for table.  Test hash (assuming
%% the server is already running), then test hash again by running the 
%% server internal to medici, then test table the same way.  Need to
%% review eunit docs to get this one right...

-ifdef(EUNIT).

get_random_count() ->
    get_random_count(1000).

get_random_count(Max) ->
    crypto:start(),
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    round(Max * random:uniform()).

%% setup_table_data() ->
%%     ColData = [{"rec1", [{"name", "alice"}, {"sport", "baseball"}]},
%%            {"rec2", [{"name", "bob"}, {"sport", "basketball"}]},
%%            {"rec3", [{"name", "carol"}, {"age", "24"}]},
%%            {"rec4", [{"name", "trent"}, {"age", "33"}, {"sport", "football"}]},
%%            {"rec5", [{"name", "mallet"}, {"sport", "tennis"}, {"fruit", "apple"}]}
%%            ],
%%     lists:foreach(fun({Key, ValProplist}) ->
%%               ok = ?MODULE:put(Key, ValProplist)
%%           end, ColData).

init_test() ->
    ?MODULE:start(),
    ?MODULE:stop(),
    ok.

%% init_with_args_test() ->
%%     ?MODULE:start([{foobar, 32}]),
%%     {ok, Options} = application:get_env(medici, options),
%%     ?assert(lists:member({foobar, 32}, Options)),
%%     ?MODULE:stop().

medici_api_test_() ->
    {setup, 
     fun() -> ?MODULE:start() end,
     fun(_Cleanup) -> ?MODULE:stop() end,
     [?_test(put_get_unit()),
      ?_test(put_get_random_unit()),
      ?_test(putkeep_unit()),
      ?_test(putcat_unit()),
      ?_test(putshl_unit()),
%%      ?_test(putnr_unit()),
      ?_test(out_unit()),
      ?_test(mget_unit()),
      ?_test(vsiz_unit()),
      ?_test(vanish_unit()),
      ?_test(iter_unit()),
      ?_test(fwmkeys_unit()),
%%      ?_test(addint_unit()),
      ?_test(sync_unit()),
      ?_test(rnum_unit()),
      ?_test(size_unit()),
      ?_test(stat_unit()),
      ?_test(optimize_unit())]
    }.

put_get_unit() ->
    ?assert(?MODULE:put("put_get1", "testval") =:= ok),
    ?assert(?MODULE:put(<<"put_get2">>, <<32,145,56,0,14>>) =:= ok),
    ?assert(?MODULE:get(<<"put_get1">>) =:= <<"testval">>),
    ?assert(?MODULE:get("put_get2") =:= <<32, 145, 56, 0, 14>>).

put_get_random_unit() ->
    ElementCount = get_random_count(),
    PutVals = lists:foldl(fun(_Seq, Acc) ->
                  KeySize = random:uniform(1024),
                  Key = crypto:rand_bytes(KeySize),
                  ValSize = random:uniform(65536),
                  Val = crypto:rand_bytes(ValSize),
                  ok = ?MODULE:put(Key, Val),
                  [{Key, Val} | Acc]
              end, [], lists:seq(1, ElementCount)),
    lists:foreach(fun({K, V}) ->
              ?assert(?MODULE:get(K) =:= V)
          end, PutVals).

putkeep_unit() ->
    ok = ?MODULE:put(<<"putkeep1">>, <<"foo">>),
    ?assert(?MODULE:get(<<"putkeep1">>) =:= <<"foo">>),
    ?assertMatch({error, _}, ?MODULE:putkeep(<<"putkeep1">>, <<"bar">>)),
    ?assert(?MODULE:get(<<"putkeep1">>) =:= <<"foo">>), % no effect if key already exists before putkeep
    ok = ?MODULE:putkeep(<<"putkeep2">>, <<"baz">>),
    ?assert(?MODULE:get(<<"putkeep2">>) =:= <<"baz">>). % puts the key if key does not exist already

putcat_unit() ->
    ok = ?MODULE:put(<<"putcat1">>, <<"foo">>),
    % append "bar" to the end
    ok = ?MODULE:putcat(<<"putcat1">>, <<"bar">>),
    ?assert(?MODULE:get(<<"putcat1">>) =:= <<"foobar">>).

putshl_unit() ->
    ok = ?MODULE:put(<<"putshl">>, <<"foo">>),
    % append "bar" to the end and shift to the left to retain the width of "4"
    ok = ?MODULE:putshl(<<"putshl">>, <<"bar">>, 4),
    ?assert(?MODULE:get(<<"putshl">>) =:= <<"obar">>).

%% putnr_unit() ->
%%     ?MODULE:putnr(<<"putnr1">>, <<"no reply">>),
%%     ?assert(?MODULE:get(<<"putnr1">>) =:= <<"no reply">>).

out_unit() ->
    ok = ?MODULE:put(<<"out1">>, <<"to remove">>),
    ?assert(?MODULE:get(<<"out1">>) =:= <<"to remove">>),
    ok = ?MODULE:out(<<"out1">>),
    ?assertMatch({error, _}, ?MODULE:get(<<"out1">>)).

mget_unit() ->
    ok = ?MODULE:put(<<"mget1">>, <<"alice">>),
    ok = ?MODULE:put(<<"mget2">>, <<"bob">>),
    ok = ?MODULE:put(<<"mget3">>, <<"carol">>),
    ok = ?MODULE:put(<<"mget4">>, <<"trent">>),
    ?assert(?MODULE:mget([<<"mget1">>, <<"mget2">>, 
              <<"mget3">>, <<"mget4">>]) =:= 
        [{<<"mget1">>, <<"alice">>}, 
         {<<"mget2">>, <<"bob">>}, 
         {<<"mget3">>, <<"carol">>}, 
         {<<"mget4">>, <<"trent">>}]).

vsiz_unit() ->
    ok = ?MODULE:put(<<"vsiz1">>, <<"vsiz test">>),
    ?assert(?MODULE:vsiz(<<"vsiz1">>) =:= 9).

vanish_unit() ->
    ok = ?MODULE:put(<<"vanish1">>, <<"going away">>),
    ok = ?MODULE:vanish(),
    ?assertMatch({error, _}, ?MODULE:get(<<"vanish1">>)).

iter_unit() ->
    ok = ?MODULE:vanish(),
    ok = ?MODULE:put(<<"a">>, <<"first">>),
    ok = ?MODULE:iterinit(),
    <<"a">> = ?MODULE:iternext(), % "a" should be the first key
    % Now to test a bit of real iteration
    ok = ?MODULE:put(<<"b">>, <<"second">>),
    ok = ?MODULE:put(<<"c">>, <<"third">>),
    ok = ?MODULE:iterinit(),
    One = ?MODULE:iternext(),
    Two = ?MODULE:iternext(),
    Three = ?MODULE:iternext(),
    ?assertMatch({error, _}, ?MODULE:iternext()),
    ?assertMatch([<<"a">>, <<"b">>, <<"c">>], lists:sort([One, Two, Three])).

fwmkeys_unit() ->
    ok = ?MODULE:vanish(),
    ok = ?MODULE:put(<<"fwmkeys1">>, <<"1">>),
    ok = ?MODULE:put(<<"fwmkeys2">>, <<"2">>),
    ok = ?MODULE:put(<<"fwmkeys3">>, <<"3">>),
    ok = ?MODULE:put(<<"fwmkeys4">>, <<"4">>),
    Keys1 = ?MODULE:fwmkeys(<<"fwmkeys">>, 4),
    ?assert(length(Keys1) =:= 4),
    ?assert(lists:member(<<"fwmkeys1">>, Keys1)),
    ?assert(lists:member(<<"fwmkeys2">>, Keys1)),
    ?assert(lists:member(<<"fwmkeys3">>, Keys1)),
    ?assert(lists:member(<<"fwmkeys4">>, Keys1)),
    Keys2 = ?MODULE:fwmkeys(<<"fwmkeys">>, 2),
    ?assert(length(Keys2) =:= 2).

%% addint_unit() ->
%%     ok = ?MODULE:put(<<"addint1">>, 100),
%%     ?assert(?MODULE:addint(<<"addint1">>, 20) =:= 120).

sync_unit() ->
    ?assert(?MODULE:sync() =:= ok).

rnum_unit() ->
    ok = ?MODULE:vanish(),
    ok = ?MODULE:put(<<"rnum1">>, <<"foo">>),
    ok = ?MODULE:put(<<"rnum2">>, <<"foo">>),
    ?assert(?MODULE:rnum() =:= 2),
    ok = ?MODULE:vanish(),
    ?assert(?MODULE:rnum() =:= 0).

size_unit() ->
    OldSize = ?MODULE:size(),
    ok = ?MODULE:put(<<"size1">>, <<"foo">>),
    NewSize = ?MODULE:size(),
    ?assert(NewSize > OldSize).

stat_unit() ->
    StatInfo = ?MODULE:stat(),
    Protocol = proplists:get_value(protver, StatInfo),
    ?assert(list_to_float(Protocol) > 0.9).

optimize_unit() ->
    ?assert(?MODULE:optimize("#bnum=1000000#opts=ld") =:= ok).

-endif.
