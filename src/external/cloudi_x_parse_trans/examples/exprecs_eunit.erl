-module(exprecs_eunit).
-include_lib("eunit/include/eunit.hrl").

-export([module_test/0]).


module_test() ->
    M = test_exprecs,
    RecNames = lists:concat(
		 [Rs || {export_records, Rs} <- M:module_info(attributes)]),
    io:fwrite("RecNames = ~p~n", [RecNames]),
    false = M:'#is_record-'([]),
    false = M:'#is_record-'([], []),
    [test_record(R, M) || R <- RecNames].

test_record(R, M) ->
    Rec = M:'#new-'(R),
    Fields = M:'#info-'(R),
    FieldCount = length(Fields),
    InfoF = list_to_existing_atom("#info-" ++ atom_to_list(R)),
    ?assertEqual(FieldCount+1, M:InfoF(size)),
    ?assertEqual(true, M:'#is_record-'(Rec)),
    ?assertMatch({R,true}, {R, M:'#is_record-'(R, Rec)}),
    NewF = list_to_existing_atom("#new-" ++ atom_to_list(R)),
    L = lists:seq(1, FieldCount),
    Vals = lists:zip(Fields, L),
    Rec1 = M:NewF(Vals),
    Rec1 = M:'#set-'(Vals, Rec),
    Rec1 = M:'#fromlist-'(Vals, M:'#new-'(R)),
    L = M:'#get-'(Fields, Rec1),
    ?assertError(bad_record_op, M:'#get-'(17,Rec1)),
    PosL = lists:seq(2, FieldCount + 1),
    PosL = [M:'#pos-'(R, A) || A <- Fields],
    ?assertEqual(0, M:'#pos-'(R, bad_attr_name)).
