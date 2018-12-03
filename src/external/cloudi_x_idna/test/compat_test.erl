%%%-------------------------------------------------------------------
%%% @author benoitc
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Aug 2018 14:36
%%%-------------------------------------------------------------------
-module(compat_test).
-author("benoitc").

%% API
-export([to_ascii_test/0, to_unicode_test/0]).


-include_lib("eunit/include/eunit.hrl").

to_ascii_test() ->
  ?assertEqual("xn--zckzah.xn--zckzah", idna:to_ascii("テスト.xn--zckzah")).

to_unicode_test() ->
  ?assertEqual([12486,12473,12488,46,12486,12473,12488], idna:to_unicode("xn--zckzah.xn--zckzah")).