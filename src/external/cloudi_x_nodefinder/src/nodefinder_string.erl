%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:

-module(nodefinder_string).

%% external interface
-export([lowercase/1,
         pad/2,
         split/2,
         uppercase/1]).

-include("nodefinder.hrl").

-spec lowercase(String :: string() | binary()) ->
    string() | binary().

-ifdef(ERLANG_OTP_VERSION_20_FEATURES).
lowercase(String) ->
    string:lowercase(String).
-else.
lowercase(String)
    when is_list(String) ->
    string:to_lower(String);
lowercase(String)
    when is_binary(String) ->
    erlang:list_to_binary(string:to_lower(erlang:binary_to_list(String))).
-endif.

-spec uppercase(String :: string() | binary()) ->
    string() | binary().

-ifdef(ERLANG_OTP_VERSION_20_FEATURES).
uppercase(String) ->
    string:uppercase(String).
-else.
uppercase(String)
    when is_list(String) ->
    string:to_upper(String);
uppercase(String)
    when is_binary(String) ->
    erlang:list_to_binary(string:to_upper(erlang:binary_to_list(String))).
-endif.

-spec split(String :: string() | binary(),
            SearchPattern :: string() | binary() | list(string() | binary())) ->
    list(string() | binary()).

-ifdef(ERLANG_OTP_VERSION_20_FEATURES).
split(String, SearchPattern) ->
    string:split(String, SearchPattern, all).
-else.
split(String, SearchPattern)
    when is_list(String) ->
    [erlang:binary_to_list(S)
     || S <- split(erlang:list_to_binary(String), SearchPattern)];
split(String, SearchPattern)
    when is_binary(String) ->
    Pattern = if
        is_binary(SearchPattern) ->
            [SearchPattern];
        is_integer(hd(SearchPattern)) ->
            [erlang:list_to_binary(SearchPattern)];
        is_list(SearchPattern) ->
            [erlang:iolist_to_binary(S) || S <- SearchPattern]
    end,
    binary:split(String, Pattern, [global]).
-endif.

-spec pad(String :: string() | binary(),
          Length :: non_neg_integer()) ->
    string() | binary().

-ifdef(ERLANG_OTP_VERSION_20_FEATURES).
pad(String, Length) ->
    lists:sublist(lists:flatten(string:pad(String, Length)), Length).
-else.
pad(String, Length)
    when is_list(String) ->
    string:left(String, Length);
pad(String)
    when is_binary(String) ->
    erlang:list_to_binary(string:left(erlang:binary_to_list(String),
                                      Length)).
-endif.


