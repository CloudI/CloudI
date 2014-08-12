-module(rest_expires_binary).

-export([init/3]).
-export([content_types_provided/2]).
-export([get_text_plain/2]).
-export([expires/2]).

init(_Transport, _Req, _Opts) ->
	{upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
	{[{{<<"text">>, <<"plain">>, []}, get_text_plain}], Req, State}.

get_text_plain(Req, State) ->
	{<<"This is REST!">>, Req, State}.

expires(Req, State) ->
	{<<"0">>, Req, State}.
