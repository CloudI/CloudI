-module(rest_forbidden_resource).
-export([init/3, rest_init/2, allowed_methods/2, forbidden/2,
		content_types_provided/2, content_types_accepted/2,
		to_text/2, from_text/2]).

init(_Transport, _Req, _Opts) ->
	{upgrade, protocol, cowboy_rest}.

rest_init(Req, [Forbidden]) ->
	{ok, Req, Forbidden}.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"HEAD">>, <<"POST">>], Req, State}.

forbidden(Req, State=true) ->
	{true, Req, State};
forbidden(Req, State=false) ->
	{false, Req, State}.

content_types_provided(Req, State) ->
	{[{{<<"text">>, <<"plain">>, []}, to_text}], Req, State}.

content_types_accepted(Req, State) ->
	{[{{<<"text">>, <<"plain">>, []}, from_text}], Req, State}.

to_text(Req, State) ->
	{<<"This is REST!">>, Req, State}.

from_text(Req, State) ->
	{Path, Req2} = cowboy_req:path(Req),
	{{true, Path}, Req2, State}.
