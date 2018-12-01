%% This module crashes on request input data
%% depending on the given option.

-module(input_crash_h).

-export([init/3]).

init(_, Req, content_length) ->
	cowboy1_error_h:ignore(cow1_http_hd, number, 2),
	cowboy1_req:parse_header(<<"content-length">>, Req).
