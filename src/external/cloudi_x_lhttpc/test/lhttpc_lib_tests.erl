%%% -*- coding: latin-1 -*-
%%% ----------------------------------------------------------------------------
%%% Copyright (c) 2009, Erlang Training and Consulting Ltd.
%%% All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%    * Redistributions of source code must retain the above copyright
%%%      notice, this list of conditions and the following disclaimer.
%%%    * Redistributions in binary form must reproduce the above copyright
%%%      notice, this list of conditions and the following disclaimer in the
%%%      documentation and/or other materials provided with the distribution.
%%%    * Neither the name of Erlang Training and Consulting Ltd. nor the
%%%      names of its contributors may be used to endorse or promote products
%%%      derived from this software without specific prior written permission.
%%% 
%%% THIS SOFTWARE IS PROVIDED BY Erlang Training and Consulting Ltd. ''AS IS''
%%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL Erlang Training and Consulting Ltd. BE
%%% LIABLE SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
%%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
%%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
%%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%% ----------------------------------------------------------------------------

%%% @author Oscar Hellström <oscar@hellstrom.st>
-module(lhttpc_lib_tests).

-include("../include/lhttpc_types.hrl").
-include("../include/lhttpc.hrl").
-include_lib("eunit/include/eunit.hrl").

parse_url_test_() ->
    [
        ?_assertEqual(#lhttpc_url{
                         host = "host",
                         port = 80,
                         path = "/",
                         is_ssl = false,
                         user = "",
                         password = ""
                        },
                      lhttpc_lib:parse_url("http://host")),

        ?_assertEqual(#lhttpc_url{
                         host = "host",
                         port = 80,
                         path = "/",
                         is_ssl = false,
                         user = "",
                         password = ""
                        },
                      lhttpc_lib:parse_url("http://host/")),

        ?_assertEqual(#lhttpc_url{
                         host = "host",
                         port = 443,
                         path = "/",
                         is_ssl = true,
                         user = "",
                         password = ""
                        },
                      lhttpc_lib:parse_url("https://host")),

        ?_assertEqual(#lhttpc_url{
                         host = "host",
                         port = 443,
                         path = "/",
                         is_ssl = true,
                         user = "",
                         password = ""
                        },
                      lhttpc_lib:parse_url("https://host/")),

        ?_assertEqual(#lhttpc_url{
                         host = "host",
                         port = 180,
                         path = "/",
                         is_ssl = false,
                         user = "",
                         password = ""
                        },
                      lhttpc_lib:parse_url("http://host:180")),

        ?_assertEqual(#lhttpc_url{
                         host = "host",
                         port = 180,
                         path = "/",
                         is_ssl = false,
                         user = "",
                         password = ""
                        },
                      lhttpc_lib:parse_url("http://host:180/")),

        ?_assertEqual(#lhttpc_url{
                         host = "host",
                         port = 180,
                         path = "/foo",
                         is_ssl = false,
                         user = "",
                         password = ""
                        },
                      lhttpc_lib:parse_url("http://host:180/foo")),

        ?_assertEqual(#lhttpc_url{
                         host = "host",
                         port = 180,
                         path = "/foo/bar",
                         is_ssl = false,
                         user = "",
                         password = ""
                        },
                      lhttpc_lib:parse_url("http://host:180/foo/bar")),

        ?_assertEqual(#lhttpc_url{
                         host = "host",
                         port = 180,
                         path = "/foo/bar",
                         is_ssl = false,
                         user = "joe",
                         password = "erlang"
                        },
                      lhttpc_lib:parse_url("http://joe:erlang@host:180/foo/bar")),


        ?_assertEqual(#lhttpc_url{
                         host = "host",
                         port = 180,
                         path = "/foo/bar",
                         is_ssl = false,
                         user = "joe",
                         password = "erl@ng"
                        },
                      lhttpc_lib:parse_url("http://joe:erl%40ng@host:180/foo/bar")),

        ?_assertEqual(#lhttpc_url{
                         host = "host",
                         port = 180,
                         path = "/foo/bar",
                         is_ssl = false,
                         user = "joe",
                         password = ""
                        },
                      lhttpc_lib:parse_url("http://joe@host:180/foo/bar")),

        ?_assertEqual(#lhttpc_url{
                         host = "host",
                         port = 180,
                         path = "/foo/bar",
                         is_ssl = false,
                         user = "",
                         password = ""
                        },
                      lhttpc_lib:parse_url("http://@host:180/foo/bar")),

        ?_assertEqual(#lhttpc_url{
                         host = "host",
                         port = 180,
                         path = "/foo/bar",
                         is_ssl = false,
                         user = "joe:arm",
                         password = "erlang"
                        },
                      lhttpc_lib:parse_url("http://joe%3Aarm:erlang@host:180/foo/bar")),

        ?_assertEqual(#lhttpc_url{
                         host = "host",
                         port = 180,
                         path = "/foo/bar",
                         is_ssl = false,
                         user = "joe:arm",
                         password = "erlang/otp"
                        },
                      lhttpc_lib:parse_url("http://joe%3aarm:erlang%2Fotp@host:180/foo/bar")),

        ?_assertEqual(#lhttpc_url{
                         host = "::1",
                         port = 80,
                         path = "/foo/bar",
                         is_ssl = false,
                         user = "",
                         password = ""
                        },
                      lhttpc_lib:parse_url("http://[::1]/foo/bar")),

        ?_assertEqual(#lhttpc_url{
                         host = "::1",
                         port = 180,
                         path = "/foo/bar",
                         is_ssl = false,
                         user = "",
                         password = ""
                        },
                      lhttpc_lib:parse_url("http://[::1]:180/foo/bar")),

        ?_assertEqual(#lhttpc_url{
                         host = "::1",
                         port = 180,
                         path = "/foo/bar",
                         is_ssl = false,
                         user = "joe",
                         password = "erlang"
                        },
                      lhttpc_lib:parse_url("http://joe:erlang@[::1]:180/foo/bar")),

        ?_assertEqual(#lhttpc_url{
                         host = "1080:0:0:0:8:800:200c:417a",
                         port = 180,
                         path = "/foo/bar",
                         is_ssl = false,
                         user = "joe",
                         password = "erlang"
                        },
                      lhttpc_lib:parse_url("http://joe:erlang@[1080:0:0:0:8:800:200C:417A]:180/foo/bar")),

        ?_assertEqual(#lhttpc_url{
                         host = "www.example.com",
                         port = 80,
                         path = "/?a=b",
                         is_ssl = false,
                         user = "",
                         password = ""
                        },
                      lhttpc_lib:parse_url("http://www.example.com?a=b"))
    ].
