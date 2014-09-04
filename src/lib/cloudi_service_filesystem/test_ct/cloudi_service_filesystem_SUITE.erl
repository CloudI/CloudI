%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Service Filesystem Tests==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2014, Michael Truog <mjtruog at gmail dot com>
%%% All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%% 
%%%     * Redistributions of source code must retain the above copyright
%%%       notice, this list of conditions and the following disclaimer.
%%%     * Redistributions in binary form must reproduce the above copyright
%%%       notice, this list of conditions and the following disclaimer in
%%%       the documentation and/or other materials provided with the
%%%       distribution.
%%%     * All advertising materials mentioning features or use of this
%%%       software must display the following acknowledgment:
%%%         This product includes software developed by Michael Truog
%%%     * The name of the author may not be used to endorse or promote
%%%       products derived from this software without specific prior
%%%       written permission
%%% 
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
%%% CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
%%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
%%% OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
%%% CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
%%% DAMAGE.
%%%
%%% @author Michael Truog <mjtruog [at] gmail (dot) com>
%%% @copyright 2014 Michael Truog
%%% @version 1.3.3 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_filesystem_SUITE).

%% CT callbacks
-export([all/0,
         groups/0,
         suite/0,
         init_per_suite/1,
         end_per_suite/1,
         group/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

%% test callbacks
-export([t_filesystem_basic_read_1/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("cloudi_core/include/cloudi_logger.hrl").

-define(SERVICE_PREFIX1, "/filesystem/").
% ASCII_FILE created with: file:write_file("ASCII.bin", lists:seq(0, 126)).
-define(ASCII_FILE, "ASCII.bin").

%%%------------------------------------------------------------------------
%%% Callback functions from CT
%%%------------------------------------------------------------------------

all() ->
    [{group, filesystem_basic_1}].

groups() ->
    [{filesystem_basic_1, [],
      [t_filesystem_basic_read_1]}].

suite() ->
    [{ct_hooks, [cth_surefire]},
     {timetrap, 10100}].

init_per_suite(Config) ->
    ok = cloudi_x_reltool_util:application_start(cloudi_core, [], infinity),
    Config.

end_per_suite(_Config) ->
    ok = cloudi_x_reltool_util:application_stop(cloudi_core),
    ok.

group(_GroupName) ->
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(TestCase, Config)
    when (TestCase =:= t_filesystem_basic_read_1) ->
    {ok, ServiceIds} = cloudi_service_api:services_add([
        % using proplist configuration format, not the tuple/record format
        [{prefix, ?SERVICE_PREFIX1},
         {module, cloudi_service_filesystem},
         {args, [{directory, "${TEST_DIR}"}]},
         {dest_refresh, none},
         {timeout_init, 10000}]
        ], infinity),
    [{service_ids, ServiceIds} | Config].

end_per_testcase(TestCase, Config) ->
    {value, {_, ServiceIds}, NewConfig} = lists:keytake(service_ids, 1, Config),
    ok = cloudi_service_api:services_remove(ServiceIds, infinity),
    NewConfig.

%%%------------------------------------------------------------------------
%%% test cases
%%%------------------------------------------------------------------------

t_filesystem_basic_read_1(_Config) ->
    Context = cloudi:new(),
    ServiceNameHead = ?SERVICE_PREFIX1 ++ ?ASCII_FILE ++ "/head",
    ServiceNameGet = ?SERVICE_PREFIX1 ++ ?ASCII_FILE ++ "/get",
    ASCII = erlang:list_to_binary(lists:seq(0, 126)),
    Request = <<>>,
    Timeout = undefined, % default
    Priority = undefined, % default
    {ok,
     [{<<"content-disposition">>,
       <<"attachment; filename=\"ASCII.bin\"">>},
      {<<"content-type">>, <<"application/octet-stream">>},
      {<<"etag">>, ETag},
      {<<"last-modified">>, LastModified},
      {<<"date">>, _},
      {<<"accept-ranges">>, <<"bytes">>}],
     ASCII} = cloudi:send_sync(Context, ServiceNameGet,
                               <<>>, Request,
                               Timeout, Priority),
    RequestInfo1 = [{<<"range">>, 
                     % "Hello World!" test
                     <<"bytes="
                       "72-72,101-101,108-108,108-108,111-111,"
                       "32-32,"
                       "87-87,111-111,114-114,108-108,100-100,"
                       "33-33">>}],
    {ok,
     [{<<"status">>, <<"206">>},
      {<<"content-type">>, <<"multipart/byteranges">>},
      {<<"etag">>, ETag},
      {<<"last-modified">>, LastModified},
      {<<"date">>, _},
      {<<"accept-ranges">>, <<"bytes">>}],
     <<>>} = cloudi:send_sync(Context, ServiceNameHead,
                              RequestInfo1, Request,
                              Timeout, Priority),
    BSize = 512, % 64 characters for a boundary, from cowlib
    {ok,
     [{<<"status">>, <<"206">>},
      {<<"content-type">>,
       <<"multipart/byteranges; boundary=", Boundary1:BSize/bitstring>>},
      {<<"etag">>, ETag},
      {<<"last-modified">>, LastModified},
      {<<"date">>, _},
      {<<"accept-ranges">>, <<"bytes">>}],
     <<"\r\n--", Boundary1:BSize/bitstring,
       "\r\ncontent-range: bytes 72-72/127\r\n\r\n"
       "H"
       "\r\n--", Boundary1:BSize/bitstring,
       "\r\ncontent-range: bytes 101-101/127\r\n\r\n"
       "e"
       "\r\n--", Boundary1:BSize/bitstring,
       "\r\ncontent-range: bytes 108-108/127\r\n\r\n"
       "l"
       "\r\n--", Boundary1:BSize/bitstring,
       "\r\ncontent-range: bytes 108-108/127\r\n\r\n"
       "l"
       "\r\n--", Boundary1:BSize/bitstring,
       "\r\ncontent-range: bytes 111-111/127\r\n\r\n"
       "o"
       "\r\n--", Boundary1:BSize/bitstring,
       "\r\ncontent-range: bytes 32-32/127\r\n\r\n"
       " "
       "\r\n--", Boundary1:BSize/bitstring,
       "\r\ncontent-range: bytes 87-87/127\r\n\r\n"
       "W"
       "\r\n--", Boundary1:BSize/bitstring,
       "\r\ncontent-range: bytes 111-111/127\r\n\r\n"
       "o"
       "\r\n--", Boundary1:BSize/bitstring,
       "\r\ncontent-range: bytes 114-114/127\r\n\r\n"
       "r"
       "\r\n--", Boundary1:BSize/bitstring,
       "\r\ncontent-range: bytes 108-108/127\r\n\r\n"
       "l"
       "\r\n--", Boundary1:BSize/bitstring,
       "\r\ncontent-range: bytes 100-100/127\r\n\r\n"
       "d"
       "\r\n--", Boundary1:BSize/bitstring,
       "\r\ncontent-range: bytes 33-33/127\r\n\r\n"
       "!"
       "\r\n--", Boundary1:BSize/bitstring,
       "--\r\n">>} = cloudi:send_sync(Context, ServiceNameGet,
                                      RequestInfo1, Request,
                                      Timeout, Priority),
    RequestInfo2 = [{<<"range">>, 
                     % "abc" test
                     <<"bytes="
                       "97-99">>},
                    {<<"if-range">>, ETag}],
    {ok,
     [{<<"status">>, <<"206">>},
      {<<"content-type">>,<<"application/octet-stream">>},
      {<<"content-range">>, <<"bytes 97-99/127">>},
      {<<"etag">>, ETag},
      {<<"last-modified">>, LastModified},
      {<<"date">>, _},
      {<<"accept-ranges">>, <<"bytes">>}],
     <<"abc">>} = cloudi:send_sync(Context, ServiceNameGet,
                                   RequestInfo2, Request,
                                   Timeout, Priority),
    RequestInfo3 = [{<<"range">>, 
                     % "abc" test
                     <<"bytes="
                       "97-99">>},
                    {<<"if-range">>, LastModified}],
    {ok,
     [{<<"status">>, <<"206">>},
      {<<"content-type">>,<<"application/octet-stream">>},
      {<<"content-range">>, <<"bytes 97-99/127">>},
      {<<"etag">>, ETag},
      {<<"last-modified">>, LastModified},
      {<<"date">>, _},
      {<<"accept-ranges">>, <<"bytes">>}],
     <<"abc">>} = cloudi:send_sync(Context, ServiceNameGet,
                                   RequestInfo3, Request,
                                   Timeout, Priority),
    RequestInfo4 = [{<<"range">>, 
                     % "abc" test
                     <<"bytes="
                       "97-99">>},
                    {<<"if-range">>, <<ETag/binary, (<<"x">>)/binary>>}],
    {ok,
     [{<<"content-disposition">>,
       <<"attachment; filename=\"ASCII.bin\"">>},
      {<<"content-type">>, <<"application/octet-stream">>},
      {<<"etag">>, ETag},
      {<<"last-modified">>, LastModified},
      {<<"date">>, _},
      {<<"accept-ranges">>, <<"bytes">>}],
     % bad ETag provides the whole ASCII file
     ASCII} = cloudi:send_sync(Context, ServiceNameGet,
                               RequestInfo4, Request,
                               Timeout, Priority),
    % alter the last-modified date to have the wrong seconds value
    <<LastModified0:184/bitstring,
      LastModified1:16/bitstring,
      LastModified2:32/bitstring>> = LastModified,
    LastModified1New = erlang:integer_to_binary(
        (erlang:binary_to_integer(LastModified1) + 1) rem 60),
    LastModifiedFake = <<LastModified0/binary,
                         LastModified1New/binary,
                         LastModified2/binary>>,
    RequestInfo5 = [{<<"range">>, 
                     % "abc" test
                     <<"bytes="
                       "97-99">>},
                    {<<"if-range">>, LastModifiedFake}],
    {ok,
     [{<<"content-disposition">>,
       <<"attachment; filename=\"ASCII.bin\"">>},
      {<<"content-type">>, <<"application/octet-stream">>},
      {<<"etag">>, ETag},
      {<<"last-modified">>, LastModified},
      {<<"date">>, _},
      {<<"accept-ranges">>, <<"bytes">>}],
     % bad LastModified provides the whole ASCII file
     ASCII} = cloudi:send_sync(Context, ServiceNameGet,
                               RequestInfo5, Request,
                               Timeout, Priority),
    RequestInfo6 = [{<<"range">>, 
                     % "{|}~" test
                     <<"bytes="
                       "-4">>}],
    {ok,
     [{<<"status">>, <<"206">>},
      {<<"content-type">>,<<"application/octet-stream">>},
      {<<"content-range">>, <<"bytes 123-126/127">>},
      {<<"etag">>, ETag},
      {<<"last-modified">>, LastModified},
      {<<"date">>, _},
      {<<"accept-ranges">>, <<"bytes">>}],
     <<"{|}~">>} = cloudi:send_sync(Context, ServiceNameGet,
                                    RequestInfo6, Request,
                                    Timeout, Priority),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

