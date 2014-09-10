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
-export([t_filesystem_basic_read_1/1,
         t_filesystem_basic_read_wcache_1/1,
         t_filesystem_basic_write_truncate_1/1,
         t_filesystem_basic_write_truncate_wcache_1/1,
         t_filesystem_basic_write_append_1/1,
         t_filesystem_basic_write_append_wcache_1/1,
         t_filesystem_basic_read_cache_1/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("cloudi_core/include/cloudi_logger.hrl").

-define(SERVICE_PREFIX1, "/filesystem/").
% ASCII_FILE created with: file:write_file("ASCII.bin", lists:seq(0, 126)).
-define(ASCII_FILE, "ASCII.bin").
-define(WRITABLE_DIRECTORY, "/tmp/").
-define(WRITABLE_FILENAME, "cloudi_service_filesystem_test.txt").
-define(WRITABLE_FILEPATH, ?WRITABLE_DIRECTORY ?WRITABLE_FILENAME).
-define(TIMEOUT, 480000).
-define(REFRESH, 1200). % ((?TIMEOUT * 2.5) div 1000)
-define(REFRESH_STRING, "1200").

%%%------------------------------------------------------------------------
%%% Callback functions from CT
%%%------------------------------------------------------------------------

all() ->
    [{group, filesystem_basic_1}].

groups() ->
    [{filesystem_basic_1, [],
      [t_filesystem_basic_read_1,
       t_filesystem_basic_read_wcache_1,
       t_filesystem_basic_write_truncate_1,
       t_filesystem_basic_write_truncate_wcache_1,
       t_filesystem_basic_write_append_1,
       t_filesystem_basic_write_append_wcache_1,
       t_filesystem_basic_read_cache_1]}].

suite() ->
    [{ct_hooks, [cth_surefire]},
     {timetrap, ?TIMEOUT * 2 + 100}].

init_per_suite(Config) ->
    ?REFRESH = erlang:round(?TIMEOUT * 2.5) div 1000,
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
         {args,
          [{directory, "${TEST_DIR}"}]},
         {dest_refresh, none},
         {timeout_init, ?TIMEOUT},
         {timeout_sync, ?TIMEOUT}]
        ], infinity),
    [{service_ids, ServiceIds} | Config];
init_per_testcase(TestCase, Config)
    when (TestCase =:= t_filesystem_basic_read_wcache_1) ->
    {ok, ServiceIds} = cloudi_service_api:services_add([
        % using proplist configuration format, not the tuple/record format
        [{prefix, ?SERVICE_PREFIX1},
         {module, cloudi_service_filesystem},
         {args,
          [{directory, "${TEST_DIR}"},
           {cache, 1},
           {refresh, ?REFRESH}]},
         {dest_refresh, none},
         {timeout_init, ?TIMEOUT},
         {timeout_sync, ?TIMEOUT}]
        ], infinity),
    [{service_ids, ServiceIds} | Config];
init_per_testcase(TestCase, Config)
    when (TestCase =:= t_filesystem_basic_write_truncate_1) ->
    ok = file:write_file(?WRITABLE_FILEPATH, <<>>),
    {ok, ServiceIds} = cloudi_service_api:services_add([
        % using proplist configuration format, not the tuple/record format
        [{prefix, ?SERVICE_PREFIX1},
         {module, cloudi_service_filesystem},
         {args,
          [{directory, ?WRITABLE_DIRECTORY},
           {write_truncate,
            [?SERVICE_PREFIX1 ?WRITABLE_FILENAME]}]},
         {dest_refresh, none},
         {timeout_init, ?TIMEOUT},
         {timeout_sync, ?TIMEOUT}]
        ], infinity),
    [{service_ids, ServiceIds} | Config];
init_per_testcase(TestCase, Config)
    when (TestCase =:= t_filesystem_basic_write_truncate_wcache_1) ->
    ok = file:write_file(?WRITABLE_FILEPATH, <<>>),
    {ok, ServiceIds} = cloudi_service_api:services_add([
        % using proplist configuration format, not the tuple/record format
        [{prefix, ?SERVICE_PREFIX1},
         {module, cloudi_service_filesystem},
         {args,
          [{directory, ?WRITABLE_DIRECTORY},
           {cache, 1},
           {refresh, ?REFRESH},
           {write_truncate,
            [?SERVICE_PREFIX1 ?WRITABLE_FILENAME]}]},
         {dest_refresh, none},
         {timeout_init, ?TIMEOUT},
         {timeout_sync, ?TIMEOUT}]
        ], infinity),
    [{service_ids, ServiceIds} | Config];
init_per_testcase(TestCase, Config)
    when (TestCase =:= t_filesystem_basic_write_append_1) ->
    ok = file:write_file(?WRITABLE_FILEPATH, <<>>),
    {ok, ServiceIds} = cloudi_service_api:services_add([
        % using proplist configuration format, not the tuple/record format
        [{prefix, ?SERVICE_PREFIX1},
         {module, cloudi_service_filesystem},
         {args,
          [{directory, ?WRITABLE_DIRECTORY},
           {write_append,
            [?SERVICE_PREFIX1 ?WRITABLE_FILENAME]}]},
         {dest_refresh, none},
         {timeout_init, ?TIMEOUT},
         {timeout_sync, ?TIMEOUT}]
        ], infinity),
    [{service_ids, ServiceIds} | Config];
init_per_testcase(TestCase, Config)
    when (TestCase =:= t_filesystem_basic_write_append_wcache_1) ->
    ok = file:write_file(?WRITABLE_FILEPATH, <<>>),
    {ok, ServiceIds} = cloudi_service_api:services_add([
        % using proplist configuration format, not the tuple/record format
        [{prefix, ?SERVICE_PREFIX1},
         {module, cloudi_service_filesystem},
         {args,
          [{directory, ?WRITABLE_DIRECTORY},
           {cache, 1},
           {refresh, ?REFRESH},
           {write_append,
            [?SERVICE_PREFIX1 ?WRITABLE_FILENAME]}]},
         {dest_refresh, none},
         {timeout_init, ?TIMEOUT},
         {timeout_sync, ?TIMEOUT}]
        ], infinity),
    [{service_ids, ServiceIds} | Config];
init_per_testcase(TestCase, Config)
    when (TestCase =:= t_filesystem_basic_read_cache_1) ->
    {ok, ServiceIds} = cloudi_service_api:services_add([
        % using proplist configuration format, not the tuple/record format
        [{prefix, ?SERVICE_PREFIX1},
         {module, cloudi_service_filesystem},
         {args,
          [{directory, "${TEST_DIR}"},
           {cache, ?REFRESH},
           {refresh, ?REFRESH}]},
         {dest_refresh, none},
         {timeout_init, ?TIMEOUT},
         {timeout_sync, ?TIMEOUT}]
        ], infinity),
    [{service_ids, ServiceIds} | Config].

end_per_testcase(TestCase, Config) ->
    {value, {_, ServiceIds}, NewConfig} = lists:keytake(service_ids, 1, Config),
    ok = cloudi_service_api:services_remove(ServiceIds, infinity),
    if
        TestCase =:= t_filesystem_basic_write_truncate_1;
        TestCase =:= t_filesystem_basic_write_truncate_wcache_1;
        TestCase =:= t_filesystem_basic_write_append_1;
        TestCase =:= t_filesystem_basic_write_append_wcache_1 ->
            ok = file:delete(?WRITABLE_FILEPATH);
        true ->
            ok
    end,
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

t_filesystem_basic_read_wcache_1(_Config) ->
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
      {<<"cache-control">>, <<"public,max-age=1">>},
      {<<"expires">>, _},
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
      {<<"cache-control">>, <<"public,max-age=1">>},
      {<<"expires">>, _},
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
      {<<"cache-control">>, <<"public,max-age=1">>},
      {<<"expires">>, _},
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
      {<<"cache-control">>, <<"public,max-age=1">>},
      {<<"expires">>, _},
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
      {<<"cache-control">>, <<"public,max-age=1">>},
      {<<"expires">>, _},
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
      {<<"cache-control">>, <<"public,max-age=1">>},
      {<<"expires">>, _},
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
      {<<"cache-control">>, <<"public,max-age=1">>},
      {<<"expires">>, _},
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
      {<<"cache-control">>, <<"public,max-age=1">>},
      {<<"expires">>, _},
      {<<"last-modified">>, LastModified},
      {<<"date">>, _},
      {<<"accept-ranges">>, <<"bytes">>}],
     <<"{|}~">>} = cloudi:send_sync(Context, ServiceNameGet,
                                    RequestInfo6, Request,
                                    Timeout, Priority),
    ok.

t_filesystem_basic_write_truncate_1(_Config) ->
    Context = cloudi:new(),
    ServiceNameGet = ?SERVICE_PREFIX1 ++ ?WRITABLE_FILENAME ++ "/get",
    ServiceNamePut = ?SERVICE_PREFIX1 ++ ?WRITABLE_FILENAME ++ "/put",
    Timeout = undefined, % default
    Priority = undefined, % default
    {ok, <<>>} = file:read_file(?WRITABLE_FILEPATH),
    {ok,
     [{<<"content-disposition">>,
       <<"attachment; filename=\"" ?WRITABLE_FILENAME "\"">>},
      {<<"content-type">>, <<"text/plain">>},
      {<<"etag">>, ETag0},
      {<<"last-modified">>, LastModified0},
      {<<"date">>, _},
      {<<"accept-ranges">>, <<"bytes">>}],
     <<>>} = cloudi:send_sync(Context, ServiceNameGet,
                              <<>>, <<>>,
                              Timeout, Priority),
    Response1 = <<"Hello">>,
    {ok,
     [{<<"content-disposition">>,
       <<"attachment; filename=\"" ?WRITABLE_FILENAME "\"">>},
      {<<"content-type">>, <<"text/plain">>},
      {<<"etag">>, ETag1},
      {<<"last-modified">>, LastModified1},
      {<<"date">>, _},
      {<<"accept-ranges">>, <<"bytes">>}],
     Response1} = cloudi:send_sync(Context, ServiceNamePut,
                                   <<>>, Response1,
                                   Timeout, Priority),
    Response2 = <<"Hello World!">>,
    {ok,
     [{<<"content-disposition">>,
       <<"attachment; filename=\"" ?WRITABLE_FILENAME "\"">>},
      {<<"content-type">>, <<"text/plain">>},
      {<<"etag">>, ETag2},
      {<<"last-modified">>, LastModified2},
      {<<"date">>, _},
      {<<"accept-ranges">>, <<"bytes">>}],
     Response2} = cloudi:send_sync(Context, ServiceNamePut,
                                   <<>>, Response2,
                                   Timeout, Priority),
    true = if
        LastModified0 < LastModified1 ->
            ETag0 /= ETag1;
        LastModified0 == LastModified1 ->
            ETag0 == ETag1
    end,
    true = if
        LastModified1 < LastModified2 ->
            ETag1 /= ETag2;
        LastModified1 == LastModified2 ->
            ETag1 == ETag2
    end,
    {ok, Response2} = file:read_file(?WRITABLE_FILEPATH),
    {ok,
     [{<<"status">>, <<"400">>}],
     <<>>} = cloudi:send_sync(Context, ServiceNamePut,
                              [{<<"range">>, <<"bytes=-6">>}], <<"Error!">>,
                              Timeout, Priority),
    ok.

t_filesystem_basic_write_truncate_wcache_1(_Config) ->
    Context = cloudi:new(),
    ServiceNameGet = ?SERVICE_PREFIX1 ++ ?WRITABLE_FILENAME ++ "/get",
    ServiceNamePut = ?SERVICE_PREFIX1 ++ ?WRITABLE_FILENAME ++ "/put",
    Timeout = undefined, % default
    Priority = undefined, % default
    {ok, <<>>} = file:read_file(?WRITABLE_FILEPATH),
    {ok,
     [{<<"content-disposition">>,
       <<"attachment; filename=\"" ?WRITABLE_FILENAME "\"">>},
      {<<"content-type">>, <<"text/plain">>},
      {<<"etag">>, ETag0},
      {<<"cache-control">>, <<"public,max-age=1">>},
      {<<"expires">>, _},
      {<<"last-modified">>, LastModified0},
      {<<"date">>, _},
      {<<"accept-ranges">>, <<"bytes">>}],
     <<>>} = cloudi:send_sync(Context, ServiceNameGet,
                              <<>>, <<>>,
                              Timeout, Priority),
    Response1 = <<"Hello">>,
    {ok,
     [{<<"content-disposition">>,
       <<"attachment; filename=\"" ?WRITABLE_FILENAME "\"">>},
      {<<"content-type">>, <<"text/plain">>},
      {<<"etag">>, ETag1},
      {<<"cache-control">>, <<"public,max-age=1">>},
      {<<"expires">>, _},
      {<<"last-modified">>, LastModified1},
      {<<"date">>, _},
      {<<"accept-ranges">>, <<"bytes">>}],
     Response1} = cloudi:send_sync(Context, ServiceNamePut,
                                   <<>>, Response1,
                                   Timeout, Priority),
    Response2 = <<"Hello World!">>,
    {ok,
     [{<<"content-disposition">>,
       <<"attachment; filename=\"" ?WRITABLE_FILENAME "\"">>},
      {<<"content-type">>, <<"text/plain">>},
      {<<"etag">>, ETag2},
      {<<"cache-control">>, <<"public,max-age=1">>},
      {<<"expires">>, _},
      {<<"last-modified">>, LastModified2},
      {<<"date">>, _},
      {<<"accept-ranges">>, <<"bytes">>}],
     Response2} = cloudi:send_sync(Context, ServiceNamePut,
                                   <<>>, Response2,
                                   Timeout, Priority),
    true = if
        LastModified0 < LastModified1 ->
            ETag0 /= ETag1;
        LastModified0 == LastModified1 ->
            ETag0 == ETag1
    end,
    true = if
        LastModified1 < LastModified2 ->
            ETag1 /= ETag2;
        LastModified1 == LastModified2 ->
            ETag1 == ETag2
    end,
    {ok, Response2} = file:read_file(?WRITABLE_FILEPATH),
    {ok,
     [{<<"status">>, <<"400">>}],
     <<>>} = cloudi:send_sync(Context, ServiceNamePut,
                              [{<<"range">>, <<"bytes=-6">>}], <<"Error!">>,
                              Timeout, Priority),
    ok.

t_filesystem_basic_write_append_1(_Config) ->
    Context = cloudi:new(),
    ServiceNameGet = ?SERVICE_PREFIX1 ++ ?WRITABLE_FILENAME ++ "/get",
    ServiceNamePost = ?SERVICE_PREFIX1 ++ ?WRITABLE_FILENAME ++ "/post",
    Timeout = undefined, % default
    Priority = undefined, % default
    {ok, <<>>} = file:read_file(?WRITABLE_FILEPATH),
    {ok,
     [{<<"content-disposition">>,
       <<"attachment; filename=\"" ?WRITABLE_FILENAME "\"">>},
      {<<"content-type">>, <<"text/plain">>},
      {<<"etag">>, ETag0},
      {<<"last-modified">>, LastModified0},
      {<<"date">>, _},
      {<<"accept-ranges">>, <<"bytes">>}],
     <<>>} = cloudi:send_sync(Context, ServiceNameGet,
                              <<>>, <<>>,
                              Timeout, Priority),
    Request1 = <<"The quick brown fox tripped">>,
    {ok,
     [{<<"content-disposition">>,
       <<"attachment; filename=\"" ?WRITABLE_FILENAME "\"">>},
      {<<"content-type">>, <<"text/plain">>},
      {<<"etag">>, ETag1},
      {<<"last-modified">>, LastModified1},
      {<<"date">>, _},
      {<<"accept-ranges">>, <<"bytes">>}],
     Request1} = cloudi:send_sync(Context, ServiceNamePost,
                                  <<>>, Request1,
                                  Timeout, Priority),

    RequestInfo2 = [{<<"range">>, 
                     <<"bytes="
                       "-7">>}],
    Request2 = <<"jumps over the lazy dog">>,
    Response2 = <<"The quick brown fox jumps over the lazy dog">>,
    {ok,
     [{<<"content-disposition">>,
       <<"attachment; filename=\"" ?WRITABLE_FILENAME "\"">>},
      {<<"content-type">>, <<"text/plain">>},
      {<<"etag">>, ETag2},
      {<<"last-modified">>, LastModified2},
      {<<"date">>, _},
      {<<"accept-ranges">>, <<"bytes">>}],
     Response2} = cloudi:send_sync(Context, ServiceNamePost,
                                   RequestInfo2, Request2,
                                   Timeout, Priority),
    true = if
        LastModified0 < LastModified1 ->
            ETag0 /= ETag1;
        LastModified0 == LastModified1 ->
            ETag0 == ETag1
    end,
    true = if
        LastModified1 < LastModified2 ->
            ETag1 /= ETag2;
        LastModified1 == LastModified2 ->
            ETag1 == ETag2
    end,
    RequestInfo3 = [{<<"range">>, 
                     <<"bytes="
                       "10-14">>}],
    Request3 = <<"red">>,
    Response3 = <<"The quick red fox jumps over the lazy dog">>,
    {ok,
     [{<<"content-disposition">>,
       <<"attachment; filename=\"" ?WRITABLE_FILENAME "\"">>},
      {<<"content-type">>, <<"text/plain">>},
      {<<"etag">>, ETag3},
      {<<"last-modified">>, LastModified3},
      {<<"date">>, _},
      {<<"accept-ranges">>, <<"bytes">>}],
     Response3} = cloudi:send_sync(Context, ServiceNamePost,
                                   RequestInfo3, Request3,
                                   Timeout, Priority),
    true = if
        LastModified2 < LastModified3 ->
            ETag2 /= ETag3;
        LastModified2 == LastModified3 ->
            ETag2 == ETag3
    end,
    RequestInfo4 = [{<<"range">>, 
                     <<"bytes="
                       "22-22">>}],
    Request4 = <<"ed">>,
    Response4 = <<"The quick red fox jumped over the lazy dog">>,
    {ok,
     [{<<"content-disposition">>,
       <<"attachment; filename=\"" ?WRITABLE_FILENAME "\"">>},
      {<<"content-type">>, <<"text/plain">>},
      {<<"etag">>, ETag4},
      {<<"last-modified">>, LastModified4},
      {<<"date">>, _},
      {<<"accept-ranges">>, <<"bytes">>}],
     Response4} = cloudi:send_sync(Context, ServiceNamePost,
                                   RequestInfo4, Request4,
                                   Timeout, Priority),
    true = if
        LastModified3 < LastModified4 ->
            ETag3 /= ETag4;
        LastModified3 == LastModified4 ->
            ETag3 == ETag4
    end,
    RequestInfo5 = [{<<"range">>, 
                     <<"bytes="
                       "42-42">>}],
    Request5 = <<"s.">>,
    Response5 = <<"The quick red fox jumped over the lazy dogs.">>,
    {ok,
     [{<<"content-disposition">>,
       <<"attachment; filename=\"" ?WRITABLE_FILENAME "\"">>},
      {<<"content-type">>, <<"text/plain">>},
      {<<"etag">>, ETag5},
      {<<"last-modified">>, LastModified5},
      {<<"date">>, _},
      {<<"accept-ranges">>, <<"bytes">>}],
     Response5} = cloudi:send_sync(Context, ServiceNamePost,
                                   RequestInfo5, Request5,
                                   Timeout, Priority),
    true = if
        LastModified4 < LastModified5 ->
            ETag4 /= ETag5;
        LastModified4 == LastModified5 ->
            ETag4 == ETag5
    end,
    RequestInfo6 = [{<<"range">>, 
                     <<"bytes="
                       "45-45">>}],
    {ok,
     [{<<"status">>, <<"416">>},
      {<<"content-range">>, <<"bytes */44">>},
      {<<"accept-ranges">>, <<"bytes">>}],
     <<>>} = cloudi:send_sync(Context, ServiceNamePost,
                              RequestInfo6, <<" Ignore this data!">>,
                              Timeout, Priority),
    RequestInfo7 = [{<<"range">>, 
                     <<"bytes="
                       "44-44">>}],
    Request7 = <<" The fox later became a Hollywood actor.">>,
    Response7 = <<"The quick red fox jumped over the lazy dogs.",
                  Request7/binary>>,
    {ok,
     [{<<"content-disposition">>,
       <<"attachment; filename=\"" ?WRITABLE_FILENAME "\"">>},
      {<<"content-type">>, <<"text/plain">>},
      {<<"etag">>, ETag7},
      {<<"last-modified">>, LastModified7},
      {<<"date">>, _},
      {<<"accept-ranges">>, <<"bytes">>}],
     Response7} = cloudi:send_sync(Context, ServiceNamePost,
                                   RequestInfo7, Request7,
                                   Timeout, Priority),
    true = if
        LastModified5 < LastModified7 ->
            ETag5 /= ETag7;
        LastModified5 == LastModified7 ->
            ETag5 == ETag7
    end,
    Request8 = <<" The end.">>,
    Response8 = <<Response7/binary,
                  Request8/binary>>,
    {ok,
     [{<<"content-disposition">>,
       <<"attachment; filename=\"" ?WRITABLE_FILENAME "\"">>},
      {<<"content-type">>, <<"text/plain">>},
      {<<"etag">>, ETag8},
      {<<"last-modified">>, LastModified8},
      {<<"date">>, _},
      {<<"accept-ranges">>, <<"bytes">>}],
     Response8} = cloudi:send_sync(Context, ServiceNamePost,
                                   <<>>, Request8,
                                   Timeout, Priority),
    true = if
        LastModified7 < LastModified8 ->
            ETag7 /= ETag8;
        LastModified7 == LastModified8 ->
            ETag7 == ETag8
    end,
    {ok, Response8} = file:read_file(?WRITABLE_FILEPATH),
    ok.

t_filesystem_basic_write_append_wcache_1(_Config) ->
    Context = cloudi:new(),
    ServiceNameGet = ?SERVICE_PREFIX1 ++ ?WRITABLE_FILENAME ++ "/get",
    ServiceNamePost = ?SERVICE_PREFIX1 ++ ?WRITABLE_FILENAME ++ "/post",
    Timeout = undefined, % default
    Priority = undefined, % default
    {ok, <<>>} = file:read_file(?WRITABLE_FILEPATH),
    {ok,
     [{<<"content-disposition">>,
       <<"attachment; filename=\"" ?WRITABLE_FILENAME "\"">>},
      {<<"content-type">>, <<"text/plain">>},
      {<<"etag">>, ETag0},
      {<<"cache-control">>, <<"public,max-age=1">>},
      {<<"expires">>, _},
      {<<"last-modified">>, LastModified0},
      {<<"date">>, _},
      {<<"accept-ranges">>, <<"bytes">>}],
     <<>>} = cloudi:send_sync(Context, ServiceNameGet,
                              <<>>, <<>>,
                              Timeout, Priority),
    Request1 = <<"The quick brown fox tripped">>,
    {ok,
     [{<<"content-disposition">>,
       <<"attachment; filename=\"" ?WRITABLE_FILENAME "\"">>},
      {<<"content-type">>, <<"text/plain">>},
      {<<"etag">>, ETag1},
      {<<"cache-control">>, <<"public,max-age=1">>},
      {<<"expires">>, _},
      {<<"last-modified">>, LastModified1},
      {<<"date">>, _},
      {<<"accept-ranges">>, <<"bytes">>}],
     Request1} = cloudi:send_sync(Context, ServiceNamePost,
                                  <<>>, Request1,
                                  Timeout, Priority),

    RequestInfo2 = [{<<"range">>, 
                     <<"bytes="
                       "-7">>}],
    Request2 = <<"jumps over the lazy dog">>,
    Response2 = <<"The quick brown fox jumps over the lazy dog">>,
    {ok,
     [{<<"content-disposition">>,
       <<"attachment; filename=\"" ?WRITABLE_FILENAME "\"">>},
      {<<"content-type">>, <<"text/plain">>},
      {<<"etag">>, ETag2},
      {<<"cache-control">>, <<"public,max-age=1">>},
      {<<"expires">>, _},
      {<<"last-modified">>, LastModified2},
      {<<"date">>, _},
      {<<"accept-ranges">>, <<"bytes">>}],
     Response2} = cloudi:send_sync(Context, ServiceNamePost,
                                   RequestInfo2, Request2,
                                   Timeout, Priority),
    true = if
        LastModified0 < LastModified1 ->
            ETag0 /= ETag1;
        LastModified0 == LastModified1 ->
            ETag0 == ETag1
    end,
    true = if
        LastModified1 < LastModified2 ->
            ETag1 /= ETag2;
        LastModified1 == LastModified2 ->
            ETag1 == ETag2
    end,
    RequestInfo3 = [{<<"range">>, 
                     <<"bytes="
                       "10-14">>}],
    Request3 = <<"red">>,
    Response3 = <<"The quick red fox jumps over the lazy dog">>,
    {ok,
     [{<<"content-disposition">>,
       <<"attachment; filename=\"" ?WRITABLE_FILENAME "\"">>},
      {<<"content-type">>, <<"text/plain">>},
      {<<"etag">>, ETag3},
      {<<"cache-control">>, <<"public,max-age=1">>},
      {<<"expires">>, _},
      {<<"last-modified">>, LastModified3},
      {<<"date">>, _},
      {<<"accept-ranges">>, <<"bytes">>}],
     Response3} = cloudi:send_sync(Context, ServiceNamePost,
                                   RequestInfo3, Request3,
                                   Timeout, Priority),
    true = if
        LastModified2 < LastModified3 ->
            ETag2 /= ETag3;
        LastModified2 == LastModified3 ->
            ETag2 == ETag3
    end,
    RequestInfo4 = [{<<"range">>, 
                     <<"bytes="
                       "22-22">>}],
    Request4 = <<"ed">>,
    Response4 = <<"The quick red fox jumped over the lazy dog">>,
    {ok,
     [{<<"content-disposition">>,
       <<"attachment; filename=\"" ?WRITABLE_FILENAME "\"">>},
      {<<"content-type">>, <<"text/plain">>},
      {<<"etag">>, ETag4},
      {<<"cache-control">>, <<"public,max-age=1">>},
      {<<"expires">>, _},
      {<<"last-modified">>, LastModified4},
      {<<"date">>, _},
      {<<"accept-ranges">>, <<"bytes">>}],
     Response4} = cloudi:send_sync(Context, ServiceNamePost,
                                   RequestInfo4, Request4,
                                   Timeout, Priority),
    true = if
        LastModified3 < LastModified4 ->
            ETag3 /= ETag4;
        LastModified3 == LastModified4 ->
            ETag3 == ETag4
    end,
    RequestInfo5 = [{<<"range">>, 
                     <<"bytes="
                       "42-42">>}],
    Request5 = <<"s.">>,
    Response5 = <<"The quick red fox jumped over the lazy dogs.">>,
    {ok,
     [{<<"content-disposition">>,
       <<"attachment; filename=\"" ?WRITABLE_FILENAME "\"">>},
      {<<"content-type">>, <<"text/plain">>},
      {<<"etag">>, ETag5},
      {<<"cache-control">>, <<"public,max-age=1">>},
      {<<"expires">>, _},
      {<<"last-modified">>, LastModified5},
      {<<"date">>, _},
      {<<"accept-ranges">>, <<"bytes">>}],
     Response5} = cloudi:send_sync(Context, ServiceNamePost,
                                   RequestInfo5, Request5,
                                   Timeout, Priority),
    true = if
        LastModified4 < LastModified5 ->
            ETag4 /= ETag5;
        LastModified4 == LastModified5 ->
            ETag4 == ETag5
    end,
    RequestInfo6 = [{<<"range">>, 
                     <<"bytes="
                       "45-45">>}],
    {ok,
     [{<<"status">>, <<"416">>},
      {<<"content-range">>, <<"bytes */44">>},
      {<<"accept-ranges">>, <<"bytes">>}],
     <<>>} = cloudi:send_sync(Context, ServiceNamePost,
                              RequestInfo6, <<" Ignore this data!">>,
                              Timeout, Priority),
    RequestInfo7 = [{<<"range">>, 
                     <<"bytes="
                       "44-44">>}],
    Request7 = <<" The fox later became a Hollywood actor.">>,
    Response7 = <<"The quick red fox jumped over the lazy dogs.",
                  Request7/binary>>,
    {ok,
     [{<<"content-disposition">>,
       <<"attachment; filename=\"" ?WRITABLE_FILENAME "\"">>},
      {<<"content-type">>, <<"text/plain">>},
      {<<"etag">>, ETag7},
      {<<"cache-control">>, <<"public,max-age=1">>},
      {<<"expires">>, _},
      {<<"last-modified">>, LastModified7},
      {<<"date">>, _},
      {<<"accept-ranges">>, <<"bytes">>}],
     Response7} = cloudi:send_sync(Context, ServiceNamePost,
                                   RequestInfo7, Request7,
                                   Timeout, Priority),
    true = if
        LastModified5 < LastModified7 ->
            ETag5 /= ETag7;
        LastModified5 == LastModified7 ->
            ETag5 == ETag7
    end,
    Request8 = <<" The end.">>,
    Response8 = <<Response7/binary,
                  Request8/binary>>,
    {ok,
     [{<<"content-disposition">>,
       <<"attachment; filename=\"" ?WRITABLE_FILENAME "\"">>},
      {<<"content-type">>, <<"text/plain">>},
      {<<"etag">>, ETag8},
      {<<"cache-control">>, <<"public,max-age=1">>},
      {<<"expires">>, _},
      {<<"last-modified">>, LastModified8},
      {<<"date">>, _},
      {<<"accept-ranges">>, <<"bytes">>}],
     Response8} = cloudi:send_sync(Context, ServiceNamePost,
                                   <<>>, Request8,
                                   Timeout, Priority),
    true = if
        LastModified7 < LastModified8 ->
            ETag7 /= ETag8;
        LastModified7 == LastModified8 ->
            ETag7 == ETag8
    end,
    {ok, Response8} = file:read_file(?WRITABLE_FILEPATH),
    ok.

t_filesystem_basic_read_cache_1(_Config) ->
    Context = cloudi:new(),
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
      {<<"cache-control">>, <<"public,max-age=" ?REFRESH_STRING>>},
      {<<"expires">>, _},
      {<<"last-modified">>, LastModified},
      {<<"date">>, _},
      {<<"accept-ranges">>, <<"bytes">>}],
     ASCII} = cloudi:send_sync(Context, ServiceNameGet,
                               <<>>, Request,
                               Timeout, Priority),
    RequestInfo1 = [{<<"if-none-match">>, ETag}],
    {ok,
     [{<<"status">>, <<"304">>},
      {<<"last-modified">>, LastModified},
      {<<"date">>, _},
      {<<"accept-ranges">>, <<"bytes">>}],
     <<>>} = cloudi:send_sync(Context, ServiceNameGet,
                              RequestInfo1, Request,
                              Timeout, Priority),
    InvalidETag = <<"invalid-etag">>,
    RequestInfo2 = [{<<"if-none-match">>, InvalidETag}],
    {ok,
     [{<<"content-disposition">>,
       <<"attachment; filename=\"ASCII.bin\"">>},
      {<<"content-type">>, <<"application/octet-stream">>},
      {<<"etag">>, ETag},
      {<<"cache-control">>, <<"public,max-age=" ?REFRESH_STRING>>},
      {<<"expires">>, _},
      {<<"last-modified">>, LastModified},
      {<<"date">>, _},
      {<<"accept-ranges">>, <<"bytes">>}],
     ASCII} = cloudi:send_sync(Context, ServiceNameGet,
                               RequestInfo2, Request,
                               Timeout, Priority),
    RequestInfo3 = [{<<"if-match">>, InvalidETag}],
    {ok,
     [{<<"status">>, <<"412">>},
      {<<"last-modified">>, LastModified},
      {<<"date">>, _},
      {<<"accept-ranges">>, <<"bytes">>}],
     <<>>} = cloudi:send_sync(Context, ServiceNameGet,
                              RequestInfo3, Request,
                              Timeout, Priority),
    RequestInfo4 = [{<<"if-match">>, ETag}],
    {ok,
     [{<<"content-disposition">>,
       <<"attachment; filename=\"ASCII.bin\"">>},
      {<<"content-type">>, <<"application/octet-stream">>},
      {<<"etag">>, ETag},
      {<<"cache-control">>, <<"public,max-age=" ?REFRESH_STRING>>},
      {<<"expires">>, _},
      {<<"last-modified">>, LastModified},
      {<<"date">>, _},
      {<<"accept-ranges">>, <<"bytes">>}],
     ASCII} = cloudi:send_sync(Context, ServiceNameGet,
                               RequestInfo4, Request,
                               Timeout, Priority),
    RequestInfo5 = [{<<"if-modified-since">>, LastModified}],
    {ok,
     [{<<"status">>, <<"304">>},
      {<<"last-modified">>, LastModified},
      {<<"date">>, _},
      {<<"accept-ranges">>, <<"bytes">>}],
     <<>>} = cloudi:send_sync(Context, ServiceNameGet,
                              RequestInfo5, Request,
                              Timeout, Priority),
    OldDate = <<"Mon, 01 Jan 1900 00:00:00 GMT">>,
    RequestInfo6 = [{<<"if-modified-since">>, OldDate}],
    {ok,
     [{<<"content-disposition">>,
       <<"attachment; filename=\"ASCII.bin\"">>},
      {<<"content-type">>, <<"application/octet-stream">>},
      {<<"etag">>, ETag},
      {<<"cache-control">>, <<"public,max-age=" ?REFRESH_STRING>>},
      {<<"expires">>, _},
      {<<"last-modified">>, LastModified},
      {<<"date">>, _},
      {<<"accept-ranges">>, <<"bytes">>}],
     ASCII} = cloudi:send_sync(Context, ServiceNameGet,
                               RequestInfo6, Request,
                               Timeout, Priority),
    RequestInfo7 = [{<<"if-unmodified-since">>, LastModified}],
    {ok,
     [{<<"status">>, <<"412">>},
      {<<"last-modified">>, LastModified},
      {<<"date">>, _},
      {<<"accept-ranges">>, <<"bytes">>}],
     <<>>} = cloudi:send_sync(Context, ServiceNameGet,
                              RequestInfo7, Request,
                              Timeout, Priority),
    RequestInfo8 = [{<<"if-unmodified-since">>, OldDate}],
    {ok,
     [{<<"content-disposition">>,
       <<"attachment; filename=\"ASCII.bin\"">>},
      {<<"content-type">>, <<"application/octet-stream">>},
      {<<"etag">>, ETag},
      {<<"cache-control">>, <<"public,max-age=" ?REFRESH_STRING>>},
      {<<"expires">>, _},
      {<<"last-modified">>, LastModified},
      {<<"date">>, _},
      {<<"accept-ranges">>, <<"bytes">>}],
     ASCII} = cloudi:send_sync(Context, ServiceNameGet,
                               RequestInfo8, Request,
                               Timeout, Priority),
    ok.
%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

