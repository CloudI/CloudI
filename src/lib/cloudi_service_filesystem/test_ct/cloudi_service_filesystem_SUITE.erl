%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Service Filesystem Tests==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2014-2017 Michael Truog <mjtruog at gmail dot com>
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a
%%% copy of this software and associated documentation files (the "Software"),
%%% to deal in the Software without restriction, including without limitation
%%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%%% and/or sell copies of the Software, and to permit persons to whom the
%%% Software is furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%%% DEALINGS IN THE SOFTWARE.
%%%
%%% @author Michael Truog <mjtruog [at] gmail (dot) com>
%%% @copyright 2014-2017 Michael Truog
%%% @version 1.7.1 {@date} {@time}
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
         t_filesystem_basic_read_cache_1/1,
         t_filesystem_size_limit_1/1,
         t_filesystem_size_limit_2/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("cloudi_core/include/cloudi_logger.hrl").

-define(SERVICE_PREFIX1, "/filesystem/").
% ASCII_FILE created with: file:write_file("ASCII.bin", lists:seq(0, 126)).
-define(ASCII_FILE, "ASCII.bin").
-define(WRITABLE_FILENAME1, "filename1.txt").
-define(TIMEOUT, 960000).
-define(REFRESH, 2400). % ((?TIMEOUT * 2.5) div 1000)
-define(REFRESH_STRING, "2400").

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
       t_filesystem_basic_read_cache_1,
       t_filesystem_size_limit_1,
       t_filesystem_size_limit_2]}].

suite() ->
    [{ct_hooks, [cth_surefire]},
     {timetrap, ?TIMEOUT * 2 + 100}].

init_per_suite(Config) ->
    ?REFRESH = erlang:round(?TIMEOUT * 2.5) div 1000,
    ok = cloudi_x_reltool_util:application_start(cloudi_core, [], infinity),
    TmpDir = os:getenv("TMPDIR", "/tmp"),
    TmpDir1 = TmpDir ++ "/cloudi_service_filesystem_test_dir1/",
    TmpDir2 = TmpDir ++ "/cloudi_service_filesystem_test_dir2/",
    ok = filelib:ensure_dir(TmpDir1),
    ok = filelib:ensure_dir(TmpDir2),
    [{tmpdir1, TmpDir1},
     {tmpdir2, TmpDir2} | Config].

end_per_suite(Config) ->
    ok = cloudi_x_reltool_util:application_stop(cloudi_core),
    TmpDir1Rm = os:cmd("rmdir " ++ ?config(tmpdir1, Config)),
    if
        TmpDir1Rm == [] ->
            ok;
        true ->
            ?LOG_ERROR("~s", [TmpDir1Rm])
    end,
    TmpDir2Rm = os:cmd("rmdir " ++ ?config(tmpdir2, Config)),
    if
        TmpDir2Rm == [] ->
            ok;
        true ->
            ?LOG_ERROR("~s", [TmpDir2Rm])
    end,
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
          [{directory, "${TEST_DIR}"},
           {use_content_disposition, true}]},
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
           {refresh, ?REFRESH},
           {use_content_disposition, true}]},
         {dest_refresh, none},
         {timeout_init, ?TIMEOUT},
         {timeout_sync, ?TIMEOUT}]
        ], infinity),
    [{service_ids, ServiceIds} | Config];
init_per_testcase(TestCase, Config)
    when (TestCase =:= t_filesystem_basic_write_truncate_1) ->
    ok = file:write_file(writable_filepath1(Config), <<>>),
    {ok, ServiceIds} = cloudi_service_api:services_add([
        % using proplist configuration format, not the tuple/record format
        [{prefix, ?SERVICE_PREFIX1},
         {module, cloudi_service_filesystem},
         {args,
          [{directory, ?config(tmpdir1, Config)},
           {write_truncate,
            [?SERVICE_PREFIX1 ?WRITABLE_FILENAME1]},
           {use_content_disposition, true}]},
         {dest_refresh, none},
         {timeout_init, ?TIMEOUT},
         {timeout_sync, ?TIMEOUT}]
        ], infinity),
    [{service_ids, ServiceIds} | Config];
init_per_testcase(TestCase, Config)
    when (TestCase =:= t_filesystem_basic_write_truncate_wcache_1) ->
    ok = file:write_file(writable_filepath1(Config), <<>>),
    {ok, ServiceIds} = cloudi_service_api:services_add([
        % using proplist configuration format, not the tuple/record format
        [{prefix, ?SERVICE_PREFIX1},
         {module, cloudi_service_filesystem},
         {args,
          [{directory, ?config(tmpdir1, Config)},
           {cache, 1},
           {refresh, ?REFRESH},
           {write_truncate,
            [?SERVICE_PREFIX1 ?WRITABLE_FILENAME1]},
           {use_content_disposition, true}]},
         {dest_refresh, none},
         {timeout_init, ?TIMEOUT},
         {timeout_sync, ?TIMEOUT}]
        ], infinity),
    [{service_ids, ServiceIds} | Config];
init_per_testcase(TestCase, Config)
    when (TestCase =:= t_filesystem_basic_write_append_1) ->
    ok = file:write_file(writable_filepath1(Config), <<>>),
    {ok, ServiceIds} = cloudi_service_api:services_add([
        % using proplist configuration format, not the tuple/record format
        [{prefix, ?SERVICE_PREFIX1},
         {module, cloudi_service_filesystem},
         {args,
          [{directory, ?config(tmpdir1, Config)},
           {write_append,
            [?SERVICE_PREFIX1 ?WRITABLE_FILENAME1]},
           {use_content_disposition, true}]},
         {dest_refresh, none},
         {timeout_init, ?TIMEOUT},
         {timeout_sync, ?TIMEOUT}]
        ], infinity),
    [{service_ids, ServiceIds} | Config];
init_per_testcase(TestCase, Config)
    when (TestCase =:= t_filesystem_basic_write_append_wcache_1) ->
    ok = file:write_file(writable_filepath1(Config), <<>>),
    {ok, ServiceIds} = cloudi_service_api:services_add([
        % using proplist configuration format, not the tuple/record format
        [{prefix, ?SERVICE_PREFIX1},
         {module, cloudi_service_filesystem},
         {args,
          [{directory, ?config(tmpdir1, Config)},
           {cache, 1},
           {refresh, ?REFRESH},
           {write_append,
            [?SERVICE_PREFIX1 ?WRITABLE_FILENAME1]},
           {use_content_disposition, true}]},
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
           {refresh, ?REFRESH},
           {use_content_disposition, true}]},
         {dest_refresh, none},
         {timeout_init, ?TIMEOUT},
         {timeout_sync, ?TIMEOUT}]
        ], infinity),
    [{service_ids, ServiceIds} | Config];
init_per_testcase(TestCase, Config)
    when (TestCase =:= t_filesystem_size_limit_1) ->
    _ = file:delete(writable_filepath2(Config)),
    {ok, ServiceIds} = cloudi_service_api:services_add([
        % using proplist configuration format, not the tuple/record format
        [{prefix, ?SERVICE_PREFIX1},
         {module, cloudi_service_filesystem},
         {args,
          [{directory, ?config(tmpdir2, Config)},
           {files_size, 2}, % Kb
           {write_append,
            [?SERVICE_PREFIX1 ?WRITABLE_FILENAME1]},
           {use_content_disposition, true}]},
         {dest_refresh, none},
         {timeout_init, ?TIMEOUT},
         {timeout_sync, ?TIMEOUT}]
        ], infinity),
    [{service_ids, ServiceIds} | Config];
init_per_testcase(TestCase, Config)
    when (TestCase =:= t_filesystem_size_limit_2) ->
    _ = file:delete(writable_filepath2(Config)),
    {ok, ServiceIds} = cloudi_service_api:services_add([
        % using proplist configuration format, not the tuple/record format
        [{prefix, ?SERVICE_PREFIX1},
         {module, cloudi_service_filesystem},
         {args,
          [{directory, ?config(tmpdir2, Config)},
           {files_size, 1}, % Kb
           {write_truncate,
            [?SERVICE_PREFIX1 ?WRITABLE_FILENAME1]},
           {use_content_disposition, true}]},
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
            ok = file:delete(writable_filepath1(Config));
        TestCase =:= t_filesystem_size_limit_1;
        TestCase =:= t_filesystem_size_limit_2 ->
            ok = file:delete(writable_filepath2(Config));
        true ->
            ok
    end,
    NewConfig.

%%%------------------------------------------------------------------------
%%% test cases
%%%------------------------------------------------------------------------

t_filesystem_basic_read_1(_Config) ->
    Context0 = cloudi:new(),
    ServiceNameHead = ?SERVICE_PREFIX1 ++ ?ASCII_FILE ++ "/head",
    ServiceNameGet = ?SERVICE_PREFIX1 ++ ?ASCII_FILE ++ "/get",
    ASCII = erlang:list_to_binary(lists:seq(0, 126)),
    Request = <<>>,
    Timeout = undefined, % default
    Priority = undefined, % default
    {{ok,
      [{<<"content-type">>, <<"application/octet-stream">>},
       {<<"content-disposition">>,
        <<"attachment; filename=\"ASCII.bin\"">>},
       {<<"etag">>, ETag},
       {<<"last-modified">>, LastModified},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      ASCII},
     Context1} = cloudi:send_sync(Context0, ServiceNameGet,
                                  <<>>, Request,
                                  Timeout, Priority),
    RequestInfo1 = [{<<"range">>, 
                     % "Hello World!" test
                     <<"bytes="
                       "72-72,101-101,108-108,108-108,111-111,"
                       "32-32,"
                       "87-87,111-111,114-114,108-108,100-100,"
                       "33-33">>}],
    {{ok,
      [{<<"status">>, <<"206">>},
       {<<"content-type">>, <<"multipart/byteranges">>},
       {<<"etag">>, ETag},
       {<<"last-modified">>, LastModified},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      <<>>},
     Context2} = cloudi:send_sync(Context1, ServiceNameHead,
                                  RequestInfo1, Request,
                                  Timeout, Priority),
    BSize = 512, % 64 characters for a boundary, from cowlib
    {{ok,
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
        "--\r\n">>},
     Context3} = cloudi:send_sync(Context2, ServiceNameGet,
                                  RequestInfo1, Request,
                                  Timeout, Priority),
    RequestInfo2 = [{<<"range">>, 
                     % "abc" test
                     <<"bytes="
                       "97-99">>},
                    {<<"if-range">>, ETag}],
    {{ok,
      [{<<"status">>, <<"206">>},
       {<<"content-type">>,<<"application/octet-stream">>},
       {<<"content-range">>, <<"bytes 97-99/127">>},
       {<<"etag">>, ETag},
       {<<"last-modified">>, LastModified},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      <<"abc">>},
     Context4} = cloudi:send_sync(Context3, ServiceNameGet,
                                  RequestInfo2, Request,
                                  Timeout, Priority),
    RequestInfo3 = [{<<"range">>, 
                     % "abc" test
                     <<"bytes="
                       "97-99">>},
                    {<<"if-range">>, LastModified}],
    {{ok,
      [{<<"status">>, <<"206">>},
       {<<"content-type">>,<<"application/octet-stream">>},
       {<<"content-range">>, <<"bytes 97-99/127">>},
       {<<"etag">>, ETag},
       {<<"last-modified">>, LastModified},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      <<"abc">>},
     Context5} = cloudi:send_sync(Context4, ServiceNameGet,
                                  RequestInfo3, Request,
                                  Timeout, Priority),
    RequestInfo4 = [{<<"range">>, 
                     % "abc" test
                     <<"bytes="
                       "97-99">>},
                    {<<"if-range">>, <<ETag/binary, (<<"x">>)/binary>>}],
    {{ok,
      [{<<"content-type">>, <<"application/octet-stream">>},
       {<<"content-disposition">>,
        <<"attachment; filename=\"ASCII.bin\"">>},
       {<<"etag">>, ETag},
       {<<"last-modified">>, LastModified},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      % bad ETag provides the whole ASCII file
      ASCII},
     Context6} = cloudi:send_sync(Context5, ServiceNameGet,
                                  RequestInfo4, Request,
                                  Timeout, Priority),
    % alter the last-modified date to have the wrong seconds value
    <<LastModified0:184/bitstring,
      LastModified1:16/bitstring,
      LastModified2:32/bitstring>> = LastModified,
    LastModified1Future = erlang:integer_to_binary(
        (erlang:binary_to_integer(LastModified1) + 1) rem 60),
    LastModified1New = if
        byte_size(LastModified1Future) == 1 ->
            <<"0", LastModified1Future/binary>>;
        true ->
            LastModified1Future
    end,
    LastModifiedFake = <<LastModified0/binary,
                         LastModified1New/binary,
                         LastModified2/binary>>,
    RequestInfo5 = [{<<"range">>, 
                     % "abc" test
                     <<"bytes="
                       "97-99">>},
                    {<<"if-range">>, LastModifiedFake}],
    {{ok,
      [{<<"content-type">>, <<"application/octet-stream">>},
       {<<"content-disposition">>,
        <<"attachment; filename=\"ASCII.bin\"">>},
       {<<"etag">>, ETag},
       {<<"last-modified">>, LastModified},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      % bad LastModified provides the whole ASCII file
      ASCII},
     Context7} = cloudi:send_sync(Context6, ServiceNameGet,
                                  RequestInfo5, Request,
                                  Timeout, Priority),
    RequestInfo6 = [{<<"range">>, 
                     % "{|}~" test
                     <<"bytes="
                       "-4">>}],
    {{ok,
      [{<<"status">>, <<"206">>},
       {<<"content-type">>,<<"application/octet-stream">>},
       {<<"content-range">>, <<"bytes 123-126/127">>},
       {<<"etag">>, ETag},
       {<<"last-modified">>, LastModified},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      <<"{|}~">>},
     _} = cloudi:send_sync(Context7, ServiceNameGet,
                           RequestInfo6, Request,
                           Timeout, Priority),
    ok.

t_filesystem_basic_read_wcache_1(_Config) ->
    Context0 = cloudi:new(),
    ServiceNameHead = ?SERVICE_PREFIX1 ++ ?ASCII_FILE ++ "/head",
    ServiceNameGet = ?SERVICE_PREFIX1 ++ ?ASCII_FILE ++ "/get",
    ASCII = erlang:list_to_binary(lists:seq(0, 126)),
    Request = <<>>,
    Timeout = undefined, % default
    Priority = undefined, % default
    {{ok,
      [{<<"content-type">>, <<"application/octet-stream">>},
       {<<"content-disposition">>,
        <<"attachment; filename=\"ASCII.bin\"">>},
       {<<"etag">>, ETag},
       {<<"cache-control">>, <<"public,max-age=1">>},
       {<<"expires">>, _},
       {<<"last-modified">>, LastModified},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      ASCII},
     Context1} = cloudi:send_sync(Context0, ServiceNameGet,
                                  <<>>, Request,
                                  Timeout, Priority),
    RequestInfo1 = [{<<"range">>, 
                     % "Hello World!" test
                     <<"bytes="
                       "72-72,101-101,108-108,108-108,111-111,"
                       "32-32,"
                       "87-87,111-111,114-114,108-108,100-100,"
                       "33-33">>}],
    {{ok,
      [{<<"status">>, <<"206">>},
       {<<"content-type">>, <<"multipart/byteranges">>},
       {<<"etag">>, ETag},
       {<<"cache-control">>, <<"public,max-age=1">>},
       {<<"expires">>, _},
       {<<"last-modified">>, LastModified},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      <<>>},
     Context2} = cloudi:send_sync(Context1, ServiceNameHead,
                                  RequestInfo1, Request,
                                  Timeout, Priority),
    BSize = 512, % 64 characters for a boundary, from cowlib
    {{ok,
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
        "--\r\n">>},
     Context3} = cloudi:send_sync(Context2, ServiceNameGet,
                                  RequestInfo1, Request,
                                  Timeout, Priority),
    RequestInfo2 = [{<<"range">>, 
                     % "abc" test
                     <<"bytes="
                       "97-99">>},
                    {<<"if-range">>, ETag}],
    {{ok,
      [{<<"status">>, <<"206">>},
       {<<"content-type">>,<<"application/octet-stream">>},
       {<<"content-range">>, <<"bytes 97-99/127">>},
       {<<"etag">>, ETag},
       {<<"cache-control">>, <<"public,max-age=1">>},
       {<<"expires">>, _},
       {<<"last-modified">>, LastModified},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      <<"abc">>},
     Context4} = cloudi:send_sync(Context3, ServiceNameGet,
                                  RequestInfo2, Request,
                                  Timeout, Priority),
    RequestInfo3 = [{<<"range">>, 
                     % "abc" test
                     <<"bytes="
                       "97-99">>},
                    {<<"if-range">>, LastModified}],
    {{ok,
      [{<<"status">>, <<"206">>},
       {<<"content-type">>,<<"application/octet-stream">>},
       {<<"content-range">>, <<"bytes 97-99/127">>},
       {<<"etag">>, ETag},
       {<<"cache-control">>, <<"public,max-age=1">>},
       {<<"expires">>, _},
       {<<"last-modified">>, LastModified},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      <<"abc">>},
     Context5} = cloudi:send_sync(Context4, ServiceNameGet,
                                  RequestInfo3, Request,
                                  Timeout, Priority),
    RequestInfo4 = [{<<"range">>, 
                     % "abc" test
                     <<"bytes="
                       "97-99">>},
                    {<<"if-range">>, <<ETag/binary, (<<"x">>)/binary>>}],
    {{ok,
      [{<<"content-type">>, <<"application/octet-stream">>},
       {<<"content-disposition">>,
        <<"attachment; filename=\"ASCII.bin\"">>},
       {<<"etag">>, ETag},
       {<<"cache-control">>, <<"public,max-age=1">>},
       {<<"expires">>, _},
       {<<"last-modified">>, LastModified},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      % bad ETag provides the whole ASCII file
      ASCII},
     Context6} = cloudi:send_sync(Context5, ServiceNameGet,
                                  RequestInfo4, Request,
                                  Timeout, Priority),
    % alter the last-modified date to have the wrong seconds value
    <<LastModified0:184/bitstring,
      LastModified1:16/bitstring,
      LastModified2:32/bitstring>> = LastModified,
    LastModified1Future = erlang:integer_to_binary(
        (erlang:binary_to_integer(LastModified1) + 1) rem 60),
    LastModified1New = if
        byte_size(LastModified1Future) == 1 ->
            <<"0", LastModified1Future/binary>>;
        true ->
            LastModified1Future
    end,
    LastModifiedFake = <<LastModified0/binary,
                         LastModified1New/binary,
                         LastModified2/binary>>,
    RequestInfo5 = [{<<"range">>, 
                     % "abc" test
                     <<"bytes="
                       "97-99">>},
                    {<<"if-range">>, LastModifiedFake}],
    {{ok,
      [{<<"content-type">>, <<"application/octet-stream">>},
       {<<"content-disposition">>,
        <<"attachment; filename=\"ASCII.bin\"">>},
       {<<"etag">>, ETag},
       {<<"cache-control">>, <<"public,max-age=1">>},
       {<<"expires">>, _},
       {<<"last-modified">>, LastModified},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      % bad LastModified provides the whole ASCII file
      ASCII},
     Context7}= cloudi:send_sync(Context6, ServiceNameGet,
                                 RequestInfo5, Request,
                                 Timeout, Priority),
    RequestInfo6 = [{<<"range">>, 
                     % "{|}~" test
                     <<"bytes="
                       "-4">>}],
    {{ok,
      [{<<"status">>, <<"206">>},
       {<<"content-type">>,<<"application/octet-stream">>},
       {<<"content-range">>, <<"bytes 123-126/127">>},
       {<<"etag">>, ETag},
       {<<"cache-control">>, <<"public,max-age=1">>},
       {<<"expires">>, _},
       {<<"last-modified">>, LastModified},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      <<"{|}~">>},
     _}= cloudi:send_sync(Context7, ServiceNameGet,
                          RequestInfo6, Request,
                          Timeout, Priority),
    ok.

t_filesystem_basic_write_truncate_1(Config) ->
    Context0 = cloudi:new(),
    ServiceNameGet = ?SERVICE_PREFIX1 ++ ?WRITABLE_FILENAME1 ++ "/get",
    ServiceNamePut = ?SERVICE_PREFIX1 ++ ?WRITABLE_FILENAME1 ++ "/put",
    Timeout = undefined, % default
    Priority = undefined, % default
    {ok, <<>>} = file:read_file(writable_filepath1(Config)),
    {{ok,
      [{<<"content-type">>, <<"text/plain">>},
       {<<"content-disposition">>,
        <<"attachment; filename=\"" ?WRITABLE_FILENAME1 "\"">>},
       {<<"etag">>, ETag0},
       {<<"last-modified">>, LastModified0},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      <<>>},
     Context1} = cloudi:send_sync(Context0, ServiceNameGet,
                                  <<>>, <<>>,
                                  Timeout, Priority),
    Response1 = <<"Hello">>,
    {{ok,
      [{<<"content-type">>, <<"text/plain">>},
       {<<"content-disposition">>,
        <<"attachment; filename=\"" ?WRITABLE_FILENAME1 "\"">>},
       {<<"etag">>, ETag1},
       {<<"last-modified">>, LastModified1},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      Response1},
     Context2} = cloudi:send_sync(Context1, ServiceNamePut,
                                  <<>>, Response1,
                                  Timeout, Priority),
    Response2 = <<"Hello World!">>,
    {{ok,
      [{<<"content-type">>, <<"text/plain">>},
       {<<"content-disposition">>,
        <<"attachment; filename=\"" ?WRITABLE_FILENAME1 "\"">>},
       {<<"etag">>, ETag2},
       {<<"last-modified">>, LastModified2},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      Response2},
     Context3} = cloudi:send_sync(Context2, ServiceNamePut,
                                  <<>>, Response2,
                                  Timeout, Priority),
    true = ETag0 /= ETag1,
    true = LastModified0 =< LastModified1,
    true = ETag1 /= ETag2,
    true = LastModified1 =< LastModified2,
    {ok, Response2} = file:read_file(writable_filepath1(Config)),
    {{ok,
      [{<<"status">>, <<"400">>}],
      <<>>},
     _} = cloudi:send_sync(Context3, ServiceNamePut,
                           [{<<"range">>, <<"bytes=-6">>}], <<"Error!">>,
                           Timeout, Priority),
    ok.

t_filesystem_basic_write_truncate_wcache_1(Config) ->
    Context0 = cloudi:new(),
    ServiceNameGet = ?SERVICE_PREFIX1 ++ ?WRITABLE_FILENAME1 ++ "/get",
    ServiceNamePut = ?SERVICE_PREFIX1 ++ ?WRITABLE_FILENAME1 ++ "/put",
    Timeout = undefined, % default
    Priority = undefined, % default
    {ok, <<>>} = file:read_file(writable_filepath1(Config)),
    {{ok,
      [{<<"content-type">>, <<"text/plain">>},
       {<<"content-disposition">>,
        <<"attachment; filename=\"" ?WRITABLE_FILENAME1 "\"">>},
       {<<"etag">>, ETag0},
       {<<"cache-control">>, <<"public,max-age=1">>},
       {<<"expires">>, _},
       {<<"last-modified">>, LastModified0},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      <<>>},
     Context1} = cloudi:send_sync(Context0, ServiceNameGet,
                                  <<>>, <<>>,
                                  Timeout, Priority),
    Response1 = <<"Hello">>,
    {{ok,
      [{<<"content-type">>, <<"text/plain">>},
       {<<"content-disposition">>,
        <<"attachment; filename=\"" ?WRITABLE_FILENAME1 "\"">>},
       {<<"etag">>, ETag1},
       {<<"cache-control">>, <<"public,max-age=1">>},
       {<<"expires">>, _},
       {<<"last-modified">>, LastModified1},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      Response1},
     Context2} = cloudi:send_sync(Context1, ServiceNamePut,
                                  <<>>, Response1,
                                  Timeout, Priority),
    Response2 = <<"Hello World!">>,
    {{ok,
      [{<<"content-type">>, <<"text/plain">>},
       {<<"content-disposition">>,
        <<"attachment; filename=\"" ?WRITABLE_FILENAME1 "\"">>},
       {<<"etag">>, ETag2},
       {<<"cache-control">>, <<"public,max-age=1">>},
       {<<"expires">>, _},
       {<<"last-modified">>, LastModified2},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      Response2},
     Context3} = cloudi:send_sync(Context2, ServiceNamePut,
                                  <<>>, Response2,
                                  Timeout, Priority),
    true = ETag0 /= ETag1,
    true = LastModified0 =< LastModified1,
    true = ETag1 /= ETag2,
    true = LastModified1 =< LastModified2,
    {ok, Response2} = file:read_file(writable_filepath1(Config)),
    {{ok,
      [{<<"status">>, <<"400">>}],
      <<>>},
     _} = cloudi:send_sync(Context3, ServiceNamePut,
                           [{<<"range">>, <<"bytes=-6">>}], <<"Error!">>,
                           Timeout, Priority),
    ok.

t_filesystem_basic_write_append_1(Config) ->
    Context0 = cloudi:new(),
    ServiceNameGet = ?SERVICE_PREFIX1 ++ ?WRITABLE_FILENAME1 ++ "/get",
    ServiceNamePost = ?SERVICE_PREFIX1 ++ ?WRITABLE_FILENAME1 ++ "/post",
    Timeout = undefined, % default
    Priority = undefined, % default
    {ok, <<>>} = file:read_file(writable_filepath1(Config)),
    {{ok,
      [{<<"content-type">>, <<"text/plain">>},
       {<<"content-disposition">>,
        <<"attachment; filename=\"" ?WRITABLE_FILENAME1 "\"">>},
       {<<"etag">>, ETag0},
       {<<"last-modified">>, LastModified0},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      <<>>},
     Context1} = cloudi:send_sync(Context0, ServiceNameGet,
                                  <<>>, <<>>,
                                  Timeout, Priority),
    Request1 = <<"The quick brown fox tripped">>,
    {{ok,
      [{<<"content-type">>, <<"text/plain">>},
       {<<"content-disposition">>,
        <<"attachment; filename=\"" ?WRITABLE_FILENAME1 "\"">>},
       {<<"etag">>, ETag1},
       {<<"last-modified">>, LastModified1},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      Request1},
     Context2} = cloudi:send_sync(Context1, ServiceNamePost,
                                  <<>>, Request1,
                                  Timeout, Priority),

    RequestInfo2 = [{<<"range">>, 
                     <<"bytes="
                       "-7">>}],
    Request2 = <<"jumps over the lazy dog">>,
    Response2 = <<"The quick brown fox jumps over the lazy dog">>,
    {{ok,
      [{<<"content-type">>, <<"text/plain">>},
       {<<"content-disposition">>,
        <<"attachment; filename=\"" ?WRITABLE_FILENAME1 "\"">>},
       {<<"etag">>, ETag2},
       {<<"last-modified">>, LastModified2},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      Response2},
     Context3} = cloudi:send_sync(Context2, ServiceNamePost,
                                  RequestInfo2, Request2,
                                  Timeout, Priority),
    true = ETag0 /= ETag1,
    true = LastModified0 =< LastModified1,
    true = ETag1 /= ETag2,
    true = LastModified1 =< LastModified2,
    RequestInfo3 = [{<<"range">>, 
                     <<"bytes="
                       "10-14">>}],
    Request3 = <<"red">>,
    Response3 = <<"The quick red fox jumps over the lazy dog">>,
    {{ok,
      [{<<"content-type">>, <<"text/plain">>},
       {<<"content-disposition">>,
        <<"attachment; filename=\"" ?WRITABLE_FILENAME1 "\"">>},
       {<<"etag">>, ETag3},
       {<<"last-modified">>, LastModified3},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      Response3},
     Context4} = cloudi:send_sync(Context3, ServiceNamePost,
                                  RequestInfo3, Request3,
                                  Timeout, Priority),
    true = ETag2 /= ETag3,
    true = LastModified2 =< LastModified3,
    RequestInfo4 = [{<<"range">>, 
                     <<"bytes="
                       "22-22">>}],
    Request4 = <<"ed">>,
    Response4 = <<"The quick red fox jumped over the lazy dog">>,
    {{ok,
      [{<<"content-type">>, <<"text/plain">>},
       {<<"content-disposition">>,
        <<"attachment; filename=\"" ?WRITABLE_FILENAME1 "\"">>},
       {<<"etag">>, ETag4},
       {<<"last-modified">>, LastModified4},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      Response4},
     Context5} = cloudi:send_sync(Context4, ServiceNamePost,
                                  RequestInfo4, Request4,
                                  Timeout, Priority),
    true = ETag3 /= ETag4,
    true = LastModified3 =< LastModified4,
    RequestInfo5 = [{<<"range">>, 
                     <<"bytes="
                       "42-42">>}],
    Request5 = <<"s.">>,
    Response5 = <<"The quick red fox jumped over the lazy dogs.">>,
    {{ok,
      [{<<"content-type">>, <<"text/plain">>},
       {<<"content-disposition">>,
        <<"attachment; filename=\"" ?WRITABLE_FILENAME1 "\"">>},
       {<<"etag">>, ETag5},
       {<<"last-modified">>, LastModified5},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      Response5},
     Context6} = cloudi:send_sync(Context5, ServiceNamePost,
                                  RequestInfo5, Request5,
                                  Timeout, Priority),
    true = ETag4 /= ETag5,
    true = LastModified4 =< LastModified5,
    RequestInfo6 = [{<<"range">>, 
                     <<"bytes="
                       "45-44">>}],
    {{ok,
      [{<<"status">>, <<"416">>},
       {<<"content-range">>, <<"bytes */44">>},
       {<<"accept-ranges">>, <<"bytes">>}],
      <<>>},
     Context7} = cloudi:send_sync(Context6, ServiceNamePost,
                                  RequestInfo6, <<" Ignore this data!">>,
                                  Timeout, Priority),
    RequestInfo7 = [{<<"range">>, 
                     <<"bytes="
                       "44-44">>}],
    Request7 = <<" The fox later became a Hollywood actor.">>,
    Response7 = <<"The quick red fox jumped over the lazy dogs.",
                  Request7/binary>>,
    {{ok,
      [{<<"content-type">>, <<"text/plain">>},
       {<<"content-disposition">>,
        <<"attachment; filename=\"" ?WRITABLE_FILENAME1 "\"">>},
       {<<"etag">>, ETag7},
       {<<"last-modified">>, LastModified7},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      Response7},
     Context8} = cloudi:send_sync(Context7, ServiceNamePost,
                                  RequestInfo7, Request7,
                                  Timeout, Priority),
    true = ETag5 /= ETag7,
    true = LastModified5 =< LastModified7,
    Request8 = <<" The end.">>,
    Response8 = <<Response7/binary,
                  Request8/binary>>,
    {{ok,
      [{<<"content-type">>, <<"text/plain">>},
       {<<"content-disposition">>,
        <<"attachment; filename=\"" ?WRITABLE_FILENAME1 "\"">>},
       {<<"etag">>, ETag8},
       {<<"last-modified">>, LastModified8},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      Response8},
     _} = cloudi:send_sync(Context8, ServiceNamePost,
                           <<>>, Request8,
                           Timeout, Priority),
    true = ETag7 /= ETag8,
    true = LastModified7 =< LastModified8,
    {ok, Response8} = file:read_file(writable_filepath1(Config)),
    ok.

t_filesystem_basic_write_append_wcache_1(Config) ->
    Context0 = cloudi:new(),
    ServiceNameGet = ?SERVICE_PREFIX1 ++ ?WRITABLE_FILENAME1 ++ "/get",
    ServiceNamePost = ?SERVICE_PREFIX1 ++ ?WRITABLE_FILENAME1 ++ "/post",
    Timeout = undefined, % default
    Priority = undefined, % default
    {ok, <<>>} = file:read_file(writable_filepath1(Config)),
    {{ok,
      [{<<"content-type">>, <<"text/plain">>},
       {<<"content-disposition">>,
        <<"attachment; filename=\"" ?WRITABLE_FILENAME1 "\"">>},
       {<<"etag">>, ETag0},
       {<<"cache-control">>, <<"public,max-age=1">>},
       {<<"expires">>, _},
       {<<"last-modified">>, LastModified0},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      <<>>},
     Context1} = cloudi:send_sync(Context0, ServiceNameGet,
                                  <<>>, <<>>,
                                  Timeout, Priority),
    Request1 = <<"The quick brown fox tripped">>,
    {{ok,
      [{<<"content-type">>, <<"text/plain">>},
       {<<"content-disposition">>,
        <<"attachment; filename=\"" ?WRITABLE_FILENAME1 "\"">>},
       {<<"etag">>, ETag1},
       {<<"cache-control">>, <<"public,max-age=1">>},
       {<<"expires">>, _},
       {<<"last-modified">>, LastModified1},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      Request1},
     Context2} = cloudi:send_sync(Context1, ServiceNamePost,
                                  <<>>, Request1,
                                  Timeout, Priority),

    RequestInfo2 = [{<<"range">>, 
                     <<"bytes="
                       "-7">>}],
    Request2 = <<"jumps over the lazy dog">>,
    Response2 = <<"The quick brown fox jumps over the lazy dog">>,
    {{ok,
      [{<<"content-type">>, <<"text/plain">>},
       {<<"content-disposition">>,
        <<"attachment; filename=\"" ?WRITABLE_FILENAME1 "\"">>},
       {<<"etag">>, ETag2},
       {<<"cache-control">>, <<"public,max-age=1">>},
       {<<"expires">>, _},
       {<<"last-modified">>, LastModified2},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      Response2},
     Context3} = cloudi:send_sync(Context2, ServiceNamePost,
                                  RequestInfo2, Request2,
                                  Timeout, Priority),
    true = ETag0 /= ETag1,
    true = LastModified0 =< LastModified1,
    true = ETag1 /= ETag2,
    true = LastModified1 =< LastModified2,
    RequestInfo3 = [{<<"range">>, 
                     <<"bytes="
                       "10-14">>}],
    Request3 = <<"red">>,
    Response3 = <<"The quick red fox jumps over the lazy dog">>,
    {{ok,
      [{<<"content-type">>, <<"text/plain">>},
       {<<"content-disposition">>,
        <<"attachment; filename=\"" ?WRITABLE_FILENAME1 "\"">>},
       {<<"etag">>, ETag3},
       {<<"cache-control">>, <<"public,max-age=1">>},
       {<<"expires">>, _},
       {<<"last-modified">>, LastModified3},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      Response3},
     Context4} = cloudi:send_sync(Context3, ServiceNamePost,
                                  RequestInfo3, Request3,
                                  Timeout, Priority),
    true = ETag2 /= ETag3,
    true = LastModified2 =< LastModified3,
    RequestInfo4 = [{<<"range">>, 
                     <<"bytes="
                       "22-22">>}],
    Request4 = <<"ed">>,
    Response4 = <<"The quick red fox jumped over the lazy dog">>,
    {{ok,
      [{<<"content-type">>, <<"text/plain">>},
       {<<"content-disposition">>,
        <<"attachment; filename=\"" ?WRITABLE_FILENAME1 "\"">>},
       {<<"etag">>, ETag4},
       {<<"cache-control">>, <<"public,max-age=1">>},
       {<<"expires">>, _},
       {<<"last-modified">>, LastModified4},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      Response4},
     Context5} = cloudi:send_sync(Context4, ServiceNamePost,
                                  RequestInfo4, Request4,
                                  Timeout, Priority),
    true = ETag3 /= ETag4,
    true = LastModified3 =< LastModified4,
    RequestInfo5 = [{<<"range">>, 
                     <<"bytes="
                       "42-42">>}],
    Request5 = <<"s.">>,
    Response5 = <<"The quick red fox jumped over the lazy dogs.">>,
    {{ok,
      [{<<"content-type">>, <<"text/plain">>},
       {<<"content-disposition">>,
        <<"attachment; filename=\"" ?WRITABLE_FILENAME1 "\"">>},
       {<<"etag">>, ETag5},
       {<<"cache-control">>, <<"public,max-age=1">>},
       {<<"expires">>, _},
       {<<"last-modified">>, LastModified5},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      Response5},
     Context6} = cloudi:send_sync(Context5, ServiceNamePost,
                                  RequestInfo5, Request5,
                                  Timeout, Priority),
    true = ETag4 /= ETag5,
    true = LastModified4 =< LastModified5,
    RequestInfo6 = [{<<"range">>, 
                     <<"bytes="
                       "45-44">>}],
    {{ok,
      [{<<"status">>, <<"416">>},
       {<<"content-range">>, <<"bytes */44">>},
       {<<"accept-ranges">>, <<"bytes">>}],
      <<>>},
     Context7} = cloudi:send_sync(Context6, ServiceNamePost,
                                  RequestInfo6, <<" Ignore this data!">>,
                                  Timeout, Priority),
    RequestInfo7 = [{<<"range">>, 
                     <<"bytes="
                       "44-44">>}],
    Request7 = <<" The fox later became a Hollywood actor.">>,
    Response7 = <<"The quick red fox jumped over the lazy dogs.",
                  Request7/binary>>,
    {{ok,
      [{<<"content-type">>, <<"text/plain">>},
       {<<"content-disposition">>,
        <<"attachment; filename=\"" ?WRITABLE_FILENAME1 "\"">>},
       {<<"etag">>, ETag7},
       {<<"cache-control">>, <<"public,max-age=1">>},
       {<<"expires">>, _},
       {<<"last-modified">>, LastModified7},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      Response7},
     Context8} = cloudi:send_sync(Context7, ServiceNamePost,
                                  RequestInfo7, Request7,
                                  Timeout, Priority),
    true = ETag5 /= ETag7,
    true = LastModified5 =< LastModified7,
    Request8 = <<" The end.">>,
    Response8 = <<Response7/binary,
                  Request8/binary>>,
    {{ok,
      [{<<"content-type">>, <<"text/plain">>},
       {<<"content-disposition">>,
        <<"attachment; filename=\"" ?WRITABLE_FILENAME1 "\"">>},
       {<<"etag">>, ETag8},
       {<<"cache-control">>, <<"public,max-age=1">>},
       {<<"expires">>, _},
       {<<"last-modified">>, LastModified8},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      Response8},
     _} = cloudi:send_sync(Context8, ServiceNamePost,
                           <<>>, Request8,
                           Timeout, Priority),
    true = ETag7 /= ETag8,
    true = LastModified7 =< LastModified8,
    {ok, Response8} = file:read_file(writable_filepath1(Config)),
    ok.

t_filesystem_basic_read_cache_1(_Config) ->
    Context0 = cloudi:new(),
    ServiceNameGet = ?SERVICE_PREFIX1 ++ ?ASCII_FILE ++ "/get",
    ASCII = erlang:list_to_binary(lists:seq(0, 126)),
    Request = <<>>,
    Timeout = undefined, % default
    Priority = undefined, % default
    {{ok,
      [{<<"content-type">>, <<"application/octet-stream">>},
       {<<"content-disposition">>,
        <<"attachment; filename=\"ASCII.bin\"">>},
       {<<"etag">>, ETag},
       {<<"cache-control">>, <<"public,max-age=" ?REFRESH_STRING>>},
       {<<"expires">>, _},
       {<<"last-modified">>, LastModified},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      ASCII},
     Context1} = cloudi:send_sync(Context0, ServiceNameGet,
                                  <<>>, Request,
                                  Timeout, Priority),
    RequestInfo1 = [{<<"if-none-match">>, ETag}],
    {{ok,
      [{<<"status">>, <<"304">>},
       {<<"last-modified">>, LastModified},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      <<>>},
     Context2} = cloudi:send_sync(Context1, ServiceNameGet,
                                  RequestInfo1, Request,
                                  Timeout, Priority),
    InvalidETag = <<"invalid-etag">>,
    RequestInfo2 = [{<<"if-none-match">>, InvalidETag}],
    {{ok,
      [{<<"content-type">>, <<"application/octet-stream">>},
       {<<"content-disposition">>,
        <<"attachment; filename=\"ASCII.bin\"">>},
       {<<"etag">>, ETag},
       {<<"cache-control">>, <<"public,max-age=" ?REFRESH_STRING>>},
       {<<"expires">>, _},
       {<<"last-modified">>, LastModified},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      ASCII},
     Context3} = cloudi:send_sync(Context2, ServiceNameGet,
                                  RequestInfo2, Request,
                                  Timeout, Priority),
    RequestInfo3 = [{<<"if-match">>, InvalidETag}],
    {{ok,
      [{<<"status">>, <<"412">>},
       {<<"last-modified">>, LastModified},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      <<>>},
     Context4} = cloudi:send_sync(Context3, ServiceNameGet,
                                  RequestInfo3, Request,
                                  Timeout, Priority),
    RequestInfo4 = [{<<"if-match">>, ETag}],
    {{ok,
      [{<<"content-type">>, <<"application/octet-stream">>},
       {<<"content-disposition">>,
        <<"attachment; filename=\"ASCII.bin\"">>},
       {<<"etag">>, ETag},
       {<<"cache-control">>, <<"public,max-age=" ?REFRESH_STRING>>},
       {<<"expires">>, _},
       {<<"last-modified">>, LastModified},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      ASCII},
     Context5} = cloudi:send_sync(Context4, ServiceNameGet,
                                  RequestInfo4, Request,
                                  Timeout, Priority),
    RequestInfo5 = [{<<"if-modified-since">>, LastModified}],
    {{ok,
      [{<<"status">>, <<"304">>},
       {<<"last-modified">>, LastModified},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      <<>>},
     Context6} = cloudi:send_sync(Context5, ServiceNameGet,
                                  RequestInfo5, Request,
                                  Timeout, Priority),
    OldDate = <<"Mon, 01 Jan 1900 00:00:00 GMT">>,
    RequestInfo6 = [{<<"if-modified-since">>, OldDate}],
    {{ok,
      [{<<"content-type">>, <<"application/octet-stream">>},
       {<<"content-disposition">>,
        <<"attachment; filename=\"ASCII.bin\"">>},
       {<<"etag">>, ETag},
       {<<"cache-control">>, <<"public,max-age=" ?REFRESH_STRING>>},
       {<<"expires">>, _},
       {<<"last-modified">>, LastModified},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      ASCII},
     Context7} = cloudi:send_sync(Context6, ServiceNameGet,
                                  RequestInfo6, Request,
                                  Timeout, Priority),
    RequestInfo7 = [{<<"if-unmodified-since">>, LastModified}],
    {{ok,
      [{<<"status">>, <<"412">>},
       {<<"last-modified">>, LastModified},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      <<>>},
     Context8} = cloudi:send_sync(Context7, ServiceNameGet,
                                  RequestInfo7, Request,
                                  Timeout, Priority),
    RequestInfo8 = [{<<"if-unmodified-since">>, OldDate}],
    {{ok,
      [{<<"content-type">>, <<"application/octet-stream">>},
       {<<"content-disposition">>,
        <<"attachment; filename=\"ASCII.bin\"">>},
       {<<"etag">>, ETag},
       {<<"cache-control">>, <<"public,max-age=" ?REFRESH_STRING>>},
       {<<"expires">>, _},
       {<<"last-modified">>, LastModified},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      ASCII},
     _} = cloudi:send_sync(Context8, ServiceNameGet,
                           RequestInfo8, Request,
                           Timeout, Priority),
    ok.

t_filesystem_size_limit_1(Config) ->
    Context0 = cloudi:new(),
    ServiceNameGet = ?SERVICE_PREFIX1 ++ ?WRITABLE_FILENAME1 ++ "/get",
    ServiceNamePost = ?SERVICE_PREFIX1 ++ ?WRITABLE_FILENAME1 ++ "/post",
    Timeout = undefined, % default
    Priority = undefined, % default
    {{ok,
      [{<<"content-type">>, <<"text/plain">>},
       {<<"content-disposition">>,
        <<"attachment; filename=\"" ?WRITABLE_FILENAME1 "\"">>},
       {<<"etag">>, ETag0},
       {<<"last-modified">>, LastModified0},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      <<>>},
     Context1} = cloudi:send_sync(Context0, ServiceNameGet,
                                  <<>>, <<>>,
                                  Timeout, Priority),
    {error, enoent} = file:read_file(writable_filepath2(Config)),
    Request1 = <<0:8192>>, % 1 Kb
    {{ok,
      [{<<"content-type">>, <<"text/plain">>},
       {<<"content-disposition">>,
        <<"attachment; filename=\"" ?WRITABLE_FILENAME1 "\"">>},
       {<<"etag">>, ETag1},
       {<<"last-modified">>, LastModified1},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      Request1},
     Context2} = cloudi:send_sync(Context1, ServiceNamePost,
                                  <<>>, Request1,
                                  Timeout, Priority),
    {ok, Request1} = file:read_file(writable_filepath2(Config)),
    true = ETag0 /= ETag1,
    true = LastModified0 =< LastModified1,
    Request2 = <<1:8192>>, % 1 Kb
    Response2 = <<Request1/binary, Request2/binary>>,
    {{ok,
      [{<<"content-type">>, <<"text/plain">>},
       {<<"content-disposition">>,
        <<"attachment; filename=\"" ?WRITABLE_FILENAME1 "\"">>},
       {<<"etag">>, ETag2},
       {<<"last-modified">>, LastModified2},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      Response2},
     Context3} = cloudi:send_sync(Context2, ServiceNamePost,
                                  <<>>, Request2,
                                  Timeout, Priority),
    true = ETag1 /= ETag2,
    true = LastModified1 =< LastModified2,
    {ok, Response2} = file:read_file(writable_filepath2(Config)),
    Request3 = <<2:8>>, % 1 byte
    {{ok,
      [{<<"status">>, <<"400">>}],
      <<>>},
     _} = cloudi:send_sync(Context3, ServiceNamePost,
                           <<>>, Request3,
                           Timeout, Priority),
    ok.

t_filesystem_size_limit_2(Config) ->
    Context0 = cloudi:new(),
    ServiceNameGet = ?SERVICE_PREFIX1 ++ ?WRITABLE_FILENAME1 ++ "/get",
    ServiceNamePut = ?SERVICE_PREFIX1 ++ ?WRITABLE_FILENAME1 ++ "/put",
    Timeout = undefined, % default
    Priority = undefined, % default
    {{ok,
      [{<<"content-type">>, <<"text/plain">>},
       {<<"content-disposition">>,
        <<"attachment; filename=\"" ?WRITABLE_FILENAME1 "\"">>},
       {<<"etag">>, ETag0},
       {<<"last-modified">>, LastModified0},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      <<>>},
     Context1} = cloudi:send_sync(Context0, ServiceNameGet,
                                  <<>>, <<>>,
                                  Timeout, Priority),
    {error, enoent} = file:read_file(writable_filepath2(Config)),
    Request1 = <<0:8192>>, % 1 Kb
    {{ok,
      [{<<"content-type">>, <<"text/plain">>},
       {<<"content-disposition">>,
        <<"attachment; filename=\"" ?WRITABLE_FILENAME1 "\"">>},
       {<<"etag">>, ETag1},
       {<<"last-modified">>, LastModified1},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>}],
      Request1},
     Context2} = cloudi:send_sync(Context1, ServiceNamePut,
                                  <<>>, Request1,
                                  Timeout, Priority),
    {ok, Request1} = file:read_file(writable_filepath2(Config)),
    true = ETag0 /= ETag1,
    true = LastModified0 =< LastModified1,
    Request2 = <<0:8200>>, % 1 Kb + 1 byte
    {{ok,
      [{<<"status">>, <<"400">>}],
      <<>>},
     _} = cloudi:send_sync(Context2, ServiceNamePut,
                           <<>>, Request2,
                           Timeout, Priority),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

writable_filepath1(Config) ->
    filename:join(?config(tmpdir1, Config), ?WRITABLE_FILENAME1).

writable_filepath2(Config) ->
    filename:join(?config(tmpdir2, Config), ?WRITABLE_FILENAME1).

