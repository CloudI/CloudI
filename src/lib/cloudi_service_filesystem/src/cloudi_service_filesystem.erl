%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Filesystem==
%%% A service that caches filesystem data for quick responses.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2011-2014, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2011-2014 Michael Truog
%%% @version 1.3.2 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_filesystem).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloudi_service).

%% external interface
-export([notify_all/3,
         notify_all/4,
         notify_all/5,
         notify_one/3,
         notify_one/4,
         notify_one/5,
         notify_clear/2]).

%% cloudi_service callbacks
-export([cloudi_service_init/3,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/2]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").
-include_lib("kernel/include/file.hrl").

-define(DEFAULT_REFRESH,             undefined). % seconds
-define(DEFAULT_CACHE,               undefined). % seconds
-define(DEFAULT_NOTIFY_ONE,                 []). % {Name, NotifyName}
-define(DEFAULT_NOTIFY_ALL,                 []). % {Name, NotifyName}
-define(DEFAULT_NOTIFY_ON_START,          true). % send notify in init
-define(DEFAULT_USE_HTTP_GET_SUFFIX,      true). % get as a name suffix

-record(state,
    {
        prefix :: string(),
        directory :: string(),
        refresh :: undefined | pos_integer(),
        cache :: undefined | pos_integer(),
        use_http_get_suffix :: boolean(),
        toggle :: boolean(),
        files :: cloudi_x_trie:cloudi_x_trie()
    }).

-record(file_notify,
    {
        send :: mcast_async | send_async,
        service_name :: cloudi_service:service_name(),
        timeout :: cloudi_service:timeout_milliseconds(),
        priority :: cloudi_service:priority()
    }).

-record(file,
    {
        contents :: binary(),
        path :: string(),
        mtime :: calendar:datetime(),
        toggle :: boolean(),
        notify = [] :: list(#file_notify{})
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Subscribe all service processes to be notified of file updates.===
%% The current file contents is returned, if the file is found.
%% @end
%%-------------------------------------------------------------------------

-spec notify_all(Dispatcher :: cloudi_service:dispatcher() |
                               cloudi:context(),
                 Name :: cloudi_service:service_name(),
                 NotifyName :: cloudi_service:service_name()) ->
    {ok, binary()} | {error, any()}.

notify_all(Dispatcher, Name, NotifyName) ->
    notify_all(Dispatcher, Name,
               NotifyName, undefined, undefined).

%%-------------------------------------------------------------------------
%% @doc
%% ===Subscribe all service processes to be notified of file updates.===
%% The current file contents is returned, if the file is found.
%% @end
%%-------------------------------------------------------------------------

-spec notify_all(Dispatcher :: cloudi_service:dispatcher() |
                               cloudi:context(),
                 Name :: cloudi_service:service_name(),
                 NotifyName :: cloudi_service:service_name(),
                 NotifyTimeout :: cloudi_service:timeout_milliseconds()) ->
    {ok, binary()} | {error, any()}.

notify_all(Dispatcher, Name, NotifyName, NotifyTimeout) ->
    notify_all(Dispatcher, Name,
               NotifyName, NotifyTimeout, undefined).

%%-------------------------------------------------------------------------
%% @doc
%% ===Subscribe all service processes to be notified of file updates.===
%% The current file contents is returned, if the file is found.
%% @end
%%-------------------------------------------------------------------------

-spec notify_all(Dispatcher :: cloudi_service:dispatcher() |
                               cloudi:context(),
                 Name :: cloudi_service:service_name(),
                 NotifyName :: cloudi_service:service_name(),
                 NotifyTimeout :: cloudi_service:timeout_milliseconds(),
                 NotifyPriority :: cloudi_service:priority()) ->
    {ok, binary()} | {error, any()}.

notify_all(Dispatcher, Name, NotifyName, NotifyTimeout, NotifyPriority) ->
    notify(Dispatcher, Name,
           NotifyName, NotifyTimeout, NotifyPriority, mcast_async).

%%-------------------------------------------------------------------------
%% @doc
%% ===Subscribe to have a service process notified of file updates.===
%% The current file contents is returned, if the file is found.
%% @end
%%-------------------------------------------------------------------------

-spec notify_one(Dispatcher :: cloudi_service:dispatcher() |
                               cloudi:context(),
                 Name :: cloudi_service:service_name(),
                 NotifyName :: cloudi_service:service_name()) ->
    {ok, binary()} | {error, any()}.

notify_one(Dispatcher, Name, NotifyName) ->
    notify_one(Dispatcher, Name,
               NotifyName, undefined, undefined).

%%-------------------------------------------------------------------------
%% @doc
%% ===Subscribe to have a service process notified of file updates.===
%% The current file contents is returned, if the file is found.
%% @end
%%-------------------------------------------------------------------------

-spec notify_one(Dispatcher :: cloudi_service:dispatcher() |
                               cloudi:context(),
                 Name :: cloudi_service:service_name(),
                 NotifyName :: cloudi_service:service_name(),
                 NotifyTimeout :: cloudi_service:timeout_milliseconds()) ->
    {ok, binary()} | {error, any()}.

notify_one(Dispatcher, Name, NotifyName, NotifyTimeout) ->
    notify_one(Dispatcher, Name,
               NotifyName, NotifyTimeout, undefined).

%%-------------------------------------------------------------------------
%% @doc
%% ===Subscribe to have a service process notified of file updates.===
%% The current file contents is returned, if the file is found.
%% @end
%%-------------------------------------------------------------------------

-spec notify_one(Dispatcher :: cloudi_service:dispatcher() |
                               cloudi:context(),
                 Name :: cloudi_service:service_name(),
                 NotifyName :: cloudi_service:service_name(),
                 NotifyTimeout :: cloudi_service:timeout_milliseconds(),
                 NotifyPriority :: cloudi_service:priority()) ->
    {ok, binary()} | {error, any()}.

notify_one(Dispatcher, Name, NotifyName, NotifyTimeout, NotifyPriority) ->
    notify(Dispatcher, Name,
           NotifyName, NotifyTimeout, NotifyPriority, send_async).

%%-------------------------------------------------------------------------
%% @doc
%% ===Clear all notification subscriptions for a file.===
%% @end
%%-------------------------------------------------------------------------

-spec notify_clear(Dispatcher :: cloudi_service:dispatcher() |
                                 cloudi:context(),
                   Name :: cloudi_service:service_name()) ->
    ok | {error, any()}.

notify_clear(Dispatcher, Name) ->
    case cloudi:send_sync(Dispatcher, Name, notify_clear) of
        {error, _} = Error ->
            Error;
        {ok, {error, _} = Error} ->
            Error;
        {ok, ok} ->
            ok
    end.

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, Prefix, Dispatcher) ->
    Defaults = [
        {directory,              undefined},
        {refresh,                ?DEFAULT_REFRESH},
        {cache,                  ?DEFAULT_CACHE},
        {notify_one,             ?DEFAULT_NOTIFY_ONE},
        {notify_all,             ?DEFAULT_NOTIFY_ALL},
        {notify_on_start,        ?DEFAULT_NOTIFY_ON_START},
        {use_http_get_suffix,    ?DEFAULT_USE_HTTP_GET_SUFFIX}],
    [DirectoryRaw, Refresh, Cache, NotifyOneL, NotifyAllL, NotifyOnStart,
     UseHttpGetSuffix] =
        cloudi_proplists:take_values(Defaults, Args),
    true = is_list(DirectoryRaw),
    true = ((Refresh =:= undefined) orelse
            (is_integer(Refresh) andalso
             (Refresh > 0) andalso (Refresh =< 4294967))),
    true = ((Cache =:= undefined) orelse
            ((Refresh /= undefined) andalso
             is_integer(Cache) andalso
             (Cache > 0) andalso (Cache =< 31536000))),
    true = is_list(NotifyOneL),
    true = is_list(NotifyAllL),
    true = is_boolean(NotifyOnStart),
    true = is_boolean(UseHttpGetSuffix),
    Directory = cloudi_service:environment_transform(DirectoryRaw),
    Toggle = true,
    Files1 = fold_files(Directory, fun(FilePath, FileName, FileInfo, Files0) ->
        #file_info{mtime = MTime} = FileInfo,
        case file_read_data(FileInfo, FilePath) of
            {ok, Contents} ->
                File = #file{contents = Contents,
                             path = FilePath,
                             mtime = MTime,
                             toggle = Toggle},
                file_add(FileName, File, Files0,
                         UseHttpGetSuffix, Prefix, Dispatcher);
            {error, Reason} ->
                ?LOG_WARN("file read ~s error: ~p", [FilePath, Reason]),
                Files0
        end
    end, cloudi_x_trie:new()),
    Timeout = cloudi_service:timeout_async(Dispatcher),
    Priority = cloudi_service:priority_default(Dispatcher),
    Files4 = lists:foldl(fun({NameOne, NotifyNameOne}, Files2) ->
        case files_notify(#file_notify{send = send_async,
                                       service_name = NotifyNameOne},
                          NameOne, Files2, Timeout, Priority,
                          Prefix, Directory, UseHttpGetSuffix) of
            {ok, _, Files3} ->
                Files3;
            {error, Reason} ->
                ?LOG_ERROR("~p: ~p", [Reason, NameOne]),
                Files2
        end
    end, Files1, NotifyOneL),
    Files7 = lists:foldl(fun({NameAll, NotifyNameAll}, Files5) ->
        case files_notify(#file_notify{send = mcast_async,
                                       service_name = NotifyNameAll},
                          NameAll, Files5, Timeout, Priority,
                          Prefix, Directory, UseHttpGetSuffix) of
            {ok, _, Files6} ->
                Files6;
            {error, Reason} ->
                ?LOG_ERROR("~p: ~p", [Reason, NameAll]),
                Files5
        end
    end, Files4, NotifyAllL),
    if
        NotifyOnStart =:= true ->
            DirectoryLength = erlang:length(Directory),
            cloudi_x_trie:foreach(fun(Name, #file{contents = Contents,
                                                  path = FilePath,
                                                  notify = NotifyL}) ->
                {_, FileName} = lists:split(DirectoryLength, FilePath),
                case lists:prefix(Prefix ++ FileName, Name) of
                    true ->
                        file_notify_send(NotifyL, Contents, Dispatcher);
                    false ->
                        ok
                end
            end, Files7);
        true ->
            ok
    end,
    if
        is_integer(Refresh) ->
            erlang:send_after(Refresh * 1000,
                              cloudi_service:self(Dispatcher), refresh);
        true ->
            ok
    end,
    {ok, #state{prefix = Prefix,
                directory = Directory,
                refresh = Refresh,
                cache = Cache,
                toggle = Toggle,
                files = Files7,
                use_http_get_suffix = UseHttpGetSuffix}}.

cloudi_service_handle_request(_Type, _Name, Pattern, _RequestInfo,
                              #file_notify{} = Notify,
                              Timeout, Priority, _TransId, _Pid,
                              #state{prefix = Prefix,
                                     directory = Directory,
                                     files = Files,
                                     use_http_get_suffix = UseHttpGetSuffix
                                     } = State, _Dispatcher) ->
    case files_notify(Notify, Pattern, Files, Timeout, Priority,
                      Prefix, Directory, UseHttpGetSuffix) of
        {ok, Contents, NewFiles} ->
            {reply, Contents, State#state{files = NewFiles}};
        {error, _} = Error ->
            {reply, Error, State}
    end;
cloudi_service_handle_request(_Type, _Name, Pattern, _RequestInfo,
                              notify_clear,
                              _Timeout, _Priority, _TransId, _Pid,
                              #state{prefix = Prefix,
                                     directory = Directory,
                                     files = Files,
                                     use_http_get_suffix = UseHttpGetSuffix
                                     } = State, _Dispatcher) ->
    case cloudi_x_trie:find(Pattern, Files) of
        {ok, #file{path = FilePath} = File} ->
            DirectoryLength = erlang:length(Directory),
            {_, FileName} = lists:split(DirectoryLength, FilePath),
            NewFiles = file_refresh(FileName, File#file{notify = []},
                                    Files, UseHttpGetSuffix, Prefix),
            {reply, ok, State#state{files = NewFiles}};
        error ->
            {reply, {error, not_found}, State}
    end;
cloudi_service_handle_request(_Type, _Name, Pattern, RequestInfo, _Request,
                              _Timeout, _Priority, _TransId, _Pid,
                              #state{cache = Cache,
                                     files = Files} = State, _Dispatcher) ->
    case cloudi_x_trie:find(Pattern, Files) of
        {ok, #file{contents = Contents}} when Cache =:= undefined ->
            {reply, Contents, State};
        {ok, #file{mtime = MTime,
                   contents = Contents}} ->
            NowTime = calendar:now_to_universal_time(os:timestamp()),
            KeyValues = cloudi_service:
                        request_info_key_value_parse(RequestInfo),
            case cache_status(KeyValues, MTime) of
                200 ->
                    {reply, cache_headers_data(NowTime, MTime, Cache),
                     Contents, State};
                Status ->
                    % not modified due to caching
                    {reply,
                     [{<<"status">>, erlang:integer_to_binary(Status)} |
                      cache_headers_empty(NowTime, MTime)], <<>>, State}
            end;
        error ->
            % possible if a sending service has stale service name lookup data
            % with a file that was removed during a refresh
            {reply,
             [{<<"status">>, erlang:integer_to_binary(404)}], <<>>, State}
    end.

cloudi_service_handle_info(refresh,
                           #state{prefix = Prefix,
                                  directory = Directory,
                                  refresh = Refresh,
                                  toggle = Toggle,
                                  files = Files,
                                  use_http_get_suffix =
                                      UseHttpGetSuffix} = State,
                           Dispatcher) ->
    NewToggle = not Toggle,
    NewFiles = files_refresh(Directory, NewToggle,
                             Files, UseHttpGetSuffix, Prefix, Dispatcher),
    erlang:send_after(Refresh * 1000,
                      cloudi_service:self(Dispatcher), refresh),
    {noreply, State#state{toggle = NewToggle,
                          files = NewFiles}};

cloudi_service_handle_info(Request, State, _) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_service_terminate(_, #state{}) ->
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

notify(Dispatcher, Name, [I | _] = NotifyName,
       NotifyTimeout, NotifyPriority, Send)
    when is_list(NotifyName), is_integer(I),
         ((is_integer(NotifyTimeout) andalso NotifyTimeout >= 0 andalso
           NotifyTimeout =< 4294967295) orelse
          (NotifyTimeout =:= undefined) orelse (NotifyTimeout =:= immediate)),
         ((is_integer(NotifyPriority) andalso NotifyPriority >= -128 andalso
           NotifyPriority =< 127) orelse (NotifyPriority =:= undefined)) ->
    case cloudi:send_sync(Dispatcher, Name,
                          #file_notify{send = Send,
                                       service_name = NotifyName,
                                       timeout = NotifyTimeout,
                                       priority = NotifyPriority}) of
        {error, _} = Error ->
            Error;
        {ok, {error, _} = Error} ->
            Error;
        {ok, Contents} = Success when is_binary(Contents) ->
            Success
    end.

files_notify(#file_notify{timeout = NotifyTimeout,
                          priority = NotifyPriority} = Notify,
             Name, Files, Timeout, Priority,
             Prefix, Directory, UseHttpGetSuffix) ->
    case cloudi_x_trie:find(Name, Files) of
        {ok, #file{contents = Contents,
                   path = FilePath,
                   notify = NotifyL} = File} ->
            NewNotifyTimeout = if
                is_integer(NotifyTimeout) ->
                    NotifyTimeout;
                true ->
                    Timeout
            end,
            NewNotifyPriority = if
                is_integer(NotifyPriority) ->
                    NotifyPriority;
                true ->
                    Priority
            end,
            NewNotify = Notify#file_notify{timeout = NewNotifyTimeout,
                                           priority = NewNotifyPriority},
            DirectoryLength = erlang:length(Directory),
            {_, FileName} = lists:split(DirectoryLength, FilePath),
            NewFiles = file_refresh(FileName,
                                    File#file{notify = [NewNotify | NotifyL]},
                                    Files, UseHttpGetSuffix, Prefix),
            {ok, Contents, NewFiles};
        error ->
            {error, not_found}
    end.

service_name_suffix_root(true) ->
    "/get";
service_name_suffix_root(false) ->
    "".

service_name_suffix_root_join([], Suffix) ->
    Suffix;
service_name_suffix_root_join(PathParts, Suffix) ->
    filename:join(lists:reverse(PathParts)) ++ [$/ | Suffix].

file_name_suffix_root("index.htm") ->
    true;
file_name_suffix_root("index.html") ->
    true;
file_name_suffix_root(_) ->
    false.

service_name_suffix_root(FileName, UseHttpGetSuffix) ->
    [File | PathParts] = lists:reverse(filename:split(FileName)),
    case file_name_suffix_root(File) of
        true ->
            service_name_suffix_root_join(PathParts,
                service_name_suffix_root(UseHttpGetSuffix));
        false ->
            undefined
    end.

service_name_suffix(FileName, true) ->
    FileName ++ "/get";
service_name_suffix(FileName, false) ->
    FileName.

file_add(FileName, File, Files0, UseHttpGetSuffix, Prefix, Dispatcher) ->
    Files1 = case service_name_suffix_root(FileName, UseHttpGetSuffix) of
        undefined ->
            Files0;
        Suffix0 ->
            cloudi_service:subscribe(Dispatcher, Suffix0),
            cloudi_x_trie:store(Prefix ++ Suffix0, File, Files0)
    end,
    Suffix1 = service_name_suffix(FileName, UseHttpGetSuffix),
    cloudi_service:subscribe(Dispatcher, Suffix1),
    cloudi_x_trie:store(Prefix ++ Suffix1, File, Files1).

file_refresh(FileName, File, Files0, UseHttpGetSuffix, Prefix) ->
    Files1 = case service_name_suffix_root(FileName, UseHttpGetSuffix) of
        undefined ->
            Files0;
        Suffix0 ->
            cloudi_x_trie:store(Prefix ++ Suffix0, File, Files0)
    end,
    Suffix1 = service_name_suffix(FileName, UseHttpGetSuffix),
    cloudi_x_trie:store(Prefix ++ Suffix1, File, Files1).

file_remove(FileName, Files0, UseHttpGetSuffix, Prefix, Dispatcher) ->
    Files1 = case service_name_suffix_root(FileName, UseHttpGetSuffix) of
        undefined ->
            Files0;
        Suffix0 ->
            cloudi_service:unsubscribe(Dispatcher, Suffix0),
            cloudi_x_trie:erase(Prefix ++ Suffix0, Files0)
    end,
    Suffix1 = service_name_suffix(FileName, UseHttpGetSuffix),
    cloudi_service:unsubscribe(Dispatcher, Suffix1),
    cloudi_x_trie:erase(Prefix ++ Suffix1, Files1).

file_exists(FileName, Files, UseHttpGetSuffix, Prefix) ->
    case service_name_suffix_root(FileName, UseHttpGetSuffix) of
        undefined ->
            Suffix1 = service_name_suffix(FileName, UseHttpGetSuffix),
            cloudi_x_trie:find(Prefix ++ Suffix1, Files);
        Suffix0 ->
            case cloudi_x_trie:find(Prefix ++ Suffix0, Files) of
                {ok, _} = Result0 ->
                    Result0;
                error ->
                    Suffix1 = service_name_suffix(FileName, UseHttpGetSuffix),
                    cloudi_x_trie:find(Prefix ++ Suffix1, Files)
            end
    end.

% allow it to read from any non-directory/link type (device, other, regular)
file_read_data(#file_info{access = Access}, FilePath)
    when Access =:= read; Access =:= read_write ->
    file:read_file(FilePath);
file_read_data(_, _) ->
    {error, enoent}.

file_notify_send([], _, _) ->
    ok;
file_notify_send([#file_notify{send = Send,
                               service_name = Name,
                               timeout = Timeout,
                               priority = Priority} | NotifyL],
                 Contents, Dispatcher) ->
    cloudi_service:Send(Dispatcher, Name, <<>>, Contents, Timeout, Priority),
    file_notify_send(NotifyL, Contents, Dispatcher).

files_refresh(Directory, Toggle,
              Files, UseHttpGetSuffix, Prefix, Dispatcher) ->
    Files1 = fold_files(Directory, fun(FilePath, FileName, FileInfo, Files0) ->
        #file_info{mtime = MTime} = FileInfo,
        case file_exists(FileName, Files0, UseHttpGetSuffix, Prefix) of
            {ok, #file{mtime = MTime} = File0} ->
                File1 = File0#file{toggle = Toggle},
                file_refresh(FileName, File1, Files0,
                             UseHttpGetSuffix, Prefix);
            {ok, #file{notify = NotifyL} = File0} ->
                case file_read_data(FileInfo, FilePath) of
                    {ok, Contents} ->
                        file_notify_send(NotifyL, Contents, Dispatcher),
                        File1 = File0#file{contents = Contents,
                                           toggle = Toggle,
                                           mtime = MTime},
                        file_refresh(FileName, File1, Files0,
                                     UseHttpGetSuffix, Prefix);
                    {error, _} ->
                        % file was removed during traversal
                        file_remove(FileName, Files0,
                                    UseHttpGetSuffix, Prefix,
                                    Dispatcher)
                end;
            error ->
                case file_read_data(FileInfo, FilePath) of
                    {ok, Contents} ->
                        File = #file{contents = Contents,
                                     path = FilePath,
                                     mtime = MTime,
                                     toggle = Toggle},
                        file_add(FileName, File, Files0,
                                 UseHttpGetSuffix, Prefix, Dispatcher);
                    {error, _} ->
                        % file was removed during traversal
                        file_remove(FileName, Files0,
                                    UseHttpGetSuffix, Prefix,
                                    Dispatcher)
                end
        end
    end, Files),
    PrefixLength = erlang:length(Prefix),
    cloudi_x_trie:foldl(fun(Pattern, #file{toggle = FileToggle}, Files2) ->
        if
            FileToggle =/= Toggle ->
                {_, Suffix} = lists:split(PrefixLength, Pattern),
                cloudi_service:unsubscribe(Dispatcher, Suffix),
                cloudi_x_trie:erase(Pattern, Files2);
            true ->
                Files2
        end
    end, Files1, Files1).

-spec fold_files(Directory :: string() | binary(),
                 F :: fun((string(), string(), #file_info{}, any()) -> any()),
                 A :: any()) ->
    any().

% similar to filelib:fold_files/5 but
% with recursive always true, with a regex of ".*",
% with links included, and with both the filename and file_info provided to F
fold_files(Directory, F, A)
    when is_function(F, 4) ->
    case file:list_dir_all(Directory) of
        {ok, FileNames} ->
            fold_files_directory(FileNames, Directory, F, A);
        {error, Reason} ->
            ?LOG_WARN("directory ~s error: ~p", [Directory, Reason]),
            A
    end.

fold_files(Directory, Path, F, A)
    when is_function(F, 4) ->
    case file:list_dir_all(filename:join(Directory, Path)) of
        {ok, FileNames} ->
            fold_files_directory([filename:join(Path, FileName) ||
                                  FileName <- FileNames],
                                 Directory, F, A);
        {error, Reason} ->
            ?LOG_WARN("directory ~s path ~s error: ~p",
                      [Directory, Path, Reason]),
            A
    end.

fold_files_f(FilePath, FileName, FileInfo, F, A) ->
    FilePathString = if
        is_binary(FilePath) ->
            erlang:binary_to_list(FilePath);
        is_list(FilePath) ->
            FilePath
    end,
    FileNameString = if
        is_binary(FileName) ->
            erlang:binary_to_list(FileName);
        is_list(FileName) ->
            FileName
    end,
    F(FilePathString, FileNameString, FileInfo, A).

fold_files_directory([], _, _, A) ->
    A;
fold_files_directory([FileName | FileNames], Directory, F, A) ->
    FilePath = filename:join(Directory, FileName),
    case file:read_file_info(FilePath, [{time, universal}]) of
        {ok, #file_info{type = directory}} ->
            fold_files_directory(FileNames, Directory, F,
                                 fold_files(Directory, FileName, F, A));
        {ok, FileInfo} ->
            fold_files_directory(FileNames, Directory, F,
                                 fold_files_f(FilePath, FileName, FileInfo,
                                              F, A));
        {error, Reason} ->
            ?LOG_WARN("file ~s error: ~p", [FilePath, Reason]),
            fold_files_directory(FileNames, Directory, F, A)
    end.

cache_status(KeyValues, MTime) ->
    ETag = cache_header_etag(MTime),
    cache_status_0(ETag, KeyValues, MTime).

cache_status_0(ETag, KeyValues, MTime) ->
    case cloudi_service:key_value_find(<<"if-none-match">>, KeyValues) of
        {ok, <<"*">>} ->
            cache_status_1(ETag, KeyValues, MTime);
        error ->
            cache_status_1(ETag, KeyValues, MTime);
        {ok, IfNoneMatch} ->
            case binary:match(IfNoneMatch, ETag) of
                nomatch ->
                    cache_status_1(ETag, KeyValues, MTime);
                _ ->
                    304
            end
    end.

cache_status_1(ETag, KeyValues, MTime) ->
    case cloudi_service:key_value_find(<<"if-match">>, KeyValues) of
        {ok, <<"*">>} ->
            cache_status_2(KeyValues, MTime);
        error ->
            cache_status_2(KeyValues, MTime);
        {ok, IfMatch} ->
            case binary:match(IfMatch, ETag) of
                nomatch ->
                    412;
                _ ->
                    cache_status_2(KeyValues, MTime)
            end
    end.

cache_status_2(KeyValues, MTime) ->
    case cloudi_service:key_value_find(<<"if-modified-since">>, KeyValues) of
        {ok, DateTimeBinary} ->
            case cloudi_service_filesystem_datetime:parse(DateTimeBinary) of
                {error, _} ->
                    cache_status_3(KeyValues, MTime);
                DateTime when MTime > DateTime ->
                    200;
                _ ->
                    304
            end;
        error ->
            cache_status_3(KeyValues, MTime)
    end.

cache_status_3(KeyValues, MTime) ->
    case cloudi_service:key_value_find(<<"if-unmodified-since">>, KeyValues) of
        {ok, DateTimeBinary} ->
            case cloudi_service_filesystem_datetime:parse(DateTimeBinary) of
                {error, _} ->
                    200;
                DateTime when MTime =< DateTime ->
                    412;
                _ ->
                    200
            end;
        error ->
            200
    end.

cache_header_etag(MTime) ->
    Output = io_lib:format("\"~.16b\"",
                           [calendar:datetime_to_gregorian_seconds(MTime)]),
    erlang:iolist_to_binary(Output).

cache_header_control(Cache) ->
    Output = io_lib:format("public,max-age=~w", [Cache]),
    erlang:iolist_to_binary(Output).

cache_header_expires(ATime, Cache) ->
    Seconds = calendar:datetime_to_gregorian_seconds(ATime),
    Expires = calendar:gregorian_seconds_to_datetime(Seconds + Cache),
    rfc1123_format(Expires).

cache_headers_data(NowTime, MTime, Cache) ->
    [{<<"etag">>, cache_header_etag(MTime)},
     {<<"cache-control">>, cache_header_control(Cache)},
     {<<"expires">>, cache_header_expires(NowTime, Cache)},
     {<<"last-modified">>, rfc1123_format(MTime)},
     {<<"date">>, rfc1123_format(NowTime)}].

cache_headers_empty(NowTime, MTime) ->
    [{<<"last-modified">>, rfc1123_format(MTime)},
     {<<"date">>, rfc1123_format(NowTime)}].

rfc1123_format_day(1) ->
    "Mon";
rfc1123_format_day(2) ->
    "Tue";
rfc1123_format_day(3) ->
    "Wed";
rfc1123_format_day(4) ->
    "Thu";
rfc1123_format_day(5) ->
    "Fri";
rfc1123_format_day(6) ->
    "Sat";
rfc1123_format_day(7) ->
    "Sun".

rfc1123_format_month(1) ->
    "Jan";
rfc1123_format_month(2) ->
    "Feb";
rfc1123_format_month(3) ->
    "Mar";
rfc1123_format_month(4) ->
    "Apr";
rfc1123_format_month(5) ->
    "May";
rfc1123_format_month(6) ->
    "Jun";
rfc1123_format_month(7) ->
    "Jul";
rfc1123_format_month(8) ->
    "Aug";
rfc1123_format_month(9) ->
    "Sep";
rfc1123_format_month(10) ->
    "Oct";
rfc1123_format_month(11) ->
    "Nov";
rfc1123_format_month(12) ->
    "Dec".

rfc1123_format({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    % e.g., Sun, 06 Nov 1994 08:49:37 GMT
    %       Mon, 29 Apr 2013 21:44:55 GMT
    DayStr = rfc1123_format_day(calendar:day_of_the_week(Year, Month, Day)),
    MonthStr = rfc1123_format_month(Month),
    Output = io_lib:format("~s, ~2..0w ~s ~4..0w ~2..0w:~2..0w:~2..0w GMT",
                           [DayStr, Day, MonthStr, Year, Hour, Minute, Second]),
    erlang:iolist_to_binary(Output).

