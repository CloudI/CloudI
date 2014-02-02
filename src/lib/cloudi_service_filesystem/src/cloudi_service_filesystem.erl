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
%%% @version 1.3.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_filesystem).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloudi_service).

%% external interface

%% cloudi_service callbacks
-export([cloudi_service_init/3,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/2]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").
-include_lib("kernel/include/file.hrl").

-define(DEFAULT_USE_HTTP_GET_SUFFIX,      true). % get as a name suffix
-define(DEFAULT_REFRESH,             undefined). % seconds

-record(state,
    {
        directory :: string(),
        refresh :: undefined | pos_integer(),
        toggle :: boolean(),
        files :: cloudi_x_trie:cloudi_x_trie(),
        use_http_get_suffix :: boolean(),
        prefix :: string()
    }).

-record(file,
    {
        contents :: binary(),
        path :: string(),
        mtime,
        toggle :: boolean()
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, Prefix, Dispatcher) ->
    Defaults = [
        {directory,              undefined},
        {refresh,                ?DEFAULT_REFRESH},
        {use_http_get_suffix,    ?DEFAULT_USE_HTTP_GET_SUFFIX}],
    [DirectoryRaw, Refresh, UseHttpGetSuffix] =
        cloudi_proplists:take_values(Defaults, Args),
    true = is_list(DirectoryRaw),
    true = ((Refresh =:= undefined) orelse
            (is_integer(Refresh) andalso
             (Refresh > 0) andalso (Refresh =< 4294967))),
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
            {error, _} ->
                % file was removed during traversal
                Files0
        end
    end, cloudi_x_trie:new()),
    if
        is_integer(Refresh) ->
            erlang:send_after(Refresh * 1000,
                              cloudi_service:self(Dispatcher), refresh);
        true ->
            ok
    end,
    {ok, #state{directory = Directory,
                refresh = Refresh,
                toggle = Toggle,
                files = Files1,
                use_http_get_suffix = UseHttpGetSuffix,
                prefix = Prefix}}.

cloudi_service_handle_request(_Type, _Name, Pattern, _RequestInfo, _Request,
                              _Timeout, _Priority, _TransId, _Pid,
                              #state{files = Files} = State, _Dispatcher) ->
    case cloudi_x_trie:find(Pattern, Files) of
        {ok, #file{contents = Contents}} ->
            {reply, Contents, State};
        error ->
            % possible if a sending service has stale service name lookup data
            % with a file that was removed during a refresh
            {reply,
             [{<<"status">>, erlang:integer_to_binary(404)}], <<>>, State}
    end.

cloudi_service_handle_info(refresh,
                           #state{directory = Directory,
                                  refresh = Refresh,
                                  toggle = Toggle,
                                  files = Files,
                                  use_http_get_suffix = UseHttpGetSuffix,
                                  prefix = Prefix} = State,
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

service_name_suffix_root(true) ->
    "/get";
service_name_suffix_root(false) ->
    "".
service_name_suffix_root("index.htm", UseHttpGetSuffix) ->
    service_name_suffix_root(UseHttpGetSuffix);
service_name_suffix_root("index.html", UseHttpGetSuffix) ->
    service_name_suffix_root(UseHttpGetSuffix);
service_name_suffix_root(_, _) ->
    undefined.

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

files_refresh(Directory, Toggle,
              Files, UseHttpGetSuffix, Prefix, Dispatcher) ->
    Files1 = fold_files(Directory, fun(FilePath, FileName, FileInfo, Files0) ->
        #file_info{mtime = MTime} = FileInfo,
        case file_exists(FileName, Files0, UseHttpGetSuffix, Prefix) of
            {ok, #file{mtime = MTime} = File0} ->
                File1 = File0#file{toggle = Toggle},
                file_refresh(FileName, File1, Files0,
                             UseHttpGetSuffix, Prefix);
            {ok, File0} ->
                case file_read_data(FileInfo, FilePath) of
                    {ok, Contents} ->
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
            fold_files(FilePath, F, A);
        {ok, FileInfo} ->
            fold_files_directory(FileNames, Directory, F,
                                 fold_files_f(FilePath, FileName, FileInfo,
                                              F, A));
        {error, Reason} ->
            ?LOG_WARN("file ~s error: ~p", [FilePath, Reason]),
            fold_files_directory(FileNames, Directory, F, A)
    end.

