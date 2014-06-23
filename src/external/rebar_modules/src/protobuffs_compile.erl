%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
-module(protobuffs_compile).
-export([scan_file/2]).

scan_file(Files, Options) ->
    cloudi_x_protobuffs_compile:scan_file(Files, Options).
