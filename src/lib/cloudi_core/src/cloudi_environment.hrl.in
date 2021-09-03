%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% CloudI Runtime Environment
%%%
%%% MIT License
%%%
%%% Copyright (c) 2018-2021 Michael Truog <mjtruog at protonmail dot com>
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
%%%------------------------------------------------------------------------

-include("cloudi_core_i_constants.hrl").

-ifdef(CLOUDI_CORE_STANDALONE).

-define(SYSCALL_LOCK_TYPE, undefined).
-define(CODE_STATUS_STATIC, []).

-else.

-define(SYSCALL_LOCK_TYPE, @SYSCALL_LOCK_TYPE@).
-define(CODE_STATUS_STATIC,
        [{build_machine,
          "@BUILD_MACHINE@"},
         {build_kernel_version,
          "@BUILD_KERNEL_VERSION@"},
         {build_operating_system,
          "@BUILD_OPERATING_SYSTEM@"},
         {build_erlang_otp_release,
          "@ERLANG_OTP_VER@"},
         {build_cloudi_time,
          "@BUILD_TIME@"},
         {build_cloudi_version,
          "@BUILD_REPOSITORY_VERSION@"},
         {build_cloudi_cxx_compiler_version,
          "@CXX_COMPILER_VENDOR_VERSION@"},
         {build_cloudi_cxx_dependencies_versions,
          "@CXX_DEPENDENCIES_VERSIONS@"}]).

-endif.

