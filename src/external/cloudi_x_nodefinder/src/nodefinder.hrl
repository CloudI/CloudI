%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:

% for features specific to Erlang/OTP version 20.x (and later versions)
-ifdef(ERLANG_OTP_VERSION_19).
-else.
-define(ERLANG_OTP_VERSION_20_FEATURES, true).
-ifdef(ERLANG_OTP_VERSION_20).
-else.
-ifdef(OTP_RELEASE).
% able to use -if/-elif here
-if(?OTP_RELEASE >= 23).
-define(ERLANG_OTP_VERSION_23_FEATURES, true).
-endif.
-else.
-error("Erlang/OTP version invalid").
-endif.
-endif.
-endif.

