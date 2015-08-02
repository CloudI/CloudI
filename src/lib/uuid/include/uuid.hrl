%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:

%%% provide the typical uuid namespaces
-define(UUID_NAMESPACE_DNS,  <<16#6ba7b8109dad11d180b400c04fd430c8:128>>).
-define(UUID_NAMESPACE_URL,  <<16#6ba7b8119dad11d180b400c04fd430c8:128>>).
-define(UUID_NAMESPACE_OID,  <<16#6ba7b8129dad11d180b400c04fd430c8:128>>).
-define(UUID_NAMESPACE_X500, <<16#6ba7b8149dad11d180b400c04fd430c8:128>>).

