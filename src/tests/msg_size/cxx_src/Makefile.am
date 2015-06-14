#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(cloudi_prefix)/tests/msg_size/priv"
inst_PROGRAMS = msg_size
msg_size_SOURCES = main.cpp
msg_size_CPPFLAGS = -I$(top_srcdir)/api/c/
msg_size_LDFLAGS =
msg_size_LDADD = $(top_builddir)/api/c/libcloudi.la

