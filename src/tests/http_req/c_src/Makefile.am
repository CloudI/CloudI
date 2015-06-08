#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(cloudi_prefix)/tests/http_req/priv"
inst_PROGRAMS = http_req
http_req_SOURCES = main.c
http_req_CFLAGS = -fexceptions -I$(top_srcdir)/api/c/
http_req_LDADD = $(top_builddir)/api/c/libcloudi.la -lstdc++

