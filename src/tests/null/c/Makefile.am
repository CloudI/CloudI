#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(cloudi_prefix)/tests/null"
inst_PROGRAMS = null_c
null_c_SOURCES = main.c
null_c_CFLAGS = -fexceptions -I$(top_srcdir)/api/c/
null_c_LDADD = $(top_builddir)/api/c/libcloudi.la $(CXX_STDLIB_LIB)

