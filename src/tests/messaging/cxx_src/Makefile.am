#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet:

instdir = "$(cloudi_prefix)/tests/messaging/priv"
inst_PROGRAMS = messaging
messaging_SOURCES = assert.cpp main.cpp timer.cpp
messaging_CPPFLAGS = -I$(top_srcdir)/api/c/ $(BOOST_CPPFLAGS)
messaging_LDFLAGS = $(BOOST_LDFLAGS)
messaging_LDADD = $(top_builddir)/api/c/libcloudi.la \
                  $(BOOST_THREAD_LIB) $(BOOST_SYSTEM_LIB) \
                  $(RT_LIB)

